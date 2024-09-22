with Ada.Real_Time;          use Ada.Real_Time;
with STM32.CRC;              use STM32.CRC;
with STM32.GPIO;             use STM32.GPIO;
with HAL;                    use HAL;
with Hardware_Configuration; use Hardware_Configuration;
with STM32.USARTs;           use STM32.USARTs;
with STM32.DMA;              use STM32.DMA;
with STM32.Device;           use STM32.Device;
with Physical_Types;         use Physical_Types;
with STM32.Flash;
with Self_Check;
with Ada.Characters.Latin_1;
with Step_Generator;
with Steppers;
with High_Power_Switch;
with Input_Switches;
with Thermistors;
with Heaters;
with Fans;
with System.Machine_Code;

package body Server_Communication is

   procedure Init is
   begin
      Enable_Clock (Comms_UART_DMA_RX_Controller);
      Enable_Clock (Comms_UART);
      Enable_Clock (Comms_CRC_Unit);

      Set_Word_Length (Comms_UART, Word_Length_8);
      Set_Parity (Comms_UART, No_Parity);
      Set_Mode (Comms_UART, Tx_Rx_Mode);
      Set_Oversampling_Mode (Comms_UART, Oversampling_By_16);
      Set_Stop_Bits (Comms_UART, Stopbits_1);
      Set_Flow_Control (Comms_UART, No_Flow_Control);
      Set_Baud_Rate (Comms_UART, 6_000_000);
      Enable (Comms_UART);
      --  stty -F /dev/... cs8 -cstopb -parenb raw

      delay until Clock + Milliseconds (10);

      Configure_IO
        (Comms_UART_TX_Pin,
        (Mode            => Mode_AF,
          Resistors      => Floating,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_100MHz,
          AF             => Comms_UART_TX_Pin_AF));
      Configure_IO
        (Comms_UART_RX_Pin,
        (Mode            => Mode_AF,
          Resistors      => Floating,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_100MHz,
          AF             => Comms_UART_RX_Pin_AF));

      Init_Done := True;
   end Init;

   function Is_Init_Done return Boolean is
   begin
      return Init_Done;
   end Is_Init_Done;

   procedure Run is
      function Are_All_Statuses_Clear return Boolean is
      begin
         for S in USART_Status_Flag loop
            if Status (Comms_UART, S) and S /= Transmit_Data_Register_Empty then
               return False;
            end if;
         end loop;
         return True;
      end Are_All_Statuses_Clear;

      Last_Message_Index       : Message_Index := 0;
      In_Conditional_Skip_Mode : Boolean       := False;
      Setup_Done               : Boolean       := False;
   begin
      --  Ensure nothing is transmitting junk.
      loop
         delay until Clock + Seconds (1);

         exit when Are_All_Statuses_Clear and not Rx_Ready (Comms_UART);

         for S in USART_Status_Flag loop
            Clear_Status (Comms_UART, S);
         end loop;

         declare
            Junk : UInt9 := Current_Input (Comms_UART);
         begin
            null;
         end;
      end loop;

      Configure
        (Comms_UART_DMA_RX_Controller,
         Comms_UART_DMA_RX_Stream,
         (Channel                      => Comms_UART_DMA_RX_Channel,
          Direction                    => Peripheral_To_Memory,
          Increment_Peripheral_Address => False,
          Increment_Memory_Address     => True,
          Peripheral_Data_Format       => Bytes,
          Memory_Data_Format           => Bytes,
          Operation_Mode               => Normal_Mode,
          Priority                     => Comms_UART_DMA_RX_Priority,
          Memory_Burst_Size            => Memory_Burst_Single,
          Peripheral_Burst_Size        => Peripheral_Burst_Single));

      Enable_DMA_Receive_Requests (Comms_UART);

      Start_Transfer
        (This        => Comms_UART_DMA_RX_Controller,
         Stream      => Comms_UART_DMA_RX_Stream,
         Source      => Read_Data_Register_Address (Comms_UART),
         Destination => RX_Message'Address,
         Data_Count  => UInt16 (Message_From_Server'Value_Size / 8));
      --  TODO: For some reason the above line gives a Range_Check error without the Uint16.

      Set_TX_Message_Kind (Hello_Kind);
      TX_Message.Content.Index       := Last_Message_Index;
      TX_Message.Content.Version     := 1;
      TX_Message.Content.Self_Length := Message_From_Client'Value_Size / 4;
      TX_Message.Content.ID          :=
        DO_NOT_COPY_THIS_CLIENT_ID_AS_IT_IS_MEANT_TO_IDENTIFY_THIS_PARTICULAR_BOARD_MODEL_AND_FIRMWARE;
      Set_TX_Message_CRC;

      declare
         Error : DMA_Error_Code;
      begin
         loop
            Transmit_TX_Message;
            Poll_For_Completion
              (This           => Comms_UART_DMA_RX_Controller,
               Stream         => Comms_UART_DMA_RX_Stream,
               Expected_Level => Full_Transfer,
               Timeout        => Seconds (1),
               Result         => Error);

            exit when Error = DMA_No_Error;

            --  TODO: Detect framing errors and use them to stop polling.
            if Error = DMA_Transfer_Error or Error = DMA_Device_Error then
               raise DMA_Error with Error'Image;
            end if;
         end loop;
      end;

      loop
         delay until Clock + Milliseconds (1);
         --  TODO: Is a delay required before disabling the DMA or will it always transfer the last byte?
         Disable (Comms_UART_DMA_RX_Controller, Comms_UART_DMA_RX_Stream);

         System.Machine_Code.Asm
           ("", Outputs => Message_From_Server'Asm_Output ("=g", RX_Message), Clobber => "memory", Volatile => True);

         for S in USART_Status_Flag loop
            if
              (S = Parity_Error_Indicated or S = USART_Noise_Error_Indicated or S = Framing_Error_Indicated or
               S = Overrun_Error_Indicated)
              and then Status (Comms_UART, S)
            then
               raise Constraint_Error with "USART status error: " & S'Image;
            end if;
         end loop;

         declare
            CRC_Output : UInt32;
            CRC_Input  : Block_32 (1 .. Message_From_Server_Content'Value_Size / 32) with
              Address => RX_Message.Content'Address;
         begin
            Reset_CRC (Comms_CRC_Unit);
            Set_Data_Input_Order (Comms_CRC_Unit, Word_Reversed);
            Set_Data_Output_Order (Comms_CRC_Unit, Bit_Reversed);
            Update_CRC (Comms_CRC_Unit, CRC_Input, CRC_Output);

            if CRC32 (CRC_Output xor 16#FFFF_FFFF#) /= RX_Message.Checksum then
               Transmit_String_Line
                 ("Bad CRC, expected " & CRC_Output'Image & " but got " & RX_Message.Checksum'Image);
               --  This will cause the last message to be resent, which will cause the server to resend its last
               --  message.
            elsif RX_Message.Content.Index = Last_Message_Index then
               Transmit_String_Line ("Got index " & RX_Message.Content.Index'Image & ". Resending last.");
               --  This will cause the last message to be resent. The server sending the same message twice indicates
               --  that the checksum did not match.
            elsif RX_Message.Content.Index /= Last_Message_Index + 1 then
               raise Constraint_Error
                 with "Server sent wrong message index. Expected " & Last_Message_Index'Image & " but got " &
                 RX_Message.Content.Index'Image;
            elsif not Setup_Done and RX_Message.Content.Kind /= Setup_Kind and
              RX_Message.Content.Kind not in Firmware_Update_Start_Kind .. Firmware_Update_Done_Kind
            then
               raise Constraint_Error with "Expected setup message, server sent " & RX_Message.Content.Kind'Image;
            elsif Setup_Done and RX_Message.Content.Kind = Setup_Kind then
               raise Constraint_Error with "Server sent multiple setup messages.";
            else
               Last_Message_Index := Last_Message_Index + 1;

               TX_Message.Content.Index := Last_Message_Index;
               Set_TX_Message_Kind (Status_Kind);

               case RX_Message.Content.Kind is
                  when Setup_Kind =>
                     Thermistors.Setup
                       (RX_Message.Content.Thermistor_Curves'Unrestricted_Access,
                        RX_Message.Content.Heater_Thermistors);
                     Thermistors.Start_ISR_Loop;
                     Setup_Done := True;
                  when Heater_Reconfigure_Kind =>
                     Heaters.Setup (RX_Message.Content.Heater, RX_Message.Content.Heater_Params);
                  when Loop_Setup_Kind =>
                     Step_Generator.Setup_Loop
                       (RX_Message.Content.Loop_Input_Switch, RX_Message.Content.Loop_Until_State);
                  when Regular_Step_Delta_List_Kind | Looping_Step_Delta_List_Kind =>
                     if In_Conditional_Skip_Mode then
                        if RX_Message.Content.Safe_Stop_After then
                           In_Conditional_Skip_Mode := False;
                        end if;
                     else
                        if RX_Message.Content.Kind = Looping_Step_Delta_List_Kind then
                           Step_Generator.Enqueue_Start_Loop;
                        end if;

                        for I in Step_Delta_List_Index'First .. RX_Message.Content.Last_Index loop
                           Step_Generator.Enqueue (RX_Message.Content.Steps (I));
                        end loop;

                        if RX_Message.Content.Kind = Looping_Step_Delta_List_Kind then
                           Step_Generator.Enqueue_Stop_Loop;
                        end if;

                        if RX_Message.Content.Safe_Stop_After then
                           Step_Generator.Enqueue ((Dirs => (others => Forward), Steps => (others => 0)));
                           Step_Generator.Force_Start;
                        end if;

                        for Heater in Heater_Name loop
                           Heaters.Set_Setpoint
                             (Heater, Dimensionless (RX_Message.Content.Heater_Targets (Heater)) * celcius);
                        end loop;
                        for Fan in Fan_Name loop
                           Fans.Set_PWM (Fan, RX_Message.Content.Fan_Targets (Fan));
                        end loop;
                        --  Setting fan and heater targets here means that they are set early, but that likely will not
                        --  be an issue since the total buffer size is less than a second.
                     end if;
                  when Condition_Check_Kind =>
                     if Input_Switches.Get_State (RX_Message.Content.Conditon_Input_Switch) =
                       RX_Message.Content.Skip_If_Hit_State
                     then
                        In_Conditional_Skip_Mode := True;
                     end if;
                  when TMC_Write_Kind =>
                     Steppers.UART_Write (RX_Message.Content.TMC_Write_Data);
                  when TMC_Read_Kind =>
                     Set_TX_Message_Kind (TMC_Read_Reply_Kind);
                     Steppers.UART_Read
                       (RX_Message.Content.TMC_Read_Data,
                        TX_Message.Content.TMC_Receive_Failed,
                        TX_Message.Content.TMC_Data);
                  when Status_Kind =>
                     null;
                  when Check_If_Idle_Kind =>
                     Set_TX_Message_Kind (Check_Reply_Kind);
                     TX_Message.Content.Condition_Met := Byte_Boolean (Step_Generator.Check_If_Idle);
                  when Check_If_Heater_Autotune_Done_Kind =>
                     Set_TX_Message_Kind (Check_Reply_Kind);
                     TX_Message.Content.Condition_Met :=
                       Byte_Boolean (Heaters.Check_If_Autotune_Done (RX_Message.Content.Heater_To_Check));
                  when Enable_Stepper_Kind =>
                     Steppers.Enable (RX_Message.Content.Stepper);
                  when Disable_Stepper_Kind =>
                     Steppers.Disable (RX_Message.Content.Stepper);
                  when Enable_High_Power_Switch_Kind =>
                     High_Power_Switch.Wait_For_Power_Good;
                     High_Power_Switch.Enable;
                  when Disable_High_Power_Switch_Kind =>
                     High_Power_Switch.Disable;
                  when Firmware_Update_Start_Kind =>
                     Set_TX_Message_Kind (Firmware_Update_Reply_Kind);
                     if not STM32.Flash.Is_Locked (Flash) then
                        raise Constraint_Error with "Update already started.";
                     else
                        STM32.Flash.Unlock (Flash);
                        STM32.Flash.Erase_Inactive_Bank (Flash);
                     end if;
                  when Firmware_Update_Data_Kind =>
                     Set_TX_Message_Kind (Firmware_Update_Reply_Kind);
                     if STM32.Flash.Is_Locked (Flash) then
                        raise Constraint_Error with "Flash is locked.";
                     end if;
                     declare
                        Address : constant Natural :=
                          16#0804_0000# + 10 * 1_024 * Natural (RX_Message.Content.Firmware_Offset);
                        Data    : STM32.Flash.Flash_Data (1 .. 10 * 1_024 / 8) with
                          Address => RX_Message.Content.Firmware_Data'Address;
                     begin
                        STM32.Flash.Write (Flash, System'To_Address (Address), Data);
                     end;
                  when Firmware_Update_Done_Kind =>
                     Set_TX_Message_Kind (Firmware_Update_Reply_Kind);
                     if STM32.Flash.Is_Locked (Flash) then
                        raise Constraint_Error with "Flash is locked.";
                     end if;
                     if not Self_Check.Other_Bank_Is_Valid then
                        raise Constraint_Error with "New firmware not valid. Not switching to new firmware.";
                     else
                        STM32.Flash.Switch_Active_Bank_And_Reset (Flash);
                     end if;
               end case;
            end if;
         end;

         if TX_Message.Content.Kind /= Hello_Kind and TX_Message.Content.Kind /= Firmware_Update_Reply_Kind then
            --  We always update these fields, even if the server asked for a resend.
            for Heater in Heater_Name loop
               TX_Message.Content.Heaters (Heater) := Heaters.Get_PWM (Heater);
            end loop;

            for Thermistor in Thermistor_Name loop
               declare
                  Temp : Temperature := Thermistors.Last_Reported_Temperature (Thermistor);
               begin
                  if Temp < Dimensionless (Fixed_Point_Celcius'First + 10.0) * celcius then
                     TX_Message.Content.Temperatures (Thermistor) := Fixed_Point_Celcius'First;
                  elsif Temp > Dimensionless (Fixed_Point_Celcius'Last - 10.0) * celcius then
                     TX_Message.Content.Temperatures (Thermistor) := Fixed_Point_Celcius'Last;
                  else
                     TX_Message.Content.Temperatures (Thermistor) := Fixed_Point_Celcius (Temp / celcius);
                  end if;
               end;
            end loop;

            for Switch in Input_Switch_Name loop
               TX_Message.Content.Switches (Switch) := Input_Switches.Get_State (Switch);
            end loop;

            for Fan in Fan_Name loop
               TX_Message.Content.Tachs (Fan) := Fans.Get_Tach_Counter (Fan);
            end loop;
         end if;

         Start_Transfer
           (This        => Comms_UART_DMA_RX_Controller,
            Stream      => Comms_UART_DMA_RX_Stream,
            Source      => Read_Data_Register_Address (Comms_UART),
            Destination => RX_Message'Address,
            Data_Count  => UInt16 (UInt32 (Message_From_Server'Value_Size) / 8));

         Set_TX_Message_CRC;
         Transmit_TX_Message;

         declare
            Error : DMA_Error_Code;
         begin
            Poll_For_Completion
              (This           => Comms_UART_DMA_RX_Controller,
               Stream         => Comms_UART_DMA_RX_Stream,
               Expected_Level => Full_Transfer,
               Timeout        => Seconds (5),
               Result         => Error);

            --  TODO: Change this to allow for failures if anyone reports them, specifically UART framing errors.
            case Error is
               when DMA_Transfer_Error | DMA_Device_Error =>
                  raise DMA_Error with Error'Image;
               when DMA_Timeout_Error =>
                  raise Timeout_Error with "No message from server for 5 seconds.";
               when DMA_No_Error =>
                  null;
            end case;
         end;
      end loop;
   end Run;

   procedure Set_TX_Message_Kind (Kind : Message_From_Client_Kind) is
      TX_Message_Kind : Message_From_Client_Kind with
        Address => TX_Message.Content.Kind'Address;
   begin
      TX_Message_Kind := Kind;
   end Set_TX_Message_Kind;

   procedure Set_TX_Message_CRC is
      CRC_Output : UInt32;
      CRC_Input  : Block_32 (1 .. Message_From_Client_Content'Value_Size / 32) with
        Address => TX_Message.Content'Address;
   begin
      Reset_CRC (Comms_CRC_Unit);
      Set_Data_Input_Order (Comms_CRC_Unit, Word_Reversed);
      Set_Data_Output_Order (Comms_CRC_Unit, Bit_Reversed);
      Update_CRC (Comms_CRC_Unit, CRC_Input, CRC_Output);
      TX_Message.Checksum := CRC32 (CRC_Output xor 16#FFFF_FFFF#);
   end Set_TX_Message_CRC;

   procedure Transmit_TX_Message is
      type UART_Data_4b is array (1 .. Message_From_Client'Value_Size / 4) of UInt4 with
        Pack;
      Data : UART_Data_4b with
        Address => TX_Message'Address;
   begin
      --  We send nibbles instead of bytes here and use an end-of-message marker as the messages are much smaller than
      --  messages going the other way and we need a way for the server to determine where a message ends for handling
      --  of hello messages. We could also send a break character, but Linux makes those difficult to work with using
      --  the default serial driver on the server side so we would likely need to use libFTDI or D2XX.
      --
      --  We also use characters above 128 so we can use ASCII characters for sending stack traces and other debug
      --  messages which the server can easily distinguish from regular messages.

      for Nibble of Data loop
         loop
            exit when Tx_Ready (Comms_UART);
         end loop;

         Transmit (Comms_UART, UInt9 (Nibble) + 128);
      end loop;

      loop
         exit when Tx_Ready (Comms_UART);
      end loop;

      Transmit (Comms_UART, 254);
   end Transmit_TX_Message;

   procedure Transmit_Fatal_Exception_Mark is
   begin
      loop
         exit when Tx_Ready (Comms_UART);
      end loop;

      Transmit (Comms_UART, 253);
   end Transmit_Fatal_Exception_Mark;

   procedure Transmit_String (S : String) is
   begin
      for C of S loop
         loop
            exit when Tx_Ready (Comms_UART);
         end loop;

         if Character'Enum_Rep (C) >= 128 then
            Transmit (Comms_UART, UInt9 (Character'Enum_Rep ('?')));
         else
            Transmit (Comms_UART, UInt9 (Character'Enum_Rep (C)));
         end if;
      end loop;
   end Transmit_String;

   procedure Transmit_String_Line (S : String) is
   begin
      Transmit_String (S);
      Transmit_String ("" & Ada.Characters.Latin_1.LF);
   end Transmit_String_Line;

end Server_Communication;
