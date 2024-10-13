with STM32.USARTs;           use STM32.USARTs;
with Hardware_Configuration; use Hardware_Configuration;
with STM32.GPIO;             use STM32.GPIO;
with STM32.Device;           use STM32.Device;
with STM32.DMA;              use STM32.DMA;
with Server_Communication;

package body Steppers is

   procedure Init is
   begin
      Init_Checker.Report_Init_Started;

      Enable_Clock (TMC_UART);
      Enable_Clock (TMC_UART_DMA_RX_Controller);

      Set_Word_Length (TMC_UART, Word_Length_8);
      Set_Parity (TMC_UART, No_Parity);
      Set_Mode (TMC_UART, Tx_Rx_Mode);
      Set_Oversampling_Mode (TMC_UART, Oversampling_By_16);
      Set_Stop_Bits (TMC_UART, Stopbits_1);
      Set_Flow_Control (TMC_UART, No_Flow_Control);
      Set_Baud_Rate (TMC_UART, 19_200);
      TMC_UART_Internal.CR1.FIFOEN := True;
      TMC_UART_Internal.CR3.HDSEL  := True;

      Enable (TMC_UART);

      delay until Clock + Milliseconds (10);

      Configure_IO
        (TMC_UART_Pin,
         (Mode           => Mode_AF,
          Resistors      => Floating,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_25MHz,
          AF             => TMC_UART_Pin_AF));

      delay until Clock + Milliseconds (10);

      for S in Stepper_Name loop
         Set (Stepper_Enable_Points (S));
         Configure_IO
           (Stepper_Enable_Points (S),
            (Mode => Mode_Out, Resistors => Floating, Output_Type => Push_Pull, Speed => Speed_100MHz));
      end loop;

      for S in Stepper_Name loop
         Configure_IO (Stepper_DIAG0_Points (S), (Mode => Mode_In, Resistors => Pull_Up));
      end loop;

      Configure
        (TMC_UART_DMA_RX_Controller,
         TMC_UART_DMA_RX_Stream,
        (Channel                       => TMC_UART_DMA_RX_Channel,
          Direction                    => Peripheral_To_Memory,
          Increment_Peripheral_Address => False,
          Increment_Memory_Address     => True,
          Peripheral_Data_Format       => Bytes,
          Memory_Data_Format           => Bytes,
          Operation_Mode               => Normal_Mode,
          Priority                     => TMC_UART_DMA_RX_Priority,
          Memory_Burst_Size            => Memory_Burst_Single,
          Peripheral_Burst_Size        => Peripheral_Burst_Single));

      Enable_DMA_Receive_Requests (TMC_UART);

      --  For some reason the UART says that it receives a byte when we first enable it and reading out that byte does
      --  not stop the UART from reading it out again.
      --
      --  TODO: Investigate this more.
      Start_Transfer
        (This        => TMC_UART_DMA_RX_Controller,
         Stream      => TMC_UART_DMA_RX_Stream,
         Source      => Read_Data_Register_Address (TMC_UART),
         Destination => RX_Buffer'Address,
         Data_Count  => 1);

      declare
         Error : DMA_Error_Code;
      begin
         delay until Clock + Milliseconds (10);
         Abort_Transfer (This => TMC_UART_DMA_RX_Controller, Stream => TMC_UART_DMA_RX_Stream, Result => Error);
         Clear_All_Status (TMC_UART_DMA_RX_Controller, TMC_UART_DMA_RX_Stream);
      end;

      Init_Checker.Report_Init_Done;
   end Init;

   procedure Enable (Stepper : Stepper_Name) is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      Clear (Stepper_Enable_Points (Stepper));
   end Enable;

   procedure Disable (Stepper : Stepper_Name) is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      Set (Stepper_Enable_Points (Stepper));
   end Disable;

   protected body UART_IO is
      procedure Start_Read (Input : TMC2240_UART_Query_Byte_Array) is
      begin
         Init_Checker.Raise_If_Init_Not_Done;

         if Read_Started then
            raise Constraint_Error with "Read already started.";
         end if;

         Read_Started := True;

         --  Keep the receiver off until the transmission is done in case a write just happened.
         loop
            exit when TMC_UART_Internal.ISR.TXFE and TMC_UART_Internal.ISR.TC;
         end loop;
         TMC_UART_Internal.CR1.RE := True;

         while Rx_Ready (TMC_UART) or TMC_UART_Internal.ISR.BUSY loop
            declare
               Junk : UInt9 := TMC_UART_Internal.RDR.RDR;
            begin
               Server_Communication.Transmit_String_Line
                 ("Unexpected data on TMC UART before read (" & Junk'Image & ").");
            end;
         end loop;

         --  STM32G474 has a 8 byte FIFO (Table 345, RM0440 Rev 8), so no need for DMA here.
         TMC_UART_Internal.CR1.TE := False;
         for Byte of Input loop
            Transmit (TMC_UART, UInt9 (Byte));
         end loop;

         Start_Transfer
           (This        => TMC_UART_DMA_RX_Controller,
            Stream      => TMC_UART_DMA_RX_Stream,
            Source      => Read_Data_Register_Address (TMC_UART),
            Destination => RX_Buffer'Address,
            Data_Count  => RX_Buffer'Length);
         TMC_UART_Internal.CR1.TE := True;

         Read_Start_Time := Clock;
      end Start_Read;

      function Read_Result_Ready return Boolean is
      begin
         return
           Read_Started
           and then
           (Status (TMC_UART_DMA_RX_Controller, TMC_UART_DMA_RX_Stream, Transfer_Complete_Indicated)
            or else Clock > Read_Start_Time + Milliseconds (100));
      end Read_Result_Ready;

      procedure Get_Read_Result (Output : out TMC2240_UART_Data_Byte_Array) is
      begin
         Init_Checker.Raise_If_Init_Not_Done;

         if not Read_Started then
            raise Constraint_Error with "No read started.";
         end if;

         Read_Started := False;

         declare
            Error : DMA_Error_Code;
         begin
            Poll_For_Completion
              (This           => TMC_UART_DMA_RX_Controller,
               Stream         => TMC_UART_DMA_RX_Stream,
               Expected_Level => Full_Transfer,
               Timeout        => Read_Start_Time + Milliseconds (100) - Clock,
               Result         => Error);

            if Error /= DMA_No_Error then
               Output := (others => 255);
               --  Put a known bad message on the output to indicate an error.
            end if;
         end;

         for I in 1 .. 8 loop
            Output (I) := TMC2240_UART_Byte (RX_Buffer (I + 4));
         end loop;
      end Get_Read_Result;

      procedure Write (Input : TMC2240_UART_Data_Byte_Array) is
      begin
         Init_Checker.Raise_If_Init_Not_Done;

         if Read_Started then
            raise Constraint_Error with "Can not write with pending read.";
         end if;

         while Rx_Ready (TMC_UART) or TMC_UART_Internal.ISR.BUSY loop
            declare
               Junk : UInt9 := TMC_UART_Internal.RDR.RDR;
            begin
               Server_Communication.Transmit_String_Line
                 ("Unexpected data on TMC UART before write (" & Junk'Image & ").");
            end;
         end loop;

         --  STM32G474 has a 8 byte FIFO (Table 345, RM0440 Rev 8), so no need for DMA here.
         TMC_UART_Internal.CR1.TE := False;
         TMC_UART_Internal.CR1.RE := False;
         for Byte of Input loop
            Transmit (TMC_UART, UInt9 (Byte));
         end loop;
         TMC_UART_Internal.CR1.TE := True;
      end Write;
   end UART_IO;

end Steppers;
