with Ada.Streams; use Ada.Streams;
with GNAT.CRC32;
with Ada.Strings.Bounded;
with Ada.Characters.Latin_1;
with Embedded_Resources;

package body Communications is

   protected body TMC_IO is
      procedure Read (Message : TMC2240_UART_Query_Byte_Array; Reply : out TMC2240_UART_Data_Byte_Array) is
      begin
         pragma Assert (not TMC_Query_Waiting);
         pragma Assert (not TMC_Write_Waiting);
         pragma Assert (not TMC_Reply_Waiting);

         TMC_Query         := Message;
         TMC_Query_Waiting := True;

         loop
            exit when TMC_Reply_Waiting;
            delay 0.02;
         end loop;

         Reply             := TMC_Reply;
         TMC_Reply_Waiting := False;
      end Read;

      procedure Write (Message : TMC2240_UART_Data_Byte_Array) is
      begin
         pragma Assert (not TMC_Query_Waiting);
         pragma Assert (not TMC_Write_Waiting);
         pragma Assert (not TMC_Reply_Waiting);

         TMC_Write         := Message;
         TMC_Write_Waiting := True;

         loop
            exit when not TMC_Write_Waiting;
            delay 0.02;
         end loop;
      end Write;
   end TMC_IO;

   task body Runner is
      Port : GNAT.Serial_Communications.Serial_Port;

      Last_Received_Index : Message_Index := Message_Index'First;

      In_Safe_Stop_State : Boolean := True;

      package MCU_Log_Buffer_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 5_000);

      MCU_Log_Buffer : MCU_Log_Buffer_Strings.Bounded_String;

      procedure Flush_MCU_Log_Buffer is
         use MCU_Log_Buffer_Strings;
         LF : Character renames Ada.Characters.Latin_1.LF;
      begin
         if Length (MCU_Log_Buffer) /= 0 then
            Log ("BEGIN MESSAGE FROM MCU" & LF & To_String (MCU_Log_Buffer) & LF & "END MESSAGE FROM MCU");
            Set_Bounded_String (MCU_Log_Buffer, "");
         end if;
      end Flush_MCU_Log_Buffer;

      procedure Log_MCU_Character (C : Character) is
         use MCU_Log_Buffer_Strings;
      begin
         if Length (MCU_Log_Buffer) = MCU_Log_Buffer_Strings.Max_Length then
            Flush_MCU_Log_Buffer;
         end if;
         Append (MCU_Log_Buffer, C);
      end Log_MCU_Character;

      procedure Prepare_Message
        (Message       : in out Message_From_Server;
         Bytes_To_Send :        Stream_Element_Offset := Message_From_Server'Value_Size / 8)
      is
         Checksum      : GNAT.CRC32.CRC32;
         Message_Bytes : Stream_Element_Array (1 .. Message_From_Server_Content'Value_Size / 8) with
           Address => Message.Content'Address;
      begin
         Message.Content.Index := Last_Received_Index + 1;

         GNAT.CRC32.Initialize (Checksum);

         for I in 1 .. Bytes_To_Send - 4 loop
            if I in Message_Bytes'Range then
               GNAT.CRC32.Update (Checksum, Message_Bytes (I));
            else
               GNAT.CRC32.Update (Checksum, 0);
            end if;
         end loop;

         Message.Checksum := CRC32 (GNAT.CRC32.Get_Value (Checksum));
      end Prepare_Message;

      procedure Get_Reply (Message : aliased out Message_From_Client; Checksum_Good : out Boolean) is
         type Nibble is range 0 .. 2**4 - 1 with
           Size => 4;
         type Nibble_Array is array (1 .. Message_From_Client'Value_Size / 4) of Nibble with
           Pack;
         type Byte_Array is array (1 .. Message_From_Client'Value_Size / 8) of Character with
           Pack;

         Computed_Checksum : GNAT.CRC32.CRC32;
         Received          : Stream_Element_Array (1 .. Message_From_Client'Value_Size / 4);
         Message_Nibbles   : aliased Nibble_Array with
           Address => Message'Address;
         Message_Bytes     : aliased Byte_Array with
           Address => Message'Address;
         Last              : Stream_Element_Offset := 0;
         Last_Last         : Stream_Element_Offset := 0;
      begin
         pragma Assert (Stream_Element'Last = 255 and Stream_Element'Size = 8);

         Message_Nibbles := (others => 0);

         GNAT.CRC32.Initialize (Computed_Checksum);

         loop
            GNAT.Serial_Communications.Read (Port, Received (Last + 1 .. Received'Last), Last);

            if Last_Last = Last then
               raise UART_Timeout_Error with "MCU communication timed out.";
            end if;

            declare
               Bytes_Consumed : Stream_Element_Offset := 0;
            begin
               for I in Last_Last + 1 .. Last loop
                  if Received (I) = 253 then
                     raise Constraint_Error with "Got exception from MCU. Details logged to console. (2)";
                  elsif Received (I) = 254 then
                     Message_Nibbles (Integer (I - Bytes_Consumed) .. Message_Nibbles'Last) := (others => 0);
                     if Message.Content.Kind'Valid
                       and then
                       (Message.Content.Kind = Hello_Kind or Message.Content.Kind = Firmware_Update_Reply_Kind)
                       and then Message.Content.Client_Message_Length /= 0
                       and then Message.Content.Client_Message_Length = Message_Length (I - Bytes_Consumed)
                     then
                        --  This handles a future firmware version that has a smaller client message size.
                        Flush_MCU_Log_Buffer;
                        Checksum_Good := Message.Checksum = CRC32 (GNAT.CRC32.Get_Value (Computed_Checksum));
                        return;
                     else
                        raise Constraint_Error with "Message ended too early.";
                     end if;
                  elsif Received (I) < 128 then
                     Log_MCU_Character (Character'Val (Received (I)));
                     Bytes_Consumed := @ + 1;
                  else
                     --  TODO: Currently if we get an invalid byte here we just rely on the checksum being wrong later.
                     --  This is likely fine as we use a 32 bit CRC, but it is not ideal.
                     Message_Nibbles (Integer (I - Bytes_Consumed)) := Nibble (Received (I) mod 2**4);
                     if (I - Bytes_Consumed) mod 2 = 0 and I - Bytes_Consumed > 8 then
                        --  First 8 nibbles are checksum.
                        GNAT.CRC32.Update (Computed_Checksum, Message_Bytes (Integer (I - Bytes_Consumed) / 2));
                     end if;
                  end if;
               end loop;

               --  Note that we mess up the Received array here as we do not actually need it to hold the full message.
               Last := Last - Bytes_Consumed;
            end;

            exit when Last = Received'Last;
            Last_Last := Last;
         end loop;

         declare
            Byte                      : Stream_Element_Array (1 .. 1);
            Bytes_Read                : Stream_Element_Offset;
            Have_Nibble               : Boolean               := False;
            First_Nibble              : Stream_Element;
            Extra_Nibbles_To_Checksum : Stream_Element_Offset := 0;
         begin
            if Message.Content.Kind'Valid
              and then (Message.Content.Kind = Hello_Kind or Message.Content.Kind = Firmware_Update_Reply_Kind)
              and then Message.Content.Client_Message_Length /= 0
            then
               Extra_Nibbles_To_Checksum :=
                 Message_From_Client'Value_Size / 4 - Stream_Element_Offset (Message.Content.Client_Message_Length);
            end if;

            loop
               loop
                  GNAT.Serial_Communications.Read (Port, Byte, Bytes_Read);
                  exit when Bytes_Read = 1;
               end loop;
               exit when Byte (1) = 254;
               if Byte (1) = 253 then
                  raise Constraint_Error with "Got exception from MCU. Details logged to console. (3)";
               elsif Byte (1) >= 128 then
                  if Extra_Nibbles_To_Checksum > 0 then
                     Extra_Nibbles_To_Checksum := @ - 1;
                     if Have_Nibble then
                        GNAT.CRC32.Update (Computed_Checksum, First_Nibble * 16 + Byte (1) mod 2**4);
                        Have_Nibble := False;
                     else
                        First_Nibble := Byte (1) mod 2**4;
                        Have_Nibble  := True;
                     end if;
                  else
                     raise Constraint_Error with "Expected ASCII character but got " & Byte (1)'Image;
                  end if;
               else
                  Log_MCU_Character (Character'Val (Byte (1)));
               end if;
            end loop;

            Flush_MCU_Log_Buffer;

            if Extra_Nibbles_To_Checksum /= 0 then
               Checksum_Good := False;
            else
               Checksum_Good := Message.Checksum = CRC32 (GNAT.CRC32.Get_Value (Computed_Checksum));
            end if;
         end;
      exception
         when others =>
            Flush_MCU_Log_Buffer;
            raise;
      end Get_Reply;

      procedure Send_And_Handle_Reply
        (Message       : aliased in out Message_From_Server;
         Reply         : aliased    out Message_From_Client;
         Bytes_To_Send :                Stream_Element_Offset := Message_From_Server'Value_Size / 8)
      is
         Message_Bytes : Stream_Element_Array (1 .. Message_From_Server'Value_Size / 8) with
           Address => Message'Address;

         Reply_Checksum_Good : Boolean;
      begin
         pragma Assert (Message_Bytes'Size = Message'Size);

         if Message.Content.Kind not in
             Firmware_Update_Start_Kind | Firmware_Update_Data_Kind | Firmware_Update_Done_Kind
         then
            if Message.Content.TMC_Read_Data /= TMC2240_UART_Query_Byte_Array'(others => 0) or
              Message.Content.TMC_Write_Data /= TMC2240_UART_Data_Byte_Array'(others => 0)
            then
               raise Constraint_Error with "TMC data can not be manually placed in a server message.";
            end if;
         end if;

         if TMC_Query_Waiting then
            Message.Content.TMC_Read_Data := TMC_Query;
            TMC_Query_Waiting             := False;
         elsif TMC_Write_Waiting then
            Message.Content.TMC_Write_Data := TMC_Write;
            TMC_Write_Waiting              := False;
         end if;

         if Message.Content.Kind in Regular_Step_Delta_List_Kind | Looping_Step_Delta_List_Kind then
            In_Safe_Stop_State := Message.Content.Safe_Stop_After = True;
         end if;

         Prepare_Message (Message, Bytes_To_Send);

         GNAT.Serial_Communications.Write
           (Port, Message_Bytes (1 .. Stream_Element_Offset'Min (Message_From_Server'Value_Size / 8, Bytes_To_Send)));
         for I in 1 .. Bytes_To_Send - Message_From_Server'Value_Size / 8 loop
            GNAT.Serial_Communications.Write (Port, (1 => 0));
         end loop;

         loop
            Get_Reply (Reply, Reply_Checksum_Good);
            if Reply_Checksum_Good then
               exit when Reply.Content.Index = Last_Received_Index + 1;
               exit when Reply.Content.Index = 0 and Message.Content.Kind = Firmware_Update_Done_Kind and
                 Reply.Content.Kind = Hello_Kind;
               if Reply.Content.Index /= Last_Received_Index then
                  raise Constraint_Error
                    with "Bad message index received, expected " & Last_Received_Index'Image &
                    " or successor but got " & Reply.Content.Index'Image & ".";
               end if;

               if Message.Content.Index /= Message_Index'First + 1 then
                  Log ("MCU indicated CRC error. Resending message.");
                  GNAT.Serial_Communications.Write (Port, Message_Bytes);
                  --  We do not do resends for the first message as we can have multiple messages stuck in the buffer.
                  --  Resending here in that case would mean that every future message would end up with multiple
                  --  buffered replies. This means that the initial connection could time out if the message is corrupt
                  --  when the MCU receives it, but that is fine since nothing has happened at that point so the user
                  --  can just restart the server.
               end if;
            else
               Log ("Bad CRC received. Resending message.");
               GNAT.Serial_Communications.Write (Port, Message_Bytes);
            end if;
         end loop;

         if Reply.Content.Kind /= Hello_Kind and Reply.Content.Kind /= Firmware_Update_Reply_Kind then
            for T in Thermistor_Name loop
               Report_Temperature (T, Reply.Content.Temperatures (T));
            end loop;

            for H in Heater_Name loop
               Report_Heater_Power (H, Reply.Content.Heaters (H));
            end loop;

            for S in Input_Switch_Name loop
               Report_Input_Switch_State (S, Reply.Content.Switches (S));
            end loop;

            if Reply.Content.TMC_Data (1) /= 0 then
               pragma Assert (not TMC_Reply_Waiting);
               TMC_Reply         := Reply.Content.TMC_Data;
               TMC_Reply_Waiting := True;
            end if;

            --  for F in Fan_Name loop
            --     Log (F'Image & Reply.Content.Tachs (F)'Image);
            --  end loop;
         end if;

         Last_Received_Index := Reply.Content.Index;
      end Send_And_Handle_Reply;

   begin
      accept Init (Port_Name : GNAT.Serial_Communications.Port_Name) do
         GNAT.Serial_Communications.Open (Port, Port_Name);
         GNAT.Serial_Communications.Set
           (Port      => Port,
            Rate      => GNAT.Serial_Communications.B75,
            Bits      => GNAT.Serial_Communications.CS8,
            Stop_Bits => GNAT.Serial_Communications.One,
            Parity    => GNAT.Serial_Communications.None,
            Block     => False,
            Local     => True,
            Flow      => GNAT.Serial_Communications.None,
            Timeout   => 10.0);
         --  Requesting a baud rate of 75 actually sets the board to 6M.

         declare
            Already_Tried_Update : Boolean := False;
         begin
            loop
               declare
                  Byte       : Stream_Element_Array (1 .. 1);
                  Bytes_Read : Stream_Element_Offset;
               begin
                  loop
                     loop
                        GNAT.Serial_Communications.Read (Port, Byte, Bytes_Read);
                        exit when Bytes_Read = 1;
                     end loop;
                     exit when Byte (1) = 254;
                     if Byte (1) = 253 then
                        raise Constraint_Error with "Got exception from MCU. Details logged to console. (1)";
                     elsif Byte (1) < 128 then
                        Log_MCU_Character (Character'Val (Byte (1)));
                     end if;
                  end loop;

                  Flush_MCU_Log_Buffer;
               end;

               declare
                  Received_Message : aliased Message_From_Client;

                  Received_Checksum_Good : Boolean;
               begin
                  loop
                     Get_Reply (Received_Message, Received_Checksum_Good);
                     exit when Received_Checksum_Good;
                  end loop;

                  if Received_Message.Content.ID /= (16#14BC_80C3#, 16#53B1_4CAC#, 16#DE61_09E7#, 16#6BC8_2ECD#) then
                     raise Constraint_Error with "The connected board does not appear to be a Prunt Board 2.";
                  end if;

                  if Received_Message.Content.Kind /= Hello_Kind then
                     raise Constraint_Error
                       with "Expected hello message from MCU but got " & Received_Message.Content.Kind'Image;
                  end if;

                  Log ("Firmware version " & Received_Message.Content.Version'Image & ".");

                  exit when Received_Message.Content.Version = 1;

                  Log ("Firmware version 1 required.");

                  if Already_Tried_Update then
                     raise Constraint_Error with "Board firmware update failed.";
                  else
                     Already_Tried_Update := True;
                     Prompt_For_Update;

                     declare
                        Bytes_To_Send   : constant Stream_Element_Offset                :=
                          Stream_Element_Offset (Received_Message.Content.Server_Message_Length);
                        Message_To_Send : aliased Message_From_Server;
                        Firmware        : constant access constant Stream_Element_Array :=
                          Embedded_Resources.Get_Content ("prunt_board_2_firmware_with_crc.bin");
                        Final_Offset    : constant Firmware_Data_Offset                 :=
                          Firmware_Data_Offset
                            ((Firmware.all'Length + Firmware_Data_Array'Length - 1) / Firmware_Data_Array'Length);
                     begin
                        if Bytes_To_Send < 1_048 then
                           raise Constraint_Error with "Server message size too small to send firmware updates.";
                        end if;

                        Log ("Starting firmware update.");
                        Message_To_Send.Content :=
                          (Kind  => Firmware_Update_Start_Kind,
                           Index => 0,
                           ID    =>
                             DO_NOT_COPY_THIS_CLIENT_ID_AS_IT_IS_MEANT_TO_IDENTIFY_THIS_PARTICULAR_BOARD_MODEL_AND_FIRMWARE);
                        Send_And_Handle_Reply (Message_To_Send, Received_Message, Bytes_To_Send);

                        for I in 0 .. Final_Offset loop
                           Log
                             ("Sending update part " & Positive (I + 1)'Image & " of " &
                              Positive (Final_Offset + 1)'Image & ".");
                           Message_To_Send.Content :=
                             (Kind            => Firmware_Update_Data_Kind,
                              Index           => 0,
                              Firmware_Offset => I,
                              Firmware_Data   => (others => 16#FF#));
                           for J in Stream_Element_Offset range 0 .. Firmware_Data_Array'Length - 1 loop
                              declare
                                 Byte_Index : constant Stream_Element_Offset :=
                                   Stream_Element_Offset (I) * Firmware_Data_Array'Length + J + Firmware.all'First;
                              begin
                                 if Byte_Index in Firmware.all'Range then
                                    Message_To_Send.Content.Firmware_Data (Integer (J + 1)) :=
                                      Firmware_Byte (Firmware.all (Byte_Index));
                                 end if;
                              end;
                           end loop;
                           Send_And_Handle_Reply (Message_To_Send, Received_Message, Bytes_To_Send);
                        end loop;

                        Log ("Ending firmware update.");
                        Message_To_Send.Content := (Kind => Firmware_Update_Done_Kind, Index => 0);
                        Send_And_Handle_Reply (Message_To_Send, Received_Message, Bytes_To_Send);
                     end;
                  end if;
               end;
            end loop;
         exception
            when others =>
               Flush_MCU_Log_Buffer;
               raise;
         end;
      end Init;

      loop
         declare
            Message_To_Send  : aliased Message_From_Server;
            Received_Message : aliased Message_From_Client;
         begin
            select
               accept Send_Message (Content : Message_From_Server_Content) do
                  Message_To_Send.Content := Content;
               end Send_Message;

               Send_And_Handle_Reply (Message_To_Send, Received_Message);
            or
               accept Send_Message_And_Wait_For_Reply
                 (Content : Message_From_Server_Content; Reply : out Message_From_Client_Content)
               do
                  Message_To_Send.Content := Content;
                  Send_And_Handle_Reply (Message_To_Send, Received_Message);
                  Reply := Received_Message.Content;
               end Send_Message_And_Wait_For_Reply;
            or when Last_Received_Index > Message_Index'First and In_Safe_Stop_State =>
               delay 0.05;
               Message_To_Send.Content :=
                 (Kind => Status_Kind, Index => <>, TMC_Write_Data => (others => 0), TMC_Read_Data => (others => 0));
               Send_And_Handle_Reply (Message_To_Send, Received_Message);
               --  Send a status message after a timeout, but only if setup is done and we are in a safe stop state.
            end select;
         end;
      end loop;
   exception
      when E : others =>
         Report_Error (E);
         raise;
   end Runner;

end Communications;
