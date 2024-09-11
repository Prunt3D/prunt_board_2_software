with Ada.Streams; use Ada.Streams;
with Ada.Text_IO;
with GNAT.CRC32;

package body Communications is

   task body Runner is
      Port : GNAT.Serial_Communications.Serial_Port;

      Last_Received_Index : Message_Index := Message_Index'First;

      procedure Prepare_Message (Message : in out Message_From_Server) is
         type Byte_Array is array (1 .. Message_From_Server_Content'Object_Size / 8) of Character with
           Pack;

         Checksum      : GNAT.CRC32.CRC32;
         Message_Bytes : Byte_Array with
           Address => Message.Content'Address;
      begin
         Message.Content.Index := Last_Received_Index + 1;

         GNAT.CRC32.Initialize (Checksum);

         for C of Message_Bytes loop
            GNAT.CRC32.Update (Checksum, C);
         end loop;

         Message.Checksum := CRC32 (GNAT.CRC32.Get_Value (Checksum));
      end Prepare_Message;

      procedure Get_Reply (Message : aliased out Message_From_Client) is
         type Nibble is range 0 .. 2**4 - 1 with
           Size => 4;
         type Nibble_Array is array (1 .. Message_From_Client'Object_Size / 4) of Nibble with
           Pack;

         Received           : Stream_Element_Array (1 .. Message_From_Client'Object_Size / 4);
         Message_Nibbles    : Nibble_Array with
           Address => Message'Address;
         Last               : Stream_Element_Offset := 0;
         Last_Last          : Stream_Element_Offset := 0;
         Is_First_Printable : Boolean               := True;

         procedure Put_Char (Byte : Stream_Element) is
         begin
            if Is_First_Printable then
               Ada.Text_IO.Put_Line ("BEGIN MESSAGE FROM MCU");
               Is_First_Printable := False;
            end if;
            Ada.Text_IO.Put ("" & Character'Val (Byte));
         end Put_Char;
      begin
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
                     raise Constraint_Error with "Message ended too early.";
                  elsif Received (I) < 128 then
                     Put_Char (Received (I));
                     Bytes_Consumed := @ + 1;
                  else
                     --  TODO: Currently if we get an invalid byte here we just rely on the checksum being wrong later.
                     --  This is likely fine as we use a 32 bit CRC, but it is not ideal.
                     Message_Nibbles (Integer (I - Bytes_Consumed)) := Nibble (Received (I) mod 2**4);
                  end if;
               end loop;

               --  Note that we mess up the Received array here as we do not actually need it to hold the full message.
               Last := Last - Bytes_Consumed;
            end;

            exit when Last = Received'Last;
            Last_Last := Last;
         end loop;

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
                  raise Constraint_Error with "Got exception from MCU. Details logged to console. (3)";
               elsif Byte (1) >= 128 then
                  raise Constraint_Error with "Expected ASCII character but got " & Byte'Image;
               end if;
               Put_Char (Byte (1));
            end loop;
         end;

         if not Is_First_Printable then
            Ada.Text_IO.Put_Line ("END MESSAGE FROM MCU");
         end if;
      end Get_Reply;

      function Checksum_Is_Good (Message : aliased Message_From_Client) return Boolean is
         type Byte_Array is array (1 .. Message_From_Client_Content'Object_Size / 8) of Character with
           Pack;

         Computed_Checksum : GNAT.CRC32.CRC32;
         Message_Bytes     : Byte_Array with
           Address => Message.Content'Address;
      begin
         GNAT.CRC32.Initialize (Computed_Checksum);

         for C of Message_Bytes loop
            GNAT.CRC32.Update (Computed_Checksum, C);
         end loop;

         return CRC32 (GNAT.CRC32.Get_Value (Computed_Checksum)) = Message.Checksum;
      end Checksum_Is_Good;

      procedure Send_And_Handle_Reply
        (Message : aliased in out Message_From_Server; Reply : aliased out Message_From_Client)
      is
         Message_Bytes : Stream_Element_Array (1 .. Message_From_Server'Object_Size / 8) with
           Address => Message'Address;
      begin
         pragma Assert (Message_Bytes'Size = Message'Size);

         Prepare_Message (Message);

         loop
            GNAT.Serial_Communications.Write (Port, Message_Bytes);
            --  Ada.Text_IO.Put_Line ("Sent " & Message.Content.Kind'Image);
            Get_Reply (Reply);
            --  Ada.Text_IO.Put_Line ("Received " & Reply.Content.Kind'Image);
            if Checksum_Is_Good (Reply) then
               exit when Reply.Content.Index = Last_Received_Index + 1;
               if Reply.Content.Index /= Last_Received_Index then
                  Ada.Text_IO.Put_Line (Reply'Image);
                  raise Constraint_Error
                    with "Bad message index received, expected " & Last_Received_Index'Image &
                    " or successor but got " & Reply.Content.Index'Image & ". Message logged to console.";
               end if;
            end if;
         end loop;

         for T in Thermistor_Name loop
            Report_Temperature (T, Reply.Content.Temperatures (T));
         end loop;

         for H in Heater_Name loop
            Report_Heater_Power (H, Reply.Content.Heaters (H));
         end loop;

         for S in Input_Switch_Name loop
            Report_Input_Switch_State (S, Reply.Content.Switches (S));
         end loop;

         Last_Received_Index := @ + 1;
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
            Byte               : Stream_Element_Array (1 .. 1);
            Bytes_Read         : Stream_Element_Offset;
            Is_First_Printable : Boolean := True;

            procedure Put_Char (Byte : Stream_Element) is
            begin
               if Is_First_Printable then
                  Ada.Text_IO.Put_Line ("BEGIN MESSAGE FROM MCU");
                  Is_First_Printable := False;
               end if;
               Ada.Text_IO.Put ("" & Character'Val (Byte));
            end Put_Char;
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
                  Put_Char (Byte (1));
               end if;
            end loop;

            if not Is_First_Printable then
               Ada.Text_IO.Put_Line ("END MESSAGE FROM MCU");
            end if;
         end;

         declare
            Received_Message : aliased Message_From_Client;
         begin
            loop
               Get_Reply (Received_Message);
               exit when Checksum_Is_Good (Received_Message);
            end loop;

            if Received_Message.Content.Kind /= Hello_Kind then
               raise Constraint_Error
                 with "Expected hello message from MCU but got " & Received_Message.Content.Kind'Image;
            end if;

            if Received_Message.Content.ID /= (16#14BC_80C3#, 16#53B1_4CAC#, 16#DE61_09E7#, 16#6BC8_2ECD#) then
               raise Constraint_Error with "The connected board does not appear to be a Prunt Board 1.";
            end if;

            if Received_Message.Content.Version /= 1 then
               raise Constraint_Error
                 with "Expected protocol version 1 but got " & Received_Message.Content.Version'Image &
                 ". Update the server to the latest version.";
            end if;
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
            or
               delay 0.2;
               --  Send a status message after a timeout, but only if setup is done.
               if Last_Received_Index > 0 then
                  Message_To_Send.Content := (Kind => Status_Kind, Index => <>);
                  Send_And_Handle_Reply (Message_To_Send, Received_Message);
               end if;
            end select;
         end;
      end loop;
   exception
      when E : others =>
         Report_Error (E);
         raise;
   end Runner;

end Communications;
