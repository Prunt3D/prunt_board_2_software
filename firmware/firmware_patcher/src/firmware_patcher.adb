with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with GNAT.CRC32;            use GNAT.CRC32;
with Ada.Streams;           use Ada.Streams;
with Interfaces;            use Interfaces;

procedure Firmware_Patcher is
   function Calculate_CRC32 (File_Data : Stream_Element_Array) return Unsigned_32 is
      Checksum : CRC32;
   begin
      Initialize (Checksum);
      for Element of File_Data loop
         Update (Checksum, Element);
      end loop;
      return Get_Value (Checksum);
   end Calculate_CRC32;

   function To_Stream_Element_Array (Value : Interfaces.Unsigned_32) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. 4);
   begin
      Result (1) := Stream_Element (Value mod 2**8);
      Result (2) := Stream_Element (Value / 2**8 mod 2**8);
      Result (3) := Stream_Element (Value / 2**16 mod 2**8);
      Result (4) := Stream_Element (Value / 2**24 mod 2**8);

      return Result;
   end To_Stream_Element_Array;

   Prunt_File_Data, Kalico_File_Data, Out_File_Data : access Ada.Streams.Stream_Element_Array;
   Prunt_Stream_Length, Kalico_Stream_Length        : Count;
   Prunt_Input, Kalico_Input, Output                : Ada.Streams.Stream_IO.File_Type;
   Bytes_Read                                       : Ada.Streams.Stream_Element_Offset;
begin
   if Argument_Count /= 3 then
      raise Constraint_Error with "Usage: " & Command_Name & " <prunt_binary> <kalico_binary> <output_file>";
   end if;

   Open (Prunt_Input, In_File, Argument (1));
   Open (Kalico_Input, In_File, Argument (2));

   Prunt_Stream_Length  := Size (Prunt_Input);
   Kalico_Stream_Length := Size (Kalico_Input);

   if Prunt_Stream_Length > 200 * 1_024 then
      raise Constraint_Error with "Prunt binary too big.";
   end if;

   if Kalico_Stream_Length > 55 * 1_024 then
      raise Constraint_Error with "Kalico binary too big.";
   end if;

   if Kalico_Stream_Length mod 4 /= 0 then
      raise Constraint_Error
        with "Kalico file size must be multiple of 4. " &
        "Alternatively this program may be modified to add 0xFF padding to the end of the input file.";
   end if;

   Prunt_File_Data := new Stream_Element_Array (1 .. Stream_Element_Offset (Prunt_Stream_Length));
   Read (Prunt_Input, Prunt_File_Data.all, Bytes_Read);
   if Count (Bytes_Read) /= Prunt_Stream_Length then
      raise Constraint_Error with "Did not read whole Prunt file.";
   end if;
   Close (Prunt_Input);

   Kalico_File_Data := new Stream_Element_Array (1 .. Stream_Element_Offset (Kalico_Stream_Length));
   Read (Kalico_Input, Kalico_File_Data.all, Bytes_Read);
   if Count (Bytes_Read) /= Kalico_Stream_Length then
      raise Constraint_Error with "Did not read whole Kalico file.";
   end if;
   Close (Kalico_Input);

   Create (Output, Out_File, Argument (3));
   Write (Output, Prunt_File_Data.all);

   for I in Prunt_Stream_Length + 1 .. 200 * 1_024 loop
      Write (Output, Stream_Element_Array'(1 => 16#FF#));
   end loop;

   Write (Output, Kalico_File_Data.all);

   Close (Output);
   Open (Output, In_File, Argument (3));
   Out_File_Data := new Stream_Element_Array (1 .. Stream_Element_Offset (Kalico_Stream_Length + 200 * 1_024));
   Read (Output, Out_File_Data.all, Bytes_Read);
   if Count (Bytes_Read) /= Kalico_Stream_Length + 200 * 1_024 then
      raise Constraint_Error with "Did not read whole output file.";
   end if;
   Close (Output);

   Open (Output, Append_File, Argument (3));

   Write (Output, To_Stream_Element_Array (Unsigned_32 ((Kalico_Stream_Length + 200 * 1_024) / 4) + 1));
   Write
     (Output,
      To_Stream_Element_Array
        (Calculate_CRC32
           (Out_File_Data.all &
            To_Stream_Element_Array (Unsigned_32 ((Kalico_Stream_Length + 200 * 1_024) / 4) + 1))));

   if Kalico_Stream_Length mod 8 /= 0 then
      --  The stm32g4 only allows writing of double words and stm32flash fills these with 0 instead of FF.
      Write (Output, To_Stream_Element_Array (16#FFFF_FFFF#));
   end if;

   Close (Output);
end Firmware_Patcher;
