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

   File_Data     : access Ada.Streams.Stream_Element_Array;
   Stream_Length : Count;
   Input, Output : Ada.Streams.Stream_IO.File_Type;
   Bytes_Read    : Ada.Streams.Stream_Element_Offset;
begin
   if Argument_Count /= 2 then
      raise Constraint_Error with "Usage: " & Command_Name & " <input_file> <output_file>";
   end if;

   Open (Input, In_File, Argument (1));

   Stream_Length := Size (Input);

   if Stream_Length > 255 * 1_024 then
      raise Constraint_Error with "File too big.";
   end if;

   if Stream_Length mod 4 /= 0 then
      raise Constraint_Error with
        "File size must be multiple of 4. "
        & "Alternatively this program may be modified to add 0xFF padding to the end of the input file).";
   end if;

   File_Data := new Stream_Element_Array (1 .. Stream_Element_Offset (Stream_Length));
   Read (Input, File_Data.all, Bytes_Read);

   if Count (Bytes_Read) /= Stream_Length then
      raise Constraint_Error with "Did not read whole file.";
   end if;

   Close (Input);

   Create (Output, Out_File, Argument (2));
   Write (Output, File_Data.all);
   Write (Output, To_Stream_Element_Array (Unsigned_32 (Stream_Length / 4) + 1));
   Write
     (Output,
      To_Stream_Element_Array
        (Calculate_CRC32 (File_Data.all & To_Stream_Element_Array (Unsigned_32 (Stream_Length / 4) + 1))));

   if Stream_Length mod 8 /= 0 then
      --  The stm32g4 only allows writing of double words and stm32flash fills these with 0 instead of FF.
      Write (Output, To_Stream_Element_Array (16#FFFF_FFFF#));
   end if;

   Close (Output);
end Firmware_Patcher;
