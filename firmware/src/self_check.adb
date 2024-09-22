with System;
with HAL;                    use HAL;
with Hardware_Configuration; use Hardware_Configuration;
with STM32.CRC;              use STM32.CRC;
with STM32.Device;           use STM32.Device;

package body Self_Check is

   type Flash_Bank is array (1 .. 2**16) of UInt32 with
     Component_Size => 32, Volatile, Volatile_Components;

   function Bank_Is_Valid (Bank : Flash_Bank) return Boolean is
      Computed_Checksum : UInt32;
   begin
      Enable_Clock (Self_Check_CRC_Unit);
      Reset_CRC (Self_Check_CRC_Unit);
      Set_Data_Input_Order (Self_Check_CRC_Unit, Word_Reversed);
      Set_Data_Output_Order (Self_Check_CRC_Unit, Bit_Reversed);

      for I in reverse Bank'First .. Bank'Last - 1 loop
         if Bank (I + 1) /= 16#FFFF_FFFF# then
            if I + 1 = Bank'Last then
               raise Constraint_Error with "There should never be data here.";
            end if;

            if Bank (I) /= UInt32 (I) then
               return False;
            end if;

            for X of Bank (Bank'First .. I) loop
               Update_CRC (Self_Check_CRC_Unit, X, Computed_Checksum);
            end loop;

            return (Computed_Checksum xor 16#FFFF_FFFF#) = Bank (I + 1);
         end if;
      end loop;

      return False;
   end Bank_Is_Valid;

   function Current_Bank_Is_Valid return Boolean is
      Bank : Flash_Bank with
        Address => System'To_Address (16#0800_0000#);
   begin
      return Bank_Is_Valid (Bank);
   end Current_Bank_Is_Valid;

   function Other_Bank_Is_Valid return Boolean is
      Bank : Flash_Bank with
        Address => System'To_Address (16#0804_0000#);
   begin
      return Bank_Is_Valid (Bank);
   end Other_Bank_Is_Valid;

end Self_Check;
