pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.CRC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  IDR array element
   subtype IDR_Element is HAL.UInt8;

   --  IDR array
   type IDR_Field_Array is array (1 .. 4) of IDR_Element
     with Component_Size => 8, Size => 32;

   --  Independent data register
   type IDR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IDR as a value
            Val : HAL.UInt32;
         when True =>
            --  IDR as an array
            Arr : IDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CR_POLYSIZE_Field is HAL.UInt2;
   subtype CR_REV_IN_Field is HAL.UInt2;

   --  Control register
   type CR_Register is record
      --  Write-only. RESET bit
      RESET         : Boolean := False;
      --  unspecified
      Reserved_1_2  : HAL.UInt2 := 16#0#;
      --  Polynomial size
      POLYSIZE      : CR_POLYSIZE_Field := 16#0#;
      --  Reverse input data
      REV_IN        : CR_REV_IN_Field := 16#0#;
      --  Reverse output data
      REV_OUT       : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      RESET         at 0 range 0 .. 0;
      Reserved_1_2  at 0 range 1 .. 2;
      POLYSIZE      at 0 range 3 .. 4;
      REV_IN        at 0 range 5 .. 6;
      REV_OUT       at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Cyclic redundancy check calculation unit
   type CRC_Peripheral is record
      --  Data register
      DR   : aliased HAL.UInt32;
      --  Independent data register
      IDR  : aliased IDR_Register;
      --  Control register
      CR   : aliased CR_Register;
      --  Initial CRC value
      INIT : aliased HAL.UInt32;
      --  polynomial
      POL  : aliased HAL.UInt32;
   end record
     with Volatile;

   for CRC_Peripheral use record
      DR   at 16#0# range 0 .. 31;
      IDR  at 16#4# range 0 .. 31;
      CR   at 16#8# range 0 .. 31;
      INIT at 16#10# range 0 .. 31;
      POL  at 16#14# range 0 .. 31;
   end record;

   --  Cyclic redundancy check calculation unit
   CRC_Periph : aliased CRC_Peripheral
     with Import, Address => CRC_Base;

end STM32_SVD.CRC;
