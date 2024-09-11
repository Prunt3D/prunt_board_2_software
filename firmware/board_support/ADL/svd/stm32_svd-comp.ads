pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.COMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype C1CSR_INMSEL_Field is HAL.UInt3;
   subtype C1CSR_HYST_Field is HAL.UInt3;
   subtype C1CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C1CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C1CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C1CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C1CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype C2CSR_INMSEL_Field is HAL.UInt3;
   subtype C2CSR_HYST_Field is HAL.UInt3;
   subtype C2CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C2CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C2CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C2CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C2CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype C3CSR_INMSEL_Field is HAL.UInt3;
   subtype C3CSR_HYST_Field is HAL.UInt3;
   subtype C3CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C3CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C3CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C3CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C3CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C3CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype C4CSR_INMSEL_Field is HAL.UInt3;
   subtype C4CSR_HYST_Field is HAL.UInt3;
   subtype C4CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C4CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C4CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C4CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C4CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C4CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype C5CSR_INMSEL_Field is HAL.UInt3;
   subtype C5CSR_HYST_Field is HAL.UInt3;
   subtype C5CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C5CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C5CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C5CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C5CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C5CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype C6CSR_INMSEL_Field is HAL.UInt3;
   subtype C6CSR_HYST_Field is HAL.UInt3;
   subtype C6CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C6CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C6CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C6CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C6CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C6CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype C7CSR_INMSEL_Field is HAL.UInt3;
   subtype C7CSR_HYST_Field is HAL.UInt3;
   subtype C7CSR_BLANKSEL_Field is HAL.UInt3;

   --  Comparator control/status register
   type C7CSR_Register is record
      --  EN
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  INMSEL
      INMSEL         : C7CSR_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  INPSEL
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_9_14  : HAL.UInt6 := 16#0#;
      --  POL
      POL            : Boolean := False;
      --  HYST
      HYST           : C7CSR_HYST_Field := 16#0#;
      --  BLANKSEL
      BLANKSEL       : C7CSR_BLANKSEL_Field := 16#0#;
      --  BRGEN
      BRGEN          : Boolean := False;
      --  SCALEN
      SCALEN         : Boolean := False;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  Read-only. VALUE
      VALUE          : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C7CSR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      INMSEL         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      INPSEL         at 0 range 8 .. 8;
      Reserved_9_14  at 0 range 9 .. 14;
      POL            at 0 range 15 .. 15;
      HYST           at 0 range 16 .. 18;
      BLANKSEL       at 0 range 19 .. 21;
      BRGEN          at 0 range 22 .. 22;
      SCALEN         at 0 range 23 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      VALUE          at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Comparator control and status register
   type COMP_Peripheral is record
      --  Comparator control/status register
      C1CSR : aliased C1CSR_Register;
      --  Comparator control/status register
      C2CSR : aliased C2CSR_Register;
      --  Comparator control/status register
      C3CSR : aliased C3CSR_Register;
      --  Comparator control/status register
      C4CSR : aliased C4CSR_Register;
      --  Comparator control/status register
      C5CSR : aliased C5CSR_Register;
      --  Comparator control/status register
      C6CSR : aliased C6CSR_Register;
      --  Comparator control/status register
      C7CSR : aliased C7CSR_Register;
   end record
     with Volatile;

   for COMP_Peripheral use record
      C1CSR at 16#0# range 0 .. 31;
      C2CSR at 16#4# range 0 .. 31;
      C3CSR at 16#8# range 0 .. 31;
      C4CSR at 16#C# range 0 .. 31;
      C5CSR at 16#10# range 0 .. 31;
      C6CSR at 16#14# range 0 .. 31;
      C7CSR at 16#18# range 0 .. 31;
   end record;

   --  Comparator control and status register
   COMP_Periph : aliased COMP_Peripheral
     with Import, Address => COMP_Base;

end STM32_SVD.COMP;
