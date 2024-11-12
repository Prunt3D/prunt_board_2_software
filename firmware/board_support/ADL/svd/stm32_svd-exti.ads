pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.EXTI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  IMR1_IM array
   type IMR1_IM_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Interrupt mask register
   type IMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt32;
         when True =>
            --  IM as an array
            Arr : IMR1_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  EMR1_EM array
   type EMR1_EM_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Event mask register
   type EMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt32;
         when True =>
            --  EM as an array
            Arr : EMR1_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  RTSR1_RT array
   type RTSR1_RT_Field_Array is array (0 .. 17) of Boolean
     with Component_Size => 1, Size => 18;

   --  Type definition for RTSR1_RT
   type RTSR1_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt18;
         when True =>
            --  RT as an array
            Arr : RTSR1_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 18;

   for RTSR1_RT_Field use record
      Val at 0 range 0 .. 17;
      Arr at 0 range 0 .. 17;
   end record;

   --  RTSR1_RT array
   type RTSR1_RT_Field_Array_1 is array (19 .. 22) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RTSR1_RT
   type RTSR1_RT_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt4;
         when True =>
            --  RT as an array
            Arr : RTSR1_RT_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RTSR1_RT_Field_1 use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RTSR1_RT array
   type RTSR1_RT_Field_Array_2 is array (29 .. 31) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RTSR1_RT
   type RTSR1_RT_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt3;
         when True =>
            --  RT as an array
            Arr : RTSR1_RT_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RTSR1_RT_Field_2 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Rising Trigger selection register
   type RTSR1_Register is record
      --  Rising trigger event configuration of line 0
      RT             : RTSR1_RT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Rising trigger event configuration of line 19
      RT_1           : RTSR1_RT_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_28 : HAL.UInt6 := 16#0#;
      --  Rising trigger event configuration of line 29
      RT_2           : RTSR1_RT_Field_2 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR1_Register use record
      RT             at 0 range 0 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      RT_1           at 0 range 19 .. 22;
      Reserved_23_28 at 0 range 23 .. 28;
      RT_2           at 0 range 29 .. 31;
   end record;

   --  FTSR1_FT array
   type FTSR1_FT_Field_Array is array (0 .. 17) of Boolean
     with Component_Size => 1, Size => 18;

   --  Type definition for FTSR1_FT
   type FTSR1_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt18;
         when True =>
            --  FT as an array
            Arr : FTSR1_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 18;

   for FTSR1_FT_Field use record
      Val at 0 range 0 .. 17;
      Arr at 0 range 0 .. 17;
   end record;

   --  FTSR1_FT array
   type FTSR1_FT_Field_Array_1 is array (19 .. 22) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for FTSR1_FT
   type FTSR1_FT_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt4;
         when True =>
            --  FT as an array
            Arr : FTSR1_FT_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for FTSR1_FT_Field_1 use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  FTSR1_FT array
   type FTSR1_FT_Field_Array_2 is array (29 .. 31) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for FTSR1_FT
   type FTSR1_FT_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt3;
         when True =>
            --  FT as an array
            Arr : FTSR1_FT_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for FTSR1_FT_Field_2 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Falling Trigger selection register
   type FTSR1_Register is record
      --  Falling trigger event configuration of line 0
      FT             : FTSR1_FT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Falling trigger event configuration of line 19
      FT_1           : FTSR1_FT_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_28 : HAL.UInt6 := 16#0#;
      --  Falling trigger event configuration of line 29
      FT_2           : FTSR1_FT_Field_2 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR1_Register use record
      FT             at 0 range 0 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      FT_1           at 0 range 19 .. 22;
      Reserved_23_28 at 0 range 23 .. 28;
      FT_2           at 0 range 29 .. 31;
   end record;

   --  SWIER1_SWI array
   type SWIER1_SWI_Field_Array is array (0 .. 17) of Boolean
     with Component_Size => 1, Size => 18;

   --  Type definition for SWIER1_SWI
   type SWIER1_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt18;
         when True =>
            --  SWI as an array
            Arr : SWIER1_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 18;

   for SWIER1_SWI_Field use record
      Val at 0 range 0 .. 17;
      Arr at 0 range 0 .. 17;
   end record;

   --  SWIER1_SWI array
   type SWIER1_SWI_Field_Array_1 is array (19 .. 22) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SWIER1_SWI
   type SWIER1_SWI_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt4;
         when True =>
            --  SWI as an array
            Arr : SWIER1_SWI_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SWIER1_SWI_Field_1 use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SWIER1_SWI array
   type SWIER1_SWI_Field_Array_2 is array (29 .. 31) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for SWIER1_SWI
   type SWIER1_SWI_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt3;
         when True =>
            --  SWI as an array
            Arr : SWIER1_SWI_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for SWIER1_SWI_Field_2 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Software interrupt event register
   type SWIER1_Register is record
      --  Software Interrupt on line 0
      SWI            : SWIER1_SWI_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Software Interrupt on line 19
      SWI_1          : SWIER1_SWI_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_28 : HAL.UInt6 := 16#0#;
      --  Software Interrupt on line 29
      SWI_2          : SWIER1_SWI_Field_2 :=
                        (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER1_Register use record
      SWI            at 0 range 0 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      SWI_1          at 0 range 19 .. 22;
      Reserved_23_28 at 0 range 23 .. 28;
      SWI_2          at 0 range 29 .. 31;
   end record;

   --  PR1_PIF array
   type PR1_PIF_Field_Array is array (0 .. 17) of Boolean
     with Component_Size => 1, Size => 18;

   --  Type definition for PR1_PIF
   type PR1_PIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIF as a value
            Val : HAL.UInt18;
         when True =>
            --  PIF as an array
            Arr : PR1_PIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 18;

   for PR1_PIF_Field use record
      Val at 0 range 0 .. 17;
      Arr at 0 range 0 .. 17;
   end record;

   --  PR1_PIF array
   type PR1_PIF_Field_Array_1 is array (19 .. 22) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PR1_PIF
   type PR1_PIF_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIF as a value
            Val : HAL.UInt4;
         when True =>
            --  PIF as an array
            Arr : PR1_PIF_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PR1_PIF_Field_1 use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PR1_PIF array
   type PR1_PIF_Field_Array_2 is array (29 .. 31) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for PR1_PIF
   type PR1_PIF_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIF as a value
            Val : HAL.UInt3;
         when True =>
            --  PIF as an array
            Arr : PR1_PIF_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for PR1_PIF_Field_2 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Pending register
   type PR1_Register is record
      --  Pending bit 0
      PIF            : PR1_PIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  Pending bit 19
      PIF_1          : PR1_PIF_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_28 : HAL.UInt6 := 16#0#;
      --  Pending bit 29
      PIF_2          : PR1_PIF_Field_2 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR1_Register use record
      PIF            at 0 range 0 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      PIF_1          at 0 range 19 .. 22;
      Reserved_23_28 at 0 range 23 .. 28;
      PIF_2          at 0 range 29 .. 31;
   end record;

   --  IMR2_IM array
   type IMR2_IM_Field_Array is array (32 .. 37) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for IMR2_IM
   type IMR2_IM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt6;
         when True =>
            --  IM as an array
            Arr : IMR2_IM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for IMR2_IM_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  IMR2_IM array
   type IMR2_IM_Field_Array_1 is array (40 .. 43) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for IMR2_IM
   type IMR2_IM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IM as a value
            Val : HAL.UInt4;
         when True =>
            --  IM as an array
            Arr : IMR2_IM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for IMR2_IM_Field_1 use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Interrupt mask register
   type IMR2_Register is record
      --  Interrupt Mask on external/internal line 32
      IM             : IMR2_IM_Field := (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#2#;
      --  Interrupt Mask on external/internal line 40
      IM_1           : IMR2_IM_Field_1 := (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#FFFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR2_Register use record
      IM             at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      IM_1           at 0 range 8 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  EMR2_EM array
   type EMR2_EM_Field_Array is array (32 .. 37) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for EMR2_EM
   type EMR2_EM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt6;
         when True =>
            --  EM as an array
            Arr : EMR2_EM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for EMR2_EM_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  EMR2_EM array
   type EMR2_EM_Field_Array_1 is array (40 .. 43) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for EMR2_EM
   type EMR2_EM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EM as a value
            Val : HAL.UInt4;
         when True =>
            --  EM as an array
            Arr : EMR2_EM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for EMR2_EM_Field_1 use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Event mask register
   type EMR2_Register is record
      --  Event mask on external/internal line 32
      EM             : EMR2_EM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Event mask on external/internal line 40
      EM_1           : EMR2_EM_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR2_Register use record
      EM             at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      EM_1           at 0 range 8 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  RTSR2_RT array
   type RTSR2_RT_Field_Array is array (32 .. 33) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RTSR2_RT
   type RTSR2_RT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt2;
         when True =>
            --  RT as an array
            Arr : RTSR2_RT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RTSR2_RT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RTSR2_RT array
   type RTSR2_RT_Field_Array_1 is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RTSR2_RT
   type RTSR2_RT_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RT as a value
            Val : HAL.UInt2;
         when True =>
            --  RT as an array
            Arr : RTSR2_RT_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RTSR2_RT_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Rising Trigger selection register
   type RTSR2_Register is record
      --  Rising trigger event configuration bit of line 32
      RT             : RTSR2_RT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Rising trigger event configuration bit of line 40
      RT_1           : RTSR2_RT_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR2_Register use record
      RT             at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      RT_1           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  FTSR2_FT array
   type FTSR2_FT_Field_Array is array (32 .. 33) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FTSR2_FT
   type FTSR2_FT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt2;
         when True =>
            --  FT as an array
            Arr : FTSR2_FT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FTSR2_FT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  FTSR2_FT array
   type FTSR2_FT_Field_Array_1 is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FTSR2_FT
   type FTSR2_FT_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FT as a value
            Val : HAL.UInt2;
         when True =>
            --  FT as an array
            Arr : FTSR2_FT_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FTSR2_FT_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Falling Trigger selection register
   type FTSR2_Register is record
      --  Falling trigger event configuration bit of line 32
      FT             : FTSR2_FT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Falling trigger event configuration bit of line 40
      FT_1           : FTSR2_FT_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR2_Register use record
      FT             at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      FT_1           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  SWIER2_SWI array
   type SWIER2_SWI_Field_Array is array (32 .. 33) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SWIER2_SWI
   type SWIER2_SWI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt2;
         when True =>
            --  SWI as an array
            Arr : SWIER2_SWI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWIER2_SWI_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SWIER2_SWI array
   type SWIER2_SWI_Field_Array_1 is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SWIER2_SWI
   type SWIER2_SWI_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWI as a value
            Val : HAL.UInt2;
         when True =>
            --  SWI as an array
            Arr : SWIER2_SWI_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWIER2_SWI_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Software interrupt event register
   type SWIER2_Register is record
      --  Software interrupt on line 32
      SWI            : SWIER2_SWI_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Software interrupt on line 40
      SWI_1          : SWIER2_SWI_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER2_Register use record
      SWI            at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SWI_1          at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  PR2_PIF array
   type PR2_PIF_Field_Array is array (32 .. 33) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for PR2_PIF
   type PR2_PIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIF as a value
            Val : HAL.UInt2;
         when True =>
            --  PIF as an array
            Arr : PR2_PIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for PR2_PIF_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  PR2_PIF array
   type PR2_PIF_Field_Array_1 is array (40 .. 41) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for PR2_PIF
   type PR2_PIF_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIF as a value
            Val : HAL.UInt2;
         when True =>
            --  PIF as an array
            Arr : PR2_PIF_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for PR2_PIF_Field_1 use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Pending register
   type PR2_Register is record
      --  Pending interrupt flag on line 32
      PIF            : PR2_PIF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Pending interrupt flag on line 40
      PIF_1          : PR2_PIF_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR2_Register use record
      PIF            at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      PIF_1          at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupt/event controller
   type EXTI_Peripheral is record
      --  Interrupt mask register
      IMR1   : aliased IMR1_Register;
      --  Event mask register
      EMR1   : aliased EMR1_Register;
      --  Rising Trigger selection register
      RTSR1  : aliased RTSR1_Register;
      --  Falling Trigger selection register
      FTSR1  : aliased FTSR1_Register;
      --  Software interrupt event register
      SWIER1 : aliased SWIER1_Register;
      --  Pending register
      PR1    : aliased PR1_Register;
      --  Interrupt mask register
      IMR2   : aliased IMR2_Register;
      --  Event mask register
      EMR2   : aliased EMR2_Register;
      --  Rising Trigger selection register
      RTSR2  : aliased RTSR2_Register;
      --  Falling Trigger selection register
      FTSR2  : aliased FTSR2_Register;
      --  Software interrupt event register
      SWIER2 : aliased SWIER2_Register;
      --  Pending register
      PR2    : aliased PR2_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      IMR1   at 16#0# range 0 .. 31;
      EMR1   at 16#4# range 0 .. 31;
      RTSR1  at 16#8# range 0 .. 31;
      FTSR1  at 16#C# range 0 .. 31;
      SWIER1 at 16#10# range 0 .. 31;
      PR1    at 16#14# range 0 .. 31;
      IMR2   at 16#20# range 0 .. 31;
      EMR2   at 16#24# range 0 .. 31;
      RTSR2  at 16#28# range 0 .. 31;
      FTSR2  at 16#2C# range 0 .. 31;
      SWIER2 at 16#30# range 0 .. 31;
      PR2    at 16#34# range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => EXTI_Base;

end STM32_SVD.EXTI;
