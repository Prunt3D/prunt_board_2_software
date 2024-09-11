pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.OPAMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype OPAMP1_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMP1_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMP1_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMP1_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMP1_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMP1_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMP1 control/status register
   type OPAMP1_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMP1_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMP1_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMP1_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMP1_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMP1_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMP1_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMP2_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMP2_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMP2_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMP2_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMP2_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMP2_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMP2 control/status register
   type OPAMP2_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMP2_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMP2_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMP2_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMP2_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMP2_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMP2_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMP3_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMP3_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMP3_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMP3_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMP3_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMP3_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMP3 control/status register
   type OPAMP3_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMP3_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMP3_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMP3_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMP3_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMP3_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMP3_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP3_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMP4_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMP4_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMP4_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMP4_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMP4_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMP4_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMP4 control/status register
   type OPAMP4_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMP4_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMP4_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMP4_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMP4_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMP4_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMP4_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP4_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMP5_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMP5_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMP5_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMP5_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMP5_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMP5_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMP5 control/status register
   type OPAMP5_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMP5_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMP5_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMP5_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMP5_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMP5_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMP5_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP5_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMP6_CSR_VP_SEL_Field is HAL.UInt2;
   subtype OPAMP6_CSR_VM_SEL_Field is HAL.UInt2;
   subtype OPAMP6_CSR_CALSEL_Field is HAL.UInt2;
   subtype OPAMP6_CSR_PGA_GAIN_Field is HAL.UInt5;
   subtype OPAMP6_CSR_TRIMOFFSETP_Field is HAL.UInt5;
   subtype OPAMP6_CSR_TRIMOFFSETN_Field is HAL.UInt5;

   --  OPAMP6 control/status register
   type OPAMP6_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  FORCE_VP
      FORCE_VP       : Boolean := False;
      --  VP_SEL
      VP_SEL         : OPAMP6_CSR_VP_SEL_Field := 16#0#;
      --  USERTRIM
      USERTRIM       : Boolean := False;
      --  VM_SEL
      VM_SEL         : OPAMP6_CSR_VM_SEL_Field := 16#0#;
      --  OPAHSM
      OPAHSM         : Boolean := False;
      --  OPAINTOEN
      OPAINTOEN      : Boolean := False;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  CALON
      CALON          : Boolean := False;
      --  CALSEL
      CALSEL         : OPAMP6_CSR_CALSEL_Field := 16#0#;
      --  PGA_GAIN
      PGA_GAIN       : OPAMP6_CSR_PGA_GAIN_Field := 16#0#;
      --  TRIMOFFSETP
      TRIMOFFSETP    : OPAMP6_CSR_TRIMOFFSETP_Field := 16#0#;
      --  TRIMOFFSETN
      TRIMOFFSETN    : OPAMP6_CSR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  CALOUT
      CALOUT         : Boolean := False;
      --  LOCK
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP6_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      USERTRIM       at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      OPAHSM         at 0 range 7 .. 7;
      OPAINTOEN      at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 18;
      TRIMOFFSETP    at 0 range 19 .. 23;
      TRIMOFFSETN    at 0 range 24 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPAMP1_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMP1 control/status register
   type OPAMP1_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMP1_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   subtype OPAMP2_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMP2 control/status register
   type OPAMP2_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMP2_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   subtype OPAMP3_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMP3 control/status register
   type OPAMP3_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMP3_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP3_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   subtype OPAMP4_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMP4 control/status register
   type OPAMP4_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMP4_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP4_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   subtype OPAMP5_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMP5 control/status register
   type OPAMP5_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMP5_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP5_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   subtype OPAMP6_TCMR_VPS_SEL_Field is HAL.UInt2;

   --  OPAMP6 control/status register
   type OPAMP6_TCMR_Register is record
      --  VMS_SEL
      VMS_SEL       : Boolean := False;
      --  VPS_SEL
      VPS_SEL       : OPAMP6_TCMR_VPS_SEL_Field := 16#0#;
      --  T1CM_EN
      T1CM_EN       : Boolean := False;
      --  T8CM_EN
      T8CM_EN       : Boolean := False;
      --  T20CM_EN
      T20CM_EN      : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  LOCK
      LOCK          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP6_TCMR_Register use record
      VMS_SEL       at 0 range 0 .. 0;
      VPS_SEL       at 0 range 1 .. 2;
      T1CM_EN       at 0 range 3 .. 3;
      T8CM_EN       at 0 range 4 .. 4;
      T20CM_EN      at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      LOCK          at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Operational amplifiers
   type OPAMP_Peripheral is record
      --  OPAMP1 control/status register
      OPAMP1_CSR  : aliased OPAMP1_CSR_Register;
      --  OPAMP2 control/status register
      OPAMP2_CSR  : aliased OPAMP2_CSR_Register;
      --  OPAMP3 control/status register
      OPAMP3_CSR  : aliased OPAMP3_CSR_Register;
      --  OPAMP4 control/status register
      OPAMP4_CSR  : aliased OPAMP4_CSR_Register;
      --  OPAMP5 control/status register
      OPAMP5_CSR  : aliased OPAMP5_CSR_Register;
      --  OPAMP6 control/status register
      OPAMP6_CSR  : aliased OPAMP6_CSR_Register;
      --  OPAMP1 control/status register
      OPAMP1_TCMR : aliased OPAMP1_TCMR_Register;
      --  OPAMP2 control/status register
      OPAMP2_TCMR : aliased OPAMP2_TCMR_Register;
      --  OPAMP3 control/status register
      OPAMP3_TCMR : aliased OPAMP3_TCMR_Register;
      --  OPAMP4 control/status register
      OPAMP4_TCMR : aliased OPAMP4_TCMR_Register;
      --  OPAMP5 control/status register
      OPAMP5_TCMR : aliased OPAMP5_TCMR_Register;
      --  OPAMP6 control/status register
      OPAMP6_TCMR : aliased OPAMP6_TCMR_Register;
   end record
     with Volatile;

   for OPAMP_Peripheral use record
      OPAMP1_CSR  at 16#0# range 0 .. 31;
      OPAMP2_CSR  at 16#4# range 0 .. 31;
      OPAMP3_CSR  at 16#8# range 0 .. 31;
      OPAMP4_CSR  at 16#C# range 0 .. 31;
      OPAMP5_CSR  at 16#10# range 0 .. 31;
      OPAMP6_CSR  at 16#14# range 0 .. 31;
      OPAMP1_TCMR at 16#18# range 0 .. 31;
      OPAMP2_TCMR at 16#1C# range 0 .. 31;
      OPAMP3_TCMR at 16#20# range 0 .. 31;
      OPAMP4_TCMR at 16#24# range 0 .. 31;
      OPAMP5_TCMR at 16#28# range 0 .. 31;
      OPAMP6_TCMR at 16#2C# range 0 .. 31;
   end record;

   --  Operational amplifiers
   OPAMP_Periph : aliased OPAMP_Peripheral
     with Import, Address => OPAMP_Base;

end STM32_SVD.OPAMP;
