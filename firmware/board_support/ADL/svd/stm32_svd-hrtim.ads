pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.HRTIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR1_AD1USRC_Field is HAL.UInt3;
   subtype CR1_AD2USRC_Field is HAL.UInt3;
   subtype CR1_AD3USRC_Field is HAL.UInt3;
   subtype CR1_AD4USRC_Field is HAL.UInt3;

   --  Control Register 1
   type CR1_Register is record
      --  Master Update Disable
      MUDIS          : Boolean := False;
      --  Timer A Update Disable
      TAUDIS         : Boolean := False;
      --  Timer B Update Disable
      TBUDIS         : Boolean := False;
      --  Timer C Update Disable
      TCUDIS         : Boolean := False;
      --  Timer D Update Disable
      TDUDIS         : Boolean := False;
      --  Timer E Update Disable
      TEUDIS         : Boolean := False;
      --  Timer f Update Disable
      TFUDIS         : Boolean := False;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  ADC Trigger 1 Update Source
      AD1USRC        : CR1_AD1USRC_Field := 16#0#;
      --  ADC Trigger 2 Update Source
      AD2USRC        : CR1_AD2USRC_Field := 16#0#;
      --  ADC Trigger 3 Update Source
      AD3USRC        : CR1_AD3USRC_Field := 16#0#;
      --  ADC Trigger 4 Update Source
      AD4USRC        : CR1_AD4USRC_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      MUDIS          at 0 range 0 .. 0;
      TAUDIS         at 0 range 1 .. 1;
      TBUDIS         at 0 range 2 .. 2;
      TCUDIS         at 0 range 3 .. 3;
      TDUDIS         at 0 range 4 .. 4;
      TEUDIS         at 0 range 5 .. 5;
      TFUDIS         at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      AD1USRC        at 0 range 16 .. 18;
      AD2USRC        at 0 range 19 .. 21;
      AD3USRC        at 0 range 22 .. 24;
      AD4USRC        at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Control Register 2
   type CR2_Register is record
      --  Master Timer Software update
      MSWU           : Boolean := False;
      --  Timer A Software update
      TASWU          : Boolean := False;
      --  Timer B Software Update
      TBSWU          : Boolean := False;
      --  Timer C Software Update
      TCSWU          : Boolean := False;
      --  Timer D Software Update
      TDSWU          : Boolean := False;
      --  Timer E Software Update
      TESWU          : Boolean := False;
      --  Timer f Software Update
      TFSWU          : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Master Counter software reset
      MRST           : Boolean := False;
      --  Timer A counter software reset
      TARST          : Boolean := False;
      --  Timer B counter software reset
      TBRST          : Boolean := False;
      --  Timer C counter software reset
      TCRST          : Boolean := False;
      --  Timer D counter software reset
      TDRST          : Boolean := False;
      --  Timer E counter software reset
      TERST          : Boolean := False;
      --  Timer f counter software reset
      TFRST          : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Swap Timer A outputs
      SWPA           : Boolean := False;
      --  Swap Timer B outputs
      SWPB           : Boolean := False;
      --  Swap Timer C outputs
      SWPC           : Boolean := False;
      --  Swap Timer D outputs
      SWPD           : Boolean := False;
      --  Swap Timer E outputs
      SWPE           : Boolean := False;
      --  Swap Timer F outputs
      SWPF           : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      MSWU           at 0 range 0 .. 0;
      TASWU          at 0 range 1 .. 1;
      TBSWU          at 0 range 2 .. 2;
      TCSWU          at 0 range 3 .. 3;
      TDSWU          at 0 range 4 .. 4;
      TESWU          at 0 range 5 .. 5;
      TFSWU          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MRST           at 0 range 8 .. 8;
      TARST          at 0 range 9 .. 9;
      TBRST          at 0 range 10 .. 10;
      TCRST          at 0 range 11 .. 11;
      TDRST          at 0 range 12 .. 12;
      TERST          at 0 range 13 .. 13;
      TFRST          at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWPA           at 0 range 16 .. 16;
      SWPB           at 0 range 17 .. 17;
      SWPC           at 0 range 18 .. 18;
      SWPD           at 0 range 19 .. 19;
      SWPE           at 0 range 20 .. 20;
      SWPF           at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  ISR_FLT array
   type ISR_FLT_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for ISR_FLT
   type ISR_FLT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FLT as a value
            Val : HAL.UInt5;
         when True =>
            --  FLT as an array
            Arr : ISR_FLT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for ISR_FLT_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Interrupt Status Register
   type ISR_Register is record
      --  Read-only. Fault 1 Interrupt Flag
      FLT            : ISR_FLT_Field;
      --  Read-only. System Fault Interrupt Flag
      SYSFLT         : Boolean;
      --  Read-only. Fault 6 Interrupt Flag
      FLT6           : Boolean;
      --  unspecified
      Reserved_7_15  : HAL.UInt9;
      --  Read-only. DLL Ready Interrupt Flag
      DLLRDY         : Boolean;
      --  Read-only. Burst mode Period Interrupt Flag
      BMPER          : Boolean;
      --  unspecified
      Reserved_18_31 : HAL.UInt14;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      FLT            at 0 range 0 .. 4;
      SYSFLT         at 0 range 5 .. 5;
      FLT6           at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      DLLRDY         at 0 range 16 .. 16;
      BMPER          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Interrupt Clear Register
   type ICR_Register is record
      --  Write-only. Fault 1 Interrupt Flag Clear
      FLT1C          : Boolean := False;
      --  Write-only. Fault 2 Interrupt Flag Clear
      FLT2C          : Boolean := False;
      --  Write-only. Fault 3 Interrupt Flag Clear
      FLT3C          : Boolean := False;
      --  Write-only. Fault 4 Interrupt Flag Clear
      FLT4C          : Boolean := False;
      --  Write-only. Fault 5 Interrupt Flag Clear
      FLT5C          : Boolean := False;
      --  Write-only. System Fault Interrupt Flag Clear
      SYSFLTC        : Boolean := False;
      --  Write-only. Fault 6 Interrupt Flag Clear
      FLT6C          : Boolean := False;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  Write-only. DLL Ready Interrupt flag Clear
      DLLRDYC        : Boolean := False;
      --  Write-only. Burst mode period flag Clear
      BMPERC         : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      FLT1C          at 0 range 0 .. 0;
      FLT2C          at 0 range 1 .. 1;
      FLT3C          at 0 range 2 .. 2;
      FLT4C          at 0 range 3 .. 3;
      FLT5C          at 0 range 4 .. 4;
      SYSFLTC        at 0 range 5 .. 5;
      FLT6C          at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      DLLRDYC        at 0 range 16 .. 16;
      BMPERC         at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Interrupt Enable Register
   type IER_Register is record
      --  Fault 1 Interrupt Enable
      FLT1IE         : Boolean := False;
      --  Fault 2 Interrupt Enable
      FLT2IE         : Boolean := False;
      --  Fault 3 Interrupt Enable
      FLT3IE         : Boolean := False;
      --  Fault 4 Interrupt Enable
      FLT4IE         : Boolean := False;
      --  Fault 5 Interrupt Enable
      FLT5IE         : Boolean := False;
      --  System Fault Interrupt Enable
      SYSFLTE        : Boolean := False;
      --  Fault 6 Interrupt Enable
      FLT6IE         : Boolean := False;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  DLL Ready Interrupt Enable
      DLLRDYIE       : Boolean := False;
      --  Burst mode period Interrupt Enable
      BMPERIE        : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      FLT1IE         at 0 range 0 .. 0;
      FLT2IE         at 0 range 1 .. 1;
      FLT3IE         at 0 range 2 .. 2;
      FLT4IE         at 0 range 3 .. 3;
      FLT5IE         at 0 range 4 .. 4;
      SYSFLTE        at 0 range 5 .. 5;
      FLT6IE         at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      DLLRDYIE       at 0 range 16 .. 16;
      BMPERIE        at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Output Enable Register
   type OENR_Register is record
      --  Timer A Output 1 Enable
      TA1OEN         : Boolean := False;
      --  Timer A Output 2 Enable
      TA2OEN         : Boolean := False;
      --  Timer B Output 1 Enable
      TB1OEN         : Boolean := False;
      --  Timer B Output 2 Enable
      TB2OEN         : Boolean := False;
      --  Timer C Output 1 Enable
      TC1OEN         : Boolean := False;
      --  Timer C Output 2 Enable
      TC2OEN         : Boolean := False;
      --  Timer D Output 1 Enable
      TD1OEN         : Boolean := False;
      --  Timer D Output 2 Enable
      TD2OEN         : Boolean := False;
      --  Timer E Output 1 Enable
      TE1OEN         : Boolean := False;
      --  Timer E Output 2 Enable
      TE2OEN         : Boolean := False;
      --  Timer F Output 1 disable status
      TF1OEN         : Boolean := False;
      --  Timer F Output 2 disable status
      TF2OEN         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OENR_Register use record
      TA1OEN         at 0 range 0 .. 0;
      TA2OEN         at 0 range 1 .. 1;
      TB1OEN         at 0 range 2 .. 2;
      TB2OEN         at 0 range 3 .. 3;
      TC1OEN         at 0 range 4 .. 4;
      TC2OEN         at 0 range 5 .. 5;
      TD1OEN         at 0 range 6 .. 6;
      TD2OEN         at 0 range 7 .. 7;
      TE1OEN         at 0 range 8 .. 8;
      TE2OEN         at 0 range 9 .. 9;
      TF1OEN         at 0 range 10 .. 10;
      TF2OEN         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  ODISR
   type ODISR_Register is record
      --  Write-only. TA1ODIS
      TA1ODIS        : Boolean := False;
      --  Write-only. TA2ODIS
      TA2ODIS        : Boolean := False;
      --  Write-only. TB1ODIS
      TB1ODIS        : Boolean := False;
      --  Write-only. TB2ODIS
      TB2ODIS        : Boolean := False;
      --  Write-only. TC1ODIS
      TC1ODIS        : Boolean := False;
      --  Write-only. TC2ODIS
      TC2ODIS        : Boolean := False;
      --  Write-only. TD1ODIS
      TD1ODIS        : Boolean := False;
      --  Write-only. TD2ODIS
      TD2ODIS        : Boolean := False;
      --  Write-only. TE1ODIS
      TE1ODIS        : Boolean := False;
      --  Write-only. TE2ODIS
      TE2ODIS        : Boolean := False;
      --  Write-only. TF1ODIS
      TF1ODIS        : Boolean := False;
      --  Write-only. TF2ODIS
      TF2ODIS        : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ODISR_Register use record
      TA1ODIS        at 0 range 0 .. 0;
      TA2ODIS        at 0 range 1 .. 1;
      TB1ODIS        at 0 range 2 .. 2;
      TB2ODIS        at 0 range 3 .. 3;
      TC1ODIS        at 0 range 4 .. 4;
      TC2ODIS        at 0 range 5 .. 5;
      TD1ODIS        at 0 range 6 .. 6;
      TD2ODIS        at 0 range 7 .. 7;
      TE1ODIS        at 0 range 8 .. 8;
      TE2ODIS        at 0 range 9 .. 9;
      TF1ODIS        at 0 range 10 .. 10;
      TF2ODIS        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Output Disable Status Register
   type ODSR_Register is record
      --  Read-only. Timer A Output 1 disable status
      TA1ODS         : Boolean;
      --  Read-only. Timer A Output 2 disable status
      TA2ODS         : Boolean;
      --  Read-only. Timer B Output 1 disable status
      TB1ODS         : Boolean;
      --  Read-only. Timer B Output 2 disable status
      TB2ODS         : Boolean;
      --  Read-only. Timer C Output 1 disable status
      TC1ODS         : Boolean;
      --  Read-only. Timer C Output 2 disable status
      TC2ODS         : Boolean;
      --  Read-only. Timer D Output 1 disable status
      TD1ODS         : Boolean;
      --  Read-only. Timer D Output 2 disable status
      TD2ODS         : Boolean;
      --  Read-only. Timer E Output 1 disable status
      TE1ODS         : Boolean;
      --  Read-only. Timer E Output 2 disable status
      TE2ODS         : Boolean;
      --  Read-only. TF1ODS
      TF1ODS         : Boolean;
      --  Read-only. TF2ODS
      TF2ODS         : Boolean;
      --  unspecified
      Reserved_12_31 : HAL.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ODSR_Register use record
      TA1ODS         at 0 range 0 .. 0;
      TA2ODS         at 0 range 1 .. 1;
      TB1ODS         at 0 range 2 .. 2;
      TB2ODS         at 0 range 3 .. 3;
      TC1ODS         at 0 range 4 .. 4;
      TC2ODS         at 0 range 5 .. 5;
      TD1ODS         at 0 range 6 .. 6;
      TD2ODS         at 0 range 7 .. 7;
      TE1ODS         at 0 range 8 .. 8;
      TE2ODS         at 0 range 9 .. 9;
      TF1ODS         at 0 range 10 .. 10;
      TF2ODS         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype BMCR_BMCLK_Field is HAL.UInt4;
   subtype BMCR_BMPRSC_Field is HAL.UInt4;

   --  Burst Mode Control Register
   type BMCR_Register is record
      --  Burst Mode enable
      BME            : Boolean := False;
      --  Burst Mode operating mode
      BMOM           : Boolean := False;
      --  Burst Mode Clock source
      BMCLK          : BMCR_BMCLK_Field := 16#0#;
      --  Burst Mode Prescaler
      BMPRSC         : BMCR_BMPRSC_Field := 16#0#;
      --  Burst Mode Preload Enable
      BMPREN         : Boolean := False;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Master Timer Burst Mode
      MTBM           : Boolean := False;
      --  Timer A Burst Mode
      TABM           : Boolean := False;
      --  Timer B Burst Mode
      TBBM           : Boolean := False;
      --  Timer C Burst Mode
      TCBM           : Boolean := False;
      --  Timer D Burst Mode
      TDBM           : Boolean := False;
      --  Timer E Burst Mode
      TEBM           : Boolean := False;
      --  Timer f Burst Mode
      TFBM           : Boolean := False;
      --  unspecified
      Reserved_23_30 : HAL.UInt8 := 16#0#;
      --  Burst Mode Status
      BMSTAT         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BMCR_Register use record
      BME            at 0 range 0 .. 0;
      BMOM           at 0 range 1 .. 1;
      BMCLK          at 0 range 2 .. 5;
      BMPRSC         at 0 range 6 .. 9;
      BMPREN         at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      MTBM           at 0 range 16 .. 16;
      TABM           at 0 range 17 .. 17;
      TBBM           at 0 range 18 .. 18;
      TCBM           at 0 range 19 .. 19;
      TDBM           at 0 range 20 .. 20;
      TEBM           at 0 range 21 .. 21;
      TFBM           at 0 range 22 .. 22;
      Reserved_23_30 at 0 range 23 .. 30;
      BMSTAT         at 0 range 31 .. 31;
   end record;

   --  BMTRG_MSTCMP array
   type BMTRG_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BMTRG_MSTCMP
   type BMTRG_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : BMTRG_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BMTRG_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BMTRG_TACMP array
   type BMTRG_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_TACMP
   type BMTRG_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : BMTRG_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG_TBCMP array
   type BMTRG_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_TBCMP
   type BMTRG_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : BMTRG_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG_TCCMP array
   type BMTRG_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_TCCMP
   type BMTRG_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : BMTRG_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG_TDCMP array
   type BMTRG_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_TDCMP
   type BMTRG_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : BMTRG_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG_TECMP array
   type BMTRG_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_TECMP
   type BMTRG_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : BMTRG_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG_TDEEV array
   type BMTRG_TDEEV_Field_Array is array (7 .. 8) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_TDEEV
   type BMTRG_TDEEV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDEEV as a value
            Val : HAL.UInt2;
         when True =>
            --  TDEEV as an array
            Arr : BMTRG_TDEEV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_TDEEV_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG_EEV array
   type BMTRG_EEV_Field_Array is array (7 .. 8) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BMTRG_EEV
   type BMTRG_EEV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EEV as a value
            Val : HAL.UInt2;
         when True =>
            --  EEV as an array
            Arr : BMTRG_EEV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BMTRG_EEV_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  BMTRG
   type BMTRG_Register is record
      --  SW
      SW     : Boolean := False;
      --  MSTRST
      MSTRST : Boolean := False;
      --  MSTREP
      MSTREP : Boolean := False;
      --  MSTCMP1
      MSTCMP : BMTRG_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TARST
      TARST  : Boolean := False;
      --  TAREP
      TAREP  : Boolean := False;
      --  TACMP1
      TACMP  : BMTRG_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  TBRST
      TBRST  : Boolean := False;
      --  TBREP
      TBREP  : Boolean := False;
      --  TBCMP1
      TBCMP  : BMTRG_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  TCRST
      TCRST  : Boolean := False;
      --  TCREP
      TCREP  : Boolean := False;
      --  TCCMP1
      TCCMP  : BMTRG_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  TDRST
      TDRST  : Boolean := False;
      --  TDREP
      TDREP  : Boolean := False;
      --  TDCMP1
      TDCMP  : BMTRG_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  TERST
      TERST  : Boolean := False;
      --  TEREP
      TEREP  : Boolean := False;
      --  TECMP1
      TECMP  : BMTRG_TECMP_Field := (As_Array => False, Val => 16#0#);
      --  TDEEV7
      TDEEV  : BMTRG_TDEEV_Field := (As_Array => False, Val => 16#0#);
      --  EEV7
      EEV    : BMTRG_EEV_Field := (As_Array => False, Val => 16#0#);
      --  OCHPEV
      OCHPEV : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BMTRG_Register use record
      SW     at 0 range 0 .. 0;
      MSTRST at 0 range 1 .. 1;
      MSTREP at 0 range 2 .. 2;
      MSTCMP at 0 range 3 .. 6;
      TARST  at 0 range 7 .. 7;
      TAREP  at 0 range 8 .. 8;
      TACMP  at 0 range 9 .. 10;
      TBRST  at 0 range 11 .. 11;
      TBREP  at 0 range 12 .. 12;
      TBCMP  at 0 range 13 .. 14;
      TCRST  at 0 range 15 .. 15;
      TCREP  at 0 range 16 .. 16;
      TCCMP  at 0 range 17 .. 18;
      TDRST  at 0 range 19 .. 19;
      TDREP  at 0 range 20 .. 20;
      TDCMP  at 0 range 21 .. 22;
      TERST  at 0 range 23 .. 23;
      TEREP  at 0 range 24 .. 24;
      TECMP  at 0 range 25 .. 26;
      TDEEV  at 0 range 27 .. 28;
      EEV    at 0 range 29 .. 30;
      OCHPEV at 0 range 31 .. 31;
   end record;

   subtype BMCMPR_BMCMP_Field is HAL.UInt16;

   --  BMCMPR
   type BMCMPR_Register is record
      --  BMCMP
      BMCMP          : BMCMPR_BMCMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BMCMPR_Register use record
      BMCMP          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype BMPER_BMPER_Field is HAL.UInt16;

   --  Burst Mode Period Register
   type BMPER_Register is record
      --  Burst mode Period
      BMPER          : BMPER_BMPER_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BMPER_Register use record
      BMPER          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EECR1_EE1SRC_Field is HAL.UInt2;
   subtype EECR1_EE1SNS_Field is HAL.UInt2;
   subtype EECR1_EE2SRC_Field is HAL.UInt2;
   subtype EECR1_EE2SNS_Field is HAL.UInt2;
   subtype EECR1_EE3SRC_Field is HAL.UInt2;
   subtype EECR1_EE3SNS_Field is HAL.UInt2;
   subtype EECR1_EE4SRC_Field is HAL.UInt2;
   subtype EECR1_EE4SNS_Field is HAL.UInt2;
   subtype EECR1_EE5SRC_Field is HAL.UInt2;
   subtype EECR1_EE5SNS_Field is HAL.UInt2;

   --  Timer External Event Control Register 1
   type EECR1_Register is record
      --  External Event 1 Source
      EE1SRC         : EECR1_EE1SRC_Field := 16#0#;
      --  External Event 1 Polarity
      EE1POL         : Boolean := False;
      --  External Event 1 Sensitivity
      EE1SNS         : EECR1_EE1SNS_Field := 16#0#;
      --  External Event 1 Fast mode
      EE1FAST        : Boolean := False;
      --  External Event 2 Source
      EE2SRC         : EECR1_EE2SRC_Field := 16#0#;
      --  External Event 2 Polarity
      EE2POL         : Boolean := False;
      --  External Event 2 Sensitivity
      EE2SNS         : EECR1_EE2SNS_Field := 16#0#;
      --  External Event 2 Fast mode
      EE2FAST        : Boolean := False;
      --  External Event 3 Source
      EE3SRC         : EECR1_EE3SRC_Field := 16#0#;
      --  External Event 3 Polarity
      EE3POL         : Boolean := False;
      --  External Event 3 Sensitivity
      EE3SNS         : EECR1_EE3SNS_Field := 16#0#;
      --  External Event 3 Fast mode
      EE3FAST        : Boolean := False;
      --  External Event 4 Source
      EE4SRC         : EECR1_EE4SRC_Field := 16#0#;
      --  External Event 4 Polarity
      EE4POL         : Boolean := False;
      --  External Event 4 Sensitivity
      EE4SNS         : EECR1_EE4SNS_Field := 16#0#;
      --  External Event 4 Fast mode
      EE4FAST        : Boolean := False;
      --  External Event 5 Source
      EE5SRC         : EECR1_EE5SRC_Field := 16#0#;
      --  External Event 5 Polarity
      EE5POL         : Boolean := False;
      --  External Event 5 Sensitivity
      EE5SNS         : EECR1_EE5SNS_Field := 16#0#;
      --  External Event 5 Fast mode
      EE5FAST        : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EECR1_Register use record
      EE1SRC         at 0 range 0 .. 1;
      EE1POL         at 0 range 2 .. 2;
      EE1SNS         at 0 range 3 .. 4;
      EE1FAST        at 0 range 5 .. 5;
      EE2SRC         at 0 range 6 .. 7;
      EE2POL         at 0 range 8 .. 8;
      EE2SNS         at 0 range 9 .. 10;
      EE2FAST        at 0 range 11 .. 11;
      EE3SRC         at 0 range 12 .. 13;
      EE3POL         at 0 range 14 .. 14;
      EE3SNS         at 0 range 15 .. 16;
      EE3FAST        at 0 range 17 .. 17;
      EE4SRC         at 0 range 18 .. 19;
      EE4POL         at 0 range 20 .. 20;
      EE4SNS         at 0 range 21 .. 22;
      EE4FAST        at 0 range 23 .. 23;
      EE5SRC         at 0 range 24 .. 25;
      EE5POL         at 0 range 26 .. 26;
      EE5SNS         at 0 range 27 .. 28;
      EE5FAST        at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype EECR2_EE6SRC_Field is HAL.UInt2;
   subtype EECR2_EE6SNS_Field is HAL.UInt2;
   subtype EECR2_EE7SRC_Field is HAL.UInt2;
   subtype EECR2_EE7SNS_Field is HAL.UInt2;
   subtype EECR2_EE8SRC_Field is HAL.UInt2;
   subtype EECR2_EE8SNS_Field is HAL.UInt2;
   subtype EECR2_EE9SRC_Field is HAL.UInt2;
   subtype EECR2_EE9SNS_Field is HAL.UInt2;
   subtype EECR2_EE10SRC_Field is HAL.UInt2;
   subtype EECR2_EE10SNS_Field is HAL.UInt2;

   --  Timer External Event Control Register 2
   type EECR2_Register is record
      --  EE6SRC
      EE6SRC         : EECR2_EE6SRC_Field := 16#0#;
      --  EE6POL
      EE6POL         : Boolean := False;
      --  EE6SNS
      EE6SNS         : EECR2_EE6SNS_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  EE7SRC
      EE7SRC         : EECR2_EE7SRC_Field := 16#0#;
      --  EE7POL
      EE7POL         : Boolean := False;
      --  EE7SNS
      EE7SNS         : EECR2_EE7SNS_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  EE8SRC
      EE8SRC         : EECR2_EE8SRC_Field := 16#0#;
      --  EE8POL
      EE8POL         : Boolean := False;
      --  EE8SNS
      EE8SNS         : EECR2_EE8SNS_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  EE9SRC
      EE9SRC         : EECR2_EE9SRC_Field := 16#0#;
      --  EE9POL
      EE9POL         : Boolean := False;
      --  EE9SNS
      EE9SNS         : EECR2_EE9SNS_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  EE10SRC
      EE10SRC        : EECR2_EE10SRC_Field := 16#0#;
      --  EE10POL
      EE10POL        : Boolean := False;
      --  EE10SNS
      EE10SNS        : EECR2_EE10SNS_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EECR2_Register use record
      EE6SRC         at 0 range 0 .. 1;
      EE6POL         at 0 range 2 .. 2;
      EE6SNS         at 0 range 3 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7SRC         at 0 range 6 .. 7;
      EE7POL         at 0 range 8 .. 8;
      EE7SNS         at 0 range 9 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8SRC         at 0 range 12 .. 13;
      EE8POL         at 0 range 14 .. 14;
      EE8SNS         at 0 range 15 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9SRC         at 0 range 18 .. 19;
      EE9POL         at 0 range 20 .. 20;
      EE9SNS         at 0 range 21 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10SRC        at 0 range 24 .. 25;
      EE10POL        at 0 range 26 .. 26;
      EE10SNS        at 0 range 27 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EECR3_EE6F_Field is HAL.UInt4;
   subtype EECR3_EE7F_Field is HAL.UInt4;
   subtype EECR3_EE8F_Field is HAL.UInt4;
   subtype EECR3_EE9F_Field is HAL.UInt4;
   subtype EECR3_EE10F_Field is HAL.UInt4;
   subtype EECR3_EEVSD_Field is HAL.UInt2;

   --  Timer External Event Control Register 3
   type EECR3_Register is record
      --  EE6F
      EE6F           : EECR3_EE6F_Field := 16#0#;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  EE7F
      EE7F           : EECR3_EE7F_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  EE8F
      EE8F           : EECR3_EE8F_Field := 16#0#;
      --  unspecified
      Reserved_16_17 : HAL.UInt2 := 16#0#;
      --  EE9F
      EE9F           : EECR3_EE9F_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  EE10F
      EE10F          : EECR3_EE10F_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  EEVSD
      EEVSD          : EECR3_EEVSD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EECR3_Register use record
      EE6F           at 0 range 0 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      EE7F           at 0 range 6 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      EE8F           at 0 range 12 .. 15;
      Reserved_16_17 at 0 range 16 .. 17;
      EE9F           at 0 range 18 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      EE10F          at 0 range 24 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      EEVSD          at 0 range 30 .. 31;
   end record;

   --  ADC1R_AD1MC array
   type ADC1R_AD1MC_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for ADC1R_AD1MC
   type ADC1R_AD1MC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1MC as a value
            Val : HAL.UInt4;
         when True =>
            --  AD1MC as an array
            Arr : ADC1R_AD1MC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for ADC1R_AD1MC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  ADC1R_AD1EEV array
   type ADC1R_AD1EEV_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for ADC1R_AD1EEV
   type ADC1R_AD1EEV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1EEV as a value
            Val : HAL.UInt5;
         when True =>
            --  AD1EEV as an array
            Arr : ADC1R_AD1EEV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for ADC1R_AD1EEV_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  ADC1R_AD1TAC array
   type ADC1R_AD1TAC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC1R_AD1TAC
   type ADC1R_AD1TAC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TAC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TAC as an array
            Arr : ADC1R_AD1TAC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC1R_AD1TAC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC1R_AD1TBC array
   type ADC1R_AD1TBC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC1R_AD1TBC
   type ADC1R_AD1TBC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TBC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TBC as an array
            Arr : ADC1R_AD1TBC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC1R_AD1TBC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC1R_AD1TCC array
   type ADC1R_AD1TCC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC1R_AD1TCC
   type ADC1R_AD1TCC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TCC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TCC as an array
            Arr : ADC1R_AD1TCC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC1R_AD1TCC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC1R_AD1TDC array
   type ADC1R_AD1TDC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC1R_AD1TDC
   type ADC1R_AD1TDC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TDC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TDC as an array
            Arr : ADC1R_AD1TDC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC1R_AD1TDC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC1R_AD1TEC array
   type ADC1R_AD1TEC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC1R_AD1TEC
   type ADC1R_AD1TEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TEC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TEC as an array
            Arr : ADC1R_AD1TEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC1R_AD1TEC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC Trigger 1 Register
   type ADC1R_Register is record
      --  ADC trigger 1 on Master Compare 1
      AD1MC    : ADC1R_AD1MC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Master Period
      AD1MPER  : Boolean := False;
      --  ADC trigger 1 on External Event 1
      AD1EEV   : ADC1R_AD1EEV_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Timer A compare 2
      AD1TAC   : ADC1R_AD1TAC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Timer A Period
      AD1TAPER : Boolean := False;
      --  ADC trigger 1 on Timer A Reset
      AD1TARST : Boolean := False;
      --  ADC trigger 1 on Timer B compare 2
      AD1TBC   : ADC1R_AD1TBC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Timer B Period
      AD1TBPER : Boolean := False;
      --  ADC trigger 1 on Timer B Reset
      AD1TBRST : Boolean := False;
      --  ADC trigger 1 on Timer C compare 2
      AD1TCC   : ADC1R_AD1TCC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Timer C Period
      AD1TCPER : Boolean := False;
      --  ADC trigger 1 on Timer D compare 2
      AD1TDC   : ADC1R_AD1TDC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Timer D Period
      AD1TDPER : Boolean := False;
      --  ADC trigger 1 on Timer E compare 2
      AD1TEC   : ADC1R_AD1TEC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 1 on Timer E Period
      AD1TEPER : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC1R_Register use record
      AD1MC    at 0 range 0 .. 3;
      AD1MPER  at 0 range 4 .. 4;
      AD1EEV   at 0 range 5 .. 9;
      AD1TAC   at 0 range 10 .. 12;
      AD1TAPER at 0 range 13 .. 13;
      AD1TARST at 0 range 14 .. 14;
      AD1TBC   at 0 range 15 .. 17;
      AD1TBPER at 0 range 18 .. 18;
      AD1TBRST at 0 range 19 .. 19;
      AD1TCC   at 0 range 20 .. 22;
      AD1TCPER at 0 range 23 .. 23;
      AD1TDC   at 0 range 24 .. 26;
      AD1TDPER at 0 range 27 .. 27;
      AD1TEC   at 0 range 28 .. 30;
      AD1TEPER at 0 range 31 .. 31;
   end record;

   --  ADC2R_AD2MC array
   type ADC2R_AD2MC_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for ADC2R_AD2MC
   type ADC2R_AD2MC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2MC as a value
            Val : HAL.UInt4;
         when True =>
            --  AD2MC as an array
            Arr : ADC2R_AD2MC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for ADC2R_AD2MC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  ADC2R_AD2EEV array
   type ADC2R_AD2EEV_Field_Array is array (6 .. 10) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for ADC2R_AD2EEV
   type ADC2R_AD2EEV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2EEV as a value
            Val : HAL.UInt5;
         when True =>
            --  AD2EEV as an array
            Arr : ADC2R_AD2EEV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for ADC2R_AD2EEV_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  ADC2R_AD2TAC array
   type ADC2R_AD2TAC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC2R_AD2TAC
   type ADC2R_AD2TAC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TAC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TAC as an array
            Arr : ADC2R_AD2TAC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC2R_AD2TAC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC2R_AD2TBC array
   type ADC2R_AD2TBC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC2R_AD2TBC
   type ADC2R_AD2TBC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TBC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TBC as an array
            Arr : ADC2R_AD2TBC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC2R_AD2TBC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC2R_AD2TCC array
   type ADC2R_AD2TCC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC2R_AD2TCC
   type ADC2R_AD2TCC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TCC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TCC as an array
            Arr : ADC2R_AD2TCC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC2R_AD2TCC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC2R_AD2TDC array
   type ADC2R_AD2TDC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC2R_AD2TDC
   type ADC2R_AD2TDC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TDC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TDC as an array
            Arr : ADC2R_AD2TDC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC2R_AD2TDC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC2R_AD2TEC array
   type ADC2R_AD2TEC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC2R_AD2TEC
   type ADC2R_AD2TEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TEC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TEC as an array
            Arr : ADC2R_AD2TEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC2R_AD2TEC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC Trigger 2 Register
   type ADC2R_Register is record
      --  ADC trigger 2 on Master Compare 1
      AD2MC    : ADC2R_AD2MC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Master Period
      AD2MPER  : Boolean := False;
      --  ADC trigger 2 on External Event 6
      AD2EEV   : ADC2R_AD2EEV_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Timer A compare 2
      AD2TAC   : ADC2R_AD2TAC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Timer A Period
      AD2TAPER : Boolean := False;
      --  ADC trigger 2 on Timer B compare 2
      AD2TBC   : ADC2R_AD2TBC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Timer B Period
      AD2TBPER : Boolean := False;
      --  ADC trigger 2 on Timer C compare 2
      AD2TCC   : ADC2R_AD2TCC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Timer C Period
      AD2TCPER : Boolean := False;
      --  ADC trigger 2 on Timer C Reset
      AD2TCRST : Boolean := False;
      --  ADC trigger 2 on Timer D compare 2
      AD2TDC   : ADC2R_AD2TDC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Timer D Period
      AD2TDPER : Boolean := False;
      --  ADC trigger 2 on Timer D Reset
      AD2TDRST : Boolean := False;
      --  ADC trigger 2 on Timer E compare 2
      AD2TEC   : ADC2R_AD2TEC_Field := (As_Array => False, Val => 16#0#);
      --  ADC trigger 2 on Timer E Reset
      AD2TERST : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC2R_Register use record
      AD2MC    at 0 range 0 .. 3;
      AD2MPER  at 0 range 4 .. 4;
      AD2EEV   at 0 range 5 .. 9;
      AD2TAC   at 0 range 10 .. 12;
      AD2TAPER at 0 range 13 .. 13;
      AD2TBC   at 0 range 14 .. 16;
      AD2TBPER at 0 range 17 .. 17;
      AD2TCC   at 0 range 18 .. 20;
      AD2TCPER at 0 range 21 .. 21;
      AD2TCRST at 0 range 22 .. 22;
      AD2TDC   at 0 range 23 .. 25;
      AD2TDPER at 0 range 26 .. 26;
      AD2TDRST at 0 range 27 .. 27;
      AD2TEC   at 0 range 28 .. 30;
      AD2TERST at 0 range 31 .. 31;
   end record;

   --  ADC3R_AD1MC array
   type ADC3R_AD1MC_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for ADC3R_AD1MC
   type ADC3R_AD1MC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1MC as a value
            Val : HAL.UInt4;
         when True =>
            --  AD1MC as an array
            Arr : ADC3R_AD1MC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for ADC3R_AD1MC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  ADC3R_AD1EEV array
   type ADC3R_AD1EEV_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for ADC3R_AD1EEV
   type ADC3R_AD1EEV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1EEV as a value
            Val : HAL.UInt5;
         when True =>
            --  AD1EEV as an array
            Arr : ADC3R_AD1EEV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for ADC3R_AD1EEV_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  ADC3R_AD1TAC array
   type ADC3R_AD1TAC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC3R_AD1TAC
   type ADC3R_AD1TAC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TAC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TAC as an array
            Arr : ADC3R_AD1TAC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC3R_AD1TAC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC3R_AD1TBC array
   type ADC3R_AD1TBC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC3R_AD1TBC
   type ADC3R_AD1TBC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TBC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TBC as an array
            Arr : ADC3R_AD1TBC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC3R_AD1TBC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC3R_AD1TCC array
   type ADC3R_AD1TCC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC3R_AD1TCC
   type ADC3R_AD1TCC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TCC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TCC as an array
            Arr : ADC3R_AD1TCC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC3R_AD1TCC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC3R_AD1TDC array
   type ADC3R_AD1TDC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC3R_AD1TDC
   type ADC3R_AD1TDC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TDC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TDC as an array
            Arr : ADC3R_AD1TDC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC3R_AD1TDC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC3R_AD1TEC array
   type ADC3R_AD1TEC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC3R_AD1TEC
   type ADC3R_AD1TEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD1TEC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD1TEC as an array
            Arr : ADC3R_AD1TEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC3R_AD1TEC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC Trigger 3 Register
   type ADC3R_Register is record
      --  AD1MC1
      AD1MC    : ADC3R_AD1MC_Field := (As_Array => False, Val => 16#0#);
      --  AD1MPER
      AD1MPER  : Boolean := False;
      --  AD1EEV1
      AD1EEV   : ADC3R_AD1EEV_Field := (As_Array => False, Val => 16#0#);
      --  AD1TAC2
      AD1TAC   : ADC3R_AD1TAC_Field := (As_Array => False, Val => 16#0#);
      --  AD1TAPER
      AD1TAPER : Boolean := False;
      --  AD1TARST
      AD1TARST : Boolean := False;
      --  AD1TBC2
      AD1TBC   : ADC3R_AD1TBC_Field := (As_Array => False, Val => 16#0#);
      --  AD1TBPER
      AD1TBPER : Boolean := False;
      --  AD1TBRST
      AD1TBRST : Boolean := False;
      --  AD1TCC2
      AD1TCC   : ADC3R_AD1TCC_Field := (As_Array => False, Val => 16#0#);
      --  AD1TCPER
      AD1TCPER : Boolean := False;
      --  AD1TDC2
      AD1TDC   : ADC3R_AD1TDC_Field := (As_Array => False, Val => 16#0#);
      --  AD1TDPER
      AD1TDPER : Boolean := False;
      --  AD1TEC2
      AD1TEC   : ADC3R_AD1TEC_Field := (As_Array => False, Val => 16#0#);
      --  AD1TEPER
      AD1TEPER : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC3R_Register use record
      AD1MC    at 0 range 0 .. 3;
      AD1MPER  at 0 range 4 .. 4;
      AD1EEV   at 0 range 5 .. 9;
      AD1TAC   at 0 range 10 .. 12;
      AD1TAPER at 0 range 13 .. 13;
      AD1TARST at 0 range 14 .. 14;
      AD1TBC   at 0 range 15 .. 17;
      AD1TBPER at 0 range 18 .. 18;
      AD1TBRST at 0 range 19 .. 19;
      AD1TCC   at 0 range 20 .. 22;
      AD1TCPER at 0 range 23 .. 23;
      AD1TDC   at 0 range 24 .. 26;
      AD1TDPER at 0 range 27 .. 27;
      AD1TEC   at 0 range 28 .. 30;
      AD1TEPER at 0 range 31 .. 31;
   end record;

   --  ADC4R_AD2MC array
   type ADC4R_AD2MC_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for ADC4R_AD2MC
   type ADC4R_AD2MC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2MC as a value
            Val : HAL.UInt4;
         when True =>
            --  AD2MC as an array
            Arr : ADC4R_AD2MC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for ADC4R_AD2MC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  ADC4R_AD2EEV array
   type ADC4R_AD2EEV_Field_Array is array (6 .. 10) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for ADC4R_AD2EEV
   type ADC4R_AD2EEV_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2EEV as a value
            Val : HAL.UInt5;
         when True =>
            --  AD2EEV as an array
            Arr : ADC4R_AD2EEV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for ADC4R_AD2EEV_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  ADC4R_AD2TAC array
   type ADC4R_AD2TAC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC4R_AD2TAC
   type ADC4R_AD2TAC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TAC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TAC as an array
            Arr : ADC4R_AD2TAC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC4R_AD2TAC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC4R_AD2TBC array
   type ADC4R_AD2TBC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC4R_AD2TBC
   type ADC4R_AD2TBC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TBC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TBC as an array
            Arr : ADC4R_AD2TBC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC4R_AD2TBC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC4R_AD2TCC array
   type ADC4R_AD2TCC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC4R_AD2TCC
   type ADC4R_AD2TCC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TCC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TCC as an array
            Arr : ADC4R_AD2TCC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC4R_AD2TCC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC4R_AD2TDC array
   type ADC4R_AD2TDC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC4R_AD2TDC
   type ADC4R_AD2TDC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TDC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TDC as an array
            Arr : ADC4R_AD2TDC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC4R_AD2TDC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC4R_AD2TEC array
   type ADC4R_AD2TEC_Field_Array is array (2 .. 4) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ADC4R_AD2TEC
   type ADC4R_AD2TEC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AD2TEC as a value
            Val : HAL.UInt3;
         when True =>
            --  AD2TEC as an array
            Arr : ADC4R_AD2TEC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ADC4R_AD2TEC_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC Trigger 4 Register
   type ADC4R_Register is record
      --  AD2MC1
      AD2MC    : ADC4R_AD2MC_Field := (As_Array => False, Val => 16#0#);
      --  AD2MPER
      AD2MPER  : Boolean := False;
      --  AD2EEV6
      AD2EEV   : ADC4R_AD2EEV_Field := (As_Array => False, Val => 16#0#);
      --  AD2TAC2
      AD2TAC   : ADC4R_AD2TAC_Field := (As_Array => False, Val => 16#0#);
      --  AD2TAPER
      AD2TAPER : Boolean := False;
      --  AD2TBC2
      AD2TBC   : ADC4R_AD2TBC_Field := (As_Array => False, Val => 16#0#);
      --  AD2TBPER
      AD2TBPER : Boolean := False;
      --  AD2TCC2
      AD2TCC   : ADC4R_AD2TCC_Field := (As_Array => False, Val => 16#0#);
      --  AD2TCPER
      AD2TCPER : Boolean := False;
      --  AD2TCRST
      AD2TCRST : Boolean := False;
      --  AD2TDC2
      AD2TDC   : ADC4R_AD2TDC_Field := (As_Array => False, Val => 16#0#);
      --  AD2TDPER
      AD2TDPER : Boolean := False;
      --  AD2TDRST
      AD2TDRST : Boolean := False;
      --  AD2TEC2
      AD2TEC   : ADC4R_AD2TEC_Field := (As_Array => False, Val => 16#0#);
      --  AD2TERST
      AD2TERST : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADC4R_Register use record
      AD2MC    at 0 range 0 .. 3;
      AD2MPER  at 0 range 4 .. 4;
      AD2EEV   at 0 range 5 .. 9;
      AD2TAC   at 0 range 10 .. 12;
      AD2TAPER at 0 range 13 .. 13;
      AD2TBC   at 0 range 14 .. 16;
      AD2TBPER at 0 range 17 .. 17;
      AD2TCC   at 0 range 18 .. 20;
      AD2TCPER at 0 range 21 .. 21;
      AD2TCRST at 0 range 22 .. 22;
      AD2TDC   at 0 range 23 .. 25;
      AD2TDPER at 0 range 26 .. 26;
      AD2TDRST at 0 range 27 .. 27;
      AD2TEC   at 0 range 28 .. 30;
      AD2TERST at 0 range 31 .. 31;
   end record;

   subtype DLLCR_CALRTE_Field is HAL.UInt2;

   --  DLL Control Register
   type DLLCR_Register is record
      --  DLL Calibration Start
      CAL           : Boolean := False;
      --  DLL Calibration Enable
      CALEN         : Boolean := False;
      --  DLL Calibration rate
      CALRTE        : DLLCR_CALRTE_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLLCR_Register use record
      CAL           at 0 range 0 .. 0;
      CALEN         at 0 range 1 .. 1;
      CALRTE        at 0 range 2 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype FLTINR1_FLT1F_Field is HAL.UInt4;
   subtype FLTINR1_FLT2F_Field is HAL.UInt4;
   subtype FLTINR1_FLT3F_Field is HAL.UInt4;
   subtype FLTINR1_FLT4F_Field is HAL.UInt4;

   --  HRTIM Fault Input Register 1
   type FLTINR1_Register is record
      --  FLT1E
      FLT1E   : Boolean := False;
      --  FLT1P
      FLT1P   : Boolean := False;
      --  FLT1SRC
      FLT1SRC : Boolean := False;
      --  FLT1F
      FLT1F   : FLTINR1_FLT1F_Field := 16#0#;
      --  FLT1LCK
      FLT1LCK : Boolean := False;
      --  FLT2E
      FLT2E   : Boolean := False;
      --  FLT2P
      FLT2P   : Boolean := False;
      --  FLT2SRC
      FLT2SRC : Boolean := False;
      --  FLT2F
      FLT2F   : FLTINR1_FLT2F_Field := 16#0#;
      --  FLT2LCK
      FLT2LCK : Boolean := False;
      --  FLT3E
      FLT3E   : Boolean := False;
      --  FLT3P
      FLT3P   : Boolean := False;
      --  FLT3SRC
      FLT3SRC : Boolean := False;
      --  FLT3F
      FLT3F   : FLTINR1_FLT3F_Field := 16#0#;
      --  FLT3LCK
      FLT3LCK : Boolean := False;
      --  FLT4E
      FLT4E   : Boolean := False;
      --  FLT4P
      FLT4P   : Boolean := False;
      --  FLT4SRC
      FLT4SRC : Boolean := False;
      --  FLT4F
      FLT4F   : FLTINR1_FLT4F_Field := 16#0#;
      --  FLT4LCK
      FLT4LCK : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTINR1_Register use record
      FLT1E   at 0 range 0 .. 0;
      FLT1P   at 0 range 1 .. 1;
      FLT1SRC at 0 range 2 .. 2;
      FLT1F   at 0 range 3 .. 6;
      FLT1LCK at 0 range 7 .. 7;
      FLT2E   at 0 range 8 .. 8;
      FLT2P   at 0 range 9 .. 9;
      FLT2SRC at 0 range 10 .. 10;
      FLT2F   at 0 range 11 .. 14;
      FLT2LCK at 0 range 15 .. 15;
      FLT3E   at 0 range 16 .. 16;
      FLT3P   at 0 range 17 .. 17;
      FLT3SRC at 0 range 18 .. 18;
      FLT3F   at 0 range 19 .. 22;
      FLT3LCK at 0 range 23 .. 23;
      FLT4E   at 0 range 24 .. 24;
      FLT4P   at 0 range 25 .. 25;
      FLT4SRC at 0 range 26 .. 26;
      FLT4F   at 0 range 27 .. 30;
      FLT4LCK at 0 range 31 .. 31;
   end record;

   subtype FLTINR2_FLT5F_Field is HAL.UInt4;
   subtype FLTINR2_FLT6F_Field is HAL.UInt4;
   subtype FLTINR2_FLTSD_Field is HAL.UInt2;

   --  HRTIM Fault Input Register 2
   type FLTINR2_Register is record
      --  FLT5E
      FLT5E          : Boolean := False;
      --  FLT5P
      FLT5P          : Boolean := False;
      --  FLT5SRC
      FLT5SRC        : Boolean := False;
      --  FLT5F
      FLT5F          : FLTINR2_FLT5F_Field := 16#0#;
      --  FLT5LCK
      FLT5LCK        : Boolean := False;
      --  FLT6E
      FLT6E          : Boolean := False;
      --  FLT6P
      FLT6P          : Boolean := False;
      --  FLT6F
      FLT6SRC_0      : Boolean := False;
      --  FLT6F
      FLT6F          : FLTINR2_FLT6F_Field := 16#0#;
      --  FLT6LCK
      FLT6LCK        : Boolean := False;
      --  FLT1SRC_1
      FLT1SRC_1      : Boolean := False;
      --  FLT2SRC_1
      FLT2SRC_1      : Boolean := False;
      --  FLT3SRC_1
      FLT3SRC_1      : Boolean := False;
      --  FLT4SRC_1
      FLT4SRC_1      : Boolean := False;
      --  FLT5SRC_1
      FLT5SRC_1      : Boolean := False;
      --  FLT6SRC
      FLT6SRC_1      : Boolean := False;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  FLTSD
      FLTSD          : FLTINR2_FLTSD_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTINR2_Register use record
      FLT5E          at 0 range 0 .. 0;
      FLT5P          at 0 range 1 .. 1;
      FLT5SRC        at 0 range 2 .. 2;
      FLT5F          at 0 range 3 .. 6;
      FLT5LCK        at 0 range 7 .. 7;
      FLT6E          at 0 range 8 .. 8;
      FLT6P          at 0 range 9 .. 9;
      FLT6SRC_0      at 0 range 10 .. 10;
      FLT6F          at 0 range 11 .. 14;
      FLT6LCK        at 0 range 15 .. 15;
      FLT1SRC_1      at 0 range 16 .. 16;
      FLT2SRC_1      at 0 range 17 .. 17;
      FLT3SRC_1      at 0 range 18 .. 18;
      FLT4SRC_1      at 0 range 19 .. 19;
      FLT5SRC_1      at 0 range 20 .. 20;
      FLT6SRC_1      at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      FLTSD          at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  BDMUPDR_MCMP array
   type BDMUPDR_MCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDMUPDR_MCMP
   type BDMUPDR_MCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MCMP as an array
            Arr : BDMUPDR_MCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDMUPDR_MCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDMUPDR
   type BDMUPDR_Register is record
      --  MCR
      MCR            : Boolean := False;
      --  MICR
      MICR           : Boolean := False;
      --  MDIER
      MDIER          : Boolean := False;
      --  MCNT
      MCNT           : Boolean := False;
      --  MPER
      MPER           : Boolean := False;
      --  MREP
      MREP           : Boolean := False;
      --  MCMP1
      MCMP           : BDMUPDR_MCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDMUPDR_Register use record
      MCR            at 0 range 0 .. 0;
      MICR           at 0 range 1 .. 1;
      MDIER          at 0 range 2 .. 2;
      MCNT           at 0 range 3 .. 3;
      MPER           at 0 range 4 .. 4;
      MREP           at 0 range 5 .. 5;
      MCMP           at 0 range 6 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  BDTAUPR_TIMxCMP array
   type BDTAUPR_TIMxCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDTAUPR_TIMxCMP
   type BDTAUPR_TIMxCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  TIMxCMP as an array
            Arr : BDTAUPR_TIMxCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDTAUPR_TIMxCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDTAUPR_TIMxEEFR array
   type BDTAUPR_TIMxEEFR_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BDTAUPR_TIMxEEFR
   type BDTAUPR_TIMxEEFR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxEEFR as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMxEEFR as an array
            Arr : BDTAUPR_TIMxEEFR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BDTAUPR_TIMxEEFR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Burst DMA Timerx update Register
   type BDTAUPR_Register is record
      --  HRTIM_TIMxCR register update enable
      TIMxCR         : Boolean := False;
      --  HRTIM_TIMxICR register update enable
      TIMxICR        : Boolean := False;
      --  HRTIM_TIMxDIER register update enable
      TIMxDIER       : Boolean := False;
      --  HRTIM_CNTxR register update enable
      TIMxCNT        : Boolean := False;
      --  HRTIM_PERxR register update enable
      TIMxPER        : Boolean := False;
      --  HRTIM_REPxR register update enable
      TIMxREP        : Boolean := False;
      --  HRTIM_CMP1xR register update enable
      TIMxCMP        : BDTAUPR_TIMxCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_DTxR register update enable
      TIMx_DTxR      : Boolean := False;
      --  HRTIM_SET1xR register update enable
      TIMxSET1R      : Boolean := False;
      --  HRTIM_RST1xR register update enable
      TIMxRST1R      : Boolean := False;
      --  HRTIM_SET2xR register update enable
      TIMxSET2R      : Boolean := False;
      --  HRTIM_RST2xR register update enable
      TIMxRST2R      : Boolean := False;
      --  HRTIM_EEFxR1 register update enable
      TIMxEEFR       : BDTAUPR_TIMxEEFR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_RSTxR register update enable
      TIMxRSTR       : Boolean := False;
      --  HRTIM_CHPxR register update enable
      TIMxCHPR       : Boolean := False;
      --  HRTIM_OUTxR register update enable
      TIMxOUTR       : Boolean := False;
      --  HRTIM_FLTxR register update enable
      TIMxFLTR       : Boolean := False;
      --  TIMxCR2
      TIMxCR2        : Boolean := False;
      --  TIMxEEFR3
      TIMxEEFR3      : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTAUPR_Register use record
      TIMxCR         at 0 range 0 .. 0;
      TIMxICR        at 0 range 1 .. 1;
      TIMxDIER       at 0 range 2 .. 2;
      TIMxCNT        at 0 range 3 .. 3;
      TIMxPER        at 0 range 4 .. 4;
      TIMxREP        at 0 range 5 .. 5;
      TIMxCMP        at 0 range 6 .. 9;
      TIMx_DTxR      at 0 range 10 .. 10;
      TIMxSET1R      at 0 range 11 .. 11;
      TIMxRST1R      at 0 range 12 .. 12;
      TIMxSET2R      at 0 range 13 .. 13;
      TIMxRST2R      at 0 range 14 .. 14;
      TIMxEEFR       at 0 range 15 .. 16;
      TIMxRSTR       at 0 range 17 .. 17;
      TIMxCHPR       at 0 range 18 .. 18;
      TIMxOUTR       at 0 range 19 .. 19;
      TIMxFLTR       at 0 range 20 .. 20;
      TIMxCR2        at 0 range 21 .. 21;
      TIMxEEFR3      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  BDTBUPR_TIMxCMP array
   type BDTBUPR_TIMxCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDTBUPR_TIMxCMP
   type BDTBUPR_TIMxCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  TIMxCMP as an array
            Arr : BDTBUPR_TIMxCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDTBUPR_TIMxCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDTBUPR_TIMxEEFR array
   type BDTBUPR_TIMxEEFR_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BDTBUPR_TIMxEEFR
   type BDTBUPR_TIMxEEFR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxEEFR as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMxEEFR as an array
            Arr : BDTBUPR_TIMxEEFR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BDTBUPR_TIMxEEFR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Burst DMA Timerx update Register
   type BDTBUPR_Register is record
      --  HRTIM_TIMxCR register update enable
      TIMxCR         : Boolean := False;
      --  HRTIM_TIMxICR register update enable
      TIMxICR        : Boolean := False;
      --  HRTIM_TIMxDIER register update enable
      TIMxDIER       : Boolean := False;
      --  HRTIM_CNTxR register update enable
      TIMxCNT        : Boolean := False;
      --  HRTIM_PERxR register update enable
      TIMxPER        : Boolean := False;
      --  HRTIM_REPxR register update enable
      TIMxREP        : Boolean := False;
      --  HRTIM_CMP1xR register update enable
      TIMxCMP        : BDTBUPR_TIMxCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_DTxR register update enable
      TIMx_DTxR      : Boolean := False;
      --  HRTIM_SET1xR register update enable
      TIMxSET1R      : Boolean := False;
      --  HRTIM_RST1xR register update enable
      TIMxRST1R      : Boolean := False;
      --  HRTIM_SET2xR register update enable
      TIMxSET2R      : Boolean := False;
      --  HRTIM_RST2xR register update enable
      TIMxRST2R      : Boolean := False;
      --  HRTIM_EEFxR1 register update enable
      TIMxEEFR       : BDTBUPR_TIMxEEFR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_RSTxR register update enable
      TIMxRSTR       : Boolean := False;
      --  HRTIM_CHPxR register update enable
      TIMxCHPR       : Boolean := False;
      --  HRTIM_OUTxR register update enable
      TIMxOUTR       : Boolean := False;
      --  HRTIM_FLTxR register update enable
      TIMxFLTR       : Boolean := False;
      --  TIMxCR2
      TIMxCR2        : Boolean := False;
      --  TIMxEEFR3
      TIMxEEFR3      : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTBUPR_Register use record
      TIMxCR         at 0 range 0 .. 0;
      TIMxICR        at 0 range 1 .. 1;
      TIMxDIER       at 0 range 2 .. 2;
      TIMxCNT        at 0 range 3 .. 3;
      TIMxPER        at 0 range 4 .. 4;
      TIMxREP        at 0 range 5 .. 5;
      TIMxCMP        at 0 range 6 .. 9;
      TIMx_DTxR      at 0 range 10 .. 10;
      TIMxSET1R      at 0 range 11 .. 11;
      TIMxRST1R      at 0 range 12 .. 12;
      TIMxSET2R      at 0 range 13 .. 13;
      TIMxRST2R      at 0 range 14 .. 14;
      TIMxEEFR       at 0 range 15 .. 16;
      TIMxRSTR       at 0 range 17 .. 17;
      TIMxCHPR       at 0 range 18 .. 18;
      TIMxOUTR       at 0 range 19 .. 19;
      TIMxFLTR       at 0 range 20 .. 20;
      TIMxCR2        at 0 range 21 .. 21;
      TIMxEEFR3      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  BDTCUPR_TIMxCMP array
   type BDTCUPR_TIMxCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDTCUPR_TIMxCMP
   type BDTCUPR_TIMxCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  TIMxCMP as an array
            Arr : BDTCUPR_TIMxCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDTCUPR_TIMxCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDTCUPR_TIMxEEFR array
   type BDTCUPR_TIMxEEFR_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BDTCUPR_TIMxEEFR
   type BDTCUPR_TIMxEEFR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxEEFR as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMxEEFR as an array
            Arr : BDTCUPR_TIMxEEFR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BDTCUPR_TIMxEEFR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Burst DMA Timerx update Register
   type BDTCUPR_Register is record
      --  HRTIM_TIMxCR register update enable
      TIMxCR         : Boolean := False;
      --  HRTIM_TIMxICR register update enable
      TIMxICR        : Boolean := False;
      --  HRTIM_TIMxDIER register update enable
      TIMxDIER       : Boolean := False;
      --  HRTIM_CNTxR register update enable
      TIMxCNT        : Boolean := False;
      --  HRTIM_PERxR register update enable
      TIMxPER        : Boolean := False;
      --  HRTIM_REPxR register update enable
      TIMxREP        : Boolean := False;
      --  HRTIM_CMP1xR register update enable
      TIMxCMP        : BDTCUPR_TIMxCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_DTxR register update enable
      TIMx_DTxR      : Boolean := False;
      --  HRTIM_SET1xR register update enable
      TIMxSET1R      : Boolean := False;
      --  HRTIM_RST1xR register update enable
      TIMxRST1R      : Boolean := False;
      --  HRTIM_SET2xR register update enable
      TIMxSET2R      : Boolean := False;
      --  HRTIM_RST2xR register update enable
      TIMxRST2R      : Boolean := False;
      --  HRTIM_EEFxR1 register update enable
      TIMxEEFR       : BDTCUPR_TIMxEEFR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_RSTxR register update enable
      TIMxRSTR       : Boolean := False;
      --  HRTIM_CHPxR register update enable
      TIMxCHPR       : Boolean := False;
      --  HRTIM_OUTxR register update enable
      TIMxOUTR       : Boolean := False;
      --  HRTIM_FLTxR register update enable
      TIMxFLTR       : Boolean := False;
      --  TIMxCR2
      TIMxCR2        : Boolean := False;
      --  TIMxEEFR3
      TIMxEEFR3      : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTCUPR_Register use record
      TIMxCR         at 0 range 0 .. 0;
      TIMxICR        at 0 range 1 .. 1;
      TIMxDIER       at 0 range 2 .. 2;
      TIMxCNT        at 0 range 3 .. 3;
      TIMxPER        at 0 range 4 .. 4;
      TIMxREP        at 0 range 5 .. 5;
      TIMxCMP        at 0 range 6 .. 9;
      TIMx_DTxR      at 0 range 10 .. 10;
      TIMxSET1R      at 0 range 11 .. 11;
      TIMxRST1R      at 0 range 12 .. 12;
      TIMxSET2R      at 0 range 13 .. 13;
      TIMxRST2R      at 0 range 14 .. 14;
      TIMxEEFR       at 0 range 15 .. 16;
      TIMxRSTR       at 0 range 17 .. 17;
      TIMxCHPR       at 0 range 18 .. 18;
      TIMxOUTR       at 0 range 19 .. 19;
      TIMxFLTR       at 0 range 20 .. 20;
      TIMxCR2        at 0 range 21 .. 21;
      TIMxEEFR3      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  BDTDUPR_TIMxCMP array
   type BDTDUPR_TIMxCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDTDUPR_TIMxCMP
   type BDTDUPR_TIMxCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  TIMxCMP as an array
            Arr : BDTDUPR_TIMxCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDTDUPR_TIMxCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDTDUPR_TIMxEEFR array
   type BDTDUPR_TIMxEEFR_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BDTDUPR_TIMxEEFR
   type BDTDUPR_TIMxEEFR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxEEFR as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMxEEFR as an array
            Arr : BDTDUPR_TIMxEEFR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BDTDUPR_TIMxEEFR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Burst DMA Timerx update Register
   type BDTDUPR_Register is record
      --  HRTIM_TIMxCR register update enable
      TIMxCR         : Boolean := False;
      --  HRTIM_TIMxICR register update enable
      TIMxICR        : Boolean := False;
      --  HRTIM_TIMxDIER register update enable
      TIMxDIER       : Boolean := False;
      --  HRTIM_CNTxR register update enable
      TIMxCNT        : Boolean := False;
      --  HRTIM_PERxR register update enable
      TIMxPER        : Boolean := False;
      --  HRTIM_REPxR register update enable
      TIMxREP        : Boolean := False;
      --  HRTIM_CMP1xR register update enable
      TIMxCMP        : BDTDUPR_TIMxCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_DTxR register update enable
      TIMx_DTxR      : Boolean := False;
      --  HRTIM_SET1xR register update enable
      TIMxSET1R      : Boolean := False;
      --  HRTIM_RST1xR register update enable
      TIMxRST1R      : Boolean := False;
      --  HRTIM_SET2xR register update enable
      TIMxSET2R      : Boolean := False;
      --  HRTIM_RST2xR register update enable
      TIMxRST2R      : Boolean := False;
      --  HRTIM_EEFxR1 register update enable
      TIMxEEFR       : BDTDUPR_TIMxEEFR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_RSTxR register update enable
      TIMxRSTR       : Boolean := False;
      --  HRTIM_CHPxR register update enable
      TIMxCHPR       : Boolean := False;
      --  HRTIM_OUTxR register update enable
      TIMxOUTR       : Boolean := False;
      --  HRTIM_FLTxR register update enable
      TIMxFLTR       : Boolean := False;
      --  TIMxCR2
      TIMxCR2        : Boolean := False;
      --  TIMxEEFR3
      TIMxEEFR3      : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTDUPR_Register use record
      TIMxCR         at 0 range 0 .. 0;
      TIMxICR        at 0 range 1 .. 1;
      TIMxDIER       at 0 range 2 .. 2;
      TIMxCNT        at 0 range 3 .. 3;
      TIMxPER        at 0 range 4 .. 4;
      TIMxREP        at 0 range 5 .. 5;
      TIMxCMP        at 0 range 6 .. 9;
      TIMx_DTxR      at 0 range 10 .. 10;
      TIMxSET1R      at 0 range 11 .. 11;
      TIMxRST1R      at 0 range 12 .. 12;
      TIMxSET2R      at 0 range 13 .. 13;
      TIMxRST2R      at 0 range 14 .. 14;
      TIMxEEFR       at 0 range 15 .. 16;
      TIMxRSTR       at 0 range 17 .. 17;
      TIMxCHPR       at 0 range 18 .. 18;
      TIMxOUTR       at 0 range 19 .. 19;
      TIMxFLTR       at 0 range 20 .. 20;
      TIMxCR2        at 0 range 21 .. 21;
      TIMxEEFR3      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  BDTEUPR_TIMxCMP array
   type BDTEUPR_TIMxCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDTEUPR_TIMxCMP
   type BDTEUPR_TIMxCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  TIMxCMP as an array
            Arr : BDTEUPR_TIMxCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDTEUPR_TIMxCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDTEUPR_TIMxEEFR array
   type BDTEUPR_TIMxEEFR_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BDTEUPR_TIMxEEFR
   type BDTEUPR_TIMxEEFR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxEEFR as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMxEEFR as an array
            Arr : BDTEUPR_TIMxEEFR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BDTEUPR_TIMxEEFR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Burst DMA Timerx update Register
   type BDTEUPR_Register is record
      --  HRTIM_TIMxCR register update enable
      TIMxCR         : Boolean := False;
      --  HRTIM_TIMxICR register update enable
      TIMxICR        : Boolean := False;
      --  HRTIM_TIMxDIER register update enable
      TIMxDIER       : Boolean := False;
      --  HRTIM_CNTxR register update enable
      TIMxCNT        : Boolean := False;
      --  HRTIM_PERxR register update enable
      TIMxPER        : Boolean := False;
      --  HRTIM_REPxR register update enable
      TIMxREP        : Boolean := False;
      --  HRTIM_CMP1xR register update enable
      TIMxCMP        : BDTEUPR_TIMxCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_DTxR register update enable
      TIMx_DTxR      : Boolean := False;
      --  HRTIM_SET1xR register update enable
      TIMxSET1R      : Boolean := False;
      --  HRTIM_RST1xR register update enable
      TIMxRST1R      : Boolean := False;
      --  HRTIM_SET2xR register update enable
      TIMxSET2R      : Boolean := False;
      --  HRTIM_RST2xR register update enable
      TIMxRST2R      : Boolean := False;
      --  HRTIM_EEFxR1 register update enable
      TIMxEEFR       : BDTEUPR_TIMxEEFR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_RSTxR register update enable
      TIMxRSTR       : Boolean := False;
      --  HRTIM_CHPxR register update enable
      TIMxCHPR       : Boolean := False;
      --  HRTIM_OUTxR register update enable
      TIMxOUTR       : Boolean := False;
      --  HRTIM_FLTxR register update enable
      TIMxFLTR       : Boolean := False;
      --  TIMxCR2
      TIMxCR2        : Boolean := False;
      --  TIMxEEFR3
      TIMxEEFR3      : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTEUPR_Register use record
      TIMxCR         at 0 range 0 .. 0;
      TIMxICR        at 0 range 1 .. 1;
      TIMxDIER       at 0 range 2 .. 2;
      TIMxCNT        at 0 range 3 .. 3;
      TIMxPER        at 0 range 4 .. 4;
      TIMxREP        at 0 range 5 .. 5;
      TIMxCMP        at 0 range 6 .. 9;
      TIMx_DTxR      at 0 range 10 .. 10;
      TIMxSET1R      at 0 range 11 .. 11;
      TIMxRST1R      at 0 range 12 .. 12;
      TIMxSET2R      at 0 range 13 .. 13;
      TIMxRST2R      at 0 range 14 .. 14;
      TIMxEEFR       at 0 range 15 .. 16;
      TIMxRSTR       at 0 range 17 .. 17;
      TIMxCHPR       at 0 range 18 .. 18;
      TIMxOUTR       at 0 range 19 .. 19;
      TIMxFLTR       at 0 range 20 .. 20;
      TIMxCR2        at 0 range 21 .. 21;
      TIMxEEFR3      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  BDTFUPR_TIMxCMP array
   type BDTFUPR_TIMxCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for BDTFUPR_TIMxCMP
   type BDTFUPR_TIMxCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  TIMxCMP as an array
            Arr : BDTFUPR_TIMxCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for BDTFUPR_TIMxCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  BDTFUPR_TIMxEEFR array
   type BDTFUPR_TIMxEEFR_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for BDTFUPR_TIMxEEFR
   type BDTFUPR_TIMxEEFR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMxEEFR as a value
            Val : HAL.UInt2;
         when True =>
            --  TIMxEEFR as an array
            Arr : BDTFUPR_TIMxEEFR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for BDTFUPR_TIMxEEFR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Burst DMA Timerx update Register
   type BDTFUPR_Register is record
      --  HRTIM_TIMxCR register update enable
      TIMxCR         : Boolean := False;
      --  HRTIM_TIMxICR register update enable
      TIMxICR        : Boolean := False;
      --  HRTIM_TIMxDIER register update enable
      TIMxDIER       : Boolean := False;
      --  HRTIM_CNTxR register update enable
      TIMxCNT        : Boolean := False;
      --  HRTIM_PERxR register update enable
      TIMxPER        : Boolean := False;
      --  HRTIM_REPxR register update enable
      TIMxREP        : Boolean := False;
      --  HRTIM_CMP1xR register update enable
      TIMxCMP        : BDTFUPR_TIMxCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_DTxR register update enable
      TIMx_DTxR      : Boolean := False;
      --  HRTIM_SET1xR register update enable
      TIMxSET1R      : Boolean := False;
      --  HRTIM_RST1xR register update enable
      TIMxRST1R      : Boolean := False;
      --  HRTIM_SET2xR register update enable
      TIMxSET2R      : Boolean := False;
      --  HRTIM_RST2xR register update enable
      TIMxRST2R      : Boolean := False;
      --  HRTIM_EEFxR1 register update enable
      TIMxEEFR       : BDTFUPR_TIMxEEFR_Field :=
                        (As_Array => False, Val => 16#0#);
      --  HRTIM_RSTxR register update enable
      TIMxRSTR       : Boolean := False;
      --  HRTIM_CHPxR register update enable
      TIMxCHPR       : Boolean := False;
      --  HRTIM_OUTxR register update enable
      TIMxOUTR       : Boolean := False;
      --  HRTIM_FLTxR register update enable
      TIMxFLTR       : Boolean := False;
      --  TIMxCR2
      TIMxCR2        : Boolean := False;
      --  TIMxEEFR3
      TIMxEEFR3      : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTFUPR_Register use record
      TIMxCR         at 0 range 0 .. 0;
      TIMxICR        at 0 range 1 .. 1;
      TIMxDIER       at 0 range 2 .. 2;
      TIMxCNT        at 0 range 3 .. 3;
      TIMxPER        at 0 range 4 .. 4;
      TIMxREP        at 0 range 5 .. 5;
      TIMxCMP        at 0 range 6 .. 9;
      TIMx_DTxR      at 0 range 10 .. 10;
      TIMxSET1R      at 0 range 11 .. 11;
      TIMxRST1R      at 0 range 12 .. 12;
      TIMxSET2R      at 0 range 13 .. 13;
      TIMxRST2R      at 0 range 14 .. 14;
      TIMxEEFR       at 0 range 15 .. 16;
      TIMxRSTR       at 0 range 17 .. 17;
      TIMxCHPR       at 0 range 18 .. 18;
      TIMxOUTR       at 0 range 19 .. 19;
      TIMxFLTR       at 0 range 20 .. 20;
      TIMxCR2        at 0 range 21 .. 21;
      TIMxEEFR3      at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype ADCER_ADC5TRG_Field is HAL.UInt5;
   subtype ADCER_ADC6TRG_Field is HAL.UInt5;
   subtype ADCER_ADC7TRG_Field is HAL.UInt5;
   subtype ADCER_ADC8TRG_Field is HAL.UInt5;
   subtype ADCER_ADC9TRG_Field is HAL.UInt5;
   subtype ADCER_ADC10TRG_Field is HAL.UInt5;

   --  HRTIM ADC Extended Trigger Register
   type ADCER_Register is record
      --  ADC5TRG
      ADC5TRG        : ADCER_ADC5TRG_Field := 16#0#;
      --  ADC6TRG
      ADC6TRG        : ADCER_ADC6TRG_Field := 16#0#;
      --  ADC7TRG
      ADC7TRG        : ADCER_ADC7TRG_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  ADC8TRG
      ADC8TRG        : ADCER_ADC8TRG_Field := 16#0#;
      --  ADC9TRG
      ADC9TRG        : ADCER_ADC9TRG_Field := 16#0#;
      --  ADC10TRG
      ADC10TRG       : ADCER_ADC10TRG_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADCER_Register use record
      ADC5TRG        at 0 range 0 .. 4;
      ADC6TRG        at 0 range 5 .. 9;
      ADC7TRG        at 0 range 10 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      ADC8TRG        at 0 range 16 .. 20;
      ADC9TRG        at 0 range 21 .. 25;
      ADC10TRG       at 0 range 26 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype ADCUR_AD5USRC_Field is HAL.UInt3;
   subtype ADCUR_AD6USRC_Field is HAL.UInt3;
   subtype ADCUR_AD7USRC_Field is HAL.UInt3;
   subtype ADCUR_AD8USRC_Field is HAL.UInt3;
   subtype ADCUR_AD9USRC_Field is HAL.UInt3;
   subtype ADCUR_AD10USRC_Field is HAL.UInt3;

   --  HRTIM ADC Trigger Update Register
   type ADCUR_Register is record
      --  AD5USRC
      AD5USRC        : ADCUR_AD5USRC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  AD6USRC
      AD6USRC        : ADCUR_AD6USRC_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  AD7USRC
      AD7USRC        : ADCUR_AD7USRC_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  AD8USRC
      AD8USRC        : ADCUR_AD8USRC_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  AD9USRC
      AD9USRC        : ADCUR_AD9USRC_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  AD10USRC
      AD10USRC       : ADCUR_AD10USRC_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADCUR_Register use record
      AD5USRC        at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      AD6USRC        at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      AD7USRC        at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      AD8USRC        at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      AD9USRC        at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      AD10USRC       at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype ADCPS1_ADC1PSC_Field is HAL.UInt5;
   subtype ADCPS1_ADC2PSC_Field is HAL.UInt5;
   subtype ADCPS1_ADC3PSC_Field is HAL.UInt5;
   subtype ADCPS1_ADC4PSC_Field is HAL.UInt5;
   subtype ADCPS1_ADC5PSC_Field is HAL.UInt5;

   --  HRTIM ADC Post Scaler Register 1
   type ADCPS1_Register is record
      --  ADC1PSC
      ADC1PSC        : ADCPS1_ADC1PSC_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  ADC2PSC
      ADC2PSC        : ADCPS1_ADC2PSC_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  ADC3PSC
      ADC3PSC        : ADCPS1_ADC3PSC_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  ADC4PSC
      ADC4PSC        : ADCPS1_ADC4PSC_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  ADC5PSC
      ADC5PSC        : ADCPS1_ADC5PSC_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADCPS1_Register use record
      ADC1PSC        at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ADC2PSC        at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      ADC3PSC        at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      ADC4PSC        at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      ADC5PSC        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype ADCPS2_ADC6PSC_Field is HAL.UInt5;
   subtype ADCPS2_ADC7PSC_Field is HAL.UInt5;
   subtype ADCPS2_ADC8PSC_Field is HAL.UInt5;
   subtype ADCPS2_ADC9PSC_Field is HAL.UInt5;
   subtype ADCPS2_ADC10PSC_Field is HAL.UInt5;

   --  HRTIM ADC Post Scaler Register 2
   type ADCPS2_Register is record
      --  ADC6PSC
      ADC6PSC        : ADCPS2_ADC6PSC_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  ADC7PSC
      ADC7PSC        : ADCPS2_ADC7PSC_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  ADC8PSC
      ADC8PSC        : ADCPS2_ADC8PSC_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  ADC9PSC
      ADC9PSC        : ADCPS2_ADC9PSC_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  ADC10PSC
      ADC10PSC       : ADCPS2_ADC10PSC_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADCPS2_Register use record
      ADC6PSC        at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ADC7PSC        at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      ADC8PSC        at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      ADC9PSC        at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      ADC10PSC       at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype FLTINR3_FLT1CNT_Field is HAL.UInt4;
   subtype FLTINR3_FLT2CNT_Field is HAL.UInt4;
   subtype FLTINR3_FLT3CNT_Field is HAL.UInt4;
   subtype FLTINR3_FLT4CNT_Field is HAL.UInt4;

   --  HRTIM Fault Input Register 3
   type FLTINR3_Register is record
      --  FLT1BLKE
      FLT1BLKE : Boolean := False;
      --  FLT1BLKS
      FLT1BLKS : Boolean := False;
      --  FLT1CNT
      FLT1CNT  : FLTINR3_FLT1CNT_Field := 16#0#;
      --  FLT1CRES
      FLT1CRES : Boolean := False;
      --  FLT1RSTM
      FLT1RSTM : Boolean := False;
      --  FLT2BLKE
      FLT2BLKE : Boolean := False;
      --  FLT2BLKS
      FLT2BLKS : Boolean := False;
      --  FLT2CNT
      FLT2CNT  : FLTINR3_FLT2CNT_Field := 16#0#;
      --  FLT2CRES
      FLT2CRES : Boolean := False;
      --  FLT2RSTM
      FLT2RSTM : Boolean := False;
      --  FLT3BLKE
      FLT3BLKE : Boolean := False;
      --  FLT3BLKS
      FLT3BLKS : Boolean := False;
      --  FLT3CNT
      FLT3CNT  : FLTINR3_FLT3CNT_Field := 16#0#;
      --  FLT3CRES
      FLT3CRES : Boolean := False;
      --  FLT3RSTM
      FLT3RSTM : Boolean := False;
      --  FLT4BLKE
      FLT4BLKE : Boolean := False;
      --  FLT4BLKS
      FLT4BLKS : Boolean := False;
      --  FLT4CNT
      FLT4CNT  : FLTINR3_FLT4CNT_Field := 16#0#;
      --  FLT4CRES
      FLT4CRES : Boolean := False;
      --  FLT4RSTM
      FLT4RSTM : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTINR3_Register use record
      FLT1BLKE at 0 range 0 .. 0;
      FLT1BLKS at 0 range 1 .. 1;
      FLT1CNT  at 0 range 2 .. 5;
      FLT1CRES at 0 range 6 .. 6;
      FLT1RSTM at 0 range 7 .. 7;
      FLT2BLKE at 0 range 8 .. 8;
      FLT2BLKS at 0 range 9 .. 9;
      FLT2CNT  at 0 range 10 .. 13;
      FLT2CRES at 0 range 14 .. 14;
      FLT2RSTM at 0 range 15 .. 15;
      FLT3BLKE at 0 range 16 .. 16;
      FLT3BLKS at 0 range 17 .. 17;
      FLT3CNT  at 0 range 18 .. 21;
      FLT3CRES at 0 range 22 .. 22;
      FLT3RSTM at 0 range 23 .. 23;
      FLT4BLKE at 0 range 24 .. 24;
      FLT4BLKS at 0 range 25 .. 25;
      FLT4CNT  at 0 range 26 .. 29;
      FLT4CRES at 0 range 30 .. 30;
      FLT4RSTM at 0 range 31 .. 31;
   end record;

   subtype FLTINR4_FLT5CNT_Field is HAL.UInt4;
   subtype FLTINR4_FLT6CNT_Field is HAL.UInt4;

   --  HRTIM Fault Input Register 4
   type FLTINR4_Register is record
      --  FLT5BLKE
      FLT5BLKE       : Boolean := False;
      --  FLT5BLKS
      FLT5BLKS       : Boolean := False;
      --  FLT5CNT
      FLT5CNT        : FLTINR4_FLT5CNT_Field := 16#0#;
      --  FLT5CRES
      FLT5CRES       : Boolean := False;
      --  FLT5RSTM
      FLT5RSTM       : Boolean := False;
      --  FLT6BLKE
      FLT6BLKE       : Boolean := False;
      --  FLT6BLKS
      FLT6BLKS       : Boolean := False;
      --  FLT6CNT
      FLT6CNT        : FLTINR4_FLT6CNT_Field := 16#0#;
      --  FLT6CRES
      FLT6CRES       : Boolean := False;
      --  FLT6RSTM
      FLT6RSTM       : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTINR4_Register use record
      FLT5BLKE       at 0 range 0 .. 0;
      FLT5BLKS       at 0 range 1 .. 1;
      FLT5CNT        at 0 range 2 .. 5;
      FLT5CRES       at 0 range 6 .. 6;
      FLT5RSTM       at 0 range 7 .. 7;
      FLT6BLKE       at 0 range 8 .. 8;
      FLT6BLKS       at 0 range 9 .. 9;
      FLT6CNT        at 0 range 10 .. 13;
      FLT6CRES       at 0 range 14 .. 14;
      FLT6RSTM       at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MCR_CKPSC_Field is HAL.UInt3;
   subtype MCR_INTLVD_Field is HAL.UInt2;
   subtype MCR_SYNCIN_Field is HAL.UInt2;
   subtype MCR_SYNCOUT_Field is HAL.UInt2;
   subtype MCR_SYNCSRC_Field is HAL.UInt2;
   subtype MCR_DACSYNC_Field is HAL.UInt2;
   subtype MCR_BRSTDMA_Field is HAL.UInt2;

   --  Master Timer Control Register
   type MCR_Register is record
      --  HRTIM Master Clock prescaler
      CKPSC          : MCR_CKPSC_Field := 16#0#;
      --  Master Continuous mode
      CONT           : Boolean := False;
      --  Master Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Interleaved mode
      INTLVD         : MCR_INTLVD_Field := 16#0#;
      --  synchronization input
      SYNCIN         : MCR_SYNCIN_Field := 16#0#;
      --  Synchronization Resets Master
      SYNCRSTM       : Boolean := False;
      --  Synchronization Starts Master
      SYNCSTRTM      : Boolean := False;
      --  Synchronization output
      SYNCOUT        : MCR_SYNCOUT_Field := 16#0#;
      --  Synchronization source
      SYNCSRC        : MCR_SYNCSRC_Field := 16#0#;
      --  Master Counter enable
      MCEN           : Boolean := False;
      --  Timer A counter enable
      TACEN          : Boolean := False;
      --  Timer B counter enable
      TBCEN          : Boolean := False;
      --  Timer C counter enable
      TCCEN          : Boolean := False;
      --  Timer D counter enable
      TDCEN          : Boolean := False;
      --  Timer E counter enable
      TECEN          : Boolean := False;
      --  Timer F counter enable
      TFCEN          : Boolean := False;
      --  unspecified
      Reserved_23_24 : HAL.UInt2 := 16#0#;
      --  AC Synchronization
      DACSYNC        : MCR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  unspecified
      Reserved_28_28 : HAL.Bit := 16#0#;
      --  Master Timer Repetition update
      MREPU          : Boolean := False;
      --  Burst DMA Update
      BRSTDMA        : MCR_BRSTDMA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCR_Register use record
      CKPSC          at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      INTLVD         at 0 range 6 .. 7;
      SYNCIN         at 0 range 8 .. 9;
      SYNCRSTM       at 0 range 10 .. 10;
      SYNCSTRTM      at 0 range 11 .. 11;
      SYNCOUT        at 0 range 12 .. 13;
      SYNCSRC        at 0 range 14 .. 15;
      MCEN           at 0 range 16 .. 16;
      TACEN          at 0 range 17 .. 17;
      TBCEN          at 0 range 18 .. 18;
      TCCEN          at 0 range 19 .. 19;
      TDCEN          at 0 range 20 .. 20;
      TECEN          at 0 range 21 .. 21;
      TFCEN          at 0 range 22 .. 22;
      Reserved_23_24 at 0 range 23 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      Reserved_28_28 at 0 range 28 .. 28;
      MREPU          at 0 range 29 .. 29;
      BRSTDMA        at 0 range 30 .. 31;
   end record;

   --  MISR_MCMP array
   type MISR_MCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for MISR_MCMP
   type MISR_MCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MCMP as an array
            Arr : MISR_MCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for MISR_MCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Master Timer Interrupt Status Register
   type MISR_Register is record
      --  Read-only. Master Compare 1 Interrupt Flag
      MCMP          : MISR_MCMP_Field;
      --  Read-only. Master Repetition Interrupt Flag
      MREP          : Boolean;
      --  Read-only. Sync Input Interrupt Flag
      SYNC          : Boolean;
      --  Read-only. Master Update Interrupt Flag
      MUPD          : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
      MCMP          at 0 range 0 .. 3;
      MREP          at 0 range 4 .. 4;
      SYNC          at 0 range 5 .. 5;
      MUPD          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Master Timer Interrupt Clear Register
   type MICR_Register is record
      --  Write-only. Master Compare 1 Interrupt flag clear
      MCMP1C        : Boolean := False;
      --  Write-only. Master Compare 2 Interrupt flag clear
      MCMP2C        : Boolean := False;
      --  Write-only. Master Compare 3 Interrupt flag clear
      MCMP3C        : Boolean := False;
      --  Write-only. Master Compare 4 Interrupt flag clear
      MCMP4C        : Boolean := False;
      --  Write-only. Repetition Interrupt flag clear
      MREPC         : Boolean := False;
      --  Write-only. Sync Input Interrupt flag clear
      SYNCC         : Boolean := False;
      --  Write-only. Master update Interrupt flag clear
      MUPDC         : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MICR_Register use record
      MCMP1C        at 0 range 0 .. 0;
      MCMP2C        at 0 range 1 .. 1;
      MCMP3C        at 0 range 2 .. 2;
      MCMP4C        at 0 range 3 .. 3;
      MREPC         at 0 range 4 .. 4;
      SYNCC         at 0 range 5 .. 5;
      MUPDC         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  HRTIM Master Timer DMA / Interrupt Enable Register
   type MDIER_Register is record
      --  MCMP1IE
      MCMP1IE        : Boolean := False;
      --  MCMP2IE
      MCMP2IE        : Boolean := False;
      --  MCMP3IE
      MCMP3IE        : Boolean := False;
      --  MCMP4IE
      MCMP4IE        : Boolean := False;
      --  MREPIE
      MREPIE         : Boolean := False;
      --  SYNCIE
      SYNCIE         : Boolean := False;
      --  MUPDIE
      MUPDIE         : Boolean := False;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  MCMP1DE
      MCMP1DE        : Boolean := False;
      --  MCMP2DE
      MCMP2DE        : Boolean := False;
      --  MCMP3DE
      MCMP3DE        : Boolean := False;
      --  MCMP4DE
      MCMP4DE        : Boolean := False;
      --  MREPDE
      MREPDE         : Boolean := False;
      --  SYNCDE
      SYNCDE         : Boolean := False;
      --  MUPDDE
      MUPDDE         : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDIER_Register use record
      MCMP1IE        at 0 range 0 .. 0;
      MCMP2IE        at 0 range 1 .. 1;
      MCMP3IE        at 0 range 2 .. 2;
      MCMP4IE        at 0 range 3 .. 3;
      MREPIE         at 0 range 4 .. 4;
      SYNCIE         at 0 range 5 .. 5;
      MUPDIE         at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      MCMP1DE        at 0 range 16 .. 16;
      MCMP2DE        at 0 range 17 .. 17;
      MCMP3DE        at 0 range 18 .. 18;
      MCMP4DE        at 0 range 19 .. 19;
      MREPDE         at 0 range 20 .. 20;
      SYNCDE         at 0 range 21 .. 21;
      MUPDDE         at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype MCNTR_MCNT_Field is HAL.UInt16;

   --  Master Timer Counter Register
   type MCNTR_Register is record
      --  Counter value
      MCNT           : MCNTR_MCNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCNTR_Register use record
      MCNT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MPER_MPER_Field is HAL.UInt16;

   --  Master Timer Period Register
   type MPER_Register is record
      --  Master Timer Period value
      MPER           : MPER_MPER_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPER_Register use record
      MPER           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MREP_MREP_Field is HAL.UInt8;

   --  Master Timer Repetition Register
   type MREP_Register is record
      --  Master Timer Repetition counter value
      MREP          : MREP_MREP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MREP_Register use record
      MREP          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MCMP1R_MCMP1_Field is HAL.UInt16;

   --  Master Timer Compare 1 Register
   type MCMP1R_Register is record
      --  Master Timer Compare 1 value
      MCMP1          : MCMP1R_MCMP1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCMP1R_Register use record
      MCMP1          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MCMP2R_MCMP2_Field is HAL.UInt16;

   --  Master Timer Compare 2 Register
   type MCMP2R_Register is record
      --  Master Timer Compare 2 value
      MCMP2          : MCMP2R_MCMP2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCMP2R_Register use record
      MCMP2          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MCMP3R_MCMP3_Field is HAL.UInt16;

   --  Master Timer Compare 3 Register
   type MCMP3R_Register is record
      --  Master Timer Compare 3 value
      MCMP3          : MCMP3R_MCMP3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCMP3R_Register use record
      MCMP3          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MCMP4R_MCMP4_Field is HAL.UInt16;

   --  Master Timer Compare 4 Register
   type MCMP4R_Register is record
      --  Master Timer Compare 4 value
      MCMP4          : MCMP4R_MCMP4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCMP4R_Register use record
      MCMP4          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TIMACR_CKPSCx_Field is HAL.UInt3;
   subtype TIMACR_INTLVD_Field is HAL.UInt2;
   --  TIMACR_DELCMP array element
   subtype TIMACR_DELCMP_Element is HAL.UInt2;

   --  TIMACR_DELCMP array
   type TIMACR_DELCMP_Field_Array is array (2 .. 3) of TIMACR_DELCMP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for TIMACR_DELCMP
   type TIMACR_DELCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DELCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  DELCMP as an array
            Arr : TIMACR_DELCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMACR_DELCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype TIMACR_DACSYNC_Field is HAL.UInt2;
   subtype TIMACR_UPDGAT_Field is HAL.UInt4;

   --  Timerx Control Register
   type TIMACR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMACR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMACR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMACR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  TFU
      TFU            : Boolean := False;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  TBU
      TBU            : Boolean := False;
      --  TCU
      TCU            : Boolean := False;
      --  TDU
      TDU            : Boolean := False;
      --  TEU
      TEU            : Boolean := False;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMACR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMACR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMACR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      TFU            at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      TBU            at 0 range 20 .. 20;
      TCU            at 0 range 21 .. 21;
      TDU            at 0 range 22 .. 22;
      TEU            at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  TIMAISR_CMP array
   type TIMAISR_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TIMAISR_CMP
   type TIMAISR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : TIMAISR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMAISR_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TIMAISR_CPT array
   type TIMAISR_CPT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMAISR_CPT
   type TIMAISR_CPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CPT as a value
            Val : HAL.UInt2;
         when True =>
            --  CPT as an array
            Arr : TIMAISR_CPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMAISR_CPT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Interrupt Status Register
   type TIMAISR_Register is record
      --  Read-only. Compare 1 Interrupt Flag
      CMP            : TIMAISR_CMP_Field;
      --  Read-only. Repetition Interrupt Flag
      REP            : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Update Interrupt Flag
      UPD            : Boolean;
      --  Read-only. Capture1 Interrupt Flag
      CPT            : TIMAISR_CPT_Field;
      --  Read-only. Output 1 Set Interrupt Flag
      SETx1          : Boolean;
      --  Read-only. Output 1 Reset Interrupt Flag
      RSTx1          : Boolean;
      --  Read-only. Output 2 Set Interrupt Flag
      SETx2          : Boolean;
      --  Read-only. Output 2 Reset Interrupt Flag
      RSTx2          : Boolean;
      --  Read-only. Reset Interrupt Flag
      RST            : Boolean;
      --  Read-only. Delayed Protection Flag
      DLYPRT         : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Current Push Pull Status
      CPPSTAT        : Boolean;
      --  Read-only. Idle Push Pull Status
      IPPSTAT        : Boolean;
      --  Read-only. Output 1 State
      O1STAT         : Boolean;
      --  Read-only. Output 2 State
      O2STAT         : Boolean;
      --  Read-only. Output 1 Copy
      O1CPY          : Boolean;
      --  Read-only. Output 2 Copy
      O2CPY          : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMAISR_Register use record
      CMP            at 0 range 0 .. 3;
      REP            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPD            at 0 range 6 .. 6;
      CPT            at 0 range 7 .. 8;
      SETx1          at 0 range 9 .. 9;
      RSTx1          at 0 range 10 .. 10;
      SETx2          at 0 range 11 .. 11;
      RSTx2          at 0 range 12 .. 12;
      RST            at 0 range 13 .. 13;
      DLYPRT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CPPSTAT        at 0 range 16 .. 16;
      IPPSTAT        at 0 range 17 .. 17;
      O1STAT         at 0 range 18 .. 18;
      O2STAT         at 0 range 19 .. 19;
      O1CPY          at 0 range 20 .. 20;
      O2CPY          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Timerx Interrupt Clear Register
   type TIMAICR_Register is record
      --  Write-only. Compare 1 Interrupt flag Clear
      CMP1C          : Boolean := False;
      --  Write-only. Compare 2 Interrupt flag Clear
      CMP2C          : Boolean := False;
      --  Write-only. Compare 3 Interrupt flag Clear
      CMP3C          : Boolean := False;
      --  Write-only. Compare 4 Interrupt flag Clear
      CMP4C          : Boolean := False;
      --  Write-only. Repetition Interrupt flag Clear
      REPC           : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Update Interrupt flag Clear
      UPDC           : Boolean := False;
      --  Write-only. Capture1 Interrupt flag Clear
      CPT1C          : Boolean := False;
      --  Write-only. Capture2 Interrupt flag Clear
      CPT2C          : Boolean := False;
      --  Write-only. Output 1 Set flag Clear
      SET1xC         : Boolean := False;
      --  Write-only. Output 1 Reset flag Clear
      RSTx1C         : Boolean := False;
      --  Write-only. Output 2 Set flag Clear
      SET2xC         : Boolean := False;
      --  Write-only. Output 2 Reset flag Clear
      RSTx2C         : Boolean := False;
      --  Write-only. Reset Interrupt flag Clear
      RSTC           : Boolean := False;
      --  Write-only. Delayed Protection Flag Clear
      DLYPRTC        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMAICR_Register use record
      CMP1C          at 0 range 0 .. 0;
      CMP2C          at 0 range 1 .. 1;
      CMP3C          at 0 range 2 .. 2;
      CMP4C          at 0 range 3 .. 3;
      REPC           at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDC           at 0 range 6 .. 6;
      CPT1C          at 0 range 7 .. 7;
      CPT2C          at 0 range 8 .. 8;
      SET1xC         at 0 range 9 .. 9;
      RSTx1C         at 0 range 10 .. 10;
      SET2xC         at 0 range 11 .. 11;
      RSTx2C         at 0 range 12 .. 12;
      RSTC           at 0 range 13 .. 13;
      DLYPRTC        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  TIMxDIER
   type TIMADIER_Register is record
      --  CMP1IE
      CMP1IE         : Boolean := False;
      --  CMP2IE
      CMP2IE         : Boolean := False;
      --  CMP3IE
      CMP3IE         : Boolean := False;
      --  CMP4IE
      CMP4IE         : Boolean := False;
      --  REPIE
      REPIE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  UPDIE
      UPDIE          : Boolean := False;
      --  CPT1IE
      CPT1IE         : Boolean := False;
      --  CPT2IE
      CPT2IE         : Boolean := False;
      --  SET1xIE
      SET1xIE        : Boolean := False;
      --  RSTx1IE
      RSTx1IE        : Boolean := False;
      --  SETx2IE
      SETx2IE        : Boolean := False;
      --  RSTx2IE
      RSTx2IE        : Boolean := False;
      --  RSTIE
      RSTIE          : Boolean := False;
      --  DLYPRTIE
      DLYPRTIE       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  CMP1DE
      CMP1DE         : Boolean := False;
      --  CMP2DE
      CMP2DE         : Boolean := False;
      --  CMP3DE
      CMP3DE         : Boolean := False;
      --  CMP4DE
      CMP4DE         : Boolean := False;
      --  REPDE
      REPDE          : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  UPDDE
      UPDDE          : Boolean := False;
      --  CPT1DE
      CPT1DE         : Boolean := False;
      --  CPT2DE
      CPT2DE         : Boolean := False;
      --  SET1xDE
      SET1xDE        : Boolean := False;
      --  RSTx1DE
      RSTx1DE        : Boolean := False;
      --  SETx2DE
      SETx2DE        : Boolean := False;
      --  RSTx2DE
      RSTx2DE        : Boolean := False;
      --  RSTDE
      RSTDE          : Boolean := False;
      --  DLYPRTDE
      DLYPRTDE       : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMADIER_Register use record
      CMP1IE         at 0 range 0 .. 0;
      CMP2IE         at 0 range 1 .. 1;
      CMP3IE         at 0 range 2 .. 2;
      CMP4IE         at 0 range 3 .. 3;
      REPIE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDIE          at 0 range 6 .. 6;
      CPT1IE         at 0 range 7 .. 7;
      CPT2IE         at 0 range 8 .. 8;
      SET1xIE        at 0 range 9 .. 9;
      RSTx1IE        at 0 range 10 .. 10;
      SETx2IE        at 0 range 11 .. 11;
      RSTx2IE        at 0 range 12 .. 12;
      RSTIE          at 0 range 13 .. 13;
      DLYPRTIE       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CMP1DE         at 0 range 16 .. 16;
      CMP2DE         at 0 range 17 .. 17;
      CMP3DE         at 0 range 18 .. 18;
      CMP4DE         at 0 range 19 .. 19;
      REPDE          at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      UPDDE          at 0 range 22 .. 22;
      CPT1DE         at 0 range 23 .. 23;
      CPT2DE         at 0 range 24 .. 24;
      SET1xDE        at 0 range 25 .. 25;
      RSTx1DE        at 0 range 26 .. 26;
      SETx2DE        at 0 range 27 .. 27;
      RSTx2DE        at 0 range 28 .. 28;
      RSTDE          at 0 range 29 .. 29;
      DLYPRTDE       at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CNTAR_CNTx_Field is HAL.UInt16;

   --  Timerx Counter Register
   type CNTAR_Register is record
      --  Timerx Counter value
      CNTx           : CNTAR_CNTx_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTAR_Register use record
      CNTx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PERAR_PERx_Field is HAL.UInt16;

   --  Timerx Period Register
   type PERAR_Register is record
      --  Timerx Period value
      PERx           : PERAR_PERx_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERAR_Register use record
      PERx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype REPAR_REPx_Field is HAL.UInt8;

   --  Timerx Repetition Register
   type REPAR_Register is record
      --  Timerx Repetition counter value
      REPx          : REPAR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPAR_Register use record
      REPx          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CMP1AR_CMP1x_Field is HAL.UInt16;

   --  Timerx Compare 1 Register
   type CMP1AR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1AR_CMP1x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1AR_Register use record
      CMP1x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP1CAR_CMP1x_Field is HAL.UInt16;
   subtype CMP1CAR_REPx_Field is HAL.UInt8;

   --  Timerx Compare 1 Compound Register
   type CMP1CAR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CAR_CMP1x_Field := 16#0#;
      --  Timerx Repetition value (aliased from HRTIM_REPx register)
      REPx           : CMP1CAR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CAR_Register use record
      CMP1x          at 0 range 0 .. 15;
      REPx           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CMP2AR_CMP2x_Field is HAL.UInt16;

   --  Timerx Compare 2 Register
   type CMP2AR_Register is record
      --  Timerx Compare 2 value
      CMP2x          : CMP2AR_CMP2x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP2AR_Register use record
      CMP2x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP3AR_CMP3x_Field is HAL.UInt16;

   --  Timerx Compare 3 Register
   type CMP3AR_Register is record
      --  Timerx Compare 3 value
      CMP3x          : CMP3AR_CMP3x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP3AR_Register use record
      CMP3x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP4AR_CMP4x_Field is HAL.UInt16;

   --  Timerx Compare 4 Register
   type CMP4AR_Register is record
      --  Timerx Compare 4 value
      CMP4x          : CMP4AR_CMP4x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP4AR_Register use record
      CMP4x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CPT1AR_CPT1x_Field is HAL.UInt16;

   --  Timerx Capture 1 Register
   type CPT1AR_Register is record
      --  Read-only. Timerx Capture 1 value
      CPT1x          : CPT1AR_CPT1x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1AR_Register use record
      CPT1x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CPT2AR_CPT2x_Field is HAL.UInt16;

   --  Timerx Capture 2 Register
   type CPT2AR_Register is record
      --  Read-only. Timerx Capture 2 value
      CPT2x          : CPT2AR_CPT2x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2AR_Register use record
      CPT2x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DTAR_DTRx_Field is HAL.UInt9;
   subtype DTAR_DTPRSC_Field is HAL.UInt3;
   subtype DTAR_DTFx_Field is HAL.UInt9;

   --  Timerx Deadtime Register
   type DTAR_Register is record
      --  Deadtime Rising value
      DTRx           : DTAR_DTRx_Field := 16#0#;
      --  Sign Deadtime Rising value
      SDTRx          : Boolean := False;
      --  Deadtime Prescaler
      DTPRSC         : DTAR_DTPRSC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Deadtime Rising Sign Lock
      DTRSLKx        : Boolean := False;
      --  Deadtime Rising Lock
      DTRLKx         : Boolean := False;
      --  Deadtime Falling value
      DTFx           : DTAR_DTFx_Field := 16#0#;
      --  Sign Deadtime Falling value
      SDTFx          : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Deadtime Falling Sign Lock
      DTFSLKx        : Boolean := False;
      --  Deadtime Falling Lock
      DTFLKx         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTAR_Register use record
      DTRx           at 0 range 0 .. 8;
      SDTRx          at 0 range 9 .. 9;
      DTPRSC         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      DTRSLKx        at 0 range 14 .. 14;
      DTRLKx         at 0 range 15 .. 15;
      DTFx           at 0 range 16 .. 24;
      SDTFx          at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DTFSLKx        at 0 range 30 .. 30;
      DTFLKx         at 0 range 31 .. 31;
   end record;

   --  SETA1R_CMP array
   type SETA1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETA1R_CMP
   type SETA1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETA1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETA1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETA1R_MSTCMP array
   type SETA1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETA1R_MSTCMP
   type SETA1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETA1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETA1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETA1R_TIMEVNT array
   type SETA1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETA1R_TIMEVNT
   type SETA1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETA1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETA1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETA1R_EXTEVNT array
   type SETA1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETA1R_EXTEVNT
   type SETA1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETA1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETA1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Set Register
   type SETA1R_Register is record
      --  Software Set trigger
      SST     : Boolean := False;
      --  Timer A resynchronizaton
      RESYNC  : Boolean := False;
      --  Timer A Period
      PER     : Boolean := False;
      --  Timer A compare 1
      CMP     : SETA1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master Period
      MSTPER  : Boolean := False;
      --  Master Compare 1
      MSTCMP  : SETA1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer Event 1
      TIMEVNT : SETA1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT : SETA1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Registers update (transfer preload to active)
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETA1R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTA1R_CMP array
   type RSTA1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTA1R_CMP
   type RSTA1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTA1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTA1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTA1R_MSTCMP array
   type RSTA1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTA1R_MSTCMP
   type RSTA1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTA1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTA1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTA1R_TIMEVNT array
   type RSTA1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTA1R_TIMEVNT
   type RSTA1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTA1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTA1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTA1R_EXTEVNT array
   type RSTA1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTA1R_EXTEVNT
   type RSTA1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTA1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTA1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Reset Register
   type RSTA1R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTA1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTA1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTA1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTA1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTA1R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  SETA2R_CMP array
   type SETA2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETA2R_CMP
   type SETA2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETA2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETA2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETA2R_MSTCMP array
   type SETA2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETA2R_MSTCMP
   type SETA2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETA2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETA2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETA2R_TIMEVNT array
   type SETA2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETA2R_TIMEVNT
   type SETA2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETA2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETA2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETA2R_EXTEVNT array
   type SETA2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETA2R_EXTEVNT
   type SETA2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETA2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETA2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Set Register
   type SETA2R_Register is record
      --  SST
      SST     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : SETA2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : SETA2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : SETA2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : SETA2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETA2R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTA2R_CMP array
   type RSTA2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTA2R_CMP
   type RSTA2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTA2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTA2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTA2R_MSTCMP array
   type RSTA2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTA2R_MSTCMP
   type RSTA2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTA2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTA2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTA2R_TIMEVNT array
   type RSTA2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTA2R_TIMEVNT
   type RSTA2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTA2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTA2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTA2R_EXTEVNT array
   type RSTA2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTA2R_EXTEVNT
   type RSTA2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTA2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTA2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Reset Register
   type RSTA2R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTA2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTA2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTA2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTA2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTA2R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   subtype EEFAR1_EE1FLTR_Field is HAL.UInt4;
   subtype EEFAR1_EE2FLTR_Field is HAL.UInt4;
   subtype EEFAR1_EE3FLTR_Field is HAL.UInt4;
   subtype EEFAR1_EE4FLTR_Field is HAL.UInt4;
   subtype EEFAR1_EE5FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 1
   type EEFAR1_Register is record
      --  External Event 1 latch
      EE1LTCH        : Boolean := False;
      --  External Event 1 filter
      EE1FLTR        : EEFAR1_EE1FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 2 latch
      EE2LTCH        : Boolean := False;
      --  External Event 2 filter
      EE2FLTR        : EEFAR1_EE2FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 3 latch
      EE3LTCH        : Boolean := False;
      --  External Event 3 filter
      EE3FLTR        : EEFAR1_EE3FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 4 latch
      EE4LTCH        : Boolean := False;
      --  External Event 4 filter
      EE4FLTR        : EEFAR1_EE4FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 5 latch
      EE5LTCH        : Boolean := False;
      --  External Event 5 filter
      EE5FLTR        : EEFAR1_EE5FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFAR1_Register use record
      EE1LTCH        at 0 range 0 .. 0;
      EE1FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE2LTCH        at 0 range 6 .. 6;
      EE2FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE3LTCH        at 0 range 12 .. 12;
      EE3FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE4LTCH        at 0 range 18 .. 18;
      EE4FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE5LTCH        at 0 range 24 .. 24;
      EE5FLTR        at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EEFAR2_EE6FLTR_Field is HAL.UInt4;
   subtype EEFAR2_EE7FLTR_Field is HAL.UInt4;
   subtype EEFAR2_EE8FLTR_Field is HAL.UInt4;
   subtype EEFAR2_EE9FLTR_Field is HAL.UInt4;
   subtype EEFAR2_EE10FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 2
   type EEFAR2_Register is record
      --  External Event 6 latch
      EE6LTCH        : Boolean := False;
      --  External Event 6 filter
      EE6FLTR        : EEFAR2_EE6FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 7 latch
      EE7LTCH        : Boolean := False;
      --  External Event 7 filter
      EE7FLTR        : EEFAR2_EE7FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 8 latch
      EE8LTCH        : Boolean := False;
      --  External Event 8 filter
      EE8FLTR        : EEFAR2_EE8FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 9 latch
      EE9LTCH        : Boolean := False;
      --  External Event 9 filter
      EE9FLTR        : EEFAR2_EE9FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 10 latch
      EE10LTCH       : Boolean := False;
      --  External Event 10 filter
      EE10FLTR       : EEFAR2_EE10FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFAR2_Register use record
      EE6LTCH        at 0 range 0 .. 0;
      EE6FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7LTCH        at 0 range 6 .. 6;
      EE7FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8LTCH        at 0 range 12 .. 12;
      EE8FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9LTCH        at 0 range 18 .. 18;
      EE9FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10LTCH       at 0 range 24 .. 24;
      EE10FLTR       at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  RSTAR_CMP array
   type RSTAR_CMP_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RSTAR_CMP
   type RSTAR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  CMP as an array
            Arr : RSTAR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RSTAR_CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RSTAR_MSTCMP array
   type RSTAR_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTAR_MSTCMP
   type RSTAR_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTAR_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTAR_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTAR_EXTEVNT array
   type RSTAR_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTAR_EXTEVNT
   type RSTAR_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTAR_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTAR_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  RSTAR_TIMBCMP array
   type RSTAR_TIMBCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTAR_TIMBCMP
   type RSTAR_TIMBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMBCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMBCMP as an array
            Arr : RSTAR_TIMBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTAR_TIMBCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTAR_TIMCCMP array
   type RSTAR_TIMCCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTAR_TIMCCMP
   type RSTAR_TIMCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMCCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMCCMP as an array
            Arr : RSTAR_TIMCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTAR_TIMCCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTAR_TIMDCMP array
   type RSTAR_TIMDCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTAR_TIMDCMP
   type RSTAR_TIMDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMDCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMDCMP as an array
            Arr : RSTAR_TIMDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTAR_TIMDCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTAR_TIMECMP array
   type RSTAR_TIMECMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTAR_TIMECMP
   type RSTAR_TIMECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMECMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMECMP as an array
            Arr : RSTAR_TIMECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTAR_TIMECMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TimerA Reset Register
   type RSTAR_Register is record
      --  Timer A Update reset
      TIMFCMP1 : Boolean := False;
      --  Timer A Update reset
      UPDT     : Boolean := False;
      --  Timer A compare 2 reset
      CMP      : RSTAR_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master timer Period
      MSTPER   : Boolean := False;
      --  Master compare 1
      MSTCMP   : RSTAR_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT  : RSTAR_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Timer B Compare 1
      TIMBCMP  : RSTAR_TIMBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C Compare 1
      TIMCCMP  : RSTAR_TIMCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D Compare 1
      TIMDCMP  : RSTAR_TIMDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E Compare 1
      TIMECMP  : RSTAR_TIMECMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer F Compare 2
      TIMFCPM2 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTAR_Register use record
      TIMFCMP1 at 0 range 0 .. 0;
      UPDT     at 0 range 1 .. 1;
      CMP      at 0 range 2 .. 3;
      MSTPER   at 0 range 4 .. 4;
      MSTCMP   at 0 range 5 .. 8;
      EXTEVNT  at 0 range 9 .. 18;
      TIMBCMP  at 0 range 19 .. 21;
      TIMCCMP  at 0 range 22 .. 24;
      TIMDCMP  at 0 range 25 .. 27;
      TIMECMP  at 0 range 28 .. 30;
      TIMFCPM2 at 0 range 31 .. 31;
   end record;

   subtype CHPAR_CHPFRQ_Field is HAL.UInt4;
   subtype CHPAR_CHPDTY_Field is HAL.UInt3;
   subtype CHPAR_STRTPW_Field is HAL.UInt4;

   --  Timerx Chopper Register
   type CHPAR_Register is record
      --  Timerx carrier frequency value
      CHPFRQ         : CHPAR_CHPFRQ_Field := 16#0#;
      --  Timerx chopper duty cycle value
      CHPDTY         : CHPAR_CHPDTY_Field := 16#0#;
      --  STRTPW
      STRTPW         : CHPAR_STRTPW_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHPAR_Register use record
      CHPFRQ         at 0 range 0 .. 3;
      CHPDTY         at 0 range 4 .. 6;
      STRTPW         at 0 range 7 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CPT1ACR_TFCMP array
   type CPT1ACR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ACR_TFCMP
   type CPT1ACR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT1ACR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ACR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ACR_TBCMP array
   type CPT1ACR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ACR_TBCMP
   type CPT1ACR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT1ACR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ACR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ACR_TCCMP array
   type CPT1ACR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ACR_TCCMP
   type CPT1ACR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT1ACR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ACR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ACR_TDCMP array
   type CPT1ACR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ACR_TDCMP
   type CPT1ACR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT1ACR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ACR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ACR_TECMP array
   type CPT1ACR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ACR_TECMP
   type CPT1ACR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT1ACR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ACR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 1 Control Register
   type CPT1ACR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT1ACR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT1ACR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT1ACR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT1ACR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT1ACR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1ACR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TF1SET    at 0 range 12 .. 12;
      TF1RST    at 0 range 13 .. 13;
      TFCMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   --  CPT2ACR_TFCMP array
   type CPT2ACR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ACR_TFCMP
   type CPT2ACR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT2ACR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ACR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ACR_TBCMP array
   type CPT2ACR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ACR_TBCMP
   type CPT2ACR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT2ACR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ACR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ACR_TCCMP array
   type CPT2ACR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ACR_TCCMP
   type CPT2ACR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT2ACR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ACR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ACR_TDCMP array
   type CPT2ACR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ACR_TDCMP
   type CPT2ACR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT2ACR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ACR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ACR_TECMP array
   type CPT2ACR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ACR_TECMP
   type CPT2ACR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT2ACR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ACR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 2 Control Register
   type CPT2ACR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT2ACR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT2ACR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT2ACR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT2ACR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT2ACR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2ACR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TF1SET    at 0 range 12 .. 12;
      TF1RST    at 0 range 13 .. 13;
      TFCMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   subtype OUTAR_FAULT1_Field is HAL.UInt2;
   subtype OUTAR_DLYPRT_Field is HAL.UInt3;
   subtype OUTAR_FAULT2_Field is HAL.UInt2;

   --  Timerx Output Register
   type OUTAR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Output 1 polarity
      POL1           : Boolean := False;
      --  Output 1 Idle mode
      IDLEM1         : Boolean := False;
      --  Output 1 Idle State
      IDLES1         : Boolean := False;
      --  Output 1 Fault state
      FAULT1         : OUTAR_FAULT1_Field := 16#0#;
      --  Output 1 Chopper enable
      CHP1           : Boolean := False;
      --  Output 1 Deadtime upon burst mode Idle entry
      DIDL1          : Boolean := False;
      --  Deadtime enable
      DTEN           : Boolean := False;
      --  Delayed Protection Enable
      DLYPRTEN       : Boolean := False;
      --  Delayed Protection
      DLYPRT         : OUTAR_DLYPRT_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Balanced Idle Automatic Resume
      BIAR           : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Output 2 polarity
      POL2           : Boolean := False;
      --  Output 2 Idle mode
      IDLEM2         : Boolean := False;
      --  Output 2 Idle State
      IDLES2         : Boolean := False;
      --  Output 2 Fault state
      FAULT2         : OUTAR_FAULT2_Field := 16#0#;
      --  Output 2 Chopper enable
      CHP2           : Boolean := False;
      --  Output 2 Deadtime upon burst mode Idle entry
      DIDL2          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTAR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      POL1           at 0 range 1 .. 1;
      IDLEM1         at 0 range 2 .. 2;
      IDLES1         at 0 range 3 .. 3;
      FAULT1         at 0 range 4 .. 5;
      CHP1           at 0 range 6 .. 6;
      DIDL1          at 0 range 7 .. 7;
      DTEN           at 0 range 8 .. 8;
      DLYPRTEN       at 0 range 9 .. 9;
      DLYPRT         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      BIAR           at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      POL2           at 0 range 17 .. 17;
      IDLEM2         at 0 range 18 .. 18;
      IDLES2         at 0 range 19 .. 19;
      FAULT2         at 0 range 20 .. 21;
      CHP2           at 0 range 22 .. 22;
      DIDL2          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Timerx Fault Register
   type FLTAR_Register is record
      --  Fault 1 enable
      FLT1EN        : Boolean := False;
      --  Fault 2 enable
      FLT2EN        : Boolean := False;
      --  Fault 3 enable
      FLT3EN        : Boolean := False;
      --  Fault 4 enable
      FLT4EN        : Boolean := False;
      --  Fault 5 enable
      FLT5EN        : Boolean := False;
      --  Fault 6 enable
      FLT6EN        : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  Fault sources Lock
      FLTLCK        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTAR_Register use record
      FLT1EN        at 0 range 0 .. 0;
      FLT2EN        at 0 range 1 .. 1;
      FLT3EN        at 0 range 2 .. 2;
      FLT4EN        at 0 range 3 .. 3;
      FLT5EN        at 0 range 4 .. 4;
      FLT6EN        at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      FLTLCK        at 0 range 31 .. 31;
   end record;

   subtype TIMACR2_ROM_Field is HAL.UInt2;
   subtype TIMACR2_OUTROM_Field is HAL.UInt2;
   subtype TIMACR2_ADROM_Field is HAL.UInt2;
   subtype TIMACR2_BMROM_Field is HAL.UInt2;
   subtype TIMACR2_FEROM_Field is HAL.UInt2;

   --  TIMACR2_GTCMP array
   type TIMACR2_GTCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMACR2_GTCMP
   type TIMACR2_GTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GTCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  GTCMP as an array
            Arr : TIMACR2_GTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMACR2_GTCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  HRTIM Timerx Control Register 2
   type TIMACR2_Register is record
      --  Dual Channel DAC trigger enable
      DCDE           : Boolean := False;
      --  Dual Channel DAC Step trigger
      DCDS           : Boolean := False;
      --  Dual Channel DAC Reset trigger
      DCDR           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Up-Down Mode
      UDM            : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Roll-Over Mode
      ROM            : TIMACR2_ROM_Field := 16#0#;
      --  Output Roll-Over Mode
      OUTROM         : TIMACR2_OUTROM_Field := 16#0#;
      --  ADC Roll-Over Mode
      ADROM          : TIMACR2_ADROM_Field := 16#0#;
      --  Burst Mode Roll-Over Mode
      BMROM          : TIMACR2_BMROM_Field := 16#0#;
      --  Fault and Event Roll-Over Mode
      FEROM          : TIMACR2_FEROM_Field := 16#0#;
      --  Greater than Compare 1 PWM mode
      GTCMP          : TIMACR2_GTCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Triggered-half mode
      TRGHLF         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMACR2_Register use record
      DCDE           at 0 range 0 .. 0;
      DCDS           at 0 range 1 .. 1;
      DCDR           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      UDM            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ROM            at 0 range 6 .. 7;
      OUTROM         at 0 range 8 .. 9;
      ADROM          at 0 range 10 .. 11;
      BMROM          at 0 range 12 .. 13;
      FEROM          at 0 range 14 .. 15;
      GTCMP          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TRGHLF         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype EEFAR3_EEVASEL_Field is HAL.UInt4;
   subtype EEFAR3_EEVACNT_Field is HAL.UInt6;

   --  HRTIM Timerx External Event Filtering Register 3
   type EEFAR3_Register is record
      --  External Event A Counter Enable
      EEVACE         : Boolean := False;
      --  External Event A Counter Reset
      EEVACRES       : Boolean := False;
      --  External Event A Reset Mode
      EEVARSTM       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  External Event A Selection
      EEVASEL        : EEFAR3_EEVASEL_Field := 16#0#;
      --  External Event A counter
      EEVACNT        : EEFAR3_EEVACNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFAR3_Register use record
      EEVACE         at 0 range 0 .. 0;
      EEVACRES       at 0 range 1 .. 1;
      EEVARSTM       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EEVASEL        at 0 range 4 .. 7;
      EEVACNT        at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TIMBCR_CKPSCx_Field is HAL.UInt3;
   subtype TIMBCR_INTLVD_Field is HAL.UInt2;
   --  TIMBCR_DELCMP array element
   subtype TIMBCR_DELCMP_Element is HAL.UInt2;

   --  TIMBCR_DELCMP array
   type TIMBCR_DELCMP_Field_Array is array (2 .. 3) of TIMBCR_DELCMP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for TIMBCR_DELCMP
   type TIMBCR_DELCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DELCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  DELCMP as an array
            Arr : TIMBCR_DELCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMBCR_DELCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype TIMBCR_DACSYNC_Field is HAL.UInt2;
   subtype TIMBCR_UPDGAT_Field is HAL.UInt4;

   --  Timerx Control Register
   type TIMBCR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMBCR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMBCR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMBCR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  TFU
      TFU            : Boolean := False;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  TAU
      TAU            : Boolean := False;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  TCU
      TCU            : Boolean := False;
      --  TDU
      TDU            : Boolean := False;
      --  TEU
      TEU            : Boolean := False;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMBCR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMBCR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMBCR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      TFU            at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      TAU            at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      TCU            at 0 range 21 .. 21;
      TDU            at 0 range 22 .. 22;
      TEU            at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  TIMBISR_CMP array
   type TIMBISR_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TIMBISR_CMP
   type TIMBISR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : TIMBISR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMBISR_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TIMBISR_CPT array
   type TIMBISR_CPT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMBISR_CPT
   type TIMBISR_CPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CPT as a value
            Val : HAL.UInt2;
         when True =>
            --  CPT as an array
            Arr : TIMBISR_CPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMBISR_CPT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Interrupt Status Register
   type TIMBISR_Register is record
      --  Read-only. Compare 1 Interrupt Flag
      CMP            : TIMBISR_CMP_Field;
      --  Read-only. Repetition Interrupt Flag
      REP            : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Update Interrupt Flag
      UPD            : Boolean;
      --  Read-only. Capture1 Interrupt Flag
      CPT            : TIMBISR_CPT_Field;
      --  Read-only. Output 1 Set Interrupt Flag
      SETx1          : Boolean;
      --  Read-only. Output 1 Reset Interrupt Flag
      RSTx1          : Boolean;
      --  Read-only. Output 2 Set Interrupt Flag
      SETx2          : Boolean;
      --  Read-only. Output 2 Reset Interrupt Flag
      RSTx2          : Boolean;
      --  Read-only. Reset Interrupt Flag
      RST            : Boolean;
      --  Read-only. Delayed Protection Flag
      DLYPRT         : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Current Push Pull Status
      CPPSTAT        : Boolean;
      --  Read-only. Idle Push Pull Status
      IPPSTAT        : Boolean;
      --  Read-only. Output 1 State
      O1STAT         : Boolean;
      --  Read-only. Output 2 State
      O2STAT         : Boolean;
      --  Read-only. Output 1 Copy
      O1CPY          : Boolean;
      --  Read-only. Output 2 Copy
      O2CPY          : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMBISR_Register use record
      CMP            at 0 range 0 .. 3;
      REP            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPD            at 0 range 6 .. 6;
      CPT            at 0 range 7 .. 8;
      SETx1          at 0 range 9 .. 9;
      RSTx1          at 0 range 10 .. 10;
      SETx2          at 0 range 11 .. 11;
      RSTx2          at 0 range 12 .. 12;
      RST            at 0 range 13 .. 13;
      DLYPRT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CPPSTAT        at 0 range 16 .. 16;
      IPPSTAT        at 0 range 17 .. 17;
      O1STAT         at 0 range 18 .. 18;
      O2STAT         at 0 range 19 .. 19;
      O1CPY          at 0 range 20 .. 20;
      O2CPY          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Timerx Interrupt Clear Register
   type TIMBICR_Register is record
      --  Write-only. Compare 1 Interrupt flag Clear
      CMP1C          : Boolean := False;
      --  Write-only. Compare 2 Interrupt flag Clear
      CMP2C          : Boolean := False;
      --  Write-only. Compare 3 Interrupt flag Clear
      CMP3C          : Boolean := False;
      --  Write-only. Compare 4 Interrupt flag Clear
      CMP4C          : Boolean := False;
      --  Write-only. Repetition Interrupt flag Clear
      REPC           : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Update Interrupt flag Clear
      UPDC           : Boolean := False;
      --  Write-only. Capture1 Interrupt flag Clear
      CPT1C          : Boolean := False;
      --  Write-only. Capture2 Interrupt flag Clear
      CPT2C          : Boolean := False;
      --  Write-only. Output 1 Set flag Clear
      SET1xC         : Boolean := False;
      --  Write-only. Output 1 Reset flag Clear
      RSTx1C         : Boolean := False;
      --  Write-only. Output 2 Set flag Clear
      SET2xC         : Boolean := False;
      --  Write-only. Output 2 Reset flag Clear
      RSTx2C         : Boolean := False;
      --  Write-only. Reset Interrupt flag Clear
      RSTC           : Boolean := False;
      --  Write-only. Delayed Protection Flag Clear
      DLYPRTC        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMBICR_Register use record
      CMP1C          at 0 range 0 .. 0;
      CMP2C          at 0 range 1 .. 1;
      CMP3C          at 0 range 2 .. 2;
      CMP4C          at 0 range 3 .. 3;
      REPC           at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDC           at 0 range 6 .. 6;
      CPT1C          at 0 range 7 .. 7;
      CPT2C          at 0 range 8 .. 8;
      SET1xC         at 0 range 9 .. 9;
      RSTx1C         at 0 range 10 .. 10;
      SET2xC         at 0 range 11 .. 11;
      RSTx2C         at 0 range 12 .. 12;
      RSTC           at 0 range 13 .. 13;
      DLYPRTC        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  TIMxDIER
   type TIMBDIER_Register is record
      --  CMP1IE
      CMP1IE         : Boolean := False;
      --  CMP2IE
      CMP2IE         : Boolean := False;
      --  CMP3IE
      CMP3IE         : Boolean := False;
      --  CMP4IE
      CMP4IE         : Boolean := False;
      --  REPIE
      REPIE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  UPDIE
      UPDIE          : Boolean := False;
      --  CPT1IE
      CPT1IE         : Boolean := False;
      --  CPT2IE
      CPT2IE         : Boolean := False;
      --  SET1xIE
      SET1xIE        : Boolean := False;
      --  RSTx1IE
      RSTx1IE        : Boolean := False;
      --  SETx2IE
      SETx2IE        : Boolean := False;
      --  RSTx2IE
      RSTx2IE        : Boolean := False;
      --  RSTIE
      RSTIE          : Boolean := False;
      --  DLYPRTIE
      DLYPRTIE       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  CMP1DE
      CMP1DE         : Boolean := False;
      --  CMP2DE
      CMP2DE         : Boolean := False;
      --  CMP3DE
      CMP3DE         : Boolean := False;
      --  CMP4DE
      CMP4DE         : Boolean := False;
      --  REPDE
      REPDE          : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  UPDDE
      UPDDE          : Boolean := False;
      --  CPT1DE
      CPT1DE         : Boolean := False;
      --  CPT2DE
      CPT2DE         : Boolean := False;
      --  SET1xDE
      SET1xDE        : Boolean := False;
      --  RSTx1DE
      RSTx1DE        : Boolean := False;
      --  SETx2DE
      SETx2DE        : Boolean := False;
      --  RSTx2DE
      RSTx2DE        : Boolean := False;
      --  RSTDE
      RSTDE          : Boolean := False;
      --  DLYPRTDE
      DLYPRTDE       : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMBDIER_Register use record
      CMP1IE         at 0 range 0 .. 0;
      CMP2IE         at 0 range 1 .. 1;
      CMP3IE         at 0 range 2 .. 2;
      CMP4IE         at 0 range 3 .. 3;
      REPIE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDIE          at 0 range 6 .. 6;
      CPT1IE         at 0 range 7 .. 7;
      CPT2IE         at 0 range 8 .. 8;
      SET1xIE        at 0 range 9 .. 9;
      RSTx1IE        at 0 range 10 .. 10;
      SETx2IE        at 0 range 11 .. 11;
      RSTx2IE        at 0 range 12 .. 12;
      RSTIE          at 0 range 13 .. 13;
      DLYPRTIE       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CMP1DE         at 0 range 16 .. 16;
      CMP2DE         at 0 range 17 .. 17;
      CMP3DE         at 0 range 18 .. 18;
      CMP4DE         at 0 range 19 .. 19;
      REPDE          at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      UPDDE          at 0 range 22 .. 22;
      CPT1DE         at 0 range 23 .. 23;
      CPT2DE         at 0 range 24 .. 24;
      SET1xDE        at 0 range 25 .. 25;
      RSTx1DE        at 0 range 26 .. 26;
      SETx2DE        at 0 range 27 .. 27;
      RSTx2DE        at 0 range 28 .. 28;
      RSTDE          at 0 range 29 .. 29;
      DLYPRTDE       at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CNTR_CNTx_Field is HAL.UInt16;

   --  Timerx Counter Register
   type CNTR_Register is record
      --  Timerx Counter value
      CNTx           : CNTR_CNTx_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTR_Register use record
      CNTx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PERBR_PERx_Field is HAL.UInt16;

   --  Timerx Period Register
   type PERBR_Register is record
      --  Timerx Period value
      PERx           : PERBR_PERx_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERBR_Register use record
      PERx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype REPBR_REPx_Field is HAL.UInt8;

   --  Timerx Repetition Register
   type REPBR_Register is record
      --  Timerx Repetition counter value
      REPx          : REPBR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPBR_Register use record
      REPx          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CMP1BR_CMP1x_Field is HAL.UInt16;

   --  Timerx Compare 1 Register
   type CMP1BR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1BR_CMP1x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1BR_Register use record
      CMP1x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP1CBR_CMP1x_Field is HAL.UInt16;
   subtype CMP1CBR_REPx_Field is HAL.UInt8;

   --  Timerx Compare 1 Compound Register
   type CMP1CBR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CBR_CMP1x_Field := 16#0#;
      --  Timerx Repetition value (aliased from HRTIM_REPx register)
      REPx           : CMP1CBR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CBR_Register use record
      CMP1x          at 0 range 0 .. 15;
      REPx           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CMP2BR_CMP2x_Field is HAL.UInt16;

   --  Timerx Compare 2 Register
   type CMP2BR_Register is record
      --  Timerx Compare 2 value
      CMP2x          : CMP2BR_CMP2x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP2BR_Register use record
      CMP2x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP3BR_CMP3x_Field is HAL.UInt16;

   --  Timerx Compare 3 Register
   type CMP3BR_Register is record
      --  Timerx Compare 3 value
      CMP3x          : CMP3BR_CMP3x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP3BR_Register use record
      CMP3x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP4BR_CMP4x_Field is HAL.UInt16;

   --  Timerx Compare 4 Register
   type CMP4BR_Register is record
      --  Timerx Compare 4 value
      CMP4x          : CMP4BR_CMP4x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP4BR_Register use record
      CMP4x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CPT1BR_CPT1x_Field is HAL.UInt16;

   --  Timerx Capture 1 Register
   type CPT1BR_Register is record
      --  Read-only. Timerx Capture 1 value
      CPT1x          : CPT1BR_CPT1x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1BR_Register use record
      CPT1x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CPT2BR_CPT2x_Field is HAL.UInt16;

   --  Timerx Capture 2 Register
   type CPT2BR_Register is record
      --  Read-only. Timerx Capture 2 value
      CPT2x          : CPT2BR_CPT2x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2BR_Register use record
      CPT2x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DTBR_DTRx_Field is HAL.UInt9;
   subtype DTBR_DTPRSC_Field is HAL.UInt3;
   subtype DTBR_DTFx_Field is HAL.UInt9;

   --  Timerx Deadtime Register
   type DTBR_Register is record
      --  Deadtime Rising value
      DTRx           : DTBR_DTRx_Field := 16#0#;
      --  Sign Deadtime Rising value
      SDTRx          : Boolean := False;
      --  Deadtime Prescaler
      DTPRSC         : DTBR_DTPRSC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Deadtime Rising Sign Lock
      DTRSLKx        : Boolean := False;
      --  Deadtime Rising Lock
      DTRLKx         : Boolean := False;
      --  Deadtime Falling value
      DTFx           : DTBR_DTFx_Field := 16#0#;
      --  Sign Deadtime Falling value
      SDTFx          : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Deadtime Falling Sign Lock
      DTFSLKx        : Boolean := False;
      --  Deadtime Falling Lock
      DTFLKx         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTBR_Register use record
      DTRx           at 0 range 0 .. 8;
      SDTRx          at 0 range 9 .. 9;
      DTPRSC         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      DTRSLKx        at 0 range 14 .. 14;
      DTRLKx         at 0 range 15 .. 15;
      DTFx           at 0 range 16 .. 24;
      SDTFx          at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DTFSLKx        at 0 range 30 .. 30;
      DTFLKx         at 0 range 31 .. 31;
   end record;

   --  SETB1R_CMP array
   type SETB1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETB1R_CMP
   type SETB1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETB1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETB1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETB1R_MSTCMP array
   type SETB1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETB1R_MSTCMP
   type SETB1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETB1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETB1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETB1R_TIMEVNT array
   type SETB1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETB1R_TIMEVNT
   type SETB1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETB1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETB1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETB1R_EXTEVNT array
   type SETB1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETB1R_EXTEVNT
   type SETB1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETB1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETB1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Set Register
   type SETB1R_Register is record
      --  Software Set trigger
      SST     : Boolean := False;
      --  Timer A resynchronizaton
      RESYNC  : Boolean := False;
      --  Timer A Period
      PER     : Boolean := False;
      --  Timer A compare 1
      CMP     : SETB1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master Period
      MSTPER  : Boolean := False;
      --  Master Compare 1
      MSTCMP  : SETB1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer Event 1
      TIMEVNT : SETB1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT : SETB1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Registers update (transfer preload to active)
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETB1R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTB1R_CMP array
   type RSTB1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTB1R_CMP
   type RSTB1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTB1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTB1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTB1R_MSTCMP array
   type RSTB1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTB1R_MSTCMP
   type RSTB1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTB1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTB1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTB1R_TIMEVNT array
   type RSTB1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTB1R_TIMEVNT
   type RSTB1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTB1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTB1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTB1R_EXTEVNT array
   type RSTB1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTB1R_EXTEVNT
   type RSTB1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTB1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTB1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Reset Register
   type RSTB1R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTB1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTB1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTB1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTB1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTB1R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  SETB2R_CMP array
   type SETB2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETB2R_CMP
   type SETB2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETB2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETB2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETB2R_MSTCMP array
   type SETB2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETB2R_MSTCMP
   type SETB2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETB2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETB2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETB2R_TIMEVNT array
   type SETB2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETB2R_TIMEVNT
   type SETB2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETB2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETB2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETB2R_EXTEVNT array
   type SETB2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETB2R_EXTEVNT
   type SETB2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETB2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETB2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Set Register
   type SETB2R_Register is record
      --  SST
      SST     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : SETB2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : SETB2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : SETB2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : SETB2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETB2R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTB2R_CMP array
   type RSTB2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTB2R_CMP
   type RSTB2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTB2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTB2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTB2R_MSTCMP array
   type RSTB2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTB2R_MSTCMP
   type RSTB2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTB2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTB2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTB2R_TIMEVNT array
   type RSTB2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTB2R_TIMEVNT
   type RSTB2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTB2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTB2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTB2R_EXTEVNT array
   type RSTB2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTB2R_EXTEVNT
   type RSTB2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTB2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTB2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Reset Register
   type RSTB2R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTB2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTB2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTB2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTB2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTB2R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   subtype EEFBR1_EE1FLTR_Field is HAL.UInt4;
   subtype EEFBR1_EE2FLTR_Field is HAL.UInt4;
   subtype EEFBR1_EE3FLTR_Field is HAL.UInt4;
   subtype EEFBR1_EE4FLTR_Field is HAL.UInt4;
   subtype EEFBR1_EE5FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 1
   type EEFBR1_Register is record
      --  External Event 1 latch
      EE1LTCH        : Boolean := False;
      --  External Event 1 filter
      EE1FLTR        : EEFBR1_EE1FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 2 latch
      EE2LTCH        : Boolean := False;
      --  External Event 2 filter
      EE2FLTR        : EEFBR1_EE2FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 3 latch
      EE3LTCH        : Boolean := False;
      --  External Event 3 filter
      EE3FLTR        : EEFBR1_EE3FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 4 latch
      EE4LTCH        : Boolean := False;
      --  External Event 4 filter
      EE4FLTR        : EEFBR1_EE4FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 5 latch
      EE5LTCH        : Boolean := False;
      --  External Event 5 filter
      EE5FLTR        : EEFBR1_EE5FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFBR1_Register use record
      EE1LTCH        at 0 range 0 .. 0;
      EE1FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE2LTCH        at 0 range 6 .. 6;
      EE2FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE3LTCH        at 0 range 12 .. 12;
      EE3FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE4LTCH        at 0 range 18 .. 18;
      EE4FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE5LTCH        at 0 range 24 .. 24;
      EE5FLTR        at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EEFBR2_EE6FLTR_Field is HAL.UInt4;
   subtype EEFBR2_EE7FLTR_Field is HAL.UInt4;
   subtype EEFBR2_EE8FLTR_Field is HAL.UInt4;
   subtype EEFBR2_EE9FLTR_Field is HAL.UInt4;
   subtype EEFBR2_EE10FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 2
   type EEFBR2_Register is record
      --  External Event 6 latch
      EE6LTCH        : Boolean := False;
      --  External Event 6 filter
      EE6FLTR        : EEFBR2_EE6FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 7 latch
      EE7LTCH        : Boolean := False;
      --  External Event 7 filter
      EE7FLTR        : EEFBR2_EE7FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 8 latch
      EE8LTCH        : Boolean := False;
      --  External Event 8 filter
      EE8FLTR        : EEFBR2_EE8FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 9 latch
      EE9LTCH        : Boolean := False;
      --  External Event 9 filter
      EE9FLTR        : EEFBR2_EE9FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 10 latch
      EE10LTCH       : Boolean := False;
      --  External Event 10 filter
      EE10FLTR       : EEFBR2_EE10FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFBR2_Register use record
      EE6LTCH        at 0 range 0 .. 0;
      EE6FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7LTCH        at 0 range 6 .. 6;
      EE7FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8LTCH        at 0 range 12 .. 12;
      EE8FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9LTCH        at 0 range 18 .. 18;
      EE9FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10LTCH       at 0 range 24 .. 24;
      EE10FLTR       at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  RSTBR_CMP array
   type RSTBR_CMP_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RSTBR_CMP
   type RSTBR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  CMP as an array
            Arr : RSTBR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RSTBR_CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RSTBR_MSTCMP array
   type RSTBR_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTBR_MSTCMP
   type RSTBR_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTBR_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTBR_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTBR_EXTEVNT array
   type RSTBR_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTBR_EXTEVNT
   type RSTBR_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTBR_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTBR_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  RSTBR_TIMACMP array
   type RSTBR_TIMACMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTBR_TIMACMP
   type RSTBR_TIMACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMACMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMACMP as an array
            Arr : RSTBR_TIMACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTBR_TIMACMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTBR_TIMCCMP array
   type RSTBR_TIMCCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTBR_TIMCCMP
   type RSTBR_TIMCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMCCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMCCMP as an array
            Arr : RSTBR_TIMCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTBR_TIMCCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTBR_TIMDCMP array
   type RSTBR_TIMDCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTBR_TIMDCMP
   type RSTBR_TIMDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMDCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMDCMP as an array
            Arr : RSTBR_TIMDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTBR_TIMDCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTBR_TIMECMP array
   type RSTBR_TIMECMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTBR_TIMECMP
   type RSTBR_TIMECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMECMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMECMP as an array
            Arr : RSTBR_TIMECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTBR_TIMECMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TimerA Reset Register
   type RSTBR_Register is record
      --  Timer A Update reset
      TIMFCMP1 : Boolean := False;
      --  Timer A Update reset
      UPDT     : Boolean := False;
      --  Timer A compare 2 reset
      CMP      : RSTBR_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master timer Period
      MSTPER   : Boolean := False;
      --  Master compare 1
      MSTCMP   : RSTBR_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT  : RSTBR_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Timer A Compare 1
      TIMACMP  : RSTBR_TIMACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C Compare 1
      TIMCCMP  : RSTBR_TIMCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D Compare 1
      TIMDCMP  : RSTBR_TIMDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E Compare 1
      TIMECMP  : RSTBR_TIMECMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer F Compare 2
      TIMFCPM2 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTBR_Register use record
      TIMFCMP1 at 0 range 0 .. 0;
      UPDT     at 0 range 1 .. 1;
      CMP      at 0 range 2 .. 3;
      MSTPER   at 0 range 4 .. 4;
      MSTCMP   at 0 range 5 .. 8;
      EXTEVNT  at 0 range 9 .. 18;
      TIMACMP  at 0 range 19 .. 21;
      TIMCCMP  at 0 range 22 .. 24;
      TIMDCMP  at 0 range 25 .. 27;
      TIMECMP  at 0 range 28 .. 30;
      TIMFCPM2 at 0 range 31 .. 31;
   end record;

   subtype CHPBR_CHPFRQ_Field is HAL.UInt4;
   subtype CHPBR_CHPDTY_Field is HAL.UInt3;
   subtype CHPBR_STRTPW_Field is HAL.UInt4;

   --  Timerx Chopper Register
   type CHPBR_Register is record
      --  Timerx carrier frequency value
      CHPFRQ         : CHPBR_CHPFRQ_Field := 16#0#;
      --  Timerx chopper duty cycle value
      CHPDTY         : CHPBR_CHPDTY_Field := 16#0#;
      --  STRTPW
      STRTPW         : CHPBR_STRTPW_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHPBR_Register use record
      CHPFRQ         at 0 range 0 .. 3;
      CHPDTY         at 0 range 4 .. 6;
      STRTPW         at 0 range 7 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CPT1BCR_TACMP array
   type CPT1BCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1BCR_TACMP
   type CPT1BCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT1BCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1BCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1BCR_TFCMP array
   type CPT1BCR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1BCR_TFCMP
   type CPT1BCR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT1BCR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1BCR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1BCR_TCCMP array
   type CPT1BCR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1BCR_TCCMP
   type CPT1BCR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT1BCR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1BCR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1BCR_TDCMP array
   type CPT1BCR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1BCR_TDCMP
   type CPT1BCR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT1BCR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1BCR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1BCR_TECMP array
   type CPT1BCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1BCR_TECMP
   type CPT1BCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT1BCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1BCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 1 Control Register
   type CPT1BCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT1BCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT1BCR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT1BCR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT1BCR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT1BCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1BCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TF1SET    at 0 range 16 .. 16;
      TF1RST    at 0 range 17 .. 17;
      TFCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   --  CPT2BCR_TACMP array
   type CPT2BCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2BCR_TACMP
   type CPT2BCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT2BCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2BCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2BCR_TFCMP array
   type CPT2BCR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2BCR_TFCMP
   type CPT2BCR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT2BCR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2BCR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2BCR_TCCMP array
   type CPT2BCR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2BCR_TCCMP
   type CPT2BCR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT2BCR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2BCR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2BCR_TDCMP array
   type CPT2BCR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2BCR_TDCMP
   type CPT2BCR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT2BCR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2BCR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2BCR_TECMP array
   type CPT2BCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2BCR_TECMP
   type CPT2BCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT2BCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2BCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 2 Control Register
   type CPT2BCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT2BCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT2BCR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT2BCR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT2BCR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT2BCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2BCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TF1SET    at 0 range 16 .. 16;
      TF1RST    at 0 range 17 .. 17;
      TFCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   subtype OUTBR_FAULT1_Field is HAL.UInt2;
   subtype OUTBR_DLYPRT_Field is HAL.UInt3;
   subtype OUTBR_FAULT2_Field is HAL.UInt2;

   --  Timerx Output Register
   type OUTBR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Output 1 polarity
      POL1           : Boolean := False;
      --  Output 1 Idle mode
      IDLEM1         : Boolean := False;
      --  Output 1 Idle State
      IDLES1         : Boolean := False;
      --  Output 1 Fault state
      FAULT1         : OUTBR_FAULT1_Field := 16#0#;
      --  Output 1 Chopper enable
      CHP1           : Boolean := False;
      --  Output 1 Deadtime upon burst mode Idle entry
      DIDL1          : Boolean := False;
      --  Deadtime enable
      DTEN           : Boolean := False;
      --  Delayed Protection Enable
      DLYPRTEN       : Boolean := False;
      --  Delayed Protection
      DLYPRT         : OUTBR_DLYPRT_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Balanced Idle Automatic Resume
      BIAR           : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Output 2 polarity
      POL2           : Boolean := False;
      --  Output 2 Idle mode
      IDLEM2         : Boolean := False;
      --  Output 2 Idle State
      IDLES2         : Boolean := False;
      --  Output 2 Fault state
      FAULT2         : OUTBR_FAULT2_Field := 16#0#;
      --  Output 2 Chopper enable
      CHP2           : Boolean := False;
      --  Output 2 Deadtime upon burst mode Idle entry
      DIDL2          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTBR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      POL1           at 0 range 1 .. 1;
      IDLEM1         at 0 range 2 .. 2;
      IDLES1         at 0 range 3 .. 3;
      FAULT1         at 0 range 4 .. 5;
      CHP1           at 0 range 6 .. 6;
      DIDL1          at 0 range 7 .. 7;
      DTEN           at 0 range 8 .. 8;
      DLYPRTEN       at 0 range 9 .. 9;
      DLYPRT         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      BIAR           at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      POL2           at 0 range 17 .. 17;
      IDLEM2         at 0 range 18 .. 18;
      IDLES2         at 0 range 19 .. 19;
      FAULT2         at 0 range 20 .. 21;
      CHP2           at 0 range 22 .. 22;
      DIDL2          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Timerx Fault Register
   type FLTBR_Register is record
      --  Fault 1 enable
      FLT1EN        : Boolean := False;
      --  Fault 2 enable
      FLT2EN        : Boolean := False;
      --  Fault 3 enable
      FLT3EN        : Boolean := False;
      --  Fault 4 enable
      FLT4EN        : Boolean := False;
      --  Fault 5 enable
      FLT5EN        : Boolean := False;
      --  Fault 6 enable
      FLT6EN        : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  Fault sources Lock
      FLTLCK        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTBR_Register use record
      FLT1EN        at 0 range 0 .. 0;
      FLT2EN        at 0 range 1 .. 1;
      FLT3EN        at 0 range 2 .. 2;
      FLT4EN        at 0 range 3 .. 3;
      FLT5EN        at 0 range 4 .. 4;
      FLT6EN        at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      FLTLCK        at 0 range 31 .. 31;
   end record;

   subtype TIMBCR2_ROM_Field is HAL.UInt2;
   subtype TIMBCR2_OUTROM_Field is HAL.UInt2;
   subtype TIMBCR2_ADROM_Field is HAL.UInt2;
   subtype TIMBCR2_BMROM_Field is HAL.UInt2;
   subtype TIMBCR2_FEROM_Field is HAL.UInt2;

   --  TIMBCR2_GTCMP array
   type TIMBCR2_GTCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMBCR2_GTCMP
   type TIMBCR2_GTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GTCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  GTCMP as an array
            Arr : TIMBCR2_GTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMBCR2_GTCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  HRTIM Timerx Control Register 2
   type TIMBCR2_Register is record
      --  Dual Channel DAC trigger enable
      DCDE           : Boolean := False;
      --  Dual Channel DAC Step trigger
      DCDS           : Boolean := False;
      --  Dual Channel DAC Reset trigger
      DCDR           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Up-Down Mode
      UDM            : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Roll-Over Mode
      ROM            : TIMBCR2_ROM_Field := 16#0#;
      --  Output Roll-Over Mode
      OUTROM         : TIMBCR2_OUTROM_Field := 16#0#;
      --  ADC Roll-Over Mode
      ADROM          : TIMBCR2_ADROM_Field := 16#0#;
      --  Burst Mode Roll-Over Mode
      BMROM          : TIMBCR2_BMROM_Field := 16#0#;
      --  Fault and Event Roll-Over Mode
      FEROM          : TIMBCR2_FEROM_Field := 16#0#;
      --  Greater than Compare 1 PWM mode
      GTCMP          : TIMBCR2_GTCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Triggered-half mode
      TRGHLF         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMBCR2_Register use record
      DCDE           at 0 range 0 .. 0;
      DCDS           at 0 range 1 .. 1;
      DCDR           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      UDM            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ROM            at 0 range 6 .. 7;
      OUTROM         at 0 range 8 .. 9;
      ADROM          at 0 range 10 .. 11;
      BMROM          at 0 range 12 .. 13;
      FEROM          at 0 range 14 .. 15;
      GTCMP          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TRGHLF         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype EEFBR3_EEVASEL_Field is HAL.UInt4;
   subtype EEFBR3_EEVACNT_Field is HAL.UInt6;

   --  HRTIM Timerx External Event Filtering Register 3
   type EEFBR3_Register is record
      --  External Event A Counter Enable
      EEVACE         : Boolean := False;
      --  External Event A Counter Reset
      EEVACRES       : Boolean := False;
      --  External Event A Reset Mode
      EEVARSTM       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  External Event A Selection
      EEVASEL        : EEFBR3_EEVASEL_Field := 16#0#;
      --  External Event A counter
      EEVACNT        : EEFBR3_EEVACNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFBR3_Register use record
      EEVACE         at 0 range 0 .. 0;
      EEVACRES       at 0 range 1 .. 1;
      EEVARSTM       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EEVASEL        at 0 range 4 .. 7;
      EEVACNT        at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TIMCCR_CKPSCx_Field is HAL.UInt3;
   subtype TIMCCR_INTLVD_Field is HAL.UInt2;
   --  TIMCCR_DELCMP array element
   subtype TIMCCR_DELCMP_Element is HAL.UInt2;

   --  TIMCCR_DELCMP array
   type TIMCCR_DELCMP_Field_Array is array (2 .. 3) of TIMCCR_DELCMP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for TIMCCR_DELCMP
   type TIMCCR_DELCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DELCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  DELCMP as an array
            Arr : TIMCCR_DELCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMCCR_DELCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype TIMCCR_DACSYNC_Field is HAL.UInt2;
   subtype TIMCCR_UPDGAT_Field is HAL.UInt4;

   --  Timerx Control Register
   type TIMCCR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMCCR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMCCR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMCCR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  TFU
      TFU            : Boolean := False;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  TAU
      TAU            : Boolean := False;
      --  TBU
      TBU            : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  TDU
      TDU            : Boolean := False;
      --  TEU
      TEU            : Boolean := False;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMCCR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMCCR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMCCR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      TFU            at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      TAU            at 0 range 19 .. 19;
      TBU            at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      TDU            at 0 range 22 .. 22;
      TEU            at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  TIMCISR_CMP array
   type TIMCISR_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TIMCISR_CMP
   type TIMCISR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : TIMCISR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMCISR_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TIMCISR_CPT array
   type TIMCISR_CPT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMCISR_CPT
   type TIMCISR_CPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CPT as a value
            Val : HAL.UInt2;
         when True =>
            --  CPT as an array
            Arr : TIMCISR_CPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMCISR_CPT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Interrupt Status Register
   type TIMCISR_Register is record
      --  Read-only. Compare 1 Interrupt Flag
      CMP            : TIMCISR_CMP_Field;
      --  Read-only. Repetition Interrupt Flag
      REP            : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Update Interrupt Flag
      UPD            : Boolean;
      --  Read-only. Capture1 Interrupt Flag
      CPT            : TIMCISR_CPT_Field;
      --  Read-only. Output 1 Set Interrupt Flag
      SETx1          : Boolean;
      --  Read-only. Output 1 Reset Interrupt Flag
      RSTx1          : Boolean;
      --  Read-only. Output 2 Set Interrupt Flag
      SETx2          : Boolean;
      --  Read-only. Output 2 Reset Interrupt Flag
      RSTx2          : Boolean;
      --  Read-only. Reset Interrupt Flag
      RST            : Boolean;
      --  Read-only. Delayed Protection Flag
      DLYPRT         : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Current Push Pull Status
      CPPSTAT        : Boolean;
      --  Read-only. Idle Push Pull Status
      IPPSTAT        : Boolean;
      --  Read-only. Output 1 State
      O1STAT         : Boolean;
      --  Read-only. Output 2 State
      O2STAT         : Boolean;
      --  Read-only. Output 1 Copy
      O1CPY          : Boolean;
      --  Read-only. Output 2 Copy
      O2CPY          : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMCISR_Register use record
      CMP            at 0 range 0 .. 3;
      REP            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPD            at 0 range 6 .. 6;
      CPT            at 0 range 7 .. 8;
      SETx1          at 0 range 9 .. 9;
      RSTx1          at 0 range 10 .. 10;
      SETx2          at 0 range 11 .. 11;
      RSTx2          at 0 range 12 .. 12;
      RST            at 0 range 13 .. 13;
      DLYPRT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CPPSTAT        at 0 range 16 .. 16;
      IPPSTAT        at 0 range 17 .. 17;
      O1STAT         at 0 range 18 .. 18;
      O2STAT         at 0 range 19 .. 19;
      O1CPY          at 0 range 20 .. 20;
      O2CPY          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Timerx Interrupt Clear Register
   type TIMCICR_Register is record
      --  Write-only. Compare 1 Interrupt flag Clear
      CMP1C          : Boolean := False;
      --  Write-only. Compare 2 Interrupt flag Clear
      CMP2C          : Boolean := False;
      --  Write-only. Compare 3 Interrupt flag Clear
      CMP3C          : Boolean := False;
      --  Write-only. Compare 4 Interrupt flag Clear
      CMP4C          : Boolean := False;
      --  Write-only. Repetition Interrupt flag Clear
      REPC           : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Update Interrupt flag Clear
      UPDC           : Boolean := False;
      --  Write-only. Capture1 Interrupt flag Clear
      CPT1C          : Boolean := False;
      --  Write-only. Capture2 Interrupt flag Clear
      CPT2C          : Boolean := False;
      --  Write-only. Output 1 Set flag Clear
      SET1xC         : Boolean := False;
      --  Write-only. Output 1 Reset flag Clear
      RSTx1C         : Boolean := False;
      --  Write-only. Output 2 Set flag Clear
      SET2xC         : Boolean := False;
      --  Write-only. Output 2 Reset flag Clear
      RSTx2C         : Boolean := False;
      --  Write-only. Reset Interrupt flag Clear
      RSTC           : Boolean := False;
      --  Write-only. Delayed Protection Flag Clear
      DLYPRTC        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMCICR_Register use record
      CMP1C          at 0 range 0 .. 0;
      CMP2C          at 0 range 1 .. 1;
      CMP3C          at 0 range 2 .. 2;
      CMP4C          at 0 range 3 .. 3;
      REPC           at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDC           at 0 range 6 .. 6;
      CPT1C          at 0 range 7 .. 7;
      CPT2C          at 0 range 8 .. 8;
      SET1xC         at 0 range 9 .. 9;
      RSTx1C         at 0 range 10 .. 10;
      SET2xC         at 0 range 11 .. 11;
      RSTx2C         at 0 range 12 .. 12;
      RSTC           at 0 range 13 .. 13;
      DLYPRTC        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  TIMxDIER
   type TIMCDIER_Register is record
      --  CMP1IE
      CMP1IE         : Boolean := False;
      --  CMP2IE
      CMP2IE         : Boolean := False;
      --  CMP3IE
      CMP3IE         : Boolean := False;
      --  CMP4IE
      CMP4IE         : Boolean := False;
      --  REPIE
      REPIE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  UPDIE
      UPDIE          : Boolean := False;
      --  CPT1IE
      CPT1IE         : Boolean := False;
      --  CPT2IE
      CPT2IE         : Boolean := False;
      --  SET1xIE
      SET1xIE        : Boolean := False;
      --  RSTx1IE
      RSTx1IE        : Boolean := False;
      --  SETx2IE
      SETx2IE        : Boolean := False;
      --  RSTx2IE
      RSTx2IE        : Boolean := False;
      --  RSTIE
      RSTIE          : Boolean := False;
      --  DLYPRTIE
      DLYPRTIE       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  CMP1DE
      CMP1DE         : Boolean := False;
      --  CMP2DE
      CMP2DE         : Boolean := False;
      --  CMP3DE
      CMP3DE         : Boolean := False;
      --  CMP4DE
      CMP4DE         : Boolean := False;
      --  REPDE
      REPDE          : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  UPDDE
      UPDDE          : Boolean := False;
      --  CPT1DE
      CPT1DE         : Boolean := False;
      --  CPT2DE
      CPT2DE         : Boolean := False;
      --  SET1xDE
      SET1xDE        : Boolean := False;
      --  RSTx1DE
      RSTx1DE        : Boolean := False;
      --  SETx2DE
      SETx2DE        : Boolean := False;
      --  RSTx2DE
      RSTx2DE        : Boolean := False;
      --  RSTDE
      RSTDE          : Boolean := False;
      --  DLYPRTDE
      DLYPRTDE       : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMCDIER_Register use record
      CMP1IE         at 0 range 0 .. 0;
      CMP2IE         at 0 range 1 .. 1;
      CMP3IE         at 0 range 2 .. 2;
      CMP4IE         at 0 range 3 .. 3;
      REPIE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDIE          at 0 range 6 .. 6;
      CPT1IE         at 0 range 7 .. 7;
      CPT2IE         at 0 range 8 .. 8;
      SET1xIE        at 0 range 9 .. 9;
      RSTx1IE        at 0 range 10 .. 10;
      SETx2IE        at 0 range 11 .. 11;
      RSTx2IE        at 0 range 12 .. 12;
      RSTIE          at 0 range 13 .. 13;
      DLYPRTIE       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CMP1DE         at 0 range 16 .. 16;
      CMP2DE         at 0 range 17 .. 17;
      CMP3DE         at 0 range 18 .. 18;
      CMP4DE         at 0 range 19 .. 19;
      REPDE          at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      UPDDE          at 0 range 22 .. 22;
      CPT1DE         at 0 range 23 .. 23;
      CPT2DE         at 0 range 24 .. 24;
      SET1xDE        at 0 range 25 .. 25;
      RSTx1DE        at 0 range 26 .. 26;
      SETx2DE        at 0 range 27 .. 27;
      RSTx2DE        at 0 range 28 .. 28;
      RSTDE          at 0 range 29 .. 29;
      DLYPRTDE       at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CNTCR_CNTx_Field is HAL.UInt16;

   --  Timerx Counter Register
   type CNTCR_Register is record
      --  Timerx Counter value
      CNTx           : CNTCR_CNTx_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTCR_Register use record
      CNTx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PERCR_PERx_Field is HAL.UInt16;

   --  Timerx Period Register
   type PERCR_Register is record
      --  Timerx Period value
      PERx           : PERCR_PERx_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERCR_Register use record
      PERx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype REPCR_REPx_Field is HAL.UInt8;

   --  Timerx Repetition Register
   type REPCR_Register is record
      --  Timerx Repetition counter value
      REPx          : REPCR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPCR_Register use record
      REPx          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CMP1CR_CMP1x_Field is HAL.UInt16;

   --  Timerx Compare 1 Register
   type CMP1CR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CR_CMP1x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CR_Register use record
      CMP1x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP1CCR_CMP1x_Field is HAL.UInt16;
   subtype CMP1CCR_REPx_Field is HAL.UInt8;

   --  Timerx Compare 1 Compound Register
   type CMP1CCR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CCR_CMP1x_Field := 16#0#;
      --  Timerx Repetition value (aliased from HRTIM_REPx register)
      REPx           : CMP1CCR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CCR_Register use record
      CMP1x          at 0 range 0 .. 15;
      REPx           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CMP2CR_CMP2x_Field is HAL.UInt16;

   --  Timerx Compare 2 Register
   type CMP2CR_Register is record
      --  Timerx Compare 2 value
      CMP2x          : CMP2CR_CMP2x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP2CR_Register use record
      CMP2x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP3CR_CMP3x_Field is HAL.UInt16;

   --  Timerx Compare 3 Register
   type CMP3CR_Register is record
      --  Timerx Compare 3 value
      CMP3x          : CMP3CR_CMP3x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP3CR_Register use record
      CMP3x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP4CR_CMP4x_Field is HAL.UInt16;

   --  Timerx Compare 4 Register
   type CMP4CR_Register is record
      --  Timerx Compare 4 value
      CMP4x          : CMP4CR_CMP4x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP4CR_Register use record
      CMP4x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CPT1CR_CPT1x_Field is HAL.UInt16;

   --  Timerx Capture 1 Register
   type CPT1CR_Register is record
      --  Read-only. Timerx Capture 1 value
      CPT1x          : CPT1CR_CPT1x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1CR_Register use record
      CPT1x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CPT2CR_CPT2x_Field is HAL.UInt16;

   --  Timerx Capture 2 Register
   type CPT2CR_Register is record
      --  Read-only. Timerx Capture 2 value
      CPT2x          : CPT2CR_CPT2x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2CR_Register use record
      CPT2x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DTCR_DTRx_Field is HAL.UInt9;
   subtype DTCR_DTPRSC_Field is HAL.UInt3;
   subtype DTCR_DTFx_Field is HAL.UInt9;

   --  Timerx Deadtime Register
   type DTCR_Register is record
      --  Deadtime Rising value
      DTRx           : DTCR_DTRx_Field := 16#0#;
      --  Sign Deadtime Rising value
      SDTRx          : Boolean := False;
      --  Deadtime Prescaler
      DTPRSC         : DTCR_DTPRSC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Deadtime Rising Sign Lock
      DTRSLKx        : Boolean := False;
      --  Deadtime Rising Lock
      DTRLKx         : Boolean := False;
      --  Deadtime Falling value
      DTFx           : DTCR_DTFx_Field := 16#0#;
      --  Sign Deadtime Falling value
      SDTFx          : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Deadtime Falling Sign Lock
      DTFSLKx        : Boolean := False;
      --  Deadtime Falling Lock
      DTFLKx         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTCR_Register use record
      DTRx           at 0 range 0 .. 8;
      SDTRx          at 0 range 9 .. 9;
      DTPRSC         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      DTRSLKx        at 0 range 14 .. 14;
      DTRLKx         at 0 range 15 .. 15;
      DTFx           at 0 range 16 .. 24;
      SDTFx          at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DTFSLKx        at 0 range 30 .. 30;
      DTFLKx         at 0 range 31 .. 31;
   end record;

   --  SETC1R_CMP array
   type SETC1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETC1R_CMP
   type SETC1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETC1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETC1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETC1R_MSTCMP array
   type SETC1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETC1R_MSTCMP
   type SETC1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETC1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETC1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETC1R_TIMEVNT array
   type SETC1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETC1R_TIMEVNT
   type SETC1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETC1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETC1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETC1R_EXTEVNT array
   type SETC1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETC1R_EXTEVNT
   type SETC1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETC1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETC1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Set Register
   type SETC1R_Register is record
      --  Software Set trigger
      SST     : Boolean := False;
      --  Timer A resynchronizaton
      RESYNC  : Boolean := False;
      --  Timer A Period
      PER     : Boolean := False;
      --  Timer A compare 1
      CMP     : SETC1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master Period
      MSTPER  : Boolean := False;
      --  Master Compare 1
      MSTCMP  : SETC1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer Event 1
      TIMEVNT : SETC1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT : SETC1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Registers update (transfer preload to active)
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETC1R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTC1R_CMP array
   type RSTC1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTC1R_CMP
   type RSTC1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTC1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTC1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTC1R_MSTCMP array
   type RSTC1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTC1R_MSTCMP
   type RSTC1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTC1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTC1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTC1R_TIMEVNT array
   type RSTC1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTC1R_TIMEVNT
   type RSTC1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTC1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTC1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTC1R_EXTEVNT array
   type RSTC1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTC1R_EXTEVNT
   type RSTC1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTC1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTC1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Reset Register
   type RSTC1R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTC1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTC1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTC1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTC1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC1R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  SETC2R_CMP array
   type SETC2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETC2R_CMP
   type SETC2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETC2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETC2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETC2R_MSTCMP array
   type SETC2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETC2R_MSTCMP
   type SETC2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETC2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETC2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETC2R_TIMEVNT array
   type SETC2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETC2R_TIMEVNT
   type SETC2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETC2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETC2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETC2R_EXTEVNT array
   type SETC2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETC2R_EXTEVNT
   type SETC2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETC2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETC2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Set Register
   type SETC2R_Register is record
      --  SST
      SST     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : SETC2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : SETC2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : SETC2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : SETC2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETC2R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTC2R_CMP array
   type RSTC2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTC2R_CMP
   type RSTC2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTC2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTC2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTC2R_MSTCMP array
   type RSTC2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTC2R_MSTCMP
   type RSTC2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTC2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTC2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTC2R_TIMEVNT array
   type RSTC2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTC2R_TIMEVNT
   type RSTC2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTC2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTC2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTC2R_EXTEVNT array
   type RSTC2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTC2R_EXTEVNT
   type RSTC2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTC2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTC2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Reset Register
   type RSTC2R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTC2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTC2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTC2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTC2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC2R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   subtype EEFCR1_EE1FLTR_Field is HAL.UInt4;
   subtype EEFCR1_EE2FLTR_Field is HAL.UInt4;
   subtype EEFCR1_EE3FLTR_Field is HAL.UInt4;
   subtype EEFCR1_EE4FLTR_Field is HAL.UInt4;
   subtype EEFCR1_EE5FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 1
   type EEFCR1_Register is record
      --  External Event 1 latch
      EE1LTCH        : Boolean := False;
      --  External Event 1 filter
      EE1FLTR        : EEFCR1_EE1FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 2 latch
      EE2LTCH        : Boolean := False;
      --  External Event 2 filter
      EE2FLTR        : EEFCR1_EE2FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 3 latch
      EE3LTCH        : Boolean := False;
      --  External Event 3 filter
      EE3FLTR        : EEFCR1_EE3FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 4 latch
      EE4LTCH        : Boolean := False;
      --  External Event 4 filter
      EE4FLTR        : EEFCR1_EE4FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 5 latch
      EE5LTCH        : Boolean := False;
      --  External Event 5 filter
      EE5FLTR        : EEFCR1_EE5FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFCR1_Register use record
      EE1LTCH        at 0 range 0 .. 0;
      EE1FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE2LTCH        at 0 range 6 .. 6;
      EE2FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE3LTCH        at 0 range 12 .. 12;
      EE3FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE4LTCH        at 0 range 18 .. 18;
      EE4FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE5LTCH        at 0 range 24 .. 24;
      EE5FLTR        at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EEFCR2_EE6FLTR_Field is HAL.UInt4;
   subtype EEFCR2_EE7FLTR_Field is HAL.UInt4;
   subtype EEFCR2_EE8FLTR_Field is HAL.UInt4;
   subtype EEFCR2_EE9FLTR_Field is HAL.UInt4;
   subtype EEFCR2_EE10FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 2
   type EEFCR2_Register is record
      --  External Event 6 latch
      EE6LTCH        : Boolean := False;
      --  External Event 6 filter
      EE6FLTR        : EEFCR2_EE6FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 7 latch
      EE7LTCH        : Boolean := False;
      --  External Event 7 filter
      EE7FLTR        : EEFCR2_EE7FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 8 latch
      EE8LTCH        : Boolean := False;
      --  External Event 8 filter
      EE8FLTR        : EEFCR2_EE8FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 9 latch
      EE9LTCH        : Boolean := False;
      --  External Event 9 filter
      EE9FLTR        : EEFCR2_EE9FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 10 latch
      EE10LTCH       : Boolean := False;
      --  External Event 10 filter
      EE10FLTR       : EEFCR2_EE10FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFCR2_Register use record
      EE6LTCH        at 0 range 0 .. 0;
      EE6FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7LTCH        at 0 range 6 .. 6;
      EE7FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8LTCH        at 0 range 12 .. 12;
      EE8FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9LTCH        at 0 range 18 .. 18;
      EE9FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10LTCH       at 0 range 24 .. 24;
      EE10FLTR       at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  RSTCR_CMP array
   type RSTCR_CMP_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RSTCR_CMP
   type RSTCR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  CMP as an array
            Arr : RSTCR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RSTCR_CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RSTCR_MSTCMP array
   type RSTCR_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTCR_MSTCMP
   type RSTCR_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTCR_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTCR_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTCR_EXTEVNT array
   type RSTCR_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTCR_EXTEVNT
   type RSTCR_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTCR_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTCR_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  RSTCR_TIMACMP array
   type RSTCR_TIMACMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTCR_TIMACMP
   type RSTCR_TIMACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMACMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMACMP as an array
            Arr : RSTCR_TIMACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTCR_TIMACMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTCR_TIMBCMP array
   type RSTCR_TIMBCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTCR_TIMBCMP
   type RSTCR_TIMBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMBCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMBCMP as an array
            Arr : RSTCR_TIMBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTCR_TIMBCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTCR_TIMDCMP array
   type RSTCR_TIMDCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTCR_TIMDCMP
   type RSTCR_TIMDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMDCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMDCMP as an array
            Arr : RSTCR_TIMDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTCR_TIMDCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTCR_TIMECMP array
   type RSTCR_TIMECMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTCR_TIMECMP
   type RSTCR_TIMECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMECMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMECMP as an array
            Arr : RSTCR_TIMECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTCR_TIMECMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TimerA Reset Register
   type RSTCR_Register is record
      --  Timer A Update reset
      TIMFCMP1 : Boolean := False;
      --  Timer A Update reset
      UPDT     : Boolean := False;
      --  Timer A compare 2 reset
      CMP      : RSTCR_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master timer Period
      MSTPER   : Boolean := False;
      --  Master compare 1
      MSTCMP   : RSTCR_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT  : RSTCR_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Timer A Compare 1
      TIMACMP  : RSTCR_TIMACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B Compare 1
      TIMBCMP  : RSTCR_TIMBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D Compare 1
      TIMDCMP  : RSTCR_TIMDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E Compare 1
      TIMECMP  : RSTCR_TIMECMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer F Compare 2
      TIMFCPM2 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTCR_Register use record
      TIMFCMP1 at 0 range 0 .. 0;
      UPDT     at 0 range 1 .. 1;
      CMP      at 0 range 2 .. 3;
      MSTPER   at 0 range 4 .. 4;
      MSTCMP   at 0 range 5 .. 8;
      EXTEVNT  at 0 range 9 .. 18;
      TIMACMP  at 0 range 19 .. 21;
      TIMBCMP  at 0 range 22 .. 24;
      TIMDCMP  at 0 range 25 .. 27;
      TIMECMP  at 0 range 28 .. 30;
      TIMFCPM2 at 0 range 31 .. 31;
   end record;

   subtype CHPCR_CHPFRQ_Field is HAL.UInt4;
   subtype CHPCR_CHPDTY_Field is HAL.UInt3;
   subtype CHPCR_STRTPW_Field is HAL.UInt4;

   --  Timerx Chopper Register
   type CHPCR_Register is record
      --  Timerx carrier frequency value
      CHPFRQ         : CHPCR_CHPFRQ_Field := 16#0#;
      --  Timerx chopper duty cycle value
      CHPDTY         : CHPCR_CHPDTY_Field := 16#0#;
      --  STRTPW
      STRTPW         : CHPCR_STRTPW_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHPCR_Register use record
      CHPFRQ         at 0 range 0 .. 3;
      CHPDTY         at 0 range 4 .. 6;
      STRTPW         at 0 range 7 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CPT1CCR_TACMP array
   type CPT1CCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1CCR_TACMP
   type CPT1CCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT1CCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1CCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1CCR_TBCMP array
   type CPT1CCR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1CCR_TBCMP
   type CPT1CCR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT1CCR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1CCR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1CCR_TFCMP array
   type CPT1CCR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1CCR_TFCMP
   type CPT1CCR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT1CCR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1CCR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1CCR_TDCMP array
   type CPT1CCR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1CCR_TDCMP
   type CPT1CCR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT1CCR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1CCR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1CCR_TECMP array
   type CPT1CCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1CCR_TECMP
   type CPT1CCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT1CCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1CCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 1 Control Register
   type CPT1CCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT1CCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT1CCR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT1CCR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT1CCR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT1CCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1CCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TF1SET    at 0 range 20 .. 20;
      TF1RST    at 0 range 21 .. 21;
      TFCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   --  CPT2CCR_TACMP array
   type CPT2CCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2CCR_TACMP
   type CPT2CCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT2CCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2CCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2CCR_TBCMP array
   type CPT2CCR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2CCR_TBCMP
   type CPT2CCR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT2CCR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2CCR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2CCR_TF1CMP array
   type CPT2CCR_TF1CMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2CCR_TF1CMP
   type CPT2CCR_TF1CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TF1CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TF1CMP as an array
            Arr : CPT2CCR_TF1CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2CCR_TF1CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2CCR_TDCMP array
   type CPT2CCR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2CCR_TDCMP
   type CPT2CCR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT2CCR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2CCR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2CCR_TECMP array
   type CPT2CCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2CCR_TECMP
   type CPT2CCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT2CCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2CCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 2 Control Register
   type CPT2CCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT2CCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT2CCR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TF1CMP1
      TF1CMP    : CPT2CCR_TF1CMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT2CCR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT2CCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2CCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TF1SET    at 0 range 20 .. 20;
      TF1RST    at 0 range 21 .. 21;
      TF1CMP    at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   subtype OUTCR_FAULT1_Field is HAL.UInt2;
   subtype OUTCR_DLYPRT_Field is HAL.UInt3;
   subtype OUTCR_FAULT2_Field is HAL.UInt2;

   --  Timerx Output Register
   type OUTCR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Output 1 polarity
      POL1           : Boolean := False;
      --  Output 1 Idle mode
      IDLEM1         : Boolean := False;
      --  Output 1 Idle State
      IDLES1         : Boolean := False;
      --  Output 1 Fault state
      FAULT1         : OUTCR_FAULT1_Field := 16#0#;
      --  Output 1 Chopper enable
      CHP1           : Boolean := False;
      --  Output 1 Deadtime upon burst mode Idle entry
      DIDL1          : Boolean := False;
      --  Deadtime enable
      DTEN           : Boolean := False;
      --  Delayed Protection Enable
      DLYPRTEN       : Boolean := False;
      --  Delayed Protection
      DLYPRT         : OUTCR_DLYPRT_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Balanced Idle Automatic Resume
      BIAR           : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Output 2 polarity
      POL2           : Boolean := False;
      --  Output 2 Idle mode
      IDLEM2         : Boolean := False;
      --  Output 2 Idle State
      IDLES2         : Boolean := False;
      --  Output 2 Fault state
      FAULT2         : OUTCR_FAULT2_Field := 16#0#;
      --  Output 2 Chopper enable
      CHP2           : Boolean := False;
      --  Output 2 Deadtime upon burst mode Idle entry
      DIDL2          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTCR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      POL1           at 0 range 1 .. 1;
      IDLEM1         at 0 range 2 .. 2;
      IDLES1         at 0 range 3 .. 3;
      FAULT1         at 0 range 4 .. 5;
      CHP1           at 0 range 6 .. 6;
      DIDL1          at 0 range 7 .. 7;
      DTEN           at 0 range 8 .. 8;
      DLYPRTEN       at 0 range 9 .. 9;
      DLYPRT         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      BIAR           at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      POL2           at 0 range 17 .. 17;
      IDLEM2         at 0 range 18 .. 18;
      IDLES2         at 0 range 19 .. 19;
      FAULT2         at 0 range 20 .. 21;
      CHP2           at 0 range 22 .. 22;
      DIDL2          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Timerx Fault Register
   type FLTCR_Register is record
      --  Fault 1 enable
      FLT1EN        : Boolean := False;
      --  Fault 2 enable
      FLT2EN        : Boolean := False;
      --  Fault 3 enable
      FLT3EN        : Boolean := False;
      --  Fault 4 enable
      FLT4EN        : Boolean := False;
      --  Fault 5 enable
      FLT5EN        : Boolean := False;
      --  Fault 6 enable
      FLT6EN        : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  Fault sources Lock
      FLTLCK        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTCR_Register use record
      FLT1EN        at 0 range 0 .. 0;
      FLT2EN        at 0 range 1 .. 1;
      FLT3EN        at 0 range 2 .. 2;
      FLT4EN        at 0 range 3 .. 3;
      FLT5EN        at 0 range 4 .. 4;
      FLT6EN        at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      FLTLCK        at 0 range 31 .. 31;
   end record;

   subtype TIMCCR2_ROM_Field is HAL.UInt2;
   subtype TIMCCR2_OUTROM_Field is HAL.UInt2;
   subtype TIMCCR2_ADROM_Field is HAL.UInt2;
   subtype TIMCCR2_BMROM_Field is HAL.UInt2;
   subtype TIMCCR2_FEROM_Field is HAL.UInt2;

   --  TIMCCR2_GTCMP array
   type TIMCCR2_GTCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMCCR2_GTCMP
   type TIMCCR2_GTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GTCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  GTCMP as an array
            Arr : TIMCCR2_GTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMCCR2_GTCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  HRTIM Timerx Control Register 2
   type TIMCCR2_Register is record
      --  Dual Channel DAC trigger enable
      DCDE           : Boolean := False;
      --  Dual Channel DAC Step trigger
      DCDS           : Boolean := False;
      --  Dual Channel DAC Reset trigger
      DCDR           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Up-Down Mode
      UDM            : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Roll-Over Mode
      ROM            : TIMCCR2_ROM_Field := 16#0#;
      --  Output Roll-Over Mode
      OUTROM         : TIMCCR2_OUTROM_Field := 16#0#;
      --  ADC Roll-Over Mode
      ADROM          : TIMCCR2_ADROM_Field := 16#0#;
      --  Burst Mode Roll-Over Mode
      BMROM          : TIMCCR2_BMROM_Field := 16#0#;
      --  Fault and Event Roll-Over Mode
      FEROM          : TIMCCR2_FEROM_Field := 16#0#;
      --  Greater than Compare 1 PWM mode
      GTCMP          : TIMCCR2_GTCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Triggered-half mode
      TRGHLF         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMCCR2_Register use record
      DCDE           at 0 range 0 .. 0;
      DCDS           at 0 range 1 .. 1;
      DCDR           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      UDM            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ROM            at 0 range 6 .. 7;
      OUTROM         at 0 range 8 .. 9;
      ADROM          at 0 range 10 .. 11;
      BMROM          at 0 range 12 .. 13;
      FEROM          at 0 range 14 .. 15;
      GTCMP          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TRGHLF         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype EEFCR3_EEVASEL_Field is HAL.UInt4;
   subtype EEFCR3_EEVACNT_Field is HAL.UInt6;

   --  HRTIM Timerx External Event Filtering Register 3
   type EEFCR3_Register is record
      --  External Event A Counter Enable
      EEVACE         : Boolean := False;
      --  External Event A Counter Reset
      EEVACRES       : Boolean := False;
      --  External Event A Reset Mode
      EEVARSTM       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  External Event A Selection
      EEVASEL        : EEFCR3_EEVASEL_Field := 16#0#;
      --  External Event A counter
      EEVACNT        : EEFCR3_EEVACNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFCR3_Register use record
      EEVACE         at 0 range 0 .. 0;
      EEVACRES       at 0 range 1 .. 1;
      EEVARSTM       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EEVASEL        at 0 range 4 .. 7;
      EEVACNT        at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TIMDCR_CKPSCx_Field is HAL.UInt3;
   subtype TIMDCR_INTLVD_Field is HAL.UInt2;
   --  TIMDCR_DELCMP array element
   subtype TIMDCR_DELCMP_Element is HAL.UInt2;

   --  TIMDCR_DELCMP array
   type TIMDCR_DELCMP_Field_Array is array (2 .. 3) of TIMDCR_DELCMP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for TIMDCR_DELCMP
   type TIMDCR_DELCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DELCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  DELCMP as an array
            Arr : TIMDCR_DELCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMDCR_DELCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype TIMDCR_DACSYNC_Field is HAL.UInt2;
   subtype TIMDCR_UPDGAT_Field is HAL.UInt4;

   --  Timerx Control Register
   type TIMDCR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMDCR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMDCR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMDCR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  TFU
      TFU            : Boolean := False;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  TAU
      TAU            : Boolean := False;
      --  TBU
      TBU            : Boolean := False;
      --  TCU
      TCU            : Boolean := False;
      --  unspecified
      Reserved_22_22 : HAL.Bit := 16#0#;
      --  TEU
      TEU            : Boolean := False;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMDCR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMDCR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMDCR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      TFU            at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      TAU            at 0 range 19 .. 19;
      TBU            at 0 range 20 .. 20;
      TCU            at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      TEU            at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  TIMDISR_CMP array
   type TIMDISR_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TIMDISR_CMP
   type TIMDISR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : TIMDISR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMDISR_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TIMDISR_CPT array
   type TIMDISR_CPT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMDISR_CPT
   type TIMDISR_CPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CPT as a value
            Val : HAL.UInt2;
         when True =>
            --  CPT as an array
            Arr : TIMDISR_CPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMDISR_CPT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Interrupt Status Register
   type TIMDISR_Register is record
      --  Read-only. Compare 1 Interrupt Flag
      CMP            : TIMDISR_CMP_Field;
      --  Read-only. Repetition Interrupt Flag
      REP            : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Update Interrupt Flag
      UPD            : Boolean;
      --  Read-only. Capture1 Interrupt Flag
      CPT            : TIMDISR_CPT_Field;
      --  Read-only. Output 1 Set Interrupt Flag
      SETx1          : Boolean;
      --  Read-only. Output 1 Reset Interrupt Flag
      RSTx1          : Boolean;
      --  Read-only. Output 2 Set Interrupt Flag
      SETx2          : Boolean;
      --  Read-only. Output 2 Reset Interrupt Flag
      RSTx2          : Boolean;
      --  Read-only. Reset Interrupt Flag
      RST            : Boolean;
      --  Read-only. Delayed Protection Flag
      DLYPRT         : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Current Push Pull Status
      CPPSTAT        : Boolean;
      --  Read-only. Idle Push Pull Status
      IPPSTAT        : Boolean;
      --  Read-only. Output 1 State
      O1STAT         : Boolean;
      --  Read-only. Output 2 State
      O2STAT         : Boolean;
      --  Read-only. Output 1 Copy
      O1CPY          : Boolean;
      --  Read-only. Output 2 Copy
      O2CPY          : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMDISR_Register use record
      CMP            at 0 range 0 .. 3;
      REP            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPD            at 0 range 6 .. 6;
      CPT            at 0 range 7 .. 8;
      SETx1          at 0 range 9 .. 9;
      RSTx1          at 0 range 10 .. 10;
      SETx2          at 0 range 11 .. 11;
      RSTx2          at 0 range 12 .. 12;
      RST            at 0 range 13 .. 13;
      DLYPRT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CPPSTAT        at 0 range 16 .. 16;
      IPPSTAT        at 0 range 17 .. 17;
      O1STAT         at 0 range 18 .. 18;
      O2STAT         at 0 range 19 .. 19;
      O1CPY          at 0 range 20 .. 20;
      O2CPY          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Timerx Interrupt Clear Register
   type TIMDICR_Register is record
      --  Write-only. Compare 1 Interrupt flag Clear
      CMP1C          : Boolean := False;
      --  Write-only. Compare 2 Interrupt flag Clear
      CMP2C          : Boolean := False;
      --  Write-only. Compare 3 Interrupt flag Clear
      CMP3C          : Boolean := False;
      --  Write-only. Compare 4 Interrupt flag Clear
      CMP4C          : Boolean := False;
      --  Write-only. Repetition Interrupt flag Clear
      REPC           : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Update Interrupt flag Clear
      UPDC           : Boolean := False;
      --  Write-only. Capture1 Interrupt flag Clear
      CPT1C          : Boolean := False;
      --  Write-only. Capture2 Interrupt flag Clear
      CPT2C          : Boolean := False;
      --  Write-only. Output 1 Set flag Clear
      SET1xC         : Boolean := False;
      --  Write-only. Output 1 Reset flag Clear
      RSTx1C         : Boolean := False;
      --  Write-only. Output 2 Set flag Clear
      SET2xC         : Boolean := False;
      --  Write-only. Output 2 Reset flag Clear
      RSTx2C         : Boolean := False;
      --  Write-only. Reset Interrupt flag Clear
      RSTC           : Boolean := False;
      --  Write-only. Delayed Protection Flag Clear
      DLYPRTC        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMDICR_Register use record
      CMP1C          at 0 range 0 .. 0;
      CMP2C          at 0 range 1 .. 1;
      CMP3C          at 0 range 2 .. 2;
      CMP4C          at 0 range 3 .. 3;
      REPC           at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDC           at 0 range 6 .. 6;
      CPT1C          at 0 range 7 .. 7;
      CPT2C          at 0 range 8 .. 8;
      SET1xC         at 0 range 9 .. 9;
      RSTx1C         at 0 range 10 .. 10;
      SET2xC         at 0 range 11 .. 11;
      RSTx2C         at 0 range 12 .. 12;
      RSTC           at 0 range 13 .. 13;
      DLYPRTC        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  TIMxDIER
   type TIMDDIER_Register is record
      --  CMP1IE
      CMP1IE         : Boolean := False;
      --  CMP2IE
      CMP2IE         : Boolean := False;
      --  CMP3IE
      CMP3IE         : Boolean := False;
      --  CMP4IE
      CMP4IE         : Boolean := False;
      --  REPIE
      REPIE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  UPDIE
      UPDIE          : Boolean := False;
      --  CPT1IE
      CPT1IE         : Boolean := False;
      --  CPT2IE
      CPT2IE         : Boolean := False;
      --  SET1xIE
      SET1xIE        : Boolean := False;
      --  RSTx1IE
      RSTx1IE        : Boolean := False;
      --  SETx2IE
      SETx2IE        : Boolean := False;
      --  RSTx2IE
      RSTx2IE        : Boolean := False;
      --  RSTIE
      RSTIE          : Boolean := False;
      --  DLYPRTIE
      DLYPRTIE       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  CMP1DE
      CMP1DE         : Boolean := False;
      --  CMP2DE
      CMP2DE         : Boolean := False;
      --  CMP3DE
      CMP3DE         : Boolean := False;
      --  CMP4DE
      CMP4DE         : Boolean := False;
      --  REPDE
      REPDE          : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  UPDDE
      UPDDE          : Boolean := False;
      --  CPT1DE
      CPT1DE         : Boolean := False;
      --  CPT2DE
      CPT2DE         : Boolean := False;
      --  SET1xDE
      SET1xDE        : Boolean := False;
      --  RSTx1DE
      RSTx1DE        : Boolean := False;
      --  SETx2DE
      SETx2DE        : Boolean := False;
      --  RSTx2DE
      RSTx2DE        : Boolean := False;
      --  RSTDE
      RSTDE          : Boolean := False;
      --  DLYPRTDE
      DLYPRTDE       : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMDDIER_Register use record
      CMP1IE         at 0 range 0 .. 0;
      CMP2IE         at 0 range 1 .. 1;
      CMP3IE         at 0 range 2 .. 2;
      CMP4IE         at 0 range 3 .. 3;
      REPIE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDIE          at 0 range 6 .. 6;
      CPT1IE         at 0 range 7 .. 7;
      CPT2IE         at 0 range 8 .. 8;
      SET1xIE        at 0 range 9 .. 9;
      RSTx1IE        at 0 range 10 .. 10;
      SETx2IE        at 0 range 11 .. 11;
      RSTx2IE        at 0 range 12 .. 12;
      RSTIE          at 0 range 13 .. 13;
      DLYPRTIE       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CMP1DE         at 0 range 16 .. 16;
      CMP2DE         at 0 range 17 .. 17;
      CMP3DE         at 0 range 18 .. 18;
      CMP4DE         at 0 range 19 .. 19;
      REPDE          at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      UPDDE          at 0 range 22 .. 22;
      CPT1DE         at 0 range 23 .. 23;
      CPT2DE         at 0 range 24 .. 24;
      SET1xDE        at 0 range 25 .. 25;
      RSTx1DE        at 0 range 26 .. 26;
      SETx2DE        at 0 range 27 .. 27;
      RSTx2DE        at 0 range 28 .. 28;
      RSTDE          at 0 range 29 .. 29;
      DLYPRTDE       at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CNTDR_CNTx_Field is HAL.UInt16;

   --  Timerx Counter Register
   type CNTDR_Register is record
      --  Timerx Counter value
      CNTx           : CNTDR_CNTx_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTDR_Register use record
      CNTx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PERDR_PERx_Field is HAL.UInt16;

   --  Timerx Period Register
   type PERDR_Register is record
      --  Timerx Period value
      PERx           : PERDR_PERx_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERDR_Register use record
      PERx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype REPDR_REPx_Field is HAL.UInt8;

   --  Timerx Repetition Register
   type REPDR_Register is record
      --  Timerx Repetition counter value
      REPx          : REPDR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPDR_Register use record
      REPx          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CMP1DR_CMP1x_Field is HAL.UInt16;

   --  Timerx Compare 1 Register
   type CMP1DR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1DR_CMP1x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1DR_Register use record
      CMP1x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP1CDR_CMP1x_Field is HAL.UInt16;
   subtype CMP1CDR_REPx_Field is HAL.UInt8;

   --  Timerx Compare 1 Compound Register
   type CMP1CDR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CDR_CMP1x_Field := 16#0#;
      --  Timerx Repetition value (aliased from HRTIM_REPx register)
      REPx           : CMP1CDR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CDR_Register use record
      CMP1x          at 0 range 0 .. 15;
      REPx           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CMP2DR_CMP2x_Field is HAL.UInt16;

   --  Timerx Compare 2 Register
   type CMP2DR_Register is record
      --  Timerx Compare 2 value
      CMP2x          : CMP2DR_CMP2x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP2DR_Register use record
      CMP2x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP3DR_CMP3x_Field is HAL.UInt16;

   --  Timerx Compare 3 Register
   type CMP3DR_Register is record
      --  Timerx Compare 3 value
      CMP3x          : CMP3DR_CMP3x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP3DR_Register use record
      CMP3x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP4DR_CMP4x_Field is HAL.UInt16;

   --  Timerx Compare 4 Register
   type CMP4DR_Register is record
      --  Timerx Compare 4 value
      CMP4x          : CMP4DR_CMP4x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP4DR_Register use record
      CMP4x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CPT1DR_CPT1x_Field is HAL.UInt16;

   --  Timerx Capture 1 Register
   type CPT1DR_Register is record
      --  Read-only. Timerx Capture 1 value
      CPT1x          : CPT1DR_CPT1x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1DR_Register use record
      CPT1x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CPT2DR_CPT2x_Field is HAL.UInt16;

   --  Timerx Capture 2 Register
   type CPT2DR_Register is record
      --  Read-only. Timerx Capture 2 value
      CPT2x          : CPT2DR_CPT2x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2DR_Register use record
      CPT2x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DTDR_DTRx_Field is HAL.UInt9;
   subtype DTDR_DTPRSC_Field is HAL.UInt3;
   subtype DTDR_DTFx_Field is HAL.UInt9;

   --  Timerx Deadtime Register
   type DTDR_Register is record
      --  Deadtime Rising value
      DTRx           : DTDR_DTRx_Field := 16#0#;
      --  Sign Deadtime Rising value
      SDTRx          : Boolean := False;
      --  Deadtime Prescaler
      DTPRSC         : DTDR_DTPRSC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Deadtime Rising Sign Lock
      DTRSLKx        : Boolean := False;
      --  Deadtime Rising Lock
      DTRLKx         : Boolean := False;
      --  Deadtime Falling value
      DTFx           : DTDR_DTFx_Field := 16#0#;
      --  Sign Deadtime Falling value
      SDTFx          : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Deadtime Falling Sign Lock
      DTFSLKx        : Boolean := False;
      --  Deadtime Falling Lock
      DTFLKx         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTDR_Register use record
      DTRx           at 0 range 0 .. 8;
      SDTRx          at 0 range 9 .. 9;
      DTPRSC         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      DTRSLKx        at 0 range 14 .. 14;
      DTRLKx         at 0 range 15 .. 15;
      DTFx           at 0 range 16 .. 24;
      SDTFx          at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DTFSLKx        at 0 range 30 .. 30;
      DTFLKx         at 0 range 31 .. 31;
   end record;

   --  SETD1R_CMP array
   type SETD1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETD1R_CMP
   type SETD1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETD1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETD1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETD1R_MSTCMP array
   type SETD1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETD1R_MSTCMP
   type SETD1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETD1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETD1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETD1R_TIMEVNT array
   type SETD1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETD1R_TIMEVNT
   type SETD1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETD1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETD1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETD1R_EXTEVNT array
   type SETD1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETD1R_EXTEVNT
   type SETD1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETD1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETD1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Set Register
   type SETD1R_Register is record
      --  Software Set trigger
      SST     : Boolean := False;
      --  Timer A resynchronizaton
      RESYNC  : Boolean := False;
      --  Timer A Period
      PER     : Boolean := False;
      --  Timer A compare 1
      CMP     : SETD1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master Period
      MSTPER  : Boolean := False;
      --  Master Compare 1
      MSTCMP  : SETD1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer Event 1
      TIMEVNT : SETD1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT : SETD1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Registers update (transfer preload to active)
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETD1R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTD1R_CMP array
   type RSTD1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTD1R_CMP
   type RSTD1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTD1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTD1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTD1R_MSTCMP array
   type RSTD1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTD1R_MSTCMP
   type RSTD1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTD1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTD1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTD1R_TIMEVNT array
   type RSTD1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTD1R_TIMEVNT
   type RSTD1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTD1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTD1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTD1R_EXTEVNT array
   type RSTD1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTD1R_EXTEVNT
   type RSTD1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTD1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTD1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Reset Register
   type RSTD1R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTD1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTD1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTD1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTD1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTD1R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  SETD2R_CMP array
   type SETD2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETD2R_CMP
   type SETD2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETD2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETD2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETD2R_MSTCMP array
   type SETD2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETD2R_MSTCMP
   type SETD2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETD2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETD2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETD2R_TIMEVNT array
   type SETD2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETD2R_TIMEVNT
   type SETD2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETD2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETD2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETD2R_EXTEVNT array
   type SETD2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETD2R_EXTEVNT
   type SETD2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETD2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETD2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Set Register
   type SETD2R_Register is record
      --  SST
      SST     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : SETD2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : SETD2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : SETD2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : SETD2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETD2R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTD2R_CMP array
   type RSTD2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTD2R_CMP
   type RSTD2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTD2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTD2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTD2R_MSTCMP array
   type RSTD2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTD2R_MSTCMP
   type RSTD2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTD2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTD2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTD2R_TIMEVNT array
   type RSTD2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTD2R_TIMEVNT
   type RSTD2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTD2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTD2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTD2R_EXTEVNT array
   type RSTD2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTD2R_EXTEVNT
   type RSTD2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTD2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTD2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Reset Register
   type RSTD2R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTD2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTD2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTD2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTD2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTD2R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   subtype EEFDR1_EE1FLTR_Field is HAL.UInt4;
   subtype EEFDR1_EE2FLTR_Field is HAL.UInt4;
   subtype EEFDR1_EE3FLTR_Field is HAL.UInt4;
   subtype EEFDR1_EE4FLTR_Field is HAL.UInt4;
   subtype EEFDR1_EE5FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 1
   type EEFDR1_Register is record
      --  External Event 1 latch
      EE1LTCH        : Boolean := False;
      --  External Event 1 filter
      EE1FLTR        : EEFDR1_EE1FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 2 latch
      EE2LTCH        : Boolean := False;
      --  External Event 2 filter
      EE2FLTR        : EEFDR1_EE2FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 3 latch
      EE3LTCH        : Boolean := False;
      --  External Event 3 filter
      EE3FLTR        : EEFDR1_EE3FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 4 latch
      EE4LTCH        : Boolean := False;
      --  External Event 4 filter
      EE4FLTR        : EEFDR1_EE4FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 5 latch
      EE5LTCH        : Boolean := False;
      --  External Event 5 filter
      EE5FLTR        : EEFDR1_EE5FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFDR1_Register use record
      EE1LTCH        at 0 range 0 .. 0;
      EE1FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE2LTCH        at 0 range 6 .. 6;
      EE2FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE3LTCH        at 0 range 12 .. 12;
      EE3FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE4LTCH        at 0 range 18 .. 18;
      EE4FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE5LTCH        at 0 range 24 .. 24;
      EE5FLTR        at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EEFDR2_EE6FLTR_Field is HAL.UInt4;
   subtype EEFDR2_EE7FLTR_Field is HAL.UInt4;
   subtype EEFDR2_EE8FLTR_Field is HAL.UInt4;
   subtype EEFDR2_EE9FLTR_Field is HAL.UInt4;
   subtype EEFDR2_EE10FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 2
   type EEFDR2_Register is record
      --  External Event 6 latch
      EE6LTCH        : Boolean := False;
      --  External Event 6 filter
      EE6FLTR        : EEFDR2_EE6FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 7 latch
      EE7LTCH        : Boolean := False;
      --  External Event 7 filter
      EE7FLTR        : EEFDR2_EE7FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 8 latch
      EE8LTCH        : Boolean := False;
      --  External Event 8 filter
      EE8FLTR        : EEFDR2_EE8FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 9 latch
      EE9LTCH        : Boolean := False;
      --  External Event 9 filter
      EE9FLTR        : EEFDR2_EE9FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 10 latch
      EE10LTCH       : Boolean := False;
      --  External Event 10 filter
      EE10FLTR       : EEFDR2_EE10FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFDR2_Register use record
      EE6LTCH        at 0 range 0 .. 0;
      EE6FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7LTCH        at 0 range 6 .. 6;
      EE7FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8LTCH        at 0 range 12 .. 12;
      EE8FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9LTCH        at 0 range 18 .. 18;
      EE9FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10LTCH       at 0 range 24 .. 24;
      EE10FLTR       at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  RSTDR_CMP array
   type RSTDR_CMP_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RSTDR_CMP
   type RSTDR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  CMP as an array
            Arr : RSTDR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RSTDR_CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RSTDR_MSTCMP array
   type RSTDR_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTDR_MSTCMP
   type RSTDR_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTDR_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTDR_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTDR_EXTEVNT array
   type RSTDR_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTDR_EXTEVNT
   type RSTDR_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTDR_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTDR_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  RSTDR_TIMACMP array
   type RSTDR_TIMACMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTDR_TIMACMP
   type RSTDR_TIMACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMACMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMACMP as an array
            Arr : RSTDR_TIMACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTDR_TIMACMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTDR_TIMBCMP array
   type RSTDR_TIMBCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTDR_TIMBCMP
   type RSTDR_TIMBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMBCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMBCMP as an array
            Arr : RSTDR_TIMBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTDR_TIMBCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTDR_TIMCCMP array
   type RSTDR_TIMCCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTDR_TIMCCMP
   type RSTDR_TIMCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMCCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMCCMP as an array
            Arr : RSTDR_TIMCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTDR_TIMCCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTDR_TIMECMP array
   type RSTDR_TIMECMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTDR_TIMECMP
   type RSTDR_TIMECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMECMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMECMP as an array
            Arr : RSTDR_TIMECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTDR_TIMECMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TimerA Reset Register
   type RSTDR_Register is record
      --  Timer A Update reset
      TIMFCMP1 : Boolean := False;
      --  Timer A Update reset
      UPDT     : Boolean := False;
      --  Timer A compare 2 reset
      CMP      : RSTDR_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master timer Period
      MSTPER   : Boolean := False;
      --  Master compare 1
      MSTCMP   : RSTDR_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT  : RSTDR_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Timer A Compare 1
      TIMACMP  : RSTDR_TIMACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B Compare 1
      TIMBCMP  : RSTDR_TIMBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C Compare 1
      TIMCCMP  : RSTDR_TIMCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E Compare 1
      TIMECMP  : RSTDR_TIMECMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer F Compare 2
      TIMFCPM2 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTDR_Register use record
      TIMFCMP1 at 0 range 0 .. 0;
      UPDT     at 0 range 1 .. 1;
      CMP      at 0 range 2 .. 3;
      MSTPER   at 0 range 4 .. 4;
      MSTCMP   at 0 range 5 .. 8;
      EXTEVNT  at 0 range 9 .. 18;
      TIMACMP  at 0 range 19 .. 21;
      TIMBCMP  at 0 range 22 .. 24;
      TIMCCMP  at 0 range 25 .. 27;
      TIMECMP  at 0 range 28 .. 30;
      TIMFCPM2 at 0 range 31 .. 31;
   end record;

   subtype CHPDR_CHPFRQ_Field is HAL.UInt4;
   subtype CHPDR_CHPDTY_Field is HAL.UInt3;
   subtype CHPDR_STRTPW_Field is HAL.UInt4;

   --  Timerx Chopper Register
   type CHPDR_Register is record
      --  Timerx carrier frequency value
      CHPFRQ         : CHPDR_CHPFRQ_Field := 16#0#;
      --  Timerx chopper duty cycle value
      CHPDTY         : CHPDR_CHPDTY_Field := 16#0#;
      --  STRTPW
      STRTPW         : CHPDR_STRTPW_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHPDR_Register use record
      CHPFRQ         at 0 range 0 .. 3;
      CHPDTY         at 0 range 4 .. 6;
      STRTPW         at 0 range 7 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CPT1DCR_TACMP array
   type CPT1DCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1DCR_TACMP
   type CPT1DCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT1DCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1DCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1DCR_TBCMP array
   type CPT1DCR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1DCR_TBCMP
   type CPT1DCR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT1DCR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1DCR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1DCR_TCCMP array
   type CPT1DCR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1DCR_TCCMP
   type CPT1DCR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT1DCR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1DCR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1DCR_TFCMP array
   type CPT1DCR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1DCR_TFCMP
   type CPT1DCR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT1DCR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1DCR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1DCR_TECMP array
   type CPT1DCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1DCR_TECMP
   type CPT1DCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT1DCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1DCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 1 Control Register
   type CPT1DCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT1DCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT1DCR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT1DCR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT1DCR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT1DCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1DCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TF1SET    at 0 range 24 .. 24;
      TF1RST    at 0 range 25 .. 25;
      TFCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   --  CPT2DCR_TACMP array
   type CPT2DCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2DCR_TACMP
   type CPT2DCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT2DCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2DCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2DCR_TBCMP array
   type CPT2DCR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2DCR_TBCMP
   type CPT2DCR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT2DCR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2DCR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2DCR_TCCMP array
   type CPT2DCR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2DCR_TCCMP
   type CPT2DCR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT2DCR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2DCR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2DCR_TFCMP array
   type CPT2DCR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2DCR_TFCMP
   type CPT2DCR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT2DCR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2DCR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2DCR_TECMP array
   type CPT2DCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2DCR_TECMP
   type CPT2DCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT2DCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2DCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 2 Control Register
   type CPT2DCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT2DCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT2DCR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT2DCR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT2DCR_TFCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer E output 1 Set
      TE1SET    : Boolean := False;
      --  Timer E output 1 Reset
      TE1RST    : Boolean := False;
      --  Timer E Compare 1
      TECMP     : CPT2DCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2DCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TF1SET    at 0 range 24 .. 24;
      TF1RST    at 0 range 25 .. 25;
      TFCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   subtype OUTDR_FAULT1_Field is HAL.UInt2;
   subtype OUTDR_DLYPRT_Field is HAL.UInt3;
   subtype OUTDR_FAULT2_Field is HAL.UInt2;

   --  Timerx Output Register
   type OUTDR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Output 1 polarity
      POL1           : Boolean := False;
      --  Output 1 Idle mode
      IDLEM1         : Boolean := False;
      --  Output 1 Idle State
      IDLES1         : Boolean := False;
      --  Output 1 Fault state
      FAULT1         : OUTDR_FAULT1_Field := 16#0#;
      --  Output 1 Chopper enable
      CHP1           : Boolean := False;
      --  Output 1 Deadtime upon burst mode Idle entry
      DIDL1          : Boolean := False;
      --  Deadtime enable
      DTEN           : Boolean := False;
      --  Delayed Protection Enable
      DLYPRTEN       : Boolean := False;
      --  Delayed Protection
      DLYPRT         : OUTDR_DLYPRT_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Balanced Idle Automatic Resume
      BIAR           : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Output 2 polarity
      POL2           : Boolean := False;
      --  Output 2 Idle mode
      IDLEM2         : Boolean := False;
      --  Output 2 Idle State
      IDLES2         : Boolean := False;
      --  Output 2 Fault state
      FAULT2         : OUTDR_FAULT2_Field := 16#0#;
      --  Output 2 Chopper enable
      CHP2           : Boolean := False;
      --  Output 2 Deadtime upon burst mode Idle entry
      DIDL2          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTDR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      POL1           at 0 range 1 .. 1;
      IDLEM1         at 0 range 2 .. 2;
      IDLES1         at 0 range 3 .. 3;
      FAULT1         at 0 range 4 .. 5;
      CHP1           at 0 range 6 .. 6;
      DIDL1          at 0 range 7 .. 7;
      DTEN           at 0 range 8 .. 8;
      DLYPRTEN       at 0 range 9 .. 9;
      DLYPRT         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      BIAR           at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      POL2           at 0 range 17 .. 17;
      IDLEM2         at 0 range 18 .. 18;
      IDLES2         at 0 range 19 .. 19;
      FAULT2         at 0 range 20 .. 21;
      CHP2           at 0 range 22 .. 22;
      DIDL2          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Timerx Fault Register
   type FLTDR_Register is record
      --  Fault 1 enable
      FLT1EN        : Boolean := False;
      --  Fault 2 enable
      FLT2EN        : Boolean := False;
      --  Fault 3 enable
      FLT3EN        : Boolean := False;
      --  Fault 4 enable
      FLT4EN        : Boolean := False;
      --  Fault 5 enable
      FLT5EN        : Boolean := False;
      --  Fault 6 enable
      FLT6EN        : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  Fault sources Lock
      FLTLCK        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTDR_Register use record
      FLT1EN        at 0 range 0 .. 0;
      FLT2EN        at 0 range 1 .. 1;
      FLT3EN        at 0 range 2 .. 2;
      FLT4EN        at 0 range 3 .. 3;
      FLT5EN        at 0 range 4 .. 4;
      FLT6EN        at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      FLTLCK        at 0 range 31 .. 31;
   end record;

   subtype TIMDCR2_ROM_Field is HAL.UInt2;
   subtype TIMDCR2_OUTROM_Field is HAL.UInt2;
   subtype TIMDCR2_ADROM_Field is HAL.UInt2;
   subtype TIMDCR2_BMROM_Field is HAL.UInt2;
   subtype TIMDCR2_FEROM_Field is HAL.UInt2;

   --  TIMDCR2_GTCMP array
   type TIMDCR2_GTCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMDCR2_GTCMP
   type TIMDCR2_GTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GTCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  GTCMP as an array
            Arr : TIMDCR2_GTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMDCR2_GTCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  HRTIM Timerx Control Register 2
   type TIMDCR2_Register is record
      --  Dual Channel DAC trigger enable
      DCDE           : Boolean := False;
      --  Dual Channel DAC Step trigger
      DCDS           : Boolean := False;
      --  Dual Channel DAC Reset trigger
      DCDR           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Up-Down Mode
      UDM            : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Roll-Over Mode
      ROM            : TIMDCR2_ROM_Field := 16#0#;
      --  Output Roll-Over Mode
      OUTROM         : TIMDCR2_OUTROM_Field := 16#0#;
      --  ADC Roll-Over Mode
      ADROM          : TIMDCR2_ADROM_Field := 16#0#;
      --  Burst Mode Roll-Over Mode
      BMROM          : TIMDCR2_BMROM_Field := 16#0#;
      --  Fault and Event Roll-Over Mode
      FEROM          : TIMDCR2_FEROM_Field := 16#0#;
      --  Greater than Compare 1 PWM mode
      GTCMP          : TIMDCR2_GTCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Triggered-half mode
      TRGHLF         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMDCR2_Register use record
      DCDE           at 0 range 0 .. 0;
      DCDS           at 0 range 1 .. 1;
      DCDR           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      UDM            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ROM            at 0 range 6 .. 7;
      OUTROM         at 0 range 8 .. 9;
      ADROM          at 0 range 10 .. 11;
      BMROM          at 0 range 12 .. 13;
      FEROM          at 0 range 14 .. 15;
      GTCMP          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TRGHLF         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype EEFDR3_EEVASEL_Field is HAL.UInt4;
   subtype EEFDR3_EEVACNT_Field is HAL.UInt6;

   --  HRTIM Timerx External Event Filtering Register 3
   type EEFDR3_Register is record
      --  External Event A Counter Enable
      EEVACE         : Boolean := False;
      --  External Event A Counter Reset
      EEVACRES       : Boolean := False;
      --  External Event A Reset Mode
      EEVARSTM       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  External Event A Selection
      EEVASEL        : EEFDR3_EEVASEL_Field := 16#0#;
      --  External Event A counter
      EEVACNT        : EEFDR3_EEVACNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFDR3_Register use record
      EEVACE         at 0 range 0 .. 0;
      EEVACRES       at 0 range 1 .. 1;
      EEVARSTM       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EEVASEL        at 0 range 4 .. 7;
      EEVACNT        at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TIMECR_CKPSCx_Field is HAL.UInt3;
   subtype TIMECR_INTLVD_Field is HAL.UInt2;
   --  TIMECR_DELCMP array element
   subtype TIMECR_DELCMP_Element is HAL.UInt2;

   --  TIMECR_DELCMP array
   type TIMECR_DELCMP_Field_Array is array (2 .. 3) of TIMECR_DELCMP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for TIMECR_DELCMP
   type TIMECR_DELCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DELCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  DELCMP as an array
            Arr : TIMECR_DELCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMECR_DELCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype TIMECR_DACSYNC_Field is HAL.UInt2;
   subtype TIMECR_UPDGAT_Field is HAL.UInt4;

   --  Timerx Control Register
   type TIMECR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMECR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMECR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMECR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  TFU
      TFU            : Boolean := False;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  TAU
      TAU            : Boolean := False;
      --  TBU
      TBU            : Boolean := False;
      --  TCU
      TCU            : Boolean := False;
      --  TDU
      TDU            : Boolean := False;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMECR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMECR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMECR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      TFU            at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      TAU            at 0 range 19 .. 19;
      TBU            at 0 range 20 .. 20;
      TCU            at 0 range 21 .. 21;
      TDU            at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  TIMEISR_CMP array
   type TIMEISR_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TIMEISR_CMP
   type TIMEISR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : TIMEISR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMEISR_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TIMEISR_CPT array
   type TIMEISR_CPT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMEISR_CPT
   type TIMEISR_CPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CPT as a value
            Val : HAL.UInt2;
         when True =>
            --  CPT as an array
            Arr : TIMEISR_CPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMEISR_CPT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Interrupt Status Register
   type TIMEISR_Register is record
      --  Read-only. Compare 1 Interrupt Flag
      CMP            : TIMEISR_CMP_Field;
      --  Read-only. Repetition Interrupt Flag
      REP            : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Update Interrupt Flag
      UPD            : Boolean;
      --  Read-only. Capture1 Interrupt Flag
      CPT            : TIMEISR_CPT_Field;
      --  Read-only. Output 1 Set Interrupt Flag
      SETx1          : Boolean;
      --  Read-only. Output 1 Reset Interrupt Flag
      RSTx1          : Boolean;
      --  Read-only. Output 2 Set Interrupt Flag
      SETx2          : Boolean;
      --  Read-only. Output 2 Reset Interrupt Flag
      RSTx2          : Boolean;
      --  Read-only. Reset Interrupt Flag
      RST            : Boolean;
      --  Read-only. Delayed Protection Flag
      DLYPRT         : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Current Push Pull Status
      CPPSTAT        : Boolean;
      --  Read-only. Idle Push Pull Status
      IPPSTAT        : Boolean;
      --  Read-only. Output 1 State
      O1STAT         : Boolean;
      --  Read-only. Output 2 State
      O2STAT         : Boolean;
      --  Read-only. Output 1 Copy
      O1CPY          : Boolean;
      --  Read-only. Output 2 Copy
      O2CPY          : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMEISR_Register use record
      CMP            at 0 range 0 .. 3;
      REP            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPD            at 0 range 6 .. 6;
      CPT            at 0 range 7 .. 8;
      SETx1          at 0 range 9 .. 9;
      RSTx1          at 0 range 10 .. 10;
      SETx2          at 0 range 11 .. 11;
      RSTx2          at 0 range 12 .. 12;
      RST            at 0 range 13 .. 13;
      DLYPRT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CPPSTAT        at 0 range 16 .. 16;
      IPPSTAT        at 0 range 17 .. 17;
      O1STAT         at 0 range 18 .. 18;
      O2STAT         at 0 range 19 .. 19;
      O1CPY          at 0 range 20 .. 20;
      O2CPY          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Timerx Interrupt Clear Register
   type TIMEICR_Register is record
      --  Write-only. Compare 1 Interrupt flag Clear
      CMP1C          : Boolean := False;
      --  Write-only. Compare 2 Interrupt flag Clear
      CMP2C          : Boolean := False;
      --  Write-only. Compare 3 Interrupt flag Clear
      CMP3C          : Boolean := False;
      --  Write-only. Compare 4 Interrupt flag Clear
      CMP4C          : Boolean := False;
      --  Write-only. Repetition Interrupt flag Clear
      REPC           : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Update Interrupt flag Clear
      UPDC           : Boolean := False;
      --  Write-only. Capture1 Interrupt flag Clear
      CPT1C          : Boolean := False;
      --  Write-only. Capture2 Interrupt flag Clear
      CPT2C          : Boolean := False;
      --  Write-only. Output 1 Set flag Clear
      SET1xC         : Boolean := False;
      --  Write-only. Output 1 Reset flag Clear
      RSTx1C         : Boolean := False;
      --  Write-only. Output 2 Set flag Clear
      SET2xC         : Boolean := False;
      --  Write-only. Output 2 Reset flag Clear
      RSTx2C         : Boolean := False;
      --  Write-only. Reset Interrupt flag Clear
      RSTC           : Boolean := False;
      --  Write-only. Delayed Protection Flag Clear
      DLYPRTC        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMEICR_Register use record
      CMP1C          at 0 range 0 .. 0;
      CMP2C          at 0 range 1 .. 1;
      CMP3C          at 0 range 2 .. 2;
      CMP4C          at 0 range 3 .. 3;
      REPC           at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDC           at 0 range 6 .. 6;
      CPT1C          at 0 range 7 .. 7;
      CPT2C          at 0 range 8 .. 8;
      SET1xC         at 0 range 9 .. 9;
      RSTx1C         at 0 range 10 .. 10;
      SET2xC         at 0 range 11 .. 11;
      RSTx2C         at 0 range 12 .. 12;
      RSTC           at 0 range 13 .. 13;
      DLYPRTC        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  TIMxDIER
   type TIMEDIER_Register is record
      --  CMP1IE
      CMP1IE         : Boolean := False;
      --  CMP2IE
      CMP2IE         : Boolean := False;
      --  CMP3IE
      CMP3IE         : Boolean := False;
      --  CMP4IE
      CMP4IE         : Boolean := False;
      --  REPIE
      REPIE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  UPDIE
      UPDIE          : Boolean := False;
      --  CPT1IE
      CPT1IE         : Boolean := False;
      --  CPT2IE
      CPT2IE         : Boolean := False;
      --  SET1xIE
      SET1xIE        : Boolean := False;
      --  RSTx1IE
      RSTx1IE        : Boolean := False;
      --  SETx2IE
      SETx2IE        : Boolean := False;
      --  RSTx2IE
      RSTx2IE        : Boolean := False;
      --  RSTIE
      RSTIE          : Boolean := False;
      --  DLYPRTIE
      DLYPRTIE       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  CMP1DE
      CMP1DE         : Boolean := False;
      --  CMP2DE
      CMP2DE         : Boolean := False;
      --  CMP3DE
      CMP3DE         : Boolean := False;
      --  CMP4DE
      CMP4DE         : Boolean := False;
      --  REPDE
      REPDE          : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  UPDDE
      UPDDE          : Boolean := False;
      --  CPT1DE
      CPT1DE         : Boolean := False;
      --  CPT2DE
      CPT2DE         : Boolean := False;
      --  SET1xDE
      SET1xDE        : Boolean := False;
      --  RSTx1DE
      RSTx1DE        : Boolean := False;
      --  SETx2DE
      SETx2DE        : Boolean := False;
      --  RSTx2DE
      RSTx2DE        : Boolean := False;
      --  RSTDE
      RSTDE          : Boolean := False;
      --  DLYPRTDE
      DLYPRTDE       : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMEDIER_Register use record
      CMP1IE         at 0 range 0 .. 0;
      CMP2IE         at 0 range 1 .. 1;
      CMP3IE         at 0 range 2 .. 2;
      CMP4IE         at 0 range 3 .. 3;
      REPIE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDIE          at 0 range 6 .. 6;
      CPT1IE         at 0 range 7 .. 7;
      CPT2IE         at 0 range 8 .. 8;
      SET1xIE        at 0 range 9 .. 9;
      RSTx1IE        at 0 range 10 .. 10;
      SETx2IE        at 0 range 11 .. 11;
      RSTx2IE        at 0 range 12 .. 12;
      RSTIE          at 0 range 13 .. 13;
      DLYPRTIE       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CMP1DE         at 0 range 16 .. 16;
      CMP2DE         at 0 range 17 .. 17;
      CMP3DE         at 0 range 18 .. 18;
      CMP4DE         at 0 range 19 .. 19;
      REPDE          at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      UPDDE          at 0 range 22 .. 22;
      CPT1DE         at 0 range 23 .. 23;
      CPT2DE         at 0 range 24 .. 24;
      SET1xDE        at 0 range 25 .. 25;
      RSTx1DE        at 0 range 26 .. 26;
      SETx2DE        at 0 range 27 .. 27;
      RSTx2DE        at 0 range 28 .. 28;
      RSTDE          at 0 range 29 .. 29;
      DLYPRTDE       at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CNTER_CNTx_Field is HAL.UInt16;

   --  Timerx Counter Register
   type CNTER_Register is record
      --  Timerx Counter value
      CNTx           : CNTER_CNTx_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTER_Register use record
      CNTx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PERER_PERx_Field is HAL.UInt16;

   --  Timerx Period Register
   type PERER_Register is record
      --  Timerx Period value
      PERx           : PERER_PERx_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERER_Register use record
      PERx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype REPER_REPx_Field is HAL.UInt8;

   --  Timerx Repetition Register
   type REPER_Register is record
      --  Timerx Repetition counter value
      REPx          : REPER_REPx_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPER_Register use record
      REPx          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CMP1ER_CMP1x_Field is HAL.UInt16;

   --  Timerx Compare 1 Register
   type CMP1ER_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1ER_CMP1x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1ER_Register use record
      CMP1x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP1CER_CMP1x_Field is HAL.UInt16;
   subtype CMP1CER_REPx_Field is HAL.UInt8;

   --  Timerx Compare 1 Compound Register
   type CMP1CER_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CER_CMP1x_Field := 16#0#;
      --  Timerx Repetition value (aliased from HRTIM_REPx register)
      REPx           : CMP1CER_REPx_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CER_Register use record
      CMP1x          at 0 range 0 .. 15;
      REPx           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CMP2ER_CMP2x_Field is HAL.UInt16;

   --  Timerx Compare 2 Register
   type CMP2ER_Register is record
      --  Timerx Compare 2 value
      CMP2x          : CMP2ER_CMP2x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP2ER_Register use record
      CMP2x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP3ER_CMP3x_Field is HAL.UInt16;

   --  Timerx Compare 3 Register
   type CMP3ER_Register is record
      --  Timerx Compare 3 value
      CMP3x          : CMP3ER_CMP3x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP3ER_Register use record
      CMP3x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP4ER_CMP4x_Field is HAL.UInt16;

   --  Timerx Compare 4 Register
   type CMP4ER_Register is record
      --  Timerx Compare 4 value
      CMP4x          : CMP4ER_CMP4x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP4ER_Register use record
      CMP4x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CPT1ER_CPT1x_Field is HAL.UInt16;

   --  Timerx Capture 1 Register
   type CPT1ER_Register is record
      --  Read-only. Timerx Capture 1 value
      CPT1x          : CPT1ER_CPT1x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1ER_Register use record
      CPT1x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CPT2ER_CPT2x_Field is HAL.UInt16;

   --  Timerx Capture 2 Register
   type CPT2ER_Register is record
      --  Read-only. Timerx Capture 2 value
      CPT2x          : CPT2ER_CPT2x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2ER_Register use record
      CPT2x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DTER_DTRx_Field is HAL.UInt9;
   subtype DTER_DTPRSC_Field is HAL.UInt3;
   subtype DTER_DTFx_Field is HAL.UInt9;

   --  Timerx Deadtime Register
   type DTER_Register is record
      --  Deadtime Rising value
      DTRx           : DTER_DTRx_Field := 16#0#;
      --  Sign Deadtime Rising value
      SDTRx          : Boolean := False;
      --  Deadtime Prescaler
      DTPRSC         : DTER_DTPRSC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Deadtime Rising Sign Lock
      DTRSLKx        : Boolean := False;
      --  Deadtime Rising Lock
      DTRLKx         : Boolean := False;
      --  Deadtime Falling value
      DTFx           : DTER_DTFx_Field := 16#0#;
      --  Sign Deadtime Falling value
      SDTFx          : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Deadtime Falling Sign Lock
      DTFSLKx        : Boolean := False;
      --  Deadtime Falling Lock
      DTFLKx         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTER_Register use record
      DTRx           at 0 range 0 .. 8;
      SDTRx          at 0 range 9 .. 9;
      DTPRSC         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      DTRSLKx        at 0 range 14 .. 14;
      DTRLKx         at 0 range 15 .. 15;
      DTFx           at 0 range 16 .. 24;
      SDTFx          at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DTFSLKx        at 0 range 30 .. 30;
      DTFLKx         at 0 range 31 .. 31;
   end record;

   --  SETE1R_CMP array
   type SETE1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETE1R_CMP
   type SETE1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETE1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETE1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETE1R_MSTCMP array
   type SETE1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETE1R_MSTCMP
   type SETE1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETE1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETE1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETE1R_TIMEVNT array
   type SETE1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETE1R_TIMEVNT
   type SETE1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETE1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETE1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETE1R_EXTEVNT array
   type SETE1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETE1R_EXTEVNT
   type SETE1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETE1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETE1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Set Register
   type SETE1R_Register is record
      --  Software Set trigger
      SST     : Boolean := False;
      --  Timer A resynchronizaton
      RESYNC  : Boolean := False;
      --  Timer A Period
      PER     : Boolean := False;
      --  Timer A compare 1
      CMP     : SETE1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master Period
      MSTPER  : Boolean := False;
      --  Master Compare 1
      MSTCMP  : SETE1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer Event 1
      TIMEVNT : SETE1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT : SETE1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Registers update (transfer preload to active)
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETE1R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTE1R_CMP array
   type RSTE1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTE1R_CMP
   type RSTE1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTE1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTE1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTE1R_MSTCMP array
   type RSTE1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTE1R_MSTCMP
   type RSTE1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTE1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTE1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTE1R_TIMEVNT array
   type RSTE1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTE1R_TIMEVNT
   type RSTE1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTE1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTE1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTE1R_EXTEVNT array
   type RSTE1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTE1R_EXTEVNT
   type RSTE1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTE1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTE1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Reset Register
   type RSTE1R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTE1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTE1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTE1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTE1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTE1R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  SETE2R_CMP array
   type SETE2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETE2R_CMP
   type SETE2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETE2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETE2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETE2R_MSTCMP array
   type SETE2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETE2R_MSTCMP
   type SETE2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETE2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETE2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETE2R_TIMEVNT array
   type SETE2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETE2R_TIMEVNT
   type SETE2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETE2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETE2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETE2R_EXTEVNT array
   type SETE2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETE2R_EXTEVNT
   type SETE2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETE2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETE2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Set Register
   type SETE2R_Register is record
      --  SST
      SST     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : SETE2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : SETE2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : SETE2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : SETE2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETE2R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTE2R_CMP array
   type RSTE2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTE2R_CMP
   type RSTE2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTE2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTE2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTE2R_MSTCMP array
   type RSTE2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTE2R_MSTCMP
   type RSTE2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTE2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTE2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTE2R_TIMEVNT array
   type RSTE2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTE2R_TIMEVNT
   type RSTE2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTE2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTE2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTE2R_EXTEVNT array
   type RSTE2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTE2R_EXTEVNT
   type RSTE2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTE2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTE2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Reset Register
   type RSTE2R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTE2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTE2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTE2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTE2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTE2R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   subtype EEFER1_EE1FLTR_Field is HAL.UInt4;
   subtype EEFER1_EE2FLTR_Field is HAL.UInt4;
   subtype EEFER1_EE3FLTR_Field is HAL.UInt4;
   subtype EEFER1_EE4FLTR_Field is HAL.UInt4;
   subtype EEFER1_EE5FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 1
   type EEFER1_Register is record
      --  External Event 1 latch
      EE1LTCH        : Boolean := False;
      --  External Event 1 filter
      EE1FLTR        : EEFER1_EE1FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 2 latch
      EE2LTCH        : Boolean := False;
      --  External Event 2 filter
      EE2FLTR        : EEFER1_EE2FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 3 latch
      EE3LTCH        : Boolean := False;
      --  External Event 3 filter
      EE3FLTR        : EEFER1_EE3FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 4 latch
      EE4LTCH        : Boolean := False;
      --  External Event 4 filter
      EE4FLTR        : EEFER1_EE4FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 5 latch
      EE5LTCH        : Boolean := False;
      --  External Event 5 filter
      EE5FLTR        : EEFER1_EE5FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFER1_Register use record
      EE1LTCH        at 0 range 0 .. 0;
      EE1FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE2LTCH        at 0 range 6 .. 6;
      EE2FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE3LTCH        at 0 range 12 .. 12;
      EE3FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE4LTCH        at 0 range 18 .. 18;
      EE4FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE5LTCH        at 0 range 24 .. 24;
      EE5FLTR        at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EEFER2_EE6FLTR_Field is HAL.UInt4;
   subtype EEFER2_EE7FLTR_Field is HAL.UInt4;
   subtype EEFER2_EE8FLTR_Field is HAL.UInt4;
   subtype EEFER2_EE9FLTR_Field is HAL.UInt4;
   subtype EEFER2_EE10FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 2
   type EEFER2_Register is record
      --  External Event 6 latch
      EE6LTCH        : Boolean := False;
      --  External Event 6 filter
      EE6FLTR        : EEFER2_EE6FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 7 latch
      EE7LTCH        : Boolean := False;
      --  External Event 7 filter
      EE7FLTR        : EEFER2_EE7FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 8 latch
      EE8LTCH        : Boolean := False;
      --  External Event 8 filter
      EE8FLTR        : EEFER2_EE8FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 9 latch
      EE9LTCH        : Boolean := False;
      --  External Event 9 filter
      EE9FLTR        : EEFER2_EE9FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 10 latch
      EE10LTCH       : Boolean := False;
      --  External Event 10 filter
      EE10FLTR       : EEFER2_EE10FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFER2_Register use record
      EE6LTCH        at 0 range 0 .. 0;
      EE6FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7LTCH        at 0 range 6 .. 6;
      EE7FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8LTCH        at 0 range 12 .. 12;
      EE8FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9LTCH        at 0 range 18 .. 18;
      EE9FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10LTCH       at 0 range 24 .. 24;
      EE10FLTR       at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  RSTER_CMP array
   type RSTER_CMP_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RSTER_CMP
   type RSTER_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  CMP as an array
            Arr : RSTER_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RSTER_CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RSTER_MSTCMP array
   type RSTER_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTER_MSTCMP
   type RSTER_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTER_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTER_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTER_EXTEVNT array
   type RSTER_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTER_EXTEVNT
   type RSTER_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTER_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTER_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  RSTER_TIMACMP array
   type RSTER_TIMACMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTER_TIMACMP
   type RSTER_TIMACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMACMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMACMP as an array
            Arr : RSTER_TIMACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTER_TIMACMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTER_TIMBCMP array
   type RSTER_TIMBCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTER_TIMBCMP
   type RSTER_TIMBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMBCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMBCMP as an array
            Arr : RSTER_TIMBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTER_TIMBCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTER_TIMCCMP array
   type RSTER_TIMCCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTER_TIMCCMP
   type RSTER_TIMCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMCCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMCCMP as an array
            Arr : RSTER_TIMCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTER_TIMCCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTER_TIMDCMP array
   type RSTER_TIMDCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTER_TIMDCMP
   type RSTER_TIMDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMDCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMDCMP as an array
            Arr : RSTER_TIMDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTER_TIMDCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TimerA Reset Register
   type RSTER_Register is record
      --  Timer A Update reset
      TIMFCMP1 : Boolean := False;
      --  Timer A Update reset
      UPDT     : Boolean := False;
      --  Timer A compare 2 reset
      CMP      : RSTER_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master timer Period
      MSTPER   : Boolean := False;
      --  Master compare 1
      MSTCMP   : RSTER_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT  : RSTER_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Timer A Compare 1
      TIMACMP  : RSTER_TIMACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B Compare 1
      TIMBCMP  : RSTER_TIMBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C Compare 1
      TIMCCMP  : RSTER_TIMCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D Compare 1
      TIMDCMP  : RSTER_TIMDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer F Compare 2
      TIMFCPM2 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTER_Register use record
      TIMFCMP1 at 0 range 0 .. 0;
      UPDT     at 0 range 1 .. 1;
      CMP      at 0 range 2 .. 3;
      MSTPER   at 0 range 4 .. 4;
      MSTCMP   at 0 range 5 .. 8;
      EXTEVNT  at 0 range 9 .. 18;
      TIMACMP  at 0 range 19 .. 21;
      TIMBCMP  at 0 range 22 .. 24;
      TIMCCMP  at 0 range 25 .. 27;
      TIMDCMP  at 0 range 28 .. 30;
      TIMFCPM2 at 0 range 31 .. 31;
   end record;

   subtype CHPER_CHPFRQ_Field is HAL.UInt4;
   subtype CHPER_CHPDTY_Field is HAL.UInt3;
   subtype CHPER_STRTPW_Field is HAL.UInt4;

   --  Timerx Chopper Register
   type CHPER_Register is record
      --  Timerx carrier frequency value
      CHPFRQ         : CHPER_CHPFRQ_Field := 16#0#;
      --  Timerx chopper duty cycle value
      CHPDTY         : CHPER_CHPDTY_Field := 16#0#;
      --  STRTPW
      STRTPW         : CHPER_STRTPW_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHPER_Register use record
      CHPFRQ         at 0 range 0 .. 3;
      CHPDTY         at 0 range 4 .. 6;
      STRTPW         at 0 range 7 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CPT1ECR_TACMP array
   type CPT1ECR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ECR_TACMP
   type CPT1ECR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT1ECR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ECR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ECR_TBCMP array
   type CPT1ECR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ECR_TBCMP
   type CPT1ECR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT1ECR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ECR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ECR_TCCMP array
   type CPT1ECR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ECR_TCCMP
   type CPT1ECR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT1ECR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ECR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ECR_TDCMP array
   type CPT1ECR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ECR_TDCMP
   type CPT1ECR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT1ECR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ECR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1ECR_TFCMP array
   type CPT1ECR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1ECR_TFCMP
   type CPT1ECR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT1ECR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1ECR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 1 Control Register
   type CPT1ECR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT1ECR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT1ECR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT1ECR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT1ECR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT1ECR_TFCMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1ECR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TF1SET    at 0 range 28 .. 28;
      TF1RST    at 0 range 29 .. 29;
      TFCMP     at 0 range 30 .. 31;
   end record;

   --  CPT2ECR_TACMP array
   type CPT2ECR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ECR_TACMP
   type CPT2ECR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT2ECR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ECR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ECR_TBCMP array
   type CPT2ECR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ECR_TBCMP
   type CPT2ECR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT2ECR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ECR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ECR_TCCMP array
   type CPT2ECR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ECR_TCCMP
   type CPT2ECR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT2ECR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ECR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ECR_TDCMP array
   type CPT2ECR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ECR_TDCMP
   type CPT2ECR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT2ECR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ECR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2ECR_TFCMP array
   type CPT2ECR_TFCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2ECR_TFCMP
   type CPT2ECR_TFCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TFCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TFCMP as an array
            Arr : CPT2ECR_TFCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2ECR_TFCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 2 Control Register
   type CPT2ECR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT2ECR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT2ECR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT2ECR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT2ECR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  TF1SET
      TF1SET    : Boolean := False;
      --  TF1RST
      TF1RST    : Boolean := False;
      --  TFCMP1
      TFCMP     : CPT2ECR_TFCMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2ECR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TF1SET    at 0 range 28 .. 28;
      TF1RST    at 0 range 29 .. 29;
      TFCMP     at 0 range 30 .. 31;
   end record;

   subtype OUTER_FAULT1_Field is HAL.UInt2;
   subtype OUTER_DLYPRT_Field is HAL.UInt3;
   subtype OUTER_FAULT2_Field is HAL.UInt2;

   --  Timerx Output Register
   type OUTER_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Output 1 polarity
      POL1           : Boolean := False;
      --  Output 1 Idle mode
      IDLEM1         : Boolean := False;
      --  Output 1 Idle State
      IDLES1         : Boolean := False;
      --  Output 1 Fault state
      FAULT1         : OUTER_FAULT1_Field := 16#0#;
      --  Output 1 Chopper enable
      CHP1           : Boolean := False;
      --  Output 1 Deadtime upon burst mode Idle entry
      DIDL1          : Boolean := False;
      --  Deadtime enable
      DTEN           : Boolean := False;
      --  Delayed Protection Enable
      DLYPRTEN       : Boolean := False;
      --  Delayed Protection
      DLYPRT         : OUTER_DLYPRT_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Balanced Idle Automatic Resume
      BIAR           : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Output 2 polarity
      POL2           : Boolean := False;
      --  Output 2 Idle mode
      IDLEM2         : Boolean := False;
      --  Output 2 Idle State
      IDLES2         : Boolean := False;
      --  Output 2 Fault state
      FAULT2         : OUTER_FAULT2_Field := 16#0#;
      --  Output 2 Chopper enable
      CHP2           : Boolean := False;
      --  Output 2 Deadtime upon burst mode Idle entry
      DIDL2          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTER_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      POL1           at 0 range 1 .. 1;
      IDLEM1         at 0 range 2 .. 2;
      IDLES1         at 0 range 3 .. 3;
      FAULT1         at 0 range 4 .. 5;
      CHP1           at 0 range 6 .. 6;
      DIDL1          at 0 range 7 .. 7;
      DTEN           at 0 range 8 .. 8;
      DLYPRTEN       at 0 range 9 .. 9;
      DLYPRT         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      BIAR           at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      POL2           at 0 range 17 .. 17;
      IDLEM2         at 0 range 18 .. 18;
      IDLES2         at 0 range 19 .. 19;
      FAULT2         at 0 range 20 .. 21;
      CHP2           at 0 range 22 .. 22;
      DIDL2          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Timerx Fault Register
   type FLTER_Register is record
      --  Fault 1 enable
      FLT1EN        : Boolean := False;
      --  Fault 2 enable
      FLT2EN        : Boolean := False;
      --  Fault 3 enable
      FLT3EN        : Boolean := False;
      --  Fault 4 enable
      FLT4EN        : Boolean := False;
      --  Fault 5 enable
      FLT5EN        : Boolean := False;
      --  Fault 6 enable
      FLT6EN        : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  Fault sources Lock
      FLTLCK        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTER_Register use record
      FLT1EN        at 0 range 0 .. 0;
      FLT2EN        at 0 range 1 .. 1;
      FLT3EN        at 0 range 2 .. 2;
      FLT4EN        at 0 range 3 .. 3;
      FLT5EN        at 0 range 4 .. 4;
      FLT6EN        at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      FLTLCK        at 0 range 31 .. 31;
   end record;

   subtype TIMECR2_ROM_Field is HAL.UInt2;
   subtype TIMECR2_OUTROM_Field is HAL.UInt2;
   subtype TIMECR2_ADROM_Field is HAL.UInt2;
   subtype TIMECR2_BMROM_Field is HAL.UInt2;
   subtype TIMECR2_FEROM_Field is HAL.UInt2;

   --  TIMECR2_GTCMP array
   type TIMECR2_GTCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMECR2_GTCMP
   type TIMECR2_GTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GTCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  GTCMP as an array
            Arr : TIMECR2_GTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMECR2_GTCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  HRTIM Timerx Control Register 2
   type TIMECR2_Register is record
      --  Dual Channel DAC trigger enable
      DCDE           : Boolean := False;
      --  Dual Channel DAC Step trigger
      DCDS           : Boolean := False;
      --  Dual Channel DAC Reset trigger
      DCDR           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Up-Down Mode
      UDM            : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Roll-Over Mode
      ROM            : TIMECR2_ROM_Field := 16#0#;
      --  Output Roll-Over Mode
      OUTROM         : TIMECR2_OUTROM_Field := 16#0#;
      --  ADC Roll-Over Mode
      ADROM          : TIMECR2_ADROM_Field := 16#0#;
      --  Burst Mode Roll-Over Mode
      BMROM          : TIMECR2_BMROM_Field := 16#0#;
      --  Fault and Event Roll-Over Mode
      FEROM          : TIMECR2_FEROM_Field := 16#0#;
      --  Greater than Compare 1 PWM mode
      GTCMP          : TIMECR2_GTCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Triggered-half mode
      TRGHLF         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMECR2_Register use record
      DCDE           at 0 range 0 .. 0;
      DCDS           at 0 range 1 .. 1;
      DCDR           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      UDM            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ROM            at 0 range 6 .. 7;
      OUTROM         at 0 range 8 .. 9;
      ADROM          at 0 range 10 .. 11;
      BMROM          at 0 range 12 .. 13;
      FEROM          at 0 range 14 .. 15;
      GTCMP          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TRGHLF         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype EEFER3_EEVASEL_Field is HAL.UInt4;
   subtype EEFER3_EEVACNT_Field is HAL.UInt6;

   --  HRTIM Timerx External Event Filtering Register 3
   type EEFER3_Register is record
      --  External Event A Counter Enable
      EEVACE         : Boolean := False;
      --  External Event A Counter Reset
      EEVACRES       : Boolean := False;
      --  External Event A Reset Mode
      EEVARSTM       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  External Event A Selection
      EEVASEL        : EEFER3_EEVASEL_Field := 16#0#;
      --  External Event A counter
      EEVACNT        : EEFER3_EEVACNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFER3_Register use record
      EEVACE         at 0 range 0 .. 0;
      EEVACRES       at 0 range 1 .. 1;
      EEVARSTM       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EEVASEL        at 0 range 4 .. 7;
      EEVACNT        at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype TIMFCR_CKPSCx_Field is HAL.UInt3;
   subtype TIMFCR_INTLVD_Field is HAL.UInt2;
   --  TIMFCR_DELCMP array element
   subtype TIMFCR_DELCMP_Element is HAL.UInt2;

   --  TIMFCR_DELCMP array
   type TIMFCR_DELCMP_Field_Array is array (2 .. 3) of TIMFCR_DELCMP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for TIMFCR_DELCMP
   type TIMFCR_DELCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DELCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  DELCMP as an array
            Arr : TIMFCR_DELCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMFCR_DELCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype TIMFCR_DACSYNC_Field is HAL.UInt2;
   subtype TIMFCR_UPDGAT_Field is HAL.UInt4;

   --  Timerx Control Register
   type TIMFCR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMFCR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMFCR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMFCR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  TAU
      TAU            : Boolean := False;
      --  TBU
      TBU            : Boolean := False;
      --  TCU
      TCU            : Boolean := False;
      --  TDU
      TDU            : Boolean := False;
      --  TEU
      TEU            : Boolean := False;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMFCR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMFCR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMFCR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      TAU            at 0 range 19 .. 19;
      TBU            at 0 range 20 .. 20;
      TCU            at 0 range 21 .. 21;
      TDU            at 0 range 22 .. 22;
      TEU            at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  TIMFISR_CMP array
   type TIMFISR_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for TIMFISR_CMP
   type TIMFISR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : TIMFISR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for TIMFISR_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  TIMFISR_CPT array
   type TIMFISR_CPT_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMFISR_CPT
   type TIMFISR_CPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CPT as a value
            Val : HAL.UInt2;
         when True =>
            --  CPT as an array
            Arr : TIMFISR_CPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMFISR_CPT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Interrupt Status Register
   type TIMFISR_Register is record
      --  Read-only. Compare 1 Interrupt Flag
      CMP            : TIMFISR_CMP_Field;
      --  Read-only. Repetition Interrupt Flag
      REP            : Boolean;
      --  unspecified
      Reserved_5_5   : HAL.Bit;
      --  Read-only. Update Interrupt Flag
      UPD            : Boolean;
      --  Read-only. Capture1 Interrupt Flag
      CPT            : TIMFISR_CPT_Field;
      --  Read-only. Output 1 Set Interrupt Flag
      SETx1          : Boolean;
      --  Read-only. Output 1 Reset Interrupt Flag
      RSTx1          : Boolean;
      --  Read-only. Output 2 Set Interrupt Flag
      SETx2          : Boolean;
      --  Read-only. Output 2 Reset Interrupt Flag
      RSTx2          : Boolean;
      --  Read-only. Reset Interrupt Flag
      RST            : Boolean;
      --  Read-only. Delayed Protection Flag
      DLYPRT         : Boolean;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Current Push Pull Status
      CPPSTAT        : Boolean;
      --  Read-only. Idle Push Pull Status
      IPPSTAT        : Boolean;
      --  Read-only. Output 1 State
      O1STAT         : Boolean;
      --  Read-only. Output 2 State
      O2STAT         : Boolean;
      --  Read-only. Output 1 Copy
      O1CPY          : Boolean;
      --  Read-only. Output 2 Copy
      O2CPY          : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMFISR_Register use record
      CMP            at 0 range 0 .. 3;
      REP            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPD            at 0 range 6 .. 6;
      CPT            at 0 range 7 .. 8;
      SETx1          at 0 range 9 .. 9;
      RSTx1          at 0 range 10 .. 10;
      SETx2          at 0 range 11 .. 11;
      RSTx2          at 0 range 12 .. 12;
      RST            at 0 range 13 .. 13;
      DLYPRT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CPPSTAT        at 0 range 16 .. 16;
      IPPSTAT        at 0 range 17 .. 17;
      O1STAT         at 0 range 18 .. 18;
      O2STAT         at 0 range 19 .. 19;
      O1CPY          at 0 range 20 .. 20;
      O2CPY          at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Timerx Interrupt Clear Register
   type TIMFICR_Register is record
      --  Write-only. Compare 1 Interrupt flag Clear
      CMP1C          : Boolean := False;
      --  Write-only. Compare 2 Interrupt flag Clear
      CMP2C          : Boolean := False;
      --  Write-only. Compare 3 Interrupt flag Clear
      CMP3C          : Boolean := False;
      --  Write-only. Compare 4 Interrupt flag Clear
      CMP4C          : Boolean := False;
      --  Write-only. Repetition Interrupt flag Clear
      REPC           : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Write-only. Update Interrupt flag Clear
      UPDC           : Boolean := False;
      --  Write-only. Capture1 Interrupt flag Clear
      CPT1C          : Boolean := False;
      --  Write-only. Capture2 Interrupt flag Clear
      CPT2C          : Boolean := False;
      --  Write-only. Output 1 Set flag Clear
      SET1xC         : Boolean := False;
      --  Write-only. Output 1 Reset flag Clear
      RSTx1C         : Boolean := False;
      --  Write-only. Output 2 Set flag Clear
      SET2xC         : Boolean := False;
      --  Write-only. Output 2 Reset flag Clear
      RSTx2C         : Boolean := False;
      --  Write-only. Reset Interrupt flag Clear
      RSTC           : Boolean := False;
      --  Write-only. Delayed Protection Flag Clear
      DLYPRTC        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMFICR_Register use record
      CMP1C          at 0 range 0 .. 0;
      CMP2C          at 0 range 1 .. 1;
      CMP3C          at 0 range 2 .. 2;
      CMP4C          at 0 range 3 .. 3;
      REPC           at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDC           at 0 range 6 .. 6;
      CPT1C          at 0 range 7 .. 7;
      CPT2C          at 0 range 8 .. 8;
      SET1xC         at 0 range 9 .. 9;
      RSTx1C         at 0 range 10 .. 10;
      SET2xC         at 0 range 11 .. 11;
      RSTx2C         at 0 range 12 .. 12;
      RSTC           at 0 range 13 .. 13;
      DLYPRTC        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  TIMxDIER
   type TIMFDIER_Register is record
      --  CMP1IE
      CMP1IE         : Boolean := False;
      --  CMP2IE
      CMP2IE         : Boolean := False;
      --  CMP3IE
      CMP3IE         : Boolean := False;
      --  CMP4IE
      CMP4IE         : Boolean := False;
      --  REPIE
      REPIE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  UPDIE
      UPDIE          : Boolean := False;
      --  CPT1IE
      CPT1IE         : Boolean := False;
      --  CPT2IE
      CPT2IE         : Boolean := False;
      --  SET1xIE
      SET1xIE        : Boolean := False;
      --  RSTx1IE
      RSTx1IE        : Boolean := False;
      --  SETx2IE
      SETx2IE        : Boolean := False;
      --  RSTx2IE
      RSTx2IE        : Boolean := False;
      --  RSTIE
      RSTIE          : Boolean := False;
      --  DLYPRTIE
      DLYPRTIE       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  CMP1DE
      CMP1DE         : Boolean := False;
      --  CMP2DE
      CMP2DE         : Boolean := False;
      --  CMP3DE
      CMP3DE         : Boolean := False;
      --  CMP4DE
      CMP4DE         : Boolean := False;
      --  REPDE
      REPDE          : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  UPDDE
      UPDDE          : Boolean := False;
      --  CPT1DE
      CPT1DE         : Boolean := False;
      --  CPT2DE
      CPT2DE         : Boolean := False;
      --  SET1xDE
      SET1xDE        : Boolean := False;
      --  RSTx1DE
      RSTx1DE        : Boolean := False;
      --  SETx2DE
      SETx2DE        : Boolean := False;
      --  RSTx2DE
      RSTx2DE        : Boolean := False;
      --  RSTDE
      RSTDE          : Boolean := False;
      --  DLYPRTDE
      DLYPRTDE       : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMFDIER_Register use record
      CMP1IE         at 0 range 0 .. 0;
      CMP2IE         at 0 range 1 .. 1;
      CMP3IE         at 0 range 2 .. 2;
      CMP4IE         at 0 range 3 .. 3;
      REPIE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      UPDIE          at 0 range 6 .. 6;
      CPT1IE         at 0 range 7 .. 7;
      CPT2IE         at 0 range 8 .. 8;
      SET1xIE        at 0 range 9 .. 9;
      RSTx1IE        at 0 range 10 .. 10;
      SETx2IE        at 0 range 11 .. 11;
      RSTx2IE        at 0 range 12 .. 12;
      RSTIE          at 0 range 13 .. 13;
      DLYPRTIE       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CMP1DE         at 0 range 16 .. 16;
      CMP2DE         at 0 range 17 .. 17;
      CMP3DE         at 0 range 18 .. 18;
      CMP4DE         at 0 range 19 .. 19;
      REPDE          at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      UPDDE          at 0 range 22 .. 22;
      CPT1DE         at 0 range 23 .. 23;
      CPT2DE         at 0 range 24 .. 24;
      SET1xDE        at 0 range 25 .. 25;
      RSTx1DE        at 0 range 26 .. 26;
      SETx2DE        at 0 range 27 .. 27;
      RSTx2DE        at 0 range 28 .. 28;
      RSTDE          at 0 range 29 .. 29;
      DLYPRTDE       at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CNTFR_CNTx_Field is HAL.UInt16;

   --  Timerx Counter Register
   type CNTFR_Register is record
      --  Timerx Counter value
      CNTx           : CNTFR_CNTx_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTFR_Register use record
      CNTx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PERFR_PERx_Field is HAL.UInt16;

   --  Timerx Period Register
   type PERFR_Register is record
      --  Timerx Period value
      PERx           : PERFR_PERx_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERFR_Register use record
      PERx           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype REPFR_REPx_Field is HAL.UInt8;

   --  Timerx Repetition Register
   type REPFR_Register is record
      --  Timerx Repetition counter value
      REPx          : REPFR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPFR_Register use record
      REPx          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CMP1FR_CMP1x_Field is HAL.UInt16;

   --  Timerx Compare 1 Register
   type CMP1FR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1FR_CMP1x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1FR_Register use record
      CMP1x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP1CFR_CMP1x_Field is HAL.UInt16;
   subtype CMP1CFR_REPx_Field is HAL.UInt8;

   --  Timerx Compare 1 Compound Register
   type CMP1CFR_Register is record
      --  Timerx Compare 1 value
      CMP1x          : CMP1CFR_CMP1x_Field := 16#0#;
      --  Timerx Repetition value (aliased from HRTIM_REPx register)
      REPx           : CMP1CFR_REPx_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP1CFR_Register use record
      CMP1x          at 0 range 0 .. 15;
      REPx           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CMP2FR_CMP2x_Field is HAL.UInt16;

   --  Timerx Compare 2 Register
   type CMP2FR_Register is record
      --  Timerx Compare 2 value
      CMP2x          : CMP2FR_CMP2x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP2FR_Register use record
      CMP2x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP3FR_CMP3x_Field is HAL.UInt16;

   --  Timerx Compare 3 Register
   type CMP3FR_Register is record
      --  Timerx Compare 3 value
      CMP3x          : CMP3FR_CMP3x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP3FR_Register use record
      CMP3x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CMP4FR_CMP4x_Field is HAL.UInt16;

   --  Timerx Compare 4 Register
   type CMP4FR_Register is record
      --  Timerx Compare 4 value
      CMP4x          : CMP4FR_CMP4x_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP4FR_Register use record
      CMP4x          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CPT1FR_CPT1x_Field is HAL.UInt16;

   --  Timerx Capture 1 Register
   type CPT1FR_Register is record
      --  Read-only. Timerx Capture 1 value
      CPT1x          : CPT1FR_CPT1x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1FR_Register use record
      CPT1x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CPT2FR_CPT2x_Field is HAL.UInt16;

   --  Timerx Capture 2 Register
   type CPT2FR_Register is record
      --  Read-only. Timerx Capture 2 value
      CPT2x          : CPT2FR_CPT2x_Field;
      --  Read-only. Timerx Capture 1 Direction status
      DIR            : Boolean;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2FR_Register use record
      CPT2x          at 0 range 0 .. 15;
      DIR            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DTFR_DTRx_Field is HAL.UInt9;
   subtype DTFR_DTPRSC_Field is HAL.UInt3;
   subtype DTFR_DTFx_Field is HAL.UInt9;

   --  Timerx Deadtime Register
   type DTFR_Register is record
      --  Deadtime Rising value
      DTRx           : DTFR_DTRx_Field := 16#0#;
      --  Sign Deadtime Rising value
      SDTRx          : Boolean := False;
      --  Deadtime Prescaler
      DTPRSC         : DTFR_DTPRSC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Deadtime Rising Sign Lock
      DTRSLKx        : Boolean := False;
      --  Deadtime Rising Lock
      DTRLKx         : Boolean := False;
      --  Deadtime Falling value
      DTFx           : DTFR_DTFx_Field := 16#0#;
      --  Sign Deadtime Falling value
      SDTFx          : Boolean := False;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  Deadtime Falling Sign Lock
      DTFSLKx        : Boolean := False;
      --  Deadtime Falling Lock
      DTFLKx         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTFR_Register use record
      DTRx           at 0 range 0 .. 8;
      SDTRx          at 0 range 9 .. 9;
      DTPRSC         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      DTRSLKx        at 0 range 14 .. 14;
      DTRLKx         at 0 range 15 .. 15;
      DTFx           at 0 range 16 .. 24;
      SDTFx          at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DTFSLKx        at 0 range 30 .. 30;
      DTFLKx         at 0 range 31 .. 31;
   end record;

   --  SETF1R_CMP array
   type SETF1R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETF1R_CMP
   type SETF1R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETF1R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETF1R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETF1R_MSTCMP array
   type SETF1R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETF1R_MSTCMP
   type SETF1R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETF1R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETF1R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETF1R_TIMEVNT array
   type SETF1R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETF1R_TIMEVNT
   type SETF1R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETF1R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETF1R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETF1R_EXTEVNT array
   type SETF1R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETF1R_EXTEVNT
   type SETF1R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETF1R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETF1R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output1 Set Register
   type SETF1R_Register is record
      --  Software Set trigger
      SST     : Boolean := False;
      --  Timer A resynchronizaton
      RESYNC  : Boolean := False;
      --  Timer A Period
      PER     : Boolean := False;
      --  Timer A compare 1
      CMP     : SETF1R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master Period
      MSTPER  : Boolean := False;
      --  Master Compare 1
      MSTCMP  : SETF1R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer Event 1
      TIMEVNT : SETF1R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT : SETF1R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Registers update (transfer preload to active)
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETF1R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  SETF2R_CMP array
   type SETF2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETF2R_CMP
   type SETF2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : SETF2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETF2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETF2R_MSTCMP array
   type SETF2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SETF2R_MSTCMP
   type SETF2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : SETF2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SETF2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SETF2R_TIMEVNT array
   type SETF2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for SETF2R_TIMEVNT
   type SETF2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : SETF2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for SETF2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  SETF2R_EXTEVNT array
   type SETF2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for SETF2R_EXTEVNT
   type SETF2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : SETF2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for SETF2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Set Register
   type SETF2R_Register is record
      --  SST
      SST     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : SETF2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : SETF2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : SETF2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : SETF2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SETF2R_Register use record
      SST     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   --  RSTF2R_CMP array
   type RSTF2R_CMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTF2R_CMP
   type RSTF2R_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt4;
         when True =>
            --  CMP as an array
            Arr : RSTF2R_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTF2R_CMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTF2R_MSTCMP array
   type RSTF2R_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTF2R_MSTCMP
   type RSTF2R_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTF2R_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTF2R_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTF2R_TIMEVNT array
   type RSTF2R_TIMEVNT_Field_Array is array (1 .. 9) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for RSTF2R_TIMEVNT
   type RSTF2R_TIMEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMEVNT as a value
            Val : HAL.UInt9;
         when True =>
            --  TIMEVNT as an array
            Arr : RSTF2R_TIMEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for RSTF2R_TIMEVNT_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  RSTF2R_EXTEVNT array
   type RSTF2R_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTF2R_EXTEVNT
   type RSTF2R_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTF2R_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTF2R_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  Timerx Output2 Reset Register
   type RSTF2R_Register is record
      --  SRT
      SRT     : Boolean := False;
      --  RESYNC
      RESYNC  : Boolean := False;
      --  PER
      PER     : Boolean := False;
      --  CMP1
      CMP     : RSTF2R_CMP_Field := (As_Array => False, Val => 16#0#);
      --  MSTPER
      MSTPER  : Boolean := False;
      --  MSTCMP1
      MSTCMP  : RSTF2R_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  TIMEVNT1
      TIMEVNT : RSTF2R_TIMEVNT_Field := (As_Array => False, Val => 16#0#);
      --  EXTEVNT1
      EXTEVNT : RSTF2R_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  UPDATE
      UPDATE  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTF2R_Register use record
      SRT     at 0 range 0 .. 0;
      RESYNC  at 0 range 1 .. 1;
      PER     at 0 range 2 .. 2;
      CMP     at 0 range 3 .. 6;
      MSTPER  at 0 range 7 .. 7;
      MSTCMP  at 0 range 8 .. 11;
      TIMEVNT at 0 range 12 .. 20;
      EXTEVNT at 0 range 21 .. 30;
      UPDATE  at 0 range 31 .. 31;
   end record;

   subtype EEFFR1_EE1FLTR_Field is HAL.UInt4;
   subtype EEFFR1_EE2FLTR_Field is HAL.UInt4;
   subtype EEFFR1_EE3FLTR_Field is HAL.UInt4;
   subtype EEFFR1_EE4FLTR_Field is HAL.UInt4;
   subtype EEFFR1_EE5FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 1
   type EEFFR1_Register is record
      --  External Event 1 latch
      EE1LTCH        : Boolean := False;
      --  External Event 1 filter
      EE1FLTR        : EEFFR1_EE1FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 2 latch
      EE2LTCH        : Boolean := False;
      --  External Event 2 filter
      EE2FLTR        : EEFFR1_EE2FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 3 latch
      EE3LTCH        : Boolean := False;
      --  External Event 3 filter
      EE3FLTR        : EEFFR1_EE3FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 4 latch
      EE4LTCH        : Boolean := False;
      --  External Event 4 filter
      EE4FLTR        : EEFFR1_EE4FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 5 latch
      EE5LTCH        : Boolean := False;
      --  External Event 5 filter
      EE5FLTR        : EEFFR1_EE5FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFFR1_Register use record
      EE1LTCH        at 0 range 0 .. 0;
      EE1FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE2LTCH        at 0 range 6 .. 6;
      EE2FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE3LTCH        at 0 range 12 .. 12;
      EE3FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE4LTCH        at 0 range 18 .. 18;
      EE4FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE5LTCH        at 0 range 24 .. 24;
      EE5FLTR        at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype EEFFR2_EE6FLTR_Field is HAL.UInt4;
   subtype EEFFR2_EE7FLTR_Field is HAL.UInt4;
   subtype EEFFR2_EE8FLTR_Field is HAL.UInt4;
   subtype EEFFR2_EE9FLTR_Field is HAL.UInt4;
   subtype EEFFR2_EE10FLTR_Field is HAL.UInt4;

   --  Timerx External Event Filtering Register 2
   type EEFFR2_Register is record
      --  External Event 6 latch
      EE6LTCH        : Boolean := False;
      --  External Event 6 filter
      EE6FLTR        : EEFFR2_EE6FLTR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  External Event 7 latch
      EE7LTCH        : Boolean := False;
      --  External Event 7 filter
      EE7FLTR        : EEFFR2_EE7FLTR_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  External Event 8 latch
      EE8LTCH        : Boolean := False;
      --  External Event 8 filter
      EE8FLTR        : EEFFR2_EE8FLTR_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  External Event 9 latch
      EE9LTCH        : Boolean := False;
      --  External Event 9 filter
      EE9FLTR        : EEFFR2_EE9FLTR_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  External Event 10 latch
      EE10LTCH       : Boolean := False;
      --  External Event 10 filter
      EE10FLTR       : EEFFR2_EE10FLTR_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFFR2_Register use record
      EE6LTCH        at 0 range 0 .. 0;
      EE6FLTR        at 0 range 1 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      EE7LTCH        at 0 range 6 .. 6;
      EE7FLTR        at 0 range 7 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      EE8LTCH        at 0 range 12 .. 12;
      EE8FLTR        at 0 range 13 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      EE9LTCH        at 0 range 18 .. 18;
      EE9FLTR        at 0 range 19 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EE10LTCH       at 0 range 24 .. 24;
      EE10FLTR       at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  RSTFR_CMP array
   type RSTFR_CMP_Field_Array is array (2 .. 3) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for RSTFR_CMP
   type RSTFR_CMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMP as a value
            Val : HAL.UInt2;
         when True =>
            --  CMP as an array
            Arr : RSTFR_CMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RSTFR_CMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  RSTFR_MSTCMP array
   type RSTFR_MSTCMP_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for RSTFR_MSTCMP
   type RSTFR_MSTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MSTCMP as a value
            Val : HAL.UInt4;
         when True =>
            --  MSTCMP as an array
            Arr : RSTFR_MSTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for RSTFR_MSTCMP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  RSTFR_EXTEVNT array
   type RSTFR_EXTEVNT_Field_Array is array (1 .. 10) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for RSTFR_EXTEVNT
   type RSTFR_EXTEVNT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTEVNT as a value
            Val : HAL.UInt10;
         when True =>
            --  EXTEVNT as an array
            Arr : RSTFR_EXTEVNT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for RSTFR_EXTEVNT_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   --  RSTFR_TIMACMP array
   type RSTFR_TIMACMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTFR_TIMACMP
   type RSTFR_TIMACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMACMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMACMP as an array
            Arr : RSTFR_TIMACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTFR_TIMACMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTFR_TIMBCMP array
   type RSTFR_TIMBCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTFR_TIMBCMP
   type RSTFR_TIMBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMBCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMBCMP as an array
            Arr : RSTFR_TIMBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTFR_TIMBCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTFR_TIMCCMP array
   type RSTFR_TIMCCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTFR_TIMCCMP
   type RSTFR_TIMCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMCCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMCCMP as an array
            Arr : RSTFR_TIMCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTFR_TIMCCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RSTFR_TIMDCMP array
   type RSTFR_TIMDCMP_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RSTFR_TIMDCMP
   type RSTFR_TIMDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMDCMP as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMDCMP as an array
            Arr : RSTFR_TIMDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RSTFR_TIMDCMP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  TimerA Reset Register
   type RSTFR_Register is record
      --  Timer A Update reset
      TIMFCMP1 : Boolean := False;
      --  Timer A Update reset
      UPDT     : Boolean := False;
      --  Timer A compare 2 reset
      CMP      : RSTFR_CMP_Field := (As_Array => False, Val => 16#0#);
      --  Master timer Period
      MSTPER   : Boolean := False;
      --  Master compare 1
      MSTCMP   : RSTFR_MSTCMP_Field := (As_Array => False, Val => 16#0#);
      --  External Event 1
      EXTEVNT  : RSTFR_EXTEVNT_Field := (As_Array => False, Val => 16#0#);
      --  Timer A Compare 1
      TIMACMP  : RSTFR_TIMACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B Compare 1
      TIMBCMP  : RSTFR_TIMBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C Compare 1
      TIMCCMP  : RSTFR_TIMCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D Compare 1
      TIMDCMP  : RSTFR_TIMDCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer F Compare 2
      TIMFCPM2 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTFR_Register use record
      TIMFCMP1 at 0 range 0 .. 0;
      UPDT     at 0 range 1 .. 1;
      CMP      at 0 range 2 .. 3;
      MSTPER   at 0 range 4 .. 4;
      MSTCMP   at 0 range 5 .. 8;
      EXTEVNT  at 0 range 9 .. 18;
      TIMACMP  at 0 range 19 .. 21;
      TIMBCMP  at 0 range 22 .. 24;
      TIMCCMP  at 0 range 25 .. 27;
      TIMDCMP  at 0 range 28 .. 30;
      TIMFCPM2 at 0 range 31 .. 31;
   end record;

   subtype CHPFR_CHPFRQ_Field is HAL.UInt4;
   subtype CHPFR_CHPDTY_Field is HAL.UInt3;
   subtype CHPFR_STRTPW_Field is HAL.UInt4;

   --  Timerx Chopper Register
   type CHPFR_Register is record
      --  Timerx carrier frequency value
      CHPFRQ         : CHPFR_CHPFRQ_Field := 16#0#;
      --  Timerx chopper duty cycle value
      CHPDTY         : CHPFR_CHPDTY_Field := 16#0#;
      --  STRTPW
      STRTPW         : CHPFR_STRTPW_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHPFR_Register use record
      CHPFRQ         at 0 range 0 .. 3;
      CHPDTY         at 0 range 4 .. 6;
      STRTPW         at 0 range 7 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CPT1FCR_TACMP array
   type CPT1FCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1FCR_TACMP
   type CPT1FCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT1FCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1FCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1FCR_TBCMP array
   type CPT1FCR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1FCR_TBCMP
   type CPT1FCR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT1FCR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1FCR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1FCR_TCCMP array
   type CPT1FCR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1FCR_TCCMP
   type CPT1FCR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT1FCR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1FCR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1FCR_TDCMP array
   type CPT1FCR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1FCR_TDCMP
   type CPT1FCR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT1FCR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1FCR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT1FCR_TECMP array
   type CPT1FCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT1FCR_TECMP
   type CPT1FCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT1FCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT1FCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 1 Control Register
   type CPT1FCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT1FCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT1FCR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT1FCR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT1FCR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  TE1SET
      TE1SET    : Boolean := False;
      --  TE1RST
      TE1RST    : Boolean := False;
      --  TECMP1
      TECMP     : CPT1FCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT1FCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   --  CPT2FCR_TACMP array
   type CPT2FCR_TACMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2FCR_TACMP
   type CPT2FCR_TACMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TACMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TACMP as an array
            Arr : CPT2FCR_TACMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2FCR_TACMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2FCR_TBCMP array
   type CPT2FCR_TBCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2FCR_TBCMP
   type CPT2FCR_TBCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TBCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TBCMP as an array
            Arr : CPT2FCR_TBCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2FCR_TBCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2FCR_TCCMP array
   type CPT2FCR_TCCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2FCR_TCCMP
   type CPT2FCR_TCCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TCCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TCCMP as an array
            Arr : CPT2FCR_TCCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2FCR_TCCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2FCR_TDCMP array
   type CPT2FCR_TDCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2FCR_TDCMP
   type CPT2FCR_TDCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TDCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TDCMP as an array
            Arr : CPT2FCR_TDCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2FCR_TDCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  CPT2FCR_TECMP array
   type CPT2FCR_TECMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for CPT2FCR_TECMP
   type CPT2FCR_TECMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TECMP as a value
            Val : HAL.UInt2;
         when True =>
            --  TECMP as an array
            Arr : CPT2FCR_TECMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CPT2FCR_TECMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Timerx Capture 2 Control Register
   type CPT2FCR_Register is record
      --  Software Capture
      SWCPT     : Boolean := False;
      --  Update Capture
      UDPCPT    : Boolean := False;
      --  External Event 1 Capture
      EXEV1CPT  : Boolean := False;
      --  External Event 2 Capture
      EXEV2CPT  : Boolean := False;
      --  External Event 3 Capture
      EXEV3CPT  : Boolean := False;
      --  External Event 4 Capture
      EXEV4CPT  : Boolean := False;
      --  External Event 5 Capture
      EXEV5CPT  : Boolean := False;
      --  External Event 6 Capture
      EXEV6CPT  : Boolean := False;
      --  External Event 7 Capture
      EXEV7CPT  : Boolean := False;
      --  External Event 8 Capture
      EXEV8CPT  : Boolean := False;
      --  External Event 9 Capture
      EXEV9CPT  : Boolean := False;
      --  External Event 10 Capture
      EXEV10CPT : Boolean := False;
      --  Timer A output 1 Set
      TA1SET    : Boolean := False;
      --  Timer A output 1 Reset
      TA1RST    : Boolean := False;
      --  Timer A Compare 1
      TACMP     : CPT2FCR_TACMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer B output 1 Set
      TB1SET    : Boolean := False;
      --  Timer B output 1 Reset
      TB1RST    : Boolean := False;
      --  Timer B Compare 1
      TBCMP     : CPT2FCR_TBCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer C output 1 Set
      TC1SET    : Boolean := False;
      --  Timer C output 1 Reset
      TC1RST    : Boolean := False;
      --  Timer C Compare 1
      TCCMP     : CPT2FCR_TCCMP_Field := (As_Array => False, Val => 16#0#);
      --  Timer D output 1 Set
      TD1SET    : Boolean := False;
      --  Timer D output 1 Reset
      TD1RST    : Boolean := False;
      --  Timer D Compare 1
      TDCMP     : CPT2FCR_TDCMP_Field := (As_Array => False, Val => 16#0#);
      --  TE1SET
      TE1SET    : Boolean := False;
      --  TE1RST
      TE1RST    : Boolean := False;
      --  TECMP1
      TECMP     : CPT2FCR_TECMP_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPT2FCR_Register use record
      SWCPT     at 0 range 0 .. 0;
      UDPCPT    at 0 range 1 .. 1;
      EXEV1CPT  at 0 range 2 .. 2;
      EXEV2CPT  at 0 range 3 .. 3;
      EXEV3CPT  at 0 range 4 .. 4;
      EXEV4CPT  at 0 range 5 .. 5;
      EXEV5CPT  at 0 range 6 .. 6;
      EXEV6CPT  at 0 range 7 .. 7;
      EXEV7CPT  at 0 range 8 .. 8;
      EXEV8CPT  at 0 range 9 .. 9;
      EXEV9CPT  at 0 range 10 .. 10;
      EXEV10CPT at 0 range 11 .. 11;
      TA1SET    at 0 range 12 .. 12;
      TA1RST    at 0 range 13 .. 13;
      TACMP     at 0 range 14 .. 15;
      TB1SET    at 0 range 16 .. 16;
      TB1RST    at 0 range 17 .. 17;
      TBCMP     at 0 range 18 .. 19;
      TC1SET    at 0 range 20 .. 20;
      TC1RST    at 0 range 21 .. 21;
      TCCMP     at 0 range 22 .. 23;
      TD1SET    at 0 range 24 .. 24;
      TD1RST    at 0 range 25 .. 25;
      TDCMP     at 0 range 26 .. 27;
      TE1SET    at 0 range 28 .. 28;
      TE1RST    at 0 range 29 .. 29;
      TECMP     at 0 range 30 .. 31;
   end record;

   subtype OUTFR_FAULT1_Field is HAL.UInt2;
   subtype OUTFR_DLYPRT_Field is HAL.UInt3;
   subtype OUTFR_FAULT2_Field is HAL.UInt2;

   --  Timerx Output Register
   type OUTFR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Output 1 polarity
      POL1           : Boolean := False;
      --  Output 1 Idle mode
      IDLEM1         : Boolean := False;
      --  Output 1 Idle State
      IDLES1         : Boolean := False;
      --  Output 1 Fault state
      FAULT1         : OUTFR_FAULT1_Field := 16#0#;
      --  Output 1 Chopper enable
      CHP1           : Boolean := False;
      --  Output 1 Deadtime upon burst mode Idle entry
      DIDL1          : Boolean := False;
      --  Deadtime enable
      DTEN           : Boolean := False;
      --  Delayed Protection Enable
      DLYPRTEN       : Boolean := False;
      --  Delayed Protection
      DLYPRT         : OUTFR_DLYPRT_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Balanced Idle Automatic Resume
      BIAR           : Boolean := False;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  Output 2 polarity
      POL2           : Boolean := False;
      --  Output 2 Idle mode
      IDLEM2         : Boolean := False;
      --  Output 2 Idle State
      IDLES2         : Boolean := False;
      --  Output 2 Fault state
      FAULT2         : OUTFR_FAULT2_Field := 16#0#;
      --  Output 2 Chopper enable
      CHP2           : Boolean := False;
      --  Output 2 Deadtime upon burst mode Idle entry
      DIDL2          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUTFR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      POL1           at 0 range 1 .. 1;
      IDLEM1         at 0 range 2 .. 2;
      IDLES1         at 0 range 3 .. 3;
      FAULT1         at 0 range 4 .. 5;
      CHP1           at 0 range 6 .. 6;
      DIDL1          at 0 range 7 .. 7;
      DTEN           at 0 range 8 .. 8;
      DLYPRTEN       at 0 range 9 .. 9;
      DLYPRT         at 0 range 10 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      BIAR           at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      POL2           at 0 range 17 .. 17;
      IDLEM2         at 0 range 18 .. 18;
      IDLES2         at 0 range 19 .. 19;
      FAULT2         at 0 range 20 .. 21;
      CHP2           at 0 range 22 .. 22;
      DIDL2          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Timerx Fault Register
   type FLTFR_Register is record
      --  Fault 1 enable
      FLT1EN        : Boolean := False;
      --  Fault 2 enable
      FLT2EN        : Boolean := False;
      --  Fault 3 enable
      FLT3EN        : Boolean := False;
      --  Fault 4 enable
      FLT4EN        : Boolean := False;
      --  Fault 5 enable
      FLT5EN        : Boolean := False;
      --  Fault 6 enable
      FLT6EN        : Boolean := False;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#0#;
      --  Fault sources Lock
      FLTLCK        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTFR_Register use record
      FLT1EN        at 0 range 0 .. 0;
      FLT2EN        at 0 range 1 .. 1;
      FLT3EN        at 0 range 2 .. 2;
      FLT4EN        at 0 range 3 .. 3;
      FLT5EN        at 0 range 4 .. 4;
      FLT6EN        at 0 range 5 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      FLTLCK        at 0 range 31 .. 31;
   end record;

   subtype TIMFCR2_ROM_Field is HAL.UInt2;
   subtype TIMFCR2_OUTROM_Field is HAL.UInt2;
   subtype TIMFCR2_ADROM_Field is HAL.UInt2;
   subtype TIMFCR2_BMROM_Field is HAL.UInt2;
   subtype TIMFCR2_FEROM_Field is HAL.UInt2;

   --  TIMFCR2_GTCMP array
   type TIMFCR2_GTCMP_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TIMFCR2_GTCMP
   type TIMFCR2_GTCMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GTCMP as a value
            Val : HAL.UInt2;
         when True =>
            --  GTCMP as an array
            Arr : TIMFCR2_GTCMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TIMFCR2_GTCMP_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  HRTIM Timerx Control Register 2
   type TIMFCR2_Register is record
      --  Dual Channel DAC trigger enable
      DCDE           : Boolean := False;
      --  Dual Channel DAC Step trigger
      DCDS           : Boolean := False;
      --  Dual Channel DAC Reset trigger
      DCDR           : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Up-Down Mode
      UDM            : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Roll-Over Mode
      ROM            : TIMFCR2_ROM_Field := 16#0#;
      --  Output Roll-Over Mode
      OUTROM         : TIMFCR2_OUTROM_Field := 16#0#;
      --  ADC Roll-Over Mode
      ADROM          : TIMFCR2_ADROM_Field := 16#0#;
      --  Burst Mode Roll-Over Mode
      BMROM          : TIMFCR2_BMROM_Field := 16#0#;
      --  Fault and Event Roll-Over Mode
      FEROM          : TIMFCR2_FEROM_Field := 16#0#;
      --  Greater than Compare 1 PWM mode
      GTCMP          : TIMFCR2_GTCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Triggered-half mode
      TRGHLF         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMFCR2_Register use record
      DCDE           at 0 range 0 .. 0;
      DCDS           at 0 range 1 .. 1;
      DCDR           at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      UDM            at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ROM            at 0 range 6 .. 7;
      OUTROM         at 0 range 8 .. 9;
      ADROM          at 0 range 10 .. 11;
      BMROM          at 0 range 12 .. 13;
      FEROM          at 0 range 14 .. 15;
      GTCMP          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TRGHLF         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype EEFFR3_EEVASEL_Field is HAL.UInt4;
   subtype EEFFR3_EEVACNT_Field is HAL.UInt6;

   --  HRTIM Timerx External Event Filtering Register 3
   type EEFFR3_Register is record
      --  External Event A Counter Enable
      EEVACE         : Boolean := False;
      --  External Event A Counter Reset
      EEVACRES       : Boolean := False;
      --  External Event A Reset Mode
      EEVARSTM       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  External Event A Selection
      EEVASEL        : EEFFR3_EEVASEL_Field := 16#0#;
      --  External Event A counter
      EEVACNT        : EEFFR3_EEVACNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EEFFR3_Register use record
      EEVACE         at 0 range 0 .. 0;
      EEVACRES       at 0 range 1 .. 1;
      EEVARSTM       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      EEVASEL        at 0 range 4 .. 7;
      EEVACNT        at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  High Resolution Timer: Common functions
   type HRTIM_Common_Peripheral is record
      --  Control Register 1
      CR1     : aliased CR1_Register;
      --  Control Register 2
      CR2     : aliased CR2_Register;
      --  Interrupt Status Register
      ISR     : aliased ISR_Register;
      --  Interrupt Clear Register
      ICR     : aliased ICR_Register;
      --  Interrupt Enable Register
      IER     : aliased IER_Register;
      --  Output Enable Register
      OENR    : aliased OENR_Register;
      --  ODISR
      ODISR   : aliased ODISR_Register;
      --  Output Disable Status Register
      ODSR    : aliased ODSR_Register;
      --  Burst Mode Control Register
      BMCR    : aliased BMCR_Register;
      --  BMTRG
      BMTRG   : aliased BMTRG_Register;
      --  BMCMPR
      BMCMPR  : aliased BMCMPR_Register;
      --  Burst Mode Period Register
      BMPER   : aliased BMPER_Register;
      --  Timer External Event Control Register 1
      EECR1   : aliased EECR1_Register;
      --  Timer External Event Control Register 2
      EECR2   : aliased EECR2_Register;
      --  Timer External Event Control Register 3
      EECR3   : aliased EECR3_Register;
      --  ADC Trigger 1 Register
      ADC1R   : aliased ADC1R_Register;
      --  ADC Trigger 2 Register
      ADC2R   : aliased ADC2R_Register;
      --  ADC Trigger 3 Register
      ADC3R   : aliased ADC3R_Register;
      --  ADC Trigger 4 Register
      ADC4R   : aliased ADC4R_Register;
      --  DLL Control Register
      DLLCR   : aliased DLLCR_Register;
      --  HRTIM Fault Input Register 1
      FLTINR1 : aliased FLTINR1_Register;
      --  HRTIM Fault Input Register 2
      FLTINR2 : aliased FLTINR2_Register;
      --  BDMUPDR
      BDMUPDR : aliased BDMUPDR_Register;
      --  Burst DMA Timerx update Register
      BDTAUPR : aliased BDTAUPR_Register;
      --  Burst DMA Timerx update Register
      BDTBUPR : aliased BDTBUPR_Register;
      --  Burst DMA Timerx update Register
      BDTCUPR : aliased BDTCUPR_Register;
      --  Burst DMA Timerx update Register
      BDTDUPR : aliased BDTDUPR_Register;
      --  Burst DMA Timerx update Register
      BDTEUPR : aliased BDTEUPR_Register;
      --  Burst DMA Data Register
      BDMADR  : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTFUPR : aliased BDTFUPR_Register;
      --  HRTIM ADC Extended Trigger Register
      ADCER   : aliased ADCER_Register;
      --  HRTIM ADC Trigger Update Register
      ADCUR   : aliased ADCUR_Register;
      --  HRTIM ADC Post Scaler Register 1
      ADCPS1  : aliased ADCPS1_Register;
      --  HRTIM ADC Post Scaler Register 2
      ADCPS2  : aliased ADCPS2_Register;
      --  HRTIM Fault Input Register 3
      FLTINR3 : aliased FLTINR3_Register;
      --  HRTIM Fault Input Register 4
      FLTINR4 : aliased FLTINR4_Register;
   end record
     with Volatile;

   for HRTIM_Common_Peripheral use record
      CR1     at 16#0# range 0 .. 31;
      CR2     at 16#4# range 0 .. 31;
      ISR     at 16#8# range 0 .. 31;
      ICR     at 16#C# range 0 .. 31;
      IER     at 16#10# range 0 .. 31;
      OENR    at 16#14# range 0 .. 31;
      ODISR   at 16#18# range 0 .. 31;
      ODSR    at 16#1C# range 0 .. 31;
      BMCR    at 16#20# range 0 .. 31;
      BMTRG   at 16#24# range 0 .. 31;
      BMCMPR  at 16#28# range 0 .. 31;
      BMPER   at 16#2C# range 0 .. 31;
      EECR1   at 16#30# range 0 .. 31;
      EECR2   at 16#34# range 0 .. 31;
      EECR3   at 16#38# range 0 .. 31;
      ADC1R   at 16#3C# range 0 .. 31;
      ADC2R   at 16#40# range 0 .. 31;
      ADC3R   at 16#44# range 0 .. 31;
      ADC4R   at 16#48# range 0 .. 31;
      DLLCR   at 16#4C# range 0 .. 31;
      FLTINR1 at 16#50# range 0 .. 31;
      FLTINR2 at 16#54# range 0 .. 31;
      BDMUPDR at 16#58# range 0 .. 31;
      BDTAUPR at 16#5C# range 0 .. 31;
      BDTBUPR at 16#60# range 0 .. 31;
      BDTCUPR at 16#64# range 0 .. 31;
      BDTDUPR at 16#68# range 0 .. 31;
      BDTEUPR at 16#6C# range 0 .. 31;
      BDMADR  at 16#70# range 0 .. 31;
      BDTFUPR at 16#74# range 0 .. 31;
      ADCER   at 16#78# range 0 .. 31;
      ADCUR   at 16#7C# range 0 .. 31;
      ADCPS1  at 16#80# range 0 .. 31;
      ADCPS2  at 16#84# range 0 .. 31;
      FLTINR3 at 16#88# range 0 .. 31;
      FLTINR4 at 16#8C# range 0 .. 31;
   end record;

   --  High Resolution Timer: Common functions
   HRTIM_Common_Periph : aliased HRTIM_Common_Peripheral
     with Import, Address => HRTIM_Common_Base;

   --  High Resolution Timer: Master Timers
   type HRTIM_Master_Peripheral is record
      --  Master Timer Control Register
      MCR    : aliased MCR_Register;
      --  Master Timer Interrupt Status Register
      MISR   : aliased MISR_Register;
      --  Master Timer Interrupt Clear Register
      MICR   : aliased MICR_Register;
      --  HRTIM Master Timer DMA / Interrupt Enable Register
      MDIER  : aliased MDIER_Register;
      --  Master Timer Counter Register
      MCNTR  : aliased MCNTR_Register;
      --  Master Timer Period Register
      MPER   : aliased MPER_Register;
      --  Master Timer Repetition Register
      MREP   : aliased MREP_Register;
      --  Master Timer Compare 1 Register
      MCMP1R : aliased MCMP1R_Register;
      --  Master Timer Compare 2 Register
      MCMP2R : aliased MCMP2R_Register;
      --  Master Timer Compare 3 Register
      MCMP3R : aliased MCMP3R_Register;
      --  Master Timer Compare 4 Register
      MCMP4R : aliased MCMP4R_Register;
   end record
     with Volatile;

   for HRTIM_Master_Peripheral use record
      MCR    at 16#0# range 0 .. 31;
      MISR   at 16#4# range 0 .. 31;
      MICR   at 16#8# range 0 .. 31;
      MDIER  at 16#C# range 0 .. 31;
      MCNTR  at 16#10# range 0 .. 31;
      MPER   at 16#14# range 0 .. 31;
      MREP   at 16#18# range 0 .. 31;
      MCMP1R at 16#1C# range 0 .. 31;
      MCMP2R at 16#24# range 0 .. 31;
      MCMP3R at 16#28# range 0 .. 31;
      MCMP4R at 16#2C# range 0 .. 31;
   end record;

   --  High Resolution Timer: Master Timers
   HRTIM_Master_Periph : aliased HRTIM_Master_Peripheral
     with Import, Address => HRTIM_Master_Base;

   --  High Resolution Timer: TIMA
   type HRTIM_TIMA_Peripheral is record
      --  Timerx Control Register
      TIMACR   : aliased TIMACR_Register;
      --  Timerx Interrupt Status Register
      TIMAISR  : aliased TIMAISR_Register;
      --  Timerx Interrupt Clear Register
      TIMAICR  : aliased TIMAICR_Register;
      --  TIMxDIER
      TIMADIER : aliased TIMADIER_Register;
      --  Timerx Counter Register
      CNTAR    : aliased CNTAR_Register;
      --  Timerx Period Register
      PERAR    : aliased PERAR_Register;
      --  Timerx Repetition Register
      REPAR    : aliased REPAR_Register;
      --  Timerx Compare 1 Register
      CMP1AR   : aliased CMP1AR_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CAR  : aliased CMP1CAR_Register;
      --  Timerx Compare 2 Register
      CMP2AR   : aliased CMP2AR_Register;
      --  Timerx Compare 3 Register
      CMP3AR   : aliased CMP3AR_Register;
      --  Timerx Compare 4 Register
      CMP4AR   : aliased CMP4AR_Register;
      --  Timerx Capture 1 Register
      CPT1AR   : aliased CPT1AR_Register;
      --  Timerx Capture 2 Register
      CPT2AR   : aliased CPT2AR_Register;
      --  Timerx Deadtime Register
      DTAR     : aliased DTAR_Register;
      --  Timerx Output1 Set Register
      SETA1R   : aliased SETA1R_Register;
      --  Timerx Output1 Reset Register
      RSTA1R   : aliased RSTA1R_Register;
      --  Timerx Output2 Set Register
      SETA2R   : aliased SETA2R_Register;
      --  Timerx Output2 Reset Register
      RSTA2R   : aliased RSTA2R_Register;
      --  Timerx External Event Filtering Register 1
      EEFAR1   : aliased EEFAR1_Register;
      --  Timerx External Event Filtering Register 2
      EEFAR2   : aliased EEFAR2_Register;
      --  TimerA Reset Register
      RSTAR    : aliased RSTAR_Register;
      --  Timerx Chopper Register
      CHPAR    : aliased CHPAR_Register;
      --  Timerx Capture 1 Control Register
      CPT1ACR  : aliased CPT1ACR_Register;
      --  Timerx Capture 2 Control Register
      CPT2ACR  : aliased CPT2ACR_Register;
      --  Timerx Output Register
      OUTAR    : aliased OUTAR_Register;
      --  Timerx Fault Register
      FLTAR    : aliased FLTAR_Register;
      --  HRTIM Timerx Control Register 2
      TIMACR2  : aliased TIMACR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFAR3   : aliased EEFAR3_Register;
   end record
     with Volatile;

   for HRTIM_TIMA_Peripheral use record
      TIMACR   at 16#0# range 0 .. 31;
      TIMAISR  at 16#4# range 0 .. 31;
      TIMAICR  at 16#8# range 0 .. 31;
      TIMADIER at 16#C# range 0 .. 31;
      CNTAR    at 16#10# range 0 .. 31;
      PERAR    at 16#14# range 0 .. 31;
      REPAR    at 16#18# range 0 .. 31;
      CMP1AR   at 16#1C# range 0 .. 31;
      CMP1CAR  at 16#20# range 0 .. 31;
      CMP2AR   at 16#24# range 0 .. 31;
      CMP3AR   at 16#28# range 0 .. 31;
      CMP4AR   at 16#2C# range 0 .. 31;
      CPT1AR   at 16#30# range 0 .. 31;
      CPT2AR   at 16#34# range 0 .. 31;
      DTAR     at 16#38# range 0 .. 31;
      SETA1R   at 16#3C# range 0 .. 31;
      RSTA1R   at 16#40# range 0 .. 31;
      SETA2R   at 16#44# range 0 .. 31;
      RSTA2R   at 16#48# range 0 .. 31;
      EEFAR1   at 16#4C# range 0 .. 31;
      EEFAR2   at 16#50# range 0 .. 31;
      RSTAR    at 16#54# range 0 .. 31;
      CHPAR    at 16#58# range 0 .. 31;
      CPT1ACR  at 16#5C# range 0 .. 31;
      CPT2ACR  at 16#60# range 0 .. 31;
      OUTAR    at 16#64# range 0 .. 31;
      FLTAR    at 16#68# range 0 .. 31;
      TIMACR2  at 16#6C# range 0 .. 31;
      EEFAR3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: TIMA
   HRTIM_TIMA_Periph : aliased HRTIM_TIMA_Peripheral
     with Import, Address => HRTIM_TIMA_Base;

   --  High Resolution Timer: TIMB
   type HRTIM_TIMB_Peripheral is record
      --  Timerx Control Register
      TIMBCR   : aliased TIMBCR_Register;
      --  Timerx Interrupt Status Register
      TIMBISR  : aliased TIMBISR_Register;
      --  Timerx Interrupt Clear Register
      TIMBICR  : aliased TIMBICR_Register;
      --  TIMxDIER
      TIMBDIER : aliased TIMBDIER_Register;
      --  Timerx Counter Register
      CNTR     : aliased CNTR_Register;
      --  Timerx Period Register
      PERBR    : aliased PERBR_Register;
      --  Timerx Repetition Register
      REPBR    : aliased REPBR_Register;
      --  Timerx Compare 1 Register
      CMP1BR   : aliased CMP1BR_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CBR  : aliased CMP1CBR_Register;
      --  Timerx Compare 2 Register
      CMP2BR   : aliased CMP2BR_Register;
      --  Timerx Compare 3 Register
      CMP3BR   : aliased CMP3BR_Register;
      --  Timerx Compare 4 Register
      CMP4BR   : aliased CMP4BR_Register;
      --  Timerx Capture 1 Register
      CPT1BR   : aliased CPT1BR_Register;
      --  Timerx Capture 2 Register
      CPT2BR   : aliased CPT2BR_Register;
      --  Timerx Deadtime Register
      DTBR     : aliased DTBR_Register;
      --  Timerx Output1 Set Register
      SETB1R   : aliased SETB1R_Register;
      --  Timerx Output1 Reset Register
      RSTB1R   : aliased RSTB1R_Register;
      --  Timerx Output2 Set Register
      SETB2R   : aliased SETB2R_Register;
      --  Timerx Output2 Reset Register
      RSTB2R   : aliased RSTB2R_Register;
      --  Timerx External Event Filtering Register 1
      EEFBR1   : aliased EEFBR1_Register;
      --  Timerx External Event Filtering Register 2
      EEFBR2   : aliased EEFBR2_Register;
      --  TimerA Reset Register
      RSTBR    : aliased RSTBR_Register;
      --  Timerx Chopper Register
      CHPBR    : aliased CHPBR_Register;
      --  Timerx Capture 1 Control Register
      CPT1BCR  : aliased CPT1BCR_Register;
      --  Timerx Capture 2 Control Register
      CPT2BCR  : aliased CPT2BCR_Register;
      --  Timerx Output Register
      OUTBR    : aliased OUTBR_Register;
      --  Timerx Fault Register
      FLTBR    : aliased FLTBR_Register;
      --  HRTIM Timerx Control Register 2
      TIMBCR2  : aliased TIMBCR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFBR3   : aliased EEFBR3_Register;
   end record
     with Volatile;

   for HRTIM_TIMB_Peripheral use record
      TIMBCR   at 16#0# range 0 .. 31;
      TIMBISR  at 16#4# range 0 .. 31;
      TIMBICR  at 16#8# range 0 .. 31;
      TIMBDIER at 16#C# range 0 .. 31;
      CNTR     at 16#10# range 0 .. 31;
      PERBR    at 16#14# range 0 .. 31;
      REPBR    at 16#18# range 0 .. 31;
      CMP1BR   at 16#1C# range 0 .. 31;
      CMP1CBR  at 16#20# range 0 .. 31;
      CMP2BR   at 16#24# range 0 .. 31;
      CMP3BR   at 16#28# range 0 .. 31;
      CMP4BR   at 16#2C# range 0 .. 31;
      CPT1BR   at 16#30# range 0 .. 31;
      CPT2BR   at 16#34# range 0 .. 31;
      DTBR     at 16#38# range 0 .. 31;
      SETB1R   at 16#3C# range 0 .. 31;
      RSTB1R   at 16#40# range 0 .. 31;
      SETB2R   at 16#44# range 0 .. 31;
      RSTB2R   at 16#48# range 0 .. 31;
      EEFBR1   at 16#4C# range 0 .. 31;
      EEFBR2   at 16#50# range 0 .. 31;
      RSTBR    at 16#54# range 0 .. 31;
      CHPBR    at 16#58# range 0 .. 31;
      CPT1BCR  at 16#5C# range 0 .. 31;
      CPT2BCR  at 16#60# range 0 .. 31;
      OUTBR    at 16#64# range 0 .. 31;
      FLTBR    at 16#68# range 0 .. 31;
      TIMBCR2  at 16#6C# range 0 .. 31;
      EEFBR3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: TIMB
   HRTIM_TIMB_Periph : aliased HRTIM_TIMB_Peripheral
     with Import, Address => HRTIM_TIMB_Base;

   --  High Resolution Timer: TIMC
   type HRTIM_TIMC_Peripheral is record
      --  Timerx Control Register
      TIMCCR   : aliased TIMCCR_Register;
      --  Timerx Interrupt Status Register
      TIMCISR  : aliased TIMCISR_Register;
      --  Timerx Interrupt Clear Register
      TIMCICR  : aliased TIMCICR_Register;
      --  TIMxDIER
      TIMCDIER : aliased TIMCDIER_Register;
      --  Timerx Counter Register
      CNTCR    : aliased CNTCR_Register;
      --  Timerx Period Register
      PERCR    : aliased PERCR_Register;
      --  Timerx Repetition Register
      REPCR    : aliased REPCR_Register;
      --  Timerx Compare 1 Register
      CMP1CR   : aliased CMP1CR_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CCR  : aliased CMP1CCR_Register;
      --  Timerx Compare 2 Register
      CMP2CR   : aliased CMP2CR_Register;
      --  Timerx Compare 3 Register
      CMP3CR   : aliased CMP3CR_Register;
      --  Timerx Compare 4 Register
      CMP4CR   : aliased CMP4CR_Register;
      --  Timerx Capture 1 Register
      CPT1CR   : aliased CPT1CR_Register;
      --  Timerx Capture 2 Register
      CPT2CR   : aliased CPT2CR_Register;
      --  Timerx Deadtime Register
      DTCR     : aliased DTCR_Register;
      --  Timerx Output1 Set Register
      SETC1R   : aliased SETC1R_Register;
      --  Timerx Output1 Reset Register
      RSTC1R   : aliased RSTC1R_Register;
      --  Timerx Output2 Set Register
      SETC2R   : aliased SETC2R_Register;
      --  Timerx Output2 Reset Register
      RSTC2R   : aliased RSTC2R_Register;
      --  Timerx External Event Filtering Register 1
      EEFCR1   : aliased EEFCR1_Register;
      --  Timerx External Event Filtering Register 2
      EEFCR2   : aliased EEFCR2_Register;
      --  TimerA Reset Register
      RSTCR    : aliased RSTCR_Register;
      --  Timerx Chopper Register
      CHPCR    : aliased CHPCR_Register;
      --  Timerx Capture 1 Control Register
      CPT1CCR  : aliased CPT1CCR_Register;
      --  Timerx Capture 2 Control Register
      CPT2CCR  : aliased CPT2CCR_Register;
      --  Timerx Output Register
      OUTCR    : aliased OUTCR_Register;
      --  Timerx Fault Register
      FLTCR    : aliased FLTCR_Register;
      --  HRTIM Timerx Control Register 2
      TIMCCR2  : aliased TIMCCR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFCR3   : aliased EEFCR3_Register;
   end record
     with Volatile;

   for HRTIM_TIMC_Peripheral use record
      TIMCCR   at 16#0# range 0 .. 31;
      TIMCISR  at 16#4# range 0 .. 31;
      TIMCICR  at 16#8# range 0 .. 31;
      TIMCDIER at 16#C# range 0 .. 31;
      CNTCR    at 16#10# range 0 .. 31;
      PERCR    at 16#14# range 0 .. 31;
      REPCR    at 16#18# range 0 .. 31;
      CMP1CR   at 16#1C# range 0 .. 31;
      CMP1CCR  at 16#20# range 0 .. 31;
      CMP2CR   at 16#24# range 0 .. 31;
      CMP3CR   at 16#28# range 0 .. 31;
      CMP4CR   at 16#2C# range 0 .. 31;
      CPT1CR   at 16#30# range 0 .. 31;
      CPT2CR   at 16#34# range 0 .. 31;
      DTCR     at 16#38# range 0 .. 31;
      SETC1R   at 16#3C# range 0 .. 31;
      RSTC1R   at 16#40# range 0 .. 31;
      SETC2R   at 16#44# range 0 .. 31;
      RSTC2R   at 16#48# range 0 .. 31;
      EEFCR1   at 16#4C# range 0 .. 31;
      EEFCR2   at 16#50# range 0 .. 31;
      RSTCR    at 16#54# range 0 .. 31;
      CHPCR    at 16#58# range 0 .. 31;
      CPT1CCR  at 16#5C# range 0 .. 31;
      CPT2CCR  at 16#60# range 0 .. 31;
      OUTCR    at 16#64# range 0 .. 31;
      FLTCR    at 16#68# range 0 .. 31;
      TIMCCR2  at 16#6C# range 0 .. 31;
      EEFCR3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: TIMC
   HRTIM_TIMC_Periph : aliased HRTIM_TIMC_Peripheral
     with Import, Address => HRTIM_TIMC_Base;

   --  High Resolution Timer: TIMD
   type HRTIM_TIMD_Peripheral is record
      --  Timerx Control Register
      TIMDCR   : aliased TIMDCR_Register;
      --  Timerx Interrupt Status Register
      TIMDISR  : aliased TIMDISR_Register;
      --  Timerx Interrupt Clear Register
      TIMDICR  : aliased TIMDICR_Register;
      --  TIMxDIER
      TIMDDIER : aliased TIMDDIER_Register;
      --  Timerx Counter Register
      CNTDR    : aliased CNTDR_Register;
      --  Timerx Period Register
      PERDR    : aliased PERDR_Register;
      --  Timerx Repetition Register
      REPDR    : aliased REPDR_Register;
      --  Timerx Compare 1 Register
      CMP1DR   : aliased CMP1DR_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CDR  : aliased CMP1CDR_Register;
      --  Timerx Compare 2 Register
      CMP2DR   : aliased CMP2DR_Register;
      --  Timerx Compare 3 Register
      CMP3DR   : aliased CMP3DR_Register;
      --  Timerx Compare 4 Register
      CMP4DR   : aliased CMP4DR_Register;
      --  Timerx Capture 1 Register
      CPT1DR   : aliased CPT1DR_Register;
      --  Timerx Capture 2 Register
      CPT2DR   : aliased CPT2DR_Register;
      --  Timerx Deadtime Register
      DTDR     : aliased DTDR_Register;
      --  Timerx Output1 Set Register
      SETD1R   : aliased SETD1R_Register;
      --  Timerx Output1 Reset Register
      RSTD1R   : aliased RSTD1R_Register;
      --  Timerx Output2 Set Register
      SETD2R   : aliased SETD2R_Register;
      --  Timerx Output2 Reset Register
      RSTD2R   : aliased RSTD2R_Register;
      --  Timerx External Event Filtering Register 1
      EEFDR1   : aliased EEFDR1_Register;
      --  Timerx External Event Filtering Register 2
      EEFDR2   : aliased EEFDR2_Register;
      --  TimerA Reset Register
      RSTDR    : aliased RSTDR_Register;
      --  Timerx Chopper Register
      CHPDR    : aliased CHPDR_Register;
      --  Timerx Capture 1 Control Register
      CPT1DCR  : aliased CPT1DCR_Register;
      --  Timerx Capture 2 Control Register
      CPT2DCR  : aliased CPT2DCR_Register;
      --  Timerx Output Register
      OUTDR    : aliased OUTDR_Register;
      --  Timerx Fault Register
      FLTDR    : aliased FLTDR_Register;
      --  HRTIM Timerx Control Register 2
      TIMDCR2  : aliased TIMDCR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFDR3   : aliased EEFDR3_Register;
   end record
     with Volatile;

   for HRTIM_TIMD_Peripheral use record
      TIMDCR   at 16#0# range 0 .. 31;
      TIMDISR  at 16#4# range 0 .. 31;
      TIMDICR  at 16#8# range 0 .. 31;
      TIMDDIER at 16#C# range 0 .. 31;
      CNTDR    at 16#10# range 0 .. 31;
      PERDR    at 16#14# range 0 .. 31;
      REPDR    at 16#18# range 0 .. 31;
      CMP1DR   at 16#1C# range 0 .. 31;
      CMP1CDR  at 16#20# range 0 .. 31;
      CMP2DR   at 16#24# range 0 .. 31;
      CMP3DR   at 16#28# range 0 .. 31;
      CMP4DR   at 16#2C# range 0 .. 31;
      CPT1DR   at 16#30# range 0 .. 31;
      CPT2DR   at 16#34# range 0 .. 31;
      DTDR     at 16#38# range 0 .. 31;
      SETD1R   at 16#3C# range 0 .. 31;
      RSTD1R   at 16#40# range 0 .. 31;
      SETD2R   at 16#44# range 0 .. 31;
      RSTD2R   at 16#48# range 0 .. 31;
      EEFDR1   at 16#4C# range 0 .. 31;
      EEFDR2   at 16#50# range 0 .. 31;
      RSTDR    at 16#54# range 0 .. 31;
      CHPDR    at 16#58# range 0 .. 31;
      CPT1DCR  at 16#5C# range 0 .. 31;
      CPT2DCR  at 16#60# range 0 .. 31;
      OUTDR    at 16#64# range 0 .. 31;
      FLTDR    at 16#68# range 0 .. 31;
      TIMDCR2  at 16#6C# range 0 .. 31;
      EEFDR3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: TIMD
   HRTIM_TIMD_Periph : aliased HRTIM_TIMD_Peripheral
     with Import, Address => HRTIM_TIMD_Base;

   --  High Resolution Timer: TIME
   type HRTIM_TIME_Peripheral is record
      --  Timerx Control Register
      TIMECR   : aliased TIMECR_Register;
      --  Timerx Interrupt Status Register
      TIMEISR  : aliased TIMEISR_Register;
      --  Timerx Interrupt Clear Register
      TIMEICR  : aliased TIMEICR_Register;
      --  TIMxDIER
      TIMEDIER : aliased TIMEDIER_Register;
      --  Timerx Counter Register
      CNTER    : aliased CNTER_Register;
      --  Timerx Period Register
      PERER    : aliased PERER_Register;
      --  Timerx Repetition Register
      REPER    : aliased REPER_Register;
      --  Timerx Compare 1 Register
      CMP1ER   : aliased CMP1ER_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CER  : aliased CMP1CER_Register;
      --  Timerx Compare 2 Register
      CMP2ER   : aliased CMP2ER_Register;
      --  Timerx Compare 3 Register
      CMP3ER   : aliased CMP3ER_Register;
      --  Timerx Compare 4 Register
      CMP4ER   : aliased CMP4ER_Register;
      --  Timerx Capture 1 Register
      CPT1ER   : aliased CPT1ER_Register;
      --  Timerx Capture 2 Register
      CPT2ER   : aliased CPT2ER_Register;
      --  Timerx Deadtime Register
      DTER     : aliased DTER_Register;
      --  Timerx Output1 Set Register
      SETE1R   : aliased SETE1R_Register;
      --  Timerx Output1 Reset Register
      RSTE1R   : aliased RSTE1R_Register;
      --  Timerx Output2 Set Register
      SETE2R   : aliased SETE2R_Register;
      --  Timerx Output2 Reset Register
      RSTE2R   : aliased RSTE2R_Register;
      --  Timerx External Event Filtering Register 1
      EEFER1   : aliased EEFER1_Register;
      --  Timerx External Event Filtering Register 2
      EEFER2   : aliased EEFER2_Register;
      --  TimerA Reset Register
      RSTER    : aliased RSTER_Register;
      --  Timerx Chopper Register
      CHPER    : aliased CHPER_Register;
      --  Timerx Capture 1 Control Register
      CPT1ECR  : aliased CPT1ECR_Register;
      --  Timerx Capture 2 Control Register
      CPT2ECR  : aliased CPT2ECR_Register;
      --  Timerx Output Register
      OUTER    : aliased OUTER_Register;
      --  Timerx Fault Register
      FLTER    : aliased FLTER_Register;
      --  HRTIM Timerx Control Register 2
      TIMECR2  : aliased TIMECR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFER3   : aliased EEFER3_Register;
   end record
     with Volatile;

   for HRTIM_TIME_Peripheral use record
      TIMECR   at 16#0# range 0 .. 31;
      TIMEISR  at 16#4# range 0 .. 31;
      TIMEICR  at 16#8# range 0 .. 31;
      TIMEDIER at 16#C# range 0 .. 31;
      CNTER    at 16#10# range 0 .. 31;
      PERER    at 16#14# range 0 .. 31;
      REPER    at 16#18# range 0 .. 31;
      CMP1ER   at 16#1C# range 0 .. 31;
      CMP1CER  at 16#20# range 0 .. 31;
      CMP2ER   at 16#24# range 0 .. 31;
      CMP3ER   at 16#28# range 0 .. 31;
      CMP4ER   at 16#2C# range 0 .. 31;
      CPT1ER   at 16#30# range 0 .. 31;
      CPT2ER   at 16#34# range 0 .. 31;
      DTER     at 16#38# range 0 .. 31;
      SETE1R   at 16#3C# range 0 .. 31;
      RSTE1R   at 16#40# range 0 .. 31;
      SETE2R   at 16#44# range 0 .. 31;
      RSTE2R   at 16#48# range 0 .. 31;
      EEFER1   at 16#4C# range 0 .. 31;
      EEFER2   at 16#50# range 0 .. 31;
      RSTER    at 16#54# range 0 .. 31;
      CHPER    at 16#58# range 0 .. 31;
      CPT1ECR  at 16#5C# range 0 .. 31;
      CPT2ECR  at 16#60# range 0 .. 31;
      OUTER    at 16#64# range 0 .. 31;
      FLTER    at 16#68# range 0 .. 31;
      TIMECR2  at 16#6C# range 0 .. 31;
      EEFER3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: TIME
   HRTIM_TIME_Periph : aliased HRTIM_TIME_Peripheral
     with Import, Address => HRTIM_TIME_Base;

   --  High Resolution Timer: TIMF
   type HRTIM_TIMF_Peripheral is record
      --  Timerx Control Register
      TIMFCR   : aliased TIMFCR_Register;
      --  Timerx Interrupt Status Register
      TIMFISR  : aliased TIMFISR_Register;
      --  Timerx Interrupt Clear Register
      TIMFICR  : aliased TIMFICR_Register;
      --  TIMxDIER
      TIMFDIER : aliased TIMFDIER_Register;
      --  Timerx Counter Register
      CNTFR    : aliased CNTFR_Register;
      --  Timerx Period Register
      PERFR    : aliased PERFR_Register;
      --  Timerx Repetition Register
      REPFR    : aliased REPFR_Register;
      --  Timerx Compare 1 Register
      CMP1FR   : aliased CMP1FR_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CFR  : aliased CMP1CFR_Register;
      --  Timerx Compare 2 Register
      CMP2FR   : aliased CMP2FR_Register;
      --  Timerx Compare 3 Register
      CMP3FR   : aliased CMP3FR_Register;
      --  Timerx Compare 4 Register
      CMP4FR   : aliased CMP4FR_Register;
      --  Timerx Capture 1 Register
      CPT1FR   : aliased CPT1FR_Register;
      --  Timerx Capture 2 Register
      CPT2FR   : aliased CPT2FR_Register;
      --  Timerx Deadtime Register
      DTFR     : aliased DTFR_Register;
      --  Timerx Output1 Set Register
      SETF1R   : aliased SETF1R_Register;
      --  Timerx Output1 Reset Register
      RSTE1R   : aliased RSTE1R_Register;
      --  Timerx Output2 Set Register
      SETF2R   : aliased SETF2R_Register;
      --  Timerx Output2 Reset Register
      RSTF2R   : aliased RSTF2R_Register;
      --  Timerx External Event Filtering Register 1
      EEFFR1   : aliased EEFFR1_Register;
      --  Timerx External Event Filtering Register 2
      EEFFR2   : aliased EEFFR2_Register;
      --  TimerA Reset Register
      RSTFR    : aliased RSTFR_Register;
      --  Timerx Chopper Register
      CHPFR    : aliased CHPFR_Register;
      --  Timerx Capture 1 Control Register
      CPT1FCR  : aliased CPT1FCR_Register;
      --  Timerx Capture 2 Control Register
      CPT2FCR  : aliased CPT2FCR_Register;
      --  Timerx Output Register
      OUTFR    : aliased OUTFR_Register;
      --  Timerx Fault Register
      FLTFR    : aliased FLTFR_Register;
      --  HRTIM Timerx Control Register 2
      TIMFCR2  : aliased TIMFCR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFFR3   : aliased EEFFR3_Register;
   end record
     with Volatile;

   for HRTIM_TIMF_Peripheral use record
      TIMFCR   at 16#0# range 0 .. 31;
      TIMFISR  at 16#4# range 0 .. 31;
      TIMFICR  at 16#8# range 0 .. 31;
      TIMFDIER at 16#C# range 0 .. 31;
      CNTFR    at 16#10# range 0 .. 31;
      PERFR    at 16#14# range 0 .. 31;
      REPFR    at 16#18# range 0 .. 31;
      CMP1FR   at 16#1C# range 0 .. 31;
      CMP1CFR  at 16#20# range 0 .. 31;
      CMP2FR   at 16#24# range 0 .. 31;
      CMP3FR   at 16#28# range 0 .. 31;
      CMP4FR   at 16#2C# range 0 .. 31;
      CPT1FR   at 16#30# range 0 .. 31;
      CPT2FR   at 16#34# range 0 .. 31;
      DTFR     at 16#38# range 0 .. 31;
      SETF1R   at 16#3C# range 0 .. 31;
      RSTE1R   at 16#40# range 0 .. 31;
      SETF2R   at 16#44# range 0 .. 31;
      RSTF2R   at 16#48# range 0 .. 31;
      EEFFR1   at 16#4C# range 0 .. 31;
      EEFFR2   at 16#50# range 0 .. 31;
      RSTFR    at 16#54# range 0 .. 31;
      CHPFR    at 16#58# range 0 .. 31;
      CPT1FCR  at 16#5C# range 0 .. 31;
      CPT2FCR  at 16#60# range 0 .. 31;
      OUTFR    at 16#64# range 0 .. 31;
      FLTFR    at 16#68# range 0 .. 31;
      TIMFCR2  at 16#6C# range 0 .. 31;
      EEFFR3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: TIMF
   HRTIM_TIMF_Periph : aliased HRTIM_TIMF_Peripheral
     with Import, Address => HRTIM_TIMF_Base;

end STM32_SVD.HRTIM;
