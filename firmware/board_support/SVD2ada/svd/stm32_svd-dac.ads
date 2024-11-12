pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.DAC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_TSEL1_Field is HAL.UInt4;
   subtype CR_WAVE1_Field is HAL.UInt2;
   subtype CR_MAMP1_Field is HAL.UInt4;
   subtype CR_TSEL2_Field is HAL.UInt4;
   subtype CR_WAVE2_Field is HAL.UInt2;
   subtype CR_MAMP2_Field is HAL.UInt4;

   --  DAC control register
   type CR_Register is record
      --  DAC channel1 enable This bit is set and cleared by software to
      --  enable/disable DAC channel1.
      EN1            : Boolean := False;
      --  DAC channel1 trigger enable
      TEN1           : Boolean := False;
      --  DAC channel1 trigger selection These bits select the external event
      --  used to trigger DAC channel1. Note: Only used if bit TEN1 = 1 (DAC
      --  channel1 trigger enabled).
      TSEL1          : CR_TSEL1_Field := 16#0#;
      --  DAC channel1 noise/triangle wave generation enable These bits are set
      --  and cleared by software. Note: Only used if bit TEN1 = 1 (DAC
      --  channel1 trigger enabled).
      WAVE1          : CR_WAVE1_Field := 16#0#;
      --  DAC channel1 mask/amplitude selector These bits are written by
      --  software to select mask in wave generation mode or amplitude in
      --  triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle
      --  amplitude equal to 4095
      MAMP1          : CR_MAMP1_Field := 16#0#;
      --  DAC channel1 DMA enable This bit is set and cleared by software.
      DMAEN1         : Boolean := False;
      --  DAC channel1 DMA Underrun Interrupt enable This bit is set and
      --  cleared by software.
      DMAUDRIE1      : Boolean := False;
      --  DAC Channel 1 calibration enable This bit is set and cleared by
      --  software to enable/disable DAC channel 1 calibration, it can be
      --  written only if bit EN1=0 into DAC_CR (the calibration mode can be
      --  entered/exit only when the DAC channel is disabled) Otherwise, the
      --  write operation is ignored.
      CEN1           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  DAC channel2 enable This bit is set and cleared by software to
      --  enable/disable DAC channel2.
      EN2            : Boolean := False;
      --  DAC channel2 trigger enable
      TEN2           : Boolean := False;
      --  DAC channel2 trigger selection These bits select the external event
      --  used to trigger DAC channel2 Note: Only used if bit TEN2 = 1 (DAC
      --  channel2 trigger enabled).
      TSEL2          : CR_TSEL2_Field := 16#0#;
      --  DAC channel2 noise/triangle wave generation enable These bits are
      --  set/reset by software. 1x: Triangle wave generation enabled Note:
      --  Only used if bit TEN2 = 1 (DAC channel2 trigger enabled)
      WAVE2          : CR_WAVE2_Field := 16#0#;
      --  DAC channel2 mask/amplitude selector These bits are written by
      --  software to select mask in wave generation mode or amplitude in
      --  triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle
      --  amplitude equal to 4095
      MAMP2          : CR_MAMP2_Field := 16#0#;
      --  DAC channel2 DMA enable This bit is set and cleared by software.
      DMAEN2         : Boolean := False;
      --  DAC channel2 DMA underrun interrupt enable This bit is set and
      --  cleared by software.
      DMAUDRIE2      : Boolean := False;
      --  DAC Channel 2 calibration enable This bit is set and cleared by
      --  software to enable/disable DAC channel 2 calibration, it can be
      --  written only if bit EN2=0 into DAC_CR (the calibration mode can be
      --  entered/exit only when the DAC channel is disabled) Otherwise, the
      --  write operation is ignored.
      CEN2           : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN1            at 0 range 0 .. 0;
      TEN1           at 0 range 1 .. 1;
      TSEL1          at 0 range 2 .. 5;
      WAVE1          at 0 range 6 .. 7;
      MAMP1          at 0 range 8 .. 11;
      DMAEN1         at 0 range 12 .. 12;
      DMAUDRIE1      at 0 range 13 .. 13;
      CEN1           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      EN2            at 0 range 16 .. 16;
      TEN2           at 0 range 17 .. 17;
      TSEL2          at 0 range 18 .. 21;
      WAVE2          at 0 range 22 .. 23;
      MAMP2          at 0 range 24 .. 27;
      DMAEN2         at 0 range 28 .. 28;
      DMAUDRIE2      at 0 range 29 .. 29;
      CEN2           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  SWTRGR_SWTRIG array
   type SWTRGR_SWTRIG_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SWTRGR_SWTRIG
   type SWTRGR_SWTRIG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWTRIG as a value
            Val : HAL.UInt2;
         when True =>
            --  SWTRIG as an array
            Arr : SWTRGR_SWTRIG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWTRGR_SWTRIG_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  SWTRGR_SWTRIGB array
   type SWTRGR_SWTRIGB_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SWTRGR_SWTRIGB
   type SWTRGR_SWTRIGB_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWTRIGB as a value
            Val : HAL.UInt2;
         when True =>
            --  SWTRIGB as an array
            Arr : SWTRGR_SWTRIGB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWTRGR_SWTRIGB_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DAC software trigger register
   type SWTRGR_Register is record
      --  Write-only. DAC channel1 software trigger This bit is set by software
      --  to trigger the DAC in software trigger mode. Note: This bit is
      --  cleared by hardware (one APB1 clock cycle later) once the DAC_DHR1
      --  register value has been loaded into the DAC_DOR1 register.
      SWTRIG         : SWTRGR_SWTRIG_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Write-only. DAC channel1 software trigger B
      SWTRIGB        : SWTRGR_SWTRIGB_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWTRGR_Register use record
      SWTRIG         at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      SWTRIGB        at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype DHR12R1_DACC1DHR_Field is HAL.UInt12;
   subtype DHR12R1_DACC1DHRB_Field is HAL.UInt12;

   --  DAC channel1 12-bit right-aligned data holding register
   type DHR12R1_Register is record
      --  DAC channel1 12-bit right-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel1.
      DACC1DHR       : DHR12R1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit right-aligned data B
      DACC1DHRB      : DHR12R1_DACC1DHRB_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12R1_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC1DHRB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DHR12L1_DACC1DHR_Field is HAL.UInt12;
   subtype DHR12L1_DACC1DHRB_Field is HAL.UInt12;

   --  DAC channel1 12-bit left aligned data holding register
   type DHR12L1_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel1.
      DACC1DHR       : DHR12L1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data B
      DACC1DHRB      : DHR12L1_DACC1DHRB_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12L1_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC1DHRB      at 0 range 20 .. 31;
   end record;

   subtype DHR8R1_DACC1DHR_Field is HAL.UInt8;
   subtype DHR8R1_DACC1DHRB_Field is HAL.UInt8;

   --  DAC channel1 8-bit right aligned data holding register
   type DHR8R1_Register is record
      --  DAC channel1 8-bit right-aligned data These bits are written by
      --  software which specifies 8-bit data for DAC channel1.
      DACC1DHR       : DHR8R1_DACC1DHR_Field := 16#0#;
      --  DAC channel1 8-bit right-aligned data
      DACC1DHRB      : DHR8R1_DACC1DHRB_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR8R1_Register use record
      DACC1DHR       at 0 range 0 .. 7;
      DACC1DHRB      at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DHR12R2_DACC2DHR_Field is HAL.UInt12;
   subtype DHR12R2_DACC2DHRB_Field is HAL.UInt12;

   --  DAC channel2 12-bit right aligned data holding register
   type DHR12R2_Register is record
      --  DAC channel2 12-bit right-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel2.
      DACC2DHR       : DHR12R2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit right-aligned data
      DACC2DHRB      : DHR12R2_DACC2DHRB_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12R2_Register use record
      DACC2DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DHRB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DHR12L2_DACC2DHR_Field is HAL.UInt12;
   subtype DHR12L2_DACC2DHRB_Field is HAL.UInt12;

   --  DAC channel2 12-bit left aligned data holding register
   type DHR12L2_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data These bits are written by
      --  software which specify 12-bit data for DAC channel2.
      DACC2DHR       : DHR12L2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data B
      DACC2DHRB      : DHR12L2_DACC2DHRB_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12L2_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC2DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC2DHRB      at 0 range 20 .. 31;
   end record;

   subtype DHR8R2_DACC2DHR_Field is HAL.UInt8;
   subtype DHR8R2_DACC2DHRB_Field is HAL.UInt8;

   --  DAC channel2 8-bit right-aligned data holding register
   type DHR8R2_Register is record
      --  DAC channel2 8-bit right-aligned data These bits are written by
      --  software which specifies 8-bit data for DAC channel2.
      DACC2DHR       : DHR8R2_DACC2DHR_Field := 16#0#;
      --  DAC channel2 8-bit right-aligned data
      DACC2DHRB      : DHR8R2_DACC2DHRB_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR8R2_Register use record
      DACC2DHR       at 0 range 0 .. 7;
      DACC2DHRB      at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DHR12RD_DACC1DHR_Field is HAL.UInt12;
   subtype DHR12RD_DACC2DHR_Field is HAL.UInt12;

   --  Dual DAC 12-bit right-aligned data holding register
   type DHR12RD_Register is record
      --  DAC channel1 12-bit right-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel1.
      DACC1DHR       : DHR12RD_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit right-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel2.
      DACC2DHR       : DHR12RD_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12RD_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DHR       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DHR12LD_DACC1DHR_Field is HAL.UInt12;
   subtype DHR12LD_DACC2DHR_Field is HAL.UInt12;

   --  DUAL DAC 12-bit left aligned data holding register
   type DHR12LD_Register is record
      --  unspecified
      Reserved_0_3   : HAL.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel1.
      DACC1DHR       : DHR12LD_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data These bits are written by
      --  software which specifies 12-bit data for DAC channel2.
      DACC2DHR       : DHR12LD_DACC2DHR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12LD_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC2DHR       at 0 range 20 .. 31;
   end record;

   subtype DHR8RD_DACC1DHR_Field is HAL.UInt8;
   subtype DHR8RD_DACC2DHR_Field is HAL.UInt8;

   --  DUAL DAC 8-bit right aligned data holding register
   type DHR8RD_Register is record
      --  DAC channel1 8-bit right-aligned data These bits are written by
      --  software which specifies 8-bit data for DAC channel1.
      DACC1DHR       : DHR8RD_DACC1DHR_Field := 16#0#;
      --  DAC channel2 8-bit right-aligned data These bits are written by
      --  software which specifies 8-bit data for DAC channel2.
      DACC2DHR       : DHR8RD_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR8RD_Register use record
      DACC1DHR       at 0 range 0 .. 7;
      DACC2DHR       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOR1_DACC1DOR_Field is HAL.UInt12;
   subtype DOR1_DACC1DORB_Field is HAL.UInt12;

   --  DAC channel1 data output register
   type DOR1_Register is record
      --  Read-only. DAC channel1 data output These bits are read-only, they
      --  contain data output for DAC channel1.
      DACC1DOR       : DOR1_DACC1DOR_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. DAC channel1 data output
      DACC1DORB      : DOR1_DACC1DORB_Field;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOR1_Register use record
      DACC1DOR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC1DORB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DOR2_DACC2DOR_Field is HAL.UInt12;
   subtype DOR2_DACC2DORB_Field is HAL.UInt12;

   --  DAC channel2 data output register
   type DOR2_Register is record
      --  Read-only. DAC channel2 data output These bits are read-only, they
      --  contain data output for DAC channel2.
      DACC2DOR       : DOR2_DACC2DOR_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. DAC channel2 data output
      DACC2DORB      : DOR2_DACC2DORB_Field;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOR2_Register use record
      DACC2DOR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DORB      at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  DAC status register
   type SR_Register is record
      --  unspecified
      Reserved_0_10  : HAL.UInt11 := 16#0#;
      --  DAC channel1 ready status bit
      DAC1RDY        : Boolean := False;
      --  DAC channel1 output register status bit
      DORSTAT1       : Boolean := False;
      --  DAC channel1 DMA underrun flag This bit is set by hardware and
      --  cleared by software (by writing it to 1).
      DMAUDR1        : Boolean := False;
      --  Read-only. DAC Channel 1 calibration offset status This bit is set
      --  and cleared by hardware
      CAL_FLAG1      : Boolean := False;
      --  Read-only. DAC Channel 1 busy writing sample time flag This bit is
      --  systematically set just after Sample & Hold mode enable and is set
      --  each time the software writes the register DAC_SHSR1, It is cleared
      --  by hardware when the write operation of DAC_SHSR1 is complete. (It
      --  takes about 3LSI periods of synchronization).
      BWST1          : Boolean := False;
      --  unspecified
      Reserved_16_26 : HAL.UInt11 := 16#0#;
      --  DAC channel 2 ready status bit
      DAC2RDY        : Boolean := False;
      --  DAC channel 2 output register status bit
      DORSTAT2       : Boolean := False;
      --  DAC channel2 DMA underrun flag This bit is set by hardware and
      --  cleared by software (by writing it to 1).
      DMAUDR2        : Boolean := False;
      --  Read-only. DAC Channel 2 calibration offset status This bit is set
      --  and cleared by hardware
      CAL_FLAG2      : Boolean := False;
      --  Read-only. DAC Channel 2 busy writing sample time flag This bit is
      --  systematically set just after Sample & Hold mode enable and is set
      --  each time the software writes the register DAC_SHSR2, It is cleared
      --  by hardware when the write operation of DAC_SHSR2 is complete. (It
      --  takes about 3 LSI periods of synchronization).
      BWST2          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      Reserved_0_10  at 0 range 0 .. 10;
      DAC1RDY        at 0 range 11 .. 11;
      DORSTAT1       at 0 range 12 .. 12;
      DMAUDR1        at 0 range 13 .. 13;
      CAL_FLAG1      at 0 range 14 .. 14;
      BWST1          at 0 range 15 .. 15;
      Reserved_16_26 at 0 range 16 .. 26;
      DAC2RDY        at 0 range 27 .. 27;
      DORSTAT2       at 0 range 28 .. 28;
      DMAUDR2        at 0 range 29 .. 29;
      CAL_FLAG2      at 0 range 30 .. 30;
      BWST2          at 0 range 31 .. 31;
   end record;

   subtype CCR_OTRIM1_Field is HAL.UInt5;
   subtype CCR_OTRIM2_Field is HAL.UInt5;

   --  DAC calibration control register
   type CCR_Register is record
      --  DAC Channel 1 offset trimming value
      OTRIM1         : CCR_OTRIM1_Field := 16#0#;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  DAC Channel 2 offset trimming value
      OTRIM2         : CCR_OTRIM2_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      OTRIM1         at 0 range 0 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      OTRIM2         at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype MCR_MODE1_Field is HAL.UInt3;
   subtype MCR_HFSEL_Field is HAL.UInt2;
   subtype MCR_MODE2_Field is HAL.UInt3;

   --  DAC mode control register
   type MCR_Register is record
      --  DAC Channel 1 mode These bits can be written only when the DAC is
      --  disabled and not in the calibration mode (when bit EN1=0 and bit CEN1
      --  =0 in the DAC_CR register). If EN1=1 or CEN1 =1 the write operation
      --  is ignored. They can be set and cleared by software to select the DAC
      --  Channel 1 mode: DAC Channel 1 in normal Mode DAC Channel 1 in sample
      --  &amp; hold mode
      MODE1          : MCR_MODE1_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  DAC Channel1 DMA double data mode
      DMADOUBLE1     : Boolean := False;
      --  Enable signed format for DAC channel1
      SINFORMAT1     : Boolean := False;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  High frequency interface mode selection
      HFSEL          : MCR_HFSEL_Field := 16#0#;
      --  DAC Channel 2 mode These bits can be written only when the DAC is
      --  disabled and not in the calibration mode (when bit EN2=0 and bit CEN2
      --  =0 in the DAC_CR register). If EN2=1 or CEN2 =1 the write operation
      --  is ignored. They can be set and cleared by software to select the DAC
      --  Channel 2 mode: DAC Channel 2 in normal Mode DAC Channel 2 in sample
      --  &amp; hold mode
      MODE2          : MCR_MODE2_Field := 16#0#;
      --  unspecified
      Reserved_19_23 : HAL.UInt5 := 16#0#;
      --  DAC Channel2 DMA double data mode
      DMADOUBLE2     : Boolean := False;
      --  Enable signed format for DAC channel2
      SINFORMAT2     : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCR_Register use record
      MODE1          at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      DMADOUBLE1     at 0 range 8 .. 8;
      SINFORMAT1     at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      HFSEL          at 0 range 14 .. 15;
      MODE2          at 0 range 16 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      DMADOUBLE2     at 0 range 24 .. 24;
      SINFORMAT2     at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype SHSR1_TSAMPLE1_Field is HAL.UInt10;

   --  DAC Sample and Hold sample time register 1
   type SHSR1_Register is record
      --  DAC Channel 1 sample Time (only valid in sample &amp; hold mode)
      --  These bits can be written when the DAC channel1 is disabled or also
      --  during normal operation. in the latter case, the write can be done
      --  only when BWSTx of DAC_SR register is low, If BWSTx=1, the write
      --  operation is ignored.
      TSAMPLE1       : SHSR1_TSAMPLE1_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHSR1_Register use record
      TSAMPLE1       at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype SHSR2_TSAMPLE2_Field is HAL.UInt10;

   --  DAC Sample and Hold sample time register 2
   type SHSR2_Register is record
      --  DAC Channel 2 sample Time (only valid in sample &amp; hold mode)
      --  These bits can be written when the DAC channel2 is disabled or also
      --  during normal operation. in the latter case, the write can be done
      --  only when BWSTx of DAC_SR register is low, if BWSTx=1, the write
      --  operation is ignored.
      TSAMPLE2       : SHSR2_TSAMPLE2_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHSR2_Register use record
      TSAMPLE2       at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype SHHR_THOLD1_Field is HAL.UInt10;
   subtype SHHR_THOLD2_Field is HAL.UInt10;

   --  DAC Sample and Hold hold time register
   type SHHR_Register is record
      --  DAC Channel 1 hold Time (only valid in sample &amp; hold mode) Hold
      --  time= (THOLD[9:0]) x T LSI
      THOLD1         : SHHR_THOLD1_Field := 16#1#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  DAC Channel 2 hold time (only valid in sample &amp; hold mode). Hold
      --  time= (THOLD[9:0]) x T LSI
      THOLD2         : SHHR_THOLD2_Field := 16#1#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHHR_Register use record
      THOLD1         at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      THOLD2         at 0 range 16 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype SHRR_TREFRESH1_Field is HAL.UInt8;
   subtype SHRR_TREFRESH2_Field is HAL.UInt8;

   --  DAC Sample and Hold refresh time register
   type SHRR_Register is record
      --  DAC Channel 1 refresh Time (only valid in sample &amp; hold mode)
      --  Refresh time= (TREFRESH[7:0]) x T LSI
      TREFRESH1      : SHRR_TREFRESH1_Field := 16#1#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      --  DAC Channel 2 refresh Time (only valid in sample &amp; hold mode)
      --  Refresh time= (TREFRESH[7:0]) x T LSI
      TREFRESH2      : SHRR_TREFRESH2_Field := 16#1#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHRR_Register use record
      TREFRESH1      at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      TREFRESH2      at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype STR1_STRSTDATA1_Field is HAL.UInt12;
   subtype STR1_STINCDATA1_Field is HAL.UInt16;

   --  Sawtooth register
   type STR1_Register is record
      --  DAC Channel 1 Sawtooth reset value
      STRSTDATA1     : STR1_STRSTDATA1_Field := 16#0#;
      --  DAC Channel1 Sawtooth direction setting
      STDIR1         : Boolean := False;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  DAC CH1 Sawtooth increment value (12.4 bit format)
      STINCDATA1     : STR1_STINCDATA1_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STR1_Register use record
      STRSTDATA1     at 0 range 0 .. 11;
      STDIR1         at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      STINCDATA1     at 0 range 16 .. 31;
   end record;

   subtype STR2_STRSTDATA2_Field is HAL.UInt12;
   subtype STR2_STINCDATA2_Field is HAL.UInt16;

   --  Sawtooth register
   type STR2_Register is record
      --  DAC Channel 2 Sawtooth reset value
      STRSTDATA2     : STR2_STRSTDATA2_Field := 16#0#;
      --  DAC Channel2 Sawtooth direction setting
      STDIR2         : Boolean := False;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  DAC CH2 Sawtooth increment value (12.4 bit format)
      STINCDATA2     : STR2_STINCDATA2_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STR2_Register use record
      STRSTDATA2     at 0 range 0 .. 11;
      STDIR2         at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      STINCDATA2     at 0 range 16 .. 31;
   end record;

   subtype STMODR_STRSTTRIGSEL1_Field is HAL.UInt4;
   subtype STMODR_STINCTRIGSEL1_Field is HAL.UInt4;
   subtype STMODR_STRSTTRIGSEL2_Field is HAL.UInt4;
   subtype STMODR_STINCTRIGSEL2_Field is HAL.UInt4;

   --  Sawtooth Mode register
   type STMODR_Register is record
      --  DAC Channel 1 Sawtooth Reset trigger selection
      STRSTTRIGSEL1  : STMODR_STRSTTRIGSEL1_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  DAC Channel 1 Sawtooth Increment trigger selection
      STINCTRIGSEL1  : STMODR_STINCTRIGSEL1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  DAC Channel 1 Sawtooth Reset trigger selection
      STRSTTRIGSEL2  : STMODR_STRSTTRIGSEL2_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  DAC Channel 2 Sawtooth Increment trigger selection
      STINCTRIGSEL2  : STMODR_STINCTRIGSEL2_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STMODR_Register use record
      STRSTTRIGSEL1  at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      STINCTRIGSEL1  at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      STRSTTRIGSEL2  at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      STINCTRIGSEL2  at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital-to-analog converter
   type DAC_Peripheral is record
      --  DAC control register
      CR      : aliased CR_Register;
      --  DAC software trigger register
      SWTRGR  : aliased SWTRGR_Register;
      --  DAC channel1 12-bit right-aligned data holding register
      DHR12R1 : aliased DHR12R1_Register;
      --  DAC channel1 12-bit left aligned data holding register
      DHR12L1 : aliased DHR12L1_Register;
      --  DAC channel1 8-bit right aligned data holding register
      DHR8R1  : aliased DHR8R1_Register;
      --  DAC channel2 12-bit right aligned data holding register
      DHR12R2 : aliased DHR12R2_Register;
      --  DAC channel2 12-bit left aligned data holding register
      DHR12L2 : aliased DHR12L2_Register;
      --  DAC channel2 8-bit right-aligned data holding register
      DHR8R2  : aliased DHR8R2_Register;
      --  Dual DAC 12-bit right-aligned data holding register
      DHR12RD : aliased DHR12RD_Register;
      --  DUAL DAC 12-bit left aligned data holding register
      DHR12LD : aliased DHR12LD_Register;
      --  DUAL DAC 8-bit right aligned data holding register
      DHR8RD  : aliased DHR8RD_Register;
      --  DAC channel1 data output register
      DOR1    : aliased DOR1_Register;
      --  DAC channel2 data output register
      DOR2    : aliased DOR2_Register;
      --  DAC status register
      SR      : aliased SR_Register;
      --  DAC calibration control register
      CCR     : aliased CCR_Register;
      --  DAC mode control register
      MCR     : aliased MCR_Register;
      --  DAC Sample and Hold sample time register 1
      SHSR1   : aliased SHSR1_Register;
      --  DAC Sample and Hold sample time register 2
      SHSR2   : aliased SHSR2_Register;
      --  DAC Sample and Hold hold time register
      SHHR    : aliased SHHR_Register;
      --  DAC Sample and Hold refresh time register
      SHRR    : aliased SHRR_Register;
      --  Sawtooth register
      STR1    : aliased STR1_Register;
      --  Sawtooth register
      STR2    : aliased STR2_Register;
      --  Sawtooth Mode register
      STMODR  : aliased STMODR_Register;
   end record
     with Volatile;

   for DAC_Peripheral use record
      CR      at 16#0# range 0 .. 31;
      SWTRGR  at 16#4# range 0 .. 31;
      DHR12R1 at 16#8# range 0 .. 31;
      DHR12L1 at 16#C# range 0 .. 31;
      DHR8R1  at 16#10# range 0 .. 31;
      DHR12R2 at 16#14# range 0 .. 31;
      DHR12L2 at 16#18# range 0 .. 31;
      DHR8R2  at 16#1C# range 0 .. 31;
      DHR12RD at 16#20# range 0 .. 31;
      DHR12LD at 16#24# range 0 .. 31;
      DHR8RD  at 16#28# range 0 .. 31;
      DOR1    at 16#2C# range 0 .. 31;
      DOR2    at 16#30# range 0 .. 31;
      SR      at 16#34# range 0 .. 31;
      CCR     at 16#38# range 0 .. 31;
      MCR     at 16#3C# range 0 .. 31;
      SHSR1   at 16#40# range 0 .. 31;
      SHSR2   at 16#44# range 0 .. 31;
      SHHR    at 16#48# range 0 .. 31;
      SHRR    at 16#4C# range 0 .. 31;
      STR1    at 16#58# range 0 .. 31;
      STR2    at 16#5C# range 0 .. 31;
      STMODR  at 16#60# range 0 .. 31;
   end record;

   --  Digital-to-analog converter
   DAC1_Periph : aliased DAC_Peripheral
     with Import, Address => DAC1_Base;

   --  Digital-to-analog converter
   DAC2_Periph : aliased DAC_Peripheral
     with Import, Address => DAC2_Base;

   --  Digital-to-analog converter
   DAC3_Periph : aliased DAC_Peripheral
     with Import, Address => DAC3_Base;

   --  Digital-to-analog converter
   DAC4_Periph : aliased DAC_Peripheral
     with Import, Address => DAC4_Base;

end STM32_SVD.DAC;
