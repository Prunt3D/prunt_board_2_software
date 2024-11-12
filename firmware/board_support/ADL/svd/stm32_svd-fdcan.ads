pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.FDCAN is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CREL_DAY_Field is HAL.UInt8;
   subtype CREL_MON_Field is HAL.UInt8;
   subtype CREL_YEAR_Field is HAL.UInt4;
   subtype CREL_SUBSTEP_Field is HAL.UInt4;
   subtype CREL_STEP_Field is HAL.UInt4;
   subtype CREL_REL_Field is HAL.UInt4;

   --  FDCAN Core Release Register
   type CREL_Register is record
      --  Read-only. DAY
      DAY     : CREL_DAY_Field;
      --  Read-only. MON
      MON     : CREL_MON_Field;
      --  Read-only. YEAR
      YEAR    : CREL_YEAR_Field;
      --  Read-only. SUBSTEP
      SUBSTEP : CREL_SUBSTEP_Field;
      --  Read-only. STEP
      STEP    : CREL_STEP_Field;
      --  Read-only. REL
      REL     : CREL_REL_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CREL_Register use record
      DAY     at 0 range 0 .. 7;
      MON     at 0 range 8 .. 15;
      YEAR    at 0 range 16 .. 19;
      SUBSTEP at 0 range 20 .. 23;
      STEP    at 0 range 24 .. 27;
      REL     at 0 range 28 .. 31;
   end record;

   subtype DBTP_DSJW_Field is HAL.UInt4;
   subtype DBTP_DTSEG2_Field is HAL.UInt4;
   subtype DBTP_DTSEG1_Field is HAL.UInt5;
   subtype DBTP_DBRP_Field is HAL.UInt5;

   --  This register is only writable if bits CCCR.CCE and CCCR.INIT are set.
   --  The CAN bit time may be programed in the range of 4 to 25 time quanta.
   --  The CAN time quantum may be programmed in the range of 1 to 1024 FDCAN
   --  clock periods. tq = (DBRP + 1) FDCAN clock period. DTSEG1 is the sum of
   --  Prop_Seg and Phase_Seg1. DTSEG2 is Phase_Seg2. Therefore the length of
   --  the bit time is (programmed values) [DTSEG1 + DTSEG2 + 3] tq or
   --  (functional values) [Sync_Seg + Prop_Seg + Phase_Seg1 + Phase_Seg2] tq.
   --  The Information Processing Time (IPT) is zero, meaning the data for the
   --  next bit is available at the first clock edge after the sample point.
   type DBTP_Register is record
      --  DSJW
      DSJW           : DBTP_DSJW_Field := 16#3#;
      --  DTSEG2
      DTSEG2         : DBTP_DTSEG2_Field := 16#3#;
      --  Write-only. DTSEG1
      DTSEG1         : DBTP_DTSEG1_Field := 16#A#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  DBRP
      DBRP           : DBTP_DBRP_Field := 16#0#;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#0#;
      --  Read-only. TDC
      TDC            : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBTP_Register use record
      DSJW           at 0 range 0 .. 3;
      DTSEG2         at 0 range 4 .. 7;
      DTSEG1         at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DBRP           at 0 range 16 .. 20;
      Reserved_21_22 at 0 range 21 .. 22;
      TDC            at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype TEST_TX_Field is HAL.UInt2;

   --  Write access to the Test Register has to be enabled by setting bit
   --  CCCR[TEST] to 1 . All Test Register functions are set to their reset
   --  values when bit CCCR[TEST] is reset. Loop Back mode and software control
   --  of Tx pin FDCANx_TX are hardware test modes. Programming TX differently
   --  from 00 may disturb the message transfer on the CAN bus.
   type TEST_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  LBCK
      LBCK          : Boolean := False;
      --  TX
      TX            : TEST_TX_Field := 16#0#;
      --  RX
      RX            : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TEST_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      LBCK          at 0 range 4 .. 4;
      TX            at 0 range 5 .. 6;
      RX            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RWD_WDC_Field is HAL.UInt8;
   subtype RWD_WDV_Field is HAL.UInt8;

   --  The RAM Watchdog monitors the READY output of the Message RAM. A Message
   --  RAM access starts the Message RAM Watchdog Counter with the value
   --  configured by the RWD[WDC] bits. The counter is reloaded with RWD[WDC]
   --  bits when the Message RAM signals successful completion by activating
   --  its READY output. In case there is no response from the Message RAM
   --  until the counter has counted down to 0, the counter stops and interrupt
   --  flag IR[WDI] bit is set. The RAM Watchdog Counter is clocked by the
   --  fdcan_pclk clock.
   type RWD_Register is record
      --  WDC
      WDC            : RWD_WDC_Field := 16#0#;
      --  Read-only. WDV
      WDV            : RWD_WDV_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RWD_Register use record
      WDC            at 0 range 0 .. 7;
      WDV            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  For details about setting and resetting of single bits see Software
   --  initialization.
   type CCCR_Register is record
      --  INIT
      INIT           : Boolean := True;
      --  CCE
      CCE            : Boolean := False;
      --  ASM
      ASM            : Boolean := False;
      --  CSA
      CSA            : Boolean := False;
      --  CSR
      CSR            : Boolean := False;
      --  MON
      MON            : Boolean := False;
      --  DAR
      DAR            : Boolean := False;
      --  TEST
      TEST           : Boolean := False;
      --  FDOE
      FDOE           : Boolean := False;
      --  BRSE
      BRSE           : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  PXHD
      PXHD           : Boolean := False;
      --  EFBI
      EFBI           : Boolean := False;
      --  TXP
      TXP            : Boolean := False;
      --  NISO
      NISO           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCCR_Register use record
      INIT           at 0 range 0 .. 0;
      CCE            at 0 range 1 .. 1;
      ASM            at 0 range 2 .. 2;
      CSA            at 0 range 3 .. 3;
      CSR            at 0 range 4 .. 4;
      MON            at 0 range 5 .. 5;
      DAR            at 0 range 6 .. 6;
      TEST           at 0 range 7 .. 7;
      FDOE           at 0 range 8 .. 8;
      BRSE           at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PXHD           at 0 range 12 .. 12;
      EFBI           at 0 range 13 .. 13;
      TXP            at 0 range 14 .. 14;
      NISO           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype NBTP_NTSEG2_Field is HAL.UInt7;
   subtype NBTP_NTSEG1_Field is HAL.UInt8;
   subtype NBTP_NBRP_Field is HAL.UInt9;
   subtype NBTP_NSJW_Field is HAL.UInt7;

   --  FDCAN_NBTP
   type NBTP_Register is record
      --  NTSEG2
      NTSEG2       : NBTP_NTSEG2_Field := 16#33#;
      --  unspecified
      Reserved_7_7 : HAL.Bit := 16#0#;
      --  NTSEG1
      NTSEG1       : NBTP_NTSEG1_Field := 16#A#;
      --  NBRP
      NBRP         : NBTP_NBRP_Field := 16#0#;
      --  NSJW
      NSJW         : NBTP_NSJW_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NBTP_Register use record
      NTSEG2       at 0 range 0 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
      NTSEG1       at 0 range 8 .. 15;
      NBRP         at 0 range 16 .. 24;
      NSJW         at 0 range 25 .. 31;
   end record;

   subtype TSCC_TSS_Field is HAL.UInt2;
   subtype TSCC_TCP_Field is HAL.UInt4;

   --  FDCAN Timestamp Counter Configuration Register
   type TSCC_Register is record
      --  TSS
      TSS            : TSCC_TSS_Field := 16#0#;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  TCP
      TCP            : TSCC_TCP_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSCC_Register use record
      TSS            at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      TCP            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype TSCV_TSC_Field is HAL.UInt16;

   --  FDCAN Timestamp Counter Value Register
   type TSCV_Register is record
      --  Read-only. TSC
      TSC            : TSCV_TSC_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSCV_Register use record
      TSC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TOCC_TOS_Field is HAL.UInt2;
   subtype TOCC_TOP_Field is HAL.UInt16;

   --  FDCAN Timeout Counter Configuration Register
   type TOCC_Register is record
      --  ETOC
      ETOC          : Boolean := False;
      --  Write-only. TOS
      TOS           : TOCC_TOS_Field := 16#0#;
      --  unspecified
      Reserved_3_15 : HAL.UInt13 := 16#0#;
      --  TOP
      TOP           : TOCC_TOP_Field := 16#FFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TOCC_Register use record
      ETOC          at 0 range 0 .. 0;
      TOS           at 0 range 1 .. 2;
      Reserved_3_15 at 0 range 3 .. 15;
      TOP           at 0 range 16 .. 31;
   end record;

   subtype TOCV_TOC_Field is HAL.UInt16;

   --  FDCAN Timeout Counter Value Register
   type TOCV_Register is record
      --  Read-only. TOC
      TOC            : TOCV_TOC_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TOCV_Register use record
      TOC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ECR_TEC_Field is HAL.UInt8;
   subtype ECR_REC_Field is HAL.UInt7;
   subtype ECR_CEL_Field is HAL.UInt8;

   --  FDCAN Error Counter Register
   type ECR_Register is record
      --  Read-only. TEC
      TEC            : ECR_TEC_Field;
      --  Read-only. TREC
      REC            : ECR_REC_Field;
      --  Read-only. RP
      RP             : Boolean;
      --  Read-only. CEL
      CEL            : ECR_CEL_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ECR_Register use record
      TEC            at 0 range 0 .. 7;
      REC            at 0 range 8 .. 14;
      RP             at 0 range 15 .. 15;
      CEL            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PSR_LEC_Field is HAL.UInt3;
   subtype PSR_ACT_Field is HAL.UInt2;
   subtype PSR_DLEC_Field is HAL.UInt3;
   subtype PSR_TDCV_Field is HAL.UInt7;

   --  FDCAN Protocol Status Register
   type PSR_Register is record
      --  LEC
      LEC            : PSR_LEC_Field := 16#7#;
      --  Write-only. ACT
      ACT            : PSR_ACT_Field := 16#0#;
      --  EP
      EP             : Boolean := False;
      --  EW
      EW             : Boolean := False;
      --  BO
      BO             : Boolean := False;
      --  Write-only. DLEC
      DLEC           : PSR_DLEC_Field := 16#7#;
      --  RESI
      RESI           : Boolean := False;
      --  RBRS
      RBRS           : Boolean := False;
      --  REDL
      REDL           : Boolean := False;
      --  PXE
      PXE            : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  TDCV
      TDCV           : PSR_TDCV_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSR_Register use record
      LEC            at 0 range 0 .. 2;
      ACT            at 0 range 3 .. 4;
      EP             at 0 range 5 .. 5;
      EW             at 0 range 6 .. 6;
      BO             at 0 range 7 .. 7;
      DLEC           at 0 range 8 .. 10;
      RESI           at 0 range 11 .. 11;
      RBRS           at 0 range 12 .. 12;
      REDL           at 0 range 13 .. 13;
      PXE            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      TDCV           at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype TDCR_TDCF_Field is HAL.UInt7;
   subtype TDCR_TDCO_Field is HAL.UInt7;

   --  FDCAN Transmitter Delay Compensation Register
   type TDCR_Register is record
      --  TDCF
      TDCF           : TDCR_TDCF_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  TDCO
      TDCO           : TDCR_TDCO_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TDCR_Register use record
      TDCF           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TDCO           at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  The flags are set when one of the listed conditions is detected
   --  (edge-sensitive). The flags remain set until the Host clears them. A
   --  flag is cleared by writing a 1 to the corresponding bit position.
   --  Writing a 0 has no effect. A hard reset will clear the register. The
   --  configuration of IE controls whether an interrupt is generated. The
   --  configuration of ILS controls on which interrupt line an interrupt is
   --  signaled.
   type IR_Register is record
      --  RF0N
      RF0N           : Boolean := False;
      --  RF0F
      RF0F           : Boolean := False;
      --  RF0L
      RF0L           : Boolean := False;
      --  RF1N
      RF1N           : Boolean := False;
      --  RF1F
      RF1F           : Boolean := False;
      --  RF1L
      RF1L           : Boolean := False;
      --  HPM
      HPM            : Boolean := False;
      --  TC
      TC             : Boolean := False;
      --  TCF
      TCF            : Boolean := False;
      --  TFE
      TFE            : Boolean := False;
      --  TEFN
      TEFN           : Boolean := False;
      --  TEFF
      TEFF           : Boolean := False;
      --  TEFL
      TEFL           : Boolean := False;
      --  TSW
      TSW            : Boolean := False;
      --  MRAF
      MRAF           : Boolean := False;
      --  TOO
      TOO            : Boolean := False;
      --  ELO
      ELO            : Boolean := False;
      --  EP
      EP             : Boolean := False;
      --  EW
      EW             : Boolean := False;
      --  BO
      BO             : Boolean := False;
      --  WDI
      WDI            : Boolean := False;
      --  PEA
      PEA            : Boolean := False;
      --  PED
      PED            : Boolean := False;
      --  ARA
      ARA            : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IR_Register use record
      RF0N           at 0 range 0 .. 0;
      RF0F           at 0 range 1 .. 1;
      RF0L           at 0 range 2 .. 2;
      RF1N           at 0 range 3 .. 3;
      RF1F           at 0 range 4 .. 4;
      RF1L           at 0 range 5 .. 5;
      HPM            at 0 range 6 .. 6;
      TC             at 0 range 7 .. 7;
      TCF            at 0 range 8 .. 8;
      TFE            at 0 range 9 .. 9;
      TEFN           at 0 range 10 .. 10;
      TEFF           at 0 range 11 .. 11;
      TEFL           at 0 range 12 .. 12;
      TSW            at 0 range 13 .. 13;
      MRAF           at 0 range 14 .. 14;
      TOO            at 0 range 15 .. 15;
      ELO            at 0 range 16 .. 16;
      EP             at 0 range 17 .. 17;
      EW             at 0 range 18 .. 18;
      BO             at 0 range 19 .. 19;
      WDI            at 0 range 20 .. 20;
      PEA            at 0 range 21 .. 21;
      PED            at 0 range 22 .. 22;
      ARA            at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  The settings in the Interrupt Enable register determine which status
   --  changes in the Interrupt Register will be signaled on an interrupt line.
   type IE_Register is record
      --  RF0NE
      RF0NE          : Boolean := False;
      --  RF0FE
      RF0FE          : Boolean := False;
      --  RF0LE
      RF0LE          : Boolean := False;
      --  RF1NE
      RF1NE          : Boolean := False;
      --  RF1FE
      RF1FE          : Boolean := False;
      --  RF1LE
      RF1LE          : Boolean := False;
      --  HPME
      HPME           : Boolean := False;
      --  TCE
      TCE            : Boolean := False;
      --  TCFE
      TCFE           : Boolean := False;
      --  TFEE
      TFEE           : Boolean := False;
      --  TEFNE
      TEFNE          : Boolean := False;
      --  TEFFE
      TEFFE          : Boolean := False;
      --  TEFLE
      TEFLE          : Boolean := False;
      --  TSWE
      TSWE           : Boolean := False;
      --  MRAFE
      MRAFE          : Boolean := False;
      --  TOOE
      TOOE           : Boolean := False;
      --  ELOE
      ELOE           : Boolean := False;
      --  EPE
      EPE            : Boolean := False;
      --  EWE
      EWE            : Boolean := False;
      --  BOE
      BOE            : Boolean := False;
      --  WDIE
      WDIE           : Boolean := False;
      --  PEAE
      PEAE           : Boolean := False;
      --  PEDE
      PEDE           : Boolean := False;
      --  ARAE
      ARAE           : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IE_Register use record
      RF0NE          at 0 range 0 .. 0;
      RF0FE          at 0 range 1 .. 1;
      RF0LE          at 0 range 2 .. 2;
      RF1NE          at 0 range 3 .. 3;
      RF1FE          at 0 range 4 .. 4;
      RF1LE          at 0 range 5 .. 5;
      HPME           at 0 range 6 .. 6;
      TCE            at 0 range 7 .. 7;
      TCFE           at 0 range 8 .. 8;
      TFEE           at 0 range 9 .. 9;
      TEFNE          at 0 range 10 .. 10;
      TEFFE          at 0 range 11 .. 11;
      TEFLE          at 0 range 12 .. 12;
      TSWE           at 0 range 13 .. 13;
      MRAFE          at 0 range 14 .. 14;
      TOOE           at 0 range 15 .. 15;
      ELOE           at 0 range 16 .. 16;
      EPE            at 0 range 17 .. 17;
      EWE            at 0 range 18 .. 18;
      BOE            at 0 range 19 .. 19;
      WDIE           at 0 range 20 .. 20;
      PEAE           at 0 range 21 .. 21;
      PEDE           at 0 range 22 .. 22;
      ARAE           at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  ILS_RXFIFO array
   type ILS_RXFIFO_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for ILS_RXFIFO
   type ILS_RXFIFO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RXFIFO as a value
            Val : HAL.UInt2;
         when True =>
            --  RXFIFO as an array
            Arr : ILS_RXFIFO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for ILS_RXFIFO_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  The Interrupt Line Select register assigns an interrupt generated by a
   --  specific interrupt flag from the Interrupt Register to one of the two
   --  module interrupt lines. For interrupt generation the respective
   --  interrupt line has to be enabled via ILE[EINT0] and ILE[EINT1].
   type ILS_Register is record
      --  RXFIFO0
      RXFIFO        : ILS_RXFIFO_Field := (As_Array => False, Val => 16#0#);
      --  SMSG
      SMSG          : Boolean := False;
      --  TFERR
      TFERR         : Boolean := False;
      --  MISC
      MISC          : Boolean := False;
      --  BERR
      BERR          : Boolean := False;
      --  PERR
      PERR          : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ILS_Register use record
      RXFIFO        at 0 range 0 .. 1;
      SMSG          at 0 range 2 .. 2;
      TFERR         at 0 range 3 .. 3;
      MISC          at 0 range 4 .. 4;
      BERR          at 0 range 5 .. 5;
      PERR          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  ILE_EINT array
   type ILE_EINT_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for ILE_EINT
   type ILE_EINT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EINT as a value
            Val : HAL.UInt2;
         when True =>
            --  EINT as an array
            Arr : ILE_EINT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for ILE_EINT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Each of the two interrupt lines to the CPU can be enabled/disabled
   --  separately by programming bits EINT0 and EINT1.
   type ILE_Register is record
      --  EINT0
      EINT          : ILE_EINT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ILE_Register use record
      EINT          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype RXGFC_ANFE_Field is HAL.UInt2;
   subtype RXGFC_ANFS_Field is HAL.UInt2;
   subtype RXGFC_LSS_Field is HAL.UInt5;
   subtype RXGFC_LSE_Field is HAL.UInt4;

   --  Global settings for Message ID filtering. The Global Filter
   --  Configuration controls the filter path for standard and extended
   --  messages as described in Figure706: Standard Message ID filter path and
   --  Figure707: Extended Message ID filter path.
   type RXGFC_Register is record
      --  RRFE
      RRFE           : Boolean := False;
      --  RRFS
      RRFS           : Boolean := False;
      --  Write-only. ANFE
      ANFE           : RXGFC_ANFE_Field := 16#0#;
      --  Write-only. ANFS
      ANFS           : RXGFC_ANFS_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  F1OM
      F1OM           : Boolean := False;
      --  F1OM
      F0OM           : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  LSS
      LSS            : RXGFC_LSS_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  LSE
      LSE            : RXGFC_LSE_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXGFC_Register use record
      RRFE           at 0 range 0 .. 0;
      RRFS           at 0 range 1 .. 1;
      ANFE           at 0 range 2 .. 3;
      ANFS           at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      F1OM           at 0 range 8 .. 8;
      F0OM           at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      LSS            at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      LSE            at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype XIDAM_EIDM_Field is HAL.UInt29;

   --  FDCAN Extended ID and Mask Register
   type XIDAM_Register is record
      --  EIDM
      EIDM           : XIDAM_EIDM_Field := 16#1FFFFFFF#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for XIDAM_Register use record
      EIDM           at 0 range 0 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype HPMS_BIDX_Field is HAL.UInt3;
   subtype HPMS_MSI_Field is HAL.UInt2;
   subtype HPMS_FIDX_Field is HAL.UInt5;

   --  This register is updated every time a Message ID filter element
   --  configured to generate a priority event match. This can be used to
   --  monitor the status of incoming high priority messages and to enable fast
   --  access to these messages.
   type HPMS_Register is record
      --  Read-only. BIDX
      BIDX           : HPMS_BIDX_Field;
      --  unspecified
      Reserved_3_5   : HAL.UInt3;
      --  Read-only. MSI
      MSI            : HPMS_MSI_Field;
      --  Read-only. FIDX
      FIDX           : HPMS_FIDX_Field;
      --  unspecified
      Reserved_13_14 : HAL.UInt2;
      --  Read-only. FLST
      FLST           : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HPMS_Register use record
      BIDX           at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      MSI            at 0 range 6 .. 7;
      FIDX           at 0 range 8 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      FLST           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RXF0S_F0FL_Field is HAL.UInt4;
   subtype RXF0S_F0GI_Field is HAL.UInt2;
   subtype RXF0S_F0PI_Field is HAL.UInt2;

   --  FDCAN Rx FIFO 0 Status Register
   type RXF0S_Register is record
      --  F0FL
      F0FL           : RXF0S_F0FL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  F0GI
      F0GI           : RXF0S_F0GI_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  F0PI
      F0PI           : RXF0S_F0PI_Field := 16#0#;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  F0F
      F0F            : Boolean := False;
      --  RF0L
      RF0L           : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXF0S_Register use record
      F0FL           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      F0GI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      F0PI           at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      F0F            at 0 range 24 .. 24;
      RF0L           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype RXF0A_F0AI_Field is HAL.UInt3;

   --  CAN Rx FIFO 0 Acknowledge Register
   type RXF0A_Register is record
      --  F0AI
      F0AI          : RXF0A_F0AI_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXF0A_Register use record
      F0AI          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype RXF1S_F1FL_Field is HAL.UInt3;
   subtype RXF1S_F1GI_Field is HAL.UInt2;
   subtype RXF1S_F1PI_Field is HAL.UInt2;

   --  FDCAN Rx FIFO 1 Status Register
   type RXF1S_Register is record
      --  Read-only. F1FL
      F1FL           : RXF1S_F1FL_Field;
      --  unspecified
      Reserved_3_7   : HAL.UInt5;
      --  Read-only. F1GI
      F1GI           : RXF1S_F1GI_Field;
      --  unspecified
      Reserved_10_15 : HAL.UInt6;
      --  Read-only. F1PI
      F1PI           : RXF1S_F1PI_Field;
      --  unspecified
      Reserved_18_23 : HAL.UInt6;
      --  Read-only. F1F
      F1F            : Boolean;
      --  Read-only. RF1L
      RF1L           : Boolean;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXF1S_Register use record
      F1FL           at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      F1GI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      F1PI           at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      F1F            at 0 range 24 .. 24;
      RF1L           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype RXF1A_F1AI_Field is HAL.UInt3;

   --  FDCAN Rx FIFO 1 Acknowledge Register
   type RXF1A_Register is record
      --  F1AI
      F1AI          : RXF1A_F1AI_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXF1A_Register use record
      F1AI          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  FDCAN Tx Buffer Configuration Register
   type TXBC_Register is record
      --  unspecified
      Reserved_0_29  : HAL.UInt30 := 16#0#;
      --  TFQM
      TFQM           : Boolean := False;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBC_Register use record
      Reserved_0_29  at 0 range 0 .. 29;
      TFQM           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype TXFQS_TFFL_Field is HAL.UInt3;
   subtype TXFQS_TFGI_Field is HAL.UInt2;
   subtype TXFQS_TFQPI_Field is HAL.UInt2;

   --  The Tx FIFO/Queue status is related to the pending Tx requests listed in
   --  register TXBRP. Therefore the effect of Add/Cancellation requests may be
   --  delayed due to a running Tx scan (TXBRP not yet updated).
   type TXFQS_Register is record
      --  Read-only. TFFL
      TFFL           : TXFQS_TFFL_Field;
      --  unspecified
      Reserved_3_7   : HAL.UInt5;
      --  Read-only. TFGI
      TFGI           : TXFQS_TFGI_Field;
      --  unspecified
      Reserved_10_15 : HAL.UInt6;
      --  Read-only. TFQPI
      TFQPI          : TXFQS_TFQPI_Field;
      --  unspecified
      Reserved_18_20 : HAL.UInt3;
      --  Read-only. TFQF
      TFQF           : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXFQS_Register use record
      TFFL           at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TFGI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      TFQPI          at 0 range 16 .. 17;
      Reserved_18_20 at 0 range 18 .. 20;
      TFQF           at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  TXBRP_TRP array
   type TXBRP_TRP_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBRP_TRP
   type TXBRP_TRP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TRP as a value
            Val : HAL.UInt3;
         when True =>
            --  TRP as an array
            Arr : TXBRP_TRP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBRP_TRP_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Request Pending Register
   type TXBRP_Register is record
      --  Read-only. TRP0
      TRP           : TXBRP_TRP_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBRP_Register use record
      TRP           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  TXBAR_AR array
   type TXBAR_AR_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBAR_AR
   type TXBAR_AR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AR as a value
            Val : HAL.UInt3;
         when True =>
            --  AR as an array
            Arr : TXBAR_AR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBAR_AR_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Add Request Register
   type TXBAR_Register is record
      --  TRP0
      AR            : TXBAR_AR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBAR_Register use record
      AR            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  TXBCR_CR array
   type TXBCR_CR_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBCR_CR
   type TXBCR_CR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CR as a value
            Val : HAL.UInt3;
         when True =>
            --  CR as an array
            Arr : TXBCR_CR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBCR_CR_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Cancellation Request Register
   type TXBCR_Register is record
      --  TRP0
      CR            : TXBCR_CR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBCR_Register use record
      CR            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  TXBTO_TO array
   type TXBTO_TO_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBTO_TO
   type TXBTO_TO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TO as a value
            Val : HAL.UInt3;
         when True =>
            --  TO as an array
            Arr : TXBTO_TO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBTO_TO_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Transmission Occurred Register
   type TXBTO_Register is record
      --  Read-only. TRP0
      TO            : TXBTO_TO_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBTO_Register use record
      TO            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  TXBCF_CF array
   type TXBCF_CF_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBCF_CF
   type TXBCF_CF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CF as a value
            Val : HAL.UInt3;
         when True =>
            --  CF as an array
            Arr : TXBCF_CF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBCF_CF_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Cancellation Finished Register
   type TXBCF_Register is record
      --  Read-only. TRP0
      CF            : TXBCF_CF_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBCF_Register use record
      CF            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  TXBTIE_TIE array
   type TXBTIE_TIE_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBTIE_TIE
   type TXBTIE_TIE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIE as a value
            Val : HAL.UInt3;
         when True =>
            --  TIE as an array
            Arr : TXBTIE_TIE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBTIE_TIE_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Transmission Interrupt Enable Register
   type TXBTIE_Register is record
      --  TRP0
      TIE           : TXBTIE_TIE_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBTIE_Register use record
      TIE           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  TXBCIE_CFIE array
   type TXBCIE_CFIE_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TXBCIE_CFIE
   type TXBCIE_CFIE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CFIE as a value
            Val : HAL.UInt3;
         when True =>
            --  CFIE as an array
            Arr : TXBCIE_CFIE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TXBCIE_CFIE_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
   type TXBCIE_Register is record
      --  TRP0
      CFIE          : TXBCIE_CFIE_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXBCIE_Register use record
      CFIE          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype TXEFS_EFFL_Field is HAL.UInt3;
   subtype TXEFS_EFGI_Field is HAL.UInt2;
   subtype TXEFS_EFPI_Field is HAL.UInt2;

   --  FDCAN Tx Event FIFO Status Register
   type TXEFS_Register is record
      --  Read-only. EFFL
      EFFL           : TXEFS_EFFL_Field;
      --  unspecified
      Reserved_3_7   : HAL.UInt5;
      --  Read-only. EFGI
      EFGI           : TXEFS_EFGI_Field;
      --  unspecified
      Reserved_10_15 : HAL.UInt6;
      --  Read-only. EFPI
      EFPI           : TXEFS_EFPI_Field;
      --  unspecified
      Reserved_18_23 : HAL.UInt6;
      --  Read-only. EFF
      EFF            : Boolean;
      --  Read-only. TEFL
      TEFL           : Boolean;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXEFS_Register use record
      EFFL           at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      EFGI           at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      EFPI           at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      EFF            at 0 range 24 .. 24;
      TEFL           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype TXEFA_EFAI_Field is HAL.UInt2;

   --  FDCAN Tx Event FIFO Acknowledge Register
   type TXEFA_Register is record
      --  EFAI
      EFAI          : TXEFA_EFAI_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXEFA_Register use record
      EFAI          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype CKDIV_PDIV_Field is HAL.UInt4;

   --  FDCAN CFG clock divider register
   type CKDIV_Register is record
      --  input clock divider. the APB clock could be divided prior to be used
      --  by the CAN sub
      PDIV          : CKDIV_PDIV_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CKDIV_Register use record
      PDIV          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  FDCAN
   type FDCAN_Peripheral is record
      --  FDCAN Core Release Register
      CREL   : aliased CREL_Register;
      --  FDCAN Core Release Register
      ENDN   : aliased HAL.UInt32;
      --  This register is only writable if bits CCCR.CCE and CCCR.INIT are
      --  set. The CAN bit time may be programed in the range of 4 to 25 time
      --  quanta. The CAN time quantum may be programmed in the range of 1 to
      --  1024 FDCAN clock periods. tq = (DBRP + 1) FDCAN clock period. DTSEG1
      --  is the sum of Prop_Seg and Phase_Seg1. DTSEG2 is Phase_Seg2.
      --  Therefore the length of the bit time is (programmed values) [DTSEG1 +
      --  DTSEG2 + 3] tq or (functional values) [Sync_Seg + Prop_Seg +
      --  Phase_Seg1 + Phase_Seg2] tq. The Information Processing Time (IPT) is
      --  zero, meaning the data for the next bit is available at the first
      --  clock edge after the sample point.
      DBTP   : aliased DBTP_Register;
      --  Write access to the Test Register has to be enabled by setting bit
      --  CCCR[TEST] to 1 . All Test Register functions are set to their reset
      --  values when bit CCCR[TEST] is reset. Loop Back mode and software
      --  control of Tx pin FDCANx_TX are hardware test modes. Programming TX
      --  differently from 00 may disturb the message transfer on the CAN bus.
      TEST   : aliased TEST_Register;
      --  The RAM Watchdog monitors the READY output of the Message RAM. A
      --  Message RAM access starts the Message RAM Watchdog Counter with the
      --  value configured by the RWD[WDC] bits. The counter is reloaded with
      --  RWD[WDC] bits when the Message RAM signals successful completion by
      --  activating its READY output. In case there is no response from the
      --  Message RAM until the counter has counted down to 0, the counter
      --  stops and interrupt flag IR[WDI] bit is set. The RAM Watchdog Counter
      --  is clocked by the fdcan_pclk clock.
      RWD    : aliased RWD_Register;
      --  For details about setting and resetting of single bits see Software
      --  initialization.
      CCCR   : aliased CCCR_Register;
      --  FDCAN_NBTP
      NBTP   : aliased NBTP_Register;
      --  FDCAN Timestamp Counter Configuration Register
      TSCC   : aliased TSCC_Register;
      --  FDCAN Timestamp Counter Value Register
      TSCV   : aliased TSCV_Register;
      --  FDCAN Timeout Counter Configuration Register
      TOCC   : aliased TOCC_Register;
      --  FDCAN Timeout Counter Value Register
      TOCV   : aliased TOCV_Register;
      --  FDCAN Error Counter Register
      ECR    : aliased ECR_Register;
      --  FDCAN Protocol Status Register
      PSR    : aliased PSR_Register;
      --  FDCAN Transmitter Delay Compensation Register
      TDCR   : aliased TDCR_Register;
      --  The flags are set when one of the listed conditions is detected
      --  (edge-sensitive). The flags remain set until the Host clears them. A
      --  flag is cleared by writing a 1 to the corresponding bit position.
      --  Writing a 0 has no effect. A hard reset will clear the register. The
      --  configuration of IE controls whether an interrupt is generated. The
      --  configuration of ILS controls on which interrupt line an interrupt is
      --  signaled.
      IR     : aliased IR_Register;
      --  The settings in the Interrupt Enable register determine which status
      --  changes in the Interrupt Register will be signaled on an interrupt
      --  line.
      IE     : aliased IE_Register;
      --  The Interrupt Line Select register assigns an interrupt generated by
      --  a specific interrupt flag from the Interrupt Register to one of the
      --  two module interrupt lines. For interrupt generation the respective
      --  interrupt line has to be enabled via ILE[EINT0] and ILE[EINT1].
      ILS    : aliased ILS_Register;
      --  Each of the two interrupt lines to the CPU can be enabled/disabled
      --  separately by programming bits EINT0 and EINT1.
      ILE    : aliased ILE_Register;
      --  Global settings for Message ID filtering. The Global Filter
      --  Configuration controls the filter path for standard and extended
      --  messages as described in Figure706: Standard Message ID filter path
      --  and Figure707: Extended Message ID filter path.
      RXGFC  : aliased RXGFC_Register;
      --  FDCAN Extended ID and Mask Register
      XIDAM  : aliased XIDAM_Register;
      --  This register is updated every time a Message ID filter element
      --  configured to generate a priority event match. This can be used to
      --  monitor the status of incoming high priority messages and to enable
      --  fast access to these messages.
      HPMS   : aliased HPMS_Register;
      --  FDCAN Rx FIFO 0 Status Register
      RXF0S  : aliased RXF0S_Register;
      --  CAN Rx FIFO 0 Acknowledge Register
      RXF0A  : aliased RXF0A_Register;
      --  FDCAN Rx FIFO 1 Status Register
      RXF1S  : aliased RXF1S_Register;
      --  FDCAN Rx FIFO 1 Acknowledge Register
      RXF1A  : aliased RXF1A_Register;
      --  FDCAN Tx Buffer Configuration Register
      TXBC   : aliased TXBC_Register;
      --  The Tx FIFO/Queue status is related to the pending Tx requests listed
      --  in register TXBRP. Therefore the effect of Add/Cancellation requests
      --  may be delayed due to a running Tx scan (TXBRP not yet updated).
      TXFQS  : aliased TXFQS_Register;
      --  FDCAN Tx Buffer Request Pending Register
      TXBRP  : aliased TXBRP_Register;
      --  FDCAN Tx Buffer Add Request Register
      TXBAR  : aliased TXBAR_Register;
      --  FDCAN Tx Buffer Cancellation Request Register
      TXBCR  : aliased TXBCR_Register;
      --  FDCAN Tx Buffer Transmission Occurred Register
      TXBTO  : aliased TXBTO_Register;
      --  FDCAN Tx Buffer Cancellation Finished Register
      TXBCF  : aliased TXBCF_Register;
      --  FDCAN Tx Buffer Transmission Interrupt Enable Register
      TXBTIE : aliased TXBTIE_Register;
      --  FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
      TXBCIE : aliased TXBCIE_Register;
      --  FDCAN Tx Event FIFO Status Register
      TXEFS  : aliased TXEFS_Register;
      --  FDCAN Tx Event FIFO Acknowledge Register
      TXEFA  : aliased TXEFA_Register;
      --  FDCAN CFG clock divider register
      CKDIV  : aliased CKDIV_Register;
   end record
     with Volatile;

   for FDCAN_Peripheral use record
      CREL   at 16#0# range 0 .. 31;
      ENDN   at 16#4# range 0 .. 31;
      DBTP   at 16#C# range 0 .. 31;
      TEST   at 16#10# range 0 .. 31;
      RWD    at 16#14# range 0 .. 31;
      CCCR   at 16#18# range 0 .. 31;
      NBTP   at 16#1C# range 0 .. 31;
      TSCC   at 16#20# range 0 .. 31;
      TSCV   at 16#24# range 0 .. 31;
      TOCC   at 16#28# range 0 .. 31;
      TOCV   at 16#2C# range 0 .. 31;
      ECR    at 16#40# range 0 .. 31;
      PSR    at 16#44# range 0 .. 31;
      TDCR   at 16#48# range 0 .. 31;
      IR     at 16#50# range 0 .. 31;
      IE     at 16#54# range 0 .. 31;
      ILS    at 16#58# range 0 .. 31;
      ILE    at 16#5C# range 0 .. 31;
      RXGFC  at 16#80# range 0 .. 31;
      XIDAM  at 16#84# range 0 .. 31;
      HPMS   at 16#88# range 0 .. 31;
      RXF0S  at 16#90# range 0 .. 31;
      RXF0A  at 16#94# range 0 .. 31;
      RXF1S  at 16#98# range 0 .. 31;
      RXF1A  at 16#9C# range 0 .. 31;
      TXBC   at 16#C0# range 0 .. 31;
      TXFQS  at 16#C4# range 0 .. 31;
      TXBRP  at 16#C8# range 0 .. 31;
      TXBAR  at 16#CC# range 0 .. 31;
      TXBCR  at 16#D0# range 0 .. 31;
      TXBTO  at 16#D4# range 0 .. 31;
      TXBCF  at 16#D8# range 0 .. 31;
      TXBTIE at 16#DC# range 0 .. 31;
      TXBCIE at 16#E0# range 0 .. 31;
      TXEFS  at 16#E4# range 0 .. 31;
      TXEFA  at 16#E8# range 0 .. 31;
      CKDIV  at 16#100# range 0 .. 31;
   end record;

   --  FDCAN
   FDCAN_Periph : aliased FDCAN_Peripheral
     with Import, Address => FDCAN_Base;

   --  FDCAN
   FDCAN1_Periph : aliased FDCAN_Peripheral
     with Import, Address => FDCAN1_Base;

   --  FDCAN
   FDCAN2_Periph : aliased FDCAN_Peripheral
     with Import, Address => FDCAN2_Base;

   --  FDCAN
   FDCAN3_Periph : aliased FDCAN_Peripheral
     with Import, Address => FDCAN3_Base;

end STM32_SVD.FDCAN;
