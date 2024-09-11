pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.USB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype EP0R_EA_Field is HAL.UInt4;
   subtype EP0R_STAT_TX_Field is HAL.UInt2;
   subtype EP0R_EP_TYPE_Field is HAL.UInt2;
   subtype EP0R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP0R_Register is record
      --  EA
      EA             : EP0R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP0R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP0R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP0R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP0R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP1R_EA_Field is HAL.UInt4;
   subtype EP1R_STAT_TX_Field is HAL.UInt2;
   subtype EP1R_EP_TYPE_Field is HAL.UInt2;
   subtype EP1R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP1R_Register is record
      --  EA
      EA             : EP1R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP1R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP1R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP1R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP1R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP2R_EA_Field is HAL.UInt4;
   subtype EP2R_STAT_TX_Field is HAL.UInt2;
   subtype EP2R_EP_TYPE_Field is HAL.UInt2;
   subtype EP2R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP2R_Register is record
      --  EA
      EA             : EP2R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP2R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP2R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP2R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP2R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP3R_EA_Field is HAL.UInt4;
   subtype EP3R_STAT_TX_Field is HAL.UInt2;
   subtype EP3R_EP_TYPE_Field is HAL.UInt2;
   subtype EP3R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP3R_Register is record
      --  EA
      EA             : EP3R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP3R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP3R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP3R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP3R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP4R_EA_Field is HAL.UInt4;
   subtype EP4R_STAT_TX_Field is HAL.UInt2;
   subtype EP4R_EP_TYPE_Field is HAL.UInt2;
   subtype EP4R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP4R_Register is record
      --  EA
      EA             : EP4R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP4R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP4R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP4R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP4R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP5R_EA_Field is HAL.UInt4;
   subtype EP5R_STAT_TX_Field is HAL.UInt2;
   subtype EP5R_EP_TYPE_Field is HAL.UInt2;
   subtype EP5R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP5R_Register is record
      --  EA
      EA             : EP5R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP5R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP5R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP5R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP5R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP6R_EA_Field is HAL.UInt4;
   subtype EP6R_STAT_TX_Field is HAL.UInt2;
   subtype EP6R_EP_TYPE_Field is HAL.UInt2;
   subtype EP6R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP6R_Register is record
      --  EA
      EA             : EP6R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP6R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP6R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP6R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP6R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype EP7R_EA_Field is HAL.UInt4;
   subtype EP7R_STAT_TX_Field is HAL.UInt2;
   subtype EP7R_EP_TYPE_Field is HAL.UInt2;
   subtype EP7R_STAT_RX_Field is HAL.UInt2;

   --  USB endpoint n register
   type EP7R_Register is record
      --  EA
      EA             : EP7R_EA_Field := 16#0#;
      --  STAT_TX
      STAT_TX        : EP7R_STAT_TX_Field := 16#0#;
      --  DTOG_TX
      DTOG_TX        : Boolean := False;
      --  CTR_TX
      CTR_TX         : Boolean := False;
      --  EP_KIND
      EP_KIND        : Boolean := False;
      --  EP_TYPE
      EP_TYPE        : EP7R_EP_TYPE_Field := 16#0#;
      --  SETUP
      SETUP          : Boolean := False;
      --  STAT_RX
      STAT_RX        : EP7R_STAT_RX_Field := 16#0#;
      --  DTOG_RX
      DTOG_RX        : Boolean := False;
      --  CTR_RX
      CTR_RX         : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EP7R_Register use record
      EA             at 0 range 0 .. 3;
      STAT_TX        at 0 range 4 .. 5;
      DTOG_TX        at 0 range 6 .. 6;
      CTR_TX         at 0 range 7 .. 7;
      EP_KIND        at 0 range 8 .. 8;
      EP_TYPE        at 0 range 9 .. 10;
      SETUP          at 0 range 11 .. 11;
      STAT_RX        at 0 range 12 .. 13;
      DTOG_RX        at 0 range 14 .. 14;
      CTR_RX         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  USB control register
   type CNTR_Register is record
      --  FRES
      FRES           : Boolean := False;
      --  PDWN
      PDWN           : Boolean := False;
      --  LP_MODE
      LP_MODE        : Boolean := False;
      --  FSUSP
      FSUSP          : Boolean := False;
      --  RESUME
      RESUME         : Boolean := False;
      --  L1RESUME
      L1RESUME       : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  L1REQM
      L1REQM         : Boolean := False;
      --  ESOFM
      ESOFM          : Boolean := False;
      --  SOFM
      SOFM           : Boolean := False;
      --  RESETM
      RESETM         : Boolean := False;
      --  SUSPM
      SUSPM          : Boolean := False;
      --  WKUPM
      WKUPM          : Boolean := False;
      --  ERRM
      ERRM           : Boolean := False;
      --  PMAOVRM
      PMAOVRM        : Boolean := False;
      --  CTRM
      CTRM           : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNTR_Register use record
      FRES           at 0 range 0 .. 0;
      PDWN           at 0 range 1 .. 1;
      LP_MODE        at 0 range 2 .. 2;
      FSUSP          at 0 range 3 .. 3;
      RESUME         at 0 range 4 .. 4;
      L1RESUME       at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      L1REQM         at 0 range 7 .. 7;
      ESOFM          at 0 range 8 .. 8;
      SOFM           at 0 range 9 .. 9;
      RESETM         at 0 range 10 .. 10;
      SUSPM          at 0 range 11 .. 11;
      WKUPM          at 0 range 12 .. 12;
      ERRM           at 0 range 13 .. 13;
      PMAOVRM        at 0 range 14 .. 14;
      CTRM           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ISTR_EP_ID_Field is HAL.UInt4;

   --  USB interrupt status register
   type ISTR_Register is record
      --  EP_ID
      EP_ID          : ISTR_EP_ID_Field := 16#0#;
      --  DIR
      DIR            : Boolean := False;
      --  unspecified
      Reserved_5_6   : HAL.UInt2 := 16#0#;
      --  L1REQ
      L1REQ          : Boolean := False;
      --  ESOF
      ESOF           : Boolean := False;
      --  SOF
      SOF            : Boolean := False;
      --  RESET
      RESET          : Boolean := False;
      --  SUSP
      SUSP           : Boolean := False;
      --  WKUP
      WKUP           : Boolean := False;
      --  ERR
      ERR            : Boolean := False;
      --  PMAOVR
      PMAOVR         : Boolean := False;
      --  CTR
      CTR            : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISTR_Register use record
      EP_ID          at 0 range 0 .. 3;
      DIR            at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      L1REQ          at 0 range 7 .. 7;
      ESOF           at 0 range 8 .. 8;
      SOF            at 0 range 9 .. 9;
      RESET          at 0 range 10 .. 10;
      SUSP           at 0 range 11 .. 11;
      WKUP           at 0 range 12 .. 12;
      ERR            at 0 range 13 .. 13;
      PMAOVR         at 0 range 14 .. 14;
      CTR            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FNR_FN_Field is HAL.UInt11;
   subtype FNR_LSOF_Field is HAL.UInt2;

   --  USB frame number register
   type FNR_Register is record
      --  Read-only. FN
      FN             : FNR_FN_Field;
      --  Read-only. LSOF
      LSOF           : FNR_LSOF_Field;
      --  Read-only. LCK
      LCK            : Boolean;
      --  Read-only. RXDM
      RXDM           : Boolean;
      --  Read-only. RXDP
      RXDP           : Boolean;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FNR_Register use record
      FN             at 0 range 0 .. 10;
      LSOF           at 0 range 11 .. 12;
      LCK            at 0 range 13 .. 13;
      RXDM           at 0 range 14 .. 14;
      RXDP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DADDR_ADD_Field is HAL.UInt7;

   --  USB device address
   type DADDR_Register is record
      --  ADD
      ADD           : DADDR_ADD_Field := 16#0#;
      --  EF
      EF            : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DADDR_Register use record
      ADD           at 0 range 0 .. 6;
      EF            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype BTABLE_BTABLE_Field is HAL.UInt13;

   --  Buffer table address
   type BTABLE_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  BTABLE
      BTABLE         : BTABLE_BTABLE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BTABLE_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      BTABLE         at 0 range 3 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB_FS_device
   type USB_FS_device_Peripheral is record
      --  USB endpoint n register
      EP0R   : aliased EP0R_Register;
      --  USB endpoint n register
      EP1R   : aliased EP1R_Register;
      --  USB endpoint n register
      EP2R   : aliased EP2R_Register;
      --  USB endpoint n register
      EP3R   : aliased EP3R_Register;
      --  USB endpoint n register
      EP4R   : aliased EP4R_Register;
      --  USB endpoint n register
      EP5R   : aliased EP5R_Register;
      --  USB endpoint n register
      EP6R   : aliased EP6R_Register;
      --  USB endpoint n register
      EP7R   : aliased EP7R_Register;
      --  USB control register
      CNTR   : aliased CNTR_Register;
      --  USB interrupt status register
      ISTR   : aliased ISTR_Register;
      --  USB frame number register
      FNR    : aliased FNR_Register;
      --  USB device address
      DADDR  : aliased DADDR_Register;
      --  Buffer table address
      BTABLE : aliased BTABLE_Register;
   end record
     with Volatile;

   for USB_FS_device_Peripheral use record
      EP0R   at 16#0# range 0 .. 31;
      EP1R   at 16#4# range 0 .. 31;
      EP2R   at 16#8# range 0 .. 31;
      EP3R   at 16#C# range 0 .. 31;
      EP4R   at 16#10# range 0 .. 31;
      EP5R   at 16#14# range 0 .. 31;
      EP6R   at 16#18# range 0 .. 31;
      EP7R   at 16#1C# range 0 .. 31;
      CNTR   at 16#40# range 0 .. 31;
      ISTR   at 16#44# range 0 .. 31;
      FNR    at 16#48# range 0 .. 31;
      DADDR  at 16#4C# range 0 .. 31;
      BTABLE at 16#50# range 0 .. 31;
   end record;

   --  USB_FS_device
   USB_FS_device_Periph : aliased USB_FS_device_Peripheral
     with Import, Address => USB_FS_device_Base;

end STM32_SVD.USB;
