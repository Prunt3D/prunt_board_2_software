pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.DMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  interrupt status register
   type ISR_Register is record
      --  Read-only. GIF1
      GIF1  : Boolean;
      --  Read-only. TCIF1
      TCIF1 : Boolean;
      --  Read-only. HTIF1
      HTIF1 : Boolean;
      --  Read-only. TEIF1
      TEIF1 : Boolean;
      --  Read-only. GIF2
      GIF2  : Boolean;
      --  Read-only. TCIF2
      TCIF2 : Boolean;
      --  Read-only. HTIF2
      HTIF2 : Boolean;
      --  Read-only. TEIF2
      TEIF2 : Boolean;
      --  Read-only. GIF3
      GIF3  : Boolean;
      --  Read-only. TCIF3
      TCIF3 : Boolean;
      --  Read-only. HTIF3
      HTIF3 : Boolean;
      --  Read-only. TEIF3
      TEIF3 : Boolean;
      --  Read-only. GIF4
      GIF4  : Boolean;
      --  Read-only. TCIF4
      TCIF4 : Boolean;
      --  Read-only. HTIF4
      HTIF4 : Boolean;
      --  Read-only. TEIF4
      TEIF4 : Boolean;
      --  Read-only. GIF5
      GIF5  : Boolean;
      --  Read-only. TCIF5
      TCIF5 : Boolean;
      --  Read-only. HTIF5
      HTIF5 : Boolean;
      --  Read-only. TEIF5
      TEIF5 : Boolean;
      --  Read-only. GIF6
      GIF6  : Boolean;
      --  Read-only. TCIF6
      TCIF6 : Boolean;
      --  Read-only. HTIF6
      HTIF6 : Boolean;
      --  Read-only. TEIF6
      TEIF6 : Boolean;
      --  Read-only. GIF7
      GIF7  : Boolean;
      --  Read-only. TCIF7
      TCIF7 : Boolean;
      --  Read-only. HTIF7
      HTIF7 : Boolean;
      --  Read-only. TEIF7
      TEIF7 : Boolean;
      --  Read-only. GIF8
      GIF8  : Boolean;
      --  Read-only. TCIF8
      TCIF8 : Boolean;
      --  Read-only. HTIF8
      HTIF8 : Boolean;
      --  Read-only. TEIF8
      TEIF8 : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      GIF1  at 0 range 0 .. 0;
      TCIF1 at 0 range 1 .. 1;
      HTIF1 at 0 range 2 .. 2;
      TEIF1 at 0 range 3 .. 3;
      GIF2  at 0 range 4 .. 4;
      TCIF2 at 0 range 5 .. 5;
      HTIF2 at 0 range 6 .. 6;
      TEIF2 at 0 range 7 .. 7;
      GIF3  at 0 range 8 .. 8;
      TCIF3 at 0 range 9 .. 9;
      HTIF3 at 0 range 10 .. 10;
      TEIF3 at 0 range 11 .. 11;
      GIF4  at 0 range 12 .. 12;
      TCIF4 at 0 range 13 .. 13;
      HTIF4 at 0 range 14 .. 14;
      TEIF4 at 0 range 15 .. 15;
      GIF5  at 0 range 16 .. 16;
      TCIF5 at 0 range 17 .. 17;
      HTIF5 at 0 range 18 .. 18;
      TEIF5 at 0 range 19 .. 19;
      GIF6  at 0 range 20 .. 20;
      TCIF6 at 0 range 21 .. 21;
      HTIF6 at 0 range 22 .. 22;
      TEIF6 at 0 range 23 .. 23;
      GIF7  at 0 range 24 .. 24;
      TCIF7 at 0 range 25 .. 25;
      HTIF7 at 0 range 26 .. 26;
      TEIF7 at 0 range 27 .. 27;
      GIF8  at 0 range 28 .. 28;
      TCIF8 at 0 range 29 .. 29;
      HTIF8 at 0 range 30 .. 30;
      TEIF8 at 0 range 31 .. 31;
   end record;

   --  DMA interrupt flag clear register
   type IFCR_Register is record
      --  Write-only. CGIF1
      CGIF1  : Boolean := False;
      --  Write-only. CTCIF1
      CTCIF1 : Boolean := False;
      --  Write-only. CHTIF1
      CHTIF1 : Boolean := False;
      --  Write-only. CTEIF1
      CTEIF1 : Boolean := False;
      --  Write-only. CGIF2
      CGIF2  : Boolean := False;
      --  Write-only. CTCIF2
      CTCIF2 : Boolean := False;
      --  Write-only. CHTIF2
      CHTIF2 : Boolean := False;
      --  Write-only. CTEIF2
      CTEIF2 : Boolean := False;
      --  Write-only. CGIF3
      CGIF3  : Boolean := False;
      --  Write-only. CTCIF3
      CTCIF3 : Boolean := False;
      --  Write-only. CHTIF3
      CHTIF3 : Boolean := False;
      --  Write-only. CTEIF3
      CTEIF3 : Boolean := False;
      --  Write-only. CGIF4
      CGIF4  : Boolean := False;
      --  Write-only. CTCIF4
      CTCIF4 : Boolean := False;
      --  Write-only. CHTIF4
      CHTIF4 : Boolean := False;
      --  Write-only. CTEIF4
      CTEIF4 : Boolean := False;
      --  Write-only. CGIF5
      CGIF5  : Boolean := False;
      --  Write-only. CTCIF5
      CTCIF5 : Boolean := False;
      --  Write-only. CHTIF5
      CHTIF5 : Boolean := False;
      --  Write-only. CTEIF5
      CTEIF5 : Boolean := False;
      --  Write-only. CGIF6
      CGIF6  : Boolean := False;
      --  Write-only. CTCIF6
      CTCIF6 : Boolean := False;
      --  Write-only. CHTIF6
      CHTIF6 : Boolean := False;
      --  Write-only. CTEIF6
      CTEIF6 : Boolean := False;
      --  Write-only. CGIF7
      CGIF7  : Boolean := False;
      --  Write-only. CTCIF7
      CTCIF7 : Boolean := False;
      --  Write-only. HTIF7
      CHTIF7 : Boolean := False;
      --  Write-only. CTEIF7
      CTEIF7 : Boolean := False;
      --  Write-only. CGIF8
      CGIF8  : Boolean := False;
      --  Write-only. CTCIF8
      CTCIF8 : Boolean := False;
      --  Write-only. CHTIF8
      CHTIF8 : Boolean := False;
      --  Write-only. CTEIF8
      CTEIF8 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      CGIF1  at 0 range 0 .. 0;
      CTCIF1 at 0 range 1 .. 1;
      CHTIF1 at 0 range 2 .. 2;
      CTEIF1 at 0 range 3 .. 3;
      CGIF2  at 0 range 4 .. 4;
      CTCIF2 at 0 range 5 .. 5;
      CHTIF2 at 0 range 6 .. 6;
      CTEIF2 at 0 range 7 .. 7;
      CGIF3  at 0 range 8 .. 8;
      CTCIF3 at 0 range 9 .. 9;
      CHTIF3 at 0 range 10 .. 10;
      CTEIF3 at 0 range 11 .. 11;
      CGIF4  at 0 range 12 .. 12;
      CTCIF4 at 0 range 13 .. 13;
      CHTIF4 at 0 range 14 .. 14;
      CTEIF4 at 0 range 15 .. 15;
      CGIF5  at 0 range 16 .. 16;
      CTCIF5 at 0 range 17 .. 17;
      CHTIF5 at 0 range 18 .. 18;
      CTEIF5 at 0 range 19 .. 19;
      CGIF6  at 0 range 20 .. 20;
      CTCIF6 at 0 range 21 .. 21;
      CHTIF6 at 0 range 22 .. 22;
      CTEIF6 at 0 range 23 .. 23;
      CGIF7  at 0 range 24 .. 24;
      CTCIF7 at 0 range 25 .. 25;
      CHTIF7 at 0 range 26 .. 26;
      CTEIF7 at 0 range 27 .. 27;
      CGIF8  at 0 range 28 .. 28;
      CTCIF8 at 0 range 29 .. 29;
      CHTIF8 at 0 range 30 .. 30;
      CTEIF8 at 0 range 31 .. 31;
   end record;

   subtype CCR_PSIZE_Field is HAL.UInt2;
   subtype CCR_MSIZE_Field is HAL.UInt2;
   subtype CCR_PL_Field is HAL.UInt2;

   --  DMA channel 1 configuration register
   type CCR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  TCIE
      TCIE           : Boolean := False;
      --  HTIE
      HTIE           : Boolean := False;
      --  TEIE
      TEIE           : Boolean := False;
      --  DIR
      DIR            : Boolean := False;
      --  CIRC
      CIRC           : Boolean := False;
      --  PINC
      PINC           : Boolean := False;
      --  MINC
      MINC           : Boolean := False;
      --  PSIZE
      PSIZE          : CCR_PSIZE_Field := 16#0#;
      --  MSIZE
      MSIZE          : CCR_MSIZE_Field := 16#0#;
      --  PL
      PL             : CCR_PL_Field := 16#0#;
      --  MEM2MEM
      MEM2MEM        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      EN             at 0 range 0 .. 0;
      TCIE           at 0 range 1 .. 1;
      HTIE           at 0 range 2 .. 2;
      TEIE           at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      CIRC           at 0 range 5 .. 5;
      PINC           at 0 range 6 .. 6;
      MINC           at 0 range 7 .. 7;
      PSIZE          at 0 range 8 .. 9;
      MSIZE          at 0 range 10 .. 11;
      PL             at 0 range 12 .. 13;
      MEM2MEM        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype CNDTR_NDT_Field is HAL.UInt16;

   --  channel x number of data to transfer register
   type CNDTR_Register is record
      --  Number of data items to transfer
      NDT            : CNDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMA controller
   type DMA_Peripheral is record
      --  interrupt status register
      ISR    : aliased ISR_Register;
      --  DMA interrupt flag clear register
      IFCR   : aliased IFCR_Register;
      --  DMA channel 1 configuration register
      CCR1   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR1 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR1  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR1  : aliased HAL.UInt32;
      --  DMA channel 2 configuration register
      CCR2   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR2 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR2  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR2  : aliased HAL.UInt32;
      --  DMA channel 3 configuration register
      CCR3   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR3 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR3  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR3  : aliased HAL.UInt32;
      --  DMA channel 3 configuration register
      CCR4   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR4 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR4  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR4  : aliased HAL.UInt32;
      --  DMA channel 4 configuration register
      CCR5   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR5 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR5  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR5  : aliased HAL.UInt32;
      --  DMA channel 5 configuration register
      CCR6   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR6 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR6  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR6  : aliased HAL.UInt32;
      --  DMA channel 6 configuration register
      CCR7   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR7 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR7  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR7  : aliased HAL.UInt32;
      --  DMA channel 7 configuration register
      CCR8   : aliased CCR_Register;
      --  channel x number of data to transfer register
      CNDTR8 : aliased CNDTR_Register;
      --  DMA channel x peripheral address register
      CPAR8  : aliased HAL.UInt32;
      --  DMA channel x memory address register
      CMAR8  : aliased HAL.UInt32;
   end record
     with Volatile;

   for DMA_Peripheral use record
      ISR    at 16#0# range 0 .. 31;
      IFCR   at 16#4# range 0 .. 31;
      CCR1   at 16#8# range 0 .. 31;
      CNDTR1 at 16#C# range 0 .. 31;
      CPAR1  at 16#10# range 0 .. 31;
      CMAR1  at 16#14# range 0 .. 31;
      CCR2   at 16#1C# range 0 .. 31;
      CNDTR2 at 16#20# range 0 .. 31;
      CPAR2  at 16#24# range 0 .. 31;
      CMAR2  at 16#28# range 0 .. 31;
      CCR3   at 16#30# range 0 .. 31;
      CNDTR3 at 16#34# range 0 .. 31;
      CPAR3  at 16#38# range 0 .. 31;
      CMAR3  at 16#3C# range 0 .. 31;
      CCR4   at 16#44# range 0 .. 31;
      CNDTR4 at 16#48# range 0 .. 31;
      CPAR4  at 16#4C# range 0 .. 31;
      CMAR4  at 16#50# range 0 .. 31;
      CCR5   at 16#58# range 0 .. 31;
      CNDTR5 at 16#5C# range 0 .. 31;
      CPAR5  at 16#60# range 0 .. 31;
      CMAR5  at 16#64# range 0 .. 31;
      CCR6   at 16#6C# range 0 .. 31;
      CNDTR6 at 16#70# range 0 .. 31;
      CPAR6  at 16#74# range 0 .. 31;
      CMAR6  at 16#78# range 0 .. 31;
      CCR7   at 16#80# range 0 .. 31;
      CNDTR7 at 16#84# range 0 .. 31;
      CPAR7  at 16#88# range 0 .. 31;
      CMAR7  at 16#8C# range 0 .. 31;
      CCR8   at 16#94# range 0 .. 31;
      CNDTR8 at 16#98# range 0 .. 31;
      CPAR8  at 16#9C# range 0 .. 31;
      CMAR8  at 16#A0# range 0 .. 31;
   end record;

   --  DMA controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => DMA1_Base;

   --  DMA controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => DMA2_Base;

end STM32_SVD.DMA;
