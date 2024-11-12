--
--  Copyright (C) 2022, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd


with System;

package Interfaces.STM32.RCC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR_HSION_Field is Interfaces.STM32.Bit;
   subtype CR_HSIKERON_Field is Interfaces.STM32.Bit;
   subtype CR_HSIRDY_Field is Interfaces.STM32.Bit;
   subtype CR_HSEON_Field is Interfaces.STM32.Bit;
   subtype CR_HSERDY_Field is Interfaces.STM32.Bit;
   subtype CR_HSEBYP_Field is Interfaces.STM32.Bit;
   subtype CR_CSSON_Field is Interfaces.STM32.Bit;
   subtype CR_PLLON_Field is Interfaces.STM32.Bit;
   subtype CR_PLLRDY_Field is Interfaces.STM32.Bit;

   --  Clock control register
   type CR_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.STM32.Byte := 16#63#;
      --  HSI clock enable
      HSION          : CR_HSION_Field := 16#0#;
      --  HSI always enable for peripheral kernels
      HSIKERON       : CR_HSIKERON_Field := 16#0#;
      --  Read-only. HSI clock ready flag
      HSIRDY         : CR_HSIRDY_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : Interfaces.STM32.UInt5 := 16#0#;
      --  HSE clock enable
      HSEON          : CR_HSEON_Field := 16#0#;
      --  Read-only. HSE clock ready flag
      HSERDY         : CR_HSERDY_Field := 16#0#;
      --  HSE crystal oscillator bypass
      HSEBYP         : CR_HSEBYP_Field := 16#0#;
      --  Write-only. Clock security system enable
      CSSON          : CR_CSSON_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : Interfaces.STM32.UInt4 := 16#0#;
      --  Main PLL enable
      PLLON          : CR_PLLON_Field := 16#0#;
      --  Read-only. Main PLL clock ready flag
      PLLRDY         : CR_PLLRDY_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : Interfaces.STM32.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      HSION          at 0 range 8 .. 8;
      HSIKERON       at 0 range 9 .. 9;
      HSIRDY         at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      HSEON          at 0 range 16 .. 16;
      HSERDY         at 0 range 17 .. 17;
      HSEBYP         at 0 range 18 .. 18;
      CSSON          at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      PLLON          at 0 range 24 .. 24;
      PLLRDY         at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype ICSCR_HSICAL0_Field is Interfaces.STM32.Byte;
   subtype ICSCR_HSITRIM_Field is Interfaces.STM32.UInt7;

   --  Internal clock sources calibration register
   type ICSCR_Register is record
      --  unspecified
      Reserved_0_15  : Interfaces.STM32.UInt16 := 16#0#;
      --  Read-only. Internal High Speed clock Calibration
      HSICAL0        : ICSCR_HSICAL0_Field := 16#0#;
      --  Internal High Speed clock trimming
      HSITRIM        : ICSCR_HSITRIM_Field := 16#40#;
      --  unspecified
      Reserved_31_31 : Interfaces.STM32.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICSCR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      HSICAL0        at 0 range 16 .. 23;
      HSITRIM        at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CFGR_SW_Field is Interfaces.STM32.UInt2;
   subtype CFGR_SWS_Field is Interfaces.STM32.UInt2;
   subtype CFGR_HPRE_Field is Interfaces.STM32.UInt4;
   --  CFGR_PPRE array element
   subtype CFGR_PPRE_Element is Interfaces.STM32.UInt3;

   --  CFGR_PPRE array
   type CFGR_PPRE_Field_Array is array (1 .. 2) of CFGR_PPRE_Element
     with Component_Size => 3, Size => 6;

   --  Type definition for CFGR_PPRE
   type CFGR_PPRE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PPRE as a value
            Val : Interfaces.STM32.UInt6;
         when True =>
            --  PPRE as an array
            Arr : CFGR_PPRE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CFGR_PPRE_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype CFGR_MCOSEL_Field is Interfaces.STM32.UInt4;
   subtype CFGR_MCOPRE_Field is Interfaces.STM32.UInt3;

   --  Clock configuration register
   type CFGR_Register is record
      --  System clock switch
      SW             : CFGR_SW_Field := 16#1#;
      --  Read-only. System clock switch status
      SWS            : CFGR_SWS_Field := 16#1#;
      --  AHB prescaler
      HPRE           : CFGR_HPRE_Field := 16#0#;
      --  PB low-speed prescaler (APB1)
      PPRE           : CFGR_PPRE_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_23 : Interfaces.STM32.UInt10 := 16#0#;
      --  Microcontroller clock output
      MCOSEL         : CFGR_MCOSEL_Field := 16#0#;
      --  Microcontroller clock output prescaler
      MCOPRE         : CFGR_MCOPRE_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.STM32.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      SW             at 0 range 0 .. 1;
      SWS            at 0 range 2 .. 3;
      HPRE           at 0 range 4 .. 7;
      PPRE           at 0 range 8 .. 13;
      Reserved_14_23 at 0 range 14 .. 23;
      MCOSEL         at 0 range 24 .. 27;
      MCOPRE         at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype PLLCFGR_PLLSRC_Field is Interfaces.STM32.UInt2;
   subtype PLLCFGR_PLLM_Field is Interfaces.STM32.UInt4;
   subtype PLLCFGR_PLLN_Field is Interfaces.STM32.UInt7;
   subtype PLLCFGR_PLLPEN_Field is Interfaces.STM32.Bit;
   subtype PLLCFGR_PLLP_Field is Interfaces.STM32.Bit;
   subtype PLLCFGR_PLLQEN_Field is Interfaces.STM32.Bit;
   subtype PLLCFGR_PLLQ_Field is Interfaces.STM32.UInt2;
   subtype PLLCFGR_PLLREN_Field is Interfaces.STM32.Bit;
   subtype PLLCFGR_PLLR_Field is Interfaces.STM32.UInt2;
   subtype PLLCFGR_PLLPDIV_Field is Interfaces.STM32.UInt5;

   --  PLL configuration register
   type PLLCFGR_Register is record
      --  Main PLL, PLLSAI1 and PLLSAI2 entry clock source
      PLLSRC         : PLLCFGR_PLLSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : Interfaces.STM32.UInt2 := 16#0#;
      --  Division factor for the main PLL and audio PLL (PLLSAI1 and PLLSAI2)
      --  input clock
      PLLM           : PLLCFGR_PLLM_Field := 16#0#;
      --  Main PLL multiplication factor for VCO
      PLLN           : PLLCFGR_PLLN_Field := 16#10#;
      --  unspecified
      Reserved_15_15 : Interfaces.STM32.Bit := 16#0#;
      --  Main PLL PLLSAI3CLK output enable
      PLLPEN         : PLLCFGR_PLLPEN_Field := 16#0#;
      --  Main PLL division factor for PLLSAI3CLK (SAI1 and SAI2 clock)
      PLLP           : PLLCFGR_PLLP_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.STM32.UInt2 := 16#0#;
      --  Main PLL PLLUSB1CLK output enable
      PLLQEN         : PLLCFGR_PLLQEN_Field := 16#0#;
      --  Main PLL division factor for PLLUSB1CLK(48 MHz clock)
      PLLQ           : PLLCFGR_PLLQ_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : Interfaces.STM32.Bit := 16#0#;
      --  Main PLL PLLCLK output enable
      PLLREN         : PLLCFGR_PLLREN_Field := 16#0#;
      --  Main PLL division factor for PLLCLK (system clock)
      PLLR           : PLLCFGR_PLLR_Field := 16#0#;
      --  Main PLL division factor for PLLSAI2CLK
      PLLPDIV        : PLLCFGR_PLLPDIV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLLCFGR_Register use record
      PLLSRC         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      PLLM           at 0 range 4 .. 7;
      PLLN           at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PLLPEN         at 0 range 16 .. 16;
      PLLP           at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      PLLQEN         at 0 range 20 .. 20;
      PLLQ           at 0 range 21 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      PLLREN         at 0 range 24 .. 24;
      PLLR           at 0 range 25 .. 26;
      PLLPDIV        at 0 range 27 .. 31;
   end record;

   subtype CIER_LSIRDYIE_Field is Interfaces.STM32.Bit;
   subtype CIER_LSERDYIE_Field is Interfaces.STM32.Bit;
   subtype CIER_HSIRDYIE_Field is Interfaces.STM32.Bit;
   subtype CIER_HSERDYIE_Field is Interfaces.STM32.Bit;
   subtype CIER_PLLSYSRDYIE_Field is Interfaces.STM32.Bit;
   subtype CIER_LSECSSIE_Field is Interfaces.STM32.Bit;
   subtype CIER_RC48RDYIE_Field is Interfaces.STM32.Bit;

   --  Clock interrupt enable register
   type CIER_Register is record
      --  LSI ready interrupt enable
      LSIRDYIE       : CIER_LSIRDYIE_Field := 16#0#;
      --  LSE ready interrupt enable
      LSERDYIE       : CIER_LSERDYIE_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : Interfaces.STM32.Bit := 16#0#;
      --  HSI ready interrupt enable
      HSIRDYIE       : CIER_HSIRDYIE_Field := 16#0#;
      --  HSE ready interrupt enable
      HSERDYIE       : CIER_HSERDYIE_Field := 16#0#;
      --  PLL ready interrupt enable
      PLLSYSRDYIE    : CIER_PLLSYSRDYIE_Field := 16#0#;
      --  unspecified
      Reserved_6_8   : Interfaces.STM32.UInt3 := 16#0#;
      --  LSE clock security system interrupt enable
      LSECSSIE       : CIER_LSECSSIE_Field := 16#0#;
      --  HSI48 ready interrupt enable
      RC48RDYIE      : CIER_RC48RDYIE_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : Interfaces.STM32.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIER_Register use record
      LSIRDYIE       at 0 range 0 .. 0;
      LSERDYIE       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      HSIRDYIE       at 0 range 3 .. 3;
      HSERDYIE       at 0 range 4 .. 4;
      PLLSYSRDYIE    at 0 range 5 .. 5;
      Reserved_6_8   at 0 range 6 .. 8;
      LSECSSIE       at 0 range 9 .. 9;
      RC48RDYIE      at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype CIFR_LSIRDYF_Field is Interfaces.STM32.Bit;
   subtype CIFR_LSERDYF_Field is Interfaces.STM32.Bit;
   subtype CIFR_HSIRDYF_Field is Interfaces.STM32.Bit;
   subtype CIFR_HSERDYF_Field is Interfaces.STM32.Bit;
   subtype CIFR_PLLSYSRDYF_Field is Interfaces.STM32.Bit;
   subtype CIFR_HSECSSF_Field is Interfaces.STM32.Bit;
   subtype CIFR_LSECSSF_Field is Interfaces.STM32.Bit;
   subtype CIFR_RC48RDYF_Field is Interfaces.STM32.Bit;

   --  Clock interrupt flag register
   type CIFR_Register is record
      --  Read-only. LSI ready interrupt flag
      LSIRDYF        : CIFR_LSIRDYF_Field;
      --  Read-only. LSE ready interrupt flag
      LSERDYF        : CIFR_LSERDYF_Field;
      --  unspecified
      Reserved_2_2   : Interfaces.STM32.Bit;
      --  Read-only. HSI ready interrupt flag
      HSIRDYF        : CIFR_HSIRDYF_Field;
      --  Read-only. HSE ready interrupt flag
      HSERDYF        : CIFR_HSERDYF_Field;
      --  Read-only. PLL ready interrupt flag
      PLLSYSRDYF     : CIFR_PLLSYSRDYF_Field;
      --  unspecified
      Reserved_6_7   : Interfaces.STM32.UInt2;
      --  Read-only. Clock security system interrupt flag
      HSECSSF        : CIFR_HSECSSF_Field;
      --  Read-only. LSE Clock security system interrupt flag
      LSECSSF        : CIFR_LSECSSF_Field;
      --  Read-only. HSI48 ready interrupt flag
      RC48RDYF       : CIFR_RC48RDYF_Field;
      --  unspecified
      Reserved_11_31 : Interfaces.STM32.UInt21;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CIFR_Register use record
      LSIRDYF        at 0 range 0 .. 0;
      LSERDYF        at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      HSIRDYF        at 0 range 3 .. 3;
      HSERDYF        at 0 range 4 .. 4;
      PLLSYSRDYF     at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      HSECSSF        at 0 range 8 .. 8;
      LSECSSF        at 0 range 9 .. 9;
      RC48RDYF       at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype CICR_LSIRDYC_Field is Interfaces.STM32.Bit;
   subtype CICR_LSERDYC_Field is Interfaces.STM32.Bit;
   subtype CICR_HSIRDYC_Field is Interfaces.STM32.Bit;
   subtype CICR_HSERDYC_Field is Interfaces.STM32.Bit;
   subtype CICR_PLLSYSRDYC_Field is Interfaces.STM32.Bit;
   subtype CICR_HSECSSC_Field is Interfaces.STM32.Bit;
   subtype CICR_LSECSSC_Field is Interfaces.STM32.Bit;
   subtype CICR_RC48RDYC_Field is Interfaces.STM32.Bit;

   --  Clock interrupt clear register
   type CICR_Register is record
      --  Write-only. LSI ready interrupt clear
      LSIRDYC        : CICR_LSIRDYC_Field := 16#0#;
      --  Write-only. LSE ready interrupt clear
      LSERDYC        : CICR_LSERDYC_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : Interfaces.STM32.Bit := 16#0#;
      --  Write-only. HSI ready interrupt clear
      HSIRDYC        : CICR_HSIRDYC_Field := 16#0#;
      --  Write-only. HSE ready interrupt clear
      HSERDYC        : CICR_HSERDYC_Field := 16#0#;
      --  Write-only. PLL ready interrupt clear
      PLLSYSRDYC     : CICR_PLLSYSRDYC_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : Interfaces.STM32.UInt2 := 16#0#;
      --  Write-only. Clock security system interrupt clear
      HSECSSC        : CICR_HSECSSC_Field := 16#0#;
      --  Write-only. LSE Clock security system interrupt clear
      LSECSSC        : CICR_LSECSSC_Field := 16#0#;
      --  Write-only. HSI48 oscillator ready interrupt clear
      RC48RDYC       : CICR_RC48RDYC_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : Interfaces.STM32.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CICR_Register use record
      LSIRDYC        at 0 range 0 .. 0;
      LSERDYC        at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      HSIRDYC        at 0 range 3 .. 3;
      HSERDYC        at 0 range 4 .. 4;
      PLLSYSRDYC     at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      HSECSSC        at 0 range 8 .. 8;
      LSECSSC        at 0 range 9 .. 9;
      RC48RDYC       at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype AHB1RSTR_DMA1RST_Field is Interfaces.STM32.Bit;
   subtype AHB1RSTR_DMA2RST_Field is Interfaces.STM32.Bit;
   subtype AHB1RSTR_DMAMUX1RST_Field is Interfaces.STM32.Bit;
   subtype AHB1RSTR_CORDICRST_Field is Interfaces.STM32.Bit;
   subtype AHB1RSTR_FMACRST_Field is Interfaces.STM32.Bit;
   subtype AHB1RSTR_FLASHRST_Field is Interfaces.STM32.Bit;
   subtype AHB1RSTR_CRCRST_Field is Interfaces.STM32.Bit;

   --  AHB1 peripheral reset register
   type AHB1RSTR_Register is record
      --  DMA1 reset
      DMA1RST        : AHB1RSTR_DMA1RST_Field := 16#0#;
      --  DMA2 reset
      DMA2RST        : AHB1RSTR_DMA2RST_Field := 16#0#;
      --  DMAMUXRST
      DMAMUX1RST     : AHB1RSTR_DMAMUX1RST_Field := 16#0#;
      --  CORDIC reset
      CORDICRST      : AHB1RSTR_CORDICRST_Field := 16#0#;
      --  FMAC reset
      FMACRST        : AHB1RSTR_FMACRST_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3 := 16#0#;
      --  FLASH reset
      FLASHRST       : AHB1RSTR_FLASHRST_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : Interfaces.STM32.UInt3 := 16#0#;
      --  CRC reset
      CRCRST         : AHB1RSTR_CRCRST_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.STM32.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB1RSTR_Register use record
      DMA1RST        at 0 range 0 .. 0;
      DMA2RST        at 0 range 1 .. 1;
      DMAMUX1RST     at 0 range 2 .. 2;
      CORDICRST      at 0 range 3 .. 3;
      FMACRST        at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      FLASHRST       at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      CRCRST         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype AHB2RSTR_GPIOARST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_GPIOBRST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_GPIOCRST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_GPIODRST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_GPIOERST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_GPIOFRST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_GPIOGRST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_ADC12RST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_ADC345RST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_DAC1RST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_DAC2RST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_DAC3RST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_DAC4RST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_AESRST_Field is Interfaces.STM32.Bit;
   subtype AHB2RSTR_RNGRST_Field is Interfaces.STM32.Bit;

   --  AHB2 peripheral reset register
   type AHB2RSTR_Register is record
      --  IO port A reset
      GPIOARST       : AHB2RSTR_GPIOARST_Field := 16#0#;
      --  IO port B reset
      GPIOBRST       : AHB2RSTR_GPIOBRST_Field := 16#0#;
      --  IO port C reset
      GPIOCRST       : AHB2RSTR_GPIOCRST_Field := 16#0#;
      --  IO port D reset
      GPIODRST       : AHB2RSTR_GPIODRST_Field := 16#0#;
      --  IO port E reset
      GPIOERST       : AHB2RSTR_GPIOERST_Field := 16#0#;
      --  IO port F reset
      GPIOFRST       : AHB2RSTR_GPIOFRST_Field := 16#0#;
      --  IO port G reset
      GPIOGRST       : AHB2RSTR_GPIOGRST_Field := 16#0#;
      --  unspecified
      Reserved_7_12  : Interfaces.STM32.UInt6 := 16#0#;
      --  ADC reset
      ADC12RST       : AHB2RSTR_ADC12RST_Field := 16#0#;
      --  SAR ADC345 interface reset
      ADC345RST      : AHB2RSTR_ADC345RST_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.STM32.Bit := 16#0#;
      --  DAC1 interface reset
      DAC1RST        : AHB2RSTR_DAC1RST_Field := 16#0#;
      --  DAC2 interface reset
      DAC2RST        : AHB2RSTR_DAC2RST_Field := 16#0#;
      --  DAC3 interface reset
      DAC3RST        : AHB2RSTR_DAC3RST_Field := 16#0#;
      --  DAC4 interface reset
      DAC4RST        : AHB2RSTR_DAC4RST_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : Interfaces.STM32.UInt4 := 16#0#;
      --  Cryptography module reset
      AESRST         : AHB2RSTR_AESRST_Field := 16#0#;
      --  unspecified
      Reserved_25_25 : Interfaces.STM32.Bit := 16#0#;
      --  Random Number Generator module reset
      RNGRST         : AHB2RSTR_RNGRST_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : Interfaces.STM32.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB2RSTR_Register use record
      GPIOARST       at 0 range 0 .. 0;
      GPIOBRST       at 0 range 1 .. 1;
      GPIOCRST       at 0 range 2 .. 2;
      GPIODRST       at 0 range 3 .. 3;
      GPIOERST       at 0 range 4 .. 4;
      GPIOFRST       at 0 range 5 .. 5;
      GPIOGRST       at 0 range 6 .. 6;
      Reserved_7_12  at 0 range 7 .. 12;
      ADC12RST       at 0 range 13 .. 13;
      ADC345RST      at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DAC1RST        at 0 range 16 .. 16;
      DAC2RST        at 0 range 17 .. 17;
      DAC3RST        at 0 range 18 .. 18;
      DAC4RST        at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      AESRST         at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      RNGRST         at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype AHB3RSTR_FMCRST_Field is Interfaces.STM32.Bit;
   subtype AHB3RSTR_QSPIRST_Field is Interfaces.STM32.Bit;

   --  AHB3 peripheral reset register
   type AHB3RSTR_Register is record
      --  Flexible memory controller reset
      FMCRST        : AHB3RSTR_FMCRST_Field := 16#0#;
      --  unspecified
      Reserved_1_7  : Interfaces.STM32.UInt7 := 16#0#;
      --  Quad SPI 1 module reset
      QSPIRST       : AHB3RSTR_QSPIRST_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3RSTR_Register use record
      FMCRST        at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      QSPIRST       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype APB1RSTR1_TIM2RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_TIM3RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_TIM4RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_TIM5RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_TIM6RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_TIM7RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_CRSRST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_SPI2RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_SPI3RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_USART2RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_USART3RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_UART4RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_UART5RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_I2C1RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_I2C2RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_USBDRST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_FDCANRST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_PWRRST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_I2C3RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR1_LPTIM1RST_Field is Interfaces.STM32.Bit;

   --  APB1 peripheral reset register 1
   type APB1RSTR1_Register is record
      --  TIM2 timer reset
      TIM2RST        : APB1RSTR1_TIM2RST_Field := 16#0#;
      --  TIM3 timer reset
      TIM3RST        : APB1RSTR1_TIM3RST_Field := 16#0#;
      --  TIM3 timer reset
      TIM4RST        : APB1RSTR1_TIM4RST_Field := 16#0#;
      --  TIM5 timer reset
      TIM5RST        : APB1RSTR1_TIM5RST_Field := 16#0#;
      --  TIM6 timer reset
      TIM6RST        : APB1RSTR1_TIM6RST_Field := 16#0#;
      --  TIM7 timer reset
      TIM7RST        : APB1RSTR1_TIM7RST_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : Interfaces.STM32.UInt2 := 16#0#;
      --  Clock recovery system reset
      CRSRST         : APB1RSTR1_CRSRST_Field := 16#0#;
      --  unspecified
      Reserved_9_13  : Interfaces.STM32.UInt5 := 16#0#;
      --  SPI2 reset
      SPI2RST        : APB1RSTR1_SPI2RST_Field := 16#0#;
      --  SPI3 reset
      SPI3RST        : APB1RSTR1_SPI3RST_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : Interfaces.STM32.Bit := 16#0#;
      --  USART2 reset
      USART2RST      : APB1RSTR1_USART2RST_Field := 16#0#;
      --  USART3 reset
      USART3RST      : APB1RSTR1_USART3RST_Field := 16#0#;
      --  UART4 reset
      UART4RST       : APB1RSTR1_UART4RST_Field := 16#0#;
      --  UART5 reset
      UART5RST       : APB1RSTR1_UART5RST_Field := 16#0#;
      --  I2C1 reset
      I2C1RST        : APB1RSTR1_I2C1RST_Field := 16#0#;
      --  I2C2 reset
      I2C2RST        : APB1RSTR1_I2C2RST_Field := 16#0#;
      --  USBD reset
      USBDRST        : APB1RSTR1_USBDRST_Field := 16#0#;
      --  unspecified
      Reserved_24_24 : Interfaces.STM32.Bit := 16#0#;
      --  FDCAN reset
      FDCANRST       : APB1RSTR1_FDCANRST_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : Interfaces.STM32.UInt2 := 16#0#;
      --  Power interface reset
      PWRRST         : APB1RSTR1_PWRRST_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : Interfaces.STM32.Bit := 16#0#;
      --  I2C3 interface reset
      I2C3RST        : APB1RSTR1_I2C3RST_Field := 16#0#;
      --  Low Power Timer 1 reset
      LPTIM1RST      : APB1RSTR1_LPTIM1RST_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1RSTR1_Register use record
      TIM2RST        at 0 range 0 .. 0;
      TIM3RST        at 0 range 1 .. 1;
      TIM4RST        at 0 range 2 .. 2;
      TIM5RST        at 0 range 3 .. 3;
      TIM6RST        at 0 range 4 .. 4;
      TIM7RST        at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      CRSRST         at 0 range 8 .. 8;
      Reserved_9_13  at 0 range 9 .. 13;
      SPI2RST        at 0 range 14 .. 14;
      SPI3RST        at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      USART2RST      at 0 range 17 .. 17;
      USART3RST      at 0 range 18 .. 18;
      UART4RST       at 0 range 19 .. 19;
      UART5RST       at 0 range 20 .. 20;
      I2C1RST        at 0 range 21 .. 21;
      I2C2RST        at 0 range 22 .. 22;
      USBDRST        at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      FDCANRST       at 0 range 25 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      PWRRST         at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      I2C3RST        at 0 range 30 .. 30;
      LPTIM1RST      at 0 range 31 .. 31;
   end record;

   subtype APB1RSTR2_LPUART1RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR2_I2C4RST_Field is Interfaces.STM32.Bit;
   subtype APB1RSTR2_USBPDRST_Field is Interfaces.STM32.Bit;

   --  APB1 peripheral reset register 2
   type APB1RSTR2_Register is record
      --  Low-power UART 1 reset
      LPUART1RST    : APB1RSTR2_LPUART1RST_Field := 16#0#;
      --  I2C4 reset
      I2C4RST       : APB1RSTR2_I2C4RST_Field := 16#0#;
      --  unspecified
      Reserved_2_7  : Interfaces.STM32.UInt6 := 16#0#;
      --  USBPD reset
      USBPDRST      : APB1RSTR2_USBPDRST_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1RSTR2_Register use record
      LPUART1RST    at 0 range 0 .. 0;
      I2C4RST       at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      USBPDRST      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype APB2RSTR_SYSCFGRST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_TIM1RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_SPI1RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_TIM8RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_USART1RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_SPI4RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_TIM15RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_TIM16RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_TIM17RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_TIM20RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_SAI1RST_Field is Interfaces.STM32.Bit;
   subtype APB2RSTR_HRTIM1RST_Field is Interfaces.STM32.Bit;

   --  APB2 peripheral reset register
   type APB2RSTR_Register is record
      --  System configuration (SYSCFG) reset
      SYSCFGRST      : APB2RSTR_SYSCFGRST_Field := 16#0#;
      --  unspecified
      Reserved_1_10  : Interfaces.STM32.UInt10 := 16#0#;
      --  TIM1 timer reset
      TIM1RST        : APB2RSTR_TIM1RST_Field := 16#0#;
      --  SPI1 reset
      SPI1RST        : APB2RSTR_SPI1RST_Field := 16#0#;
      --  TIM8 timer reset
      TIM8RST        : APB2RSTR_TIM8RST_Field := 16#0#;
      --  USART1 reset
      USART1RST      : APB2RSTR_USART1RST_Field := 16#0#;
      --  SPI 4 reset
      SPI4RST        : APB2RSTR_SPI4RST_Field := 16#0#;
      --  TIM15 timer reset
      TIM15RST       : APB2RSTR_TIM15RST_Field := 16#0#;
      --  TIM16 timer reset
      TIM16RST       : APB2RSTR_TIM16RST_Field := 16#0#;
      --  TIM17 timer reset
      TIM17RST       : APB2RSTR_TIM17RST_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : Interfaces.STM32.Bit := 16#0#;
      --  Timer 20 reset
      TIM20RST       : APB2RSTR_TIM20RST_Field := 16#0#;
      --  Serial audio interface 1 (SAI1) reset
      SAI1RST        : APB2RSTR_SAI1RST_Field := 16#0#;
      --  unspecified
      Reserved_22_25 : Interfaces.STM32.UInt4 := 16#0#;
      --  HRTIMER reset
      HRTIM1RST      : APB2RSTR_HRTIM1RST_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : Interfaces.STM32.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2RSTR_Register use record
      SYSCFGRST      at 0 range 0 .. 0;
      Reserved_1_10  at 0 range 1 .. 10;
      TIM1RST        at 0 range 11 .. 11;
      SPI1RST        at 0 range 12 .. 12;
      TIM8RST        at 0 range 13 .. 13;
      USART1RST      at 0 range 14 .. 14;
      SPI4RST        at 0 range 15 .. 15;
      TIM15RST       at 0 range 16 .. 16;
      TIM16RST       at 0 range 17 .. 17;
      TIM17RST       at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      TIM20RST       at 0 range 20 .. 20;
      SAI1RST        at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      HRTIM1RST      at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype AHB1ENR_DMA1EN_Field is Interfaces.STM32.Bit;
   subtype AHB1ENR_DMA2EN_Field is Interfaces.STM32.Bit;
   subtype AHB1ENR_DMAMUXEN_Field is Interfaces.STM32.Bit;
   subtype AHB1ENR_CORDICEN_Field is Interfaces.STM32.Bit;
   subtype AHB1ENR_FMACEN_Field is Interfaces.STM32.Bit;
   subtype AHB1ENR_FLITFEN_Field is Interfaces.STM32.Bit;
   subtype AHB1ENR_CRCEN_Field is Interfaces.STM32.Bit;

   --  AHB1 peripheral clock enable register
   type AHB1ENR_Register is record
      --  DMA1 clock enable
      DMA1EN         : AHB1ENR_DMA1EN_Field := 16#0#;
      --  DMA2 clock enable
      DMA2EN         : AHB1ENR_DMA2EN_Field := 16#0#;
      --  DMAMUX clock enable
      DMAMUXEN       : AHB1ENR_DMAMUXEN_Field := 16#0#;
      --  CORDIC clock enable
      CORDICEN       : AHB1ENR_CORDICEN_Field := 16#0#;
      --  FMAC clock enable
      FMACEN         : AHB1ENR_FMACEN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3 := 16#0#;
      --  FLITF clock enable
      FLITFEN        : AHB1ENR_FLITFEN_Field := 16#1#;
      --  unspecified
      Reserved_9_11  : Interfaces.STM32.UInt3 := 16#0#;
      --  CRC clock enable
      CRCEN          : AHB1ENR_CRCEN_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.STM32.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB1ENR_Register use record
      DMA1EN         at 0 range 0 .. 0;
      DMA2EN         at 0 range 1 .. 1;
      DMAMUXEN       at 0 range 2 .. 2;
      CORDICEN       at 0 range 3 .. 3;
      FMACEN         at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      FLITFEN        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      CRCEN          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype AHB2ENR_GPIOAEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_GPIOBEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_GPIOCEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_GPIODEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_GPIOEEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_GPIOFEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_GPIOGEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_ADC12EN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_ADC345EN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_DAC1EN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_DAC2EN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_DAC3EN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_DAC4EN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_CRYPTEN_Field is Interfaces.STM32.Bit;
   subtype AHB2ENR_RNGEN_Field is Interfaces.STM32.Bit;

   --  AHB2 peripheral clock enable register
   type AHB2ENR_Register is record
      --  IO port A clock enable
      GPIOAEN        : AHB2ENR_GPIOAEN_Field := 16#0#;
      --  IO port B clock enable
      GPIOBEN        : AHB2ENR_GPIOBEN_Field := 16#0#;
      --  IO port C clock enable
      GPIOCEN        : AHB2ENR_GPIOCEN_Field := 16#0#;
      --  IO port D clock enable
      GPIODEN        : AHB2ENR_GPIODEN_Field := 16#0#;
      --  IO port E clock enable
      GPIOEEN        : AHB2ENR_GPIOEEN_Field := 16#0#;
      --  IO port F clock enable
      GPIOFEN        : AHB2ENR_GPIOFEN_Field := 16#0#;
      --  IO port G clock enable
      GPIOGEN        : AHB2ENR_GPIOGEN_Field := 16#0#;
      --  unspecified
      Reserved_7_12  : Interfaces.STM32.UInt6 := 16#0#;
      --  ADC12 clock enable
      ADC12EN        : AHB2ENR_ADC12EN_Field := 16#0#;
      --  ADC345 clock enable
      ADC345EN       : AHB2ENR_ADC345EN_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.STM32.Bit := 16#0#;
      --  DAC1 clock enable
      DAC1EN         : AHB2ENR_DAC1EN_Field := 16#0#;
      --  DAC2 clock enable
      DAC2EN         : AHB2ENR_DAC2EN_Field := 16#0#;
      --  DAC3 clock enable
      DAC3EN         : AHB2ENR_DAC3EN_Field := 16#0#;
      --  DAC4 clock enable
      DAC4EN         : AHB2ENR_DAC4EN_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : Interfaces.STM32.UInt4 := 16#0#;
      --  Cryptography clock enable
      CRYPTEN        : AHB2ENR_CRYPTEN_Field := 16#0#;
      --  unspecified
      Reserved_25_25 : Interfaces.STM32.Bit := 16#0#;
      --  Random Number Generator clock enable
      RNGEN          : AHB2ENR_RNGEN_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : Interfaces.STM32.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB2ENR_Register use record
      GPIOAEN        at 0 range 0 .. 0;
      GPIOBEN        at 0 range 1 .. 1;
      GPIOCEN        at 0 range 2 .. 2;
      GPIODEN        at 0 range 3 .. 3;
      GPIOEEN        at 0 range 4 .. 4;
      GPIOFEN        at 0 range 5 .. 5;
      GPIOGEN        at 0 range 6 .. 6;
      Reserved_7_12  at 0 range 7 .. 12;
      ADC12EN        at 0 range 13 .. 13;
      ADC345EN       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DAC1EN         at 0 range 16 .. 16;
      DAC2EN         at 0 range 17 .. 17;
      DAC3EN         at 0 range 18 .. 18;
      DAC4EN         at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CRYPTEN        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      RNGEN          at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype AHB3ENR_FMCEN_Field is Interfaces.STM32.Bit;
   subtype AHB3ENR_QSPIEN_Field is Interfaces.STM32.Bit;

   --  AHB3 peripheral clock enable register
   type AHB3ENR_Register is record
      --  Flexible memory controller clock enable
      FMCEN         : AHB3ENR_FMCEN_Field := 16#0#;
      --  unspecified
      Reserved_1_7  : Interfaces.STM32.UInt7 := 16#0#;
      --  Quad SPI 1 module clock enable
      QSPIEN        : AHB3ENR_QSPIEN_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3ENR_Register use record
      FMCEN         at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      QSPIEN        at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype APB1ENR1_TIM2EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_TIM3EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_TIM4EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_TIM5EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_TIM6EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_TIM7EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_CRSEN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_RTCAPBEN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_WWDGEN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_SPI2EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_SPI3EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_USART2EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_USART3EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_UART4EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_UART5EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_I2C1EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_I2C2EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_USBDEN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_FDCANEN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_PWREN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_I2C3EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR1_LPTIM1EN_Field is Interfaces.STM32.Bit;

   --  APB1ENR1
   type APB1ENR1_Register is record
      --  TIM2 timer clock enable
      TIM2EN         : APB1ENR1_TIM2EN_Field := 16#0#;
      --  TIM3 timer clock enable
      TIM3EN         : APB1ENR1_TIM3EN_Field := 16#0#;
      --  TIM4 timer clock enable
      TIM4EN         : APB1ENR1_TIM4EN_Field := 16#0#;
      --  TIM5 timer clock enable
      TIM5EN         : APB1ENR1_TIM5EN_Field := 16#0#;
      --  TIM6 timer clock enable
      TIM6EN         : APB1ENR1_TIM6EN_Field := 16#0#;
      --  TIM7 timer clock enable
      TIM7EN         : APB1ENR1_TIM7EN_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : Interfaces.STM32.UInt2 := 16#0#;
      --  CRSclock enable
      CRSEN          : APB1ENR1_CRSEN_Field := 16#0#;
      --  unspecified
      Reserved_9_9   : Interfaces.STM32.Bit := 16#0#;
      --  RTC APB clock enable
      RTCAPBEN       : APB1ENR1_RTCAPBEN_Field := 16#0#;
      --  Window watchdog clock enable
      WWDGEN         : APB1ENR1_WWDGEN_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : Interfaces.STM32.UInt2 := 16#0#;
      --  SPI2 clock enable
      SPI2EN         : APB1ENR1_SPI2EN_Field := 16#0#;
      --  SPI3 clock enable
      SPI3EN         : APB1ENR1_SPI3EN_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : Interfaces.STM32.Bit := 16#0#;
      --  USART2 clock enable
      USART2EN       : APB1ENR1_USART2EN_Field := 16#0#;
      --  USART3 clock enable
      USART3EN       : APB1ENR1_USART3EN_Field := 16#0#;
      --  UART4 clock enable
      UART4EN        : APB1ENR1_UART4EN_Field := 16#0#;
      --  UART5 clock enable
      UART5EN        : APB1ENR1_UART5EN_Field := 16#0#;
      --  I2C1 clock enable
      I2C1EN         : APB1ENR1_I2C1EN_Field := 16#0#;
      --  I2C2 clock enable
      I2C2EN         : APB1ENR1_I2C2EN_Field := 16#0#;
      --  USBDclock enable
      USBDEN         : APB1ENR1_USBDEN_Field := 16#0#;
      --  unspecified
      Reserved_24_24 : Interfaces.STM32.Bit := 16#0#;
      --  FDCAN clock enable
      FDCANEN        : APB1ENR1_FDCANEN_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : Interfaces.STM32.UInt2 := 16#0#;
      --  Power interface clock enable
      PWREN          : APB1ENR1_PWREN_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : Interfaces.STM32.Bit := 16#0#;
      --  I2C3 clock enable
      I2C3EN         : APB1ENR1_I2C3EN_Field := 16#0#;
      --  Low power timer 1 clock enable
      LPTIM1EN       : APB1ENR1_LPTIM1EN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1ENR1_Register use record
      TIM2EN         at 0 range 0 .. 0;
      TIM3EN         at 0 range 1 .. 1;
      TIM4EN         at 0 range 2 .. 2;
      TIM5EN         at 0 range 3 .. 3;
      TIM6EN         at 0 range 4 .. 4;
      TIM7EN         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      CRSEN          at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      RTCAPBEN       at 0 range 10 .. 10;
      WWDGEN         at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      SPI2EN         at 0 range 14 .. 14;
      SPI3EN         at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      USART2EN       at 0 range 17 .. 17;
      USART3EN       at 0 range 18 .. 18;
      UART4EN        at 0 range 19 .. 19;
      UART5EN        at 0 range 20 .. 20;
      I2C1EN         at 0 range 21 .. 21;
      I2C2EN         at 0 range 22 .. 22;
      USBDEN         at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      FDCANEN        at 0 range 25 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      PWREN          at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      I2C3EN         at 0 range 30 .. 30;
      LPTIM1EN       at 0 range 31 .. 31;
   end record;

   subtype APB1ENR2_LPUART1EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR2_I2C4EN_Field is Interfaces.STM32.Bit;
   subtype APB1ENR2_USBPDEN_Field is Interfaces.STM32.Bit;

   --  APB1 peripheral clock enable register 2
   type APB1ENR2_Register is record
      --  Low power UART 1 clock enable
      LPUART1EN     : APB1ENR2_LPUART1EN_Field := 16#0#;
      --  I2C4 clock enable
      I2C4EN        : APB1ENR2_I2C4EN_Field := 16#0#;
      --  unspecified
      Reserved_2_7  : Interfaces.STM32.UInt6 := 16#0#;
      --  USBPD clock enable
      USBPDEN       : APB1ENR2_USBPDEN_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1ENR2_Register use record
      LPUART1EN     at 0 range 0 .. 0;
      I2C4EN        at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      USBPDEN       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype APB2ENR_SYSCFGEN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_TIM1EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_SPI1EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_TIM8EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_USART1EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_SPI4EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_TIM15EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_TIM16EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_TIM17EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_TIM20EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_SAI1EN_Field is Interfaces.STM32.Bit;
   subtype APB2ENR_HRTIM1EN_Field is Interfaces.STM32.Bit;

   --  APB2ENR
   type APB2ENR_Register is record
      --  SYSCFG clock enable
      SYSCFGEN       : APB2ENR_SYSCFGEN_Field := 16#0#;
      --  unspecified
      Reserved_1_10  : Interfaces.STM32.UInt10 := 16#0#;
      --  TIM1 timer clock enable
      TIM1EN         : APB2ENR_TIM1EN_Field := 16#0#;
      --  SPI1 clock enable
      SPI1EN         : APB2ENR_SPI1EN_Field := 16#0#;
      --  TIM8 timer clock enable
      TIM8EN         : APB2ENR_TIM8EN_Field := 16#0#;
      --  USART1clock enable
      USART1EN       : APB2ENR_USART1EN_Field := 16#0#;
      --  SPI 4 clock enable
      SPI4EN         : APB2ENR_SPI4EN_Field := 16#0#;
      --  TIM15 timer clock enable
      TIM15EN        : APB2ENR_TIM15EN_Field := 16#0#;
      --  TIM16 timer clock enable
      TIM16EN        : APB2ENR_TIM16EN_Field := 16#0#;
      --  TIM17 timer clock enable
      TIM17EN        : APB2ENR_TIM17EN_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : Interfaces.STM32.Bit := 16#0#;
      --  Timer 20 clock enable
      TIM20EN        : APB2ENR_TIM20EN_Field := 16#0#;
      --  SAI1 clock enable
      SAI1EN         : APB2ENR_SAI1EN_Field := 16#0#;
      --  unspecified
      Reserved_22_25 : Interfaces.STM32.UInt4 := 16#0#;
      --  HRTIMER clock enable
      HRTIM1EN       : APB2ENR_HRTIM1EN_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : Interfaces.STM32.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2ENR_Register use record
      SYSCFGEN       at 0 range 0 .. 0;
      Reserved_1_10  at 0 range 1 .. 10;
      TIM1EN         at 0 range 11 .. 11;
      SPI1EN         at 0 range 12 .. 12;
      TIM8EN         at 0 range 13 .. 13;
      USART1EN       at 0 range 14 .. 14;
      SPI4EN         at 0 range 15 .. 15;
      TIM15EN        at 0 range 16 .. 16;
      TIM16EN        at 0 range 17 .. 17;
      TIM17EN        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      TIM20EN        at 0 range 20 .. 20;
      SAI1EN         at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      HRTIM1EN       at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype AHB1SMENR_DMA1SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_DMA2SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_DMAMUX1SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_CORDICSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_FMACSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_FLASHSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_SRAM1SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB1SMENR_CRCSMEN_Field is Interfaces.STM32.Bit;

   --  AHB1 peripheral clocks enable in Sleep and Stop modes register
   type AHB1SMENR_Register is record
      --  DMA1 clocks enable during Sleep and Stop modes
      DMA1SMEN       : AHB1SMENR_DMA1SMEN_Field := 16#1#;
      --  DMA2 clocks enable during Sleep and Stop modes
      DMA2SMEN       : AHB1SMENR_DMA2SMEN_Field := 16#1#;
      --  DMAMUX clock enable during Sleep and Stop modes
      DMAMUX1SMEN    : AHB1SMENR_DMAMUX1SMEN_Field := 16#1#;
      --  CORDIC clock enable during sleep mode
      CORDICSMEN     : AHB1SMENR_CORDICSMEN_Field := 16#1#;
      --  FMACSM clock enable
      FMACSMEN       : AHB1SMENR_FMACSMEN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3 := 16#0#;
      --  Flash memory interface clocks enable during Sleep and Stop modes
      FLASHSMEN      : AHB1SMENR_FLASHSMEN_Field := 16#1#;
      --  SRAM1 interface clocks enable during Sleep and Stop modes
      SRAM1SMEN      : AHB1SMENR_SRAM1SMEN_Field := 16#1#;
      --  unspecified
      Reserved_10_11 : Interfaces.STM32.UInt2 := 16#0#;
      --  CRCSMEN
      CRCSMEN        : AHB1SMENR_CRCSMEN_Field := 16#1#;
      --  unspecified
      Reserved_13_31 : Interfaces.STM32.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB1SMENR_Register use record
      DMA1SMEN       at 0 range 0 .. 0;
      DMA2SMEN       at 0 range 1 .. 1;
      DMAMUX1SMEN    at 0 range 2 .. 2;
      CORDICSMEN     at 0 range 3 .. 3;
      FMACSMEN       at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      FLASHSMEN      at 0 range 8 .. 8;
      SRAM1SMEN      at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      CRCSMEN        at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype AHB2SMENR_GPIOASMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_GPIOBSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_GPIOCSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_GPIODSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_GPIOESMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_GPIOFSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_GPIOGSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_SRAM2SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_SRAM3SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_AD12CSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_ADC345SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_DAC1SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_DAC2SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_DAC3SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_DAC4SMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_CRYPTSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB2SMENR_RNGSMEN_Field is Interfaces.STM32.Bit;

   --  AHB2 peripheral clocks enable in Sleep and Stop modes register
   type AHB2SMENR_Register is record
      --  IO port A clocks enable during Sleep and Stop modes
      GPIOASMEN      : AHB2SMENR_GPIOASMEN_Field := 16#1#;
      --  IO port B clocks enable during Sleep and Stop modes
      GPIOBSMEN      : AHB2SMENR_GPIOBSMEN_Field := 16#1#;
      --  IO port C clocks enable during Sleep and Stop modes
      GPIOCSMEN      : AHB2SMENR_GPIOCSMEN_Field := 16#1#;
      --  IO port D clocks enable during Sleep and Stop modes
      GPIODSMEN      : AHB2SMENR_GPIODSMEN_Field := 16#1#;
      --  IO port E clocks enable during Sleep and Stop modes
      GPIOESMEN      : AHB2SMENR_GPIOESMEN_Field := 16#1#;
      --  IO port F clocks enable during Sleep and Stop modes
      GPIOFSMEN      : AHB2SMENR_GPIOFSMEN_Field := 16#1#;
      --  IO port G clocks enable during Sleep and Stop modes
      GPIOGSMEN      : AHB2SMENR_GPIOGSMEN_Field := 16#1#;
      --  unspecified
      Reserved_7_8   : Interfaces.STM32.UInt2 := 16#0#;
      --  SRAM2 interface clocks enable during Sleep and Stop modes
      SRAM2SMEN      : AHB2SMENR_SRAM2SMEN_Field := 16#1#;
      --  SRAM2 interface clocks enable during Sleep and Stop modes
      SRAM3SMEN      : AHB2SMENR_SRAM3SMEN_Field := 16#1#;
      --  unspecified
      Reserved_11_12 : Interfaces.STM32.UInt2 := 16#0#;
      --  ADC clocks enable during Sleep and Stop modes
      AD12CSMEN      : AHB2SMENR_AD12CSMEN_Field := 16#1#;
      --  DCMI clock enable during Sleep and Stop modes
      ADC345SMEN     : AHB2SMENR_ADC345SMEN_Field := 16#1#;
      --  unspecified
      Reserved_15_15 : Interfaces.STM32.Bit := 16#0#;
      --  AES accelerator clocks enable during Sleep and Stop modes
      DAC1SMEN       : AHB2SMENR_DAC1SMEN_Field := 16#1#;
      --  HASH clock enable during Sleep and Stop modes
      DAC2SMEN       : AHB2SMENR_DAC2SMEN_Field := 16#1#;
      --  DAC3 clock enable during sleep mode
      DAC3SMEN       : AHB2SMENR_DAC3SMEN_Field := 16#1#;
      --  DAC4 clock enable during sleep mode
      DAC4SMEN       : AHB2SMENR_DAC4SMEN_Field := 16#1#;
      --  unspecified
      Reserved_20_23 : Interfaces.STM32.UInt4 := 16#0#;
      --  Cryptography clock enable during sleep mode
      CRYPTSMEN      : AHB2SMENR_CRYPTSMEN_Field := 16#1#;
      --  unspecified
      Reserved_25_25 : Interfaces.STM32.Bit := 16#0#;
      --  Random Number Generator clock enable during sleep mode
      RNGSMEN        : AHB2SMENR_RNGSMEN_Field := 16#1#;
      --  unspecified
      Reserved_27_31 : Interfaces.STM32.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB2SMENR_Register use record
      GPIOASMEN      at 0 range 0 .. 0;
      GPIOBSMEN      at 0 range 1 .. 1;
      GPIOCSMEN      at 0 range 2 .. 2;
      GPIODSMEN      at 0 range 3 .. 3;
      GPIOESMEN      at 0 range 4 .. 4;
      GPIOFSMEN      at 0 range 5 .. 5;
      GPIOGSMEN      at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SRAM2SMEN      at 0 range 9 .. 9;
      SRAM3SMEN      at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      AD12CSMEN      at 0 range 13 .. 13;
      ADC345SMEN     at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DAC1SMEN       at 0 range 16 .. 16;
      DAC2SMEN       at 0 range 17 .. 17;
      DAC3SMEN       at 0 range 18 .. 18;
      DAC4SMEN       at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      CRYPTSMEN      at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      RNGSMEN        at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype AHB3SMENR_FMCSMEN_Field is Interfaces.STM32.Bit;
   subtype AHB3SMENR_QSPISMEN_Field is Interfaces.STM32.Bit;

   --  AHB3 peripheral clocks enable in Sleep and Stop modes register
   type AHB3SMENR_Register is record
      --  Flexible memory controller clocks enable during Sleep and Stop modes
      FMCSMEN       : AHB3SMENR_FMCSMEN_Field := 16#1#;
      --  unspecified
      Reserved_1_7  : Interfaces.STM32.UInt7 := 16#0#;
      --  QUAD SPI 1 module clock enable during sleep mode
      QSPISMEN      : AHB3SMENR_QSPISMEN_Field := 16#1#;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3SMENR_Register use record
      FMCSMEN       at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      QSPISMEN      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype APB1SMENR1_TIM2SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_TIM3SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_TIM4SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_TIM5SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_TIM6SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_TIM7SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_CRSSMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_RTCAPBSMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_WWDGSMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_SPI2SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_SP3SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_USART2SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_USART3SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_UART4SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_UART5SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_I2C1SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_I2C2SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_I2C3SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_FDCANSMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_PWRSMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_I2C3SMEN_3_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR1_LPTIM1SMEN_Field is Interfaces.STM32.Bit;

   --  APB1SMENR1
   type APB1SMENR1_Register is record
      --  TIM2 timer clocks enable during Sleep and Stop modes
      TIM2SMEN       : APB1SMENR1_TIM2SMEN_Field := 16#1#;
      --  TIM3 timer clocks enable during Sleep and Stop modes
      TIM3SMEN       : APB1SMENR1_TIM3SMEN_Field := 16#1#;
      --  TIM4 timer clocks enable during Sleep and Stop modes
      TIM4SMEN       : APB1SMENR1_TIM4SMEN_Field := 16#1#;
      --  TIM5 timer clocks enable during Sleep and Stop modes
      TIM5SMEN       : APB1SMENR1_TIM5SMEN_Field := 16#1#;
      --  TIM6 timer clocks enable during Sleep and Stop modes
      TIM6SMEN       : APB1SMENR1_TIM6SMEN_Field := 16#1#;
      --  TIM7 timer clocks enable during Sleep and Stop modes
      TIM7SMEN       : APB1SMENR1_TIM7SMEN_Field := 16#1#;
      --  unspecified
      Reserved_6_7   : Interfaces.STM32.UInt2 := 16#0#;
      --  CRS clock enable during sleep mode
      CRSSMEN        : APB1SMENR1_CRSSMEN_Field := 16#1#;
      --  unspecified
      Reserved_9_9   : Interfaces.STM32.Bit := 16#0#;
      --  RTC APB clock enable during Sleep and Stop modes
      RTCAPBSMEN     : APB1SMENR1_RTCAPBSMEN_Field := 16#1#;
      --  Window watchdog clocks enable during Sleep and Stop modes
      WWDGSMEN       : APB1SMENR1_WWDGSMEN_Field := 16#1#;
      --  unspecified
      Reserved_12_13 : Interfaces.STM32.UInt2 := 16#0#;
      --  SPI2 clocks enable during Sleep and Stop modes
      SPI2SMEN       : APB1SMENR1_SPI2SMEN_Field := 16#1#;
      --  SPI3 clocks enable during Sleep and Stop modes
      SP3SMEN        : APB1SMENR1_SP3SMEN_Field := 16#1#;
      --  unspecified
      Reserved_16_16 : Interfaces.STM32.Bit := 16#0#;
      --  USART2 clocks enable during Sleep and Stop modes
      USART2SMEN     : APB1SMENR1_USART2SMEN_Field := 16#1#;
      --  USART3 clocks enable during Sleep and Stop modes
      USART3SMEN     : APB1SMENR1_USART3SMEN_Field := 16#1#;
      --  UART4 clocks enable during Sleep and Stop modes
      UART4SMEN      : APB1SMENR1_UART4SMEN_Field := 16#1#;
      --  UART5 clocks enable during Sleep and Stop modes
      UART5SMEN      : APB1SMENR1_UART5SMEN_Field := 16#1#;
      --  I2C1 clocks enable during Sleep and Stop modes
      I2C1SMEN       : APB1SMENR1_I2C1SMEN_Field := 16#1#;
      --  I2C2 clocks enable during Sleep and Stop modes
      I2C2SMEN       : APB1SMENR1_I2C2SMEN_Field := 16#1#;
      --  I2C3 clocks enable during Sleep and Stop modes
      I2C3SMEN       : APB1SMENR1_I2C3SMEN_Field := 16#1#;
      --  unspecified
      Reserved_24_24 : Interfaces.STM32.Bit := 16#0#;
      --  FDCAN clock enable during sleep mode
      FDCANSMEN      : APB1SMENR1_FDCANSMEN_Field := 16#1#;
      --  unspecified
      Reserved_26_27 : Interfaces.STM32.UInt2 := 16#0#;
      --  Power interface clocks enable during Sleep and Stop modes
      PWRSMEN        : APB1SMENR1_PWRSMEN_Field := 16#1#;
      --  unspecified
      Reserved_29_29 : Interfaces.STM32.Bit := 16#0#;
      --  I2C 3 interface clock enable during sleep mode
      I2C3SMEN_3     : APB1SMENR1_I2C3SMEN_3_Field := 16#1#;
      --  Low Power Timer1 clock enable during sleep mode
      LPTIM1SMEN     : APB1SMENR1_LPTIM1SMEN_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1SMENR1_Register use record
      TIM2SMEN       at 0 range 0 .. 0;
      TIM3SMEN       at 0 range 1 .. 1;
      TIM4SMEN       at 0 range 2 .. 2;
      TIM5SMEN       at 0 range 3 .. 3;
      TIM6SMEN       at 0 range 4 .. 4;
      TIM7SMEN       at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      CRSSMEN        at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      RTCAPBSMEN     at 0 range 10 .. 10;
      WWDGSMEN       at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      SPI2SMEN       at 0 range 14 .. 14;
      SP3SMEN        at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      USART2SMEN     at 0 range 17 .. 17;
      USART3SMEN     at 0 range 18 .. 18;
      UART4SMEN      at 0 range 19 .. 19;
      UART5SMEN      at 0 range 20 .. 20;
      I2C1SMEN       at 0 range 21 .. 21;
      I2C2SMEN       at 0 range 22 .. 22;
      I2C3SMEN       at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      FDCANSMEN      at 0 range 25 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      PWRSMEN        at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      I2C3SMEN_3     at 0 range 30 .. 30;
      LPTIM1SMEN     at 0 range 31 .. 31;
   end record;

   subtype APB1SMENR2_LPUART1SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR2_I2C4SMEN_Field is Interfaces.STM32.Bit;
   subtype APB1SMENR2_USBPDSMEN_Field is Interfaces.STM32.Bit;

   --  APB1 peripheral clocks enable in Sleep and Stop modes register 2
   type APB1SMENR2_Register is record
      --  Low power UART 1 clocks enable during Sleep and Stop modes
      LPUART1SMEN   : APB1SMENR2_LPUART1SMEN_Field := 16#1#;
      --  I2C4 clocks enable during Sleep and Stop modes
      I2C4SMEN      : APB1SMENR2_I2C4SMEN_Field := 16#1#;
      --  unspecified
      Reserved_2_7  : Interfaces.STM32.UInt6 := 16#0#;
      --  USB PD clock enable during sleep mode
      USBPDSMEN     : APB1SMENR2_USBPDSMEN_Field := 16#1#;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1SMENR2_Register use record
      LPUART1SMEN   at 0 range 0 .. 0;
      I2C4SMEN      at 0 range 1 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      USBPDSMEN     at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype APB2SMENR_SYSCFGSMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_TIM1SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_SPI1SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_TIM8SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_USART1SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_SPI4SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_TIM15SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_TIM16SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_TIM17SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_TIM20SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_SAI1SMEN_Field is Interfaces.STM32.Bit;
   subtype APB2SMENR_HRTIMERSMEN_Field is Interfaces.STM32.Bit;

   --  APB2SMENR
   type APB2SMENR_Register is record
      --  SYSCFG clocks enable during Sleep and Stop modes
      SYSCFGSMEN     : APB2SMENR_SYSCFGSMEN_Field := 16#1#;
      --  unspecified
      Reserved_1_10  : Interfaces.STM32.UInt10 := 16#0#;
      --  TIM1 timer clocks enable during Sleep and Stop modes
      TIM1SMEN       : APB2SMENR_TIM1SMEN_Field := 16#1#;
      --  SPI1 clocks enable during Sleep and Stop modes
      SPI1SMEN       : APB2SMENR_SPI1SMEN_Field := 16#1#;
      --  TIM8 timer clocks enable during Sleep and Stop modes
      TIM8SMEN       : APB2SMENR_TIM8SMEN_Field := 16#1#;
      --  USART1clocks enable during Sleep and Stop modes
      USART1SMEN     : APB2SMENR_USART1SMEN_Field := 16#1#;
      --  SPI4 timer clocks enable during Sleep and Stop modes
      SPI4SMEN       : APB2SMENR_SPI4SMEN_Field := 16#1#;
      --  TIM15 timer clocks enable during Sleep and Stop modes
      TIM15SMEN      : APB2SMENR_TIM15SMEN_Field := 16#1#;
      --  TIM16 timer clocks enable during Sleep and Stop modes
      TIM16SMEN      : APB2SMENR_TIM16SMEN_Field := 16#1#;
      --  TIM17 timer clocks enable during Sleep and Stop modes
      TIM17SMEN      : APB2SMENR_TIM17SMEN_Field := 16#1#;
      --  unspecified
      Reserved_19_19 : Interfaces.STM32.Bit := 16#0#;
      --  Timer 20clock enable during sleep mode
      TIM20SMEN      : APB2SMENR_TIM20SMEN_Field := 16#1#;
      --  SAI1 clock enable during sleep mode
      SAI1SMEN       : APB2SMENR_SAI1SMEN_Field := 16#1#;
      --  unspecified
      Reserved_22_25 : Interfaces.STM32.UInt4 := 16#0#;
      --  HRTIMER clock enable during sleep mode
      HRTIMERSMEN    : APB2SMENR_HRTIMERSMEN_Field := 16#1#;
      --  unspecified
      Reserved_27_31 : Interfaces.STM32.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2SMENR_Register use record
      SYSCFGSMEN     at 0 range 0 .. 0;
      Reserved_1_10  at 0 range 1 .. 10;
      TIM1SMEN       at 0 range 11 .. 11;
      SPI1SMEN       at 0 range 12 .. 12;
      TIM8SMEN       at 0 range 13 .. 13;
      USART1SMEN     at 0 range 14 .. 14;
      SPI4SMEN       at 0 range 15 .. 15;
      TIM15SMEN      at 0 range 16 .. 16;
      TIM16SMEN      at 0 range 17 .. 17;
      TIM17SMEN      at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      TIM20SMEN      at 0 range 20 .. 20;
      SAI1SMEN       at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      HRTIMERSMEN    at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype CCIPR_USART1SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_USART2SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_USART3SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_UART4SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_UART5SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_LPUART1SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_I2C1SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_I2C2SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_I2C3SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_LPTIM1SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_SAI1SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_I2S23SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_FDCANSEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_CLK48SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_ADC12SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR_ADC345SEL_Field is Interfaces.STM32.UInt2;

   --  CCIPR
   type CCIPR_Register is record
      --  USART1 clock source selection
      USART1SEL  : CCIPR_USART1SEL_Field := 16#0#;
      --  USART2 clock source selection
      USART2SEL  : CCIPR_USART2SEL_Field := 16#0#;
      --  USART3 clock source selection
      USART3SEL  : CCIPR_USART3SEL_Field := 16#0#;
      --  UART4 clock source selection
      UART4SEL   : CCIPR_UART4SEL_Field := 16#0#;
      --  UART5 clock source selection
      UART5SEL   : CCIPR_UART5SEL_Field := 16#0#;
      --  LPUART1 clock source selection
      LPUART1SEL : CCIPR_LPUART1SEL_Field := 16#0#;
      --  I2C1 clock source selection
      I2C1SEL    : CCIPR_I2C1SEL_Field := 16#0#;
      --  I2C2 clock source selection
      I2C2SEL    : CCIPR_I2C2SEL_Field := 16#0#;
      --  I2C3 clock source selection
      I2C3SEL    : CCIPR_I2C3SEL_Field := 16#0#;
      --  Low power timer 1 clock source selection
      LPTIM1SEL  : CCIPR_LPTIM1SEL_Field := 16#0#;
      --  SAI1 clock source selection
      SAI1SEL    : CCIPR_SAI1SEL_Field := 16#0#;
      --  I2S clock source selection
      I2S23SEL   : CCIPR_I2S23SEL_Field := 16#0#;
      --  SAI2 clock source selection
      FDCANSEL   : CCIPR_FDCANSEL_Field := 16#0#;
      --  48 MHz clock source selection
      CLK48SEL   : CCIPR_CLK48SEL_Field := 16#0#;
      --  ADCs clock source selection
      ADC12SEL   : CCIPR_ADC12SEL_Field := 16#0#;
      --  ADC3/4/5 clock source selection
      ADC345SEL  : CCIPR_ADC345SEL_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCIPR_Register use record
      USART1SEL  at 0 range 0 .. 1;
      USART2SEL  at 0 range 2 .. 3;
      USART3SEL  at 0 range 4 .. 5;
      UART4SEL   at 0 range 6 .. 7;
      UART5SEL   at 0 range 8 .. 9;
      LPUART1SEL at 0 range 10 .. 11;
      I2C1SEL    at 0 range 12 .. 13;
      I2C2SEL    at 0 range 14 .. 15;
      I2C3SEL    at 0 range 16 .. 17;
      LPTIM1SEL  at 0 range 18 .. 19;
      SAI1SEL    at 0 range 20 .. 21;
      I2S23SEL   at 0 range 22 .. 23;
      FDCANSEL   at 0 range 24 .. 25;
      CLK48SEL   at 0 range 26 .. 27;
      ADC12SEL   at 0 range 28 .. 29;
      ADC345SEL  at 0 range 30 .. 31;
   end record;

   subtype BDCR_LSEON_Field is Interfaces.STM32.Bit;
   subtype BDCR_LSERDY_Field is Interfaces.STM32.Bit;
   subtype BDCR_LSEBYP_Field is Interfaces.STM32.Bit;
   subtype BDCR_LSEDRV_Field is Interfaces.STM32.UInt2;
   subtype BDCR_LSECSSON_Field is Interfaces.STM32.Bit;
   subtype BDCR_LSECSSD_Field is Interfaces.STM32.Bit;
   subtype BDCR_RTCSEL_Field is Interfaces.STM32.UInt2;
   subtype BDCR_RTCEN_Field is Interfaces.STM32.Bit;
   subtype BDCR_BDRST_Field is Interfaces.STM32.Bit;
   subtype BDCR_LSCOEN_Field is Interfaces.STM32.Bit;
   subtype BDCR_LSCOSEL_Field is Interfaces.STM32.Bit;

   --  BDCR
   type BDCR_Register is record
      --  LSE oscillator enable
      LSEON          : BDCR_LSEON_Field := 16#0#;
      --  Read-only. LSE oscillator ready
      LSERDY         : BDCR_LSERDY_Field := 16#0#;
      --  LSE oscillator bypass
      LSEBYP         : BDCR_LSEBYP_Field := 16#0#;
      --  SE oscillator drive capability
      LSEDRV         : BDCR_LSEDRV_Field := 16#0#;
      --  LSECSSON
      LSECSSON       : BDCR_LSECSSON_Field := 16#0#;
      --  Read-only. LSECSSD
      LSECSSD        : BDCR_LSECSSD_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.STM32.Bit := 16#0#;
      --  RTC clock source selection
      RTCSEL         : BDCR_RTCSEL_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : Interfaces.STM32.UInt5 := 16#0#;
      --  RTC clock enable
      RTCEN          : BDCR_RTCEN_Field := 16#0#;
      --  BDRST domain software reset
      BDRST          : BDCR_BDRST_Field := 16#0#;
      --  unspecified
      Reserved_17_23 : Interfaces.STM32.UInt7 := 16#0#;
      --  Low speed clock output enable
      LSCOEN         : BDCR_LSCOEN_Field := 16#0#;
      --  Low speed clock output selection
      LSCOSEL        : BDCR_LSCOSEL_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : Interfaces.STM32.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDCR_Register use record
      LSEON          at 0 range 0 .. 0;
      LSERDY         at 0 range 1 .. 1;
      LSEBYP         at 0 range 2 .. 2;
      LSEDRV         at 0 range 3 .. 4;
      LSECSSON       at 0 range 5 .. 5;
      LSECSSD        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      RTCSEL         at 0 range 8 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      RTCEN          at 0 range 15 .. 15;
      BDRST          at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      LSCOEN         at 0 range 24 .. 24;
      LSCOSEL        at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CSR_LSION_Field is Interfaces.STM32.Bit;
   subtype CSR_LSIRDY_Field is Interfaces.STM32.Bit;
   subtype CSR_RMVF_Field is Interfaces.STM32.Bit;
   subtype CSR_OBLRSTF_Field is Interfaces.STM32.Bit;
   subtype CSR_PADRSTF_Field is Interfaces.STM32.Bit;
   subtype CSR_BORRSTF_Field is Interfaces.STM32.Bit;
   subtype CSR_SFTRSTF_Field is Interfaces.STM32.Bit;
   subtype CSR_IWDGRSTF_Field is Interfaces.STM32.Bit;
   subtype CSR_WWDGRSTF_Field is Interfaces.STM32.Bit;
   subtype CSR_LPWRSTF_Field is Interfaces.STM32.Bit;

   --  CSR
   type CSR_Register is record
      --  LSI oscillator enable
      LSION          : CSR_LSION_Field := 16#0#;
      --  Read-only. LSI oscillator ready
      LSIRDY         : CSR_LSIRDY_Field := 16#0#;
      --  unspecified
      Reserved_2_22  : Interfaces.STM32.UInt21 := 16#0#;
      --  Remove reset flag
      RMVF           : CSR_RMVF_Field := 16#0#;
      --  unspecified
      Reserved_24_24 : Interfaces.STM32.Bit := 16#0#;
      --  Read-only. Option byte loader reset flag
      OBLRSTF        : CSR_OBLRSTF_Field := 16#0#;
      --  Read-only. Pad reset flag
      PADRSTF        : CSR_PADRSTF_Field := 16#1#;
      --  Read-only. BOR flag
      BORRSTF        : CSR_BORRSTF_Field := 16#1#;
      --  Read-only. Software reset flag
      SFTRSTF        : CSR_SFTRSTF_Field := 16#0#;
      --  Read-only. Independent window watchdog reset flag
      IWDGRSTF       : CSR_IWDGRSTF_Field := 16#0#;
      --  Read-only. Window watchdog reset flag
      WWDGRSTF       : CSR_WWDGRSTF_Field := 16#0#;
      --  Read-only. Low-power reset flag
      LPWRSTF        : CSR_LPWRSTF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      LSION          at 0 range 0 .. 0;
      LSIRDY         at 0 range 1 .. 1;
      Reserved_2_22  at 0 range 2 .. 22;
      RMVF           at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      OBLRSTF        at 0 range 25 .. 25;
      PADRSTF        at 0 range 26 .. 26;
      BORRSTF        at 0 range 27 .. 27;
      SFTRSTF        at 0 range 28 .. 28;
      IWDGRSTF       at 0 range 29 .. 29;
      WWDGRSTF       at 0 range 30 .. 30;
      LPWRSTF        at 0 range 31 .. 31;
   end record;

   subtype CRRCR_HSI48ON_Field is Interfaces.STM32.Bit;
   subtype CRRCR_HSI48RDY_Field is Interfaces.STM32.Bit;
   subtype CRRCR_HSI48CAL_Field is Interfaces.STM32.UInt9;

   --  Clock recovery RC register
   type CRRCR_Register is record
      --  HSI48 clock enable
      HSI48ON        : CRRCR_HSI48ON_Field := 16#0#;
      --  Read-only. HSI48 clock ready flag
      HSI48RDY       : CRRCR_HSI48RDY_Field := 16#0#;
      --  unspecified
      Reserved_2_6   : Interfaces.STM32.UInt5 := 16#0#;
      --  Read-only. HSI48 clock calibration
      HSI48CAL       : CRRCR_HSI48CAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRRCR_Register use record
      HSI48ON        at 0 range 0 .. 0;
      HSI48RDY       at 0 range 1 .. 1;
      Reserved_2_6   at 0 range 2 .. 6;
      HSI48CAL       at 0 range 7 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CCIPR2_I2C4SEL_Field is Interfaces.STM32.UInt2;
   subtype CCIPR2_QSPISEL_Field is Interfaces.STM32.UInt2;

   --  Peripherals independent clock configuration register
   type CCIPR2_Register is record
      --  I2C4 clock source selection
      I2C4SEL        : CCIPR2_I2C4SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_19  : Interfaces.STM32.UInt18 := 16#0#;
      --  Octospi clock source selection
      QSPISEL        : CCIPR2_QSPISEL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : Interfaces.STM32.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCIPR2_Register use record
      I2C4SEL        at 0 range 0 .. 1;
      Reserved_2_19  at 0 range 2 .. 19;
      QSPISEL        at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Reset and clock control
   type RCC_Peripheral is record
      --  Clock control register
      CR         : aliased CR_Register;
      --  Internal clock sources calibration register
      ICSCR      : aliased ICSCR_Register;
      --  Clock configuration register
      CFGR       : aliased CFGR_Register;
      --  PLL configuration register
      PLLCFGR    : aliased PLLCFGR_Register;
      --  Clock interrupt enable register
      CIER       : aliased CIER_Register;
      --  Clock interrupt flag register
      CIFR       : aliased CIFR_Register;
      --  Clock interrupt clear register
      CICR       : aliased CICR_Register;
      --  AHB1 peripheral reset register
      AHB1RSTR   : aliased AHB1RSTR_Register;
      --  AHB2 peripheral reset register
      AHB2RSTR   : aliased AHB2RSTR_Register;
      --  AHB3 peripheral reset register
      AHB3RSTR   : aliased AHB3RSTR_Register;
      --  APB1 peripheral reset register 1
      APB1RSTR1  : aliased APB1RSTR1_Register;
      --  APB1 peripheral reset register 2
      APB1RSTR2  : aliased APB1RSTR2_Register;
      --  APB2 peripheral reset register
      APB2RSTR   : aliased APB2RSTR_Register;
      --  AHB1 peripheral clock enable register
      AHB1ENR    : aliased AHB1ENR_Register;
      --  AHB2 peripheral clock enable register
      AHB2ENR    : aliased AHB2ENR_Register;
      --  AHB3 peripheral clock enable register
      AHB3ENR    : aliased AHB3ENR_Register;
      --  APB1ENR1
      APB1ENR1   : aliased APB1ENR1_Register;
      --  APB1 peripheral clock enable register 2
      APB1ENR2   : aliased APB1ENR2_Register;
      --  APB2ENR
      APB2ENR    : aliased APB2ENR_Register;
      --  AHB1 peripheral clocks enable in Sleep and Stop modes register
      AHB1SMENR  : aliased AHB1SMENR_Register;
      --  AHB2 peripheral clocks enable in Sleep and Stop modes register
      AHB2SMENR  : aliased AHB2SMENR_Register;
      --  AHB3 peripheral clocks enable in Sleep and Stop modes register
      AHB3SMENR  : aliased AHB3SMENR_Register;
      --  APB1SMENR1
      APB1SMENR1 : aliased APB1SMENR1_Register;
      --  APB1 peripheral clocks enable in Sleep and Stop modes register 2
      APB1SMENR2 : aliased APB1SMENR2_Register;
      --  APB2SMENR
      APB2SMENR  : aliased APB2SMENR_Register;
      --  CCIPR
      CCIPR      : aliased CCIPR_Register;
      --  BDCR
      BDCR       : aliased BDCR_Register;
      --  CSR
      CSR        : aliased CSR_Register;
      --  Clock recovery RC register
      CRRCR      : aliased CRRCR_Register;
      --  Peripherals independent clock configuration register
      CCIPR2     : aliased CCIPR2_Register;
   end record
     with Volatile;

   for RCC_Peripheral use record
      CR         at 16#0# range 0 .. 31;
      ICSCR      at 16#4# range 0 .. 31;
      CFGR       at 16#8# range 0 .. 31;
      PLLCFGR    at 16#C# range 0 .. 31;
      CIER       at 16#18# range 0 .. 31;
      CIFR       at 16#1C# range 0 .. 31;
      CICR       at 16#20# range 0 .. 31;
      AHB1RSTR   at 16#28# range 0 .. 31;
      AHB2RSTR   at 16#2C# range 0 .. 31;
      AHB3RSTR   at 16#30# range 0 .. 31;
      APB1RSTR1  at 16#38# range 0 .. 31;
      APB1RSTR2  at 16#3C# range 0 .. 31;
      APB2RSTR   at 16#40# range 0 .. 31;
      AHB1ENR    at 16#48# range 0 .. 31;
      AHB2ENR    at 16#4C# range 0 .. 31;
      AHB3ENR    at 16#50# range 0 .. 31;
      APB1ENR1   at 16#58# range 0 .. 31;
      APB1ENR2   at 16#5C# range 0 .. 31;
      APB2ENR    at 16#60# range 0 .. 31;
      AHB1SMENR  at 16#68# range 0 .. 31;
      AHB2SMENR  at 16#6C# range 0 .. 31;
      AHB3SMENR  at 16#70# range 0 .. 31;
      APB1SMENR1 at 16#78# range 0 .. 31;
      APB1SMENR2 at 16#7C# range 0 .. 31;
      APB2SMENR  at 16#80# range 0 .. 31;
      CCIPR      at 16#88# range 0 .. 31;
      BDCR       at 16#90# range 0 .. 31;
      CSR        at 16#94# range 0 .. 31;
      CRRCR      at 16#98# range 0 .. 31;
      CCIPR2     at 16#9C# range 0 .. 31;
   end record;

   --  Reset and clock control
   RCC_Periph : aliased RCC_Peripheral
     with Import, Address => RCC_Base;

end Interfaces.STM32.RCC;
