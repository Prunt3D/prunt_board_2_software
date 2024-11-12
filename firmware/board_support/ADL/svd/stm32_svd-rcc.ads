pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.RCC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Clock control register
   type CR_Register is record
      --  unspecified
      Reserved_0_7   : HAL.UInt8 := 16#63#;
      --  HSI clock enable
      HSION          : Boolean := False;
      --  HSI always enable for peripheral kernels
      HSIKERON       : Boolean := False;
      --  Read-only. HSI clock ready flag
      HSIRDY         : Boolean := False;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  HSE clock enable
      HSEON          : Boolean := False;
      --  Read-only. HSE clock ready flag
      HSERDY         : Boolean := False;
      --  HSE crystal oscillator bypass
      HSEBYP         : Boolean := False;
      --  Write-only. Clock security system enable
      CSSON          : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Main PLL enable
      PLLON          : Boolean := False;
      --  Read-only. Main PLL clock ready flag
      PLLRDY         : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
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

   subtype ICSCR_HSICAL0_Field is HAL.UInt8;
   subtype ICSCR_HSITRIM_Field is HAL.UInt7;

   --  Internal clock sources calibration register
   type ICSCR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Read-only. Internal High Speed clock Calibration
      HSICAL0        : ICSCR_HSICAL0_Field := 16#0#;
      --  Internal High Speed clock trimming
      HSITRIM        : ICSCR_HSITRIM_Field := 16#40#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICSCR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      HSICAL0        at 0 range 16 .. 23;
      HSITRIM        at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CFGR_SW_Field is HAL.UInt2;
   subtype CFGR_SWS_Field is HAL.UInt2;
   subtype CFGR_HPRE_Field is HAL.UInt4;
   --  CFGR_PPRE array element
   subtype CFGR_PPRE_Element is HAL.UInt3;

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
            Val : HAL.UInt6;
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

   subtype CFGR_MCOSEL_Field is HAL.UInt4;
   subtype CFGR_MCOPRE_Field is HAL.UInt3;

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
      Reserved_14_23 : HAL.UInt10 := 16#0#;
      --  Microcontroller clock output
      MCOSEL         : CFGR_MCOSEL_Field := 16#0#;
      --  Microcontroller clock output prescaler
      MCOPRE         : CFGR_MCOPRE_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
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

   subtype PLLCFGR_PLLSRC_Field is HAL.UInt2;
   subtype PLLCFGR_PLLM_Field is HAL.UInt4;
   subtype PLLCFGR_PLLN_Field is HAL.UInt7;
   subtype PLLCFGR_PLLQ_Field is HAL.UInt2;
   subtype PLLCFGR_PLLR_Field is HAL.UInt2;
   subtype PLLCFGR_PLLPDIV_Field is HAL.UInt5;

   --  PLL configuration register
   type PLLCFGR_Register is record
      --  Main PLL, PLLSAI1 and PLLSAI2 entry clock source
      PLLSRC         : PLLCFGR_PLLSRC_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Division factor for the main PLL and audio PLL (PLLSAI1 and PLLSAI2)
      --  input clock
      PLLM           : PLLCFGR_PLLM_Field := 16#0#;
      --  Main PLL multiplication factor for VCO
      PLLN           : PLLCFGR_PLLN_Field := 16#10#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Main PLL PLLSAI3CLK output enable
      PLLPEN         : Boolean := False;
      --  Main PLL division factor for PLLSAI3CLK (SAI1 and SAI2 clock)
      PLLP           : Boolean := False;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Main PLL PLLUSB1CLK output enable
      PLLQEN         : Boolean := False;
      --  Main PLL division factor for PLLUSB1CLK(48 MHz clock)
      PLLQ           : PLLCFGR_PLLQ_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Main PLL PLLCLK output enable
      PLLREN         : Boolean := False;
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

   --  Clock interrupt enable register
   type CIER_Register is record
      --  LSI ready interrupt enable
      LSIRDYIE       : Boolean := False;
      --  LSE ready interrupt enable
      LSERDYIE       : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  HSI ready interrupt enable
      HSIRDYIE       : Boolean := False;
      --  HSE ready interrupt enable
      HSERDYIE       : Boolean := False;
      --  PLL ready interrupt enable
      PLLSYSRDYIE    : Boolean := False;
      --  unspecified
      Reserved_6_8   : HAL.UInt3 := 16#0#;
      --  LSE clock security system interrupt enable
      LSECSSIE       : Boolean := False;
      --  HSI48 ready interrupt enable
      RC48RDYIE      : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
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

   --  Clock interrupt flag register
   type CIFR_Register is record
      --  Read-only. LSI ready interrupt flag
      LSIRDYF        : Boolean;
      --  Read-only. LSE ready interrupt flag
      LSERDYF        : Boolean;
      --  unspecified
      Reserved_2_2   : HAL.Bit;
      --  Read-only. HSI ready interrupt flag
      HSIRDYF        : Boolean;
      --  Read-only. HSE ready interrupt flag
      HSERDYF        : Boolean;
      --  Read-only. PLL ready interrupt flag
      PLLSYSRDYF     : Boolean;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. Clock security system interrupt flag
      HSECSSF        : Boolean;
      --  Read-only. LSE Clock security system interrupt flag
      LSECSSF        : Boolean;
      --  Read-only. HSI48 ready interrupt flag
      RC48RDYF       : Boolean;
      --  unspecified
      Reserved_11_31 : HAL.UInt21;
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

   --  Clock interrupt clear register
   type CICR_Register is record
      --  Write-only. LSI ready interrupt clear
      LSIRDYC        : Boolean := False;
      --  Write-only. LSE ready interrupt clear
      LSERDYC        : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Write-only. HSI ready interrupt clear
      HSIRDYC        : Boolean := False;
      --  Write-only. HSE ready interrupt clear
      HSERDYC        : Boolean := False;
      --  Write-only. PLL ready interrupt clear
      PLLSYSRDYC     : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Write-only. Clock security system interrupt clear
      HSECSSC        : Boolean := False;
      --  Write-only. LSE Clock security system interrupt clear
      LSECSSC        : Boolean := False;
      --  Write-only. HSI48 oscillator ready interrupt clear
      RC48RDYC       : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
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

   --  AHB1 peripheral reset register
   type AHB1RSTR_Register is record
      --  DMA1 reset
      DMA1RST        : Boolean := False;
      --  DMA2 reset
      DMA2RST        : Boolean := False;
      --  DMAMUXRST
      DMAMUX1RST     : Boolean := False;
      --  CORDIC reset
      CORDICRST      : Boolean := False;
      --  FMAC reset
      FMACRST        : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  FLASH reset
      FLASHRST       : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  CRC reset
      CRCRST         : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
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

   --  AHB2 peripheral reset register
   type AHB2RSTR_Register is record
      --  IO port A reset
      GPIOARST       : Boolean := False;
      --  IO port B reset
      GPIOBRST       : Boolean := False;
      --  IO port C reset
      GPIOCRST       : Boolean := False;
      --  IO port D reset
      GPIODRST       : Boolean := False;
      --  IO port E reset
      GPIOERST       : Boolean := False;
      --  IO port F reset
      GPIOFRST       : Boolean := False;
      --  IO port G reset
      GPIOGRST       : Boolean := False;
      --  unspecified
      Reserved_7_12  : HAL.UInt6 := 16#0#;
      --  ADC reset
      ADC12RST       : Boolean := False;
      --  SAR ADC345 interface reset
      ADC345RST      : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  DAC1 interface reset
      DAC1RST        : Boolean := False;
      --  DAC2 interface reset
      DAC2RST        : Boolean := False;
      --  DAC3 interface reset
      DAC3RST        : Boolean := False;
      --  DAC4 interface reset
      DAC4RST        : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Cryptography module reset
      AESRST         : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Random Number Generator module reset
      RNGRST         : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
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

   --  AHB3 peripheral reset register
   type AHB3RSTR_Register is record
      --  Flexible memory controller reset
      FMCRST        : Boolean := False;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  Quad SPI 1 module reset
      QSPIRST       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3RSTR_Register use record
      FMCRST        at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      QSPIRST       at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  APB1 peripheral reset register 1
   type APB1RSTR1_Register is record
      --  TIM2 timer reset
      TIM2RST        : Boolean := False;
      --  TIM3 timer reset
      TIM3RST        : Boolean := False;
      --  TIM3 timer reset
      TIM4RST        : Boolean := False;
      --  TIM5 timer reset
      TIM5RST        : Boolean := False;
      --  TIM6 timer reset
      TIM6RST        : Boolean := False;
      --  TIM7 timer reset
      TIM7RST        : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Clock recovery system reset
      CRSRST         : Boolean := False;
      --  unspecified
      Reserved_9_13  : HAL.UInt5 := 16#0#;
      --  SPI2 reset
      SPI2RST        : Boolean := False;
      --  SPI3 reset
      SPI3RST        : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  USART2 reset
      USART2RST      : Boolean := False;
      --  USART3 reset
      USART3RST      : Boolean := False;
      --  UART4 reset
      UART4RST       : Boolean := False;
      --  UART5 reset
      UART5RST       : Boolean := False;
      --  I2C1 reset
      I2C1RST        : Boolean := False;
      --  I2C2 reset
      I2C2RST        : Boolean := False;
      --  USBD reset
      USBDRST        : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  FDCAN reset
      FDCANRST       : Boolean := False;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Power interface reset
      PWRRST         : Boolean := False;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  I2C3 interface reset
      I2C3RST        : Boolean := False;
      --  Low Power Timer 1 reset
      LPTIM1RST      : Boolean := False;
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

   --  APB1 peripheral reset register 2
   type APB1RSTR2_Register is record
      --  Low-power UART 1 reset
      LPUART1RST    : Boolean := False;
      --  I2C4 reset
      I2C4RST       : Boolean := False;
      --  unspecified
      Reserved_2_7  : HAL.UInt6 := 16#0#;
      --  USBPD reset
      USBPDRST      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
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

   --  APB2 peripheral reset register
   type APB2RSTR_Register is record
      --  System configuration (SYSCFG) reset
      SYSCFGRST      : Boolean := False;
      --  unspecified
      Reserved_1_10  : HAL.UInt10 := 16#0#;
      --  TIM1 timer reset
      TIM1RST        : Boolean := False;
      --  SPI1 reset
      SPI1RST        : Boolean := False;
      --  TIM8 timer reset
      TIM8RST        : Boolean := False;
      --  USART1 reset
      USART1RST      : Boolean := False;
      --  SPI 4 reset
      SPI4RST        : Boolean := False;
      --  TIM15 timer reset
      TIM15RST       : Boolean := False;
      --  TIM16 timer reset
      TIM16RST       : Boolean := False;
      --  TIM17 timer reset
      TIM17RST       : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Timer 20 reset
      TIM20RST       : Boolean := False;
      --  Serial audio interface 1 (SAI1) reset
      SAI1RST        : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  HRTIMER reset
      HRTIM1RST      : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
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

   --  AHB1 peripheral clock enable register
   type AHB1ENR_Register is record
      --  DMA1 clock enable
      DMA1EN         : Boolean := False;
      --  DMA2 clock enable
      DMA2EN         : Boolean := False;
      --  DMAMUX clock enable
      DMAMUXEN       : Boolean := False;
      --  CORDIC clock enable
      CORDICEN       : Boolean := False;
      --  FMAC clock enable
      FMACEN         : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  FLITF clock enable
      FLITFEN        : Boolean := True;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  CRC clock enable
      CRCEN          : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
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

   --  AHB2 peripheral clock enable register
   type AHB2ENR_Register is record
      --  IO port A clock enable
      GPIOAEN        : Boolean := False;
      --  IO port B clock enable
      GPIOBEN        : Boolean := False;
      --  IO port C clock enable
      GPIOCEN        : Boolean := False;
      --  IO port D clock enable
      GPIODEN        : Boolean := False;
      --  IO port E clock enable
      GPIOEEN        : Boolean := False;
      --  IO port F clock enable
      GPIOFEN        : Boolean := False;
      --  IO port G clock enable
      GPIOGEN        : Boolean := False;
      --  unspecified
      Reserved_7_12  : HAL.UInt6 := 16#0#;
      --  ADC12 clock enable
      ADC12EN        : Boolean := False;
      --  ADC345 clock enable
      ADC345EN       : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  DAC1 clock enable
      DAC1EN         : Boolean := False;
      --  DAC2 clock enable
      DAC2EN         : Boolean := False;
      --  DAC3 clock enable
      DAC3EN         : Boolean := False;
      --  DAC4 clock enable
      DAC4EN         : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Cryptography clock enable
      CRYPTEN        : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Random Number Generator clock enable
      RNGEN          : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
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

   --  AHB3 peripheral clock enable register
   type AHB3ENR_Register is record
      --  Flexible memory controller clock enable
      FMCEN         : Boolean := False;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  Quad SPI 1 module clock enable
      QSPIEN        : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3ENR_Register use record
      FMCEN         at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      QSPIEN        at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  APB1ENR1
   type APB1ENR1_Register is record
      --  TIM2 timer clock enable
      TIM2EN         : Boolean := False;
      --  TIM3 timer clock enable
      TIM3EN         : Boolean := False;
      --  TIM4 timer clock enable
      TIM4EN         : Boolean := False;
      --  TIM5 timer clock enable
      TIM5EN         : Boolean := False;
      --  TIM6 timer clock enable
      TIM6EN         : Boolean := False;
      --  TIM7 timer clock enable
      TIM7EN         : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  CRSclock enable
      CRSEN          : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  RTC APB clock enable
      RTCAPBEN       : Boolean := False;
      --  Window watchdog clock enable
      WWDGEN         : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  SPI2 clock enable
      SPI2EN         : Boolean := False;
      --  SPI3 clock enable
      SPI3EN         : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  USART2 clock enable
      USART2EN       : Boolean := False;
      --  USART3 clock enable
      USART3EN       : Boolean := False;
      --  UART4 clock enable
      UART4EN        : Boolean := False;
      --  UART5 clock enable
      UART5EN        : Boolean := False;
      --  I2C1 clock enable
      I2C1EN         : Boolean := False;
      --  I2C2 clock enable
      I2C2EN         : Boolean := False;
      --  USBDclock enable
      USBDEN         : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  FDCAN clock enable
      FDCANEN        : Boolean := False;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Power interface clock enable
      PWREN          : Boolean := False;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  I2C3 clock enable
      I2C3EN         : Boolean := False;
      --  Low power timer 1 clock enable
      LPTIM1EN       : Boolean := False;
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

   --  APB1 peripheral clock enable register 2
   type APB1ENR2_Register is record
      --  Low power UART 1 clock enable
      LPUART1EN     : Boolean := False;
      --  I2C4 clock enable
      I2C4EN        : Boolean := False;
      --  unspecified
      Reserved_2_7  : HAL.UInt6 := 16#0#;
      --  USBPD clock enable
      USBPDEN       : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
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

   --  APB2ENR
   type APB2ENR_Register is record
      --  SYSCFG clock enable
      SYSCFGEN       : Boolean := False;
      --  unspecified
      Reserved_1_10  : HAL.UInt10 := 16#0#;
      --  TIM1 timer clock enable
      TIM1EN         : Boolean := False;
      --  SPI1 clock enable
      SPI1EN         : Boolean := False;
      --  TIM8 timer clock enable
      TIM8EN         : Boolean := False;
      --  USART1clock enable
      USART1EN       : Boolean := False;
      --  SPI 4 clock enable
      SPI4EN         : Boolean := False;
      --  TIM15 timer clock enable
      TIM15EN        : Boolean := False;
      --  TIM16 timer clock enable
      TIM16EN        : Boolean := False;
      --  TIM17 timer clock enable
      TIM17EN        : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Timer 20 clock enable
      TIM20EN        : Boolean := False;
      --  SAI1 clock enable
      SAI1EN         : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  HRTIMER clock enable
      HRTIM1EN       : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
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

   --  AHB1 peripheral clocks enable in Sleep and Stop modes register
   type AHB1SMENR_Register is record
      --  DMA1 clocks enable during Sleep and Stop modes
      DMA1SMEN       : Boolean := True;
      --  DMA2 clocks enable during Sleep and Stop modes
      DMA2SMEN       : Boolean := True;
      --  DMAMUX clock enable during Sleep and Stop modes
      DMAMUX1SMEN    : Boolean := True;
      --  CORDIC clock enable during sleep mode
      CORDICSMEN     : Boolean := True;
      --  FMACSM clock enable
      FMACSMEN       : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Flash memory interface clocks enable during Sleep and Stop modes
      FLASHSMEN      : Boolean := True;
      --  SRAM1 interface clocks enable during Sleep and Stop modes
      SRAM1SMEN      : Boolean := True;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  CRCSMEN
      CRCSMEN        : Boolean := True;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
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

   --  AHB2 peripheral clocks enable in Sleep and Stop modes register
   type AHB2SMENR_Register is record
      --  IO port A clocks enable during Sleep and Stop modes
      GPIOASMEN      : Boolean := True;
      --  IO port B clocks enable during Sleep and Stop modes
      GPIOBSMEN      : Boolean := True;
      --  IO port C clocks enable during Sleep and Stop modes
      GPIOCSMEN      : Boolean := True;
      --  IO port D clocks enable during Sleep and Stop modes
      GPIODSMEN      : Boolean := True;
      --  IO port E clocks enable during Sleep and Stop modes
      GPIOESMEN      : Boolean := True;
      --  IO port F clocks enable during Sleep and Stop modes
      GPIOFSMEN      : Boolean := True;
      --  IO port G clocks enable during Sleep and Stop modes
      GPIOGSMEN      : Boolean := True;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  SRAM2 interface clocks enable during Sleep and Stop modes
      SRAM2SMEN      : Boolean := True;
      --  SRAM2 interface clocks enable during Sleep and Stop modes
      SRAM3SMEN      : Boolean := True;
      --  unspecified
      Reserved_11_12 : HAL.UInt2 := 16#0#;
      --  ADC clocks enable during Sleep and Stop modes
      AD12CSMEN      : Boolean := True;
      --  DCMI clock enable during Sleep and Stop modes
      ADC345SMEN     : Boolean := True;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  AES accelerator clocks enable during Sleep and Stop modes
      DAC1SMEN       : Boolean := True;
      --  HASH clock enable during Sleep and Stop modes
      DAC2SMEN       : Boolean := True;
      --  DAC3 clock enable during sleep mode
      DAC3SMEN       : Boolean := True;
      --  DAC4 clock enable during sleep mode
      DAC4SMEN       : Boolean := True;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  Cryptography clock enable during sleep mode
      CRYPTSMEN      : Boolean := True;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Random Number Generator clock enable during sleep mode
      RNGSMEN        : Boolean := True;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
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

   --  AHB3 peripheral clocks enable in Sleep and Stop modes register
   type AHB3SMENR_Register is record
      --  Flexible memory controller clocks enable during Sleep and Stop modes
      FMCSMEN       : Boolean := True;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  QUAD SPI 1 module clock enable during sleep mode
      QSPISMEN      : Boolean := True;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AHB3SMENR_Register use record
      FMCSMEN       at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      QSPISMEN      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  APB1SMENR1
   type APB1SMENR1_Register is record
      --  TIM2 timer clocks enable during Sleep and Stop modes
      TIM2SMEN       : Boolean := True;
      --  TIM3 timer clocks enable during Sleep and Stop modes
      TIM3SMEN       : Boolean := True;
      --  TIM4 timer clocks enable during Sleep and Stop modes
      TIM4SMEN       : Boolean := True;
      --  TIM5 timer clocks enable during Sleep and Stop modes
      TIM5SMEN       : Boolean := True;
      --  TIM6 timer clocks enable during Sleep and Stop modes
      TIM6SMEN       : Boolean := True;
      --  TIM7 timer clocks enable during Sleep and Stop modes
      TIM7SMEN       : Boolean := True;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  CRS clock enable during sleep mode
      CRSSMEN        : Boolean := True;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  RTC APB clock enable during Sleep and Stop modes
      RTCAPBSMEN     : Boolean := True;
      --  Window watchdog clocks enable during Sleep and Stop modes
      WWDGSMEN       : Boolean := True;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  SPI2 clocks enable during Sleep and Stop modes
      SPI2SMEN       : Boolean := True;
      --  SPI3 clocks enable during Sleep and Stop modes
      SP3SMEN        : Boolean := True;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  USART2 clocks enable during Sleep and Stop modes
      USART2SMEN     : Boolean := True;
      --  USART3 clocks enable during Sleep and Stop modes
      USART3SMEN     : Boolean := True;
      --  UART4 clocks enable during Sleep and Stop modes
      UART4SMEN      : Boolean := True;
      --  UART5 clocks enable during Sleep and Stop modes
      UART5SMEN      : Boolean := True;
      --  I2C1 clocks enable during Sleep and Stop modes
      I2C1SMEN       : Boolean := True;
      --  I2C2 clocks enable during Sleep and Stop modes
      I2C2SMEN       : Boolean := True;
      --  I2C3 clocks enable during Sleep and Stop modes
      I2C3SMEN       : Boolean := True;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  FDCAN clock enable during sleep mode
      FDCANSMEN      : Boolean := True;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Power interface clocks enable during Sleep and Stop modes
      PWRSMEN        : Boolean := True;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  I2C 3 interface clock enable during sleep mode
      I2C3SMEN_3     : Boolean := True;
      --  Low Power Timer1 clock enable during sleep mode
      LPTIM1SMEN     : Boolean := True;
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

   --  APB1 peripheral clocks enable in Sleep and Stop modes register 2
   type APB1SMENR2_Register is record
      --  Low power UART 1 clocks enable during Sleep and Stop modes
      LPUART1SMEN   : Boolean := True;
      --  I2C4 clocks enable during Sleep and Stop modes
      I2C4SMEN      : Boolean := True;
      --  unspecified
      Reserved_2_7  : HAL.UInt6 := 16#0#;
      --  USB PD clock enable during sleep mode
      USBPDSMEN     : Boolean := True;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
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

   --  APB2SMENR
   type APB2SMENR_Register is record
      --  SYSCFG clocks enable during Sleep and Stop modes
      SYSCFGSMEN     : Boolean := True;
      --  unspecified
      Reserved_1_10  : HAL.UInt10 := 16#0#;
      --  TIM1 timer clocks enable during Sleep and Stop modes
      TIM1SMEN       : Boolean := True;
      --  SPI1 clocks enable during Sleep and Stop modes
      SPI1SMEN       : Boolean := True;
      --  TIM8 timer clocks enable during Sleep and Stop modes
      TIM8SMEN       : Boolean := True;
      --  USART1clocks enable during Sleep and Stop modes
      USART1SMEN     : Boolean := True;
      --  SPI4 timer clocks enable during Sleep and Stop modes
      SPI4SMEN       : Boolean := True;
      --  TIM15 timer clocks enable during Sleep and Stop modes
      TIM15SMEN      : Boolean := True;
      --  TIM16 timer clocks enable during Sleep and Stop modes
      TIM16SMEN      : Boolean := True;
      --  TIM17 timer clocks enable during Sleep and Stop modes
      TIM17SMEN      : Boolean := True;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Timer 20clock enable during sleep mode
      TIM20SMEN      : Boolean := True;
      --  SAI1 clock enable during sleep mode
      SAI1SMEN       : Boolean := True;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  HRTIMER clock enable during sleep mode
      HRTIMERSMEN    : Boolean := True;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
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

   subtype CCIPR_USART1SEL_Field is HAL.UInt2;
   subtype CCIPR_USART2SEL_Field is HAL.UInt2;
   subtype CCIPR_USART3SEL_Field is HAL.UInt2;
   subtype CCIPR_UART4SEL_Field is HAL.UInt2;
   subtype CCIPR_UART5SEL_Field is HAL.UInt2;
   subtype CCIPR_LPUART1SEL_Field is HAL.UInt2;
   subtype CCIPR_I2C1SEL_Field is HAL.UInt2;
   subtype CCIPR_I2C2SEL_Field is HAL.UInt2;
   subtype CCIPR_I2C3SEL_Field is HAL.UInt2;
   subtype CCIPR_LPTIM1SEL_Field is HAL.UInt2;
   subtype CCIPR_SAI1SEL_Field is HAL.UInt2;
   subtype CCIPR_I2S23SEL_Field is HAL.UInt2;
   subtype CCIPR_FDCANSEL_Field is HAL.UInt2;
   subtype CCIPR_CLK48SEL_Field is HAL.UInt2;
   subtype CCIPR_ADC12SEL_Field is HAL.UInt2;
   subtype CCIPR_ADC345SEL_Field is HAL.UInt2;

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

   subtype BDCR_LSEDRV_Field is HAL.UInt2;
   subtype BDCR_RTCSEL_Field is HAL.UInt2;

   --  BDCR
   type BDCR_Register is record
      --  LSE oscillator enable
      LSEON          : Boolean := False;
      --  Read-only. LSE oscillator ready
      LSERDY         : Boolean := False;
      --  LSE oscillator bypass
      LSEBYP         : Boolean := False;
      --  SE oscillator drive capability
      LSEDRV         : BDCR_LSEDRV_Field := 16#0#;
      --  LSECSSON
      LSECSSON       : Boolean := False;
      --  Read-only. LSECSSD
      LSECSSD        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  RTC clock source selection
      RTCSEL         : BDCR_RTCSEL_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  RTC clock enable
      RTCEN          : Boolean := False;
      --  BDRST domain software reset
      BDRST          : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  Low speed clock output enable
      LSCOEN         : Boolean := False;
      --  Low speed clock output selection
      LSCOSEL        : Boolean := False;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
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

   --  CSR
   type CSR_Register is record
      --  LSI oscillator enable
      LSION          : Boolean := False;
      --  Read-only. LSI oscillator ready
      LSIRDY         : Boolean := False;
      --  unspecified
      Reserved_2_22  : HAL.UInt21 := 16#0#;
      --  Remove reset flag
      RMVF           : Boolean := False;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  Read-only. Option byte loader reset flag
      OBLRSTF        : Boolean := False;
      --  Read-only. Pad reset flag
      PADRSTF        : Boolean := True;
      --  Read-only. BOR flag
      BORRSTF        : Boolean := True;
      --  Read-only. Software reset flag
      SFTRSTF        : Boolean := False;
      --  Read-only. Independent window watchdog reset flag
      IWDGRSTF       : Boolean := False;
      --  Read-only. Window watchdog reset flag
      WWDGRSTF       : Boolean := False;
      --  Read-only. Low-power reset flag
      LPWRSTF        : Boolean := False;
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

   subtype CRRCR_HSI48CAL_Field is HAL.UInt9;

   --  Clock recovery RC register
   type CRRCR_Register is record
      --  HSI48 clock enable
      HSI48ON        : Boolean := False;
      --  Read-only. HSI48 clock ready flag
      HSI48RDY       : Boolean := False;
      --  unspecified
      Reserved_2_6   : HAL.UInt5 := 16#0#;
      --  Read-only. HSI48 clock calibration
      HSI48CAL       : CRRCR_HSI48CAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
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

   subtype CCIPR2_I2C4SEL_Field is HAL.UInt2;
   subtype CCIPR2_QSPISEL_Field is HAL.UInt2;

   --  Peripherals independent clock configuration register
   type CCIPR2_Register is record
      --  I2C4 clock source selection
      I2C4SEL        : CCIPR2_I2C4SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_19  : HAL.UInt18 := 16#0#;
      --  Octospi clock source selection
      QSPISEL        : CCIPR2_QSPISEL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
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

end STM32_SVD.RCC;
