------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.STM32;

package System.STM32 is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.STM32);

   subtype Frequency is Interfaces.STM32.UInt32;

   --  See RM0440 rev. 6 pg. 276 chapter 7.2, and pg. 279 for clock tree
   type RCC_System_Clocks is record
      SYSCLK    : Frequency;
      HCLK      : Frequency;
      PCLK1     : Frequency;
      PCLK2     : Frequency;
      TIMCLK1   : Frequency; --  For TIMs 2 .. 7
      TIMCLK2   : Frequency; --  For TIMs 1, 8, 20, 15 .. 17, HRTIM1
      TIMCLK3   : Frequency; --  For LPTIMs 1 .. 2
   end record;

   function System_Clocks return RCC_System_Clocks;

   --  MODER constants
   subtype GPIO_MODER_Values is Interfaces.STM32.UInt2;
   Mode_IN  : constant GPIO_MODER_Values := 0;
   Mode_OUT : constant GPIO_MODER_Values := 1;
   Mode_AF  : constant GPIO_MODER_Values := 2;
   Mode_AN  : constant GPIO_MODER_Values := 3;

   --  OTYPER constants
   subtype GPIO_OTYPER_Values is Interfaces.STM32.Bit;
   Push_Pull  : constant GPIO_OTYPER_Values := 0;
   Open_Drain : constant GPIO_OTYPER_Values := 1;

   --  OSPEEDR constants
   subtype GPIO_OSPEEDR_Values is Interfaces.STM32.UInt2;
   Speed_2MHz   : constant GPIO_OSPEEDR_Values := 0; -- Low speed
   Speed_25MHz  : constant GPIO_OSPEEDR_Values := 1; -- Medium speed
   Speed_50MHz  : constant GPIO_OSPEEDR_Values := 2; -- Fast speed
   Speed_100MHz : constant GPIO_OSPEEDR_Values := 3; -- High speed

   --  PUPDR constants
   subtype GPIO_PUPDR_Values is Interfaces.STM32.UInt2;
   No_Pull   : constant GPIO_PUPDR_Values := 0;
   Pull_Up   : constant GPIO_PUPDR_Values := 1;
   Pull_Down : constant GPIO_PUPDR_Values := 2;

   --  AFL constants
   AF_USART1  : constant Interfaces.STM32.UInt4 := 7;
   AF_USART3  : constant Interfaces.STM32.UInt4 := 7;

   type MCU_ID_Register is record
      DEV_ID   : Interfaces.STM32.UInt12;
      Reserved : Interfaces.STM32.UInt4;
      REV_ID   : Interfaces.STM32.UInt16;
   end record with Pack, Size => 32;

   --  RCC constants

   type PLL_Source is
     (PLL_SRC_HSI,
      PLL_SRC_HSE)
     with Size => 2;

   for PLL_Source use
     (PLL_SRC_HSI   => 2#10#,
      PLL_SRC_HSE   => 2#11#);

   type SYSCLK_Source is
     (SYSCLK_SRC_HSI,
      SYSCLK_SRC_HSE,
      SYSCLK_SRC_PLL)
     with Size => 2;

   for SYSCLK_Source use
     (SYSCLK_SRC_HSI => 2#01#,
      SYSCLK_SRC_HSE => 2#10#,
      SYSCLK_SRC_PLL => 2#11#);

   type AHB_Prescaler_Enum is
     (DIV2,  DIV4,   DIV8,   DIV16,
      DIV64, DIV128, DIV256, DIV512)
     with Size => 3;

   type AHB_Prescaler is record
      Enabled : Boolean := False;
      Value   : AHB_Prescaler_Enum := AHB_Prescaler_Enum'First;
   end record with Size => 4;

   for AHB_Prescaler use record
      Enabled at 0 range 3 .. 3;
      Value   at 0 range 0 .. 2;
   end record;

   AHBPRE_DIV1 : constant AHB_Prescaler := (Enabled => False, Value => DIV2);

   type APB_Prescaler_Enum is
     (DIV2,  DIV4,  DIV8,  DIV16)
     with Size => 2;

   type APB_Prescaler is record
      Enabled : Boolean;
      Value   : APB_Prescaler_Enum;
   end record with Size => 3;

   for APB_Prescaler use record
      Enabled at 0 range 2 .. 2;
      Value   at 0 range 0 .. 1;
   end record;

   type MCO_Clock_Source is
     (MCOSEL_Disabled,
      MCOSEL_SYSCLK,
      MCOSEL_HSI,
      MCOSEL_HSE,
      MCOSEL_PLL,
      MCOSEL_LSI,
      MCOSEL_LSE,
      MCOSEL_HSI48)
     with Size => 4;

   for MCO_Clock_Source use
     (MCOSEL_Disabled => 2#0000#,
      MCOSEL_SYSCLK   => 2#0001#,
      MCOSEL_HSI      => 2#0011#,
      MCOSEL_HSE      => 2#0100#,
      MCOSEL_PLL      => 2#0101#,
      MCOSEL_LSI      => 2#0110#,
      MCOSEL_LSE      => 2#0111#,
      MCOSEL_HSI48    => 2#1000#);

   type MCO_Prescaler is
     (MCOPRE_DIV1,
      MCOPRE_DIV2,
      MCOPRE_DIV4,
      MCOPRE_DIV8,
      MCOPRE_DIV16)
     with Size => 3;

   for MCO_Prescaler use
     (MCOPRE_DIV1  => 2#000#,
      MCOPRE_DIV2  => 2#001#,
      MCOPRE_DIV4  => 2#010#,
      MCOPRE_DIV8  => 2#011#,
      MCOPRE_DIV16 => 2#100#);

   --  Constants for RCC CR register

   subtype PLLM_Range is Integer range   1 ..  16;
   subtype PLLN_Range is Integer range   8 .. 127;
   subtype PLLP_Range is Integer range   2 ..  31;
   subtype PLLQ_Range is Integer range   2 ..   8
     with Static_Predicate => (case PLLQ_Range is
                                 when 2 | 4 | 6 | 8 => True,
                                 when others => False);
   subtype PLLR_Range is Integer range   2 ..   8
     with Static_Predicate => (case PLLR_Range is
                                 when 2 | 4 | 6 | 8 => True,
                                 when others => False);

   subtype HSECLK_Range   is Integer range   4_000_000 ..  48_000_000;
   subtype PLLM_OUT_Range is Integer range   2_660_000 ..  16_000_000;
   subtype PLLN_OUT_Range is Integer range  96_000_000 .. 344_000_000;
   subtype PLLP_OUT_Range is Integer range   1_000_000 .. 170_000_000;
   subtype PLLQ_OUT_Range is Integer range  47_880_000 ..  48_120_000;
   subtype PLLCLK_Range   is Integer range   8_000_000 .. 170_000_000;
   subtype SYSCLK_Range   is Integer range           1 .. 170_000_000;
   subtype HCLK_Range     is Integer range           1 .. 170_000_000;
   subtype PCLK1_Range    is Integer range           1 .. 170_000_000;
   subtype PCLK2_Range    is Integer range           1 .. 170_000_000;

   --  Constants for Flash Latency
   --               with VCORE Range 1 boost mode | Range 1 normal mode
   --  000: Zero wait state    0 < HCLK ≤  34 MHz |   0 < HCLK ≤  30 MHz
   --  001: One wait state    34 < HCLK ≤  68 MHz |  30 < HCLK ≤  60 MHz
   --  010: Two wait sates    68 < HCLK ≤ 102 MHz |  60 < HCLK ≤  90 MHz
   --  011: Three wait sates 102 < HCLK ≤ 136 MHz |  90 < HCLK ≤ 120 MHz
   --  100: Four wait sates  136 < HCLK ≤ 170 MHz | 120 < HCLK ≤ 150 MHz
   --  RM0440 STM32G474 pg. 97 chapter 3.3.3 and pg. 129 chapter 3.7.1

   subtype FLASH_Latency_0 is Integer range           1 ..  34_000_000;
   subtype FLASH_Latency_1 is Integer range  35_000_000 ..  68_000_000;
   subtype FLASH_Latency_2 is Integer range  69_000_000 .. 102_000_000;
   subtype FLASH_Latency_3 is Integer range 103_000_000 .. 136_000_000;
   subtype FLASH_Latency_4 is Integer range 137_000_000 .. 170_000_000;

   --  Flash wait states
   type FLASH_WS is (FWS0, FWS1, FWS2, FWS3, FWS4)
     with Size => 4;

   FLASH_Latency : Interfaces.STM32.UInt4 := FLASH_WS'Enum_Rep (FWS4);

   --  These internal low and high speed clocks are fixed (do not modify)
   HSICLK   : constant := 16_000_000;
   HSI48CLK : constant := 48_000_000;
   LSICLK   : constant :=     32_000;

   MCU_ID : MCU_ID_Register
     with Volatile, Address => System'To_Address (16#E004_2000#);
   --  Only 32-bits access supported (read-only) RM0440 pg. 2086 chapter 47.6.1

   DEV_ID_STM32F40xxx : constant := 16#413#; --  STM32F40xxx/41xxx
   DEV_ID_STM32F42xxx : constant := 16#419#; --  STM32F42xxx/43xxx
   DEV_ID_STM32F46xxx : constant := 16#434#; --  STM32F469xx/479xx
   DEV_ID_STM32F74xxx : constant := 16#449#; --  STM32F74xxx/75xxx
   DEV_ID_STM32F76xxx : constant := 16#451#; --  STM32F76xxx/77xxx
   DEV_ID_STM32F334xx : constant := 16#438#; --  STM32F334xx
   DEV_ID_STM32G474x2 : constant := 16#468#; --  STM32G474xx category 2
   DEV_ID_STM32G474x3 : constant := 16#469#; --  STM32G474xx category 3
   DEV_ID_STM32G474x4 : constant := 16#479#; --  STM32G474xx category 4

end System.STM32;
