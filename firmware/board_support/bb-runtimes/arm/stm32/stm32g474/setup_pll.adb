------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2019, Free Software Foundation, Inc.         --
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

pragma Ada_2012; -- To work around pre-commit check?
pragma Suppress (All_Checks);

--  This initialization procedure mainly initializes the PLLs and
--  all derived clocks.

with Ada.Unchecked_Conversion;

with Interfaces.STM32;         use Interfaces, Interfaces.STM32;
with Interfaces.STM32.Flash;   use Interfaces.STM32.Flash;
with Interfaces.STM32.RCC;     use Interfaces.STM32.RCC;

with System.BB.Parameters;     use System.BB.Parameters;
with System.BB.MCU_Parameters;
with System.STM32;             use System.STM32;

procedure Setup_Pll is
   procedure Initialize_Clocks;
   procedure Reset_Clocks;

   ------------------------------
   -- Clock Tree Configuration --
   ------------------------------

   HSE_Enabled : constant Boolean := True; --  use high-speed external clock
   HSE_Bypass  : constant Boolean := True; --  bypass osc. with external clock
   LSE_Enabled : constant Boolean := False; --  use low-speed external clock

   --  HSI_Enabled : constant Boolean := (if HSE_Enabled then False else True);
   --  use high-speed HSI16 internal clock
   LSI_Enabled : constant Boolean := True; --  use low-speed internal clock

   Activate_PLL : constant Boolean := True;
   --  Activate_Overdrive : constant Boolean := True;

   -----------------------
   -- Initialize_Clocks --
   -----------------------

   procedure Initialize_Clocks
   is
      -------------------------------
      -- Compute Clock Frequencies --
      -------------------------------

      PLLP_Value    : constant := 2;
      PLLQ_Value    : constant := 4;
      PLLR_Value    : constant := 2;

      PLLCLKIN : constant Integer := 4_000_000;
      --  PLL input clock value
      PLLM_Value : constant Integer  :=
        (if HSE_Enabled then HSE_Clock else HSI_Clock) / PLLCLKIN;
      --  First divider M is set to produce a 4Mhz clock

      PLLN_Value : constant Integer :=
        (PLLR_Value * Clock_Frequency) / PLLCLKIN;
      --  Compute N to to generate the required frequency

      pragma Compile_Time_Error
        (Activate_PLL and PLLM_Value not in PLLM_Range,
         "Invalid PLLM clock configuration value");

      pragma Compile_Time_Error
        (Activate_PLL and PLLN_Value not in PLLN_Range,
         "Invalid PLLN clock configuration value");

      pragma Compile_Time_Error
        (Activate_PLL and PLLR_Value not in PLLR_Range,
         "Invalid PLLR clock configuration value");

      pragma Compile_Time_Error
        (Activate_PLL and PLLP_Value not in PLLP_Range,
         "Invalid PLLP clock configuration value");

      pragma Compile_Time_Error
        (Activate_PLL and PLLQ_Value not in PLLQ_Range,
         "Invalid PLLQ clock configuration value");

      PLLVCO : constant Integer := PLLCLKIN * PLLN_Value; --  PLLN_OUT

      pragma Compile_Time_Error
        (Activate_PLL and PLLVCO not in PLLN_OUT_Range,
         "Invalid PLLN clock configuration output value");

      PLLCLKOUT : constant Integer := PLLVCO / PLLR_Value; --  PLLCLK

      pragma Compile_Time_Error
        (Activate_PLL and PLLCLKOUT not in PLLCLK_Range,
         "Invalid PLLR clock configuration output value");

      PLLM : constant UInt4 := UInt4 (PLLM_Value - 1);
      PLLN : constant UInt7 := UInt7 (PLLN_Value);
      PLLP : constant UInt5 := UInt5 (PLLP_Value);
      PLLQ : constant UInt2 := UInt2 (PLLQ_Value / 2 - 1);
      PLLR : constant UInt2 := UInt2 (PLLR_Value / 2 - 1);

      SW : constant SYSCLK_Source :=
             (if Activate_PLL then SYSCLK_SRC_PLL
              else (if HSE_Enabled then SYSCLK_SRC_HSE else SYSCLK_SRC_HSI));

      SW_Value : constant CFGR_SW_Field := SYSCLK_Source'Enum_Rep (SW);

      SYSCLK : constant Integer :=
                 (if Activate_PLL then PLLCLKOUT
                  else (if HSE_Enabled then HSE_Clock else HSI_Clock));

      HCLK : constant Integer :=
               (if not AHB_PRE.Enabled then SYSCLK
                else
                   (case AHB_PRE.Value is
                       when DIV2   => SYSCLK / 2,
                       when DIV4   => SYSCLK / 4,
                       when DIV8   => SYSCLK / 8,
                       when DIV16  => SYSCLK / 16,
                       when DIV64  => SYSCLK / 64,
                       when DIV128 => SYSCLK / 128,
                       when DIV256 => SYSCLK / 256,
                       when DIV512 => SYSCLK / 512));
      PCLK1 : constant Integer :=
                (if not APB1_PRE.Enabled then HCLK
                 else
                    (case APB1_PRE.Value is
                        when DIV2  => HCLK / 2,
                        when DIV4  => HCLK / 4,
                        when DIV8  => HCLK / 8,
                        when DIV16 => HCLK / 16));
      PCLK2 : constant Integer :=
                (if not APB2_PRE.Enabled then HCLK
                 else
                    (case APB2_PRE.Value is
                        when DIV2  => HCLK / 2,
                        when DIV4  => HCLK / 4,
                        when DIV8  => HCLK / 8,
                        when DIV16 => HCLK / 16));

      function To_AHB is new Ada.Unchecked_Conversion
        (AHB_Prescaler, UInt4);
      function To_APB is new Ada.Unchecked_Conversion
        (APB_Prescaler, UInt3);

   begin

      pragma Compile_Time_Error
        (SYSCLK /= Clock_Frequency,
           "Cannot generate requested clock");

      --  Cannot be checked at compile time, depends on APB1_PRE and APB2_PRE
      pragma Assert
        (HCLK not in HCLK_Range
           or else PCLK1 not in PCLK1_Range
           or else PCLK2 not in PCLK2_Range,
         "Invalid AHB/APB prescalers configuration");

      --  PWR clock enable
      RCC_Periph.APB1ENR1.PWREN := 1;

      --  Reset the power interface
      RCC_Periph.APB1RSTR1.PWRRST := 1;
      RCC_Periph.APB1RSTR1.PWRRST := 0;

      --  PWR initialization
      System.BB.MCU_Parameters.PWR_Initialize;

      if not HSE_Enabled then
         --  Setup high-speed internal clock and wait for stabilisation.
         RCC_Periph.CR.HSION := 1;
         loop
            exit when RCC_Periph.CR.HSIRDY = 1;
         end loop;

      else
         --  Configure high-speed external clock, if enabled
         RCC_Periph.CR.HSEBYP := (if HSE_Bypass then 1 else 0);
         --  Enable security for HSERDY
         RCC_Periph.CR.CSSON := 1;
         --  Setup high-speed external clock
         RCC_Periph.CR.HSEON := 1;
         --  Wait for HSE stabilisation.
         loop
            exit when RCC_Periph.CR.HSERDY = 1;
         end loop;
      end if;

      if LSE_Enabled then
         --  Setup low-speed external clock and wait for stabilization.
         RCC_Periph.BDCR.LSEON := 1;
         loop
            exit when RCC_Periph.BDCR.LSERDY = 1;
         end loop;
      end if;

      if LSI_Enabled then
         --  Setup low-speed internal clock and wait for stabilization.
         RCC_Periph.CSR.LSION := 1;
         loop
            exit when RCC_Periph.CSR.LSIRDY = 1;
         end loop;
      end if;

      --  Activate PLL if enabled
      if Activate_PLL then
         --  Disable the main PLL before configuring it
         RCC_Periph.CR.PLLON := 0;

         --  Configure the PLL clock source, multiplication and division
         --  factors
         RCC_Periph.PLLCFGR :=
           (PLLM    => PLLM,
            PLLN    => PLLN,
            PLLPDIV => PLLP,
            PLLQ    => PLLQ,
            PLLR    => PLLR,
            PLLSRC  => (if HSE_Enabled
                        then PLL_Source'Enum_Rep (PLL_SRC_HSE)
                        else PLL_Source'Enum_Rep (PLL_SRC_HSI)),
            others => <>);

         --  Setup PLL and wait for stabilization.
         RCC_Periph.CR.PLLON := 1;
         loop
            exit when RCC_Periph.CR.PLLRDY = 1;
         end loop;

         RCC_Periph.PLLCFGR.PLLPEN := 1;
         RCC_Periph.PLLCFGR.PLLQEN := 1;
         RCC_Periph.PLLCFGR.PLLREN := 1;
      end if;

      --  Configure flash
      --  Must be done before increasing the frequency, otherwise the CPU
      --  won't be able to fetch new instructions.

      if HCLK in FLASH_Latency_0 then
         FLASH_Latency := FLASH_WS'Enum_Rep (FWS0);
      elsif HCLK in FLASH_Latency_1 then
         FLASH_Latency := FLASH_WS'Enum_Rep (FWS1);
      elsif HCLK in FLASH_Latency_2 then
         FLASH_Latency := FLASH_WS'Enum_Rep (FWS2);
      elsif HCLK in FLASH_Latency_3 then
         FLASH_Latency := FLASH_WS'Enum_Rep (FWS3);
      elsif HCLK in FLASH_Latency_4 then
         FLASH_Latency := FLASH_WS'Enum_Rep (FWS4);
      end if;

      --  Disable and reset Instruction cache
      FLASH_Periph.ACR.ICEN := 0;
      FLASH_Periph.ACR.ICRST := 1;
      FLASH_Periph.ACR.ICRST := 0;

      --  Disable and reset Data cache
      FLASH_Periph.ACR.DCEN := 0;
      FLASH_Periph.ACR.DCRST := 1;
      FLASH_Periph.ACR.DCRST := 0;

      FLASH_Periph.ACR :=
        (LATENCY => FLASH_Latency,
         PRFTEN  => 1, --  Prefetch enable
         ICEN    => 1, --  Instruction cache enable
         DCEN    => 1, --  Data cache enable
         others  => <>);

      --  Configure derived clocks
      RCC_Periph.CFGR :=
        (SW     => SW_Value,
         HPRE   => To_AHB (AHB_PRE),
         PPRE   => (As_Array => True,
                    Arr      => (1 => To_APB (APB1_PRE),
                                 2 => To_APB (APB2_PRE))),
         --  Microcontroller clock output
         MCOSEL => MCO_Clock_Source'Enum_Rep (MCOSEL_HSI),
         MCOPRE => MCO_Prescaler'Enum_Rep (MCOPRE_DIV1),
         others => <>);

      --  Test system clock switch status
      case SW is
         when SYSCLK_SRC_PLL =>
            loop
               exit when RCC_Periph.CFGR.SWS =
                 SYSCLK_Source'Enum_Rep (SYSCLK_SRC_PLL);
            end loop;
         when SYSCLK_SRC_HSE =>
            loop
               exit when RCC_Periph.CFGR.SWS =
                 SYSCLK_Source'Enum_Rep (SYSCLK_SRC_HSE);
            end loop;
         when SYSCLK_SRC_HSI =>
            loop
               exit when RCC_Periph.CFGR.SWS =
                 SYSCLK_Source'Enum_Rep (SYSCLK_SRC_HSI);
            end loop;
      end case;

   end Initialize_Clocks;

   ------------------
   -- Reset_Clocks --
   ------------------

   procedure Reset_Clocks is
   begin
      --  Switch on high speed internal clock
      RCC_Periph.CR.HSION := 1;

      --  Reset CFGR regiser
      RCC_Periph.CFGR := (others => <>);

      --  Reset HSEON, CSSON and PLLON bits
      RCC_Periph.CR.HSEON := 0;
      RCC_Periph.CR.CSSON := 0;
      RCC_Periph.CR.PLLON := 0;

      --  Reset PLL configuration register
      RCC_Periph.PLLCFGR := (others => <>);

      --  Reset HSE bypass bit
      RCC_Periph.CR.HSEBYP := 0;

      --  Disable all interrupts
      RCC_Periph.CIER := (others => <>);
   end Reset_Clocks;

begin
   Reset_Clocks;
   Initialize_Clocks;
end Setup_Pll;
