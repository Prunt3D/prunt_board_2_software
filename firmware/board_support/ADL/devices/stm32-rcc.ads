------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------
pragma Restrictions (No_Elaboration_Code);

package STM32.RCC is

   procedure AHB_Force_Reset with Inline;
   procedure AHB_Release_Reset with Inline;
   procedure APB1_Force_Reset with Inline;
   procedure APB1_Release_Reset with Inline;
   procedure APB2_Force_Reset with Inline;
   procedure APB2_Release_Reset with Inline;

   procedure Backup_Domain_Reset;
   --  Disable LSE clock and RTC and reset its configurations.

   ---------------------------------------------------------------------------
   --  Clock Configuration  --------------------------------------------------
   ---------------------------------------------------------------------------

   ---------------
   -- HSE Clock --
   ---------------

   procedure Set_HSE_Clock
     (Enable     : Boolean;
      Bypass     : Boolean := False;
      Enable_CSS : Boolean := False)
     with Post => HSE_Clock_Enabled = Enable;

   function HSE_Clock_Enabled return Boolean;

   ---------------
   -- LSE Clock --
   ---------------

   type HSE_Capability is
     (Lowest_Drive,
      Low_Drive,
      High_Drive,
      Highest_Drive)
     with Size => 2;

   procedure Set_LSE_Clock
     (Enable     : Boolean;
      Bypass     : Boolean := False;
      Enable_CSS : Boolean := False;
      Capability : HSE_Capability)
     with Post => LSE_Clock_Enabled = Enable;

   function LSE_Clock_Enabled return Boolean;

   ---------------
   -- HSI Clock --
   ---------------

   procedure Set_HSI_Clock (Enable : Boolean)
     with Post => HSI_Clock_Enabled = Enable;
   --  The HSI clock can't be disabled if it is used directly (via SW mux) as
   --  system clock or if the HSI is selected as reference clock for PLL with
   --  PLL enabled (PLLON bit set to ‘1’). It is set by hardware if it is used
   --  directly or indirectly as system clock.

   function HSI_Clock_Enabled return Boolean;

   -----------------
   -- HSI48 Clock --
   -----------------

   procedure Set_HSI48_Clock (Enable : Boolean)
     with Post => HSI48_Clock_Enabled = Enable;

   function HSI48_Clock_Enabled return Boolean;

   ---------------
   -- LSI Clock --
   ---------------

   procedure Set_LSI_Clock (Enable : Boolean)
     with Post => LSI_Clock_Enabled = Enable;

   function LSI_Clock_Enabled return Boolean;

   ------------------
   -- System Clock --
   ------------------

   type SYSCLK_Clock_Source is
     (SYSCLK_SRC_HSI,
      SYSCLK_SRC_HSE,
      SYSCLK_SRC_PLL)
     with Size => 2;

   for SYSCLK_Clock_Source use
     (SYSCLK_SRC_HSI => 2#01#,
      SYSCLK_SRC_HSE => 2#10#,
      SYSCLK_SRC_PLL => 2#11#);

   procedure Configure_System_Clock_Mux (Source : SYSCLK_Clock_Source);

   ------------------------
   -- AHB and APB Clocks --
   ------------------------

   type AHB_Prescaler_Enum is
     (DIV_2,  DIV_4,   DIV_8,   DIV_16,
      DIV_64, DIV_128, DIV_256, DIV_512)
     with Size => 3;

   type AHB_Prescaler is record
      Enable : Boolean := False;
      Value  : AHB_Prescaler_Enum := AHB_Prescaler_Enum'First;
   end record with Size => 4;

   for AHB_Prescaler use record
      Enable at 0 range 3 .. 3;
      Value  at 0 range 0 .. 2;
   end record;

   procedure Configure_AHB_Clock_Prescaler
     (Value : AHB_Prescaler);
   --  The AHB clock bus is the CPU clock selected by the AHB prescaler.
   --  Example to create a variable:
   --  AHB_PRE  : AHB_Prescaler := (Enable => True, Value => DIV_2);

   type APB_Prescaler_Enum is
     (DIV_2,  DIV_4,  DIV_8,  DIV_16)
     with Size => 2;

   type APB_Prescaler is record
      Enable : Boolean;
      Value  : APB_Prescaler_Enum := APB_Prescaler_Enum'First;
   end record with Size => 3;

   for APB_Prescaler use record
      Enable at 0 range 2 .. 2;
      Value  at 0 range 0 .. 1;
   end record;

   type APB_Clock_Range is (APB_1, APB_2);

   procedure Configure_APB_Clock_Prescaler
     (Bus   : APB_Clock_Range;
      Value : APB_Prescaler);
   --  The APB1 clock bus is the APB1 peripheral clock selected by the APB1
   --  prescaler.
   --  The APB2 clock bus is the APB2 peripheral clock selected by the APB2
   --  prescaler.
   --  Example to create a variable:
   --  APB_PRE  : APB_Prescaler := (Enable => True, Value => DIV_2);

   ----------------
   -- PLL Clocks --
   ----------------

   type PLL_Clock_Source is
     (PLL_No_Source_PWR_OFF,
      PLL_No_Source,
      PLL_SRC_HSI,
      PLL_SRC_HSE)
     with Size => 2;

   for PLL_Clock_Source use
     (PLL_No_Source_PWR_OFF => 2#00#,
      PLL_No_Source         => 2#01#,
      PLL_SRC_HSI           => 2#10#,
      PLL_SRC_HSE           => 2#11#);

   procedure Configure_PLL_Source_Mux (Source : PLL_Clock_Source);

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

   procedure Configure_PLL
     (Enable          : Boolean;
      PLLM            : PLLM_Range;
      PLLN            : PLLN_Range;
      PLLP            : PLLP_Range;
      Enable_Output_P : Boolean;
      PLLQ            : PLLQ_Range;
      Enable_Output_Q : Boolean;
      PLLR            : PLLR_Range;
      Enable_Output_R : Boolean);
   --  Configure PLL according with RM0440 rev 6 Chapter 7.2.4 section "PLL"
   --  pg 282.

   -------------------
   -- Output Clocks --
   -------------------

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

   procedure Configure_MCO_Output_Clock
     (Source : MCO_Clock_Source;
      Value  : MCO_Prescaler);
   --  Select the source for micro-controller clock output.

   type LSCO_Clock_Source is (LSI, LSE);

   procedure Configure_LSCO_Output_Clock
     (Enable : Boolean;
      Source : LSCO_Clock_Source := LSCO_Clock_Source'First);

   ------------------
   -- Flash Memory --
   ------------------

   --  Flash wait states
   type FLASH_Wait_State is (FWS0, FWS1, FWS2, FWS3, FWS4)
     with Size => 4;

   procedure Set_FLASH_Latency (Latency : FLASH_Wait_State);
   --  Constants for Flash Latency
   --               with VCORE Range 1 boost mode | Range 1 normal mode
   --  000: Zero wait state    0 < HCLK ≤  34 MHz |   0 < HCLK ≤  30 MHz
   --  001: One wait state    34 < HCLK ≤  68 MHz |  30 < HCLK ≤  60 MHz
   --  010: Two wait sates    68 < HCLK ≤ 102 MHz |  60 < HCLK ≤  90 MHz
   --  011: Three wait sates 102 < HCLK ≤ 136 MHz |  90 < HCLK ≤ 120 MHz
   --  100: Four wait sates  136 < HCLK ≤ 170 MHz | 120 < HCLK ≤ 150 MHz
   --  RM0440 STM32G474 pg. 97 chapter 3.3.3 and pg. 129 chapter 3.7.1

   -------------------
   -- VCORE Scaling --
   -------------------

   type VCORE_Scaling_Selection is
     (Range_1,
      Range_2);

   for VCORE_Scaling_Selection use
     (Range_1 => 2#01#,
      Range_2 => 2#10#);

   procedure Set_VCORE_Scaling (Scale : VCORE_Scaling_Selection);
   --  Range 2 => PLL max. is 26 MHz,
   --  Range 1 => PLL max. is 150 MHz,
   --  Range 1 boost => PLL max. is 170 MHz,
   --  See RM0440 rev 6 Chapter 7.2.8 for frequency x voltage scaling.

   procedure PWR_Overdrive_Enable;
   --  Range 1 boost mode (R1MODE = 0) when SYSCLK ≤ 170 MHz.
   --  Range 1 normal mode (R1MODE = 1) when SYSCLK ≤ 150 MHz.
   --  See RM0440 ver 4 pg. 235 chapter 6.1.5 for other modes.

end STM32.RCC;
