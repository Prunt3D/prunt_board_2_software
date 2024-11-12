------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with STM32_SVD.RCC;       use STM32_SVD.RCC;
with STM32_SVD.SCB;       use STM32_SVD.SCB;
with System.Machine_Code; use System.Machine_Code;

package body STM32.Power_Control is

   ------------
   -- Enable --
   ------------

   procedure Enable is
   begin
      RCC_Periph.APB1ENR1.PWREN := True;
   end Enable;

   --------------------------------------
   -- Disable_Backup_Domain_Protection --
   --------------------------------------

   procedure Disable_Backup_Domain_Protection is
   begin
      PWR_Periph.CR1.DBP := True;
   end Disable_Backup_Domain_Protection;

   -------------------------------------
   -- Enable_Backup_Domain_Protection --
   -------------------------------------

   procedure Enable_Backup_Domain_Protection is
   begin
      PWR_Periph.CR1.DBP := False;
   end Enable_Backup_Domain_Protection;

   -----------------------
   -- Enable_Wakeup_Pin --
   -----------------------

   procedure Enable_Wakeup_Pin (Pin : Wakeup_Pin) is
   begin
      PWR_Periph.CR3.EWUP.Arr (Pin'Enum_Rep) := True;
   end Enable_Wakeup_Pin;

   -----------------
   -- Wakeup_Flag --
   -----------------

   function Wakeup_Flag (Pin : Wakeup_Pin) return Boolean
   is (PWR_Periph.SR1.WUF.Arr (Pin'Enum_Rep));

   -----------------------
   -- Clear_Wakeup_Flag --
   -----------------------

   procedure Clear_Wakeup_Flag (Pin : Wakeup_Pin) is
   begin
      PWR_Periph.SCR.CWUF.Arr (Pin'Enum_Rep) := True;
   end Clear_Wakeup_Flag;

   -----------------------
   -- Clear_Wakeup_Flag --
   -----------------------

   procedure Clear_Wakeup_Flag (Pins : Wakeup_Pin_List) is
   begin
      for Pin of Pins loop
         PWR_Periph.SCR.CWUF.Arr (Pin'Enum_Rep) := True;
      end loop;
   end Clear_Wakeup_Flag;

   ------------------
   -- Standby_Flag --
   ------------------

   function Standby_Flag return Boolean
   is (PWR_Periph.SR1.SBF);

   ------------------------
   -- Clear_Standby_Flag --
   ------------------------

   procedure Clear_Standby_Flag is
   begin
      PWR_Periph.SCR.CSBF := True;
   end Clear_Standby_Flag;

   ------------------------------
   -- Set_Power_Down_Deepsleep --
   ------------------------------

   procedure Set_Power_Down_Deepsleep (Enabled : Boolean := True) is
   begin
      PWR_Periph.CR1.LPMS := Low_Power_Mode'Enum_Rep (Shutdown);
      PWR_Periph.CR1.LPR := Enabled;
   end Set_Power_Down_Deepsleep;

   -----------------------------
   -- Set_Low_Power_Deepsleep --
   -----------------------------

   procedure Set_Low_Power_Deepsleep (Enabled : Boolean := True) is
   begin
      PWR_Periph.CR1.LPMS := Low_Power_Mode'Enum_Rep (Stop_1);
      PWR_Periph.CR1.LPR := Enabled;
   end Set_Low_Power_Deepsleep;

   ------------------------
   -- Enter_Standby_Mode --
   ------------------------

   procedure Enter_Standby_Mode is
   begin
      for Pin in Wakeup_Pin'Range loop
         Clear_Wakeup_Flag (Pin);
      end loop;

      Clear_Standby_Flag;

      Set_Power_Down_Deepsleep;

      SCB_Periph.SCR.SLEEPDEEP := True;

      loop
         Asm ("wfi", Volatile => True);
      end loop;
   end Enter_Standby_Mode;

end STM32.Power_Control;
