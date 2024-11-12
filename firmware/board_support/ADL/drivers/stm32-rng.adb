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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_rng.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   RNG HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32_SVD.RCC; use STM32_SVD.RCC;
with STM32_SVD.RNG; use STM32_SVD.RNG;

package body STM32.RNG is

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out RNG_Generator) is
   begin
      This.CR.RNGEN := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out RNG_Generator) is
   begin
      This.CR.RNGEN := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : RNG_Generator) return Boolean is
      (This.CR.RNGEN);

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (This : in out RNG_Generator) is
   begin
      This.CR.IE := True;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (This : in out RNG_Generator) is
   begin
      This.CR.IE := False;
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled (This : RNG_Generator) return Boolean is
      (This.CR.IE);

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (This : RNG_Generator) return UInt32
     is (This.DR);

   ------------
   -- Status --
   ------------

   function Status
     (This : RNG_Generator;
      Flag : Status_Flag) return Boolean
   is
   begin
      case Flag is
         when Data_Ready =>
            return This.SR.DRDY;
         when Clock_Error =>
            return This.SR.CECS;
         when Seed_Error =>
            return This.SR.SECS;
         when Clock_Error_Interrupt =>
            return This.SR.CEIS;
         when Seed_Error_Interrupt =>
            return This.SR.SEIS;
      end case;
   end Status;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This : in out RNG_Generator;
      Flag : Clearable_Status_Flag)
   is
   begin
      case Flag is
         when Clock_Error_Interrupt =>
            This.SR.CEIS := False;
         when Seed_Error_Interrupt =>
            This.SR.CEIS := False;
      end case;
   end Clear_Interrupt_Pending;

end STM32.RNG;
