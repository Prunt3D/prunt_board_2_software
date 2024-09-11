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
--   @file    stm32f4xx_hal_rng.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of RNG HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides the API for the random number generator on the STM32F4
--  (ARM Cortex M4F) microcontrollers from ST Microelectronics.
--
--  See the child packages for routines to initialze the generator and acquire
--  numbers, using either polling or interrupts.

with STM32_SVD.RNG;

package STM32.RNG is

   type RNG_Generator is limited private;

   procedure Enable (This : in out RNG_Generator)
     with Post => Enabled (This);

   procedure Disable (This : in out RNG_Generator)
     with Post => not Enabled (This);

   function Enabled (This : RNG_Generator) return Boolean;

   procedure Enable_Interrupt (This : in out RNG_Generator)
     with Inline,
       Post => Interrupt_Enabled (This);

   procedure Disable_Interrupt (This : in out RNG_Generator)
     with Inline,
       Post => not Interrupt_Enabled (This);

   function Interrupt_Enabled (This : RNG_Generator) return Boolean;

   function Read_Data (This : RNG_Generator) return UInt32;

   type Status_Flag is
     (Data_Ready,
      Clock_Error,
      Seed_Error,
      Clock_Error_Interrupt,
      Seed_Error_Interrupt);

   subtype Clearable_Status_Flag is Status_Flag
     range Clock_Error_Interrupt .. Seed_Error_Interrupt;

   function Status (This : RNG_Generator; Flag : Status_Flag) return Boolean;

   procedure Clear_Interrupt_Pending
     (This : in out RNG_Generator;
      Flag : Clearable_Status_Flag)
     with Inline,
       Post => not Status (This, Flag);

private

   type RNG_Generator is new STM32_SVD.RNG.RNG_Peripheral;

end STM32.RNG;
