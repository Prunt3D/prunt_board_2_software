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

with Ada.Interrupts.Names;

with STM32.Device;

package body STM32.RNG.Interrupts is

   type Buffer_Content is array (Integer range <>) of UInt32;

   type Ring_Buffer is record
      Content : Buffer_Content (0 .. 9);
      Head    : Integer := 0;
      Tail    : Integer := 0;
   end record;

   --------------
   -- Receiver --
   --------------

   protected Receiver is
      pragma Interrupt_Priority;
      entry Get_Random_32 (Value : out UInt32);
   private

      Last           : UInt32 := 0;
      Buffer         : Ring_Buffer;
      Data_Available : Boolean := False;

      procedure Interrupt_Handler;

      pragma Attach_Handler
        (Interrupt_Handler,
         Ada.Interrupts.Names.RNG_Interrupt);

   end Receiver;

   --------------
   -- Receiver --
   --------------

   protected body Receiver is

      -------------------
      -- Get_Random_32 --
      -------------------

      entry Get_Random_32 (Value : out UInt32)
        when Data_Available
      is
         use STM32.Device;

         Next : constant Integer :=
           (Buffer.Tail + 1) mod Buffer.Content'Length;
      begin
         --  Remove an item from our ring buffer.
         Value := Buffer.Content (Next);
         Buffer.Tail := Next;

         --  If the buffer is empty, make sure we block subsequent callers
         --  until the buffer has something in it.
         if Buffer.Tail = Buffer.Head then
            Data_Available := False;
         end if;

         Enable (RNG_Unit);
      end Get_Random_32;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
         use STM32.Device;

         Current : UInt32;
      begin
         if Status (RNG_Unit, Flag => Seed_Error_Interrupt) then
            Clear_Interrupt_Pending (RNG_Unit, Flag => Seed_Error_Interrupt);

            --  Clear then set the RNGEN bit to reinitialize and restart
            --  the RNG.
            Disable (RNG_Unit);
            Initialize (RNG_Unit);
         end if;

         if Status (RNG_Unit, Flag => Clock_Error_Interrupt) then
            --  TODO: reconfigure the clock and make sure it's okay

            --  Clear the bit.
            Clear_Interrupt_Pending (RNG_Unit, Flag => Clock_Error_Interrupt);
         end if;

         if Status (RNG_Unit, Flag => Data_Ready) then
            Current := Read_Data (RNG_Unit);

            if Current /= Last then
               --  This number is good.
               if (Buffer.Head + 1) mod Buffer.Content'Length = Buffer.Tail
               then
                  --  But our buffer is full.  Turn off the RNG.
                  Disable (RNG_Unit);
               else
                  --  Add this new data to our buffer.
                  Buffer.Head := (Buffer.Head + 1) mod Buffer.Content'Length;
                  Buffer.Content (Buffer.Head) := Current;

                  Data_Available := True;
                  Last := Current;
               end if;
            end if;
         end if;
      end Interrupt_Handler;

   end Receiver;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out RNG_Generator) is
      Discard : UInt32;
   begin
      STM32.Device.Enable_Clock (This);
      Enable_Interrupt (This);
      Enable (This);

      --  Discard the first four randomly generated number, according to
      --  RM0440 rev 6 section 26.3.5.
      for I in 1 .. 4 loop
         Receiver.Get_Random_32 (Discard);
      end loop;
   end Initialize;

   ------------
   -- Random --
   ------------

   function Random (This : RNG_Generator) return UInt32 is
      pragma Unreferenced (This);

      Result : UInt32;
   begin
      Receiver.Get_Random_32 (Result);
      return Result;
   end Random;

end STM32.RNG.Interrupts;
