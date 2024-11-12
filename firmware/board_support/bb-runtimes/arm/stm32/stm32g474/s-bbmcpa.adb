------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . B B . M C U _ P A R A M E T E R S             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.STM32.PWR; use Interfaces.STM32, Interfaces.STM32.PWR;

package body System.BB.MCU_Parameters is

   --------------------
   -- PWR_Initialize --
   --------------------

   procedure PWR_Initialize
   is
   begin

      --  Range 1 boost mode (R1MODE = 0) when SYSCLK ≤ 170 MHz.
      --  Range 1 normal mode (R1MODE = 1) when SYSCLK ≤ 150 MHz.
      --  See RM0440 ver. 6 pg. 235 chapter 6.1.5 for other modes.
      PWR_Periph.CR5.R1MODE := 0;
      --  Main regulator in range 1 boost mode.

      --  Set the PWR to Range 1 mode.
      PWR_Periph.CR1.VOS := 1;

      --  Wait for stabilization
      loop
         exit when PWR_Periph.SR2.VOSF = 0;
      end loop;

      --  Set the PWR voltage falling threshold and detector
      PWR_Periph.CR2.PLS := 4;
      PWR_Periph.CR2.PVDE := 1;

      --  Disable RTC domain write protection
      PWR_Periph.CR1.DBP := 1;

      --  Wait until voltage supply scaling has completed
      loop
         exit when Interfaces.STM32.PWR.PWR_Periph.SR2.PVDO = 0;
      end loop;

   end PWR_Initialize;

   --------------------------
   -- PWR_Overdrive_Enable --
   --------------------------

   procedure PWR_Overdrive_Enable
   is
   begin
      --  Range 1 boost mode (R1MODE = 0) when SYSCLK ≤ 170 MHz.
      --  Range 1 normal mode (R1MODE = 1) when SYSCLK ≤ 150 MHz.
      --  See RM0440 ver. 4 pg. 235 chapter 6.1.5 for other modes.
      if PWR_Periph.CR1.VOS = 2 then --  Range 2
         PWR_Periph.CR1.VOS := 1; --  put in Range 1
         --  Wait for stabilization
         loop
            exit when PWR_Periph.SR2.VOSF = 0;
         end loop;
      end if;

      PWR_Periph.CR5.R1MODE := 0;
      --  Main regulator in range 1 boost mode.
      --  Wait for stabilization
      loop
         exit when PWR_Periph.SR2.VOSF = 0;
      end loop;

   end PWR_Overdrive_Enable;

end System.BB.MCU_Parameters;
