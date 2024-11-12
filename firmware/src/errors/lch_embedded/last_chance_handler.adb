------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

--  Note this version is for use with the ravenscar-full runtime.

with Error_Handling;
with Server_Communication;
with GNAT.Source_Info;
with System.Machine_Reset;

package body Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Except : Exception_Occurrence) is
   begin
      Error_Handling.Make_Safe;

      if Server_Communication.Is_Init_Done then
         Server_Communication.Transmit_String_Line ("");
         Server_Communication.Transmit_String_Line ("Fatal exception on MCU:");
         Server_Communication.Transmit_String_Line (Exception_Information (Except));
         Server_Communication.Transmit_String_Line ("Compilation date: " & GNAT.Source_Info.Compilation_ISO_Date);
         Server_Communication.Transmit_String_Line ("Compilation time: " & GNAT.Source_Info.Compilation_Time);
         Server_Communication.Transmit_String_Line ("Restarting.");
         Server_Communication.Transmit_Fatal_Exception_Mark;
         Server_Communication.Transmit_String_Line (".....");
      end if;

      System.Machine_Reset.Stop;

      --  No-return procedure...
      loop
         null;
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
