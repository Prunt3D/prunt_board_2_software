with Ada.Real_Time;       use Ada.Real_Time;
with System.Machine_Code; use System.Machine_Code;
with STM32_SVD.Flash;     use STM32_SVD.Flash;
with STM32_SVD.SYSCFG;

package body STM32.Flash is

   procedure Disable_Cache (This : in out Flash_Memory) is
   begin
      --  TODO: We should save and restore the state.
      This.ACR.ICEN  := False;
      This.ACR.ICRST := True;
      This.ACR.ICRST := False;
      This.ACR.DCEN  := False;
      This.ACR.DCRST := True;
      This.ACR.DCRST := False;
   end Disable_Cache;

   procedure Enable_Cache (This : in out Flash_Memory) is
   begin
      --  TODO: We should save and restore the state.
      This.ACR.ICRST := True;
      This.ACR.ICRST := False;
      This.ACR.ICEN  := False;
      This.ACR.DCRST := True;
      This.ACR.DCRST := False;
      This.ACR.DCEN  := False;
   end Enable_Cache;

   function Is_Locked (This : in out Flash_Memory) return Boolean is
   begin
      return This.CR.LOCK;
   end Is_Locked;

   function Is_In_Dual_Bank_Mode (This : in out Flash_Memory) return Boolean is
   begin
      return This.OPTR.DBANK;
   end Is_In_Dual_Bank_Mode;

   procedure Erase_Inactive_Bank (This : in out Flash_Memory) is
   begin
      --  TODO: We should probably make this protected in case the user is doing multitasking stuff.
      loop
         exit when not This.SR.BSY;
      end loop;

      if This.SR /= (others => <>) then
         raise Constraint_Error with "Unexpected SR state: " & This.SR'Image;
      end if;

      Disable_Cache (This);

      if This.CR.PER or This.CR.MER1 or This.CR.MER2 then
         raise Constraint_Error with "Erase bits already set. CR state: " & This.CR'Image;
      end if;

      if STM32_SVD.SYSCFG.SYSCFG_Periph.MEMRMP.FB_mode then
         This.CR.MER1 := True;
      else
         This.CR.MER2 := True;
      end if;

      This.CR.STRT := True;

      declare
         Start_Time : constant Time := Clock;
      begin
         loop
            if Clock > Start_Time + Milliseconds (50) then
               raise Constraint_Error with "Erase taking too long. SR state: " & This.SR'Image;
            end if;
            exit when not This.SR.BSY;
         end loop;
      end;

      if This.SR /= (EOP => True, others => <>) then
         raise Constraint_Error with "Unexpected SR state: " & This.SR'Image;
      end if;
      This.SR.EOP := True;

      Enable_Cache (This);
   end Erase_Inactive_Bank;

   procedure Write (This : in out Flash_Memory; Address : System.Address; Data : Flash_Data) is
      type Flash_Array_Type is array (Data'Range, 1 .. 2) of HAL.UInt32 with
        Pack, Volatile, Volatile_Components;
      Flash_Array : Flash_Array_Type with
        Volatile, Address => Address;
   begin
      --  TODO: We should probably make this protected in case the user is doing multitasking stuff.
      loop
         exit when not This.SR.BSY;
      end loop;

      if This.SR /= (others => <>) then
         raise Constraint_Error with "Unexpected SR state: " & This.SR'Image;
      end if;

      Disable_Cache (This);

      This.CR.PG := True;
      for I in Data'Range loop
         Flash_Array (I, 1) := Data (I) (1);
         Asm ("isb 0xF", Volatile => True, Clobber => "memory");
         Flash_Array (I, 2) := Data (I) (2);
         Asm ("isb 0xF", Volatile => True, Clobber => "memory");

         declare
            Start_Time : constant Time := Clock;
         begin
            loop
               if Clock > Start_Time + Milliseconds (5) then
                  raise Constraint_Error with "Write taking too long. SR state: " & This.SR'Image;
               end if;
               exit when not This.SR.BSY;
            end loop;
         end;

         if This.SR /= (EOP => True, others => <>) then
            raise Constraint_Error with "Unexpected SR state: " & This.SR'Image;
         end if;
         This.SR.EOP := True;
      end loop;
      This.CR.PG := False;

      Enable_Cache (This);
   end Write;

   procedure Unlock (This : in out Flash_Memory) is
   begin
      --  TODO: We should probably make this protected in case the user is doing multitasking stuff.
      loop
         exit when not This.SR.BSY;
      end loop;

      This.KEYR := 16#4567_0123#;
      This.KEYR := 16#CDEF_89AB#;
   end Unlock;

   procedure Switch_Active_Bank_And_Reset (This : in out Flash_Memory) is
   begin
      --  TODO: We should probably make this protected in case the user is doing multitasking stuff.
      loop
         exit when not This.SR.BSY;
      end loop;

      This.OPTKEYR := 16#0819_2A3B#;
      This.OPTKEYR := 16#4C5D_6E7F#;

      if This.CR.OPTLOCK then
         raise Constraint_Error with "Option bytes locked.";
      end if;

      if This.SR /= (others => <>) then
         raise Constraint_Error with "Unexpected SR state: " & This.SR'Image;
      end if;

      Disable_Cache (This);

      if STM32_SVD.SYSCFG.SYSCFG_Periph.MEMRMP.FB_mode then
         This.OPTR.BFB2 := False;
      else
         This.OPTR.BFB2 := True;
      end if;

      This.CR.OPTSTRT := True;

      declare
         Start_Time : constant Time := Clock;
      begin
         loop
            if Clock > Start_Time + Milliseconds (50) then
               raise Constraint_Error with "Option write taking too long. SR state: " & This.SR'Image;
            end if;
            exit when not This.SR.BSY;
         end loop;
      end;

      This.CR.OBL_LAUNCH := True;

      declare
         Start_Time : constant Time := Clock;
      begin
         loop
            exit when Clock > Start_Time + Milliseconds (50);
         end loop;
      end;

      raise Constraint_Error with "MCU was meant to reset.";
   end Switch_Active_Bank_And_Reset;

end STM32.Flash;
