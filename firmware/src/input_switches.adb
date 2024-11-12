with Hardware_Configuration; use Hardware_Configuration;
with STM32.GPIO;             use STM32.GPIO;

package body Input_Switches is

   procedure Init is
   begin
      Init_Checker.Report_Init_Started;

      for Point of Switch_Points loop
         Configure_IO (Point, (Mode_In, Floating));
      end loop;

      Init_Checker.Report_Init_Done;
   end Init;

   function Get_State (Switch : Input_Switch_Name) return Input_Switch_State is
   begin
      if Set (Switch_Points (Switch)) then
         return High;
      else
         return Low;
      end if;
   end Get_State;

end Input_Switches;
