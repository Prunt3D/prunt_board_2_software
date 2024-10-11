with Messages; use Messages;
with Init_Checkers;

package Input_Switches is

   procedure Init;
   function Get_State (Switch : Input_Switch_Name) return Input_Switch_State;

private

   Init_Checker : Init_Checkers.Init_Checker;

end Input_Switches;
