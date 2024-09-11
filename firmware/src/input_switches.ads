with Messages; use Messages;

package Input_Switches is

   procedure Init;
   function Get_State (Switch : Input_Switch_Name) return Input_Switch_State;

end Input_Switches;
