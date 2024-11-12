with Init_Checkers;

package High_Power_Switch is

   procedure Init;
   procedure Wait_For_Power_Good;
   procedure Enable;
   procedure Disable;

private

   Init_Checker : Init_Checkers.Init_Checker;

end High_Power_Switch;
