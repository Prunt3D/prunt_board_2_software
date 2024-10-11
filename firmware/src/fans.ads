with Messages;       use Messages;
with Physical_Types; use Physical_Types;
with Init_Checkers;

package Fans is

   procedure Init;
   procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale);
   function Get_PWM (Fan : Fan_Name) return PWM_Scale;
   function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter;

private

   Init_Checker : Init_Checkers.Init_Checker;

end Fans;
