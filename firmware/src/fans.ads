with Messages;       use Messages;
with Physical_Types; use Physical_Types;

package Fans is

   procedure Init;
   procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale);
   function Get_PWM (Fan : Fan_Name) return PWM_Scale;

end Fans;
