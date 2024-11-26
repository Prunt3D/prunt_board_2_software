with Messages;       use Messages;
with Physical_Types; use Physical_Types;
with Init_Checkers;

package Fans is

   procedure Init;
   procedure Reconfigure (Fan : Fan_Name; PWM_Frequency : Fixed_Point_Fan_PWM_Frequency);
   procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale);
   function Get_PWM (Fan : Fan_Name) return PWM_Scale;
   function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter;

private

   type Fan_PWMs is array (Fan_Name) of Fixed_Point_PWM_Scale;

   protected Fan_Handlers is
      procedure Init;
      procedure Reconfigure (Fan : Fan_Name; PWM_Frequency : Fixed_Point_Fan_PWM_Frequency);
      procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale);
      function Get_PWM (Fan : Fan_Name) return PWM_Scale;
      function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter;
   private
      Last_PWMs : Fan_PWMs := (others => 0.0);
   end Fan_Handlers;

   Init_Checker : Init_Checkers.Init_Checker;

end Fans;
