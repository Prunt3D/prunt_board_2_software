private with Ada.Interrupts.Names;
private with HAL;
with STM32.HRTimers;         use STM32.HRTimers;
with STM32.Device;           use STM32.Device;
with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;

package Step_Generator is

   procedure Init;

   procedure Enqueue (Steps : Step_Delta);
   procedure Setup_Loop (Input_Switch : Input_Switch_Name; Until_State : Input_Switch_State);
   procedure Enqueue_Start_Loop;
   procedure Enqueue_Stop_Loop;
   function Check_If_Idle return Boolean;
   procedure Force_Start;

   Empty_Buffer_Error : exception;

private

   use HAL;

   type Step_Delta_Buffer_Index is mod 2**13;

   type Step_Delta_Buffer_Type is array (Step_Delta_Buffer_Index) of Step_Delta with
     Volatile_Components, Pack;

   Step_Delta_Buffer : Step_Delta_Buffer_Type with
     Volatile;

   Step_Delta_Buffer_Writer_Index : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;
   Step_Delta_Buffer_Reader_Index : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;

   Step_Delta_Buffer_Loop_Start_Index : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;
   Step_Delta_Buffer_Loop_End_Index   : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;

   Step_Delta_Buffer_Loop_Enabled : Boolean := False with
     Volatile, Atomic;

   Loop_Until_State  : Input_Switch_State with
     Volatile;
   Loop_Input_Switch : Input_Switch_Name with
     Volatile;

   Is_Idle : Boolean := True with
     Volatile, Atomic;

   Buffer_Ran_Dry : Boolean := False with
     Volatile, Atomic;

   type Step_Period_Parameters is record
      Period : UInt16;
      Toggle : UInt16;
   end record;

   protected Timer_Reload_Handler is
      pragma Interrupt_Priority (Step_Generator_Interrupt_Priority);
   private
      procedure Master_Update_Handler with
        Attach_Handler => Ada.Interrupts.Names.HRTIM_Master_IRQn_Interrupt;
   end Timer_Reload_Handler;

   HRTim_Map : constant array (Stepper_Name) of access HRTimer_Channel :=
     (Stepper_1 => STM32.Device.HRTimer_C'Access,
      Stepper_2 => STM32.Device.HRTimer_D'Access,
      Stepper_3 => STM32.Device.HRTimer_E'Access,
      Stepper_4 => STM32.Device.HRTimer_A'Access,
      Stepper_5 => STM32.Device.HRTimer_B'Access,
      Stepper_6 => STM32.Device.HRTimer_F'Access);

   Step_Count_To_Period : constant array (Step_Count) of UInt16 :=
     (0 => 0, for I in 1 .. Step_Count'Last => 60_000 / UInt16 (I));

end Step_Generator;
