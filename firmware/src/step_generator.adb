with STM32.GPIO; use STM32.GPIO;
with HAL;        use HAL;
with Input_Switches;
with Last_Chance_Handler;

package body Step_Generator is

   procedure Init is
      procedure HRTimer_Init (Timer : in out HRTimer_Channel) is
      begin
         Disable (Timer);

         Configure_Prescaler (Timer, Div_4); --  8x HRTIM clock
         Set_Counter_Mode (Timer, Up);
         Set_Counter_Operating_Mode (Timer, Continuous);
         Configure_Register_Preload_Update (Timer, Master_Update, True);
         Set_Counter_Reset_Event (Timer, Master_Timer_Period, True);
         Set_Period (Timer, Step_Count_To_Period (0));

         --  Direction
         Configure_GTC_PWM_Mode (Timer, CMP3, Counter_Equal_Compare);
         Configure_Channel_Output_Event (Timer, Output_1, Timer_Period, Set_Event, True);
         Configure_Channel_Output_Event (Timer, Output_1, Master_Period, Set_Event, True);
         Configure_Channel_Output_Event (Timer, Output_1, Timer_Compare_3, Reset_Event, True);
         Set_Compare_Value (Timer, Compare_3, 0);
         Set_Channel_Output_Polarity (Timer, Output_1, High);
         Set_Channel_Output (Timer, Output_1, True);

         --  Step
         Configure_GTC_PWM_Mode (Timer, CMP1, Counter_Equal_Compare);
         Configure_Channel_Output_Event (Timer, Output_2, Timer_Compare_1, Set_Event, True);
         Configure_Channel_Output_Event (Timer, Output_2, Timer_Compare_1, Reset_Event, True);
         Set_Compare_Value (Timer, Compare_1, 65_535);
         Set_Channel_Output_Polarity (Timer, Output_2, Low);
         Set_Channel_Output (Timer, Output_2, True);

         Set_Register_Preload (Timer, True);
      end HRTimer_Init;

      Port_Config_AF_13 : constant GPIO_Port_Configuration :=
        (Mode           => Mode_AF,
         Resistors      => Floating,
         AF_Output_Type => Push_Pull,
         AF_Speed       => Speed_100MHz,
         AF             => GPIO_AF_HRTIM1_13);
      Port_Config_AF_3  : constant GPIO_Port_Configuration :=
        (Mode           => Mode_AF,
         Resistors      => Floating,
         AF_Output_Type => Push_Pull,
         AF_Speed       => Speed_100MHz,
         AF             => GPIO_AF_HRTIM1_3);
   begin
      Init_Checker.Report_Init_Started;

      Enable_Clock (STM32.Device.HRTimer_M);
      Enable_Clock (STM32.Device.HRTimer_A);
      Enable_Clock (STM32.Device.HRTimer_B);
      Enable_Clock (STM32.Device.HRTimer_C);
      Enable_Clock (STM32.Device.HRTimer_D);
      Enable_Clock (STM32.Device.HRTimer_E);
      Enable_Clock (STM32.Device.HRTimer_F);

      Configure_DLL_Calibration
        (Calibration_Start => True, Periodic_Calibration => True, Calibration_Rate => tHRTIMx2E11);

      HRTimer_Init (STM32.Device.HRTimer_A);
      HRTimer_Init (STM32.Device.HRTimer_B);
      HRTimer_Init (STM32.Device.HRTimer_C);
      HRTimer_Init (STM32.Device.HRTimer_D);
      HRTimer_Init (STM32.Device.HRTimer_E);
      HRTimer_Init (STM32.Device.HRTimer_F);

      Configure_Prescaler (STM32.Device.HRTimer_M, Div_4); --  8x HRTIM clock
      Set_Counter_Operating_Mode (STM32.Device.HRTimer_M, Continuous);
      Set_Period (STM32.Device.HRTimer_M, 60_000); --  Divide 1200MHz by this value, assuming 150MHz clock.
      Set_Repetition_Counter (STM32.Device.HRTimer_M, 0);
      Configure_Register_Preload_Update (STM32.Device.HRTimer_M, Repetition => True, Burst_DMA => Independent);
      Set_Register_Preload (STM32.Device.HRTimer_M, True);
      Enable_Interrupt (STM32.Device.HRTimer_M, Update_Interrupt);

      Enable
        ((STM32.HRTimers.HRTimer_M,
          STM32.HRTimers.HRTimer_A,
          STM32.HRTimers.HRTimer_B,
          STM32.HRTimers.HRTimer_C,
          STM32.HRTimers.HRTimer_D,
          STM32.HRTimers.HRTimer_E,
          STM32.HRTimers.HRTimer_F));

      Configure_IO (PB12, Port_Config_AF_13);
      Configure_IO (PB13, Port_Config_AF_13);
      Configure_IO (PB14, Port_Config_AF_13);
      Configure_IO (PB15, Port_Config_AF_13);
      Configure_IO (PC6, Port_Config_AF_13);
      Configure_IO (PC7, Port_Config_AF_13);
      Configure_IO (PC8, Port_Config_AF_3);
      Configure_IO (PC9, Port_Config_AF_3);
      Configure_IO (PA8, Port_Config_AF_13);
      Configure_IO (PA9, Port_Config_AF_13);
      Configure_IO (PA10, Port_Config_AF_13);
      Configure_IO (PA11, Port_Config_AF_13);

      Init_Checker.Report_Init_Done;
   end Init;

   procedure Enqueue (Steps : Step_Delta) is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      --  TODO: Ensure that a loop move does exceed the length of the buffer.

      if Buffer_Ran_Dry then
         raise Empty_Buffer_Error with "Step generator ISR indicated that buffer is empty.";
      end if;

      loop
         exit when Step_Delta_Buffer_Writer_Index + 1 /= Step_Delta_Buffer_Reader_Index and
           (if Step_Delta_Buffer_Loop_Enabled then
              Step_Delta_Buffer_Writer_Index + 1 /= Step_Delta_Buffer_Loop_Start_Index
            else True);
         if Step_Delta_Buffer_Loop_Enabled then
            raise Constraint_Error with "Enqueue blocking during loop move.";
         end if;
      end loop;

      Step_Delta_Buffer (Step_Delta_Buffer_Writer_Index) := Steps;
      Step_Delta_Buffer_Writer_Index                     := Step_Delta_Buffer_Writer_Index + 1;

      if Is_Idle and then Step_Delta_Buffer_Writer_Index + 1 = Step_Delta_Buffer_Reader_Index then
         Is_Idle := False;
      end if;
   end Enqueue;

   procedure Setup_Loop (Input_Switch : Input_Switch_Name; Until_State : Input_Switch_State) is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      Loop_Input_Switch := Input_Switch;
      Loop_Until_State  := Until_State;
   end Setup_Loop;

   procedure Enqueue_Start_Loop is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      if Step_Delta_Buffer_Loop_Enabled then
         raise Constraint_Error with "Tried to start loop when loop is already running.";
      end if;

      Step_Delta_Buffer_Loop_Start_Index := Step_Delta_Buffer_Writer_Index;
   end Enqueue_Start_Loop;

   procedure Enqueue_Stop_Loop is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      if Step_Delta_Buffer_Writer_Index = Step_Delta_Buffer_Loop_Start_Index then
         raise Constraint_Error with "Loop has no moves.";
      end if;

      Step_Delta_Buffer_Loop_End_Index := Step_Delta_Buffer_Writer_Index;
      Step_Delta_Buffer_Loop_Enabled   := True;
   end Enqueue_Stop_Loop;

   function Check_If_Idle return Boolean is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      if Is_Idle and then Step_Delta_Buffer_Reader_Index /= Step_Delta_Buffer_Writer_Index then
         Is_Idle := False;
      end if;

      return Is_Idle;
   end Check_If_Idle;

   procedure Force_Start is
   begin
      Init_Checker.Raise_If_Init_Not_Done;

      if Buffer_Ran_Dry then
         raise Empty_Buffer_Error with "Step generator ISR indicated that buffer is empty.";
      end if;

      if Is_Idle and then Step_Delta_Buffer_Reader_Index /= Step_Delta_Buffer_Writer_Index then
         Is_Idle := False;
      end if;
   end Force_Start;

   function Loop_Enqueued return Boolean is
   begin
      return Step_Delta_Buffer_Loop_Enabled;
   end Loop_Enqueued;

   function Enqueue_Would_Block (Number_Of_Steps : Step_Delta_List_Index) return Boolean is
      Free_Spots_Normal : constant Step_Delta_Buffer_Index :=
        Step_Delta_Buffer_Reader_Index - Step_Delta_Buffer_Writer_Index - 1;
      Free_Spots_Loop   : constant Step_Delta_Buffer_Index :=
        Step_Delta_Buffer_Loop_Start_Index - Step_Delta_Buffer_Writer_Index - 1;
   begin
      if Step_Delta_Buffer_Index (Number_Of_Steps) >= Free_Spots_Normal then
         return True;
      elsif Step_Delta_Buffer_Loop_Enabled and Step_Delta_Buffer_Index (Number_Of_Steps) >= Free_Spots_Loop then
         return True;
      else
         return False;
      end if;
   end Enqueue_Would_Block;

   protected body Timer_Reload_Handler is
      procedure Master_Update_Handler is
         Steps : Step_Delta_Steps renames Step_Delta_Buffer (Step_Delta_Buffer_Reader_Index).Steps;
         Dirs  : Step_Delta_Dirs renames Step_Delta_Buffer (Step_Delta_Buffer_Reader_Index).Dirs;
      begin
         Clear_Pending_Interrupt (STM32.Device.HRTimer_M, Update_Interrupt);

         if Is_Idle or Buffer_Ran_Dry then
            return;
         end if;

         for S in Stepper_Name loop
            Set_Period (HRTim_Map (S).all, Step_Count_To_Period (Steps (S)));
            if Steps (S) = 0 then
               Set_Compare_Value (HRTim_Map (S).all, Compare_1, 65_535);
            else
               Set_Compare_Value (HRTim_Map (S).all, Compare_1, Step_Count_To_Period (Steps (S)) / 2);
            end if;
            Set_Compare_Value (HRTim_Map (S).all, Compare_3, (if Dirs (S) = Forward then 0 else 65_535));
         end loop;

         if Step_Delta_Buffer_Loop_Enabled then
            if Input_Switches.Get_State (Loop_Input_Switch) = Loop_Until_State then
               Step_Delta_Buffer_Reader_Index := Step_Delta_Buffer_Loop_End_Index - 1;
               Step_Delta_Buffer_Loop_Enabled := False;
            elsif Step_Delta_Buffer_Reader_Index + 1 = Step_Delta_Buffer_Loop_End_Index then
               Step_Delta_Buffer_Reader_Index := Step_Delta_Buffer_Loop_Start_Index - 1;
            end if;
         end if;

         if Step_Delta_Buffer_Reader_Index + 1 = Step_Delta_Buffer_Writer_Index then
            if Step_Delta_Buffer (Step_Delta_Buffer_Reader_Index).Steps = Step_Delta_Steps'(others => 0) then
               --  We assume that the machine can stop without issue if there are zero steps in a given period, even if
               --  this is not a safe stop point.
               Step_Delta_Buffer_Reader_Index := Step_Delta_Buffer_Reader_Index + 1;
               Is_Idle                        := True;
            else
               --  TODO: Maybe slow down instead of immediately going to zero.
               for S in Stepper_Name loop
                  Set_Period (HRTim_Map (S).all, Step_Count_To_Period (0));
                  Set_Compare_Value (HRTim_Map (S).all, Compare_1, 65_535);
               end loop;
               Buffer_Ran_Dry := True;
            end if;
         else
            Step_Delta_Buffer_Reader_Index := Step_Delta_Buffer_Reader_Index + 1;
         end if;
      exception
         when E : others =>
            Last_Chance_Handler.Last_Chance_Handler (E);
      end Master_Update_Handler;
   end Timer_Reload_Handler;

end Step_Generator;
