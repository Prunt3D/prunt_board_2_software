
--  This file provides definitions for the high resolution timers on the
--  STM32F3 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

pragma Restrictions (No_Elaboration_Code);

with System;          use System;

with STM32_SVD.HRTIM; use STM32_SVD.HRTIM, STM32_SVD;

package STM32.HRTimers is

   type HRTimer_Master is limited private;
   type HRTimer_Channel is limited private;

   ----------------------------------------------------------------------------

   --  HRTimer Master functions -----------------------------------------------

   ----------------------------------------------------------------------------

   procedure Enable (This : HRTimer_Master)
     with Post => Enabled (This);

   procedure Disable (This : HRTimer_Master)
     with Post => (if This'Address = HRTIM_Master_Base then
                      -- Test if HRTimer A to F has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TA1OEN or
                               HRTIM_Common_Periph.OENR.TA2OEN) and
                          not (HRTIM_Common_Periph.OENR.TB1OEN or
                               HRTIM_Common_Periph.OENR.TB2OEN) and
                          not (HRTIM_Common_Periph.OENR.TC1OEN or
                               HRTIM_Common_Periph.OENR.TC2OEN) and
                          not (HRTIM_Common_Periph.OENR.TD1OEN or
                               HRTIM_Common_Periph.OENR.TD2OEN) and
                          not (HRTIM_Common_Periph.OENR.TE1OEN or
                               HRTIM_Common_Periph.OENR.TE2OEN) and
                          not (HRTIM_Common_Periph.OENR.TF1OEN or
                               HRTIM_Common_Periph.OENR.TF2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This)));

   function Enabled (This : HRTimer_Master) return Boolean;

   type HRTimer is
     (HRTimer_M, --  Master
      HRTimer_A,
      HRTimer_B,
      HRTimer_C,
      HRTimer_D,
      HRTimer_E,
      HRTimer_F)
     with Size => 7;

   for HRTimer use
     (HRTimer_M => 2#0000001#,
      HRTimer_A => 2#0000010#,
      HRTimer_B => 2#0000100#,
      HRTimer_C => 2#0001000#,
      HRTimer_D => 2#0010000#,
      HRTimer_E => 2#0100000#,
      HRTimer_F => 2#1000000#);

   type HRTimer_List is array (Positive range <>) of HRTimer;

   procedure Enable (Counters : HRTimer_List);
   --  Start all chosen timer counters at the same time.

   procedure Disable (Counters : HRTimer_List);
   --  Stop all chosen timer counters at the same time if they don't have
   --  outputs enabled.

   procedure Set_Register_Preload
     (This    : in out HRTimer_Master;
      Enabled : Boolean);
   --  Enables the registers preload mechanism and defines whether the write
   --  accesses to the memory mapped registers are done into HRTIM active or
   --  preload registers.

   type HRTimer_Prescaler is
     (Div_1, --  fHRCK = 32 * fHRTIM
      Div_2, --  fHRCK = 16 * fHRTIM
      Div_4, --  fHRCK = 8 * fHRTIM
      Div_8, --  fHRCK = 4 * fHRTIM
      Div_16, --  fHRCK = 2 * fHRTIM
      Div_32, --  fHRCK = fHRTIM
      Div_64, --  fHRCK = fHRTIM / 2
      Div_128) --  fHRCK = fHRTIM / 4
     with Size => 3;
   --  The fHRCK input clock to the prescaler is equal to 32 x fHRTIM, so when
   --  the prescaler ratio is 32 (2#101#), fHRCK = fHRTIM. With fHRTIM = 170 MHz,
   --  fHRCK = 5.44 GHz with resolution 184 ps. See RM0440 rev 6 Chapter 27.3.3
   --  "Clocks".

   for HRTimer_Prescaler use
     (Div_1   => 2#000#,
      Div_2   => 2#001#,
      Div_4   => 2#010#,
      Div_8   => 2#011#,
      Div_16  => 2#100#,
      Div_32  => 2#101#,
      Div_64  => 2#110#,
      Div_128 => 2#111#);

   procedure Configure_Prescaler
     (This        : in out HRTimer_Master;
      Prescaler   : HRTimer_Prescaler)
     with Pre => not Enabled (This),
     Post => Current_Prescaler (This) = Prescaler;
   --  The actual prescaler value is 2**Prescaler. The equivalent
   --  counter clock frequency (fCOUNTER) is equal to fHRCK / 2**CKPSC[2:0].
   --  It is mandatory to have the same prescaling factors for all timers
   --  sharing resources (for instance master timer and Timer A must have
   --  identical CKPSC[2:0] values if master timer is controlling HRTIM_CHA1 or
   --  HRTIM_CHA2 outputs).

   function Current_Prescaler (This : HRTimer_Master) return HRTimer_Prescaler;

   type Synchronization_Input_Source is
     (Disabled,
      Internal_Event,
      External_Event);

   for Synchronization_Input_Source use
     (Disabled       => 2#00#,
      Internal_Event => 2#10#,
      External_Event => 2#11#);
   --  When disabled, the HRTIM is not synchronized and runs in standalone mode,
   --  when internal event, it is synchronized with the on-chip timer, when
   --  external event (input pin), a positive pulse on HRTIM_SCIN input triggers
   --  the HRTIM.

   procedure Configure_Synchronization_Input
     (This   : in out HRTimer_Master;
      Source : Synchronization_Input_Source;
      Reset  : Boolean;
      Start  : Boolean)
     with Pre => not Enabled (This);
   --  Define if the synchronization input source reset and/or start the master
   --  timer.

   type Synchronization_Output_Source is
     (Master_Timer_Start,
      Master_Timer_Compare_1_Event,
      Timer_A_StartReset,
      Timer_A_Compare_1_Event);
   --  Define the source and event to be sent on the synchronization outputs
   --  SYNCOUT[2:1]

   type Synchronization_Output_Event is
     (Disabled,
      Positive_Pulse_HRTIM_SCOUT,
      Negative_Pulse_HRTIM_SCOUT);
   --  Define the routing and conditioning of the synchronization output event.

   for Synchronization_Output_Event use
     (Disabled                   => 2#00#,
      Positive_Pulse_HRTIM_SCOUT => 2#10#,
      Negative_Pulse_HRTIM_SCOUT => 2#11#);

   procedure Configure_Synchronization_Output
     (This   : in out HRTimer_Master;
      Source : Synchronization_Output_Source;
      Event  : Synchronization_Output_Event)
     with Pre => not Enabled (This);

   type DAC_Synchronization_Trigger is
     (No_Trigger,
      DACtrigOut1,
      DACtrigOut2,
      DACtrigOut3);

   procedure Configure_DAC_Synchronization_Trigger
     (This    : in out HRTimer_Master;
      Trigger : DAC_Synchronization_Trigger);
   --  A DAC synchronization event can be enabled and generated when the master
   --  timer update occurs. These bits are defining on which output the DAC
   --  synchronization is sent (refer to Section 21.3.19 in RM0364: DAC triggers
   --  for connections details).

   type Burst_DMA_Update_Mode is
     (Independent,
      DMA_Burst_Complete,
      Master_RollOver);

   procedure Configure_Register_Preload_Update
     (This       : in out HRTimer_Master;
      Repetition : Boolean;
      Burst_DMA  : Burst_DMA_Update_Mode)
     with Pre => HRTIM_Master_Periph.MCR.BRSTDMA = 2#00# or
                 HRTIM_Master_Periph.MCR.BRSTDMA = 2#01#;
   --  Defines whether an update occurs when the master timer repetition period
   --  is completed (either due to roll-over or reset events) and if it starts
   --  Interrupt and/or DMA requests.
   --  MREPU can be set only if BRSTDMA[1:0] = 00 or 01.
   --
   --  Define how the update occurs relatively to a burst DMA transaction:
   --  update done independently from the DMA burst transfer completion;
   --  update done when the DMA burst transfer is completed; update done on
   --  master timer roll-over following a DMA burst transfer completion (this
   --  mode only works in continuous mode).

   procedure Set_HalfPeriod_Mode
     (This : in out HRTimer_Master;
      Mode : Boolean);
   --  Enables the half duty-cycle mode: the HRTIM_MCMP1xR active register is
   --  automatically updated with HRTIM_MPERxR/2 value when HRTIM_MPERxR
   --  register is written.

   procedure Set_Period (This : in out HRTimer_Master;  Value : UInt16)
     with Post => Current_Period (This) = Value;

   function Current_Period (This : HRTimer_Master) return UInt16;

   procedure Configure
     (This      : in out HRTimer_Master;
      Prescaler : HRTimer_Prescaler;
      Period    : UInt16)
     with Pre => not Enabled (This),
          Post => Current_Prescaler (This) = Prescaler and
                  Current_Period (This) = Period;

   procedure Compute_Prescaler_And_Period
     (This                : HRTimer_Master;
      Requested_Frequency : UInt32;
      Prescaler           : out HRTimer_Prescaler;
      Period              : out UInt32)
     with Pre => Requested_Frequency > 0;
   --  Computes the minimum prescaler and thus the maximum resolution for the
   --  given timer, based on the system clocks and the requested frequency.
   --  Computes the period required for the requested frequency.

   type Counter_Operating_Mode is
     (SingleShot_NonRetriggerable,
      SingleShot_Retriggerable,
      Continuous);

   procedure Set_Counter_Operating_Mode
     (This : in out HRTimer_Master;
      Mode : Counter_Operating_Mode);
   --  The timer operates in single-shot mode and stops when it reaches the
   --  MPER value or operates in continuous (free-running) mode and rolls over
   --  to zero when it reaches the MPER value. In single-shot mode it may be
   --  not re-triggerable - a counter reset can be done only if the counter is
   --  stoped (period elapsed), or re-triggerable - a counter reset is done
   --  whatever the counter state (running or stopped).
   --  See pg. 976 in RM0440 rev. 6.

   procedure Set_Counter (This : in out HRTimer_Master;  Value : UInt16)
     with Post => Current_Counter (This) = Value;

   function Current_Counter (This : HRTimer_Master) return UInt16;

   procedure Set_Repetition_Counter
     (This        : in out HRTimer_Master;
      Repetitions : UInt8)
     with Post => Current_Repetition_Counter (This) = Repetitions;
   --  The repetition period value for the master counter. It  holds either
   --  the content of the preload register or the content of the active
   --  register if preload is disabled.

   function Current_Repetition_Counter (This : HRTimer_Master) return UInt8;

   procedure Configure_Repetition_Counter
     (This        : in out HRTimer_Master;
      Repetitions : UInt8;
      Interrupt   : Boolean;
      DMA_Request : Boolean)
     with Post => Current_Repetition_Counter (This) = Repetitions;
   --  Defines the repetition period and whether the master timer starts
   --  Interrupt and/or DMA requests at the end of repetition.

   type HRTimer_Compare_Number is
     (Compare_1,
      Compare_2,
      Compare_3,
      Compare_4);

   procedure Set_Compare_Value
     (This    : in out HRTimer_Master;
      Compare : HRTimer_Compare_Number;
      Value   : UInt16)
     with Post => Read_Compare_Value (This, Compare) = Value;
   --  Set the value for Compare registers 1 to 4.

   function Read_Compare_Value
     (This    : HRTimer_Master;
      Compare : HRTimer_Compare_Number) return UInt16;
   --  Read the value for Compare registers 1 to 4.

   type HRTimer_Master_Interrupt is
     (Compare_1_Interrupt,
      Compare_2_Interrupt,
      Compare_3_Interrupt,
      Compare_4_Interrupt,
      Repetition_Interrupt,
      Sync_Input_Interrupt,
      Update_Interrupt);

   procedure Enable_Interrupt
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_Interrupt)
     with Post => Interrupt_Enabled (This, Source);

   type HRTimer_Master_Interrupt_List is
     array (Positive range <>) of HRTimer_Master_Interrupt;

   procedure Enable_Interrupt
     (This    : in out HRTimer_Master;
      Sources : HRTimer_Master_Interrupt_List)
     with
       Post => (for all Source of Sources => Interrupt_Enabled (This, Source));

   procedure Disable_Interrupt
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_Interrupt)
     with Post => not Interrupt_Enabled (This, Source);

   function Interrupt_Enabled
     (This   : HRTimer_Master;
      Source : HRTimer_Master_Interrupt) return Boolean;

   function Interrupt_Status
     (This   : HRTimer_Master;
      Source : HRTimer_Master_Interrupt) return Boolean;

   procedure Clear_Pending_Interrupt
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_Interrupt);

   type HRTimer_Master_DMA_Request is
     (Compare_1_DMA,
      Compare_2_DMA,
      Compare_3_DMA,
      Compare_4_DMA,
      Repetition_DMA,
      Sync_Input_DMA,
      Update_DMA);

   procedure Enable_DMA_Source
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_DMA_Request)
     with Post => DMA_Source_Enabled (This, Source);

   procedure Disable_DMA_Source
     (This   : in out HRTimer_Master;
      Source : HRTimer_Master_DMA_Request)
     with Post => not DMA_Source_Enabled (This, Source);

   function DMA_Source_Enabled
     (This   : HRTimer_Master;
      Source : HRTimer_Master_DMA_Request) return Boolean;


   ----------------------------------------------------------------------------

   --  HRTimer A to E functions -----------------------------------------------

   ----------------------------------------------------------------------------

   procedure Enable (This : HRTimer_Channel)
     with Post => Enabled (This);

   procedure Disable (This : HRTimer_Channel)
     with Post => (if This'Address = HRTIM_TIMA_Base then
                      -- Test if HRTimer A has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TA1OEN or
                               HRTIM_Common_Periph.OENR.TA2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This))

                   elsif This'Address = HRTIM_TIMB_Base then
                      -- Test if HRTimer B has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TB1OEN or
                               HRTIM_Common_Periph.OENR.TB2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This))

                   elsif This'Address = HRTIM_TIMC_Base then
                      -- Test if HRTimer C has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TC1OEN or
                               HRTIM_Common_Periph.OENR.TC2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This))

                   elsif This'Address = HRTIM_TIMD_Base then
                      -- Test if HRTimer D has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TD1OEN or
                               HRTIM_Common_Periph.OENR.TD2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This))

                   elsif This'Address = HRTIM_TIME_Base then
                      -- Test if HRTimer E has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TE1OEN or
                               HRTIM_Common_Periph.OENR.TE2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This))

                   elsif This'Address = HRTIM_TIMF_Base then
                      -- Test if HRTimer E has no outputs enabled.
                      (if not (HRTIM_Common_Periph.OENR.TF1OEN or
                               HRTIM_Common_Periph.OENR.TF2OEN)
                       then
                          not Enabled (This)
                       else
                          Enabled (This)));

   function Enabled (This : HRTimer_Channel) return Boolean;

   procedure Set_Register_Preload
     (This    : in out HRTimer_Channel;
      Enabled : Boolean);
   --  Enables the registers preload mechanism and defines whether the write
   --  accesses to the memory mapped registers are done into HRTIM active or
   --  preload registers.

   procedure Configure_Prescaler
     (This      : in out HRTimer_Channel;
      Prescaler : HRTimer_Prescaler)
     with Pre => not Enabled (This),
       Post => Current_Prescaler (This) = Prescaler;
   --  The actual prescaler value is 2**Prescaler. For clock prescaling ratios
   --  below 32 (CKPSC[2:0] < 5), the least significant bits of the counter and
   --  capture registers are not significant. The least significant bits cannot
   --  be written (counter register only) and return 0 when read.
   --  See Timer clock and prescaler at pg 854 RM0440 rev 6.
   --  It is mandatory to have the same prescaling factors for all timers
   --  sharing resources (for instance master timer and Timer A must have
   --  identical CKPSC[2:0] values if master timer is controlling HRTIM_CHA1 or
   --  HRTIM_CHA2 outputs).

   function Current_Prescaler (This : HRTimer_Channel) return HRTimer_Prescaler;

   procedure Set_PushPull_Mode (This : in out HRTimer_Channel; Mode : Boolean)
     with Pre => not Enabled (This);
   --  It applies the signals generated by the crossbar to output 1 and output 2
   --  alternatively, on the period basis, maintaining the other output to its
   --  inactive state. The redirection rate (push-pull frequency) is defined by
   --  the timer’s period event. The push-pull period is twice the timer
   --  counting period. The push-pull mode is available when the timer operates
   --  in continuous mode and in single-shot mode.  It also needs to be enabled
   --  when the delayed idle protection is required.

   procedure Configure_Synchronization_Input
     (This   : in out HRTimer_Channel;
      Reset  : Boolean;
      Start  : Boolean);
   --  Define if the synchronization input source reset and/or start the timer.

   procedure Configure_DAC_Synchronization_Trigger
     (This    : in out HRTimer_Channel;
      Trigger : DAC_Synchronization_Trigger);
   --  A DAC synchronization event can be enabled and generated when the master
   --  timer update occurs. These bits are defining on which output the DAC
   --  synchronization is sent (refer to Section 27.3.21 in RM0440: DAC triggers
   --  for connections details).

   type DAC_Step_Trigger is (Compare_2_Event, Output_1_Reset_Event);

   type DAC_Reset_Trigger is (Counter_Reset_RollOver_Event, Output_1_Set_Event);

   procedure Configure_Dual_Channel_DAC_Trigger
     (This   : in out HRTimer_Channel;
      Enable : Boolean;
      Step   : DAC_Step_Trigger;
      Reset  : DAC_Reset_Trigger)
     with Pre => not Enabled (This);
   --  Enable/disable and configure the dual channel triggering mechanism

   type Comparator_AutoDelayed_Mode is (CMP2, CMP4)
     with Size => 1;

   type CMP2_AutoDelayed_Mode is
     (Always_Active,
      Active_After_Capture_1,
      Active_After_Capture_1_Compare_1,
      Active_After_Capture_1_Compare_3)
     with Size => 2;

   type CMP4_AutoDelayed_Mode is
     (Always_Active,
      Active_After_Capture_2,
      Active_After_Capture_2_Compare_1,
      Active_After_Capture_2_Compare_3)
     with Size => 2;

   type CMP_Autodelayed_Mode_Descriptor
     (Selector : Comparator_AutoDelayed_Mode := CMP2) is
     record
        case Selector is
           when CMP2 =>
              AutoDelay_1 : CMP2_AutoDelayed_Mode;
           when CMP4 =>
              AutoDelay_2 : CMP4_AutoDelayed_Mode;
        end case;
     end record with Size => 3;

   for CMP_Autodelayed_Mode_Descriptor use record
      Selector    at 0 range 2 .. 2;
      AutoDelay_1 at 0 range 0 .. 1;
      AutoDelay_2 at 0 range 0 .. 1;
   end record;

   procedure Configure_Comparator_AutoDelayed_Mode
     (This : in out HRTimer_Channel;
      Mode : CMP_Autodelayed_Mode_Descriptor)
     with Pre => not Enabled (This);

   type Triggered_Half_Mode is (User_Set, Hardware_Set);

   procedure Configure_Triggered_Half_Mode
     (This : in out HRTimer_Channel;
      Mode : Triggered_Half_Mode)
     with Pre => not Enabled (This);
   --  Defines whether the compare 2 register is behaving in standard mode
   --  (compare match issued as soon as counter equal compare), or by hardware
   --  as soon as a capture 1 event occurs. It is loaded with the (capture 1
   --  divided by 2) value. The initial value can be written by the user (as
   --  long as TRGHLF is reset), but is ignored once the first capture has been
   --  triggered (the preload mechanism is disabled for CMP2 when the TRGHLF
   --  bit is set).

   type GTC_Comparator is (CMP1, CMP3);
   --  Greater then compare comparator.

   type GTC_PWM_Event is
     (Counter_Equal_Compare,
      Counter_Greater_Then_Compare);
   --  Greater then compare event.

   procedure Configure_GTC_PWM_Mode
     (This : in out HRTimer_Channel;
      Comp : GTC_Comparator;
      Mode : GTC_PWM_Event);
   --  Configure greater then compare event for comparator CMP1 or CMP3.
   --  Compare event is generated when the counter is equal to the compare value
   --  (compare match mode) or when the counter is greater than the compare
   --  value. If the compare value is changed on-the-fly, the new compare value
   --  is compared with the current counter value and an output SET or RESET
   --  can be generated.

   type HRTimer_Update_Event is
     (Repetition_Counter_Reset,
      Counter_Reset,
      TimerA_Update,
      TimerB_Update,
      TimerC_Update,
      TimerD_Update,
      TimerE_Update,
      TimerF_Update,
      Master_Update);

   procedure Configure_Register_Preload_Update
     (This    : in out HRTimer_Channel;
      Event   : HRTimer_Update_Event;
      Enabled : Boolean)
     with Pre => (if This'Address = HRTIM_TIMA_Base then Event /= TimerA_Update
                  elsif This'Address = HRTIM_TIMB_Base then Event /= TimerB_Update
                  elsif This'Address = HRTIM_TIMC_Base then Event /= TimerC_Update
                  elsif This'Address = HRTIM_TIMD_Base then Event /= TimerD_Update
                  elsif This'Address = HRTIM_TIME_Base then Event /= TimerE_Update
                  elsif This'Address = HRTIM_TIMF_Base then Event /= TimerF_Update);
   --  Register update is triggered when the current counter rolls-over  and
   --  HRTIM_REPx = 0, or the current counter reset or rolls-over to 0 after
   --  reaching the period value in continuous mode, or by any other timer
   --  update.

   type Update_Gating_Mode is
     (Independent,
      DMA_Burst_Complete,
      DMA_Burst_Complete_Update_Event,
      Rising_Edge_Input_1, --  TIM16_OC
      Rising_Edge_Input_2, --  TIM17_OC
      Rising_Edge_Input_3, --  TIM6_TRGO
      Rising_Edge_Input_1_Update,
      Rising_Edge_Input_2_Update,
      Rising_Edge_Input_3_Update);

   procedure Set_Update_Gating_Mode
     (This : in out HRTimer_Channel;
      Mode : Update_Gating_Mode);
   --  Define how the update occurs relatively to the burst DMA transaction
   --  and the external update request on update enable inputs 1 to 3 (see
   --  Table 209: Update enable inputs and sources in section 27.3.2 RM0440
   --  rev 6). The update events, can be: MSTU, TFU, TEU, TDU, TCU, TBU, TAU,
   --  TxRSTU, TxREPU.

   procedure Set_HalfPeriod_Mode
     (This : in out HRTimer_Channel;
      Mode : Boolean);
   --  Enables the half duty-cycle mode: the HRTIM_CMP1xR active register is
   --  automatically updated with HRTIM_PERxR/2 value when HRTIM_PERxR register
   --  is written.

   procedure Set_Period (This : in out HRTimer_Channel; Value : UInt16)
     with Post => Current_Period (This) = Value;
   --  The period values must be within a lower and an upper limit related to
   --  the high-resolution implementation and listed in table 216 of the RM0440
   --  chapter 27.3.4.
   --  Prescaler  CKPSC[2:0] Min      Max
   --  Div_1      0          0x0060   0xFFDF
   --  Div_2      1          0x0030   0xFFEF
   --  Div_4      2          0x0018   0xFFF7
   --  Div_8      3          0x000C   0xFFFB
   --  Div_16     4          0x0006   0xFFFD
   --  ≥ Div_32   ≥ 5        0x0003   0xFFFD

   function Current_Period (This : HRTimer_Channel) return UInt16;

   procedure Configure
     (This      : in out HRTimer_Channel;
      Prescaler : HRTimer_Prescaler;
      Period    : UInt16)
     with PRE => not Enabled (This),
          Post => Current_Prescaler (This) = Prescaler and
                  Current_Period (This) = Period;

   procedure Compute_Prescaler_And_Period
     (This                : HRTimer_Channel;
      Requested_Frequency : UInt32;
      Prescaler           : out HRTimer_Prescaler;
      Period              : out UInt32)
     with Pre => Requested_Frequency > 0;
   --  Computes the minimum prescaler and thus the maximum resolution for the
   --  given timer, based on the system clocks and the requested frequency.
   --  Computes the period required for the requested frequency.

   Invalid_Request : exception;
   --  Raised when the requested frequency is too high or too low for the given
   --  timer and system clocks.

   type HRTimer_Counter_Mode is (Up, Down);

   procedure Set_Counter_Mode
     (This : in out HRTimer_Channel;
      Mode : HRTimer_Counter_Mode);
   --  Defines if the counter is operating in up or up-down counting mode.

   procedure Configure
     (This         : in out HRTimer_Channel;
      Prescaler    : HRTimer_Prescaler;
      Period       : UInt16;
      Counter_Mode : HRTimer_Counter_Mode)
     with Pre => not Enabled (This),
          Post => Current_Prescaler (This) = Prescaler and
                  Current_Period (This) = Period;

   type UpDown_RollOver_Mode is
     (Counter_Equal_Zero_Or_Period,
      Counter_Equal_Zero,
      Counter_Equal_Period);

   type UpDown_RollOver_Event is
     (Update_IRQ_DMA,
      --  Concerns the roll-over event with the following destinations: update
      --  trigger (to transfer content from preload to active registers), IRQ
      --  and DMA requests, repetition counter decrement and external event
      --  filtering.
      Output,
      --  Concerns the roll-over event which sets and/or resets the ouputs, as
      --  per HRTIM_SETxyR and HRTIM_RSTxyR settings.
      ADC,
      --  Concerns the roll-over event which triggers the ADC.
      Burst_Mode,
      --  Concerns the roll-over event used in the burst mode controller, as
      --  clock as burst mode trigger.
      Fault_And_Event
      --  Concerns the roll-over event used by the fault and event counters.
     );

   procedure Configure_UpDown_RollOver_Mode
     (This  : in out HRTimer_Channel;
      Mode  : UpDown_RollOver_Mode;
      Event : UpDown_RollOver_Event);
   --  Configure when the roll-over is generated, in up-down counting mode.

   procedure Set_Counter_Operating_Mode
     (This : in out HRTimer_Channel;
      Mode : Counter_Operating_Mode);
   --  The timer operates in single-shot mode and stops when it reaches the
   --  MPER value or operates in continuous (free-running) mode and rolls over
   --  to zero when it reaches the MPER value. In single-shot mode it may be
   --  not re-triggerable - a counter reset can be done only if the counter is
   --  stoped (period elapsed), or re-triggerable - a counter reset is done
   --  whatever the counter state (running or stopped).
   --  See pg. 976 in RM0440 rev. 6.

   procedure Set_Counter_Value (This : in out HRTimer_Channel; Value : UInt16)
     with Post => Current_Counter_Value (This) = Value;

   function Current_Counter_Value (This : HRTimer_Channel) return UInt16;

   procedure Set_Repetition_Counter
     (This        : in out HRTimer_Channel;
      Repetitions : UInt8)
     with Post => Current_Repetition_Counter (This) = Repetitions;
   --  The repetition counter is initialized with the content of the HRTIM_REPxR
   --  register when the timer is enabled (TXCEN bit set). Once the timer has
   --  been enabled, any time the counter is cleared, either due to a reset
   --  event or due to a counter roll-over, the repetition counter is decreased.
   --  When it reaches zero, a REP interrupt or a DMA request is issued if
   --  enabled (REPIE and REPDE bits in the HRTIM_DIER register).

   function Current_Repetition_Counter (This : HRTimer_Channel) return UInt8;

   procedure Configure_Repetition_Counter
     (This        : in out HRTimer_Channel;
      Repetitions : UInt8;
      Interrupt   : Boolean;
      DMA_Request : Boolean)
     with Post => Current_Repetition_Counter (This) = Repetitions;
   --  The repetition counter is initialized with the content of the HRTIM_REPxR
   --  register when the timer is enabled (TXCEN bit set). Once the timer has
   --  been enabled, any time the counter is cleared, either due to a reset
   --  event or due to a counter roll-over, the repetition counter is decreased.
   --  When it reaches zero, a REP interrupt or a DMA request is issued if
   --  enabled (REPIE and REPDE bits in the HRTIM_DIER register).

   type Counter_Reset_Event is
     (Option_1,
      Timer_Update,
      Timer_Compare_2,
      Timer_Compare_4,
      Master_Timer_Period,
      Master_Compare_1,
      Master_Compare_2,
      Master_Compare_3,
      Master_Compare_4,
      External_Event_1,
      External_Event_2,
      External_Event_3,
      External_Event_4,
      External_Event_5,
      External_Event_6,
      External_Event_7,
      External_Event_8,
      External_Event_9,
      External_Event_10,
      Option_20,
      Option_21,
      Option_22,
      Option_23,
      Option_24,
      Option_25,
      Option_26,
      Option_27,
      Option_28,
      Option_29,
      Option_30,
      Option_31,
      Option_32)
     with Size => 32;
   --  The HRTimer A to F are reset upon these events.
   --  Option   HRTimer_A      HRTimer_B      HRTimer_C
   --  1        Timer_F_CMP_1  Timer_F_CMP_1  Timer_F_CMP_1
   --  20       Timer_B_CMP_1  Timer_A_CMP_1  Timer_A_CMP_1
   --  21       Timer_B_CMP_2  Timer_A_CMP_2  Timer_A_CMP_2
   --  22       Timer_B_CMP_4  Timer_A_CMP_4  Timer_A_CMP_4
   --  23       Timer_C_CMP_1  Timer_C_CMP_1  Timer_B_CMP_1
   --  24       Timer_C_CMP_2  Timer_C_CMP_2  Timer_B_CMP_2
   --  25       Timer_C_CMP_4  Timer_C_CMP_4  Timer_B_CMP_4
   --  26       Timer_D_CMP_1  Timer_D_CMP_1  Timer_D_CMP_1
   --  27       Timer_D_CMP_2  Timer_D_CMP_2  Timer_D_CMP_2
   --  28       Timer_D_CMP_4  Timer_D_CMP_4  Timer_D_CMP_4
   --  29       Timer_E_CMP_1  Timer_E_CMP_1  Timer_E_CMP_1
   --  30       Timer_E_CMP_2  Timer_E_CMP_2  Timer_E_CMP_2
   --  31       Timer_E_CMP_4  Timer_E_CMP_4  Timer_E_CMP_4
   --  32       Timer_F_CMP_2  Timer_F_CMP_2  Timer_F_CMP_2
   --
   --  Option   HRTimer_D      HRTimer_E      HRTimer_F
   --  1        Timer_F_CMP_1  Timer_F_CMP_1  Timer_E_CMP_1
   --  20       Timer_A_CMP_1  Timer_A_CMP_1  Timer_A_CMP_1
   --  21       Timer_A_CMP_2  Timer_A_CMP_2  Timer_A_CMP_2
   --  22       Timer_A_CMP_4  Timer_A_CMP_4  Timer_A_CMP_4
   --  23       Timer_B_CMP_1  Timer_B_CMP_1  Timer_B_CMP_1
   --  24       Timer_B_CMP_2  Timer_B_CMP_2  Timer_B_CMP_2
   --  25       Timer_B_CMP_4  Timer_B_CMP_4  Timer_B_CMP_4
   --  26       Timer_C_CMP_1  Timer_C_CMP_1  Timer_C_CMP_1
   --  27       Timer_C_CMP_2  Timer_C_CMP_2  Timer_C_CMP_2
   --  28       Timer_C_CMP_4  Timer_C_CMP_4  Timer_C_CMP_4
   --  29       Timer_E_CMP_1  Timer_D_CMP_1  Timer_D_CMP_1
   --  30       Timer_E_CMP_2  Timer_D_CMP_2  Timer_D_CMP_2
   --  31       Timer_E_CMP_4  Timer_D_CMP_4  Timer_D_CMP_4
   --  32       Timer_F_CMP_2  Timer_F_CMP_2  Timer_E_CMP_2

   procedure Set_Counter_Reset_Event
     (This   : in out HRTimer_Channel;
      Event  : Counter_Reset_Event;
      Enable : Boolean);

   type HRTimer_Capture_Compare_State is (Disable, Enable);

   procedure Set_Compare_Value
     (This    : in out HRTimer_Channel;
      Compare : HRTimer_Compare_Number;
      Value   : UInt16)
     with Post => Current_Compare_Value (This, Compare) = Value;
   --  Set the value for Compare registers 1 to 4. A compare value greater than
   --  the period register value will not generate a compare match event.
   --  The compare values must be within a lower and an upper limit related to
   --  the high-resolution implementation and listed in table 216 of the RM0440
   --  chapter 27.3.4.
   --  Prescaler  CKPSC[2:0] Min      Max
   --  Div_1      0          0x0060   0xFFDF
   --  Div_2      1          0x0030   0xFFEF
   --  Div_4      2          0x0018   0xFFF7
   --  Div_8      3          0x000C   0xFFFB
   --  Div_16     4          0x0006   0xFFFD
   --  ≥ Div_32   ≥ 5        0x0003   0xFFFD


   function Current_Compare_Value
     (This    : HRTimer_Channel;
      Compare : HRTimer_Compare_Number) return UInt16;
   --  Read the value for Compare registers 1 to 4.

   type HRTimer_Capture_Number is
     (Capture_1,
      Capture_2);

   function Current_Capture_Value
     (This    : HRTimer_Channel;
      Capture : HRTimer_Capture_Number) return UInt16;
   --  Read the counter value when the capture event occurred.

   type HRTimer_Capture_Event is
     (Software,
      Timer_Update,
      External_Event_1,
      External_Event_2,
      External_Event_3,
      External_Event_4,
      External_Event_5,
      External_Event_6,
      External_Event_7,
      External_Event_8,
      External_Event_9,
      External_Event_10,
      Option_13,
      Option_14,
      Option_15,
      Option_16,
      Option_17,
      Option_18,
      Option_19,
      Option_20,
      Option_21,
      Option_22,
      Option_23,
      Option_24,
      Option_25,
      Option_26,
      Option_27,
      Option_28,
      Option_29,
      Option_30,
      Option_31,
      Option_32);
   --  Events that trigger the counter.
   --  Option HRTimer_A              HRTimer_B              HRTimer_C
   --  13     Timer_F_Output_1_Set   Timer_A_Output_1_Set   Timer_A_Output_1_Set
   --  14     Timer_F_Output_1_Reset Timer_A_Output_1_Reset Timer_A_Output_1_Reset
   --  15     Timer_F_Compare_1      Timer_A_Compare_1      Timer_A_Compare_1
   --  16     Timer_F_Compare_2      Timer_A_Compare_2      Timer_A_Compare_2
   --  17     Timer_B_Output_1_Set   Timer_F_Output_1_Set   Timer_B_Output_1_Set
   --  16     Timer_B_Output_1_Reset Timer_F_Output_1_Reset Timer_B_Output_1_Reset
   --  19     Timer_B_Compare_1      Timer_F_Compare_1      Timer_B_Compare_1
   --  20     Timer_B_Compare_2      Timer_F_Compare_2      Timer_B_Compare_2
   --  21     Timer_C_Output_1_Set   Timer_C_Output_1_Set   Timer_F_Output_1_Set
   --  22     Timer_C_Output_1_Reset Timer_C_Output_1_Reset Timer_F_Output_1_Reset
   --  23     Timer_C_Compare_1      Timer_C_Compare_1      Timer_F_Compare_1
   --  24     Timer_C_Compare_2      Timer_C_Compare_2      Timer_F_Compare_2
   --  25     Timer_D_Output_1_Set   Timer_D_Output_1_Set   Timer_D_Output_1_Set
   --  26     Timer_D_Output_1_Reset Timer_D_Output_1_Reset Timer_D_Output_1_Reset
   --  27     Timer_D_Compare_1      Timer_D_Compare_1      Timer_D_Compare_1
   --  28     Timer_D_Compare_2      Timer_D_Compare_2      Timer_D_Compare_2
   --  29     Timer_E_Output_1_Set   Timer_E_Output_1_Set   Timer_E_Output_1_Set
   --  30     Timer_E_Output_1_Reset Timer_E_Output_1_Reset Timer_E_Output_1_Reset
   --  31     Timer_E_Compare_1      Timer_E_Compare_1      Timer_E_Compare_1
   --  32     Timer_E_Compare_2      Timer_E_Compare_2      Timer_E_Compare_2
   --
   --  Option HRTimer_D              HRTimer_E              HRTimer_F
   --  13     Timer_A_Output_1_Set   Timer_A_Output_1_Set   Timer_A_Output_1_Set
   --  14     Timer_A_Output_1_Reset Timer_A_Output_1_Reset Timer_A_Output_1_Reset
   --  15     Timer_A_Compare_1      Timer_A_Compare_1      Timer_A_Compare_1
   --  16     Timer_A_Compare_2      Timer_A_Compare_2      Timer_A_Compare_2
   --  17     Timer_B_Output_1_Set   Timer_B_Output_1_Set   Timer_B_Output_1_Set
   --  16     Timer_B_Output_1_Reset Timer_B_Output_1_Reset Timer_B_Output_1_Reset
   --  19     Timer_B_Compare_1      Timer_B_Compare_1      Timer_B_Compare_1
   --  20     Timer_B_Compare_2      Timer_B_Compare_2      Timer_B_Compare_2
   --  21     Timer_C_Output_1_Set   Timer_C_Output_1_Set   Timer_C_Output_1_Set
   --  22     Timer_C_Output_1_Reset Timer_C_Output_1_Reset Timer_C_Output_1_Reset
   --  23     Timer_C_Compare_1      Timer_C_Compare_1      Timer_C_Compare_1
   --  24     Timer_C_Compare_2      Timer_C_Compare_2      Timer_C_Compare_2
   --  25     Timer_F_Output_1_Set   Timer_D_Output_1_Set   Timer_D_Output_1_Set
   --  26     Timer_F_Output_1_Reset Timer_D_Output_1_Reset Timer_D_Output_1_Reset
   --  27     Timer_F_Compare_1      Timer_D_Compare_1      Timer_D_Compare_1
   --  28     Timer_F_Compare_2      Timer_D_Compare_2      Timer_D_Compare_2
   --  29     Timer_E_Output_1_Set   Timer_F_Output_1_Set   Timer_E_Output_1_Set
   --  30     Timer_E_Output_1_Reset Timer_F_Output_1_Reset Timer_E_Output_1_Reset
   --  31     Timer_E_Compare_1      Timer_F_Compare_1      Timer_E_Compare_1
   --  32     Timer_E_Compare_2      Timer_F_Compare_2      Timer_E_Compare_2

   procedure Set_Capture_Event
     (This    : in out HRTimer_Channel;
      Capture : HRTimer_Capture_Number;
      Event   : HRTimer_Capture_Event;
      Enable  : Boolean);
   --  Enable/disable the event that trigger the counter.

   type HRTimer_Channel_Interrupt is
     (Compare_1_Interrupt,
      Compare_2_Interrupt,
      Compare_3_Interrupt,
      Compare_4_Interrupt,
      Repetition_Interrupt,
      Update_Interrupt,
      Capture_1_Interrupt,
      Capture_2_Interrupt,
      Output_1_Set_Interrupt,
      Output_1_Reset_Interrupt,
      Output_2_Set_Interrupt,
      Output_2_Reset_Interrupt,
      Reset_RollOver_Interrupt,
      Delayed_Protection_Interrupt);

   procedure Enable_Interrupt
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt)
     with Post => Interrupt_Enabled (This, Source);

   type HRTimer_Channel_Interrupt_List is
     array (Positive range <>) of HRTimer_Channel_Interrupt;

   procedure Enable_Interrupt
     (This    : in out HRTimer_Channel;
      Sources : HRTimer_Channel_Interrupt_List)
     with
       Post => (for all Source of Sources => Interrupt_Enabled (This, Source));

   procedure Disable_Interrupt
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt)
     with Post => not Interrupt_Enabled (This, Source);

   function Interrupt_Enabled
     (This   : HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt) return Boolean;

   function Interrupt_Status
     (This   : HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt) return Boolean;

   procedure Clear_Pending_Interrupt
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_Interrupt);

   type HRTimer_Channel_DMA_Request is
     (Compare_1_DMA,
      Compare_2_DMA,
      Compare_3_DMA,
      Compare_4_DMA,
      Repetition_DMA,
      Update_DMA,
      Capture_1_DMA,
      Capture_2_DMA,
      Output_1_Set_DMA,
      Output_1_Reset_DMA,
      Output_2_Set_DMA,
      Output_2_Reset_DMA,
      Reset_RollOver_DMA,
      Delayed_Protection_DMA);

   procedure Enable_DMA_Source
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_DMA_Request)
     with Post => DMA_Source_Enabled (This, Source);

   procedure Disable_DMA_Source
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Channel_DMA_Request)
     with Post => not DMA_Source_Enabled (This, Source);

   function DMA_Source_Enabled
     (This   : HRTimer_Channel;
      Source : HRTimer_Channel_DMA_Request) return Boolean;

   procedure Set_Deadtime (This : in out HRTimer_Channel; Enable : Boolean)
     with Pre  => not Enabled (This) or No_Outputs_Enabled (This),
          Post => Enabled_Deadtime (This) = Enable;
   --  Enable or disable the deadtime. This parameter cannot be changed once
   --  the timer is operating (TxEN bit set) or if its outputs are enabled
   --  and set/reset by another timer. See chapter 27.5.47 in RM0440 rev. 6.

   function Enabled_Deadtime (This : HRTimer_Channel) return Boolean;
   --  Return True if the timer deadtime is enabled.

   type HRTimer_Deadtime_Prescaler is
     (Mul_1,
      Mul_2,
      Mul_4,
      Mul_8,
      Mul_16,
      Mul_32,
      Mul_64,
      Mul_128);
   --  The Deadtime Prescaler input frequency is from HRTIM*8 (so the time is
   --  tHRTIM/8) and determines the time for the Deadtime Generator (DTG).
   --  tDTG is given by (2**(DTPRSC[2:0])) x (tHRTIM / 8).
   --  The maximum Deadtime is given by tDTG output * 511 (UInt9) from the
   --  rising and falling values.

   type HRTimer_Deadtime_Sign is (Positive_Sign, Negative_Sign);

   procedure Configure_Deadtime
     (This          : in out HRTimer_Channel;
      Prescaler     : HRTimer_Deadtime_Prescaler;
      Rising_Value  : UInt9;
      Rising_Sign   : HRTimer_Deadtime_Sign := Positive_Sign;
      Falling_Value : UInt9;
      Falling_Sign  : HRTimer_Deadtime_Sign := Positive_Sign);
   --  Two deadtimes values can be defined in relationship with the rising edge
   --  and the falling edge of the Output 1 reference waveform.
   --  The sign determines whether the deadtime is positive or negative
   --  (overlaping signals). See pg. 875 in RM0440 rev. 6.
   --  The deadtime cannot be used simultaneously with the push-pull mode.

   procedure Configure_Deadtime
     (This          : in out HRTimer_Channel;
      Rising_Value  : Float;
      Rising_Sign   : HRTimer_Deadtime_Sign := Positive_Sign;
      Falling_Value : Float;
      Falling_Sign  : HRTimer_Deadtime_Sign := Positive_Sign);
   --  Two deadtimes values can be defined in relationship with the rising edge
   --  and the falling edge of the Output 1 reference waveform.
   --  The sign determines whether the deadtime is positive or negative
   --  (overlaping signals). See pg. 875 in RM0440 rev. 6.
   --  The deadtime cannot be used simultaneously with the push-pull mode.

   type Deadtime_Lock is record
     Rising_Value  : Boolean;
     Rising_Sign   : Boolean;
     Falling_Value : Boolean;
     Falling_Sign  : Boolean;
   end record;

   procedure Enable_Deadtime_Lock
     (This : in out HRTimer_Channel;
      Lock : Deadtime_Lock)
     with Post => (if Lock.Rising_Value then Read_Deadtime_Lock (This).Rising_Value) and
                  (if Lock.Rising_Sign then Read_Deadtime_Lock (This).Rising_Sign) and
                  (if Lock.Falling_Value then Read_Deadtime_Lock (This).Falling_Value) and
                  (if Lock.Falling_Sign then Read_Deadtime_Lock (This).Falling_Sign);
   --  Prevents the deadtime value and sign to be modified.

   function Read_Deadtime_Lock (This : HRTimer_Channel) return Deadtime_Lock;

   type Output_State is (Low, High);

   procedure Set_Channel_Output_State
     (This  : in out HRTimer_Channel;
      Out_1 : Output_State;
      Out_2 : Output_State);

   type Output_Event is
     (Timer_A_Resynchronization,
      Timer_Period,
      Timer_Compare_1,
      Timer_Compare_2,
      Timer_Compare_3,
      Timer_Compare_4,
      Master_Period,
      Master_Compare_1,
      Master_Compare_2,
      Master_Compare_3,
      Master_Compare_4,
      Timer_Event_1,
      Timer_Event_2,
      Timer_Event_3,
      Timer_Event_4,
      Timer_Event_5,
      Timer_Event_6,
      Timer_Event_7,
      Timer_Event_8,
      Timer_Event_9,
      External_Event_1,
      External_Event_2,
      External_Event_3,
      External_Event_4,
      External_Event_5,
      External_Event_6,
      External_Event_7,
      External_Event_8,
      External_Event_9,
      External_Event_10,
      Register_Update);
   --  These events determine the set/reset crossbar of the outputs, so the
   --  output waveform is established.

   for Output_Event use
     (Timer_A_Resynchronization => 16#01#,
      Timer_Period              => 16#02#,
      Timer_Compare_1           => 16#03#,
      Timer_Compare_2           => 16#04#,
      Timer_Compare_3           => 16#05#,
      Timer_Compare_4           => 16#06#,
      Master_Period             => 16#07#,
      Master_Compare_1          => 16#08#,
      Master_Compare_2          => 16#09#,
      Master_Compare_3          => 16#0A#,
      Master_Compare_4          => 16#0B#,
      Timer_Event_1             => 16#0C#,
      Timer_Event_2             => 16#0D#,
      Timer_Event_3             => 16#0E#,
      Timer_Event_4             => 16#0F#,
      Timer_Event_5             => 16#10#,
      Timer_Event_6             => 16#11#,
      Timer_Event_7             => 16#12#,
      Timer_Event_8             => 16#13#,
      Timer_Event_9             => 16#14#,
      External_Event_1          => 16#15#,
      External_Event_2          => 16#16#,
      External_Event_3          => 16#17#,
      External_Event_4          => 16#18#,
      External_Event_5          => 16#19#,
      External_Event_6          => 16#1A#,
      External_Event_7          => 16#1B#,
      External_Event_8          => 16#1C#,
      External_Event_9          => 16#1D#,
      External_Event_10         => 16#1E#,
      Register_Update           => 16#1F#);

   type HRTimer_Channel_Output is (Output_1, Output_2);

   procedure Configure_Channel_Output_Event
     (This        : in out HRTimer_Channel;
      Output      : HRTimer_Channel_Output;
      Set_Event   : Output_Event;
      Reset_Event : Output_Event)
     with Pre => Set_Event /= Reset_Event;
   --  The output waveform is determined by this set/reset crossbar.
   --  When set and reset requests from two different sources are simultaneous,
   --  the reset action has the highest priority. If the interval between set
   --  and reset requests is below 2 fHRTIM period, the behavior depends on the
   --  time interval and on the alignment with the fHRTIM clock. See chapter
   --  27.3.7 at pg. 887 in the RM0440 rev 6 for set/reset events priorities
   --  and narrow pulses management.

   type Output_Event_Type is (Reset_Event, Set_Event);

   procedure Configure_Channel_Output_Event
     (This       : in out HRTimer_Channel;
      Output     : HRTimer_Channel_Output;
      Event      : Output_Event;
      Event_Type : Output_Event_Type;
      Enabled    : Boolean);
   --  The output waveform is determined by this set/reset crossbar. The event
   --  sources are ORed and multiple events can be simultaneously selected. See
   --  chapter 27.3.4 Set/reset crossbar in the RM0440 rev 7.

   type External_Event_Number is
     (Event_1,
      Event_2,
      Event_3,
      Event_4,
      Event_5,
      Event_6,
      Event_7,
      Event_8,
      Event_9,
      Event_10);

   type External_Event_Latch is (Ignore, Latch);
   --  Event is ignored if it happens during a blank or passed through during
   --  a window, or latched and delayed till the end of the blanking or
   --  windowing period.

   type External_Event_Blanking_Filter is
     (No_Filtering,
      Blanking_from_Counter_to_Compare_1,
      Blanking_from_Counter_to_Compare_2,
      Blanking_from_Counter_to_Compare_3,
      Blanking_from_Counter_to_Compare_4,
      Blanking_from_TIMFLTR1,
      Blanking_from_TIMFLTR2,
      Blanking_from_TIMFLTR3,
      Blanking_from_TIMFLTR4,
      Blanking_from_TIMFLTR5,
      Blanking_from_TIMFLTR6,
      Blanking_from_TIMFLTR7,
      Blanking_from_TIMFLTR8,
      Windowing_from_Counter_to_Compare_2,
      Windowing_from_Counter_to_Compare_3,
      Windowing_from_TIMWIN);

   procedure Configure_External_Event
     (This         : in out HRTimer_Channel;
      Event_Number : External_Event_Number;
      Event_Latch  : External_Event_Latch;
      Event_Filter : External_Event_Blanking_Filter)
     with Pre => not Enabled (This);
   --  The HRTIM timer can handle events not generated within the timer,
   --  referred to as “external event”. These external events come from
   --  multiple sources, either on-chip or off-chip: built-in comparators,
   --  digital input pins (typically connected to off-chip comparators and
   --  zero-crossing detectors), on-chip events for other peripheral (ADC’s
   --  analog watchdogs and general purpose timer trigger outputs).
   --  See chapter 27.3.8 at pg. 890 in RM0440 rev. 6.

   procedure Set_External_Event_A_Counter_Enable
     (This   : in out HRTimer_Channel;
      Enable : Boolean);
   --  Enable/disable the external event counter.

   procedure Set_External_Event_A_Counter_Value
     (This  : in out HRTimer_Channel;
      Value : UInt6)
   with Pre => Value > 0;
   --  Selects the external event A counter threshold. An event is considered
   --  valid when the number of events is equal to the (EEVACNT[5:0]+1) value.

   type External_Event_Counter_Reset is
     (Imediate,
      Reset_RollOver,
      Reset_RollOver_No_Event);
   --  Reset the external event counter now or on each reset/roll-over event or
   --  when the reset/roll-over occurs during last counting period.

   procedure Set_External_Event_A_Counter_Reset
     (This  : in out HRTimer_Channel;
      Event : External_Event_Counter_Reset);
   --  Reset the external event counter now or on each reset/roll-over event or
   --  when the reset/roll-over occurs during last counting period.

   procedure Set_External_Event_A_Source
     (This   : in out HRTimer_Channel;
      Source : External_Event_Number);
   --  Selects the external event A source.

   procedure Configure_External_Event_A
     (This   : in out HRTimer_Channel;
      Enable : Boolean;
      Value  : UInt6;
      Event  : External_Event_Counter_Reset;
      Source : External_Event_Number);
   --  Configure the external event A.

   procedure Set_Chopper_Mode
     (This    : in out HRTimer_Channel;
      Output1 : Boolean;
      Output2 : Boolean)
     with Post => Enabled_Chopper_Mode (This, Output_1) = Output1 and
                  Enabled_Chopper_Mode (This, Output_2) = Output2;
   --  Enable/disable chopper mode for HRTimer channel outputs.

   function Enabled_Chopper_Mode
     (This   : HRTimer_Channel;
      Output : HRTimer_Channel_Output) return Boolean;

   type Chopper_Carrier_Frequency is
     (fHRTIM_Over_16, -- 9 MHz with fHRTIM = 144 MHz
      fHRTIM_Over_32,
      fHRTIM_Over_48,
      fHRTIM_Over_64,
      fHRTIM_Over_80,
      fHRTIM_Over_96,
      fHRTIM_Over_112,
      fHRTIM_Over_128,
      fHRTIM_Over_144,
      fHRTIM_Over_160,
      fHRTIM_Over_176,
      fHRTIM_Over_192,
      fHRTIM_Over_208,
      fHRTIM_Over_224,
      fHRTIM_Over_240,
      fHRTIM_Over_256);
   --  Chopper carrier frequency FCHPFRQ = fHRTIM / (16 x (CARFRQ[3:0]+1)).
   --  This bitfield cannot be modified when one of the CHPx bits is set.

   type Chopper_Duty_Cycle is
     (Zero_Over_Eight, -- Only 1st pulse is present
      One_Over_Eight,
      Two_Over_Eight,
      Three_Over_Eight,
      Four_Over_Eight,
      Five_Over_Eight,
      Six_Over_Eight,
      Seven_Over_Eight);
   --  Duty cycle of the carrier signal. This bitfield cannot be modified
   --  when one of the CHPx bits is set.

   type Chopper_Start_PulseWidth is
     (tHRTIM_x_16, --  111 ns (1/9 MHz) for tHRTIM = 1/144 MHz
      tHRTIM_x_32,
      tHRTIM_x_48,
      tHRTIM_x_64,
      tHRTIM_x_80,
      tHRTIM_x_96,
      tHRTIM_x_112,
      tHRTIM_x_128,
      tHRTIM_x_144,
      tHRTIM_x_160,
      tHRTIM_x_176,
      tHRTIM_x_192,
      tHRTIM_x_208,
      tHRTIM_x_224,
      tHRTIM_x_240,
      tHRTIM_x_256);
   --  Initial pulsewidth following a rising edge on output signal, defined by
   --  t1STPW = tHRTIM x 16 x (STRPW[3:0]+1).
   --  This bitfield cannot be modified when one of the CHPx bits is set.

   procedure Configure_Chopper_Mode
     (This              : in out HRTimer_Channel;
      Output1           : Boolean;
      Output2           : Boolean;
      Carrier_Frequency : Chopper_Carrier_Frequency;
      Duty_Cycle        : Chopper_Duty_Cycle;
      Start_PulseWidth  : Chopper_Start_PulseWidth)
     with Pre => not Enabled (This) and
                 (not Enabled_Chopper_Mode (This, Output_1) or
                  not Enabled_Chopper_Mode (This, Output_2));
   --  A high-frequency carrier can be added on top of the timing unit output
   --  signals to drive isolation transformers. This is done in the output
   --  stage before the polarity insertion to enable chopper on outputs 1 and 2.
   --  See chapter 27.3.16 at pg. 927 in RM0440 rev. 6.

   type Burst_Mode_Idle_Output is
     (No_Action,
      Inactive,
      Active);

   procedure Set_Burst_Mode_Idle_Output
     (This   : in out HRTimer_Channel;
      Output : HRTimer_Channel_Output;
      Mode   : Burst_Mode_Idle_Output)
     with Pre => not Enabled (This) or
                 (Enabled (This) and No_Outputs_Enabled (This));
   --  The output is in idle state when requested by the burst mode controller.

   procedure Set_Deadtime_Burst_Mode_Idle
     (This   : in out HRTimer_Channel;
      Output : HRTimer_Channel_Output;
      Enable : Boolean)
     with Pre => not Enabled (This);
   --  Delay the idle mode entry by forcing a deadtime insertion before
   --  switching the outputs to their idle state.  The deadtime value is set
   --  by DTRx[8:0]. This setting only applies when entering the idle state
   --  during a burst mode operation.

   type HRTimer_State is (Disable, Enable);

   type Channel_Output_Polarity is (High, Low);

   procedure Set_Channel_Output_Polarity
     (This     : in out HRTimer_Channel;
      Output   : HRTimer_Channel_Output;
      Polarity : Channel_Output_Polarity)
     with Pre => not Enabled (This),
          Post => Current_Channel_Output_Polarity (This, Output) = Polarity;

   function Current_Channel_Output_Polarity
     (This     : HRTimer_Channel;
      Output   : HRTimer_Channel_Output) return Channel_Output_Polarity;

   procedure Configure_Channel_Output
     (This       : in out HRTimer_Channel;
      Mode       : Counter_Operating_Mode;
      State      : HRTimer_State;
      Output     : HRTimer_Channel_Output;
      Polarity   : Channel_Output_Polarity;
      Idle_State : Boolean);
   --  Configure parameters for one channel output. If the output set/reset
   --  events are already programmed, then the output state may be enabled,
   --  otherwise it must be disabled, the set/reset events are programmed and
   --  then the output is enabled. If any compare channel is used for set/reset
   --  event, it must be programmed too.

   procedure Configure_Channel_Output
     (This       : in out HRTimer_Channel;
      Mode       : Counter_Operating_Mode;
      State      : HRTimer_State;
      Compare    : HRTimer_Compare_Number;
      Pulse      : UInt16;
      Output     : HRTimer_Channel_Output;
      Polarity   : Channel_Output_Polarity;
      Idle_State : Boolean);
   --  Configure all parameters for one channel output with set/reset events
   --  already programmed (set on timer period and reset on compare value).
   --  The output state may be enabled or disabled.

   procedure Configure_Channel_Output
     (This                     : in out HRTimer_Channel;
      Mode                     : Counter_Operating_Mode;
      State                    : HRTimer_State;
      Polarity                 : Channel_Output_Polarity;
      Idle_State               : Boolean;
      Complementary_Polarity   : Channel_Output_Polarity;
      Complementary_Idle_State : Boolean);
   --  Configure parameters for channel outputs. If the outputs set/reset
   --  events are already programmed, then the output state may be enabled,
   --  otherwise it must be disabled, the set/reset events are programmed and
   --  then the output is enabled. If any compare channel is used for set/reset
   --  event, it must be programmed too.

   procedure Configure_Channel_Output
     (This                     : in out HRTimer_Channel;
      Mode                     : Counter_Operating_Mode;
      State                    : HRTimer_State;
      Compare                  : HRTimer_Compare_Number;
      Pulse                    : UInt16;
      Polarity                 : Channel_Output_Polarity;
      Idle_State               : Boolean;
      Complementary_Polarity   : Channel_Output_Polarity;
      Complementary_Idle_State : Boolean);
   --  Configure all parameters for channel outputs with set/reset events
   --  already programmed (set on timer period and reset on compare value for
   --  Output_1 and vice-versa for Output_2), so the outputs are in opposite
   --  phases. The outputs state may be enabled or disabled.

   type Delayed_Idle_Protection_Enum is
     (Option_1,
      Option_2,
      Option_3,
      Option_4,
      Option_5,
      Option_6,
      Option_7,
      Option_8)
     with Size => 3;
   --  Define the source and outputs on which the delayed protection schemes
   --  are applied.
   --  Option  HRTimer_A                      HRTimer_D
   --          HRTimer_B                      HRTimer_E
   --          HRTimer_D                      HRTimer_F
   --
   --  1       Output_1_External_Event_6      Output_1_External_Event_8
   --  2       Output_2_External_Event_6      Output_2_External_Event_8
   --  3       Output_12_External_Event_6     Output_12External_Event_8
   --  4       Balanced_Idle_External_Event_6 Balanced_Idle_External_Event_8
   --  5       Output_1_External_Event_7      Output_1_External_Event_9
   --  6       Output_2_External_Event_7      Output_2_External_Event_9
   --  7       Output_12_External_Event_7     Output_12External_Event_9
   --  8       Balanced_Idle_External_Event_7 Balanced_Idle_External_Event_9

   type Delayed_Idle_Protection is record
      Enabled : Boolean;
      Value   : Delayed_Idle_Protection_Enum;
   end record with Size => 4;

   for Delayed_Idle_Protection use record
      Enabled at 0 range 3 .. 3;
      Value   at 0 range 0 .. 2;
   end record;

   procedure Set_Delayed_Idle_Protection
     (This   : in out HRTimer_Channel;
      Option : Delayed_Idle_Protection)
     with Pre => not Enabled (This) and
     (if Option.Enabled then
        Current_Delayed_Idle_Protection (This).Enabled);

   function Current_Delayed_Idle_Protection
     (This : in out HRTimer_Channel) return Delayed_Idle_Protection;

   procedure Set_Balanced_Idle_Automatic
     (This   : in out HRTimer_Channel;
      Enable : Boolean)
     with Pre => not Enabled (This);
   --  Select if the outputs are automatically re-enabled after a balanced
   --  idle event.

   type HRTimer_Fault_Source is
     (Fault_1,
      Fault_2,
      Fault_3,
      Fault_4,
      Fault_5,
      Fault_6);

   procedure Set_Fault_Source
     (This   : in out HRTimer_Channel;
      Source : HRTimer_Fault_Source;
      Enable : Boolean)
     with Pre => not Enabled_Fault_Source_Lock (This),
       Post => Enabled_Fault_Source (This, Source) = Enable;
   --  Fault protection circuitry to disable the outputs in case of an
   --  abnormal operation. See chapter 27.3.17 pg. 928 in RM0440 ver. 6.

   function Enabled_Fault_Source
     (This : HRTimer_Channel; Source : HRTimer_Fault_Source) return Boolean;

   procedure Enable_Fault_Source_Lock
     (This : in out HRTimer_Channel)
     with Post => Enabled_Fault_Source_Lock (This);
   --  Prevents the fault value and sign to be modified.

   function Enabled_Fault_Source_Lock (This : HRTimer_Channel) return Boolean;

   ----------------------------------------------------------------------------

   --  HRTimer Common functions -----------------------------------------------

   ----------------------------------------------------------------------------

   type HRTimer_Register_Update is (Enable, Disable, Imediate);

   procedure Set_Register_Update
     (Counter : HRTimer;
      Update  : HRTimer_Register_Update);
   --  The updates are enabled or temporarily disabled to allow the software to
   --  write multiple registers that have to be simultaneously taken into
   --  account. Also forces an immediate transfer from the preload to the
   --  active register in the timer and any pending update request is canceled.

   procedure Enable_Software_Reset (Counter : HRTimer);
   --  Forces a timer reset.

   procedure Set_Register_Update
     (Counters : HRTimer_List;
      Update   : HRTimer_Register_Update);
   --  Updates some/all timer registers simultaneously.
   --  The updates are enabled or temporarily disabled to allow the software to
   --  write multiple registers that have to be simultaneously taken into
   --  account. Also forces an immediate transfer from the preload to the
   --  active register in the timer and any pending update request is canceled.

   procedure Enable_Software_Reset (Counters : HRTimer_List);
   --  Forces a timer reset for some/all timers simultaneously.

   procedure Set_Swap_Outputs (This : HRTimer_Channel; Swap : HRTimer_State);
   --  Swap the timer outputs so that Output_1 becomes output 2 and vice-versa.

   type HRTimer_Common_Interrupt is
     (Fault_1_Interrupt,
      Fault_2_Interrupt,
      Fault_3_Interrupt,
      Fault_4_Interrupt,
      Fault_5_Interrupt,
      System_Fault_Interrupt,
      DLL_Ready_Interrupt,
      Burst_Mode_Period_Interrupt);

   procedure Enable_Interrupt (Source : HRTimer_Common_Interrupt)
     with Post => Interrupt_Enabled (Source);

   type HRTimer_Common_Interrupt_List is
     array (Positive range <>) of HRTimer_Common_Interrupt;

   procedure Enable_Interrupt (Sources : HRTimer_Common_Interrupt_List)
     with Post => (for all Source of Sources => Interrupt_Enabled (Source));

   procedure Disable_Interrupt (Source : HRTimer_Common_Interrupt)
     with Post => not Interrupt_Enabled (Source);

   function Interrupt_Enabled
     (Source : HRTimer_Common_Interrupt) return Boolean;

   function Interrupt_Status
     (Source : HRTimer_Common_Interrupt) return Boolean;

   procedure Clear_Pending_Interrupt (Source : HRTimer_Common_Interrupt);

   procedure Set_Channel_Output
     (This   : HRTimer_Channel;
      Output : HRTimer_Channel_Output;
      Enable : Boolean)
     with Post => (if Output = Output_1 then
                     (if Enable then Output_Status (This)(1) = Enabled
                      else Output_Status (This)(1) = Idle or
                           Output_Status (This)(1) = Fault)
                   elsif Output = Output_2 then
                     (if Enable then Output_Status (This)(2) = Enabled
                      else Output_Status (This)(2) = Idle or
                           Output_Status (This)(2) = Fault));
   --  Enable/disable the output 1 or 2 for any HRTimer A to F.

   procedure Set_Channel_Outputs
     (This   : HRTimer_Channel;
      Enable : Boolean)
     with Inline,
          Post => Enable = not No_Outputs_Enabled (This);
   --  Enable/disable the outputs 1 and 2 for any HRTimer A to F.

   procedure Set_Channel_Outputs
     (Channels : HRTimer_List;
      Enable   : Boolean)
     with Inline;
   --  Enable/disable the outputs 1 and 2 for several HRTimer from A to F.

   type HRTimer_Channel_Output_Status is (Idle, Fault, Enabled);

   type Output_Status_List is
     array (Positive range 1 .. 2) of HRTimer_Channel_Output_Status;

   function Output_Status (This : HRTimer_Channel) return Output_Status_List;

   function Channel_Output_Enabled
     (This   : HRTimer_Channel;
      Output : HRTimer_Channel_Output) return Boolean;

   function No_Outputs_Enabled (This : HRTimer_Channel) return Boolean;
   --  Indicates whether the outputs are disabled for all timer.

   type Burst_Mode_Operating_Mode is (SingleShot, Continuous);

   type Burst_Mode_HRTimer_Mode is (Running, Stopped);
   --  When running, the HRTimer counter clock is maintained and the timer
   --  operates normally. When stopped, the HRTimer counter clock is stopped
   --  and the counter is reset.

   type Burst_Mode_Clock_Source is
     (Master_Timer_Counter, --  Reset/roll-over
      Timer_A_Counter,
      Timer_B_Counter,
      Timer_C_Counter,
      Timer_D_Counter,
      Timer_E_Counter,
      Event_1,
      Event_2,
      Event_3,
      Event_4,
      Prescaler_fHRTIM_Clock);

   type Burst_Mode_Prescaler is
     (Div_1,
      Div_2,
      Div_4,
      Div_8,
      Div_16,
      Div_32,
      Div_64,
      Div_128,
      Div_256,
      Div_512,
      Div_1024,
      Div_2048,
      Div_4096,
      Div_8192,
      Div_16384,
      Div_32768);
   --  Defines the prescaling ratio of the fHRTIM clock for the burst mode
   --  controller.

   procedure Enable_Burst_Mode
     with Pre => HRTIM_Common_Periph.BMPER.BMPER /= 16#0000#,
       Post => Burst_Mode_Enabled;
   --  Starts the burst mode controller, which becomes ready to receive the
   --  start trigger.
   --  The BMPER[15:0] must not be null when the burst mode is enabled.

   procedure Disable_Burst_Mode
     with Post => not Burst_Mode_Enabled;
   --  Causes a burst mode early termination.

   function Burst_Mode_Enabled return Boolean;

   function Burst_Mode_Status return Boolean;
   --  Indicates that a burst operation is on-going.

   procedure Configure_Burst_Mode
     (Operating_Mode : Burst_Mode_Operating_Mode;
      Clock_Source   : Burst_Mode_Clock_Source;
      Prescaler      : Burst_Mode_Prescaler;
      Preload_Enable : Boolean);
   --  The burst mode controller counter can be clocked by several sources:
   --  Masterr timer and HRTIM A..E reset/roll-over events, general purpose
   --  timers or the fHRTIM clock prescaled by a factor. See UM0440 rev 7
   --  chapter 27.3.15 Burst mode controller at section Burst mode clock.

   procedure Configure_HRTimer_Burst_Mode
     (Counter : HRTimer;
      Mode    : Burst_Mode_HRTimer_Mode);

   --  To simplify the bit-field access to the Burst Mode Trigger register,
   --  we remap it as 32-bit registers. This way we program several bits
   --  "oring" them in a 32-bit value, instead of accessing bit-by-bit.

   type Burst_Mode_Trigger_Event is
     (Software_Start,
      Master_Reset,
      Master_Repetition,
      Master_Compare_1,
      Master_Compare_2,
      Master_Compare_3,
      Master_Compare_4,
      HRTimer_A_Reset,
      HRTimer_A_Repetition,
      HRTimer_A_Compare_1,
      HRTimer_A_Compare_2,
      HRTimer_B_Reset,
      HRTimer_B_Repetition,
      HRTimer_B_Compare_1,
      HRTimer_B_Compare_2,
      HRTimer_C_Reset,
      HRTimer_C_Repetition,
      HRTimer_C_Compare_1,
      HRTimer_C_Compare_2,
      HRTimer_D_Reset,
      HRTimer_D_Repetition,
      HRTimer_D_Compare_1,
      HRTimer_D_Compare_2,
      HRTimer_E_Reset,
      HRTimer_E_Repetition,
      HRTimer_E_Compare_1,
      HRTimer_E_Compare_2,
      HRTimer_A_Period_After_EEvent_7,
      HRTimer_D_Period_After_EEvent_8,
      External_Event_7,
      External_Event_8,
      OnChip_Event_Rising_Edge);

   procedure Configure_Burst_Mode_Trigger
     (Trigger : Burst_Mode_Trigger_Event;
      Enabled : Boolean);
   --  Enable/disable each burst mode trigger.

   procedure Set_Burst_Mode_Compare (Value : UInt16)
     with Pre => (if (HRTIM_Common_Periph.BMCR.BMCLK = 2#1010# and
                      HRTIM_Common_Periph.BMCR.BMPRSC = 2#0000#)
                  then HRTIM_Common_Periph.BMCMPR.BMCMP /= 16#0000#);
   --  Defines the number of periods during which the selected timers are in
   --  idle state. This register holds either the content of the preload
   --  register or the content of the active register if the preload is
   --  disabled.
   --  BMCMP[15:0] cannot be set to 0x0000 when using the fHRTIM clock without
   --  a prescaler as the burst mode clock source, (BMCLK[3:0] = 1010 and
   --  BMPRESC[3:0] = 0000).

   procedure Set_Burst_Mode_Period (Value : UInt16);
   --  Defines the burst mode repetition periode (corresponding to the sum of
   --  the idle and run periods). This register holds either the content of the
   --  preload register or the content of the active register if preload is
   --  disabled.

   type External_Event_Source is
     (EExSrc1,
      EExSrc2,
      EExSrc3,
      EExSrc4);

   type External_Event_Polarity is (Active_High, Active_Low);

   type External_Event_Sensitivity is
     (Active_Level, --  defined by polarity bit
      Rising_Edge,
      Falling_Edge,
      Both_Edges);

   type External_Event_Fast_Mode is (fHRTIM_Latency, Low_Latency);
   --  External Event is re-synchronized by the HRTIM logic before acting
   --  on outputs, which adds a fHRTIM clock-related latency, or acts
   --  asynchronously on outputs (low latency).

   type External_Event_Frequency_Filter is
     (No_Filter, --  FLT acts assynchronously
      fHRTIM_N2,
      fHRTIM_N4,
      fHRTIM_N8,
      fFLTS_Over_2_N6,
      fFLTS_Over_2_N8,
      fFLTS_Over_4_N6,
      fFLTS_Over_4_N8,
      fFLTS_Over_8_N6,
      fFLTS_Over_8_N8,
      fFLTS_Over_16_N5,
      fFLTS_Over_16_N6,
      fFLTS_Over_16_N8,
      fFLTS_Over_32_N5,
      fFLTS_Over_32_N6,
      fFLTS_Over_32_N8);

   procedure Configure_External_Event
     (Number      : External_Event_Number;
      Source      : External_Event_Source;
      Polarity    : External_Event_Polarity;
      Sensitivity : External_Event_Sensitivity;
      Fast_Mode   : External_Event_Fast_Mode;
      Filter      : External_Event_Frequency_Filter);
   --  For events 1 to 5, filter has no effect; for events 6 to 10, fast mode
   --  has no effect.

   type External_Event_Sampling_Clock is
     (fHRTIM,
      fHRTIM_Over_2,
      fHRTIM_Over_4,
      fHRTIM_Over_8);

   procedure Set_External_Event_Clock
     (Clock : External_Event_Sampling_Clock);
   --  Set the division ratio between the timer clock frequency (fHRTIM) and
   --  the external event sampling clock (fEEVS) used by the digital filters.

   --  To simplify the bit-field access to the ADC Trigger Source registers,
   --  we remap them as 32-bit registers. This way we program several bits
   --  "oring" them in a 32-bit value, instead of accessing bit-by-bit.

   type ADC_Trigger_Output is
     (ADC_Trigger_1,
      ADC_Trigger_2,
      ADC_Trigger_3,
      ADC_Trigger_4,
      ADC_Trigger_5,
      ADC_Trigger_6,
      ADC_Trigger_7,
      ADC_Trigger_8,
      ADC_Trigger_9,
      ADC_Trigger_10);

   type ADC_Trigger_Source is
     (Master_Compare_1,
      Master_Compare_2,
      Master_Compare_3,
      Master_Compare_4,
      Master_Period,
      Option_6,
      Option_7,
      Option_8,
      Option_9,
      Option_10,
      Option_11,
      Option_12,
      Option_13,
      Option_14,
      Option_15,
      Option_16,
      Option_17,
      Option_18,
      Option_19,
      Option_20,
      Option_21,
      Option_22,
      Option_23,
      Option_24,
      Option_25,
      Option_26,
      Option_27,
      Option_28,
      Option_29,
      Option_30,
      Option_31,
      Option_32)
     with Size => 5;
   --  These bits select the trigger source for the ADC Trigger output.
   --  Option ADC13               ADC24
   --  6      External_Event_1    External_Event_6
   --  7      External_Event_2    External_Event_7
   --  8      External_Event_3    External_Event_8
   --  9      External_Event_4    External_Event_9
   --  10     External_Event_5    External_Event_10
   --  11     HRTimer_F_Compare_2 HRTimer_A_Compare_2
   --  12     HRTimer_A_Compare_3 HRTimer_F_Compare_2
   --  13     HRTimer_A_Compare_4 HRTimer_A_Compare_4
   --  14     HRTimer_A_Period    HRTimer_A_Period
   --  15     HRTimer_A_Reset     HRTimer_B_Compare_2
   --  16     HRTimer_F_Compare_3 HRTimer_F_Compare_3
   --  17     HRTimer_B_Compare_3 HRTimer_B_Compare_4
   --  18     HRTimer_B_Compare_4 HRTimer_B_Period
   --  19     HRTimer_B_Period    HRTimer_C_Compare_2
   --  20     HRTimer_B_Reset     HRTimer_F_Compare_4
   --  21     HRTimer_F_Compare_4 HRTimer_C_Compare_4
   --  22     HRTimer_C_Compare_3 HRTimer_C_Period
   --  23     HRTimer_C_Compare_4 HRTimer_C_Reset
   --  24     HRTimer_C_Period    HRTimer_D_Compare_2
   --  25     HRTimer_F_Period    HRTimer_F_Period
   --  26     HRTimer_D_Compare_3 HRTimer_D_Compare_4
   --  27     HRTimer_D_Compare_4 HRTimer_D_Period
   --  28     HRTimer_D_Period    HRTimer_D_Reset
   --  29     HRTimer_F_Reset     HRTimer_E_Compare_2
   --  30     HRTimer_E_Compare_3 HRTimer_E_Compare_3
   --  31     HRTimer_E_Compare_4 HRTimer_E_Compare_4
   --  32     HRTimer_E_Period    HRTimer_E_Reset
   --  See RM0440 rev 6 chapter 27.5.66.
   --
   --  Option ADC579              ADC6810
   --  6      External_Event_1    External_Event_6
   --  7      External_Event_2    External_Event_7
   --  8      External_Event_3    External_Event_8
   --  9      External_Event_4    External_Event_9
   --  10     External_Event_5    External_Event_10
   --  11     HRTimer_A_Compare_3 HRTimer_A_Compare_2
   --  12     HRTimer_A_Compare_4 HRTimer_A_Compare_4
   --  13     HRTimer_A_Period    HRTimer_A_Period
   --  14     HRTimer_A_Reset     HRTimer_B_Compare_2
   --  15     HRTimer_B_Compare_3 HRTimer_B_Compare_4
   --  16     HRTimer_B_Compare_4 HRTimer_B_Period
   --  17     HRTimer_B_Period    HRTimer_C_Compare_2
   --  18     HRTimer_B_Reset     HRTimer_C_Compare_4
   --  19     HRTimer_C_Compare_3 HRTimer_C_Period
   --  20     HRTimer_C_Compare_4 HRTimer_C_Reset
   --  21     HRTimer_C_Period    HRTimer_D_Compare_2
   --  22     HRTimer_D_Compare_3 HRTimer_D_Compare_4
   --  23     HRTimer_D_Compare_4 HRTimer_D_Period
   --  24     HRTimer_D_Period    HRTimer_D_Reset
   --  25     HRTimer_E_Compare_3 HRTimer_E_Compare_2
   --  26     HRTimer_E_Compare_4 HRTimer_E_Compare_3
   --  27     HRTimer_E_Period    HRTimer_E_Compare_4
   --  28     HRTimer_F_Compare_2 HRTimer_E_Reset
   --  29     HRTimer_F_Compare_3 HRTimer_F_Compare_2
   --  30     HRTimer_F_Compare_4 HRTimer_F_Compare_3
   --  31     HRTimer_F_Period    HRTimer_F_Compare_4
   --  32     HRTimer_F_Reset     HRTimer_F_Period
   --  See RM0440 rev 6 chapter 27.5.76.

   type PostScaler_Divisor is
     (Div_1,
      Div_2,
      Div_3,
      Div_4,
      Div_5,
      Div_6,
      Div_7,
      Div_8,
      Div_9,
      Div_10,
      Div_11,
      Div_12,
      Div_13,
      Div_14,
      Div_15,
      Div_16,
      Div_17,
      Div_18,
      Div_19,
      Div_20,
      Div_21,
      Div_22,
      Div_23,
      Div_24,
      Div_25,
      Div_26,
      Div_27,
      Div_28,
      Div_29,
      Div_30,
      Div_31,
      Div_32)
     with Size => 5;
   --  A post-scaling divisor allow to reduce the ADC trigger rate by its value.
   --  See RM0440 rev 7 chapter 27.3.20.

   procedure Configure_ADC_Trigger
     (Output  : ADC_Trigger_Output;
      Source  : ADC_Trigger_Source;
      Divisor : PostScaler_Divisor);

   procedure Configure_ADC_Trigger
     (Output  : ADC_Trigger_Output;
      Source  : ADC_Trigger_Source;
      Enabled : Boolean);
   --  Multiple triggering is possible within a single switching period by
   --  selecting several sources simultaneously. See chapter 27.3.20 ADC
   --  triggers in the RM0440 rev 7.

   type ADC_Trigger_Update_Source is
     (Master_Timer,
      Timer_A,
      Timer_B,
      Timer_C,
      Timer_D,
      Timer_E,
      Timer_F);

   procedure Configure_ADC_Trigger_Update
     (Output : ADC_Trigger_Output;
      Source : ADC_Trigger_Update_Source);

   type DLL_Calibration is
     (tHRTIMx2E20, --  7.3 ms
      tHRTIMx2E17, --  910 ux
      tHRTIMx2E14, --  114 us
      tHRTIMx2E11  --  14 us
      );

   procedure Configure_DLL_Calibration
     (Calibration_Start    : Boolean;
      Periodic_Calibration : Boolean;
      Calibration_Rate     : DLL_Calibration);
   --  DLL calibration must be done before starting HRTIM master and timing
   --  units. This routine exits only when the calibration is ended and HRTIM is
   --  ready. See RM0440 rev 6 chapter 27.3.24 pg. 953 for the sequence
   --  of initialization.

   procedure Set_Fault_Input
     (Input  : HRTimer_Fault_Source;
      Enable : Boolean);

   function Enabled_Fault_Input
     (Input : HRTimer_Fault_Source) return Boolean;

   type Fault_Input_Polarity is (Active_Low, Active_High);

   type Fault_Input_Source is
     (HRTIM_FLTx_Input_Pin,
      COMPx_Output,
      EEVx_MUXOut_Input_Pin);

   type Fault_Input_Filter is
     (No_Filter, --  FLT acts assynchronously
      fHRTIM_N2,
      fHRTIM_N4,
      fHRTIM_N8,
      fFLTS_Over_2_N6,
      fFLTS_Over_2_N8,
      fFLTS_Over_4_N6,
      fFLTS_Over_4_N8,
      fFLTS_Over_8_N6,
      fFLTS_Over_8_N8,
      fFLTS_Over_16_N5,
      fFLTS_Over_16_N6,
      fFLTS_Over_16_N8,
      fFLTS_Over_32_N5,
      fFLTS_Over_32_N6,
      fFLTS_Over_32_N8);

   procedure Configure_Fault_Input
     (Input    : HRTimer_Fault_Source;
      Enable   : Boolean;
      Polarity : Fault_Input_Polarity;
      Source   : Fault_Input_Source;
      Filter   : Fault_Input_Filter)
     with Pre => not Enabled_Fault_Input_Lock (Input);

   type Fault_Input_Sampling_Clock is
     (fHRTIM,
      fHRTIM_Over_2,
      fHRTIM_Over_4,
      fHRTIM_Over_8);

   procedure Configure_Fault_Input_Clock (Clock : Fault_Input_Sampling_Clock);
   --  Set the division ratio between the timer clock frequency (fHRTIM) and
   --  the fault signal sampling clock (fFLTS) used by the digital filters.

   procedure Enable_Fault_Input_Lock (Input : HRTimer_Fault_Source)
     with Post => Enabled_Fault_Input_Lock (Input);
   --  Prevents the fault enable, polarity, source and filter to be modified.

   function Enabled_Fault_Input_Lock
     (Input : HRTimer_Fault_Source) return Boolean;

   type Fault_Input_Blanking_Source is
     (ResetAligned_Window,
      Moving_Window);
   --  For each option there is a blanking window start and end, for each of the
   --  six fault source:
   --                    ResetAligned_Window          Moving_Window
   --  Fault  Timer      Start            End         Start       End
   --  1      HRTimer_A  Reset/Roll-Over  CMP3_Event  CMP4_Event  CMP3_Event
   --  2      HRTimer_B  Reset/Roll-Over  CMP3_Event  CMP4_Event  CMP3_Event
   --  3      HRTimer_C  Reset/Roll-Over  CMP3_Event  CMP4_Event  CMP3_Event
   --  4      HRTimer_D  Reset/Roll-Over  CMP3_Event  CMP4_Event  CMP3_Event
   --  5      HRTimer_E  Reset/Roll-Over  CMP3_Event  CMP4_Event  CMP3_Event
   --  6      HRTimer_F  Reset/Roll-Over  CMP3_Event  CMP4_Event  CMP3_Event

   procedure Set_Fault_Input_Blanking
     (Input  : HRTimer_Fault_Source;
      Enable : Boolean;
      Source : Fault_Input_Blanking_Source);
   --  Permits to temporary disable the fault input to blank spurious fault
   --  events. See table 235 Fault input blanking events RM0440 rev 6 chapter
   --  27.3.17 pg 931.

   procedure Set_Fault_Input_Counter
     (Input : HRTimer_Fault_Source;
      Count : UInt4);
   --  Selects the FAULTx counter threshold so that a fault is considered valid
   --  when the number of events is equal to the FLTxCNT + 1 value.

   procedure Reset_Fault_Input_Counter (Input : HRTimer_Fault_Source);
   --  Reset the FAULTx counter threshold to 0.

   type Fault_Input_Counter_Reset_Mode is
     (Reset_RollOver_Event,
      Reset_RollOver_Event_No_Fault);

   procedure Set_Fault_Input_Counter_Reset_Mode
     (Input : HRTimer_Fault_Source;
      Mode  : Fault_Input_Counter_Reset_Mode);
   --  Select the FAULTx counter reset on each reset/roll-over event or only if
   --  no fault occured during last counting period.

   --  To simplify the bit-field access to the Burst DMA Timer Update registers,
   --  we remap them as 32-bit registers. This way we program several bits
   --  "oring" them in a 32-bit value, instead of accessing bit-by-bit.
   type Burst_DMA_Master_Update is
     (MCR_Register,
      MICR_Register,
      MDIER_Register,
      MCNTR_Register,
      MPER_Register,
      MREP_Register,
      MCMP1R_Register,
      MCMP2R_Register,
      MCMP3R_Register,
      MCMP4R_Register);

   type Burst_DMA_Master_Update_List is
     array (Natural range <>) of Burst_DMA_Master_Update;

   procedure Set_Burst_DMA_Timer_Update
     (Counter   : HRTimer_Master;
      Registers : Burst_DMA_Master_Update_List;
      Enable    : Boolean);
   --  Defines which master timer register is part of the list of registers
   --  to be updated by the Burst DMA.

   type Burst_DMA_Timer_Channel_Update is
     (HRTIM_TIMxCR_Register,
      HRTIM_TIMxICR_Register,
      HRTIM_TIMxDIER_Register,
      HRTIM_CNTxR_Register,
      HRTIM_PERxR_Register,
      HRTIM_REPxR_Register,
      HRTIM_CMP1xR_Register,
      HRTIM_CMP2xR_Register,
      HRTIM_CMP3xR_Register,
      HRTIM_CMP4xR_Register,
      HRTIM_DTxR_Register,
      HRTIM_SET1xR_Register,
      HRTIM_RST1xR_Register,
      HRTIM_SET2xR_Register,
      HRTIM_RST2xR_Register,
      HRTIM_EEFxR1_Register,
      HRTIM_EEFxR2_Register,
      HRTIM_RSTxR_Register,
      HRTIM_CHPxR_Register,
      HRTIM_OUTxR_Register,
      HRTIM_FLTxR_Register);

   type Burst_DMA_Timer_Channel_Update_List is
     array (Natural range <>) of Burst_DMA_Timer_Channel_Update;

   procedure Set_Burst_DMA_Timer_Update
     (Counter   : HRTimer_Channel;
      Registers : Burst_DMA_Timer_Channel_Update_List;
      Enable    : Boolean);
   --  Defines which timer X register is part of the list of registers
   --  to be updated by the Burst DMA.

   procedure Set_Burst_DMA_Data (Data : UInt32);
   --  Writting this value triggers the copy of the data value into the
   --  registers enabled in BDTxUPR and BDMUPR register bits and the increment
   --  of the register pointer to the next location to be filled.

private

   --  High Resolution Timer: Master
   type HRTimer_Master is new STM32_SVD.HRTIM.HRTIM_Master_Peripheral;

   --  Timerx Control Register
   type TIMxCR_Register is record
      --  HRTIM Timer x Clock prescaler
      CKPSCx         : TIMACR_CKPSCx_Field := 16#0#;
      --  Continuous mode
      CONT           : Boolean := False;
      --  Re-triggerable mode
      RETRIG         : Boolean := False;
      --  Half mode enable
      HALF           : Boolean := False;
      --  Push-Pull mode enable
      PSHPLL         : Boolean := False;
      --  Interleaved mode
      INTLVD         : TIMACR_INTLVD_Field := 16#0#;
      --  Re-Synchronized Update
      RSYNCU         : Boolean := False;
      --  Synchronization Resets Timer x
      SYNCRSTx       : Boolean := False;
      --  Synchronization Starts Timer x
      SYNCSTRTx      : Boolean := False;
      --  Delayed CMP2 mode
      DELCMP         : TIMACR_DELCMP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  TFU
      TFU            : Boolean := False;
      --  Timer x Repetition update
      TxREPU         : Boolean := False;
      --  Timerx reset update
      TxRSTU         : Boolean := False;
      --  TAU
      TAU            : Boolean := False;
      --  TBU
      TBU            : Boolean := False;
      --  TCU
      TCU            : Boolean := False;
      --  TDU
      TDU            : Boolean := False;
      --  TEU
      TEU            : Boolean := False;
      --  Master Timer update
      MSTU           : Boolean := False;
      --  AC Synchronization
      DACSYNC        : TIMACR_DACSYNC_Field := 16#0#;
      --  Preload enable
      PREEN          : Boolean := False;
      --  Update Gating
      UPDGAT         : TIMACR_UPDGAT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMxCR_Register use record
      CKPSCx         at 0 range 0 .. 2;
      CONT           at 0 range 3 .. 3;
      RETRIG         at 0 range 4 .. 4;
      HALF           at 0 range 5 .. 5;
      PSHPLL         at 0 range 6 .. 6;
      INTLVD         at 0 range 7 .. 8;
      RSYNCU         at 0 range 9 .. 9;
      SYNCRSTx       at 0 range 10 .. 10;
      SYNCSTRTx      at 0 range 11 .. 11;
      DELCMP         at 0 range 12 .. 15;
      TFU            at 0 range 16 .. 16;
      TxREPU         at 0 range 17 .. 17;
      TxRSTU         at 0 range 18 .. 18;
      TAU            at 0 range 19 .. 19;
      TBU            at 0 range 20 .. 20;
      TCU            at 0 range 21 .. 21;
      TDU            at 0 range 22 .. 22;
      TEU            at 0 range 23 .. 23;
      MSTU           at 0 range 24 .. 24;
      DACSYNC        at 0 range 25 .. 26;
      PREEN          at 0 range 27 .. 27;
      UPDGAT         at 0 range 28 .. 31;
   end record;

   --  High Resolution Timer: TIMx
   type HRTimer_Channel is record
      --  Timerx Control Register
      TIMxCR   : aliased TIMxCR_Register;
      --  Timerx Interrupt Status Register
      TIMxISR  : aliased TIMAISR_Register;
      --  Timerx Interrupt Clear Register
      TIMxICR  : aliased TIMAICR_Register;
      --  TIMxDIER
      TIMxDIER : aliased TIMADIER_Register;
      --  Timerx Counter Register
      CNTxR    : aliased CNTAR_Register;
      --  Timerx Period Register
      PERxR    : aliased PERAR_Register;
      --  Timerx Repetition Register
      REPxR    : aliased REPAR_Register;
      --  Timerx Compare 1 Register
      CMP1xR   : aliased CMP1AR_Register;
      --  Timerx Compare 1 Compound Register
      CMP1CxR  : aliased CMP1CAR_Register;
      --  Timerx Compare 2 Register
      CMP2xR   : aliased CMP2AR_Register;
      --  Timerx Compare 3 Register
      CMP3xR   : aliased CMP3AR_Register;
      --  Timerx Compare 4 Register
      CMP4xR   : aliased CMP4AR_Register;
      --  Timerx Capture 1 Register
      CPT1xR   : aliased CPT1AR_Register;
      --  Timerx Capture 2 Register
      CPT2xR   : aliased CPT2AR_Register;
      --  Timerx Deadtime Register
      DTxR     : aliased DTAR_Register;
      --  Timerx Output1 Set Register
      SETx1R   : HAL.UInt32;
      --  Timerx Output1 Reset Register
      RSTx1R   : HAL.UInt32;
      --  Timerx Output2 Set Register
      SETx2R   : HAL.UInt32;
      --  Timerx Output2 Reset Register
      RSTx2R   : HAL.UInt32;
      --  Timerx External Event Filtering Register 1
      EEFxR1   : aliased EEFAR1_Register;
      --  Timerx External Event Filtering Register 2
      EEFxR2   : aliased EEFAR2_Register;
      --  TimerA Reset Register
      RSTxR    : HAL.UInt32;
      --  Timerx Chopper Register
      CHPxR    : aliased CHPAR_Register;
      --  Timerx Capture 1 Control Register
      CPT1xCR  : HAL.UInt32;
      --  Timerx Capture 2 Control Register
      CPT2xCR  : HAL.UInt32;
      --  Timerx Output Register
      OUTxR    : aliased OUTAR_Register;
      --  Timerx Fault Register
      FLTxR    : aliased FLTAR_Register;
      --  HRTIM Timerx Control Register 2
      TIMxCR2  : aliased TIMACR2_Register;
      --  HRTIM Timerx External Event Filtering Register 3
      EEFxR3   : aliased EEFAR3_Register;
   end record
     with Volatile;

   for HRTimer_Channel use record
      TIMxCR   at 16#0# range 0 .. 31;
      TIMxISR  at 16#4# range 0 .. 31;
      TIMxICR  at 16#8# range 0 .. 31;
      TIMxDIER at 16#C# range 0 .. 31;
      CNTxR    at 16#10# range 0 .. 31;
      PERxR    at 16#14# range 0 .. 31;
      REPxR    at 16#18# range 0 .. 31;
      CMP1xR   at 16#1C# range 0 .. 31;
      CMP1CxR  at 16#20# range 0 .. 31;
      CMP2xR   at 16#24# range 0 .. 31;
      CMP3xR   at 16#28# range 0 .. 31;
      CMP4xR   at 16#2C# range 0 .. 31;
      CPT1xR   at 16#30# range 0 .. 31;
      CPT2xR   at 16#34# range 0 .. 31;
      DTxR     at 16#38# range 0 .. 31;
      SETx1R   at 16#3C# range 0 .. 31;
      RSTx1R   at 16#40# range 0 .. 31;
      SETx2R   at 16#44# range 0 .. 31;
      RSTx2R   at 16#48# range 0 .. 31;
      EEFxR1   at 16#4C# range 0 .. 31;
      EEFxR2   at 16#50# range 0 .. 31;
      RSTxR    at 16#54# range 0 .. 31;
      CHPxR    at 16#58# range 0 .. 31;
      CPT1xCR  at 16#5C# range 0 .. 31;
      CPT2xCR  at 16#60# range 0 .. 31;
      OUTxR    at 16#64# range 0 .. 31;
      FLTxR    at 16#68# range 0 .. 31;
      TIMxCR2  at 16#6C# range 0 .. 31;
      EEFxR3   at 16#70# range 0 .. 31;
   end record;

   --  High Resolution Timer: Common functions
   type HRTimer_Common_Peripheral is record
      --  Control Register 1
      CR1     : aliased CR1_Register;
      --  Control Register 2
      CR2     : aliased CR2_Register;
      --  Interrupt Status Register
      ISR     : aliased ISR_Register;
      --  Interrupt Clear Register
      ICR     : aliased ICR_Register;
      --  Interrupt Enable Register
      IER     : aliased IER_Register;
      --  Output Enable Register
      OENR    : aliased OENR_Register;
      --  DISR
      ODISR   : aliased ODISR_Register;
      --  Output Disable Status Register
      ODSR    : aliased ODSR_Register;
      --  Burst Mode Control Register
      BMCR    : aliased BMCR_Register;
      --  BMTRGR
      BMTRGR  : aliased HAL.UInt32;
      --  BMCMPR
      BMCMPR  : aliased BMCMPR_Register;
      --  Burst Mode Period Register
      BMPER   : aliased BMPER_Register;
      --  Timer External Event Control Register 1
      EECR1   : aliased EECR1_Register;
      --  Timer External Event Control Register 2
      EECR2   : aliased EECR2_Register;
      --  Timer External Event Control Register 3
      EECR3   : aliased EECR3_Register;
      --  ADC Trigger 1 Register
      ADC1R   : aliased HAL.UInt32;
      --  ADC Trigger 2 Register
      ADC2R   : aliased HAL.UInt32;
      --  ADC Trigger 3 Register
      ADC3R   : aliased HAL.UInt32;
      --  ADC Trigger 4 Register
      ADC4R   : aliased HAL.UInt32;
      --  DLL Control Register
      DLLCR   : aliased DLLCR_Register;
      --  HRTIM Fault Input Register 1
      FLTINR1 : aliased FLTINR1_Register;
      --  HRTIM Fault Input Register 2
      FLTINR2 : aliased FLTINR2_Register;
      --  BDMUPR
      BDMUPR  : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTAUPR : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTBUPR : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTCUPR : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTDUPR : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTEUPR : aliased HAL.UInt32;
      --  Burst DMA Data Register
      BDMADR  : aliased HAL.UInt32;
      --  Burst DMA Timerx update Register
      BDTFUPR : aliased HAL.UInt32;
      --  HRTIM ADC Extended Trigger Register
      ADCER   : aliased ADCER_Register;
      --  HRTIM ADC Trigger Update Register
      ADCUR   : aliased ADCUR_Register;
      --  HRTIM ADC Post Scaler Register 1
      ADCPS1  : aliased ADCPS1_Register;
      --  HRTIM ADC Post Scaler Register 2
      ADCPS2  : aliased ADCPS2_Register;
      --  HRTIM Fault Input Register 3
      FLTINR3 : aliased FLTINR3_Register;
      --  HRTIM Fault Input Register 4
      FLTINR4 : aliased FLTINR4_Register;
   end record
     with Volatile;

   for HRTimer_Common_Peripheral use record
      CR1     at 16#00# range 0 .. 31;
      CR2     at 16#04# range 0 .. 31;
      ISR     at 16#08# range 0 .. 31;
      ICR     at 16#0C# range 0 .. 31;
      IER     at 16#10# range 0 .. 31;
      OENR    at 16#14# range 0 .. 31;
      ODISR   at 16#18# range 0 .. 31;
      ODSR    at 16#1C# range 0 .. 31;
      BMCR    at 16#20# range 0 .. 31;
      BMTRGR  at 16#24# range 0 .. 31;
      BMCMPR  at 16#28# range 0 .. 31;
      BMPER   at 16#2C# range 0 .. 31;
      EECR1   at 16#30# range 0 .. 31;
      EECR2   at 16#34# range 0 .. 31;
      EECR3   at 16#38# range 0 .. 31;
      ADC1R   at 16#3C# range 0 .. 31;
      ADC2R   at 16#40# range 0 .. 31;
      ADC3R   at 16#44# range 0 .. 31;
      ADC4R   at 16#48# range 0 .. 31;
      DLLCR   at 16#4C# range 0 .. 31;
      FLTINR1 at 16#50# range 0 .. 31;
      FLTINR2 at 16#54# range 0 .. 31;
      BDMUPR  at 16#58# range 0 .. 31;
      BDTAUPR at 16#5C# range 0 .. 31;
      BDTBUPR at 16#60# range 0 .. 31;
      BDTCUPR at 16#64# range 0 .. 31;
      BDTDUPR at 16#68# range 0 .. 31;
      BDTEUPR at 16#6C# range 0 .. 31;
      BDMADR  at 16#70# range 0 .. 31;
      BDTFUPR at 16#74# range 0 .. 31;
      ADCER   at 16#78# range 0 .. 31;
      ADCUR   at 16#7C# range 0 .. 31;
      ADCPS1  at 16#80# range 0 .. 31;
      ADCPS2  at 16#84# range 0 .. 31;
      FLTINR3 at 16#88# range 0 .. 31;
      FLTINR4 at 16#8C# range 0 .. 31;
   end record;

   --  High Resolution Timer: Common functions
   HRTimer_Common_Periph : aliased HRTimer_Common_Peripheral
     with Import, Address => HRTIM_Common_Base;

end STM32.HRTimers;
