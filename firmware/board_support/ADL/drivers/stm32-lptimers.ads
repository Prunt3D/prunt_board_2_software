
pragma Restrictions (No_Elaboration_Code);

with STM32_SVD.LPTIM; use STM32_SVD.LPTIM;

package STM32.LPTimers is

   type LPTimer is limited private;

   procedure Enable (This : in out LPTimer)
     with Post => Enabled (This);

   procedure Disable (This : in out LPTimer)
     with Post => not Enabled (This);

   function Enabled (This : LPTimer) return Boolean;

   type LPTimer_Prescaler is
     (Div_1,
      Div_2,
      Div_4,
      Div_8,
      Div_16,
      Div_32,
      Div_64,
      Div_128)
     with Size => 3;

   procedure Configure_Prescaler
     (This      : in out LPTimer;
      Prescaler : LPTimer_Prescaler)
     with Pre => not Enabled (This),
          Post => Current_Prescaler (This) = Prescaler;
   --  Configure the division factor.

   function Current_Prescaler (This : LPTimer) return LPTimer_Prescaler;

   procedure Set_Compare_Value
     (This  : in out LPTimer;
      Value : UInt16)
     with
       Post => Current_Compare_Value (This) = Value;

   function Current_Compare_Value (This : LPTimer) return UInt16;

   procedure Set_Autoreload_Value
     (This  : in out LPTimer;
      Value : UInt16)
     with
       Post => Current_Autoreload (This) = Value;

   function Current_Autoreload (This : LPTimer) return UInt16;

   procedure Configure
     (This      : in out LPTimer;
      Prescaler : LPTimer_Prescaler;
      Period    : UInt16)
     with Pre => not Enabled (This),
          Post => Current_Prescaler (This) = Prescaler and
                  Current_Autoreload (This) = Period;

   procedure Compute_Prescaler_And_Period
     (This                : LPTimer;
      Requested_Frequency : UInt32;
      Prescaler           : out LPTimer_Prescaler;
      Period              : out UInt32)
     with Pre => Requested_Frequency > 0;
   --  Computes the minimum prescaler and thus the maximum resolution for the
   --  given timer, based on the system clocks and the requested frequency.
   --  Computes the period required for the requested frequency.

   Invalid_Request : exception;
   --  Raised when the requested frequency is too high or too low for the given
   --  timer and system clocks.

   type LPTimer_Clock_Source is (Internal, External);
   --  LPTIM is clocked by internal clock source (APB clock or any of the
   --  embedded oscillators) or by an external clock source through the LPTIM
   --  external Input1.

   procedure Set_Counter_Clock_Source
     (This   : in out LPTimer;
      Mode   : LPTimer_Clock_Source)
     with Pre => not Enabled (This);
   --  Select which clock source is used to clock the counter.

   procedure Set_Counter_Value
     (This  : in out LPTimer;
      Value : UInt16)
     with Post => Current_Counter (This) = Value;

   function Current_Counter (This : LPTimer) return UInt16;

   type LPTimer_Counter_Reset_Mode is (After_Read_Counter, Now);

   procedure Set_Counter_Reset_Mode
     (This   : in out LPTimer;
      Mode   : LPTimer_Counter_Reset_Mode;
      Enable : Boolean := True)
     with Pre => (if Mode = Now then Enable);

   type LPTimer_Preload_Mode is (After_APB_Bus_Write, End_Current_Period);

   procedure Set_Preload_Mode
     (This   : in out LPTimer;
      Mode   : LPTimer_Preload_Mode)
     with Pre => not Enabled (This);
   --  Autoreload and the Compare registers are updated after each APB bus
   --  write access or at the end of the current LPTIM period.

   type LPTimer_Interrupt is
     (Compare_Match,
      Autorreload_Match,
      External_Trigger_Valid_Edge,
      Compare_Register_Update_Ok,
      Autorreload_Register_Update_Ok,
      Direction_Change_To_Up,
      Direction_Change_To_Down);

   procedure Enable_Interrupt
     (This   : in out LPTimer;
      Source : LPTimer_Interrupt)
     with Post => Interrupt_Enabled (This, Source);

   type LPTimer_Interrupt_List is array (Positive range <>) of LPTimer_Interrupt;

   procedure Enable_Interrupt
     (This    : in out LPTimer;
      Sources : LPTimer_Interrupt_List)
     with Post => (for all Source of Sources => Interrupt_Enabled (This, Source));

   procedure Disable_Interrupt
     (This   : in out LPTimer;
      Source : LPTimer_Interrupt)
     with Post => not Interrupt_Enabled (This, Source);

   procedure Clear_Pending_Interrupt
     (This   : in out LPTimer;
      Source : LPTimer_Interrupt);

   function Interrupt_Enabled
     (This   : LPTimer;
      Source : LPTimer_Interrupt)
      return Boolean;

   type LPTimer_Status_Flag is new LPTimer_Interrupt;

   function Status (This : LPTimer;
                    Flag : LPTimer_Status_Flag) return Boolean;

   procedure Clear_Status (This : in out LPTimer;  Flag : LPTimer_Status_Flag);

   procedure Select_Clock_Source
     (This   : in out LPTimer;
      Source : LPTimer_Clock_Source);
   --  LPTIM is clocked by internal clock source (APB clock or any of the
   --  embedded oscillators) or by an external clock source through the LPTIM
   --  external Input1.

   function Get_Clock_Source (This : LPTimer) return LPTimer_Clock_Source;

   type LPTimer_External_Clock_Polarity is
     (Rising_Edge,
      Falling_Edge,
      Both_Rising_Falling_Edges)
     with Size => 2;
   --  When the LPTIM is clocked by an external clock source, CKPOL bits is
   --  used to configure the active edge or edges used by the counter.

   type LPTimer_Digital_Filter is
     (Any_Level_Change,
      Stable_2_Clock_Periods,
      Stable_4_Clock_Periods,
      Stable_8_Clock_Periods)
   with Size => 2;
   --  Sets the number of consecutive equal samples that should be detected
   --  when a level change occurs on an external clock signal before it is
   --  considered as a valid level transition. An internal clock source must
   --  be present to use this feature.

   procedure Configure_External_Clock
     (This     : in out LPTimer;
      Polarity : LPTimer_External_Clock_Polarity;
      Filter   : LPTimer_Digital_Filter);

   type LPTimer_Input_Clock_Enum is
     (Option_1,
      Option_2,
      Option_3,
      Option_4)
     with Size => 2;
   --  Option       Input 1        Input 2
   --  1            COMP2          COMP1
   --  2            COMP4          COMP3
   --  3            COMP6          COMP5
   --  4            COMP6          COMP7
   --  See RM0440 rev 6 Chapter 32.4.2 "LPTIM input and trigger mapping".

   type LPTimer_Input_Clock is record
      Internal : Boolean := True;
      Value    : LPTimer_Input_Clock_Enum := LPTimer_Input_Clock_Enum'First;
   end record;
   --  LPTimer input 1  or input 2 connected to COMP output (Internal = True)
   --  or to GPIO (Internal = False).

   for LPTimer_Input_Clock use record
      Internal at 0 range 2 .. 2;
      Value    at 0 range 0 .. 1;
   end record;

   type LPTimer_Input is (Input_1, Input_2);

   procedure Configure_Input_Clock
     (This  : in out LPTimer;
      Input : LPTimer_Input;
      Clock : LPTimer_Input_Clock);
   --  LPTimer input 1  or input 2 connected to COMP output (Internal = True)
   --  or to GPIO (Internal = False).
   --  See RM0440 rev 6 Chapter 32.4.2 "LPTIM input and trigger mapping" and
   --  Chapter 32.4.1 "LPTIM block diagram".

   type LPTimer_Trigger_Source is
     (GPIO,
      RTC_ALARMA,
      RTC_ALARMB,
      RTC_TAMP1_OUT,
      RTC_TAMP2_OUT,
      RTC_TAMP3_OUT,
      COMP1_OUT,
      COMP2_OUT,
      COMP3_OUT,
      COMP4_OUT,
      COMP5_OUT,
      COMP6_OUT,
      COMP7_OUT)
     with Size => 4;
   --  See RM0440 rev 6 Chapter 32.4.2 "LPTIM input and trigger mapping".

   procedure Select_Trigger_Source
     (This   : in out LPTimer;
      Source : LPTimer_Trigger_Source);

   procedure Select_Trigger_Filter
     (This   : in out LPTimer;
      Filter : LPTimer_Digital_Filter);

   procedure Set_Trigger_Timeout
     (This : in out LPTimer;
      Enable : Boolean)
     with Pre => not Enabled (This);
   --  A trigger event arriving when the timer is already started will be
   --  ignored (timeout) or will reset and restart the counter.

   procedure Configure_Trigger
     (This    : in out LPTimer;
      Source  : LPTimer_Trigger_Source;
      Filter  : LPTimer_Digital_Filter;
      Timeout : Boolean);

   type LPTimer_Pulse_Mode is (Repetitive, Single);

   procedure Select_Pulse_Mode
     (This : in out LPTimer;
      Mode : LPTimer_Pulse_Mode)
     with Pre => Enabled (This);
   --  In case of software start (TRIGEN[1:0] = ‘00’), setting this bit starts
   --  the LPTIM in Continuous or Single mode. If the software start is disabled
   --  (TRIGEN[1:0] different than ‘00’), setting this bit starts the timer in
   --  Continuous or Single mode as soon as an external trigger is detected.
   --  If this bit is set when a single pulse mode counting is ongoing or in
   --  continuous mode, then the LPTIM will not stop at the next match between
   --  the LPTIM_ARR and LPTIM_CNT registers and the LPTIM counter keeps
   --  counting in Continuous or single mode.

   type LPTimer_Waveform_Shape is (PWM_One_Pulse, Set_Once);

   procedure Set_Waveform_Shape
     (This  : in out LPTimer;
      Shape : LPTimer_Waveform_Shape)
     with Pre => not Enabled (This);

   type LPTimer_Waveform_Polarity is (Direct, Inverse);

   procedure Set_Waveform_Polarity
     (This     : in out LPTimer;
      Polarity : LPTimer_Waveform_Polarity)
     with Pre => not Enabled (This);

   procedure Configure_Waveform_Shape
     (This     : in out LPTimer;
      Shape    : LPTimer_Waveform_Shape;
      Polarity : LPTimer_Waveform_Polarity)
     with Pre => not Enabled (This);
   --  Two 16-bit registers, the LPTIM_ARR (autoreload register) and LPTIM_CMP
   --  (compare register), are used to generate several different waveforms on
   --  LPTIM output. The timer can generate the following waveforms:
   --  • The PWM mode: the LPTIM output is set as soon as the counter value in
   --  LPTIM_CNT exceeds the compare value in LPTIM_CMP. The LPTIM output is
   --  reset as soon as a match occurs between the LPTIM_ARR and the LPTIM_CNT
   --  registers.
   --  • The One-pulse mode: the output waveform is similar to the one of the
   --  PWM mode for the first pulse, then the output is permanently reset.
   --  • The Set-once mode: the output waveform is similar to the One-pulse
   --  mode except that the output is kept to the last signal level (depends
   --  on the output configured polarity).
   --  See section 32.4.9 RM0440 rev 6 for waveform generation.

   procedure Set_Encoder_Mode
     (This : in out LPTimer;
      Enable : Boolean)
     with Pre => not Enabled (This);
   --  This mode allows handling signals from quadrature encoders used to detect
   --  angular position of rotary elements. Encoder interface mode acts simply
   --  as an external clock with direction selection. This means that the
   --  counter just counts continuously between 0 and the auto-reload value
   --  programmed into the LPTIM_ARR register (0 up to ARR or ARR down to 0
   --  depending on the direction). Therefore LPTIM_ARR must be configured
   --  before starting.
   --  From the two external input signals, Input1 and Input2, a clock signal is
   --  generated to clock the LPTIM counter. The phase between those two signals
   --  determines the counting direction.Direction change is signalized by the
   --  two Down and Up flags in the LPTIM_ISR register. Also, an interrupt can
   --  be generated for both direction change events if enabled through the
   --  DOWNIE bit.
   --  See section 32.4.14 RM0440 rev 6 for encoder scenarios.

private
   type LPTimer is new LPTIMER1_Peripheral;

end STM32.LPTimers;
