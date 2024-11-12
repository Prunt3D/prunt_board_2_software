
--  This file provides interfaces for the operational amplifiers on the
--  STM32G4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

private with STM32_SVD.OPAMP;

package STM32.OPAMP is

   type Operational_Amplifier is limited private;

   procedure Enable (This : in out Operational_Amplifier)
     with Post => Enabled (This);

   procedure Disable (This : in out Operational_Amplifier)
     with Post => not Enabled (This);

   function Enabled (This : Operational_Amplifier) return Boolean;

   type NI_Input_Mode is (Normal, Calibration);

   procedure Set_NI_Input_Mode
     (This  : in out Operational_Amplifier;
      Input : NI_Input_Mode)
     with Post => Get_NI_Input_Mode (This) = Input;
   --  Select a calibration reference voltage on non-inverting input and
   --  disables external connections.

   function Get_NI_Input_Mode
     (This : Operational_Amplifier) return NI_Input_Mode;
   --  Return the source connected to the non-inverting input of the
   --  operational amplifier.

   type NI_Input_Port is (VINP0, VINP1, VINP2, Option_4);
   --  These bits allows to select the source connected to the non-inverting
   --  input of the operational amplifier. The first 3 options are common, the
   --  last option change for each OPAMP:
   --  Option  OPAMP1    OPAMP2    OPAMP3    OPAMP4    OPAMP5    OPAMP6
   --  4     DAC3_CH1     VIMP3  DAC3_CH2  DAC4_CH1  DAC4_CH2  DAC3_CH1

   procedure Set_NI_Input_Port
     (This  : in out Operational_Amplifier;
      Input : NI_Input_Port)
     with Post => Get_NI_Input_Port (This) = Input;
   --  Select the source connected to the non-inverting input of the
   --  operational amplifier.

   function Get_NI_Input_Port
     (This : Operational_Amplifier) return NI_Input_Port;
   --  Return the source connected to the non-inverting input of the
   --  operational amplifier.

   type NI_Sec_Input_Port is (VINP0, VINP1, VINP2, Option_4);
   --  These bits allows to select the source connected to the non-inverting
   --  input of the operational amplifier. The first 3 options are common, the
   --  last option change for each OPAMP:
   --  Option  OPAMP1    OPAMP2    OPAMP3    OPAMP4    OPAMP5    OPAMP6
   --  4     DAC3_CH1     VIMP3  DAC3_CH2  DAC4_CH1  DAC4_CH2  DAC3_CH1

   procedure Set_NI_Sec_Input_Port
     (This  : in out Operational_Amplifier;
      Input : NI_Sec_Input_Port)
     with Post => Get_NI_Sec_Input_Port (This) = Input;
   --  Select the secondary source connected to the non-inverting input
   --  of the operational amplifier when the controlled mux mode is enabled
   --  (T1CM_EN = 1 or T8CM_EN = 1 or T20CM_EN = 1).

   function Get_NI_Sec_Input_Port
     (This : Operational_Amplifier) return NI_Sec_Input_Port;
   --  Return the secondary source connected to the non-inverting input
   --  of the operational amplifier.

   type I_Input_Port is
     (VINM0,
      VINM1,
      Feedback_Resistor_PGA_Mode,
      Follower_Mode);

   procedure Set_I_Input_Port
     (This  : in out Operational_Amplifier;
      Input : I_Input_Port)
     with Post => Get_I_Input_Port (This) = Input;
   --  Select the source connected to the inverting input of the
   --  operational amplifier.

   function Get_I_Input_Port
     (This : Operational_Amplifier) return I_Input_Port;
   --  Return the source connected to the inverting input of the
   --  operational amplifier.

   type I_Sec_Input_Port is
     (VINM0_Or_Feedback_Resistor_PGA_Mode,
      VINM1_Or_Follower_Mode);
   --  When standalone mode is used (i.e. VM_SEL = “00” or “01”):
   --  0: Input from VINM0
   --  1: Input from VINM1
   --  When PGA (VM_SEL = “10”) or Follower mode (VM_SEL = “11”) is used:
   --  0: Resistor feedback output selected (PGA mode)
   --  1: VOUT selected as input minus (follower mode)

   procedure Set_I_Sec_Input_Port
     (This  : in out Operational_Amplifier;
      Input : I_Sec_Input_Port)
     with Post => Get_I_Sec_Input_Port (This) = Input;
   --  Select the secondary source connected to the inverting input of the
   --  operational amplifier when the controlled mux mode is enabled
   --  (T1CM_EN = 1 or T8CM_EN = 1 or T20CM_EN = 1).

   function Get_I_Sec_Input_Port
     (This : Operational_Amplifier) return I_Sec_Input_Port;
   --  Return the secondary source connected to the inverting input of the
   --  operational amplifier.

   type Input_Mux_Mode is (Manual, Automatic);
   --  Timer controlled mux mode.

   type Input_Mux_Timer is (TIM1, TIM8, TIM20);
   --  Timer that constrols the mux mode.

   procedure Set_Input_Mux_Mode
     (This  : in out Operational_Amplifier;
      Timer : Input_Mux_Timer;
      Mode  : Input_Mux_Mode)
     with Post => Get_Input_Mux_Mode (This, Timer) = Mode;
   --  Select automatically the switch between the default selection
   --  (VP_SEL and VM_SEL) and the secondary selection (VPS_SEL and VMS_SEL)
   --  of the inverting and non inverting inputs of the operational amplifier.
   --  This automatic switch is triggered by the TIMx CC6 output arriving on
   --  the OPAMPx input multiplexers.

   function Get_Input_Mux_Mode
     (This  : Operational_Amplifier;
      Timer : Input_Mux_Timer) return Input_Mux_Mode;
   --  Return the selection of the selection between the default and the
   --  secondary inputs of the inverting and non inverting inputs of the
   --  operational amplifier.

   type PGA_Mode_Gain is
     (NI_Gain_2,
      NI_Gain_4,
      NI_Gain_8,
      NI_Gain_16,
      NI_Gain_32,
      NI_Gain_64,
      I_Gain_1_NI_Gain_2_VINM0,
      I_Gain_3_NI_Gain_4_VINM0,
      I_Gain_7_NI_Gain_8_VINM0,
      I_Gain_15_NI_Gain_16_VINM0,
      I_Gain_31_NI_Gain_32_VINM0,
      I_Gain_63_NI_Gain_64_VINM0,
      NI_Gain_2_Filtering_VINM0,
      NI_Gain_4_Filtering_VINM0,
      NI_Gain_8_Filtering_VINM0,
      NI_Gain_16_Filtering_VINM0,
      NI_Gain_32_Filtering_VINM0,
      NI_Gain_64_Filtering_VINM0,
      I_Gain_1_NI_Gain_2_VINM0_VINM1,
      I_Gain_3_NI_Gain_4_VINM0_VINM1,
      I_Gain_7_NI_Gain_8_VINM0_VINM1,
      I_Gain_15_NI_Gain_16_VINM0_VINM1,
      I_Gain_31_NI_Gain_32_VINM0_VINM1,
      I_Gain_63_NI_Gain_64_VINM0_VINM1)
     with Size => 5;
   --  Gain in PGA mode.

   for PGA_Mode_Gain use
     (NI_Gain_2                        => 2#00000#,
      NI_Gain_4                        => 2#00001#,
      NI_Gain_8                        => 2#00010#,
      NI_Gain_16                       => 2#00011#,
      NI_Gain_32                       => 2#00100#,
      NI_Gain_64                       => 2#00101#,
      I_Gain_1_NI_Gain_2_VINM0         => 2#01000#,
      I_Gain_3_NI_Gain_4_VINM0         => 2#01001#,
      I_Gain_7_NI_Gain_8_VINM0         => 2#01010#,
      I_Gain_15_NI_Gain_16_VINM0       => 2#01011#,
      I_Gain_31_NI_Gain_32_VINM0       => 2#01100#,
      I_Gain_63_NI_Gain_64_VINM0       => 2#01101#,
      NI_Gain_2_Filtering_VINM0        => 2#10000#,
      NI_Gain_4_Filtering_VINM0        => 2#10001#,
      NI_Gain_8_Filtering_VINM0        => 2#10010#,
      NI_Gain_16_Filtering_VINM0       => 2#10011#,
      NI_Gain_32_Filtering_VINM0       => 2#10100#,
      NI_Gain_64_Filtering_VINM0       => 2#10101#,
      I_Gain_1_NI_Gain_2_VINM0_VINM1   => 2#11000#,
      I_Gain_3_NI_Gain_4_VINM0_VINM1   => 2#11001#,
      I_Gain_7_NI_Gain_8_VINM0_VINM1   => 2#11010#,
      I_Gain_15_NI_Gain_16_VINM0_VINM1 => 2#11011#,
      I_Gain_31_NI_Gain_32_VINM0_VINM1 => 2#11100#,
      I_Gain_63_NI_Gain_64_VINM0_VINM1 => 2#11101#);

   procedure Set_PGA_Mode_Gain
     (This  : in out Operational_Amplifier;
      Input : PGA_Mode_Gain)
     with Post => Get_PGA_Mode_Gain (This) = Input;
   --  Select the gain in PGA mode.

   function Get_PGA_Mode_Gain
     (This : Operational_Amplifier) return PGA_Mode_Gain;
   --  Return the gain in PGA mode.

   type Speed_Mode is (Normal_Mode, HighSpeed_Mode);

   procedure Set_Speed_Mode
     (This  : in out Operational_Amplifier;
      Input : Speed_Mode)
     with Pre => not Enabled (This),
          Post => Get_Speed_Mode (This) = Input;
   --  OPAMP in normal or high-speed mode.

   function Get_Speed_Mode
     (This : Operational_Amplifier) return Speed_Mode;
   --  Return the OPAMP speed mode.

   type Init_Parameters is record
      Input_Minus     : I_Input_Port;
      Input_Sec_Minus : I_Sec_Input_Port;
      Input_Plus      : NI_Input_Port;
      Input_Sec_Plus  : NI_Sec_Input_Port;
      Mux_Timer       : Input_Mux_Timer;
      Mux_Mode        : Input_Mux_Mode;
      PGA_Mode        : PGA_Mode_Gain;
      Power_Mode      : Speed_Mode;
   end record;

   procedure Configure_Opamp
     (This  : in out Operational_Amplifier;
      Param : Init_Parameters);

   procedure Set_User_Trimming
     (This    : in out Operational_Amplifier;
      Enabled : Boolean)
     with Post => Get_User_Trimming (This) = Enabled;
   --  Allows to switch from ‘factory’ AOP offset trimmed values to ‘user’ AOP
   --  offset trimmed values.

   function Get_User_Trimming
     (This : Operational_Amplifier) return Boolean;
   --  Return the state of user trimming.

   type Differential_Pair is (NMOS, PMOS);

   procedure Set_Offset_Trimming
     (This  : in out Operational_Amplifier;
      Pair  : Differential_Pair;
      Input : UInt5)
     with Post => Get_Offset_Trimming (This, Pair) = Input;
   --  Select the offset trimming value for NMOS or PMOS.

   function Get_Offset_Trimming
     (This : Operational_Amplifier;
      Pair : Differential_Pair) return UInt5;
   --  Return the offset trimming value for NMOS or PMOS.

   procedure Set_Calibration_Mode
     (This    : in out Operational_Amplifier;
      Enabled : Boolean)
     with Post => Get_Calibration_Mode (This);
   --  Select the calibration mode connecting VM and VP to the OPAMP
   --  internal reference voltage.

   function Get_Calibration_Mode
     (This : Operational_Amplifier) return Boolean;
   --  Return the calibration mode.

   type Calibration_Value is
     (VREFOPAMP_Is_3_3_VDDA, --  3.3%
      VREFOPAMP_Is_10_VDDA, --  10%
      VREFOPAMP_Is_50_VDDA, --  50%
      VREFOPAMP_Is_90_VDDA --  90%
      );
   --  Offset calibration bus to generate the internal reference voltage.

   procedure Set_Calibration_Value
     (This  : in out Operational_Amplifier;
      Input : Calibration_Value)
     with Post => Get_Calibration_Value (This) = Input;
   --  Select the offset calibration bus used to generate the internal
   --  reference voltage when CALON = 1 or FORCE_VP = 1.

   function Get_Calibration_Value
     (This : Operational_Amplifier) return Calibration_Value;
   --  Return the offset calibration bus voltage.

   procedure Calibrate (This : in out Operational_Amplifier);
   --  Calibrate the NMOS and PMOS differential pair. This routine
   --  is described in the RM0440 rev 6 pg. 797. The offset trim time,
   --  during calibration, must respect the minimum time needed
   --  between two steps to have 1 mV accuracy.
   --  This routine must be executed first with normal speed mode, then with
   --  high-speed mode, if used.

   type Internal_Output is
     (Is_Output,
      Is_Not_Output);

   procedure Set_Internal_Output
     (This  : in out Operational_Amplifier;
      Input : Internal_Output)
     with Post => Get_Internal_Output (This) = Input;
   --  Connect the internal OPAMP output to output pin or internally to an ADC
   --  channel and disconnected from the output pin.

   function Get_Internal_Output
     (This : Operational_Amplifier) return Internal_Output;
   --  Return the internal output reference voltage state.

   type Output_Status_Flag is
     (NI_Lesser_Then_I,
      NI_Greater_Then_I);

   function Get_Output_Status_Flag
     (This : Operational_Amplifier) return Output_Status_Flag;
   --  Return the output status flag when the OPAMP is used as comparator
   --  during calibration.

   procedure Set_Lock_OpAmp (This : in out Operational_Amplifier)
     with Post => Get_Lock_OpAmp (This) = True;
   --  Allows to have OPAMPx_CSR register as read-only. It can only be cleared
   --  by a system reset.

   function Get_Lock_OpAmp (This : Operational_Amplifier) return Boolean;
   --  Return the OPAMP lock bit state.

private
   --  representation for the whole Operationa Amplifier type  -----------------

   type Operational_Amplifier is limited record
      CSR  : STM32_SVD.OPAMP.OPAMP1_CSR_Register;
      TCMR : STM32_SVD.OPAMP.OPAMP1_TCMR_Register;
   end record with Volatile, Size => 7 * 32;

   for Operational_Amplifier use record
      CSR  at 16#00# range  0 .. 31;
      TCMR at 16#18# range  0 .. 31;
   end record;

end STM32.OPAMP;
