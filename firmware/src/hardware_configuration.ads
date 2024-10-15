with STM32.DMA;            use STM32.DMA;
with System;               use System;
with STM32.USARTs;         use STM32.USARTs;
with Messages;             use Messages;
with STM32.GPIO;           use STM32.GPIO;
with STM32.Device;         use STM32.Device;
with STM32;                use STM32;
with STM32.CRC;            use STM32.CRC;
with STM32.Timers;         use STM32.Timers;
with STM32.LPTimers;       use STM32.LPTimers;
with STM32.ADC;            use STM32.ADC;
with STM32.COMP;           use STM32.COMP;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Interrupts;       use Ada.Interrupts;
with STM32_SVD;
with STM32_SVD.USART;
with STM32_SVD.ADC;

package Hardware_Configuration is

   --  Self_Check

   Self_Check_CRC_Unit : CRC_32 renames CRC_Unit;
   --  Shared with Server_Communications.

   --  Server_Communications

   Comms_UART_DMA_RX_Controller : DMA_Controller renames DMA_1;
   Comms_UART_DMA_RX_Stream     : constant DMA_Stream_Selector  := Stream_1;
   Comms_UART_DMA_RX_Priority   : constant DMA_Priority_Level   := Priority_Very_High;
   Comms_UART_DMA_RX_Channel    : constant DMA_Channel_Selector := USART3_RX;

   Comms_CRC_Unit : CRC_32 renames CRC_Unit;
   --  Shared with Self_Check.

   Comms_UART           : USART renames USART_3;
   Comms_UART_TX_Pin    : constant GPIO_Point              := PC10;
   Comms_UART_TX_Pin_AF : constant GPIO_Alternate_Function := GPIO_AF_USART3_7;
   Comms_UART_RX_Pin    : constant GPIO_Point              := PC11;
   Comms_UART_RX_Pin_AF : constant GPIO_Alternate_Function := GPIO_AF_USART3_7;

   --  Steppers

   TMC_UART_DMA_RX_Controller : DMA_Controller renames DMA_1;
   TMC_UART_DMA_RX_Stream     : constant DMA_Stream_Selector  := Stream_2;
   TMC_UART_DMA_RX_Priority   : constant DMA_Priority_Level   := Priority_Low;
   TMC_UART_DMA_RX_Channel    : constant DMA_Channel_Selector := USART2_RX;

   TMC_UART_Internal : aliased STM32_SVD.USART.USART_Peripheral with
     Import, Volatile, Address => STM32_SVD.USART2_Base;
   TMC_UART          : USART renames USART_2;
   TMC_UART_Pin      : constant GPIO_Point              := PA14;
   TMC_UART_Pin_AF   : constant GPIO_Alternate_Function := GPIO_AF_USART2_7;

   Stepper_Enable_Points : array (Stepper_Name) of GPIO_Point :=
     (Stepper_1 => PB11, Stepper_2 => PC5, Stepper_3 => PC12, Stepper_4 => PC14, Stepper_5 => PB6, Stepper_6 => PC3);
   Stepper_DIAG0_Points  : array (Stepper_Name) of GPIO_Point :=
     (Stepper_1 => PA12, Stepper_2 => PA13, Stepper_3 => PC13, Stepper_4 => PC15, Stepper_5 => PB3, Stepper_6 => PA2);
   --  TODO: Change the GPIO package so we can make the above constant.

   --  Input_Switches

   Switch_Points : constant array (Input_Switch_Name) of GPIO_Point :=
     (Endstop_1        => PA5,
      Endstop_2        => PA3,
      Endstop_3        => PA6,
      Endstop_4        => PA4,
      Stepper_1_Diag_0 => PA12,
      Stepper_2_Diag_0 => PA13,
      Stepper_3_Diag_0 => PC13,
      Stepper_4_Diag_0 => PC15,
      Stepper_5_Diag_0 => PB3,
      Stepper_6_Diag_0 => PA2);

   --  Step_Generator

   Step_Generator_Interrupt_Priority : constant Interrupt_Priority := Interrupt_Priority'Last_Valid;
   --  Step_Generator also uses HRTimer and all HRTimer output pins (PA9-PA11, PB12-PB15, PC6-PC9).

   --  Heaters

   Heater_Timers           : constant array (Heater_Name) of access Timer                  :=
     (Heater_1 => Timer_8'Access, Heater_2 => Timer_4'Access);
   Heater_Timer_Channels   : constant array (Heater_Name) of Timer_Channel                 :=
     (Heater_1 => Channel_1, Heater_2 => Channel_2);
   Heater_Timer_Polarities : constant array (Heater_Name) of Timer_Output_Compare_Polarity :=
     (Heater_1 => High, Heater_2 => High);
   Heater_GPIO_Points      : constant array (Heater_Name) of GPIO_Point := (Heater_1 => PA15, Heater_2 => PB7);
   Heater_GPIO_AFs         : constant array (Heater_Name) of GPIO_Alternate_Function       :=
     (Heater_1 => GPIO_AF_TIM8_2, Heater_2 => GPIO_AF_TIM4_2);
   --  Heaters package also uses IWDG.

   --  High_Power_Switch

   High_Power_Switch_ADC          : Analog_To_Digital_Converter renames ADC_1;
   High_Power_Switch_ADC_Internal : aliased STM32_SVD.ADC.ADC1_Peripheral with
     Volatile, Import, Address => STM32_SVD.ADC1_Base;
   High_Power_Switch_Output_Point : GPIO_Point                    := PB5;
   High_Power_Switch_Input_Point  : GPIO_Point                    := PC1;
   --  TODO: Change the GPIO package so we can make the above constant.
   High_Power_Switch_ADC_Channel  : constant Analog_Input_Channel := 7;

   --  Thermistors

   Thermistor_DMA_Interrupt_Priority : constant Interrupt_Priority   := Interrupt_Priority'Last_Valid - 1;
   Thermistor_DMA_Interrupt_ID       : constant Interrupt_ID         := DMA2_CH2_Interrupt;
   Thermistor_DMA_Controller         : DMA_Controller renames DMA_2;
   Thermistor_DMA_Stream             : constant DMA_Stream_Selector  := Stream_2;
   Thermistor_DMA_Priority           : constant DMA_Priority_Level   := Priority_Very_High;
   Thermistor_DMA_Channel            : constant DMA_Channel_Selector := ADC2;

   Thermistor_ADC          : Analog_To_Digital_Converter renames ADC_2;
   Thermistor_ADC_Internal : aliased STM32_SVD.ADC.ADC1_Peripheral with
     Volatile, Import, Address => STM32_SVD.ADC2_Base;
   Thermistor_ADC_Channels : constant array (Thermistor_Name) of Analog_Input_Channel :=
     (Thermistor_1 => 5, Thermistor_2 => 10, Thermistor_3 => 8, Thermistor_4 => 6);
   Thermistor_GPIO_Points  : constant array (Thermistor_Name) of GPIO_Point           :=
     (Thermistor_1 => PC4, Thermistor_2 => PF1, Thermistor_3 => PC2, Thermistor_4 => PC0);

   --  Fans

   Fan_Timers              : constant array (Fan_Name) of access Timer                  :=
     (Fan_1 => Timer_20'Access, Fan_2 => Timer_17'Access, Fan_3 => Timer_2'Access, Fan_4 => Timer_15'Access);
   Fan_Timer_Channels      : constant array (Fan_Name) of Timer_Channel                 :=
     (Fan_1 => Channel_2, Fan_2 => Channel_1, Fan_3 => Channel_3, Fan_4 => Channel_1);
   Fan_Timer_Complementary : constant array (Fan_Name) of Boolean                       :=
     (Fan_1 => False, Fan_2 => False, Fan_3 => False, Fan_4 => True);
   Fan_Timer_Polarities    : constant array (Fan_Name) of Timer_Output_Compare_Polarity :=
     (Fan_1 => Low, Fan_2 => Low, Fan_3 => Low, Fan_4 => High);
   Fan_GPIO_Points         : constant array (Fan_Name) of GPIO_Point                    :=
     (Fan_1 => PB2, Fan_2 => PB9, Fan_3 => PB10, Fan_4 => PA1);
   Fan_GPIO_AFs            : constant array (Fan_Name) of GPIO_Alternate_Function       :=
     (Fan_1 => GPIO_AF_TIM20_3, Fan_2 => GPIO_AF_TIM17_1, Fan_3 => GPIO_AF_TIM2_1, Fan_4 => GPIO_AF_TIM15_9);

   type Tach_Config_Kind is (Timer_Kind, LPTimer_Kind);

   type Tach_Config (Kind : Tach_Config_Kind := Timer_Kind) is record
      Point : GPIO_Point;
      Comp  : access Comparator;
      case Kind is
         when Timer_Kind =>
            Tim     : access Timer;
            Trigger : Timer_External_Trigger_Source;
         when LPTimer_Kind =>
            LPTim : access LPTimer;
            Clock : LPTimer_Input_Clock_Enum;
      end case;
   end record;

   Tach_Configs : constant array (Fan_Name) of Tach_Config :=
     (Fan_1 =>
        (Kind => Timer_Kind, Point => PB0, Comp => Comp_4'Access, Tim => Timer_3'Access, Trigger => Comp_4_Output),
      Fan_2 =>
        (Kind => Timer_Kind, Point => PA0, Comp => Comp_3'Access, Tim => Timer_5'Access, Trigger => Comp_3_Output),
      Fan_3 =>
        (Kind => LPTimer_Kind, Point => PB1, Comp => Comp_1'Access, LPTim => LPTimer_1'Access, Clock => Option_1),
      Fan_4 =>
        (Kind => Timer_Kind, Point => PA7, Comp => Comp_2'Access, Tim => Timer_1'Access, Trigger => Comp_2_Output));

   --  MCU_Temperature uses ADC_5.

end Hardware_Configuration;
