with STM32.DMA;            use STM32.DMA;
with System;               use System;
with STM32.USARTs;         use STM32.USARTs;
with Messages;             use Messages;
with STM32.GPIO;           use STM32.GPIO;
with STM32.Device;         use STM32.Device;
with STM32;                use STM32;
with STM32.CRC;            use STM32.CRC;
with STM32.Timers;         use STM32.Timers;
with STM32.ADC;            use STM32.ADC;
with STM32_SVD;
with STM32_SVD.USART;
with STM32_SVD.ADC;
with STM32_SVD.DMA;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Interrupts;       use Ada.Interrupts;

package Hardware_Configuration is

   --  Server_Communications

   Comms_UART_DMA_RX_Controller : DMA_Controller renames DMA_1;
   Comms_UART_DMA_RX_Stream     : constant DMA_Stream_Selector  := Stream_1;
   Comms_UART_DMA_RX_Priority   : constant DMA_Priority_Level   := Priority_Very_High;
   Comms_UART_DMA_RX_Channel    : constant DMA_Channel_Selector := USART3_RX;

   Comms_CRC_Unit : CRC_32 renames CRC_Unit;

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
     (Heater_1 => Timer_4'Access, Heater_2 => Timer_8'Access);
   Heater_Timer_Channels   : constant array (Heater_Name) of Timer_Channel                 :=
     (Heater_1 => Channel_2, Heater_2 => Channel_1);
   Heater_Timer_Polarities : constant array (Heater_Name) of Timer_Output_Compare_Polarity :=
     (Heater_1 => High, Heater_2 => High);
   Heater_GPIO_Points      : constant array (Heater_Name) of GPIO_Point := (Heater_1 => PB7, Heater_2 => PA15);
   Heater_GPIO_AFs         : constant array (Heater_Name) of GPIO_Alternate_Function       :=
     (Heater_1 => GPIO_AF_TIM4_2, Heater_2 => GPIO_AF_TIM8_2);
   --  Heaters package also uses IWDG.

   --  High_Power_Switch

   High_Power_Switch_ADC          : Analog_To_Digital_Converter renames ADC_1;
   High_Power_Switch_ADC_Internal : aliased STM32_SVD.ADC.ADC1_Peripheral with
     Volatile, Import, Address => STM32_SVD.ADC1_Base;
   High_Power_Switch_Output_Point : constant GPIO_Point           := PB5;
   High_Power_Switch_Input_Point  : constant GPIO_Point           := PC1;
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
     (Fan_1 => Timer_2'Access, Fan_2 => Timer_20'Access, Fan_3 => Timer_15'Access, Fan_4 => Timer_17'Access);
   Fan_Timer_Channels      : constant array (Fan_Name) of Timer_Channel                 :=
     (Fan_1 => Channel_3, Fan_2 => Channel_1, Fan_3 => Channel_1, Fan_4 => Channel_1);
   Fan_Timer_Complementary : constant array (Fan_Name) of Boolean                       :=
     (Fan_1 => False, Fan_2 => False, Fan_3 => True, Fan_4 => False);
   Fan_Timer_Polarities    : constant array (Fan_Name) of Timer_Output_Compare_Polarity :=
     (Fan_1 => High, Fan_2 => High, Fan_3 => Low, Fan_4 => High);
   Fan_GPIO_Points         : constant array (Fan_Name) of GPIO_Point                    :=
     (Fan_1 => PB10, Fan_2 => PB2, Fan_3 => PA1, Fan_4 => PB9);
   Fan_GPIO_AFs            : constant array (Fan_Name) of GPIO_Alternate_Function       :=
     (Fan_1 => GPIO_AF_TIM2_1, Fan_2 => GPIO_AF_TIM20_3, Fan_3 => GPIO_AF_TIM15_9, Fan_4 => GPIO_AF_TIM17_1);

   --  Change the below client ID if you are porting this code to a new board. The following command may be used to
   --  generate a random ID:
   -- hexdump -vn32 -e '1/4 "16#" "%02X" "#, "' /dev/urandom && echo ""
   DO_NOT_COPY_THIS_CLIENT_ID_AS_IT_IS_MEANT_TO_IDENTIFY_THIS_PARTICULAR_BOARD_MODEL_AND_FIRMWARE :
     constant Client_ID :=
     (16#14BC_80C3#, 16#53B1_4CAC#, 16#DE61_09E7#, 16#6BC8_2ECD#);

end Hardware_Configuration;
