package body PIDNNs is

   function Input_Layer (Data : PIDNN; Input : Temperature) return Dimensionless is
   begin
      return (Data.Setpoint - Input) / celcius;
   end Input_Layer;

   function Hidden_Layer_1 (Data : PIDNN; Input_Layer_Output : Dimensionless) return Matrix_3_1 is
      Input : constant Matrix_3_1 := Input_Layer_Output * Data.Input_To_L1_Weights;
   begin
      return (for I in 1 .. 3 => (1 => Tanh (Input (I, 1))));
   end Hidden_Layer_1;

   function Hidden_Layer_2 (Data : PIDNN; Layer_1_Output : Matrix_3_1) return Matrix_3_1 is
      Input : constant Matrix_3_1 := Data.L1_To_L2_Weights * Layer_1_Output;
   begin
      return
        (1 => (1 => Data.Proportional_Scale * Input (1, 1)),
         2 =>
           (1 =>
              Data.Last_Integral_Neuron_Output +
              Data.Integral_Scale * (1.0 / (Data.Sample_Frequency / hertz)) * Input (2, 1)),
         3 =>
           (1 =>
              (Data.Derivative_Scale * Data.Sample_Frequency / hertz) *
              (Input (3, 1) - Data.Last_Derivative_Neuron_Input)));
   end Hidden_Layer_2;

   function Output_Layer (Data : PIDNN; Layer_2_Output : Matrix_3_1) return PWM_Scale is
      Input : constant Matrix_1_1 := Data.L2_To_Output_Weights * Layer_2_Output;
   begin
      return Dimensionless'Min (Dimensionless'Max (Input (1, 1), PWM_Scale'First), PWM_Scale'Last);
   end Output_Layer;

end PIDNNs;
