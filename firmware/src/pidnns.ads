private with Ada.Numerics.Generic_Real_Arrays;
private with Ada.Numerics.Generic_Elementary_Functions;
with Physical_Types; use Physical_Types;

package PIDNNs is

   type PIDNN is private;

private

   package Dimensionless_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   function Tanh (X : Dimensionless) return Dimensionless renames Dimensionless_Elementary_Functions.Tanh;
   --  We could likely get away with a 1.0 - 2.0 / (e**(2.0 * X) + 1.0) instead of the above slower but correct
   --  implementation, but this code is called infrequently enough that it should not matter.

   package Dimensionless_Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Dimensionless);
   use Dimensionless_Real_Arrays;

   subtype Matrix_1_1 is Real_Matrix (1 .. 1, 1 .. 1);
   subtype Matrix_1_3 is Real_Matrix (1 .. 1, 1 .. 3);
   subtype Matrix_3_1 is Real_Matrix (1 .. 3, 1 .. 1);
   subtype Matrix_3_3 is Real_Matrix (1 .. 3, 1 .. 3);

   type PIDNN is record
      Sample_Frequency             : Frequency;
      Last_Input                   : Temperature;
      Setpoint                     : Temperature;
      Input_To_L1_Weights          : Matrix_3_1 := (1 .. 3 => (1 => 1.0 / 3.0));
      L1_To_L2_Weights             : Matrix_3_3 := (1 .. 3 => (1 .. 3 => 1.0));
      L2_To_Output_Weights         : Matrix_1_3 := (1 => (1 .. 3 => 1.0));
      Proportional_Scale           : Dimensionless;
      Integral_Scale               : Dimensionless;
      Derivative_Scale             : Dimensionless;
      Last_Integral_Neuron_Output  : Dimensionless;
      Last_Derivative_Neuron_Input : Dimensionless;
   end record;

   function Input_Layer (Data : PIDNN; Input : Temperature) return Dimensionless;
   function Hidden_Layer_1 (Data : PIDNN; Input_Layer_Output : Dimensionless) return Matrix_3_1;
   function Hidden_Layer_2 (Data : PIDNN; Layer_1_Output : Matrix_3_1) return Matrix_3_1;
   function Output_Layer (Data : PIDNN; Layer_2_Output : Matrix_3_1) return PWM_Scale;

end PIDNNs;
