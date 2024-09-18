with Ada.Numerics;

package Physical_Types is

   --  You may notice a lot of math similar to 5.0**(1/2) here when using the below types. This may seem like it should
   --  be evaluated as 5.0**(1/2) = 5.0**0 = 1.0, which it would be under normal circumstances, but GNAT does some
   --  magic to evaluate it as 5.0**0.5 and keeps the dimensions intact. If porting this to a different Ada compiler
   --  then you will have to use ** from Ada.Numerics.Generic_Elementary_Functions and replace all the rationals with
   --  floating point literals.
   type Dimensioned_Float is new Float with
     Dimension_System =>
      ((Unit_Name => Millimeter, Unit_Symbol => "mm", Dim_Symbol => "Length"),
       (Unit_Name => Second, Unit_Symbol => "s", Dim_Symbol => "Time"),
       (Unit_Name => Celcius, Unit_Symbol => "°C", Dim_Symbol => "Temperature"),
       (Unit_Name => Volt, Unit_Symbol => "V", Dim_Symbol => "Voltage"),
       (Unit_Name => Amp, Unit_Symbol => "A", Dim_Symbol => "Current"),
       (Unit_Name => Gram, Unit_Symbol => "g", Dim_Symbol => "Mass"));

   subtype Length is Dimensioned_Float with
       Dimension => (Symbol => "mm", Millimeter => 1, others => 0);

   subtype Time is Dimensioned_Float with
       Dimension => (Symbol => "s", Second => 1, others => 0);

   subtype Temperature is Dimensioned_Float with
       Dimension => (Symbol => "°C", Celcius => 1, others => 0);

   subtype Angle is Dimensioned_Float with
       Dimension => (Symbol => "rad", others => 0);

   subtype Dimensionless is Dimensioned_Float with
       Dimension => (Symbol => "×", others => 0);

   subtype Voltage is Dimensioned_Float with
       Dimension => (Symbol => "V", Volt => 1, others => 0);

   subtype Current is Dimensioned_Float with
       Dimension => (Symbol => "A", Amp => 1, others => 0);

   subtype Mass is Dimensioned_Float with
       Dimension => (Symbol => "g", Gram => 1, others => 0);

   subtype Resistance is Dimensioned_Float with
       Dimension => (Symbol => "Ω", Volt => 1, Amp => -1, others => 0);

   subtype Power is Dimensioned_Float with
       Dimension => (Symbol => "W", Volt => 1, Amp => 1, others => 0);

   subtype Frequency is Dimensioned_Float with
       Dimension => (Symbol => "Hz", Second => -1, others => 0);

   subtype Energy is Dimensioned_Float with
       Dimension => (Symbol => "J", Volt => 1, Amp => 1, Second => 1, others => 0);

   subtype PWM_Scale is Dimensionless range 0.0 .. 1.0;

   subtype Cruise_Ratio is Dimensionless range 0.03 .. 0.97;

   pragma Warnings (Off, "assumed to be");
   mm      : constant Length      := 1.0;
   s       : constant Time        := 1.0;
   celcius : constant Temperature := 1.0;
   radian  : constant Angle       := 1.0;
   volt    : constant Voltage     := 1.0;
   amp     : constant Current     := 1.0;
   ohm     : constant Resistance  := 1.0;
   hertz   : constant Frequency   := 1.0;
   watt    : constant Power       := 1.0;
   joule   : constant Energy      := 1.0;
   gram    : constant Mass        := 1.0;
   pragma Warnings (On, "assumed to be");

   ms  : constant Time  := s / 1_000.0;
   min : constant Time  := s * 60.0;
   deg : constant Angle := (Ada.Numerics.Pi / 180.0) * radian;

   subtype Velocity is Dimensioned_Float with
       Dimension => (Symbol => "mm/s", Millimeter => 1, Second => -1, others => 0);
   subtype Acceleration is Dimensioned_Float with
       Dimension => (Symbol => "mm/s²", Millimeter => 1, Second => -2, others => 0);
   subtype Jerk is Dimensioned_Float with
       Dimension => (Symbol => "mm/s³", Millimeter => 1, Second => -3, others => 0);
   subtype Snap is Dimensioned_Float with
       Dimension => (Symbol => "mm/s⁴", Millimeter => 1, Second => -4, others => 0);
   subtype Crackle is Dimensioned_Float with
       Dimension => (Symbol => "mm/s⁵", Millimeter => 1, Second => -5, others => 0);

   subtype Area is Dimensioned_Float with
       Dimension => (Symbol => "mm²", Millimeter => 2, others => 0);
   subtype Volume is Dimensioned_Float with
       Dimension => (Symbol => "mm³", Millimeter => 3, others => 0);
   subtype Hypervolume is Dimensioned_Float with
       Dimension => (Symbol => "mm⁴", Millimeter => 4, others => 0);

   subtype Curvature is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻¹", Millimeter => -1, others => 0);
   subtype Curvature_To_2 is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻²", Millimeter => -2, others => 0);
   subtype Curvature_To_3 is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻³", Millimeter => -3, others => 0);
   subtype Curvature_To_4 is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻⁴", Millimeter => -4, others => 0);

   subtype Heat_Flux is Dimensioned_Float with
       Dimension => (Symbol => "W/mm²", Volt => 1, Amp => 1, Millimeter => -2, others => 0);

   subtype Heat_Transfer_Coefficient is Dimensioned_Float with
       Dimension => (Symbol => "W/(mm²°C)", Volt => 1, Amp => 1, Millimeter => -2, Celcius => -1, others => 0);

   subtype Thermal_Conductance is Dimensioned_Float with
       Dimension => (Symbol => "W/°C", Volt => 1, Amp => 1, Celcius => -1, others => 0);

   subtype Heat_Capacity is Dimensioned_Float with
       Dimension => (Symbol => "J/°C", Volt => 1, Amp => 1, Second => 1, Celcius => -1, others => 0);

   subtype Specific_Heat_Capacity is Dimensioned_Float with
       Dimension => (Symbol => "J/(g°C)", Volt => 1, Amp => 1, Second => 1, Gram => -1, Celcius => -1, others => 0);

   subtype Inverse_Temperature is Dimensioned_Float with
       Dimension => (Symbol => "°C⁻¹", Celcius => -1, others => 0);
   subtype Time_Over_Temperature is Dimensioned_Float with
       Dimension => (Symbol => "s/°C", Second => 1, Celcius => -1, others => 0);
   subtype Frequency_Over_Temperature is Dimensioned_Float with
       Dimension => (Symbol => "Hz/°C", Second => -1, Celcius => -1, others => 0);

end Physical_Types;
