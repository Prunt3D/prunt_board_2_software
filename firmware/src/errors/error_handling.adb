with Heaters;
with STM32.Device;  use STM32.Device;

package body Error_Handling is

   procedure Make_Safe is
   begin
      --  Ensure clocks are enabled in case of a very early failure, although in this case the heaters should not ever
      --  be enabled.
      Enable_Clock (GPIO_A);
      Enable_Clock (GPIO_B);
      Enable_Clock (GPIO_C);
      Enable_Clock (GPIO_D);
      Enable_Clock (GPIO_F);

      Heaters.Make_Safe;
   end Make_Safe;

end Error_Handling;
