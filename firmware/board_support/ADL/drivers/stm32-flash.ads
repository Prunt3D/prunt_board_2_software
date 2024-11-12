private with STM32_SVD.Flash;
with HAL;
with System;
with System.Storage_Elements; use System.Storage_Elements;

package STM32.Flash is

   --  Only dual bank mode and writing to the inactive bank is supported.

   type Flash_Data_Element is array (1 .. 2) of HAL.UInt32 with
     Pack;

   type Flash_Data is array (Natural range <>) of Flash_Data_Element with
     Pack, Alignment => 4;

   type Flash_Memory is limited private;

   function Is_Locked (This : in out Flash_Memory) return Boolean;
   function Is_In_Dual_Bank_Mode (This : in out Flash_Memory) return Boolean;
   procedure Erase_Inactive_Bank (This : in out Flash_Memory) with
     Pre => Is_In_Dual_Bank_Mode (This) and then not Is_Locked (This);
   procedure Write (This : in out Flash_Memory; Address : System.Address; Data : Flash_Data) with
     Pre =>
      Is_In_Dual_Bank_Mode (This) and then To_Integer (Address) mod 8 = 0
      and then To_Integer (Address) >= 16#0804_0000# and then To_Integer (Address) + Data'Length * 8 < 16#0808_0000#;
   procedure Unlock (This : in out Flash_Memory) with
     Post => Is_In_Dual_Bank_Mode (This) and then not Is_Locked (This);
   procedure Switch_Active_Bank_And_Reset (This : in out Flash_Memory) with
     Pre => Is_In_Dual_Bank_Mode (This) and then not Is_Locked (This);

private

   type Flash_Memory is new STM32_SVD.Flash.FLASH_Peripheral;


   procedure Disable_Cache (This : in out Flash_Memory);
   procedure Enable_Cache (This : in out Flash_Memory);

end STM32.Flash;
