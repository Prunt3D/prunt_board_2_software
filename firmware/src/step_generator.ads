private with Ada.Interrupts.Names;
private with System;
private with HAL;
with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;

package Step_Generator is

   procedure Init;

   procedure Enqueue (Steps : Step_Delta);
   procedure Setup_Loop (Input_Switch : Input_Switch_Name; Until_State : Input_Switch_State);
   procedure Enqueue_Start_Loop;
   procedure Enqueue_Stop_Loop;
   function Check_If_Idle return Boolean;
   procedure Force_Start;

   Empty_Buffer_Error : exception;

private

   use HAL;

   type Step_Delta_Buffer_Index is mod 2**13;

   Step_Delta_Buffer : array (Step_Delta_Buffer_Index) of Step_Delta with
     Volatile_Components;

   Step_Delta_Buffer_Writer_Index : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;
   Step_Delta_Buffer_Reader_Index : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;

   Step_Delta_Buffer_Loop_Start_Index : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;
   Step_Delta_Buffer_Loop_End_Index   : Step_Delta_Buffer_Index := 0 with
     Volatile, Atomic;

   Step_Delta_Buffer_Loop_Enabled : Boolean := False with
     Volatile, Atomic;

   Loop_Until_State  : Input_Switch_State with
     Volatile;
   Loop_Input_Switch : Input_Switch_Name with
     Volatile;

   Is_Idle : Boolean := True with
     Volatile, Atomic;

   Buffer_Ran_Dry : Boolean := False with
     Volatile, Atomic;

   protected Timer_Reload_Handler is
      pragma Interrupt_Priority (Step_Generator_Interrupt_Priority);
   private
      procedure Master_Update_Handler with
        Attach_Handler => Ada.Interrupts.Names.HRTIM_Master_IRQn_Interrupt;
   end Timer_Reload_Handler;

   Step_Count_To_Period : constant array (Step_Count) of UInt16 :=
     (0   => 100,
      1   => 58_489,
      2   => 29_245,
      3   => 19_497,
      4   => 14_623,
      5   => 11_698,
      6   => 9_748,
      7   => 8_356,
      8   => 7_311,
      9   => 6_499,
      10  => 5_849,
      11  => 5_317,
      12  => 4_874,
      13  => 4_499,
      14  => 4_178,
      15  => 3_899,
      16  => 3_656,
      17  => 3_441,
      18  => 3_249,
      19  => 3_078,
      20  => 2_925,
      21  => 2_785,
      22  => 2_659,
      23  => 2_543,
      24  => 2_437,
      25  => 2_340,
      26  => 2_250,
      27  => 2_166,
      28  => 2_089,
      29  => 2_017,
      30  => 1_950,
      31  => 1_887,
      32  => 1_828,
      33  => 1_772,
      34  => 1_720,
      35  => 1_671,
      36  => 1_625,
      37  => 1_581,
      38  => 1_539,
      39  => 1_500,
      40  => 1_462,
      41  => 1_427,
      42  => 1_393,
      43  => 1_360,
      44  => 1_329,
      45  => 1_300,
      46  => 1_272,
      47  => 1_244,
      48  => 1_219,
      49  => 1_194,
      50  => 1_170,
      51  => 1_147,
      52  => 1_125,
      53  => 1_104,
      54  => 1_083,
      55  => 1_063,
      56  => 1_044,
      57  => 1_026,
      58  => 1_008,
      59  => 991,
      60  => 975,
      61  => 959,
      62  => 943,
      63  => 928,
      64  => 914,
      65  => 900,
      66  => 886,
      67  => 873,
      68  => 860,
      69  => 848,
      70  => 836,
      71  => 824,
      72  => 812,
      73  => 801,
      74  => 790,
      75  => 780,
      76  => 770,
      77  => 760,
      78  => 750,
      79  => 740,
      80  => 731,
      81  => 722,
      82  => 713,
      83  => 705,
      84  => 696,
      85  => 688,
      86  => 680,
      87  => 672,
      88  => 665,
      89  => 657,
      90  => 650,
      91  => 643,
      92  => 636,
      93  => 629,
      94  => 622,
      95  => 616,
      96  => 609,
      97  => 603,
      98  => 597,
      99  => 591,
      100 => 585);

end Step_Generator;
