pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32G474xx.svd

pragma Restrictions (No_Elaboration_Code);

with HAL;
with System;

package STM32_SVD.DMAMUX is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype C0CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C0CR_SPOL_Field is HAL.UInt2;
   subtype C0CR_NBREQ_Field is HAL.UInt5;
   subtype C0CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C0CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C0CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C0CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C0CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C0CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C0CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C1CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C1CR_SPOL_Field is HAL.UInt2;
   subtype C1CR_NBREQ_Field is HAL.UInt5;
   subtype C1CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C1CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C1CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C1CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C1CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C1CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C2CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C2CR_SPOL_Field is HAL.UInt2;
   subtype C2CR_NBREQ_Field is HAL.UInt5;
   subtype C2CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C2CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C2CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C2CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C2CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C2CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C3CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C3CR_SPOL_Field is HAL.UInt2;
   subtype C3CR_NBREQ_Field is HAL.UInt5;
   subtype C3CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C3CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C3CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C3CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C3CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C3CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C3CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C4CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C4CR_SPOL_Field is HAL.UInt2;
   subtype C4CR_NBREQ_Field is HAL.UInt5;
   subtype C4CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C4CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C4CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C4CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C4CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C4CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C4CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C5CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C5CR_SPOL_Field is HAL.UInt2;
   subtype C5CR_NBREQ_Field is HAL.UInt5;
   subtype C5CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C5CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C5CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C5CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C5CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C5CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C5CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C6CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C6CR_SPOL_Field is HAL.UInt2;
   subtype C6CR_NBREQ_Field is HAL.UInt5;
   subtype C6CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C6CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C6CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C6CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C6CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C6CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C6CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C7CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C7CR_SPOL_Field is HAL.UInt2;
   subtype C7CR_NBREQ_Field is HAL.UInt5;
   subtype C7CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C7CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C7CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C7CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C7CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C7CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C7CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C8CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C8CR_SPOL_Field is HAL.UInt2;
   subtype C8CR_NBREQ_Field is HAL.UInt5;
   subtype C8CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C8CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C8CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C8CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C8CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C8CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C8CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C9CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C9CR_SPOL_Field is HAL.UInt2;
   subtype C9CR_NBREQ_Field is HAL.UInt5;
   subtype C9CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C9CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C9CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C9CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C9CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C9CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C9CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C10CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C10CR_SPOL_Field is HAL.UInt2;
   subtype C10CR_NBREQ_Field is HAL.UInt5;
   subtype C10CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C10CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C10CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C10CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C10CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C10CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C10CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C11CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C11CR_SPOL_Field is HAL.UInt2;
   subtype C11CR_NBREQ_Field is HAL.UInt5;
   subtype C11CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C11CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C11CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C11CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C11CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C11CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C11CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C12CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C12CR_SPOL_Field is HAL.UInt2;
   subtype C12CR_NBREQ_Field is HAL.UInt5;
   subtype C12CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C12CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C12CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C12CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C12CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C12CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C12CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C13CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C13CR_SPOL_Field is HAL.UInt2;
   subtype C13CR_NBREQ_Field is HAL.UInt5;
   subtype C13CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C13CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C13CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C13CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C13CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C13CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C13CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C14CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C14CR_SPOL_Field is HAL.UInt2;
   subtype C14CR_NBREQ_Field is HAL.UInt5;
   subtype C14CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C14CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C14CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C14CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C14CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C14CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C14CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C15CR_DMAREQ_ID_Field is HAL.UInt7;
   subtype C15CR_SPOL_Field is HAL.UInt2;
   subtype C15CR_NBREQ_Field is HAL.UInt5;
   subtype C15CR_SYNC_ID_Field is HAL.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C15CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C15CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C15CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C15CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C15CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C15CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype CSR_SOF_Field is HAL.UInt16;

   --  DMAMUX request line multiplexer interrupt channel status register
   type CSR_Register is record
      --  Read-only. Synchronization overrun event flag
      SOF            : CSR_SOF_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      SOF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CFR_CSOF_Field is HAL.UInt16;

   --  DMAMUX request line multiplexer interrupt clear flag register
   type CFR_Register is record
      --  Write-only. Clear synchronization overrun event flag
      CSOF           : CFR_CSOF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFR_Register use record
      CSOF           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RG0CR_SIG_ID_Field is HAL.UInt5;
   subtype RG0CR_GPOL_Field is HAL.UInt2;
   subtype RG0CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG0CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG0CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG0CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG0CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG0CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG1CR_SIG_ID_Field is HAL.UInt5;
   subtype RG1CR_GPOL_Field is HAL.UInt2;
   subtype RG1CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG1CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG1CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG1CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG1CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG1CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG2CR_SIG_ID_Field is HAL.UInt5;
   subtype RG2CR_GPOL_Field is HAL.UInt2;
   subtype RG2CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG2CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG2CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG2CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG2CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG2CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG3CR_SIG_ID_Field is HAL.UInt5;
   subtype RG3CR_GPOL_Field is HAL.UInt2;
   subtype RG3CR_GNBREQ_Field is HAL.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG3CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG3CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG3CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG3CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG3CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RGSR_OF_Field is HAL.UInt4;

   --  DMAMux - DMA request generator status register
   type RGSR_Register is record
      --  Read-only. Trigger event overrun flag The flag is set when a trigger
      --  event occurs on DMA request generator channel x, while the DMA
      --  request generator counter value is lower than GNBREQ. The flag is
      --  cleared by writing 1 to the corresponding COFx bit in DMAMUX_RGCFR
      --  register.
      OF_k          : RGSR_OF_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGSR_Register use record
      OF_k          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype RGCFR_COF_Field is HAL.UInt4;

   --  DMAMux - DMA request generator clear flag register
   type RGCFR_Register is record
      --  Write-only. Clear trigger event overrun flag Upon setting, this bit
      --  clears the corresponding overrun flag OFx in the DMAMUX_RGCSR
      --  register.
      COF           : RGCFR_COF_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGCFR_Register use record
      COF           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMAMUX
   type DMAMUX_Peripheral is record
      --  DMAMux - DMA request line multiplexer channel x control register
      C0CR  : aliased C0CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C1CR  : aliased C1CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C2CR  : aliased C2CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C3CR  : aliased C3CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C4CR  : aliased C4CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C5CR  : aliased C5CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C6CR  : aliased C6CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C7CR  : aliased C7CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C8CR  : aliased C8CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C9CR  : aliased C9CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C10CR : aliased C10CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C11CR : aliased C11CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C12CR : aliased C12CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C13CR : aliased C13CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C14CR : aliased C14CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C15CR : aliased C15CR_Register;
      --  DMAMUX request line multiplexer interrupt channel status register
      CSR   : aliased CSR_Register;
      --  DMAMUX request line multiplexer interrupt clear flag register
      CFR   : aliased CFR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG0CR : aliased RG0CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG1CR : aliased RG1CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG2CR : aliased RG2CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG3CR : aliased RG3CR_Register;
      --  DMAMux - DMA request generator status register
      RGSR  : aliased RGSR_Register;
      --  DMAMux - DMA request generator clear flag register
      RGCFR : aliased RGCFR_Register;
   end record
     with Volatile;

   for DMAMUX_Peripheral use record
      C0CR  at 16#0# range 0 .. 31;
      C1CR  at 16#4# range 0 .. 31;
      C2CR  at 16#8# range 0 .. 31;
      C3CR  at 16#C# range 0 .. 31;
      C4CR  at 16#10# range 0 .. 31;
      C5CR  at 16#14# range 0 .. 31;
      C6CR  at 16#18# range 0 .. 31;
      C7CR  at 16#1C# range 0 .. 31;
      C8CR  at 16#20# range 0 .. 31;
      C9CR  at 16#24# range 0 .. 31;
      C10CR at 16#28# range 0 .. 31;
      C11CR at 16#2C# range 0 .. 31;
      C12CR at 16#30# range 0 .. 31;
      C13CR at 16#34# range 0 .. 31;
      C14CR at 16#38# range 0 .. 31;
      C15CR at 16#3C# range 0 .. 31;
      CSR   at 16#80# range 0 .. 31;
      CFR   at 16#84# range 0 .. 31;
      RG0CR at 16#100# range 0 .. 31;
      RG1CR at 16#104# range 0 .. 31;
      RG2CR at 16#108# range 0 .. 31;
      RG3CR at 16#10C# range 0 .. 31;
      RGSR  at 16#140# range 0 .. 31;
      RGCFR at 16#144# range 0 .. 31;
   end record;

   --  DMAMUX
   DMAMUX_Periph : aliased DMAMUX_Peripheral
     with Import, Address => DMAMUX_Base;

end STM32_SVD.DMAMUX;
