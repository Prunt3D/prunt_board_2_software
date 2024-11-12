--  Advanced Resource Embedder 1.5.0
with Ada.Streams;
package Embedded_Resources is

   type Content_Access is access constant Ada.Streams.Stream_Element_Array;

   --  Returns the data stream with the given name or null.
   function Get_Content (Name : String) return
      access constant Ada.Streams.Stream_Element_Array;

end Embedded_Resources;
