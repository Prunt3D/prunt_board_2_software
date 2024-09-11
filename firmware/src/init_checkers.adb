package body Init_Checkers is

   protected body Init_Checker is
      entry Wait_For_Init when Init_Done is
      begin
         null;
      end Wait_For_Init;

      procedure Report_Init_Started is
      begin
         if Init_Started then
            raise Constraint_Error with "Report_Init_Started called twice.";
         end if;

         Init_Started := True;
      end Report_Init_Started;

      procedure Report_Init_Done is
      begin
         if not Init_Started then
            raise Constraint_Error with "Report_Init_Done called before Report_Init_Started.";
         end if;

         if Init_Done then
            raise Constraint_Error with "Report_Init_Done called twice.";
         end if;

         Init_Done := True;
      end Report_Init_Done;

      procedure Raise_If_Init_Not_Done is
      begin
         if not Init_Done then
            raise Constraint_Error with "Init not done.";
         end if;
      end Raise_If_Init_Not_Done;
   end Init_Checker;

end Init_Checkers;
