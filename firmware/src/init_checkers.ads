package Init_Checkers is

   protected type Init_Checker is
      entry Wait_For_Init;
      --  Immediately returns. Entry condition is satisfied once Report_Init_Done is called.

      procedure Report_Init_Started;
      --  Raises a Constraint_Error if called more than once.

      procedure Report_Init_Done;
      --  Raises a Constraint_Error if called more than once or called before Report_Init_Started.

      procedure Raise_If_Init_Not_Done;
      --  Raises a Constraint_Error if called before Report_Init_Done.
   private
      Init_Started : Boolean := False;
      Init_Done    : Boolean := False;
   end Init_Checker;

end Init_Checkers;