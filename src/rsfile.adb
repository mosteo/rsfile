with Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with System.OS_Lib;

procedure Rsfile is

   function "+" (S : String) return Unbounded_String
     renames To_Unbounded_String;

   Root : Unbounded_String;

-----------------
-- Print_Usage --
-----------------

   procedure Print_Usage is
      use Ada.Command_Line;
      use Ada.Text_IO;
   begin
      Put_Line ("Usage: " & Command_Name & " <path>");
   end Print_Usage;

----------------------
-- Check_Parameters --
----------------------

   function Check_Parameters return Boolean is
     use Ada.Command_Line;
   begin
      if Argument_Count /= 1 then
         Print_Usage;
         return False;
      else
         Root := +Argument (1);
         return True;
      end if;
   end Check_Parameters;

   procedure Populate_Tree is
   begin
      null;
   end Populate_Tree;

   procedure Pick_File is
   begin
      null;
   end Pick_File;

   use System.OS_Lib;

begin
   if not Check_Parameters then
      Os_Exit(1);
   end if;

   Populate_Tree;

   Pick_File;

   Os_Exit(0);
end Rsfile;
