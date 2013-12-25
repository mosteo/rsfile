with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Gnat.OS_Lib;
with GNAT.Traceback.Symbolic;

procedure Rsfile is

   Exit_OK  : constant := 0;
   Exit_Arg : constant := 1;
   Exit_Ex  : constant := 2;

   function "+" (S : String) return Unbounded_String
                 renames To_Unbounded_String;

   procedure Log (S : String) renames Ada.Text_IO.Put_Line;

   use type Ada.Directories.File_Size;
   package Size_File is new Ada.Containers.Indefinite_Ordered_Maps
     (Ada.Directories.File_Size, String);

   Root : Unbounded_String;

   Candidates : Size_File.Map;

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

-------------------
-- Populate_Tree --
-------------------

   procedure Populate_Tree is
      use Ada.Directories;
      Root : String := To_String (Rsfile.Root);
      St   : Search_Type;
   begin
      Start_Search (St, Root, "");

   exception
      when others =>
         End_Search (St);
         raise;
   end Populate_Tree;

---------------
-- Pick_File --
---------------

   procedure Pick_File is
   begin
      null;
   end Pick_File;

   use Gnat.OS_Lib;

begin
   if not Check_Parameters then
      Os_Exit(Exit_Arg);
   end if;

   Populate_Tree;

   Pick_File;

   Os_Exit (Exit_OK);
exception
   when E : others =>
      declare
         use Ada.Exceptions;
         use Ada.Text_IO;
      begin
         Put_Line ("Couldn't complete:");
         Put_Line ("Exception:   " & Exception_Message (E));
         Put_Line ("Information: " & Exception_Information (E));
         Put_Line ("Call stack:  " & Gnat.Traceback.Symbolic.Symbolic_Traceback (E));
         Os_Exit (Exit_Ex);
      end;
end Rsfile;
