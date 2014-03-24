with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
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

   --  procedure Log (S : String) renames Ada.Text_IO.Put_Line;
   procedure Log (S : String) is null;

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
      Put_Line ("Usage: " & Ada.Directories.Simple_Name (Command_Name) & " <path>");
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
      Pos  : File_Size := 0;

      ----------------------
      -- Enumerate_Folder --
      ----------------------

      procedure Enumerate_Folder (Folder : String) is
         St   : Search_Type;
      begin
         Log ("Entering: " & Folder);
         Start_Search (St, Folder, "");

         while More_Entries (St) loop
            declare
               Item : Directory_Entry_Type;
            begin
               Get_Next_Entry (St, Item);

               case Kind (Item) is
                  when Directory =>
                     if
                       Simple_Name (Item) /= "." and then
                       Simple_Name (Item) /= ".."
                     then
                        Enumerate_Folder (Full_Name (Item));
                     end if;
                  when Ordinary_File =>
                     Pos := Pos + Size (Item) + 1;
                     Log ("Insert: " & Simple_Name (Item) & " at" & Pos'Img);
                     Candidates.Insert (Pos, Full_Name (Item));
                  when others =>
                     Log ("Skipping: " & Simple_Name (Item));
               end case;
            end;
         end loop;
      exception
         when others =>
           End_Search (St);
         raise;
      end Enumerate_Folder;

   begin
      Enumerate_Folder (Root);
   end Populate_Tree;

   ---------------
   -- Pick_File --
   ---------------

   procedure Pick_File is
      use Ada.Directories;
      subtype Targets is File_Size range 0 .. Candidates.Last_Key;
      package FSRnd is new Ada.Numerics.Discrete_Random (Targets);
      Rnd : FSRnd.Generator;
   begin
      FSRnd.Reset (Rnd);

      declare
         Index  : constant File_Size := FSRnd.Random (Rnd);

         Target : constant String :=
           Size_File.Element
             (Candidates.Ceiling
                  (Index));
      begin
         Ada.Text_IO.Put_Line (Target);
      end;
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
