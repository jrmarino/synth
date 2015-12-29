--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with GNAT.OS_Lib;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

procedure SynthExec
is
   package OSL renames GNAT.OS_Lib;
   package CLI renames Ada.Command_Line;
   package ASU renames Ada.Strings.Unbounded;
   subtype Text is ASU.Unbounded_String;
   use type CLI.Exit_Status;

   Args        : OSL.Argument_List_Access;
   return_code : Integer;
   FD          : OSL.File_Descriptor;
   uscommand   : Text;
begin
   --  argument (1) is the absolute path to the log
   --  arguments 2+ are executed as they are

   CLI.Set_Exit_Status (CLI.Failure);
   if CLI.Argument_Count < 3 then
      return;
   end if;

   uscommand := ASU.To_Unbounded_String (CLI.Argument (2));

   for k in 3 .. CLI.Argument_Count loop
      ASU.Append (uscommand, " " & CLI.Argument (k));
   end loop;

   Args := OSL.Argument_String_To_List (ASU.To_String (uscommand));
   FD   := OSL.Open_Append (Name  => CLI.Argument (1),
                            Fmode => OSL.Text);

   OSL.Spawn (Program_Name => Args (Args'First).all,
              Args         => Args (Args'First + 1 .. Args'Last),
              Return_Code  => return_code,
              Output_File_Descriptor => FD);

   OSL.Free (Args);
   OSL.Close (FD);

   if return_code = 0 then
      CLI.Set_Exit_Status (CLI.Success);
   end if;

end SynthExec;
