--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Text_IO;
with Ada.Characters.Latin_1;

with Definitions;   use Definitions;

package body Actions is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   ---------------------
   --  print_version  --
   ---------------------
   procedure print_version
   is
      version   : constant String := synth_version_major & "." &
                  synth_version_minor;
      copyright : constant String := "Copyright (C) " & copywrite_years &
                  " John R. Marino";
      tagline   : constant String := "Custom package repository builder for " &
                  "FreeBSD and DragonFly " & version;
      dashes    : constant String (1 .. tagline'Length + 4)
                  := (others => LAT.Equals_Sign);
      gap       : constant String (1 .. (dashes'Length - copyright'Length) / 2)
                  := (others => LAT.Space);
      zp        : constant String (1 .. 22) := (others => LAT.Space);
   begin
      TIO.Put_Line (LAT.LF & dashes);
      TIO.Put_Line ("  " & tagline);
      TIO.Put_Line (dashes);
      TIO.Put_Line (gap & copyright & LAT.LF & LAT.LF);
      TIO.Put_Line ("Usage: synth [zero-parameter-option]");
      TIO.Put_Line ("-or-   synth [list-option] " &
                      "<list of port origins | filename>" & LAT.LF);
      TIO.Put_Line ("zero-parameter-option includes 'help', 'configure', " &
                      "'version' (this screen),");
      TIO.Put_Line (zp & "'status', 'upgrade-system', 'update-repository',");
      TIO.Put_Line (zp & "'everything', 'purge-distfiles'");
      TIO.Put_Line ("list-option includes  'status', 'build', 'just-build', " &
                      "'install', 'test'");
   end print_version;

end Actions;
