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
                      "'install', 'force'");
      TIO.Put_Line (zp & "'test'" & LAT.LF);
   end print_version;


   ------------------
   --  print_help  --
   ------------------
   procedure print_help
   is
      subtype ofield is String (1 .. 20);
      dashes : constant String (1 .. 79) := (others => LAT.Equals_Sign);
      --  zero parameter options
      opt01 : constant ofield := "status              ";
      opt02 : constant ofield := "configure           ";
      opt03 : constant ofield := "upgrade-system      ";
      opt04 : constant ofield := "update-repository   ";
      opt05 : constant ofield := "purge-distfiles     ";
      opt06 : constant ofield := "everything          ";
      opt07 : constant ofield := "version             ";
      opt08 : constant ofield := "help                ";
      --  list options
      opt09 : constant ofield := "status [ports]      ";
      opt10 : constant ofield := "build [ports]       ";
      opt11 : constant ofield := "just-build [ports]  ";
      opt12 : constant ofield := "install [ports]     ";
      opt13 : constant ofield := "force [ports]       ";
      opt14 : constant ofield := "test [ports]        ";
      blank : constant String (1 .. 26) := (others => LAT.Space);
      synth : constant String := "synth ";
   begin
      TIO.Put_Line (LAT.LF & "Summary of command line options - " &
                      "see synth.1 man page for more details");
      TIO.Put_Line (dashes);
      TIO.Put_Line (synth & opt01 &
                      "Dry-run: Shows what 'upgrade-system' would build");
      TIO.Put_Line (synth & opt02 &
                      "Brings up interactive configuration menu");
      TIO.Put_Line (synth & opt03 &
                      "Incremental rebuild of installed packages on system.");
      TIO.Put_Line (blank &
                      "Afterwards, the local repository is rebuilt and the");
      TIO.Put_Line (blank &
                      "system packages are automatically upgraded.");
      TIO.Put_Line (synth & opt04 &
                      "Like 'upgrade-system' except system is not upgraded");
      TIO.Put_Line (synth & opt05 &
                      "Deletes obsolete source distribution files");
      TIO.Put_Line (synth & opt06 &
                      "Builds entire ports tree and rebuilds repository");
      TIO.Put_Line (synth & opt07 &
                      "Displays version, description and usage summary");
      TIO.Put_Line (synth & opt08 &
                      "Displays this screen");
      TIO.Put_Line (synth & opt09 &
                      "Dry-run: Shows what will be rebuilt with given list");
      TIO.Put_Line (synth & opt10 &
                      "Incrementally build ports based on given list, but");
      TIO.Put_Line (blank &
                      "asks before updating repository and system");
      TIO.Put_Line (synth & opt11 &
                      "Like 'build', but skips post-build questions");
      TIO.Put_Line (synth & opt12 &
                      "Like 'build', but upgrades system without asking");
      TIO.Put_Line (synth & opt13 &
                      "Like 'build', but deletes existing packages first");
      TIO.Put_Line (synth & opt14 &
                      "Like 'just-build', but with DEVELOPER=yes set");
      TIO.Put_Line (LAT.LF & "[ports] is a space-delimited list of origins, " &
                      "e.g. editors/joe editors/emacs.");
      TIO.Put_Line ("It may also be a path to a file containing one origin " &
                      "per line." & LAT.LF);
   end print_help;

end Actions;
