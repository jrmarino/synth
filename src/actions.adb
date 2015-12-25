--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.OS_Lib;

with JohnnyText;
with Parameters;
with Definitions;   use Definitions;

package body Actions is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;
   package OSL renames GNAT.OS_Lib;
   package JT  renames JohnnyText;
   package PM  renames Parameters;

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


   -----------------------------
   --  launch_configure_menu  --
   -----------------------------
   procedure launch_configure_menu
   is
      dashes : constant String (1 .. 79) := (others => LAT.Equals_Sign);
      indent : constant String (1 ..  3) := (others => LAT.Space);
      subtype ofield is String (1 .. 30);
      type option is range 1 .. 13;
      type desc_type is array (option) of ofield;
      descriptions : desc_type :=
         (
      --  012345678901234567890123456789
          "[A] Ports directory           ",
          "[B] Packages directory        ",
          "[C] Distfiles directory       ",
          "[D] Repository directory      ",
          "[E] Port options directory    ",
          "[F] Build logs directory      ",
          "[G] Build base directory      ",
          "[H] System root directory     ",
          "[I] Compiler cache directory  ",
          "[J] Num. concurrent builders  ",
          "[K] Max. jobs per builder     ",
          "[L] Use tmpfs for work area   ",
          "[M] Use tmpfs for /usr/local  ");

      optX1A : constant String := "[>]   Switch profiles (changes discarded)";
      optX1B : constant String := "[>]   Switch profiles";
      optX2A : constant String := "[ESC] Exit without saving changes";
      optX3A : constant String := "[RET] Save changes (starred)";
      optX3B : constant String := "[RET] Exit";

      dupe     : PM.configuration_record := PM.configuration;
      pristine : Boolean;
      procedure print_opt (opt : option);
      procedure print_opt (opt : option)
      is
         orig : JT.Text;
         next : JT.Text;
         show : JT.Text;
         orignat : builders;
         nextnat : builders;
         origbool : Boolean;
         nextbool : Boolean;
         equivalent : Boolean;
      begin
         TIO.Put (indent & descriptions (opt));
         case opt is
            when 1 => orig := dupe.dir_portsdir;
                      next := PM.configuration.dir_portsdir;
            when 2 => orig := dupe.dir_packages;
                      next := PM.configuration.dir_packages;
            when 3 => orig := dupe.dir_distfiles;
                      next := PM.configuration.dir_distfiles;
            when 4 => orig := dupe.dir_repository;
                      next := PM.configuration.dir_repository;
            when 5 => orig := dupe.dir_options;
                      next := PM.configuration.dir_options;
            when 6 => orig := dupe.dir_logs;
                      next := PM.configuration.dir_logs;
            when 7 => orig := dupe.dir_buildbase;
                      next := PM.configuration.dir_buildbase;
            when 8 => orig := dupe.dir_system;
                      next := PM.configuration.dir_system;
            when 9 => orig := dupe.dir_ccache;
                      next := PM.configuration.dir_ccache;
            when 10 => orignat := dupe.num_builders;
                       nextnat := PM.configuration.num_builders;
            when 11 => orignat := dupe.jobs_limit;
                       nextnat := PM.configuration.jobs_limit;
            when 12 => origbool := dupe.tmpfs_workdir;
                       nextbool := PM.configuration.tmpfs_workdir;
            when 13 => origbool := dupe.tmpfs_localbase;
                       nextbool := PM.configuration.tmpfs_localbase;
         end case;
         case opt is
            when 1 .. 9   => equivalent := JT.equivalent (orig, next);
                             show := next;
            when 10 .. 11 => equivalent := (orignat = nextnat);
                             show := JT.int2text (Integer (nextnat));
            when 12 .. 13 => equivalent := (origbool = nextbool);
                             show := JT.bool2text (nextbool);
         end case;
         if equivalent then
            TIO.Put_Line (" " & JT.USS (show));
         else
            TIO.Put_Line ("*" & JT.USS (show));
            pristine := False;
         end if;
      end print_opt;
   begin
      clear_screen;
      TIO.Put_Line ("Synth configuration profile: " &
                      JT.USS (PM.configuration.profile));
      TIO.Put_Line (dashes);
      pristine := True;
      for line in option'Range loop
         print_opt (line);
      end loop;
      TIO.Put_Line ("");
      if pristine then
         TIO.Put_Line (indent & optX1B);
         TIO.Put_Line (indent & optX3B);
      else
         TIO.Put_Line (indent & optX1A);
         TIO.Put_Line (indent & optX2A);
         TIO.Put_Line (indent & optX3A);
      end if;


   end launch_configure_menu;


   -----------------------
   --  generic_execute  --
   -----------------------
   function generic_execute (command : String) return Boolean
   is
      Args        : OSL.Argument_List_Access;
      Exit_Status : Integer;
   begin
      Args := OSL.Argument_String_To_List (command);
      Exit_Status := OSL.Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);
      return Exit_Status = 0;
   end generic_execute;


   --------------------
   --  clear_screen  --
   --------------------
   procedure clear_screen
   is
      result  : Boolean;
      command : constant String := "/usr/bin/clear";
   begin
      result := generic_execute (command);
   end clear_screen;

end Actions;
