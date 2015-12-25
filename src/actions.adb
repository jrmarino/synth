--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;

with JohnnyText;
with Parameters;
with Definitions;   use Definitions;

package body Actions is

   package AD  renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package INT renames Ada.Integer_Text_IO;
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
      --   012345678901234567890123456789
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
      procedure print_menu;
      procedure print_header;
      procedure change_boolean_option (opt : option);
      procedure change_positive_option (opt : option);
      procedure change_directory_option (opt : option);

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
            when 1 => next := dupe.dir_portsdir;
                      orig := PM.configuration.dir_portsdir;
            when 2 => next := dupe.dir_packages;
                      orig := PM.configuration.dir_packages;
            when 3 => next := dupe.dir_distfiles;
                      orig := PM.configuration.dir_distfiles;
            when 4 => next := dupe.dir_repository;
                      orig := PM.configuration.dir_repository;
            when 5 => next := dupe.dir_options;
                      orig := PM.configuration.dir_options;
            when 6 => next := dupe.dir_logs;
                      orig := PM.configuration.dir_logs;
            when 7 => next := dupe.dir_buildbase;
                      orig := PM.configuration.dir_buildbase;
            when 8 => next := dupe.dir_system;
                      orig := PM.configuration.dir_system;
            when 9 => next := dupe.dir_ccache;
                      orig := PM.configuration.dir_ccache;
            when 10 => nextnat := dupe.num_builders;
                       orignat := PM.configuration.num_builders;
            when 11 => nextnat := dupe.jobs_limit;
                       orignat := PM.configuration.jobs_limit;
            when 12 => nextbool := dupe.tmpfs_workdir;
                       origbool := PM.configuration.tmpfs_workdir;
            when 13 => nextbool := dupe.tmpfs_localbase;
                       origbool := PM.configuration.tmpfs_localbase;
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

      procedure print_header is
      begin
         TIO.Put_Line ("Synth configuration profile: " &
                         JT.USS (PM.configuration.profile));
         TIO.Put_Line (dashes);
      end print_header;

      procedure print_menu is
      begin
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
         TIO.Put_Line ("");
      end print_menu;

      procedure change_boolean_option (opt : option)
      is
         new_value : Boolean;
         TF : Character;
      begin
         clear_screen;
         print_header;
         print_opt (opt);
         TIO.Put (LAT.LF & "Set parameter value (T/F): ");
         loop
            TIO.Get_Immediate (TF);
            case TF is
            when 'T' | 't' =>
               new_value := True;
               exit;
            when 'F' | 'f' =>
               new_value := False;
               exit;
            when others => null;
            end case;
         end loop;
         case opt is
            when 12 => dupe.tmpfs_workdir := new_value;
            when 13 => dupe.tmpfs_localbase := new_value;
            when others => raise menu_error
                 with "Illegal value : " & opt'Img;
         end case;
      end change_boolean_option;

      procedure change_positive_option (opt : option)
      is
         function read_positive return Positive;
         function read_positive return Positive
         is
            number : Positive;
         begin
            INT.Get (number);
            return number;
         exception
            when others =>
               TIO.Skip_Line;
               return 100000;
         end read_positive;

         max_value : Positive;
         given_value : Positive;
         continue : Boolean;
      begin
         loop
            clear_screen;
            print_header;
            print_opt (opt);
            case opt is
            when 10 .. 11 => max_value := Integer (builders'Last);
            when others => raise menu_error
                 with "Illegal value : " & opt'Img;
            end case;
            TIO.Put (LAT.LF & "Set parameter value (1 to" &
                       max_value'Img & "): ");
            continue := True;
            given_value := read_positive;

            if given_value > max_value then
               continue := False;
            else
               case opt is
                  when 10 => dupe.num_builders := builders (given_value);
                  when 11 => dupe.jobs_limit := builders (given_value);
                  when others => null;
               end case;
               exit;
            end if;
            exit when continue;
         end loop;
      end change_positive_option;

      procedure change_directory_option (opt : option)
      is
         continue : Boolean;
      begin
         loop
            continue := False;
            clear_screen;
            print_header;
            print_opt (opt);
            TIO.Put (LAT.LF & "Set valid path for directory: ");
            declare
               testpath : String := TIO.Get_Line;
            begin
               if AD.Exists (testpath) then
                  case opt is
                  when 1 => dupe.dir_portsdir   := JT.SUS (testpath);
                  when 2 => dupe.dir_packages   := JT.SUS (testpath);
                  when 3 => dupe.dir_distfiles  := JT.SUS (testpath);
                  when 4 => dupe.dir_repository := JT.SUS (testpath);
                  when 5 => dupe.dir_options    := JT.SUS (testpath);
                  when 6 => dupe.dir_logs       := JT.SUS (testpath);
                  when 7 => dupe.dir_buildbase  := JT.SUS (testpath);
                  when 8 => dupe.dir_system     := JT.SUS (testpath);
                  when 9 => dupe.dir_ccache     := JT.SUS (testpath);
                  when others => raise menu_error
                       with "Illegal value : " & opt'Img;
                  end case;
                  continue := True;
               end if;
            exception
               when others =>
                  continue := True;
            end;
            exit when continue;
         end loop;
      end change_directory_option;

      answer   : Character;
      ascii    : Natural;
      continue : Boolean := True;
   begin
      loop
         pristine := True;
         clear_screen;
         print_header;
         print_menu;

         TIO.Put ("Press key of selection: ");
         loop
            TIO.Get_Immediate (answer);
            ascii := Character'Pos (answer);
            case answer is
               when 'A' .. 'I' =>
                  change_directory_option (option (ascii - 64));
                  exit;
               when 'a' .. 'i' =>
                  change_directory_option (option (ascii - 96));
                  exit;
               when 'J' .. 'K' =>
                  change_positive_option (option (ascii - 64));
                  exit;
               when 'j' .. 'k' =>
                  change_positive_option (option (ascii - 96));
                  exit;
               when 'L' .. 'M' =>
                  change_boolean_option (option (ascii - 64));
                  exit;
               when 'l' .. 'm' =>
                  change_boolean_option (option (ascii - 96));
                  exit;
               when '>' => exit;
               when LAT.CR | LAT.ESC =>
                  continue := False;
                  exit;
               when others => null;
            end case;
         end loop;
         exit when not continue;
      end loop;

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
