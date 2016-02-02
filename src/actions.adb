--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;

with JohnnyText;
with Parameters;

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
      copyright : constant String := "Copyright (C) " & copyright_years &
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
      TIO.Put_Line (zp & "'status', 'upgrade-system', 'prepare-system',");
      TIO.Put_Line (zp & "'status-everything', 'everything', " &
                      "'purge-distfiles',");
      TIO.Put_Line (zp & "'rebuild-repository'");
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
      opt04 : constant ofield := "prepare-system      ";
      opt16 : constant ofield := "rebuild-repository  ";
      opt05 : constant ofield := "purge-distfiles     ";
      opt15 : constant ofield := "status-everything   ";
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
      blank : constant String (1 .. 27) := (1 => LAT.LF, others => LAT.Space);
      synth : constant String := LAT.LF & "synth ";
   begin
      TIO.Put_Line (LAT.LF & "Summary of command line options - " &
                      "see synth.1 man page for more details");
      TIO.Put (dashes);
      TIO.Put_Line
        (synth & opt01 & "Dry-run: Shows what 'upgrade-system' would build" &
         synth & opt02 & "Brings up interactive configuration menu" &
         synth & opt03 & "Incremental rebuild of installed packages on system."
               & blank & "Afterwards, the local repository is rebuilt and the"
               & blank & "system packages are automatically upgraded." &
         synth & opt04 & "Like 'upgrade-system' but ends when repo is rebuilt" &
         synth & opt16 & "Rebuilds local Synth repository on command" &
         synth & opt05 & "Deletes obsolete source distribution files" &
         synth & opt15 & "Dry-run: Shows what 'everything' would build" &
         synth & opt06 & "Builds entire ports tree and rebuilds repository" &
         synth & opt07 & "Displays version, description and usage summary" &
         synth & opt08 & "Displays this screen" &
         synth & opt09 & "Dry-run: Shows what will be rebuilt with given list" &
         synth & opt10 & "Incrementally build ports based on given list, but" &
                 blank & "asks before updating repository and system" &
         synth & opt11 & "Like 'build', but skips post-build questions" &
         synth & opt12 & "Like 'build', but upgrades system without asking" &
         synth & opt13 & "Like 'build', but deletes existing packages first" &
         synth & opt14 & "Just builds with DEVELOPER=yes; pre-deletes pkgs");
      TIO.Put_Line
               (LAT.LF & "[ports] is a space-delimited list of origins, " &
                         "e.g. editors/joe editors/emacs." &
                LAT.LF & "It may also be a path to a file containing one " &
                         "origin per line.");
   end print_help;


   -----------------------------
   --  launch_configure_menu  --
   -----------------------------
   procedure launch_configure_menu (num_cores : cpu_range)
   is
      dashes : constant String (1 .. 79) := (others => LAT.Equals_Sign);
      indent : constant String (1 ..  3) := (others => LAT.Space);
      subtype ofield is String (1 .. 30);
      type option is range 1 .. 14;
      type desc_type is array (option) of ofield;
      descriptions : desc_type :=
         (
      --   012345678901234567890123456789
          "[A] Ports directory           ",
          "[B] Packages directory        ",
          "[C] Distfiles directory       ",
          "[D] Port options directory    ",
          "[E] Build logs directory      ",
          "[F] Build base directory      ",
          "[G] System root directory     ",
          "[H] Compiler cache directory  ",
          "[I] Num. concurrent builders  ",
          "[J] Max. jobs per builder     ",
          "[K] Use tmpfs for work area   ",
          "[L] Use tmpfs for /usr/local  ",
          "[M] Display using ncurses     ",
          "[N] Fetch prebuilt packages   ");

      optX1A : constant String := "[>]   Switch/create profiles " &
                                         "(changes discarded)";
      optX1B : constant String := "[>]   Switch/create profiles";
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
      procedure switch_profile;

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
            when 4 => next := dupe.dir_options;
                      orig := PM.configuration.dir_options;
            when 5 => next := dupe.dir_logs;
                      orig := PM.configuration.dir_logs;
            when 6 => next := dupe.dir_buildbase;
                      orig := PM.configuration.dir_buildbase;
            when 7 => next := dupe.dir_system;
                      orig := PM.configuration.dir_system;
            when 8 => next := dupe.dir_ccache;
                      orig := PM.configuration.dir_ccache;
            when 9 => nextnat := dupe.num_builders;
                      orignat := PM.configuration.num_builders;
            when 10 => nextnat := dupe.jobs_limit;
                       orignat := PM.configuration.jobs_limit;
            when 11 => nextbool := dupe.tmpfs_workdir;
                       origbool := PM.configuration.tmpfs_workdir;
            when 12 => nextbool := dupe.tmpfs_localbase;
                       origbool := PM.configuration.tmpfs_localbase;
            when 13 => nextbool := dupe.avec_ncurses;
                       origbool := PM.configuration.avec_ncurses;
            when 14 => nextbool := dupe.defer_prebuilt;
                       origbool := PM.configuration.defer_prebuilt;
         end case;
         case opt is
            when 1 .. 8   => equivalent := JT.equivalent (orig, next);
                             show := next;
            when 9 .. 10  => equivalent := (orignat = nextnat);
                             show := JT.int2text (Integer (nextnat));
            when 11 .. 14 => equivalent := (origbool = nextbool);
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
            when 11 => dupe.tmpfs_workdir := new_value;
            when 12 => dupe.tmpfs_localbase := new_value;
            when 13 => dupe.avec_ncurses := new_value;
            when 14 => dupe.defer_prebuilt := new_value;
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
            when 9 .. 10 => max_value := Integer (builders'Last);
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
                  when  9 => dupe.num_builders := builders (given_value);
                  when 10 => dupe.jobs_limit := builders (given_value);
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
            TIO.Put (LAT.LF & "Set valid path for directory");
            if opt = 8 then
               TIO.Put (" (or 'none' to disable ccache): ");
            else
               TIO.Put (": ");
            end if;
            declare
               testpath : String := TIO.Get_Line;
            begin
               if AD.Exists (testpath) then
                  case opt is
                  when 1 => dupe.dir_portsdir   := JT.SUS (testpath);
                  when 2 => dupe.dir_packages   := JT.SUS (testpath);
                            dupe.dir_repository := JT.SUS (testpath & "/All");
                  when 3 => dupe.dir_distfiles  := JT.SUS (testpath);
                  when 4 => dupe.dir_options    := JT.SUS (testpath);
                  when 5 => dupe.dir_logs       := JT.SUS (testpath);
                  when 6 => dupe.dir_buildbase  := JT.SUS (testpath);
                  when 7 => dupe.dir_system     := JT.SUS (testpath);
                  when 8 => dupe.dir_ccache     := JT.SUS (testpath);
                  when others => raise menu_error
                       with "Illegal value : " & opt'Img;
                  end case;
                  continue := True;
               elsif opt = 8 then
                  dupe.dir_ccache := JT.SUS (PM.no_ccache);
                  continue := True;
               end if;
            exception
               when others =>
                  continue := True;
            end;
            exit when continue;
         end loop;
      end change_directory_option;

      procedure switch_profile
      is
         function list_profiles (limit : Natural := 0; total : out Natural)
                                 return JT.Text;
         all_profiles : JT.Text := PM.sections_list;
         selected     : JT.Text;
         continue     : Boolean;
         max_menu     : Natural;
         number       : Positive;

         function list_profiles (limit : Natural := 0; total : out Natural)
                                 return JT.Text
         is
            topline  : JT.Text;
            profiles : JT.Text := all_profiles;
            crlen1   : Natural := JT.SU.Length (profiles);
            crlen2   : Natural;
         begin
            total := 0;
            loop
               JT.nextline (lineblock => profiles, firstline => topline);
               crlen2 := JT.SU.Length (profiles);
               exit when crlen1 = crlen2;
               crlen1 := crlen2;
               total := total + 1;
               if limit = 0 then
                  TIO.Put_Line
                    (indent & "[" & JT.int2str (total) & "] Switch to " &
                       LAT.Quotation & JT.USS (topline) & LAT.Quotation &
                       " profile");
               elsif limit = total then
                  return topline;
               end if;
            end loop;
            total := total + 1;
            TIO.Put_Line (indent & "[" & JT.int2str (total) &
                            "] Create new profile");
            total := total + 1;
            TIO.Put_Line (indent & "[" & JT.int2str (total) &
                            "] Do nothing (return to previous screen)");
            return JT.blank;
         end list_profiles;
      begin
         loop
            continue := False;
            clear_screen;
            print_header;
            selected := list_profiles (total => max_menu);
            TIO.Put (LAT.LF & "Select profile number: ");
            declare
            begin
               INT.Get (number);
            exception
               when others =>
                  TIO.Skip_Line;
                  number := 1000;
            end;
            if number = max_menu then
               continue := True;
            elsif number < max_menu - 1 then
               declare
                  dummy : Natural;
                  nprof : JT.Text;
               begin
                  nprof := list_profiles (limit => number, total => dummy);
                  dupe := PM.load_specific_profile (JT.USS (nprof), num_cores);
                  PM.configuration := dupe;
                  PM.write_master_section;
               end;
               continue := True;
            elsif number = max_menu - 1 then
               clear_screen;
               print_header;
               TIO.Skip_Line;
               TIO.Put (LAT.LF & "Name of new profile: ");
               declare
                  newname : String := TIO.Get_Line;
                  empty   : PM.configuration_record;
               begin
                  dupe := PM.default_profile (newname, num_cores);
                  empty.profile := dupe.profile;
                  empty.operating_sys := dupe.operating_sys;
                  PM.configuration := empty;
                  PM.write_master_section;
               exception
                  when others => null;
               end;
               continue := True;
            end if;
            exit when continue;
         end loop;
      end switch_profile;


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
               when 'A' .. 'H' =>
                  change_directory_option (option (ascii - 64));
                  exit;
               when 'a' .. 'h' =>
                  change_directory_option (option (ascii - 96));
                  exit;
               when 'I' .. 'J' =>
                  change_positive_option (option (ascii - 64));
                  exit;
               when 'i' .. 'j' =>
                  change_positive_option (option (ascii - 96));
                  exit;
               when 'K' .. 'N' =>
                  change_boolean_option (option (ascii - 64));
                  exit;
               when 'k' .. 'n' =>
                  change_boolean_option (option (ascii - 96));
                  exit;
               when '>' =>
                  switch_profile;
                  exit;
               when LAT.LF =>
                  if not pristine then
                     PM.configuration := dupe;
                     PM.write_configuration (profile => JT.USS (dupe.profile));
                  end if;
                  continue := False;
                  exit;
               when LAT.ESC =>
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
