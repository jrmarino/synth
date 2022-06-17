--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.OS_Lib;
with Unix;

package body Parameters is

   package AD  renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;
   package OSL renames GNAT.OS_Lib;

   --------------------------
   --  load_configuration  --
   --------------------------
   function load_configuration (num_cores : cpu_range) return Boolean
   is
      fields_present : Boolean;
      global_present : Boolean;
      sel_profile    : JT.Text;
   begin
      if not AD.Exists (conf_location) then
         declare
            test : String := determine_portsdirs;
         begin
            if test = "" then
               return False;
            end if;
         end;
         declare
            File_Handle : TIO.File_Type;
         begin
            mkdirp_from_file (conf_location);

            TIO.Create (File => File_Handle,
                        Mode => TIO.Out_File,
                        Name => conf_location);
            TIO.Put_Line (File_Handle, "; This Synth configuration file is " &
                            "automatically generated");
            TIO.Put_Line (File_Handle, "; Take care when hand editing!");
            TIO.Close (File => File_Handle);
         exception
            when Error : others =>
               TIO.Put_Line ("Failed to create " & conf_location);
               return False;
         end;
      end if;

      internal_config.Init (Config_File_Name => conf_location,
                            Case_Sensitive => True,
                            On_Type_Mismatch => Config.Be_Quiet);

      if section_exists (master_section, global_01) then
         global_present := all_global_present;
      else
         write_blank_section (master_section);
         global_present := False;
      end if;

      declare
         envprofile : String := OSL.Getenv ("SYNTHPROFILE").all;
      begin
         sel_profile := extract_string (master_section, global_01, live_system);
         if envprofile /= "" then
            if section_exists (envprofile, Field_01) then
               sel_profile := JT.SUS (envprofile);
            end if;
         end if;
      end;
      declare
         profile : constant String := JT.USS (sel_profile);
      begin
         if section_exists (profile, Field_01) then
            fields_present := all_params_present (profile);
         else
            write_blank_section (section => profile);
            fields_present := False;
         end if;
         configuration := load_specific_profile (profile, num_cores);
      end;
      if not fields_present then
         write_configuration (JT.USS (configuration.profile));
      end if;

      if not global_present then
         write_master_section;
      end if;

      return True;
   exception
      when mishap : others => return False;
   end load_configuration;


   ---------------------------
   --  determine_portsdirs  --
   ---------------------------
   function determine_portsdirs return String
   is
      function get_varname return String;
      function get_mkconf return String;

      function get_varname return String is
      begin
         case software_framework is
            when pkgsrc           => return "PKGSRCDIR";
            when ports_collection => return "PORTSDIR";
         end case;
      end get_varname;

      function get_mkconf return String is
      begin
         case software_framework is
            when pkgsrc           => return "mk.conf";
            when ports_collection => return "/etc/make.conf";
         end case;
      end get_mkconf;

      varname : constant String := get_varname;
      mkconf  : constant String := get_mkconf;
   begin
      --  PORTSDIR in environment takes precedence
      declare
         portsdir : String := OSL.Getenv (varname).all;
      begin
         if portsdir /= "" then
            if AD.Exists (portsdir) then
               return portsdir;
            end if;
         end if;
      end;

      case software_framework is
         when pkgsrc =>
            if AD.Exists (std_pkgsrc_loc & "/mk/pkgformat/pkgng") then
               return std_pkgsrc_loc;
            end if;
         when ports_collection =>
            declare
               portsdir : String := query_portsdir;
            begin
               if portsdir = "" then
                  TIO.Put_Line ("It seems that a blank " & varname &
                                  " is defined in " & mkconf);
                  return "";
               end if;
               if AD.Exists (portsdir) then
                  return portsdir;
               end if;
            end;
            if AD.Exists (std_dports_loc) then
               return std_dports_loc;
            elsif AD.Exists (std_ports_loc) then
               return std_ports_loc;
            end if;
      end case;

      TIO.Put_Line (varname & " cannot be determined.");
      TIO.Put_Line ("Please set " & varname & " to a valid path in the environment or " &
                    mkconf);
      return "";
   end determine_portsdirs;


   -----------------------------
   --  load_specific_profile  --
   -----------------------------
   function load_specific_profile (profile : String; num_cores : cpu_range)
                                   return configuration_record
   is
      function opsys_ok return Boolean;
      def_builders : Integer;
      def_jlimit   : Integer;
      res          : configuration_record;
      function opsys_ok return Boolean is
      begin
         case software_framework is
            when ports_collection =>
               return (JT.equivalent (res.operating_sys, "FreeBSD") or else
                       JT.equivalent (res.operating_sys, "DragonFly"));
            when pkgsrc =>
               --  OpenBSD and derivatives are not supported because they lack mount_null (!)
               return (JT.equivalent (res.operating_sys, "FreeBSD") or else
                       JT.equivalent (res.operating_sys, "DragonFly") or else
                       JT.equivalent (res.operating_sys, "NetBSD") or else
                       JT.equivalent (res.operating_sys, "Linux") or else
                       JT.equivalent (res.operating_sys, "SunOS")
                      );
         end case;
      end opsys_ok;
   begin
      --  The profile *must* exist before this procedure is called!
      default_parallelism (num_cores        => num_cores,
                           num_builders     => def_builders,
                           jobs_per_builder => def_jlimit);

      res.dir_packages := extract_string (profile, Field_01, LS_Packages);
      if param_set (profile, Field_03) then
         --  We can only check determine_portsdirs when no synth.ini file
         --  exists.  It's possible that it was configured with PORTSDIR
         --  set which can be removed later.  Use the dummy std_ports_loc
         --  when we are sure the parameter is set (so dummy will NOT be used)
         res.dir_portsdir := extract_string (profile, Field_03,
                                             std_ports_loc);
      else
         res.dir_portsdir := extract_string (profile, Field_03,
                                             determine_portsdirs);
      end if;
      res.dir_repository := res.dir_packages;
      JT.SU.Append (res.dir_repository, "/All");

      if param_set (profile, Field_04) then
         res.dir_distfiles := extract_string (profile, Field_04, std_distfiles);
      else
         res.dir_distfiles := extract_string
           (profile, Field_04, query_distfiles (JT.USS (res.dir_portsdir)));
      end if;

      res.dir_buildbase := extract_string (profile, Field_05, LS_Buildbase);
      res.dir_logs := extract_string (profile, Field_06, LS_Logs);
      res.dir_ccache := extract_string (profile, Field_07, no_ccache);
      res.num_builders := builders
        (extract_integer (profile, Field_08, def_builders));
      res.jobs_limit := builders
        (extract_integer (profile, Field_09, def_jlimit));

      if param_set (profile, Field_12) then
         res.operating_sys := extract_string (profile, Field_12, std_opsys);
      else
         res.operating_sys := extract_string
           (profile, Field_12, query_opsys (JT.USS (res.dir_portsdir)));
      end if;
      if not opsys_ok then
         TIO.Put_Line ("Unknown operating system: " & JT.USS (res.operating_sys));
         case software_framework is
            when ports_collection =>
               TIO.Put_Line ("This configuration entry must be either 'FreeBSD' or 'DragonFly'");
            when pkgsrc =>
               TIO.Put_Line ("This configuration entry must be one of: FreeBSD,DragonFly," &
                               "NetBSD,Linux,SunOS");
         end case;
         TIO.Put_Line ("Manually edit " & Definitions.host_localbase &
                         "/etc/synth/synth.ini file to remove the line of the");
         TIO.Put_Line (profile & " profile starting with 'Operating_system='");
         TIO.Put_Line ("The synth.ini file should regenerate properly on the next Synth command.");
         TIO.Put_Line ("");
         raise bad_opsys;
      end if;

      if param_set (profile, Field_10) then
         res.tmpfs_workdir := extract_boolean (profile, Field_10, False);
      else
         res.tmpfs_workdir := extract_boolean
           (profile, Field_10, enough_memory (res.num_builders, res.operating_sys));
      end if;

      if param_set (profile, Field_11) then
         res.tmpfs_localbase := extract_boolean (profile, Field_11, False);
      else
         res.tmpfs_localbase := extract_boolean
           (profile, Field_11, enough_memory (res.num_builders, res.operating_sys));
      end if;

      res.dir_options    := extract_string (profile, Field_13, std_options);
      res.dir_system     := extract_string (profile, Field_14, std_sysbase);
      res.avec_ncurses   := extract_boolean (profile, Field_15, True);
      res.defer_prebuilt := extract_boolean (profile, Field_16, False);
      res.profile        := JT.SUS (profile);
      return res;
   end load_specific_profile;


   ---------------------------
   --  write_configuration  --
   ---------------------------
   procedure write_configuration (profile : String := live_system)
   is
      contents : String := generated_section;
   begin
      internal_config.Replace_Section (profile, contents);
   exception
      when Error : others =>
         raise update_config
           with "Failed to update [" & profile & "] at " & conf_location;
   end write_configuration;


   ---------------------------
   --  write_blank_section  --
   ---------------------------
   procedure write_blank_section (section : String)
   is
      File_Handle : TIO.File_Type;
   begin
      TIO.Open (File => File_Handle,
                Mode => TIO.Append_File,
                Name => conf_location);
      TIO.Put_Line (File_Handle, "");
      TIO.Put_Line (File_Handle, "[" & section & "]");
      TIO.Close (File_Handle);
   exception
      when Error : others =>
         raise update_config
           with "Failed to append [" & section & "] at " & conf_location;
   end write_blank_section;


   ---------------------------
   --  default_parallelism  --
   ---------------------------
   procedure default_parallelism (num_cores : cpu_range;
                                  num_builders : out Integer;
                                  jobs_per_builder : out Integer)
   is
   begin
      case num_cores is
         when 1 =>
            num_builders := 1;
            jobs_per_builder := 1;
         when 2 | 3 =>
            num_builders := 2;
            jobs_per_builder := 2;
         when 4 | 5 =>
            num_builders := 3;
            jobs_per_builder := 3;
         when 6 | 7 =>
            num_builders := 4;
            jobs_per_builder := 3;
         when 8 | 9 =>
            num_builders := 6;
            jobs_per_builder := 4;
         when 10 | 11 =>
            num_builders := 8;
            jobs_per_builder := 4;
         when others =>
            num_builders := (Integer (num_cores) * 3) / 4;
            jobs_per_builder := 5;
      end case;
   end default_parallelism;


   ----------------------
   --  extract_string  --
   ----------------------
   function extract_string  (profile, mark, default : String) return JT.Text
   is
   begin
      return JT.SUS (internal_config.Value_Of (profile, mark, default));
   end extract_string;


   -----------------------
   --  extract_boolean  --
   -----------------------
   function extract_boolean (profile, mark : String; default : Boolean)
                             return Boolean
   is
   begin
      return internal_config.Value_Of (profile, mark, default);
   end extract_boolean;


   -----------------------
   --  extract_integer  --
   -----------------------
   function extract_integer (profile, mark : String; default : Integer)
                             return Integer is
   begin
      return internal_config.Value_Of (profile, mark, default);
   end extract_integer;


   -----------------
   --  param_set  --
   -----------------
   function param_set (profile, field : String) return Boolean
   is
      garbage : constant String := "this-is-garbage";
   begin
      return internal_config.Value_Of (profile, field, garbage) /= garbage;
   end param_set;


   ----------------------
   --  section_exists  --
   ----------------------
   function section_exists (profile, mark : String) return Boolean is
   begin
      return param_set (profile, mark);
   end section_exists;

   --------------------------
   --  all_params_present  --
   --------------------------
   function all_params_present (profile : String) return Boolean is
   begin
      return
        param_set (profile, Field_01) and then
        param_set (profile, Field_02) and then
        param_set (profile, Field_03) and then
        param_set (profile, Field_04) and then
        param_set (profile, Field_05) and then
        param_set (profile, Field_06) and then
        param_set (profile, Field_07) and then
        param_set (profile, Field_08) and then
        param_set (profile, Field_09) and then
        param_set (profile, Field_10) and then
        param_set (profile, Field_11) and then
        param_set (profile, Field_12) and then
        param_set (profile, Field_13) and then
        param_set (profile, Field_14) and then
        param_set (profile, Field_15) and then
        param_set (profile, Field_16);
   end all_params_present;


   --------------------------
   --  all_global_present  --
   --------------------------
   function all_global_present return Boolean is
   begin
      return
        param_set (master_section, global_01);
   end all_global_present;


   -------------------------
   --  generated_section  --
   -------------------------
   function generated_section return String
   is
      function USS (US : JT.Text) return String;
      function BDS (BD : builders) return String;
      function TFS (TF : Boolean) return String;

      function USS (US : JT.Text) return String is
      begin
         return "= " & JT.USS (US) & LAT.LF;
      end USS;
      function BDS (BD : builders) return String
      is
         BDI : constant String := Integer'Image (Integer (BD));
      begin
         return LAT.Equals_Sign & BDI & LAT.LF;
      end BDS;
      function TFS (TF : Boolean) return String is
      begin
         if TF then
            return LAT.Equals_Sign & " true" & LAT.LF;
         else
            return LAT.Equals_Sign & " false" & LAT.LF;
         end if;
      end TFS;
   begin
      return
        Field_12 & USS (configuration.operating_sys) &
        Field_01 & USS (configuration.dir_packages) &
        Field_02 & USS (configuration.dir_repository) &
        Field_03 & USS (configuration.dir_portsdir) &
        Field_13 & USS (configuration.dir_options) &
        Field_04 & USS (configuration.dir_distfiles) &
        Field_05 & USS (configuration.dir_buildbase) &
        Field_06 & USS (configuration.dir_logs) &
        Field_07 & USS (configuration.dir_ccache) &
        Field_14 & USS (configuration.dir_system) &
        Field_08 & BDS (configuration.num_builders) &
        Field_09 & BDS (configuration.jobs_limit) &
        Field_10 & TFS (configuration.tmpfs_workdir) &
        Field_11 & TFS (configuration.tmpfs_localbase) &
        Field_15 & TFS (configuration.avec_ncurses) &
        Field_16 & TFS (configuration.defer_prebuilt);
   end generated_section;


   -----------------------
   --  query_distfiles  --
   -----------------------
   function query_distfiles (portsdir : String) return  String
   is
      --  DISTDIR is used by both pkgsrc and ports collection
   begin
      return query_generic (portsdir, "DISTDIR");
   end query_distfiles;


   ------------------
   --  query_opsys  --
   -------------------
   function query_opsys (portsdir : String) return String
   is
      --  OPSYS is used by both pkgsrc and ports collection
   begin
      return query_generic (portsdir, "OPSYS");
   end query_opsys;


   ---------------------
   --  query_generic  --
   ---------------------
   function query_generic (portsdir, value : String) return String
   is
      --  devel/gmake exists on both pkgsrc and the ports collection
      --  The actual port is not significant
      command  : constant String := host_make_program & " -C " & portsdir &
                 "/devel/gmake .MAKE.EXPAND_VARIABLES=yes -V " & value;
   begin
      return query_generic_core (command);
   end query_generic;


   --------------------------
   --  query_generic_core  --
   --------------------------
   function query_generic_core (command : String) return String
   is
      content  : JT.Text;
      status   : Integer;
      CR_loc   : Integer;
      CR       : constant String (1 .. 1) := (1 => Character'Val (10));
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         raise make_query with command;
      end if;
      CR_loc := JT.SU.Index (Source => content, Pattern => CR);

      return JT.SU.Slice (Source => content, Low => 1, High => CR_loc - 1);
   end query_generic_core;


   ----------------------
   --  query_portsdir  --
   ----------------------
   function query_portsdir return String
   is
      --  This is specific to the ports collection (invalid for pkgsrc)
      command  : constant String := host_make_program &
        " -f /usr/share/mk/bsd.port.mk -V PORTSDIR";
   begin
      return query_generic_core (command);
   exception
      when others => return "";
   end query_portsdir;


   -----------------------------
   --  query_physical_memory  --
   -----------------------------
   procedure query_physical_memory is
      --  Works for *BSD, DragonFly, Bitrig
      --  DF/Free "hw.physmem: 8525971456"  (8G)
      --  NetBSD  "hw.physmem = 1073278976" (1G)
      command : constant String := "/sbin/sysctl hw.physmem";
      content : JT.Text;
      status  : Integer;
   begin
      memory_megs := 1024;
      content := Unix.piped_command (command, status);
      if status /= 0 then
         TIO.Put_Line ("command failed: " & command);
         return;
      end if;
      declare
         type styles is (unknown, dragonfly, netbsd);
         function get_number_string return String;
         style    : styles := unknown;
         response : constant String :=
           JT.USS (content) (1 .. JT.SU.Length (content) - 1);
         SP1 : constant String (1 .. 2) := (1 => LAT.Colon,
                                            2 => LAT.Space);
         SP2 : constant String (1 .. 2) := (1 => LAT.Equals_Sign,
                                            2 => LAT.Space);
         function get_number_string return String is
         begin
            case style is
               when dragonfly => return JT.part_2 (response, SP1);
               when netbsd    => return JT.part_2 (response, SP2);
               when unknown   => return "1073741824";
            end case;
         end get_number_string;
      begin
         if JT.contains (response, SP1) then
            style := dragonfly;
         elsif JT.contains (response, SP2) then
            style := netbsd;
         else
            TIO.Put_Line ("Anomaly, unable to detect physical memory");
            TIO.Put_Line (response);
            return;
         end if;

         declare
            type memtype is mod 2**64;
            numbers : String := get_number_string;
            bytes   : constant memtype := memtype'Value (numbers);
            megs    : constant memtype := bytes / 1024 / 1024;
         begin
            memory_megs := Natural (megs);
         end;
      end;
   end query_physical_memory;


   -----------------------------------
   --  query_physical_memory_linux  --
   -----------------------------------
   procedure query_physical_memory_linux
   is
      --  On linux, MemTotal should be on first line and that's what we're looking for
      command  : constant String := "/usr/bin/head /proc/meminfo";
      found    : Boolean := False;
      status   : Integer;
      comres   : JT.Text;
      topline  : JT.Text;
      crlen1   : Natural;
      crlen2   : Natural;
   begin
      comres := Unix.piped_command (command, status);
      if status /= 0 then
         raise make_query with command;
      end if;
      crlen1 := JT.SU.Length (comres);
      loop
         exit when found;
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if JT.SU.Length (topline) > 12 then
            declare
               line : String := JT.USS (topline);
            begin
               if line (line'First .. line'First + 8) = "MemTotal:" then
                  declare
                     type memtype is mod 2**64;
                     numbers : String :=
                       JT.part_1 (S         => JT.trim (line (line'First + 9 .. line'Last)),
                                  separator => " ");
                     kilobytes : constant memtype := memtype'Value (numbers);
                     megs      : constant memtype := kilobytes / 1024;
                  begin
                     memory_megs := Natural (megs);
                  end;
                  found := True;
               end if;
            end;
         end if;
      end loop;
   end query_physical_memory_linux;


   -----------------------------------
   --  query_physical_memory_sunos  --
   -----------------------------------
   procedure query_physical_memory_sunos
   is
         --  On Solaris, we're looking for "Memory size" which should be on second line
      command  : constant String := "/usr/sbin/prtconf";
      found    : Boolean := False;
      status   : Integer;
      comres   : JT.Text;
      topline  : JT.Text;
      crlen1   : Natural;
      crlen2   : Natural;
   begin
      comres := Unix.piped_command (command, status);
      if status /= 0 then
         raise make_query with command;
      end if;
      crlen1 := JT.SU.Length (comres);
      loop
         exit when found;
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if JT.SU.Length (topline) > 12 then
            declare
               line : String := JT.USS (topline);
            begin
               if line (line'First .. line'First + 11) = "Memory size:" then
                  declare
                     type memtype is mod 2**64;
                     numbers : String :=
                       JT.part_1 (line (line'First + 13 .. line'Last), " ");
                     megabytes : constant memtype := memtype'Value (numbers);
                  begin
                     memory_megs := Natural (megabytes);
                  end;
                  found := True;
               end if;
            end;
         end if;
      end loop;
   end query_physical_memory_sunos;


   ---------------------
   --  enough_memory  --
   ---------------------
   function enough_memory (num_builders : builders; opsys : JT.Text) return Boolean is
      megs_per_slave : Natural;
   begin
      if memory_megs = 0 then
         if JT.equivalent (opsys, "Linux") then
            query_physical_memory_linux;
         elsif JT.equivalent (opsys, "SunOS") then
            query_physical_memory_sunos;
         else
            query_physical_memory;
         end if;
      end if;
      megs_per_slave := memory_megs / Positive (num_builders);
      return megs_per_slave >= 1280;
   end enough_memory;


   ----------------------------
   --  write_master_section  --
   ----------------------------
   procedure write_master_section
   is
      function USS (US : JT.Text) return String;
      function USS (US : JT.Text) return String is
      begin
         return "= " & JT.USS (US) & LAT.LF;
      end USS;
      contents : String :=
        global_01 & USS (configuration.profile);
   begin
      internal_config.Replace_Section (master_section, contents);
   exception
      when Error : others =>
         raise update_config
           with "Failed to update [" & master_section & "] at " & conf_location;
   end write_master_section;


   ---------------------
   --  sections_list  --
   ---------------------
   function sections_list return JT.Text
   is
      handle : TIO.File_Type;
      result : JT.Text;
   begin
      TIO.Open (File => handle, Mode => TIO.In_File, Name => conf_location);
      while not TIO.End_Of_File (handle) loop
         declare
            Line : String := TIO.Get_Line (handle);
         begin

            if Line'Length > 0 and then
              Line (1) = '[' and then
              Line /= "[" & master_section & "]"
            then
               JT.SU.Append (result, Line (2 .. Line'Last - 1) & LAT.LF);
            end if;
         end;
      end loop;
      TIO.Close (handle);
      return result;
   end sections_list;


   -----------------------
   --  default_profile  --
   -----------------------
   function default_profile (new_profile : String;
                             num_cores : cpu_range) return configuration_record
   is
      result       : configuration_record;
      def_builders : Integer;
      def_jlimit   : Integer;
   begin
      default_parallelism (num_cores        => num_cores,
                           num_builders     => def_builders,
                           jobs_per_builder => def_jlimit);

      result.dir_portsdir    := JT.SUS (std_ports_loc);
      result.operating_sys   := JT.SUS (query_opsys (std_ports_loc));
      result.profile         := JT.SUS (new_profile);
      result.dir_system      := JT.SUS (std_sysbase);
      result.dir_repository  := JT.SUS (LS_Packages & "/All");
      result.dir_packages    := JT.SUS (LS_Packages);
      result.dir_distfiles   := JT.SUS (std_distfiles);
      result.dir_buildbase   := JT.SUS (LS_Buildbase);
      result.dir_logs        := JT.SUS (LS_Logs);
      result.dir_ccache      := JT.SUS (no_ccache);
      result.dir_options     := JT.SUS (std_options);
      result.num_builders    := builders (def_builders);
      result.jobs_limit      := builders (def_jlimit);
      result.tmpfs_workdir   := enough_memory (result.num_builders, result.operating_sys);
      result.tmpfs_localbase := enough_memory (result.num_builders, result.operating_sys);
      result.avec_ncurses    := True;
      result.defer_prebuilt  := False;

      write_blank_section (section => new_profile);

      return result;
   end default_profile;


   ------------------------
   --  mkdirp_from_file  --
   ------------------------
   procedure mkdirp_from_file (filename : String)
   is
      condir : String := AD.Containing_Directory (Name => filename);
   begin
      AD.Create_Path (New_Directory => condir);
   exception
      when others =>
         raise update_config;
   end mkdirp_from_file;


   -----------------------
   --  all_paths_valid  --
   -----------------------
   function all_paths_valid return Boolean
   is
      use type AD.File_Kind;
      function invalid_directory (folder : JT.Text; desc : String)
                                  return Boolean;
      function invalid_directory (folder : JT.Text; desc : String)
                                  return Boolean
      is
         dossier : constant String := JT.USS (folder);
         errmsg : constant String := "Configuration invalid: ";
      begin
         if AD.Exists (dossier) and then AD.Kind (dossier) = AD.Directory then
            return False;
         else
            TIO.Put_Line (errmsg & desc & " directory: " & dossier);
            return True;
         end if;
      end invalid_directory;
   begin
      declare
         synth_tmp : constant String := "/var/synth";
      begin
         if invalid_directory (JT.SUS (synth_tmp), "xxx") then
            if AD.Exists (synth_tmp) then
               AD.Delete_File (synth_tmp);
            end if;
            AD.Create_Path (New_Directory => synth_tmp);
         end if;
      exception
         when others =>
            TIO.Put_Line ("Failed to create " & synth_tmp);
            return False;
      end;
      if invalid_directory (configuration.dir_system, "[G] System root") then
         return False;
      elsif invalid_directory (configuration.dir_packages, "[B] Packages") then
         return False;
      elsif invalid_directory (configuration.dir_portsdir, "[A] Ports") then
         return False;
      elsif invalid_directory (configuration.dir_distfiles, "[C] Distfiles")
      then
         if JT.equivalent (configuration.dir_distfiles, "/usr/ports/distfiles")
         then
            TIO.Put_Line
              ("Rather than manually creating a directory at this location, " &
                 "consider" & LAT.LF &
                 "using a location outside of the ports tree.  Don't forget " &
                 "to set" & LAT.LF &
              "'DISTDIR' to this new location in /etc/make.conf though.");
         end if;
         return False;
      elsif invalid_directory (configuration.dir_logs, "[E] Build logs") then
         return False;
      elsif invalid_directory (configuration.dir_options, "[D] Port options")
      then
         return False;
      end if;

      if JT.USS (configuration.dir_ccache) = no_ccache then
         return True;
      end if;

      return not invalid_directory (configuration.dir_ccache,
                                    "[H] Compiler cache");

   end all_paths_valid;


   ----------------------
   --  delete_profile  --
   ----------------------
   procedure delete_profile (profile : String)
   is
      old_file : TIO.File_Type;
      new_file : TIO.File_Type;
      nextgen  : constant String := synth_confdir & "/synth.ini.next";
      pattern  : constant String := "[" & profile & "]";
      blocking : Boolean := False;
   begin
      if not AD.Exists (conf_location) then
         raise update_config
           with "The " & conf_location & " configuration file does not exist.";
      end if;
      --  Try to defend malicious symlink: https://en.wikipedia.org/wiki/Symlink_race
      if AD.Exists (nextgen) then
         AD.Delete_File (nextgen);
      end if;
      TIO.Create (File => new_file, Mode => TIO.Out_File, Name => nextgen);
      TIO.Open (File => old_file, Mode => TIO.In_File, Name => conf_location);
      while not TIO.End_Of_File (old_file) loop
         declare
            Line    : constant String := TIO.Get_Line (old_file);
            bracket : Boolean := False;
         begin
            if not JT.IsBlank (Line) then
               bracket := Line (1) = '[';
            end if;
            if bracket and then blocking then
               blocking := False;
            end if;
            if not blocking and then Line = pattern then
               blocking := True;
            end if;
            if not blocking then
               TIO.Put_Line (new_file, Line);
            end if;
         end;
      end loop;
      TIO.Close (old_file);
      TIO.Close (new_file);
      AD.Delete_File (conf_location);
      AD.Rename (Old_Name => nextgen, New_Name => conf_location);
   exception
      when others =>
         if TIO.Is_Open (new_file) then
            TIO.Close (new_file);
         end if;
         if AD.Exists (nextgen) then
            AD.Delete_File (nextgen);
         end if;
         if TIO.Is_Open (old_file) then
            TIO.Close (old_file);
         end if;
         raise update_config
           with  "Failed to remove " & profile & " profile";
   end delete_profile;


   ----------------------------------
   --  alternative_profiles_exist  --
   ----------------------------------
   function alternative_profiles_exist return Boolean
   is
      counter : Natural := 0;
      conf_file : TIO.File_Type;
   begin
      if not AD.Exists (conf_location) then
         return False;
      end if;
      TIO.Open (File => conf_file, Mode => TIO.In_File, Name => conf_location);
      while not TIO.End_Of_File (conf_file) loop
         declare
            Line    : constant String := TIO.Get_Line (conf_file);
            bracket : Boolean := False;
         begin
            if not JT.IsBlank (Line) then
               bracket := Line (1) = '[';
            end if;
            if bracket and then Line /= "[" & master_section & "]"
            then
               counter := counter + 1;
            end if;
         end;
      end loop;
      TIO.Close (conf_file);
      return counter > 1;
   exception
      when others =>
         if TIO.Is_Open (conf_file) then
            TIO.Close (conf_file);
         end if;
         return False;
   end alternative_profiles_exist;

end Parameters;
