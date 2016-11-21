--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with JohnnyText;
with Config;

with Definitions;  use Definitions;

package Parameters is

   package JT renames JohnnyText;

   live_system   : constant String := "LiveSystem";
   no_ccache     : constant String := "disabled";
   synth_confdir : constant String := host_localbase & "/etc/synth";
   type configuration_record is
      record
         operating_sys   : JT.Text;
         profile         : JT.Text;
         dir_system      : JT.Text;
         dir_repository  : JT.Text;
         dir_packages    : JT.Text;
         dir_portsdir    : JT.Text;
         dir_distfiles   : JT.Text;
         dir_buildbase   : JT.Text;
         dir_logs        : JT.Text;
         dir_ccache      : JT.Text;
         dir_options     : JT.Text;
         num_builders    : builders;
         jobs_limit      : builders;
         tmpfs_workdir   : Boolean;
         tmpfs_localbase : Boolean;
         avec_ncurses    : Boolean;
         defer_prebuilt  : Boolean;
      end record;

   configuration  : configuration_record;
   active_profile : JT.Text;

   update_config : exception;

   --  This procedure will create a default configuration file if one
   --  does not already exist, otherwise it will it load it.  In every case,
   --  the "configuration" record will be populated after this is run.
   --  returns "True" on success
   function load_configuration (num_cores : cpu_range) return Boolean;

   --  Maybe a previously valid directory path has been removed.  This
   --  function returns true when all the paths still work.
   --  The configuration must be loaded before it's run, of course.
   function all_paths_valid return Boolean;

   procedure write_configuration (profile : String := live_system);
   procedure write_blank_section (section : String);
   function sections_list return JT.Text;
   function alternative_profiles_exist return Boolean;
   function default_profile (new_profile : String;
                             num_cores : cpu_range) return configuration_record;
   function load_specific_profile (profile : String; num_cores : cpu_range)
                                   return configuration_record;
   procedure write_master_section;
   procedure delete_profile (profile : String);

private

   internal_config : Config.Configuration;
   distfiles_loc   : JT.Text;
   memory_megs     : Natural := 0;

   make_query      : exception;
   bad_opsys       : exception;

   --  Default Sizing by number of CPUS
   --      1 CPU ::  1 Builder,  1 job  per builder
   --    2/3 CPU ::  2 builders, 2 jobs per builder
   --    4/5 CPU ::  3 builders, 3 jobs per builder
   --    6/7 CPU ::  4 builders, 3 jobs per builder
   --    8/9 CPU ::  6 builders, 4 jobs per builder
   --  10/11 CPU ::  8 builders, 4 jobs per builder
   --    12+ CPU :: floor (75% * CPU), 5 jobs per builder

   --  Each section is identical, but represents a profile
   --  Selection 1 is the live system, a.k.a "[LiveSystem]"
   --  If /usr/dports exists, that will be the default ports tree directory,
   --  otherwise /usr/ports is.
   --  The OpSys and distfiles are queried so "std_opsys" and "std_distfiles"
   --  values are really not used at all.
   --  LS_Builders   : Integer  (Queried, see cpu-based presets)
   --  LS_Jobs_limit : Integer  (Queried, see cpu-based presets)

   LS_Packages    : constant String := "/var/synth/live_packages";
   LS_Logs        : constant String := "/var/log/synth";
   LS_Buildbase   : constant String := "/usr/obj/synth-live";
   conf_location  : constant String := synth_confdir & "/synth.ini";
   std_ports_loc  : constant String := "/usr/ports";
   std_dports_loc : constant String := "/usr/dports";
   std_pkgsrc_loc : constant String := "/usr/pkgsrc";
   std_distfiles  : constant String := "/usr/ports/distfiles";
   std_options    : constant String := "/var/db/ports";
   std_sysbase    : constant String := "/";
   std_opsys      : constant String := "UnKnown";
   master_section : constant String := "Global Configuration";

   Field_01 : constant String := "Directory_packages";
   Field_02 : constant String := "Directory_repository";
   Field_03 : constant String := "Directory_portsdir";
   Field_04 : constant String := "Directory_distfiles";
   Field_05 : constant String := "Directory_buildbase";
   Field_06 : constant String := "Directory_logs";
   Field_07 : constant String := "Directory_ccache";
   Field_08 : constant String := "Number_of_builders";
   Field_09 : constant String := "Max_jobs_per_builder";
   Field_10 : constant String := "Tmpfs_workdir";
   Field_11 : constant String := "Tmpfs_localbase";
   Field_12 : constant String := "Operating_system";
   Field_13 : constant String := "Directory_options";
   Field_14 : constant String := "Directory_system";
   Field_15 : constant String := "Display_with_ncurses";
   Field_16 : constant String := "leverage_prebuilt";

   global_01 : constant String := "profile_selected";

   procedure default_parallelism (num_cores : cpu_range;
                                  num_builders : out Integer;
                                  jobs_per_builder : out Integer);

   --  These pull requested configuration information.  If they aren't set,
   --  they'll set it to the default (which it also returns);
   function extract_string  (profile, mark, default : String) return JT.Text;
   function extract_boolean (profile, mark : String; default : Boolean)
                             return Boolean;
   function extract_integer (profile, mark : String; default : Integer)
                             return Integer;

   function section_exists (profile, mark : String) return Boolean;
   function all_params_present (profile : String) return Boolean;
   function all_global_present return Boolean;
   function generated_section return String;
   function param_set (profile, field : String) return Boolean;
   function query_generic (portsdir, value : String) return String;
   function query_generic_core (command : String) return String;
   function query_distfiles (portsdir : String) return String;
   function query_opsys (portsdir : String) return String;
   function query_portsdir return String;
   function enough_memory (num_builders : builders; opsys : JT.Text) return Boolean;
   function determine_portsdirs return String;
   procedure query_physical_memory;
   procedure query_physical_memory_linux;
   procedure query_physical_memory_sunos;
   procedure mkdirp_from_file (filename : String);

end Parameters;
