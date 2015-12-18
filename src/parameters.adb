--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Util.Streams.Pipes;
with Util.Streams.Buffered;

package body Parameters is

   package AD  renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package STR renames Util.Streams;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------
   --  load_configuration  --
   --------------------------
   function load_configuration (num_cores : cpu_range;
                                profile   : String := live_system)
                                return Boolean
   is
      def_builders   : Integer;
      def_jlimit     : Integer;
      fields_present : Boolean;
   begin
      default_parallelism (num_cores        => num_cores,
                           num_builders     => def_builders,
                           jobs_per_builder => def_jlimit);

      if not AD.Exists (conf_location) then
         declare
            File_Handle : TIO.File_Type;
         begin
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

      internal_config.Init (File_Name => conf_location,
                            On_Type_Mismatch => Config.Be_Quiet);

      if section_exists (profile => profile) then
         fields_present := all_params_present (profile);
      else
         declare
            File_Handle : TIO.File_Type;
         begin
            TIO.Open (File => File_Handle,
                      Mode => TIO.Append_File,
                      Name => conf_location);
            TIO.Put_Line (File_Handle, "");
            TIO.Put_Line (File_Handle, "[" & profile & "]");
            TIO.Close (File_Handle);
         exception
            when Error : others =>
               TIO.Put_Line ("Failed to append " & conf_location);
               return False;
         end;
         fields_present := False;
      end if;

      configuration.dir_packages :=
        extract_string (profile, Field_01, LS_Packages);

      configuration.dir_repository := configuration.dir_packages;
      JT.SU.Append (configuration.dir_repository, "/All");

      configuration.dir_portsdir :=
        extract_string (profile, Field_03, std_ports_loc);

      if param_set (profile, Field_04) then
         configuration.dir_distfiles :=
           extract_string (profile, Field_04, std_distfiles);
      else
         configuration.dir_distfiles :=
           extract_string (profile, Field_04, query_distfiles);
      end if;

      configuration.dir_buildbase :=
        extract_string (profile, Field_05, LS_Buildbase);

      configuration.dir_logs :=
        extract_string (profile, Field_06, LS_Logs);

      configuration.dir_ccache :=
        extract_string (profile, Field_07, no_ccache);

      configuration.num_builders := builders
        (extract_integer (profile, Field_08, def_builders));

      configuration.jobs_limit := builders
        (extract_integer (profile, Field_09, def_jlimit));

      if param_set (profile, Field_10) then
         configuration.tmpfs_workdir :=
           extract_boolean (profile, Field_10, False);
      else
         configuration.tmpfs_workdir :=
           extract_boolean (profile, Field_10, enough_memory);
      end if;

      if param_set (profile, Field_11) then
         configuration.tmpfs_localbase :=
           extract_boolean (profile, Field_11, False);
      else
         configuration.tmpfs_localbase :=
           extract_boolean (profile, Field_11, enough_memory);
      end if;

      if param_set (profile, Field_12) then
         configuration.operating_sys :=
           extract_string (profile, Field_12, std_opsys);
      else
         configuration.operating_sys :=
           extract_string (profile, Field_12, query_opsys);
      end if;

      configuration.dir_options :=
        extract_string (profile, Field_13, std_options);

      if not fields_present then
         declare
            contents : String := generated_section;
         begin
            internal_config.Replace_Section (profile, contents);
         exception
            when Error : others =>
               TIO.Put_Line ("Failed to update [" & profile & "] at "
                 & conf_location);
               return False;
         end;
      end if;

      return True;
   end load_configuration;


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
   function section_exists (profile : String) return Boolean is
   begin
      return param_set (profile, Field_01);
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
        param_set (profile, Field_13);
   end all_params_present;


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
        Field_08 & BDS (configuration.num_builders) &
        Field_09 & BDS (configuration.jobs_limit) &
        Field_10 & TFS (configuration.tmpfs_workdir) &
        Field_11 & TFS (configuration.tmpfs_localbase);
   end generated_section;


   -----------------------
   --  query_distfiles  --
   -----------------------
   function query_distfiles return String is
   begin
      return query_generic ("DISTDIR");
   end query_distfiles;


   -------------------
   --  query_opsys  --
   -------------------
   function query_opsys return String is
   begin
      return query_generic ("OPSYS");
   end query_opsys;


   ---------------------
   --  query_generic  --
   ---------------------
   function query_generic (value : String) return String
   is
      command  : constant String := "make -C " &
        JT.USS (configuration.dir_portsdir) & "/ports-mgmt/pkg -V " & value;
      pipe     : aliased STR.Pipes.Pipe_Stream;
      buffer   : STR.Buffered.Buffered_Stream;
      content  : JT.Text;
      status   : Integer;
      CR_loc   : Integer;
      CR       : constant String (1 .. 1) := (1 => Character'Val (10));
   begin
      pipe.Open (Command => command);
      buffer.Initialize (Output => null,
                         Input  => pipe'Unchecked_Access,
                         Size   => 4096);
      buffer.Read (Into => content);
      pipe.Close;
      status := pipe.Get_Exit_Status;
      if status /= 0 then
         raise make_query with command;
      end if;
      CR_loc := JT.SU.Index (Source => content, Pattern => CR);

      return JT.SU.Slice (Source => content, Low => 1, High => CR_loc - 1);
   end query_generic;


   -----------------------------
   --  query_physical_memory  --
   -----------------------------
   procedure query_physical_memory is
      command : constant String := "/sbin/sysctl hw.physmem";
      pipe    : aliased STR.Pipes.Pipe_Stream;
      buffer  : STR.Buffered.Buffered_Stream;
      content : JT.Text;
      status  : Integer;
      CR_loc  : Integer;
      SP_loc  : Integer;
      CR      : constant String (1 .. 1) := (1 => Character'Val (10));
      SP      : constant String (1 .. 1) := (1 => LAT.Space);
   begin
      if memory_megs > 0 then
         return;
      end if;
      pipe.Open (Command => command);
      buffer.Initialize (Output => null,
                         Input  => pipe'Unchecked_Access,
                         Size   => 4096);
      buffer.Read (Into => content);
      pipe.Close;
      status := pipe.Get_Exit_Status;
      if status /= 0 then
         raise make_query with command;
      end if;
      SP_loc := JT.SU.Index (Source => content, Pattern => SP);
      CR_loc := JT.SU.Index (Source => content, Pattern => CR);

      declare
         type memtype is mod 2**64;
         numbers : String := JT.USS (content)(SP_loc + 1 .. CR_loc - 1);
         bytes   : constant memtype := memtype'Value (numbers);
         megs    : constant memtype := bytes / 1024 / 1024;
      begin
         memory_megs := Natural (megs);
      end;

   end query_physical_memory;

   function enough_memory return Boolean is
      megs_per_slave : Natural;
   begin
      query_physical_memory;
      megs_per_slave := memory_megs / Positive (configuration.num_builders);
      return megs_per_slave >= 1280;
   end enough_memory;

end Parameters;
