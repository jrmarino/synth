--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  GCC 6.0 only (skip Container_Checks until identified need arises)
pragma Suppress (Tampering_Check);

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;

with JohnnyText;
with Parameters;
with Definitions;  use Definitions;

private with Replicant.Platform;

package PortScan is

   package JT  renames JohnnyText;
   package AC  renames Ada.Containers;
   package CAL renames Ada.Calendar;
   package AD  renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;
   package PM  renames Parameters;

   type count_type is (total, success, failure, ignored, skipped);
   type dim_handlers is array (count_type) of TIO.File_Type;

   type port_id   is private;
   port_match_failed : constant port_id;

   --  Scan the entire ports tree in order with a single, non-recursive pass
   --  Return True on success
   function scan_entire_ports_tree (portsdir : String) return Boolean;

   --  Starting with a single port, recurse to determine a limited but complete
   --  dependency tree.  Repeated calls will augment already existing data.
   --  Return True on success
   function scan_single_port (catport : String; always_build : Boolean;
                              fatal : out Boolean)
                              return Boolean;

   --  This procedure causes the reverse dependencies to be calculated, and
   --  then the extended (recursive) reverse dependencies.  The former is
   --  used progressively to determine when a port is free to build and the
   --  latter sets the build priority.
   procedure set_build_priority;

   --  Wipe out all scan data so new scan can be performed
   procedure reset_ports_tree;

   --  Returns the number of cores.  The set_cores procedure must be run first.
   --  set_cores was private previously, but we need the information to set
   --  intelligent defaults for the configuration file.
   procedure set_cores;
   function cores_available return cpu_range;

   --  Return " (port deleted)" if the catport doesn't exist
   --  Return " (directory empty)" if the directory exists but has no contents
   --  Return " (Makefile missing)" when makefile is missing
   --  otherwise return blank string
   function obvious_problem (portsdir, catport : String) return String;

   --  Attempts to generate a ports index file after acquiring all port origins
   --  Returns False (with an outputted message) if it fails to:
   --    a. create directories
   --    b. scan fails
   --    c. index file creation fails
   function generate_ports_index (index_file, portsdir : String) return Boolean;

   --  Recursively scans a ports directory tree, returning True as soon as a file or directory
   --  newer than the given time is found.
   function tree_newer_than_reference
     (portsdir  : String;
      reference : CAL.Time;
      valid     : out Boolean) return Boolean;

   --  Store origin support
   --  * store data from flavor index in hash and vector
   --  * clear data when complete
   --  * valid origin returns true when candidate available verbatim
   procedure load_index_for_store_origins;
   procedure clear_store_origin_data;
   function input_origin_valid (candidate : String) return Boolean;
   procedure suggest_flavor_for_bad_origin (candidate : String);

private

   package REP  renames Replicant;
   package PLAT renames Replicant.Platform;

   max_ports  : constant := 40000;
   scan_slave : constant builders := 9;
   ss_base    : constant String := "/SL09";
   dir_ports  : constant String := "/xports";
   chroot     : constant String := "/usr/sbin/chroot ";
   index_path : constant String := "/var/cache/synth";

   type port_id   is range -1 .. max_ports - 1;
   subtype port_index is port_id range 0 .. port_id'Last;

   port_match_failed : constant port_id := port_id'First;

   --  skip "package" because every port has same dependency on ports-mgmt/pkg
   --  except for pkg itself.  Skip "test" because these dependencies are
   --  not required to build packages.
   type dependency_type is (fetch, extract, patch, build, library, runtime);
   subtype LR_set is dependency_type range library .. runtime;

   bmake_execution  : exception;
   pkgng_execution  : exception;
   make_garbage     : exception;
   nonexistent_port : exception;
   circular_logic   : exception;
   seek_failure     : exception;
   unknown_format   : exception;

   package subqueue is new AC.Vectors
     (Element_Type => port_index,
      Index_Type   => port_index);

   package string_crate is new AC.Vectors
     (Element_Type => JT.Text,
      Index_Type   => port_index,
      "="          => JT.SU."=");

   type queue_record is
      record
         ap_index      : port_index;
         reverse_score : port_index;
      end record;

   --  Functions for ranking_crate definitions
   function "<" (L, R : queue_record) return Boolean;

   package ranking_crate is new AC.Ordered_Sets  (Element_Type => queue_record);

   --  Functions for portkey_crate and package_crate definitions
   function port_hash (key : JT.Text) return AC.Hash_Type;

   package portkey_crate is new AC.Hashed_Maps
     (Key_Type        => JT.Text,
      Element_Type    => port_index,
      Hash            => port_hash,
      Equivalent_Keys => JT.equivalent);

   package package_crate is new AC.Hashed_Maps
     (Key_Type        => JT.Text,
      Element_Type    => Boolean,
      Hash            => port_hash,
      Equivalent_Keys => JT.equivalent);

   --  Functions for block_crate definitions
   function block_hash (key : port_index) return AC.Hash_Type;
   function block_ekey (left, right : port_index) return Boolean;

   package block_crate is new AC.Hashed_Maps
     (Key_Type        => port_index,
      Element_Type    => port_index,
      Hash            => block_hash,
      Equivalent_Keys => block_ekey);

   type port_record is
      record
         sequence_id   : port_index           := 0;
         key_cursor    : portkey_crate.Cursor := portkey_crate.No_Element;
         jobs          : builders             := 1;
         ignore_reason : JT.Text              := JT.blank;
         port_version  : JT.Text              := JT.blank;
         package_name  : JT.Text              := JT.blank;
         pkg_dep_query : JT.Text              := JT.blank;
         ignored       : Boolean              := False;
         scanned       : Boolean              := False;
         rev_scanned   : Boolean              := False;
         unlist_failed : Boolean              := False;
         work_locked   : Boolean              := False;
         scan_locked   : Boolean              := False;
         pkg_present   : Boolean              := False;
         remote_pkg    : Boolean              := False;
         never_remote  : Boolean              := False;
         deletion_due  : Boolean              := False;
         use_procfs    : Boolean              := False;
         use_linprocfs : Boolean              := False;
         reverse_score : port_index           := 0;
         min_librun    : Natural              := 0;
         librun        : block_crate.Map;
         blocked_by    : block_crate.Map;
         blocks        : block_crate.Map;
         all_reverse   : block_crate.Map;
         options       : package_crate.Map;
         flavors       : string_crate.Vector;
      end record;
   type port_record_access is access all port_record;

   type dim_make_queue is array (scanners) of subqueue.Vector;
   type dim_progress   is array (scanners) of port_index;
   type dim_all_ports  is array (port_index) of aliased port_record;

   all_ports    : dim_all_ports;
   ports_keys   : portkey_crate.Map;
   portlist     : portkey_crate.Map;
   make_queue   : dim_make_queue;
   mq_progress  : dim_progress := (others => 0);
   rank_queue   : ranking_crate.Set;
   number_cores : cpu_range  := cpu_range'First;
   lot_number   : scanners   := 1;
   lot_counter  : port_index := 0;
   last_port    : port_index := 0;
   prescanned   : Boolean    := False;

   so_porthash  : portkey_crate.Map;
   so_serial    : string_crate.Vector;

   procedure iterate_reverse_deps;
   procedure iterate_drill_down;
   procedure populate_set_depends (target  : port_index;
                                   catport : String;
                                   line    : JT.Text;
                                   dtype   : dependency_type);
   procedure populate_set_options (target  : port_index;
                                   line    : JT.Text;
                                   on      : Boolean);
   procedure populate_flavors     (target  : port_index;
                                   line    : JT.Text);
   procedure populate_port_data   (target : port_index);
   procedure populate_port_data_fpc (target : port_index);
   procedure populate_port_data_nps (target : port_index);
   procedure drill_down (next_target     : port_index;
                         original_target : port_index);

   --  subroutines for populate_port_data
   procedure prescan_ports_tree (portsdir : String);
   procedure grep_Makefile (portsdir, category : String);
   procedure walk_all_subdirectories (portsdir, category : String);
   procedure wipe_make_queue;
   procedure read_flavor_index;
   procedure parallel_deep_scan (success : out Boolean;
                                 show_progress : Boolean);

   --  some helper routines
   function find_colon (Source : String) return Natural;
   function scrub_phase (Source : String) return JT.Text;
   function get_catport (PR : port_record) return String;
   function scan_progress return String;
   function get_max_lots return scanners;
   function get_pkg_name (origin : String) return String;
   function timestamp (hack : CAL.Time; www_format : Boolean := False) return String;
   function clean_up_pkgsrc_ignore_reason (dirty_string : String) return JT.Text;
   function subdirectory_is_older (portsdir, category : String;
                                   reference : CAL.Time) return Boolean;

   type dim_counters is array (count_type) of Natural;

   --  bulk run variables
   Flog        : dim_handlers;
   start_time  : CAL.Time;
   stop_time   : CAL.Time;
   scan_start  : CAL.Time;
   scan_stop   : CAL.Time;
   bld_counter : dim_counters := (0, 0, 0, 0, 0);

end PortScan;
