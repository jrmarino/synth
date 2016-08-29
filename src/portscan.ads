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

with JohnnyText;
with Parameters;
with Definitions;  use Definitions;

package PortScan is

   package JT  renames JohnnyText;
   package AC  renames Ada.Containers;
   package CAL renames Ada.Calendar;
   package AD  renames Ada.Directories;
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

private

   max_ports  : constant := 28000;
   scan_slave : constant builders := 9;
   ss_base    : constant String := "/SL09";
   watchdog_active : constant Boolean := True;

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
         use_watchdog  : Boolean              := watchdog_active;
         reverse_score : port_index           := 0;
         librun        : block_crate.Map;
         blocked_by    : block_crate.Map;
         blocks        : block_crate.Map;
         all_reverse   : block_crate.Map;
         options       : package_crate.Map;
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

   procedure iterate_reverse_deps;
   procedure iterate_drill_down;
   procedure populate_port_data (target : port_index);
   procedure drill_down (next_target     : port_index;
                         original_target : port_index);

   --  subroutines for populate_port_data
   procedure prescan_ports_tree (portsdir : String);
   procedure grep_Makefile (portsdir, category : String);
   procedure walk_all_subdirectories (portsdir, category : String);
   procedure wipe_make_queue;
   procedure parallel_deep_scan (success : out Boolean;
                                 show_progress : Boolean);

   --  some helper routines
   function find_colon (Source : String) return Natural;
   function scrub_phase (Source : String) return JT.Text;
   function get_catport (PR : port_record) return String;
   function scan_progress return String;
   function get_max_lots return scanners;
   function get_pkg_name (origin : String) return String;

   type dim_counters is array (count_type) of Natural;

   --  bulk run variables
   Flog        : dim_handlers;
   start_time  : CAL.Time;
   stop_time   : CAL.Time;
   bld_counter : dim_counters := (0, 0, 0, 0, 0);

end PortScan;
