--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  pragma Suppress (All_Checks);
--  too new: Container_Checks, Tampering_Check

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

package PortScan is

   package SU  renames Ada.Strings.Unbounded;
   package AC  renames Ada.Containers;
   package TIO renames Ada.Text_IO;

   type port_id   is private;
   type cpu_range is range 1 .. 32;
   type builders  is range 1 .. cpu_range'Last * 2;
   port_match_failed : constant port_id;

   --  Scan the entire ports tree in order with a single, non-recursive pass
   --  Return True on success
   function scan_entire_ports_tree (portsdir : String) return Boolean;

   --  Starting with a single port, recurse to determine a limited but complete
   --  dependency tree.  Repeated calls will augment already existing data.
   --  Return True on success
   function scan_single_port (repository, portsdir, catport : String;
                              always_build : Boolean := False) return Boolean;

   --  This procedure causes the reverse dependencies to be calculated, and
   --  then the extended (recursive) reverse dependencies.  The former is
   --  used progressively to determine when a port is free to build and the
   --  latter sets the build priority.
   procedure set_build_priority;

   --  Wipe out all scan data so new scan can be performed
   procedure reset_ports_tree;

private

   max_ports        : constant := 28000;
   jobs_per_cpu     : constant := 1;

   type jobs_type is range 1 .. cpu_range'Last * jobs_per_cpu;
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
     (Element_Type => SU.Unbounded_String,
      Index_Type   => port_index,
      "="          => SU."=");

   type queue_record is
      record
         ap_index      : port_index;
         reverse_score : port_index;
      end record;

   --  Functions for ranking_crate definitions
   function "<" (L, R : queue_record) return Boolean;

   package ranking_crate is new AC.Ordered_Sets  (Element_Type => queue_record);

   --  Functions for portkey_crate and package_crate definitions
   function port_hash (key : SU.Unbounded_String) return AC.Hash_Type;
   function port_ekey (left, right : SU.Unbounded_String) return Boolean;

   package portkey_crate is new AC.Hashed_Maps
     (Key_Type        => SU.Unbounded_String,
      Element_Type    => port_index,
      Hash            => port_hash,
      Equivalent_Keys => port_ekey);

   package package_crate is new AC.Hashed_Maps
     (Key_Type        => SU.Unbounded_String,
      Element_Type    => Boolean,
      Hash            => port_hash,
      Equivalent_Keys => port_ekey);

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
         jobs          : jobs_type            := 1;
         ignore_reason : SU.Unbounded_String  := SU.Null_Unbounded_String;
         port_version  : SU.Unbounded_String  := SU.Null_Unbounded_String;
         package_name  : SU.Unbounded_String  := SU.Null_Unbounded_String;
         ignored       : Boolean              := False;
         scanned       : Boolean              := False;
         rev_scanned   : Boolean              := False;
         unlist_failed : Boolean              := False;
         work_locked   : Boolean              := False;
         reverse_score : port_index           := 0;
         librun        : block_crate.Map;
         blocked_by    : block_crate.Map;
         blocks        : block_crate.Map;
         all_reverse   : block_crate.Map;
         options       : package_crate.Map;
      end record;
   type port_record_access is access all port_record;

   type dim_make_queue is array (jobs_type) of subqueue.Vector;
   type dim_all_ports  is array (port_index) of aliased port_record;

   all_ports    : dim_all_ports;
   ports_keys   : portkey_crate.Map;
   make_queue   : dim_make_queue;
   rank_queue   : ranking_crate.Set;
   number_cores : cpu_range;
   lot_number   : jobs_type  := 1;
   lot_counter  : port_index := 0;
   last_port    : port_index := 0;
   prescanned   : Boolean    := False;


   procedure iterate_reverse_deps;
   procedure iterate_drill_down;
   procedure populate_port_data (portsdir : String;
                                 target   : port_index);
   procedure drill_down (next_target     : port_index;
                         original_target : port_index);

   --  subroutines for populate_port_data
   procedure prescan_ports_tree (portsdir : String);
   procedure grep_Makefile (portsdir, category : String);
   procedure walk_all_subdirectories (portsdir, category : String);
   procedure parallel_deep_scan (portsdir : String; success : out Boolean);
   procedure wipe_make_queue;

   --  some helper routines
   procedure set_cores;
   procedure nextline (lineblock, firstline : out SU.Unbounded_String);
   function find_colon (Source : String) return Natural;
   function scrub_phase (Source : String) return SU.Unbounded_String;
   function get_catport (PR : port_record) return String;

end PortScan;
