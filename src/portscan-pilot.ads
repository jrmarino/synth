--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Pilot is

   --  Called when command line argument counts is known to be 2 or more.
   --  Argument 2 should either be a path to a file or a port origin.
   --  If it's a file, there can be no more arguments (by definition; there's
   --  no technical issue).  If there are more than 2 arguments, arguments 2+
   --  must all be port origins.  Returns "True" when all the port origins
   --  (command line or inside file) are verified and arguments are correct.
   function store_origins return Boolean;

private

   portlist : portkey_crate.Map;

   badport : constant String := "Invalid port origin: ";

   --  scan given file.  Everything line must be either blank (whitespace
   --  ignored) or a valid port origin, and returns true if it is.
   --  Internally, the ports are stacked.
   function valid_file (path : String) return Boolean;

   --  return true if "cat" exists and "port" exists
   function valid_catport (catport : String) return Boolean;

end PortScan.Pilot;
