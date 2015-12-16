--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   type cpu_range is range 1 .. 32;
   type builders  is range 1 .. cpu_range'Last * 2;

end Definitions;
