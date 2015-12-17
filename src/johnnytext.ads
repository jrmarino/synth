--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;
with Ada.Strings;

package JohnnyText is

   package AS  renames Ada.Strings;
   package SU  renames Ada.Strings.Unbounded;
   subtype Text is SU.Unbounded_String;
   blank : constant Text := SU.Null_Unbounded_String;

   --  converters : Text <==> String
   function USS (US : Text)   return String;
   function SUS (S  : String) return Text;

   --  True if the string is zero length
   function IsBlank (US : Text)   return Boolean;
   function IsBlank (S  : String) return Boolean;

   --  True if strings are identical
   function equivalent (A, B : Text) return Boolean;

   --  Trim both sides
   function trim (US : Text) return Text;

end JohnnyText;
