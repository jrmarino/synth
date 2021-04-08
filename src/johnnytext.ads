--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Strings.Maps;

package JohnnyText is

   package AS renames Ada.Strings;
   package SU renames Ada.Strings.Unbounded;
   package SM renames Ada.Strings.Maps;
   subtype Text is SU.Unbounded_String;
   type Line_Markers is private;

   blank : constant Text := SU.Null_Unbounded_String;

   --  converters : Text <==> String
   function USS (US : Text)   return String;
   function SUS (S  : String) return Text;

   --  True if the string is zero length
   function IsBlank (US : Text)   return Boolean;
   function IsBlank (S  : String) return Boolean;

   --  True if strings are identical
   function equivalent (A, B : Text) return Boolean;
   function equivalent (A : Text; B : String) return Boolean;

   --  Trim both sides
   function trim    (US : Text)  return Text;
   function trim    (S : String) return String;
   function trimtab (US : Text)  return Text;
   function trimtab (S : String) return String;

   --  unpadded numeric image
   function int2str  (A : Integer) return String;
   function int2text (A : Integer) return Text;

   --  convert boolean to lowercase string
   function bool2str  (A : Boolean) return String;
   function bool2text (A : Boolean) return Text;

   --  Return first line of block of lines (line is removed from block)
   procedure nextline (lineblock, firstline : out Text);

   --  shorthand for index
   function contains (S : String; fragment : String) return Boolean;
   function contains (US : Text; fragment : String) return Boolean;

   --  Return half of a string split by separator
   function part_1 (S : String; separator : String := "/") return String;
   function part_2 (S : String; separator : String := "/") return String;

   --  Replace a single character with another single character (first found)
   function replace (S : String; reject, shiny : Character) return String;

   --  Numeric image with left-padded zeros
   function zeropad (N : Natural; places : Positive) return String;

    --  Returns number of instances of a given character in a given string
   function count_char (S : String; focus : Character) return Natural;

   --  Search entire string S for focus character and replace all instances with substring
   function replace_char (S : String; focus : Character; substring : String) return String;

   --  Filters out control characters from String S
   function strip_control (S : String) return String;

   --  Given a single line (presumably no line feeds) with data separated by <delimited>,
   --  return the field given by field_number (starts counting at 1).
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String;

   --  Return True if S leads with fragment exactly
   function leads (S  : String; fragment : String) return Boolean;
   function leads (US : Text;   fragment : String) return Boolean;

    --  Iterate though block of text, LF is delimiter
   procedure initialize_markers
     (block_text : in String;
      shuttle    : out Line_Markers);

   function next_line_present
     (block_text : in String;
      shuttle    : in out Line_Markers)
      return Boolean;

   function next_line_with_content_present
     (block_text : in String;
      start_with : in String;
      shuttle    : in out Line_Markers)
      return Boolean;

   function extract_line
     (block_text : in String;
      shuttle    : in Line_Markers)
      return String;

   --  Head (keep all but last delimiter and field)
   function head (US : Text;   delimiter : Text)   return Text;
   function head (S  : String; delimiter : String) return String;

   --  Tail (keep only last field)
   function tail (US : Text;   delimiter : Text)   return Text;
   function tail (S  : String; delimiter : String) return String;

   --  Replaces 2 or more consecutive spaces with a single space
   function strip_excessive_spaces (S : String) return String;
private

   single_LF : constant String (1 .. 1) := (1 => ASCII.LF);
   space_and_HT : constant SM.Character_Set := SM.To_Set (" " & ASCII.HT);

   type Line_Markers is
      record
         back_marker  : Natural := 0;
         front_marker : Natural := 0;
         zero_length  : Boolean := False;
         utilized     : Boolean := False;
      end record;

end JohnnyText;
