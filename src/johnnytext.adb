--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package body JohnnyText is

   package LAT renames Ada.Characters.Latin_1;

   -----------
   --  USS  --
   -----------
   function USS (US : Text) return String is
   begin
         return SU.To_String (US);
   end USS;


   -----------
   --  SUS  --
   -----------
   function SUS (S : String) return Text is
   begin
      return SU.To_Unbounded_String (S);
   end SUS;


   -----------------
   --  IsBlank #1 --
   -----------------
   function IsBlank (US : Text)   return Boolean is
   begin
      return SU.Length (US) = 0;
   end IsBlank;


   -----------------
   --  IsBlank #2 --
   -----------------
   function IsBlank (S  : String) return Boolean is
   begin
      return S'Length = 0;
   end IsBlank;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A, B : Text) return Boolean
   is
      use type Text;
   begin
      return A = B;
   end equivalent;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A : Text; B : String) return Boolean
   is
      AS : constant String := USS (A);
   begin
      return AS = B;
   end equivalent;


   --------------
   --  trim #1 --
   --------------
   function trim (US : Text) return Text is
   begin
      return SU.Trim (US, AS.Both);
   end trim;


   --------------
   --  trim #2 --
   --------------
   function trim (S : String) return String is
   begin
      return AS.Fixed.Trim (S, AS.Both);
   end trim;

   ---------------
   --  int2str  --
   ---------------
   function int2str  (A : Integer) return String
   is
      raw : constant String := A'Img;
      len : constant Natural := raw'Length;
   begin
      if A < 0 then
         return raw;
      else
         return raw (2 .. len);
      end if;
   end int2str;


   ----------------
   --  int2text  --
   ----------------
   function int2text (A : Integer) return Text is
   begin
      return SUS (int2str (A));
   end int2text;


   ----------------
   --  bool2str  --
   ----------------
   function bool2str  (A : Boolean) return String is
   begin
      if A then
         return "true";
      end if;
      return "false";
   end bool2str;


   -----------------
   --  bool2text  --
   -----------------
   function bool2text (A : Boolean) return Text is
   begin
      return SUS (bool2str (A));
   end bool2text;


   ----------------
   --  nextline  --
   ----------------
   procedure nextline (lineblock, firstline : out Text)
   is
      CR_loc : Natural;
      CR : constant String (1 .. 1) := (1 => Character'Val (10));
   begin
      CR_loc := SU.Index (Source => lineblock, Pattern => CR);
      if CR_loc = 0 then
         firstline := lineblock;
         return;
      end if;
      firstline := SUS (SU.Slice
                        (Source => lineblock, Low => 1, High => CR_loc - 1));
      SU.Delete (Source => lineblock, From => 1, Through => CR_loc);
   end nextline;


   --------------------
   --  contains  #1  --
   --------------------
   function contains (S : String; fragment : String) return Boolean is
   begin
      return (AS.Fixed.Index (Source => S, Pattern => fragment) > 0);
   end contains;


   --------------------
   --  contains  #2  --
   --------------------
   function contains (US : Text; fragment : String) return Boolean is
   begin
      return (SU.Index (Source => US, Pattern => fragment) > 0);
   end contains;


   --------------
   --  part_1  --
   --------------
   function part_1 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (S'First .. slash - 1);
   end part_1;


   --------------
   --  part_2  --
   --------------
   function part_2 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (slash + separator'Length .. S'Last);
   end part_2;


   ---------------
   --  replace  --
   ---------------
   function replace (S : String; reject, shiny : Character) return String
   is
      rejectstr : constant String (1 .. 1) := (1 => reject);
      focus     : constant Natural :=
                           AS.Fixed.Index (Source => S, Pattern => rejectstr);
      returnstr : String := S;
   begin
      if focus > 0 then
        returnstr (focus) := shiny;
      end if;
      return returnstr;
   end replace;


   ---------------
   --  zeropad  --
   ---------------
   function zeropad (N : Natural; places : Positive) return String
   is
      template : String (1 .. places) := (others => '0');
      myimage  : constant String := trim (N'Img);
      startpos : constant Natural := 1 + places - myimage'Length;
   begin
      template (startpos .. places) := myimage;
      return template;
   end zeropad;


   ------------------
   --  count_char  --
   ------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   --------------------
   --  replace_char  --
   --------------------
   function replace_char (S : String; focus : Character; substring : String) return String
   is
      num_to_replace : constant Natural := count_char (S, focus);
   begin
      if num_to_replace = 0 then
         return S;
      end if;

      declare
         ssm1   : constant Natural := substring'Length - 1;
         strlen : constant Natural := S'Length + (num_to_replace * ssm1);
         product : String (1 .. strlen);
         ndx : Positive := 1;
      begin
         for x in S'Range loop
            if S (x) = focus then
               product (ndx .. ndx + ssm1) := substring;
               ndx := ndx + substring'Length;
            else
               product (ndx) := S (x);
               ndx := ndx + 1;
            end if;
         end loop;
         return product;
      end;
   end replace_char;


   ---------------------
   --  strip_control  --
   ---------------------
   function strip_control (S : String) return String
   is
      product : String (1 .. S'Length);
      ndx     : Natural := 0;
   begin
      for x in S'Range loop
         if Character'Pos (S (x)) >= 32 then
            ndx := ndx + 1;
            product (ndx) := S (x);
         end if;
      end loop;
      return product (1 .. ndx);
   end strip_control;


   --------------------------------------------------------------------------------------------
   --  specific_field
   --------------------------------------------------------------------------------------------
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String
   is
      back  : Integer;
      dsize : Natural := delimiter'Length;
      front : Integer := S'First;
   begin
      for field in 1 .. field_number - 1 loop
         back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
         if back <= 0 then
            return "";
         end if;
         front := back + dsize;
      end loop;
      back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
      if back > 0 then
         return S (front .. back - 1);
      else
         return S (front .. S'Last);
      end if;
   end specific_field;


   --------------------------------------------------------------------------------------------
   --  leads #1
   --------------------------------------------------------------------------------------------
   function leads (S : String; fragment : String) return Boolean is
   begin
      if fragment'Length > S'Length then
         return False;
      end if;
      return (S (S'First .. S'First + fragment'Length - 1) = fragment);
   end leads;


   --------------------------------------------------------------------------------------------
   --  leads #2
   --------------------------------------------------------------------------------------------
   function leads (US : Text; fragment : String) return Boolean is
   begin
      return leads (USS (US), fragment);
   end leads;


   --------------------------------------------------------------------------------------------
   --  initialize_markers
   --------------------------------------------------------------------------------------------
   procedure initialize_markers
     (block_text : in String;
      shuttle    : out Line_Markers) is
   begin
      shuttle.back_marker  := block_text'First;
      shuttle.front_marker := block_text'First;
      if block_text'Length > 0 then
         shuttle.zero_length  := block_text (shuttle.back_marker) = LAT.LF;
      end if;
      shuttle.utilized := False;
   end initialize_markers;


   --------------------------------------------------------------------------------------------
   --  extract_line
   --------------------------------------------------------------------------------------------
   function extract_line
     (block_text : in String;
      shuttle    : in Line_Markers)
      return String is
   begin
      if shuttle.zero_length or else
        shuttle.back_marker < block_text'First or else
        shuttle.front_marker < shuttle.back_marker or else
        shuttle.front_marker > block_text'Last
      then
         return "";
      end if;
      return block_text (shuttle.back_marker .. shuttle.front_marker);
   end extract_line;


   --------------------------------------------------------------------------------------------
   --  next_line_present
   --------------------------------------------------------------------------------------------
   function next_line_present
     (block_text : in String;
      shuttle    : in out Line_Markers)
      return Boolean is
   begin
      if shuttle.front_marker + 2 > block_text'Last then
         return False;
      end if;
      if shuttle.utilized then
         if shuttle.zero_length then
            shuttle.back_marker  := shuttle.front_marker + 1;
         else
            shuttle.back_marker  := shuttle.front_marker + 2;
         end if;
         shuttle.front_marker := shuttle.back_marker;
         shuttle.zero_length  := block_text (shuttle.back_marker) = LAT.LF;
      else
         if block_text'Length = 0 then
            return False;
         end if;
      end if;
      loop
         shuttle.utilized := True;
         exit when shuttle.front_marker = block_text'Last;
         exit when block_text (shuttle.back_marker) = LAT.LF;
         exit when block_text (shuttle.front_marker + 1) = LAT.LF;
         shuttle.front_marker := shuttle.front_marker + 1;
      end loop;
      return True;
   end next_line_present;


    --------------------------------------------------------------------------------------------
   --  next_line_with_content_present
   --------------------------------------------------------------------------------------------
   function next_line_with_content_present
     (block_text : in String;
      start_with : in String;
      shuttle    : in out Line_Markers) return Boolean
   is
      ndx : Natural;
   begin
      if shuttle.front_marker + 2 > block_text'Last then
         return False;
      end if;
      if shuttle.utilized then
         ndx := AS.Fixed.Index (Source  => block_text,
                                Pattern => LAT.LF & start_with,
                                From    => shuttle.front_marker + 1);
         if ndx = 0 then
            return False;
         else
            shuttle.back_marker := ndx + 1;
         end if;
      else
         if start_with'Length = 0 then
            return False;
         end if;
         if leads (block_text, start_with) then
            shuttle.back_marker := block_text'First;
         else
            ndx := AS.Fixed.Index (block_text, LAT.LF & start_with);
            if ndx = 0 then
               return False;
            else
               shuttle.back_marker := ndx + 1;
            end if;
         end if;
      end if;
      shuttle.utilized    := True;
      shuttle.zero_length := False;
      ndx := AS.Fixed.Index (Source  => block_text,
                             Pattern => single_LF,
                             From    => shuttle.back_marker + 1);
      if ndx = 0 then
         shuttle.front_marker := block_text'Last;
      else
         shuttle.front_marker := ndx - 1;
      end if;
      return True;

   end next_line_with_content_present;

end JohnnyText;
