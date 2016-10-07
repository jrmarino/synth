--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with JohnnyText;
with Signals;

package body Display is

   package  JT renames JohnnyText;
   package SIG renames Signals;

   ----------------------
   --  launch_monitor  --
   ----------------------
   function launch_monitor (num_builders : builders) return Boolean is
   begin
      if not Start_Curses_Mode then
         return False;
      end if;
      if not TIC.Has_Colors and then not establish_colors then
         Return_To_Text_Mode;
         return False;
      end if;
      begin
         TIC.Set_Echo_Mode (False);
         TIC.Set_Raw_Mode (True);
         TIC.Set_Cbreak_Mode (True);
         TIC.Set_Cursor_Visibility (Visibility => cursor_vis);
      exception
         when TIC.Curses_Exception =>
            Return_To_Text_Mode;
            return False;
      end;

      builders_used := Integer (num_builders);

      if not launch_summary_zone or else
        not launch_builders_zone or else
        not launch_actions_zone
      then
         terminate_monitor;
         return False;
      end if;

      draw_static_summary_zone;
      draw_static_builders_zone;
      Refresh_Zone (summary);
      Refresh_Zone (builder);
      return True;
   end launch_monitor;


   -------------------------
   --  terminate_monitor  --
   -------------------------
   procedure terminate_monitor
   is
      ok : Boolean := True;
   begin
      --  zone_window can't be used because Delete will modify Win variable
      begin
         TIC.Delete (Win => zone_summary);
         TIC.Delete (Win => zone_builders);
         TIC.Delete (Win => zone_actions);
      exception
         when TIC.Curses_Exception => ok := False;
      end;
      if ok then
         Return_To_Text_Mode;
      end if;
   end terminate_monitor;


   -----------------------------------
   --  set_full_redraw_next_update  --
   -----------------------------------
   procedure set_full_redraw_next_update is
   begin
      for zone in zones'Range loop
         begin
            TIC.Redraw (Win => zone_window (zone));
         exception
            when TIC.Curses_Exception => null;
         end;
      end loop;
   end set_full_redraw_next_update;


   ---------------------------
   --  launch_summary_zone  --
   ---------------------------
   function launch_summary_zone return Boolean is
   begin
      zone_summary := TIC.Create (Number_Of_Lines       => 2,
                                  Number_Of_Columns     => app_width,
                                  First_Line_Position   => 0,
                                  First_Column_Position => 0);
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end launch_summary_zone;


   --------------------------------
   --  draw_static_summary_zone  --
   --------------------------------
   procedure draw_static_summary_zone
   is
      line1 : constant appline :=
        " Total         Built        Ignored        Load  0.00  Pkg/hour" &
        "                ";
      line2 : constant appline :=
        "  Left        Failed        skipped        swap  0.0%   Impulse" &
        "       00:00:00 ";
   begin
      Choose_Pen (summary, bright, c_sumlabel);
      Relocate   (summary, 0);
      Scrawl     (summary, line1);
      Relocate   (summary, 1);
      Scrawl     (summary, line2);
   end draw_static_summary_zone;


   ----------------------------
   --  launch_builders_zone  --
   ----------------------------
   function launch_builders_zone return Boolean
   is
      hghtint : constant Integer := 4 + builders_used;
      height  : constant TIC.Line_Position := TIC.Line_Position (hghtint);
   begin
      zone_builders := TIC.Create (Number_Of_Lines       => height,
                                   Number_Of_Columns     => app_width,
                                   First_Line_Position   => 2,
                                   First_Column_Position => 0);
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end launch_builders_zone;


   ---------------------------------
   --  draw_static_builders_zone  --
   ---------------------------------
   procedure draw_static_builders_zone
   is
      hghtint : constant Integer := 4 + builders_used;
      height  : constant TIC.Line_Position := TIC.Line_Position (hghtint);
      lastrow : constant TIC.Line_Position := inc (height, -1);
      dashes  : constant appline := (others => '=');
      headtxt : constant appline := " ID  Elapsed   Build Phase      " &
        "Origin                                   Lines ";
   begin
      Choose_Pen (builder, bright, c_dashes);
      Relocate   (builder, 0);
      Scrawl     (builder, dashes);
      Relocate   (builder, 2);
      Scrawl     (builder, dashes);
      Relocate   (builder, lastrow);
      Scrawl     (builder, dashes);
      Relocate   (builder, 1);

      if SIG.graceful_shutdown_requested then
         Choose_Pen (builder, bright, c_advisory);
         Scrawl     (builder, shutdown_msg);
      else
         Choose_Pen (builder, normal, c_tableheader);
         Scrawl     (builder, headtxt);
      end if;

      for z in 3 .. inc (lastrow, -1) loop
         Relocate (builder, z);
         Scrawl   (builder, blank);
      end loop;
   end draw_static_builders_zone;


   ---------------------------
   --  launch_actions_zone  --
   ---------------------------
   function launch_actions_zone return Boolean
   is
      consumed   : constant Integer := builders_used + 4 + 2;
      viewpos    : constant TIC.Line_Position := TIC.Line_Position (consumed);
      difference : Integer := 0 - consumed;
   begin
      --  Guarantee at least 10 lines for history and let ncurses handle any overflow
      if difference < 10 then
         difference := 10;
      end if;
      historyheight := inc (TIC.Lines, difference);
      zone_actions := TIC.Create (Number_Of_Lines       => historyheight,
                                  Number_Of_Columns     => app_width,
                                  First_Line_Position   => viewpos,
                                  First_Column_Position => 0);
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end launch_actions_zone;


   -----------
   --  inc  --
   -----------
   function inc (X : TIC.Line_Position; by : Integer) return TIC.Line_Position
   is
      use type TIC.Line_Position;
   begin
      return X + TIC.Line_Position (by);
   end inc;


   -----------------
   --  summarize  --
   -----------------
   procedure summarize (data : summary_rec)
   is
      subtype fivelong is String (1 .. 5);
      function pad (S : String; amount : Positive := 5) return String;
      function fmtpc (f : Float; percent : Boolean) return fivelong;
      procedure colorado (S : String; color :  TIC.Color_Pair;
                          col : TIC.Column_Position;
                          row : TIC.Line_Position;
                          dim : Boolean := False);

      remaining : constant Integer := data.Initially - data.Built -
        data.Failed - data.Ignored - data.Skipped;

      function pad (S : String; amount : Positive := 5) return String
      is
         result : String (1 .. amount) := (others => ' ');
         slen   : constant Natural := S'Length;
      begin
         if slen <= amount then
            result (1 .. slen) := S;
         else
            result := S (S'First .. S'First + amount - 1);
         end if;
         return result;
      end pad;
      function fmtpc (f : Float; percent : Boolean) return fivelong
      is
         type loadtype is delta 0.01 digits 4;
         result : fivelong := (others => ' ');
         raw1   : constant loadtype := loadtype (f);
         raw2   : constant String := raw1'Img;
         raw3   : constant String := raw2 (2 .. raw2'Last);
         rlen   : constant Natural := raw3'Length;
         start  : constant Natural := 6 - rlen;
      begin
         result (start .. 5) := raw3;
         if percent then
            result (5) := '%';
         end if;
         return result;
      end fmtpc;
      procedure colorado (S : String; color :  TIC.Color_Pair;
                          col : TIC.Column_Position;
                          row : TIC.Line_Position;
                          dim : Boolean := False)
      is
         attribute : TIC.Character_Attribute_Set := bright;
      begin
         if dim then
            attribute := normal;
         end if;
         Choose_Pen (summary, attribute, color);
         Relocate   (summary, row, col);
         Scrawl     (summary, S);
      end colorado;

      L1F1 : constant String := pad (JT.int2str (data.Initially));
      L1F2 : constant String := pad (JT.int2str (data.Built));
      L1F3 : constant String := pad (JT.int2str (data.Ignored));
      L1F4 : fivelong;
      L1F5 : constant String := pad (JT.int2str (data.pkg_hour), 4);

      L2F1 : constant String := pad (JT.int2str (remaining));
      L2F2 : constant String := pad (JT.int2str (data.Failed));
      L2F3 : constant String := pad (JT.int2str (data.Skipped));
      L2F4 : fivelong;
      L2F5 : constant String := pad (JT.int2str (data.impulse), 4);

   begin
      if data.swap = 100.0 then
         L2F4 := " 100%";
      elsif data.swap > 100.0 then
         L2F4 := "  n/a";
      else
         L2F4 := fmtpc (data.swap, True);
      end if;
      if data.load >= 100.0 then
         L1F4 := pad (JT.int2str (Integer (data.load)));
      else
         L1F4 := fmtpc (data.load, False);
      end if;

      colorado (L1F1, c_standard,  7, 0);
      colorado (L1F2, c_success,  21, 0);
      colorado (L1F3, c_ignored,  36, 0);
      colorado (L1F4, c_standard, 48, 0, True);
      colorado (L1F5, c_standard, 64, 0, True);

      colorado (L2F1, c_standard,  7, 1);
      colorado (L2F2, c_failure,  21, 1);
      colorado (L2F3, c_skipped,  36, 1);
      colorado (L2F4, c_standard, 48, 1, True);
      colorado (L2F5, c_standard, 64, 1, True);
      colorado (data.elapsed, c_elapsed, 70, 1);

      Refresh_Zone (summary);
   end summarize;


   -----------------------
   --  update_builder   --
   -----------------------
   procedure update_builder (BR : builder_rec)
   is
      procedure colorado (S : String; color :  TIC.Color_Pair;
                          col : TIC.Column_Position;
                          row : TIC.Line_Position;
                          dim : Boolean := False);
      procedure print_id;
      row : TIC.Line_Position := inc (TIC.Line_Position (BR.id), 2);

      procedure print_id is
      begin
         Choose_Pen (builder, c_slave (BR.id).attribute, c_slave (BR.id).palette);
         Relocate   (builder, row, 1);
         Scrawl     (builder, BR.slavid);
      end print_id;
      procedure colorado (S : String; color :  TIC.Color_Pair;
                          col : TIC.Column_Position;
                          row : TIC.Line_Position;
                          dim : Boolean := False)
      is
         attribute : TIC.Character_Attribute_Set := bright;
      begin
         if dim then
            attribute := normal;
         end if;
         Choose_Pen (builder, attribute, color);
         Relocate   (builder, row, col);
         Scrawl     (builder, S);
      end colorado;
   begin
      if SIG.graceful_shutdown_requested then
         Choose_Pen (builder, bright, c_advisory);
         Relocate   (builder, 1);
         Scrawl     (builder, shutdown_msg);
      end if;
      print_id;
      colorado (BR.Elapsed, c_standard,  5, row, True);
      colorado (BR.phase,   c_bldphase, 15, row, True);
      colorado (BR.origin,  c_origin,   32, row, False);
      colorado (BR.LLines,  c_standard, 71, row, True);
   end update_builder;


   ------------------------------
   --  refresh_builder_window  --
   ------------------------------
   procedure refresh_builder_window is
   begin
      Refresh_Zone (builder);
   end refresh_builder_window;


   ----------------------
   --  insert_history  --
   ----------------------
   procedure insert_history (HR : history_rec) is
   begin
      if history_arrow = cyclic_range'Last then
         history_arrow := cyclic_range'First;
      else
         history_arrow := history_arrow + 1;
      end if;
      history (history_arrow) := HR;
   end insert_history;


   ------------------------------
   --  refresh_history_window  --
   ------------------------------
   procedure refresh_history_window
   is
      procedure clear_row (row : TIC.Line_Position);
      procedure colorado (S : String; color :  TIC.Color_Pair;
                          col : TIC.Column_Position;
                          row : TIC.Line_Position;
                          dim : Boolean := False);
      function col_action (status : String) return TIC.Color_Pair;
      procedure print_id (id : builders; sid : String; row : TIC.Line_Position;
                          status : String);
      procedure clear_row (row : TIC.Line_Position) is
      begin
         Choose_Pen (action, TIC.Normal_Video, c_standard);
         Relocate   (action, row);
         Scrawl     (action, blank);
      end clear_row;
      procedure colorado (S : String; color :  TIC.Color_Pair;
                          col : TIC.Column_Position;
                          row : TIC.Line_Position;
                          dim : Boolean := False)
      is
         attribute : TIC.Character_Attribute_Set := bright;
      begin
         if dim then
            attribute := normal;
         end if;
         Choose_Pen (action, attribute, color);
         Relocate   (action, row, col);
         Scrawl     (action, S);
      end colorado;
      function col_action (status : String) return TIC.Color_Pair is
      begin
         if status = "shutdown" then
            return c_shutdown;
         elsif status = "success " then
            return c_success;
         elsif status = "failure " then
            return c_failure;
         elsif status = "skipped " then
            return c_skipped;
         elsif status = "ignored " then
            return c_ignored;
         else
            return c_standard;
         end if;
      end col_action;
      procedure print_id (id : builders; sid : String; row : TIC.Line_Position;
                          status : String)
      is
      begin
         Choose_Pen (action, normal, c_standard);
         Relocate   (action, row, 10);
         Scrawl     (action, "[--]");
         if status /= "skipped " and then status /= "ignored "
         then
            Choose_Pen (action, c_slave (id).attribute, c_slave (id).palette);
            Relocate   (action, row, 11);
            Scrawl     (action, sid);
         end if;
      end print_id;

      arrow  : cyclic_range := history_arrow;
      maxrow : TIC.Line_Position;
      use type TIC.Line_Position;
   begin
      if historyheight >= TIC.Line_Position (cyclic_range'Last) then
         maxrow := TIC.Line_Position (cyclic_range'Last) - 1;
      else
         maxrow := historyheight - 1;
      end if;
      for row in TIC.Line_Position (0) .. maxrow loop

         if history (arrow).established then
            colorado (history (arrow).run_elapsed, c_standard, 1, row, True);
            print_id (id     => history (arrow).id,
                      sid    => history (arrow).slavid,
                      row    => row,
                      status => history (arrow).action);
            colorado (history (arrow).action,
                      col_action (history (arrow).action), 15, row);
            colorado (history (arrow).origin, c_origin, 24, row);
            colorado (history (arrow).pkg_elapsed, c_standard, 70, row, True);
         else
            clear_row (row);
         end if;
         if arrow = cyclic_range'First then
            arrow := cyclic_range'Last;
         else
            arrow := arrow - 1;
         end if;
      end loop;
      Refresh_Zone (action);
   end refresh_history_window;


   ------------------------
   --  establish_colors  --
   ------------------------
   function establish_colors return Boolean is
   begin
      TIC.Start_Color;
      begin
         TIC.Init_Pair (TIC.Color_Pair (1), TIC.White,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (2), TIC.Green,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (3), TIC.Red,     TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (4), TIC.Yellow,  TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (5), TIC.Black,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (6), TIC.Cyan,    TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (7), TIC.Blue,    TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (8), TIC.Magenta, TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (9), TIC.Blue,    TIC.White);
      exception
         when TIC.Curses_Exception => return False;
      end;

      c_standard    := TIC.Color_Pair (1);
      c_success     := TIC.Color_Pair (2);
      c_failure     := TIC.Color_Pair (3);
      c_ignored     := TIC.Color_Pair (4);
      c_skipped     := TIC.Color_Pair (5);
      c_sumlabel    := TIC.Color_Pair (6);
      c_dashes      := TIC.Color_Pair (7);
      c_elapsed     := TIC.Color_Pair (4);
      c_tableheader := TIC.Color_Pair (1);
      c_origin      := TIC.Color_Pair (6);
      c_bldphase    := TIC.Color_Pair (4);
      c_shutdown    := TIC.Color_Pair (1);
      c_advisory    := TIC.Color_Pair (4);

      c_slave  (1).palette   := TIC.Color_Pair (1);  --  white / Black
      c_slave  (1).attribute := bright;

      c_slave  (2).palette   := TIC.Color_Pair (2);  --  light green / Black
      c_slave  (2).attribute := bright;

      c_slave  (3).palette   := TIC.Color_Pair (4);  --  yellow / Black
      c_slave  (3).attribute := bright;

      c_slave  (4).palette   := TIC.Color_Pair (8);  --  light magenta / Black
      c_slave  (4).attribute := bright;

      c_slave  (5).palette   := TIC.Color_Pair (3);  --  light red / Black
      c_slave  (5).attribute := bright;

      c_slave  (6).palette   := TIC.Color_Pair (7);  --  light blue / Black
      c_slave  (6).attribute := bright;

      c_slave  (7).palette   := TIC.Color_Pair (6);  --  light cyan / Black
      c_slave  (7).attribute := bright;

      c_slave  (8).palette   := TIC.Color_Pair (5);  --  dark grey / Black
      c_slave  (8).attribute := bright;

      c_slave  (9).palette   := TIC.Color_Pair (1);  --  light grey / Black
      c_slave  (9).attribute := normal;

      c_slave (10).palette   := TIC.Color_Pair (2);  --  light green / Black
      c_slave (10).attribute := normal;

      c_slave (11).palette   := TIC.Color_Pair (4);  --  brown / Black
      c_slave (11).attribute := normal;

      c_slave (12).palette   := TIC.Color_Pair (8);  --  dark magenta / Black
      c_slave (12).attribute := normal;

      c_slave (13).palette   := TIC.Color_Pair (3);  --  dark red / Black
      c_slave (13).attribute := normal;

      c_slave (14).palette   := TIC.Color_Pair (7);  --  dark blue / Black
      c_slave (14).attribute := normal;

      c_slave (15).palette   := TIC.Color_Pair (6);  --  dark cyan / Black
      c_slave (15).attribute := normal;

      c_slave (16).palette   := TIC.Color_Pair (9);  --  white / dark blue
      c_slave (16).attribute := normal;

      for bld in builders (17) .. builders (32)  loop
         c_slave (bld) := c_slave (bld - 16);
         c_slave (bld).attribute.Under_Line := True;
      end loop;
      for bld in builders (33) .. builders (64)  loop
         c_slave (bld) := c_slave (bld - 32);
      end loop;

      return True;

   end establish_colors;


   ------------------------------------------------------------------------
   --  zone_window
   ------------------------------------------------------------------------
   function zone_window (zone : zones) return TIC.Window is
   begin
      case zone is
         when builder => return zone_builders;
         when summary => return zone_summary;
         when action  => return zone_actions;
      end case;
   end zone_window;


   ------------------------------------------------------------------------
   --  Relocate
   ------------------------------------------------------------------------
   procedure Relocate
     (zone        : zones;
      next_line   : TIC.Line_Position;
      next_column : TIC.Column_Position := 0)
   is
   begin
      TIC.Move_Cursor (Win    => zone_window (zone),
                       Line   => next_line,
                       Column => next_column);
   exception
      when TIC.Curses_Exception => null;
   end Relocate;


   ------------------------------------------------------------------------
   --  Scrawl
   ------------------------------------------------------------------------
   procedure Scrawl (zone : zones; information : String) is
   begin
      TIC.Add (Win => zone_window (zone), Str => information);
   exception
      when TIC.Curses_Exception => null;
   end Scrawl;


   ------------------------------------------------------------------------
   --  Choose_Pen
   ------------------------------------------------------------------------
   procedure Choose_Pen
     (zone        : zones;
      attribute   : TIC.Character_Attribute_Set;
      pen_color   : TIC.Color_Pair)
   is
   begin
      TIC.Set_Character_Attributes (Win   => zone_window (zone),
                                    Attr  => attribute,
                                    Color => pen_color);
   exception
      when TIC.Curses_Exception => null;
   end Choose_Pen;


   ------------------------------------------------------------------------
   --  Return_To_Text_Mode
   ------------------------------------------------------------------------
   procedure Return_To_Text_Mode is
   begin
      TIC.End_Windows;
   exception
      when TIC.Curses_Exception => null;
   end Return_To_Text_Mode;


   ------------------------------------------------------------------------
   --  Refresh_Zone
   ------------------------------------------------------------------------
   procedure Refresh_Zone (zone : zones) is
   begin
      TIC.Refresh (Win => zone_window (zone));
   exception
      when TIC.Curses_Exception => null;
   end Refresh_Zone;


   ------------------------------------------------------------------------
   --  Start_Curses_Mode
   ------------------------------------------------------------------------
   function Start_Curses_Mode return Boolean is
   begin
      TIC.Init_Screen;
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end Start_Curses_Mode;

end Display;
