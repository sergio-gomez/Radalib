-- Radalib, Copyright (c) 2017 by
-- Sergio Gomez (sergio.gomez@urv.cat), Alberto Fernandez (alberto.fernandez@urv.cat)
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the
-- GNU Lesser General Public License version 2.1 as published by the Free Software Foundation.
--
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License along with this
-- library (see LICENSE.txt); if not, see http://www.gnu.org/licenses/


-- @filename Chrono_Utils.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/12/2004
-- @revision 30/08/2009
-- @brief Chrono Utils

with Ada.Text_IO; use Ada.Text_IO;

with Utils.IO; use Utils.IO;

package body Chrono_Utils is

  --------------
  -- Ticketer --
  --------------

  task body Ticketer is
    type Pstring is access String;
    S: Pstring;
    R: Duration;
    Go_On: Boolean := True;
    T: Time;
  begin
    accept Start(Rate: in Duration; Ticker: in String) do
      S := new String(Ticker'Range);
      S.all := Ticker;
      R := Rate;
      T := Clock + To_Time_Span(R);
    end Start;
    while Go_On loop
      Put(S.all);
      select
        accept Stop;
        Go_On := False;
      or
        delay until T;
      end select;
      T := T + To_Time_Span(R);
    end loop;
  end Ticketer;

  -----------
  -- Start --
  -----------

  procedure Start(C: in out Chronometer; Rate: in Duration := Default_Rate; Ticker: in String := Default_Ticker) is
  begin
    C.T1 := Clock;
    C.Started := True;
    C.Stopped := False;
    if Rate = 0.0 or Ticker = "" then
      C.Tics := null;
    else
      C.Tics := new Ticketer;
      C.Tics.Start(Rate, Ticker);
    end if;
  end Start;

  ----------
  -- Stop --
  ----------

  procedure Stop(C: in out Chronometer) is
  begin
    if C.Started then
      C.T2 := Clock;
      C.Accum := C.Accum + To_Duration(C.T2 - C.T1);
      C.Started := False;
      C.Stopped := True;
      if C.Tics /= null then
        C.Tics.Stop;
      end if;
    end if;
  end Stop;

  -------------
  -- Elapsed --
  -------------

  function Elapsed(C: in Chronometer) return Duration is
  begin
    if C.Stopped then
      return To_Duration(C.T2 - C.T1);
    elsif C.Started then
      return To_Duration(Clock - C.T1);
    else
      return 0.0;
    end if;
  end Elapsed;

  -----------------
  -- Accumulated --
  -----------------

  function Accumulated(C: in Chronometer) return Duration is
  begin
    if C.Started and not C.Stopped then
      return C.Accum + To_Duration(Clock - C.T1);
    else
      return C.Accum;
    end if;
  end Accumulated;

  -----------
  -- Reset --
  -----------

  procedure Reset(C: in out Chronometer) is
  begin
    C.Accum := 0.0;
  end Reset;

  -----------------
  -- Put_Elapsed --
  -----------------

  procedure Put_Elapsed(C: in Chronometer; Aft: Natural := Default_Aft) is
  begin
    Put_Duration(Elapsed(C), Aft);
  end Put_Elapsed;

  ---------------------
  -- Put_Accumulated --
  ---------------------

  procedure Put_Accumulated(C: in Chronometer; Aft: Natural := Default_Aft) is
  begin
    Put_Duration(Accumulated(C), Aft);
  end Put_Accumulated;

end Chrono_Utils;

