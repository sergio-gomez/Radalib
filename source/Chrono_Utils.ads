-- Radalib, Copyright (c) 2022 by
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


-- @filename Chrono_Utils.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 14/12/2004
-- @revision 30/08/2009
-- @brief Chrono Utils

with Ada.Real_Time; use Ada.Real_Time;

package Chrono_Utils is

  -- Represents a Chronometer
  -- Prints a Ticker to standard output at a given Rate
  type Chronometer is limited private;

  Default_Rate  : Duration := 0.0;
  Default_Ticker: String   := ".";
  Default_Aft   : Natural  :=   6;


  -- Purpose : Start the Chronometer
  -- Note    : If Ticker is not an empty String and Rate is not zero, the Ticker is printed at the given rate
  --
  -- C       : The Chronometer
  -- Rate    : The number of seconds between consecutive printed Tickers
  -- Ticker  : The Ticker
  procedure Start(C: in out Chronometer; Rate: in Duration := Default_Rate; Ticker: in String := Default_Ticker);

  -- Purpose : Stop the Chronometer
  -- Note    : The time since last Start is Accumulated
  --
  -- C       : The Chronometer
  procedure Stop(C: in out Chronometer);

  -- Purpose : Obtain the Elapsed Time of a Chronometer
  -- Note    : If stopped, returns the time between last Start and Stop
  -- Note    : If not stopped, returns the time between last Start and now
  --
  -- C       : The Chronometer
  -- return  : The Elapsed Time
  function Elapsed(C: in Chronometer) return Duration;

  -- Purpose : Obtain the Accumulated Time of a Chronometer since first Start
  -- Note    : If stopped, returns the Accumulated time since first Start
  -- Note    : If not stopped, returns the Accumulated time plus the time between last Start and now
  --
  -- C       : The Chronometer
  -- return  : The Accumulated Time
  function Accumulated(C: in Chronometer) return Duration;

  -- Purpose : Set the Accumulated Time to zero
  -- Note    : Chronometer is not stopped, and last Start is not modified
  --
  -- C       : The Chronometer
  procedure Reset(C: in out Chronometer);

  -- Purpose : Put the Elapsed Time of a Chronometer to Standard Output
  -- Note    : If stopped, puts the time between last Start and Stop
  -- Note    : If not stopped, puts the time between last Start and now
  --
  -- C       : The Chronometer
  -- Aft     : The number of decimal digits
  procedure Put_Elapsed(C: in Chronometer; Aft: Natural := Default_Aft);

  -- Purpose : Put the Accumulated Time of a Chronometer since first Start to Standard Output
  -- Note    : If stopped, puts the Accumulated time since first Start
  -- Note    : If not stopped, puts the Accumulated time plus the time between last Start and now
  --
  -- C       : The Chronometer
  -- Aft     : The number of decimal digits
  procedure Put_Accumulated(C: in Chronometer; Aft: Natural := Default_Aft);

private

  task type Ticketer is
    entry Start(Rate: in Duration; Ticker: in String);
    entry Stop;
  end;

  type Pticketer is access Ticketer;

  type Chronometer is record
    T1: Time;
    T2: Time;
    Accum: Duration  := 0.0;
    Started: Boolean := False;
    Stopped: Boolean := False;
    Tics: Pticketer;
  end record;

end Chrono_Utils;

