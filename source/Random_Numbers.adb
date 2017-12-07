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


-- @filename Random_Numbers.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 02/03/2010
-- @revision 10/04/2013
-- @brief Several Random Number Generators

package body Random_Numbers is

  --------------------
  -- Random_Uniform --
  --------------------

  function Random_Uniform(G: in Generator; From, To: in Float) return Float is
  begin
    return From + Random_Uniform(G) * (To - From);
  end Random_Uniform;

  --------------------
  -- Random_Uniform --
  --------------------

  function Random_Uniform(G: in Generator; From, To: in Integer) return Integer is
    R: Integer;
  begin
    if From < To then
      R := From + Floor(Random_Uniform(G) * Float(To - From + 1));
      if R > To then
        R := To;
      end if;
    elsif From > To then
      R := To + Floor(Random_Uniform(G) * Float(From - To + 1));
      if R > From then
        R := From;
      end if;
    else
      return From;
    end if;
    return R;
  end Random_Uniform;

  ----------------------
  -- Random_Bernoulli --
  ----------------------

  function Random_Bernoulli(G: in Generator; P: in Probability) return Boolean is
  begin
    if P = 1.0 then
      return True;
    elsif P = 0.0 then
      return False;
    else
      return Random_Uniform(G) < P;
    end if;
  end Random_Bernoulli;

  ---------------------
  -- Random_Binomial --
  ---------------------

  function Random_Binomial(G: in Generator; N: in Natural; P: in Probability) return Natural is
    Num: Natural := 0;
  begin
    if P = 1.0 then
      return N;
    elsif P = 0.0 then
      return 0;
    else
      for I in 1..N loop
        if Random_Uniform(G) < P then
          Num := Num + 1;
        end if;
      end loop;
      return Num;
    end if;
  end Random_Binomial;

  ---------------------
  -- Random_Weighted --
  ---------------------

  function Random_Weighted(G: in Generator; Weights: in Floats) return Integer is
    Total, Partial, X: Float;
  begin
    Total := 0.0;
    for I in Weights'Range loop
      Total := Total + Weights(I);
    end loop;
    if Total <= 0.0 then
      return Random_Uniform(G, Weights'First, Weights'Last);
    else
      X := Total * Random_Uniform(G);
      Partial := 0.0;
      for I in Weights'Range loop
        Partial := Partial + Weights(I);
        if X <= Partial then
          return I;
        end if;
      end loop;
    end if;
    return Weights'Last;
  end Random_Weighted;

  ---------------------
  -- Random_Weighted --
  ---------------------

  function Random_Weighted(G: in Generator; Weights: in PFloats) return Integer is
    Total, Partial, X: Float;
  begin
    Total := 0.0;
    for I in Weights'Range loop
      Total := Total + Weights(I);
    end loop;
    if Total <= 0.0 then
      return Random_Uniform(G, Weights'First, Weights'Last);
    else
      X := Total * Random_Uniform(G);
      Partial := 0.0;
      for I in Weights'Range loop
        Partial := Partial + Weights(I);
        if X <= Partial then
          return I;
        end if;
      end loop;
    end if;
    return Weights'Last;
  end Random_Weighted;

end Random_Numbers;
