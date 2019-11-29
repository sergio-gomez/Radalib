-- Radalib, Copyright (c) 2019 by
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


-- @filename Histograms_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 7/02/2005
-- @revision 03/03/2010
-- @brief Test of the Histograms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Histograms; use Histograms;

procedure Histogram_Test is

  procedure Put(H: in Histogram; Float_Values: in Boolean := True) is
  begin
    Put("Number of Values : "); Put(Get_Num_Values(H), Width => 0); New_Line;
    if Float_Values then
      Put("     -Inf .. "); Put(Get_Lower_Limit(H), Fore => 4, Aft => 4, Exp => 0);
    else
      Put("<= "); Put(Integer(Get_Lower_Limit(H) - 0.5), Width => 2);
    end if;
    Put(" : "); Put(Get_Under_Lower_Limit(H), Width => 0); New_Line;
    for I in 1..Get_Num_Bins(H) loop
      if Float_Values then
        Put(Get_Limit(H, I - 1), Fore => 4, Aft => 4, Exp => 0);
        Put(" .. ");
        Put(Get_Limit(H, I), Fore => 4, Aft => 4, Exp => 0);
      else
        Put(Integer(Get_Limit(H, I) - 0.5), Width => 5);
      end if;
      Put(" : "); Put(Get_Bin(H, I), Width => 0); New_Line;
    end loop;
    if Float_Values then
      Put(Get_Upper_Limit(H), Fore => 4, Aft => 4, Exp => 0); Put(" ..      +Inf");
    else
      Put(">= "); Put(Integer(Get_Upper_Limit(H) + 0.5), Width => 2);
    end if;
    Put(" : "); Put(Get_Over_Upper_Limit(H), Width => 0); New_Line;
    Put_Line("---");
  end;

  H, Ch: Histogram;
  Ll: Float;
  Ul: Float;
  Nb, Bs: Positive;
  Lv: Integer;

begin

  Put_Line("Simple Histograms");
  Put_Line("---");

  Ll := 3.0;
  Ul := 9.0;
  Nb := 6;
  Initialize(H, Ll, Ul, Nb);
  for I in 0..Nb loop
    for J in 0..I loop
      Add(H, Ll + Float(2 * I + 1) * (Ul - Ll) / Float(2 * Nb));
    end loop;
  end loop;
  Put(H);
  Add(H, Ll);
  Add(H, Ul);
  Add(H, (Ll + Ul) / 2.0);
  Put(H);
  Free(H);

  Put_Line("Logarithmic Histograms");
  Put_Line("---");

  Ll := 0.001;
  Ul := 1000.0;
  Nb := 6;
  Initialize(H, Ll, Ul, Nb, True);
  for I in 0..Nb loop
    for J in 0..I loop
      Add(H, Ll * (Ul / Ll) ** (Float(2 * I + 1) / Float(2 * Nb)));
    end loop;
  end loop;
  Put(H);
  Add(H, Ll);
  Add(H, Ul);
  Add(H, Sqrt(Ll * Ul));
  Put(H);
  Free(H);

  Put_Line("Integer Histograms");
  Put_Line("---");

  Lv := -3;
  Nb := 7;
  Bs := 1;
  Initialize(H, Lv, Nb, Bs);
  for I in Lv..Lv+Nb-1 loop
    for J in 1..2 loop
      Add(H, I);
    end loop;
  end loop;
  Put(H, False);
  Add(H, 0);
  Add(H, Lv + Nb);
  Put(H, False);

  Put_Line("Accumulated Histograms");
  Put_Line("---");

  Ch := Accumulate_Histogram(H, Add_Left_Bins);
  Put(Ch, False);
  Free(Ch);
  Ch := Accumulate_Histogram(H, Add_Right_Bins);
  Put(Ch, False);
  Free(Ch);

  Free(H);

end Histogram_Test;
