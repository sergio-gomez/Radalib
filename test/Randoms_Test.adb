-- Radalib, Copyright (c) 2023 by
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
-- library (see LICENSE.txt); if not, see https://www.gnu.org/licenses/


-- @filename Randoms_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 03/03/2010
-- @revision 21/01/2018
-- @brief Test of Random Numbers package

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Utils; use Utils;
with Histograms; use Histograms;
with Random_Numbers; use Random_Numbers;


procedure Randoms_Test is

  procedure Put_Bin(Num: in Natural; Total: in Positive) is
  begin
    Put(Num, Width => 6);
    Put("  (");
    Put(Float(100 * Num) / Float(Total), Fore => 2, Aft => 2, Exp => 0);
    Put("%)");
  end Put_Bin;

  procedure Put(H: in Histogram; Float_Values: in Boolean := True) is
  begin
    Put_Line("  Number of Values : " & I2S(Get_Num_Values(H)));
    if Float_Values then
      Put("     -Inf .. " & F2Se0(Get_Lower_Limit(H), Aft => 4));
    else
      Put("  <= "); Put(Integer(Get_Lower_Limit(H) - 0.5), Width => 2);
    end if;
    Put(" : "); Put_Bin(Get_Under_Lower_Limit(H), Get_Num_Values(H)); New_Line;
    for I in 1..Get_Num_Bins(H) loop
      if Float_Values then
        Put("  " & F2Se0(Get_Limit(H, I - 1), Aft => 4) & " .. " & F2Se0(Get_Limit(H, I), Aft => 4));
      else
        Put("     " & Right_Justify(I2S(Integer(Get_Limit(H, I) - 0.5)), Width => 2));
      end if;
      Put(" : "); Put_Bin(Get_Bin(H, I), Get_Num_Values(H)); New_Line;
    end loop;
    if Float_Values then
      Put("  " & F2Se0(Get_Upper_Limit(H), Aft => 4) & " ..    +Inf");
    else
      Put("  >= "); Put(Integer(Get_Upper_Limit(H) + 0.5), Width => 2);
    end if;
    Put(" : "); Put_Bin(Get_Over_Upper_Limit(H), Get_Num_Values(H)); New_Line;
    Put_Line("---");
  end;

  procedure Put(P: in Integers) is
  begin
    for I in P'Range loop
      Put("  " & I2S(P(I)));
    end loop;
    New_Line;
  end Put;

  G: Generator;
  H: Histogram;
  Num: Positive;
  Li, Ui: Integer;
  Lf, Uf, P: Float;
  Weights: PFloats;
  Permutation: PIntegers;
begin
  Reset(G);

  Put_Line("Uniform integers:"); New_Line;
  Num := 1_000_000;
  Li := 11;
  Ui := 20;
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    Add(H, Random_Uniform(G, Li, Ui));
  end loop;
  Put(H, False);
  Free(H);

  Put_Line("Uniform integers inverted range:"); New_Line;
  Num := 1_000_000;
  Li := 11;
  Ui := 20;
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    Add(H, Random_Uniform(G, Ui, Li));
  end loop;
  Put(H, False);
  Free(H);

  Put_Line("Uniform floats:"); New_Line;
  Num := 1_000_000;
  Lf := 10.0;
  Uf := 20.0;
  Initialize(H, Lf, Uf, Integer(Uf - Lf));
  for I in 1..Num loop
    Add(H, Random_Uniform(G, Lf, Uf));
  end loop;
  Put(H);
  Free(H);

  Put_Line("Uniform floats inverted range:"); New_Line;
  Num := 1_000_000;
  Lf := 10.0;
  Uf := 20.0;
  Initialize(H, Lf, Uf, Integer(Uf - Lf));
  for I in 1..Num loop
    Add(H, Random_Uniform(G, Uf, Lf));
  end loop;
  Put(H);
  Free(H);

  Put_Line("Bernoulli:"); New_Line;
  Num := 1_000_000;
  Li := 0;
  Ui := 1;
  P := 0.2;
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    if Random_Bernoulli(G, P) then
      Add(H, 1);
    else
      Add(H, 0);
    end if;
  end loop;
  Put(H, False);
  Free(H);

  Put_Line("Binomial integers:"); New_Line;
  Num := 1_000_000;
  Li := 0;
  Ui := 10;
  P := 0.2;
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    Add(H, Random_Binomial(G, Ui, P));
  end loop;
  Put(H, False);
  Free(H);

  Put_Line("Nonuniform integers:"); New_Line;
  Num := 1_000_000;
  Li := 1;
  Ui := 4;
  Weights := Alloc(Li, Ui);
  Weights.all := (1.0, 2.0, 3.0, 4.0);
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    Add(H, Random_Weighted(G, Weights));
  end loop;
  Put(H, False);
  Free(H);
  Free(Weights);

  Put_Line("Nonuniform integers but with equal weights:"); New_Line;
  Num := 100_000;
  Li := 1;
  Ui := 4;
  Weights := Alloc(Li, Ui);
  Weights.all := (0.0, 0.0, 0.0, 0.0);
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    Add(H, Random_Weighted(G, Weights));
  end loop;
  Put(H, False);
  Free(H);
  Free(Weights);

  Put_Line("Random permutations and histogram of first element:"); New_Line;
  Num := 5;
  Li := 1;
  Ui := 9;
  Permutation := Alloc(Li, Ui);
  for I in 1..Num loop
    Random_Permutation(G, Permutation);
    Put(Permutation.all);
  end loop;
  Free(Permutation);
  New_Line;

  Num := 5;
  Li := 3;
  Ui := 7;
  Permutation := Alloc(Li, Ui);
  for I in 1..Num loop
    Random_Permutation(G, Permutation.all);
    Put(Permutation.all);
  end loop;
  Free(Permutation);
  New_Line;

  Num := 100_000;
  Li := 3;
  Ui := 7;
  Permutation := Alloc(Li, Ui);
  Initialize(H, Li, Ui - Li + 1, 1);
  for I in 1..Num loop
    Random_Permutation(G, Permutation);
    Add(H, Permutation(Li));
  end loop;
  Put(H, False);
  Free(H);
  Free(Permutation);


end Randoms_Test;
