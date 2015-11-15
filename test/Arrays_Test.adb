-- Radalib, Copyright (c) 2015 by
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


-- @filename Arrays_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 20/07/2009
-- @revision 18/12/2014
-- @brief Test of Arrays packages

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Utils; use Utils;
with Arrays_Float; use Arrays_Float;
with Arrays_Utils_Float; use Arrays_Utils_Float;
with Random_Numbers; use Random_Numbers;

procedure Arrays_Test is

  procedure Put(A: in Floats; Aft: in Field := 1) is
  begin
    Put_Line("Vector(" & I2S(A'First) & ".." & I2S(A'Last) & ")");
    for I in A'Range loop
      Put(Right_Justify(F2Se0(A(I), Aft), Aft + 4));
    end loop;
    New_Line;
  end Put;

  procedure Put(A: in Floats; Index: in Integers; Aft: in Field := 1) is
    Offset: constant Integer := Index'First - A'First;
  begin
    Put_Line("Vector(" & I2S(A'First) & ".." & I2S(A'Last) & ")");
    for I in A'Range loop
      Put(Right_Justify(I2S(Index(I + Offset)) & ":" & F2Se0(A(I), Aft), Aft + 8));
    end loop;
    New_Line;
  end Put;

  procedure Put(A: in Floatss; Aft: in Field := 1) is
  begin
    Put_Line("Matrix(" & I2S(A'First(1)) & ".." & I2S(A'Last(1)) & ", " & I2S(A'First(2)) & ".." & I2S(A'Last(2)) & ")");
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Put(Right_Justify(F2Se0(A(I, J), Aft), Aft + 4));
      end loop;
      New_Line;
    end loop;
  end Put;

  procedure Put(P: in PPsFloats; Aft: in Field := 1) is
  begin
    Put_Line("Irregular(" & I2S(P'First) & ".." & I2S(P'Last) & ")");
    for I in P'Range loop
      for J in P(I)'Range loop
        Put("  (" & I2S(I) & "," & I2S(J) & "):" & F2Se0(P(I)(J), Aft));
      end loop;
      New_Line;
    end loop;
  end Put;

  function Sorted(A: in Floats) return Boolean is
  begin
    for I in A'First..(A'Last - 1) loop
      if A(I) > A(I + 1) then
        return False;
      end if;
    end loop;
    return True;
  end Sorted;

  D1: constant := 5;
  D2: constant := 9;
  V, W: PFloats;
  M, Q: PFloatss;
  P: PPsFloats;
  Index: PIntegers;
  G: Generator;
  A: Integer;

begin
  V := Alloc(1, D1, 1.0);
  Put(V.all);
  New_Line;

  M := Alloc(1, D1, 1, D2, 0.0);
  for I in M'Range(1) loop
    for J in M'range(2) loop
      if I /= J then
        M(I, J) := Float(I + J);
      end if;
    end loop;
  end loop;
  Put(M.all);
  New_Line;

  Put(Transpose(M.all) * V.all);
  New_Line;

  V(1..3) := 2.0 * V(1..3);
  Put(V.all);
  New_Line;

  Put(Slice(M.All, 2, D1));
  New_Line;

  W := Alloc(2, D1);
  Q := Slice(M, 2, D1);
  W.all := Solve(Q.all, Slice(V.all, 2, D1));
  Put(W.all, Aft => 5);
  New_Line;

  Put(Q.all * W.all, Aft => 5);
  New_Line;

  W.all := (others => 1.0);
  W.all := Q.all * W.all;
  Put(W.all, Aft => 1);
  New_Line;

  W.all := Eigenvalues(Q.All);
--  Sort(W.all);
  Put(W.all, Aft => 5);
  New_Line;

  Swap(W.all);
  Put(W.all, Aft => 5);
  New_Line;

  Put(Column(Q.all, 3));
  New_Line;

  Put(Row(Q, D1).all);
  New_Line;

  Put(Diagonal(Q.all));
  New_Line;

  Free(V);
  Free(W);
  Free(M);
  Free(Q);

  M := Alloc(1, D1, 3, D2, 0.0);
--  M := Alloc(1, D1, 0.0);
--  M := Alloc(3, D2, 1, D1, 0.0);
  V := Alloc(2, D1 + 1);
  Set_Diagonal(M, 1.0);
  Put(M.all);
  New_Line;

  V.all := Diagonal(M.all);
  Put(V.all);
  New_Line;

  for I in V'Range loop
    V(I) := I2F(I);
  end loop;
  Set_Diagonal(M, V);
  Put(M.all);
  New_Line;

  Free(V);
  Free(M);

  Reset(G);

  P := Alloc_Upper(1, 5);
  for I in P'Range loop
    for J in P(I)'Range loop
      P(I)(J) := Round(Random(G), 2);
    end loop;
  end loop;
  Put(P, 2);
  Free(P);
  New_Line;

  P := Alloc_Lower(3, 7);
  for I in P'Range loop
    for J in P(I)'Range loop
      P(I)(J) := Round(Random(G), 2);
    end loop;
  end loop;
  Put(P, 2);
  Free(P);
  New_Line;

  Put_Line("Sorting a vector");
  V := Alloc(1, 10);
  for I in V'Range loop
    V(I) := Float(Random_Uniform(G, 0, 9));
  end loop;
  Put(V.all);
  Sort(V, Index);
  Put(V.all, Index.all);
  Free(V);
  Free(Index);
  New_Line;

  Put_Line("Checking Lexicographic error");
  for Reps in 1..5 loop
    A := Random_Uniform(G, 1, 4);
    V := Alloc(A, A + 2 + Random_Uniform(G, 0, 2));
    for I in V'Range loop
      V(I) := Float(Random_Uniform(G, 1, 2));
    end loop;
    Put(V.all);
    A := Random_Uniform(G, 1, 4);
    W := Alloc(A, A + 2 + Random_Uniform(G, 0, 2));
    for I in W'Range loop
      W(I) := Float(Random_Uniform(G, 1, 2));
    end loop;
    Put(W.all);
    if V < W then
      Put_Line("    Lower");
    elsif W < V then
      Put_Line("    Greater");
    else
      Put_Line("    Equal");
    end if;
    New_Line;
  end loop;

  Put_Line("Sorting an irregular matrix");
  P := Alloc(10, 30);
  for I in P'range loop
    P(I) := Alloc(1, 3 + Random_Uniform(G, 0, 2));
    for J in P(I)'Range loop
      P(I)(J) := Float(Random_Uniform(G, 0, 4));
    end loop;
  end loop;
  Put(P, 1);
  Sort(P);
  Put(P, 1);
  Free(P);
  New_Line;

  Put_Line("Sorting a triangular matrix");
  P := Alloc_Upper(1, 9);
  for I in P'range loop
    for J in P(I)'Range loop
      P(I)(J) := Float(Random_Uniform(G, 0, 4));
    end loop;
  end loop;
  Put(P, 1);
  Sort(P);
  Put(P, 1);
  Free(P);
  New_Line;

  Put("Sorting small random vectors ");
  for Size in 1..50 loop
    V := Alloc(1, Size);
    for Reps in 1..10_000 loop
      for I in V'Range loop
        V(I) := Float(Random_Uniform(G, 0, 9));
      end loop;
      Sort(V);
      if not Sorted(V.all) then
        Put_Line("  Error found!");
        Put(V.all, Aft => 5);
        New_Line;
        return;
      end if;
    end loop;
    Free(V);
    Put(".");
  end loop;
  New_Line;
  Put_Line("  OK");
  New_Line;

  Put("Sorting large random vectors ");
  Reset(G);
  for Factor in 1..20 loop
    V := Alloc(1, Factor * 100_000);
    for Reps in 1..2 loop
      for I in V'Range loop
        V(I) := Random(G);
      end loop;
      Sort(V);
      if not Sorted(V.All) then
        Put_Line("  Error found!");
        New_Line;
        return;
      end if;
    end loop;
    Put(".");
    Free(V);
  end loop;
  New_Line;
  Put_Line("  OK");
  New_Line;

end Arrays_Test;
