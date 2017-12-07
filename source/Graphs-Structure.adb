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


-- @filename Graphs-Structure.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 10/10/2009
-- @revision 02/03/2012
-- @brief Graphs Structure

with Ada.Unchecked_Deallocation;

package body Graphs.Structure is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Public_Edges, PPublic_Edges);

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Public_Vertices, Public_Graph);

  ----------------------
  -- Adjacency_Matrix --
  ----------------------

  function Adjacency_Matrix(Gr: in Graph) return PIntegerss is
    Am: PIntegerss;
    El: Edges_List;
    J: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Am := Alloc(1, Gr.Size, 1, Gr.Size);
    Am.all := (others => (others => 0));
    for I in 1..Gr.Size loop
      El := Edges_From(Get_Vertex(Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        J := Index_Of(To(Next(El)));
        Am(I, J) := 1;
      end loop;
      Restore(El);
    end loop;

    return Am;
  end Adjacency_Matrix;

  --------------------
  -- Weights_Matrix --
  --------------------

  function Weights_Matrix(Gr: in Graph) return PEdge_Valuess is
    Wm: PEdge_Valuess;
    El: Edges_List;
    E: Edge;
    J: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Wm := Alloc(1, Gr.Size, 1, Gr.Size);
    Wm.all := (others => (others => Zero_Value));
    for I in 1..Gr.Size loop
      El := Edges_From(Get_Vertex(Gr, I));
      Save(El);
      Reset(El);
      while Has_Next(El) loop
        E := Next(El);
        J := Index_Of(To(E));
        Wm(I, J) := Value(E);
      end loop;
      Restore(El);
    end loop;

    return Wm;
  end Weights_Matrix;

  ---------------------
  -- Graph_Structure --
  ---------------------

  function Graph_Structure(Gr: in Graph) return Public_Graph is
    Pgr: Public_Graph;
    El: Edges_List;
    Ne: Natural;
    E: Edge;
    I, J: Positive;
  begin
    if Gr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Pgr := new Public_Vertices(1..Gr.Size);
    Pgr.all := (others => (null, null));
    for I in 1..Gr.Size loop
      El := Edges_From(Get_Vertex(Gr, I));
      Ne := Number_Of_Edges(El);
      if Ne > 0 then
        Pgr(I).From := new Public_Edges(1..Ne);
        Save(El);
        Reset(El);
        for K in 1..Ne loop
          E := Next(El);
          J := Index_Of(To(E));
          Pgr(I).From(K) := (Index => J, Value => Value(E));
        end loop;
        Restore(El);
      end if;
    end loop;
    if Gr.Directed then
      for J in 1..Gr.Size loop
        El := Edges_To(Get_Vertex(Gr, J));
        Ne := Number_Of_Edges(El);
        if Ne > 0 then
          Pgr(J).To := new Public_Edges(1..Ne);
          Save(El);
          Reset(El);
          for K in 1..Ne loop
            E := Next(El);
            I := Index_Of(From(E));
            Pgr(J).To(K) := (Index => I, Value => Value(E));
          end loop;
          Restore(El);
        end if;
      end loop;
    else
      for J in 1..Gr.Size loop
        Pgr(J).To := Pgr(J).From;
      end loop;
    end if;

    return Pgr;
  end Graph_Structure;

  ------------------------------
  -- Graph_Structure_To_Graph --
  ------------------------------

  function Graph_Structure_To_Graph(Pgr: in Public_Graph) return Graph is
    Gr: Graph;
    Is_Dir: Boolean;
    J: Positive;
    Vf, Vt: Vertex;
  begin
    if Pgr = null then
      raise Uninitialized_Graph_Error;
    end if;

    Is_Dir := False;
    for I in Pgr'Range loop
      if Pgr(I).From /= Pgr(I).To then
        Is_Dir := True;
        exit;
      end if;
    end loop;
    Initialize(Gr, Pgr'Length, Is_Dir);

    for I in Pgr'Range loop
      Vf := Get_Vertex(Gr, I);
      if Pgr(I).From /= null then
        for K in Pgr(I).From'Range loop
          J := Pgr(I).From(K).Index;
          if Is_Dir or else I <= J then
            Vt := Get_Vertex(Gr, J);
            Add_Edge(Vf, Vt, Pgr(I).From(K).Value);
          end if;
        end loop;
      end if;
    end loop;

    return Gr;
  end Graph_Structure_To_Graph;

  ----------
  -- Free --
  ----------

  procedure Free(Pgr: in out Public_Graph) is
  begin
    if Pgr /= null then
      for I in Pgr'Range loop
        if Pgr(I).From = Pgr(I).To then
          if Pgr(I).From /= null then
            Dispose(Pgr(I).From);
          end if;
        else
          if Pgr(I).From /= null then
            Dispose(Pgr(I).From);
          end if;
          if Pgr(I).To /= null then
            Dispose(Pgr(I).To);
          end if;
        end if;
      end loop;
      Dispose(Pgr);
    end if;
  end Free;

end Graphs.Structure;
