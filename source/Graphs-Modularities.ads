-- Radalib, Copyright (c) 2016 by
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


-- @filename Graphs-Modularities.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 5/03/2006
-- @revision 23/09/2015
-- @brief Calculation of Modularities of Graphs

with Finite_Disjoint_Lists; use Finite_Disjoint_Lists;
with Graphs_Double;
with Graphs.Properties;

generic
  type Num is digits <>;
  type Nums is array(Integer range <>) of Num;
  type PNums is access Nums;
  type Numss is array(Integer range <>, Integer range <>) of Num;
  type PNumss is access Numss;
  with function To_Num(Value: in Edge_Value) return Num is <>;
  with function To_Value(Value: in Num) return Edge_Value is <>;
  with function Alloc(First: in Integer; Last: in Integer) return PNums is <>;
  with procedure Free(P: in out PNums) is <>;
  with function Alloc(First1, Last1, First2, Last2: in Integer) return PNumss is <>;
  with procedure Free(P: in out PNumss) is <>;
package Graphs.Modularities is

  package Graphs_Num_Properties is new Graphs.Properties(Num => Num, Nums => Nums, PNums => PNums,
                                                         Numss => Numss, PNumss => PNumss);
  use Graphs_Num_Properties;

  -- Type to choose between the different Modularity definitions
  type Modularity_Type is (
    Unweighted_Newman,
    Unweighted_Uniform_Nullcase,
    Weighted_Newman,
    Weighted_Signed,
    Weighted_Uniform_Nullcase,
    Weighted_Local_Average,
    Weighted_Uniform_Local_Average,
    Weighted_Links_Unweighted_Nullcase,
    Weighted_No_Nullcase,
    Weighted_Link_Rank
  );

  subtype Unweighted_Modularity_Type is Modularity_Type range Modularity_Type'First..Unweighted_Uniform_Nullcase;
  subtype Weighted_Modularity_Type   is Modularity_Type range Weighted_Newman..Modularity_Type'Last;

  -- Type for the Modularity Composition
  type Modularity_Rec is record
    Reward: Num := 0.0;
    Penalty: Num := 0.0;
    Total: Num := 0.0;
  end record;

  -- Type to keep the main quantities used during Modularity calculations
  type Modularity_Info is private;

  No_Resistance: constant Num;

  Unknown_Modularity_Error: exception;
  Uninitialized_Modularity_Info_Error: exception;
  Incompatible_Modules_Error: exception;
  Incompatible_Modularity_Type_Error: exception;


  -- Purpose : Obtain a Modularity Type from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    UN   | Unweighted_Newman
  -- Note    :    UUN  | Unweighted_Uniform_Nullcase
  -- Note    :    WN   | Weighted_Newman
  -- Note    :    WS   | Weighted_Signed
  -- Note    :    WUN  | Weighted_Uniform_Nullcase
  -- Note    :    WLA  | Weighted_Local_Average
  -- Note    :    WULA | Weighted_Uniform_Local_Average
  -- Note    :    WLUN | Weighted_Links_Unweighted_Nullcase
  -- Note    :    WNN  | Weighted_No_Nullcase
  -- Note    :    WLR  | Weighted_Link_Rank
  --
  -- Mt_Name : The Modularity Type Name
  -- return  : The Modularity Type
  -- raises  : Unknown_Modularity_Error
  function To_Modularity_Type(Mt_Name: in String) return Modularity_Type;

  -- Purpose : Obtain the Name of a Modularity Type
  -- Note    : Short and long names:
  -- Note    :    UN   | Unweighted_Newman
  -- Note    :    UUN  | Unweighted_Uniform_Nullcase
  -- Note    :    WN   | Weighted_Newman
  -- Note    :    WS   | Weighted_Signed
  -- Note    :    WUN  | Weighted_Uniform_Nullcase
  -- Note    :    WLA  | Weighted_Local_Average
  -- Note    :    WULA | Weighted_Uniform_Local_Average
  -- Note    :    WLUN | Weighted_Links_Unweighted_Nullcase
  -- Note    :    WNN  | Weighted_No_Nullcase
  -- Note    :    WLR  | Weighted_Link_Rank
  --
  -- Mt      : The Modularity Type
  -- return  : The Modularity Type Name
  function To_Name(Mt: in Modularity_Type; Short: in Boolean := False) return String;

  -- Purpose : Initialize Modularity Info
  -- Note    : Some modularity types need special initializations
  -- Note    : By default, only the minimum set of initializations is performed
  --
  -- Mi      : The Modularity Info
  -- Gr      : The Graph
  -- Mt      : The Modularity Type
  -- R       : The Resistance
  -- Pc      : The Penalty Coefficient
  -- raises  : Uninitialized_Graph_Error
  procedure Initialize(Mi: out Modularity_Info; Gr: in Graph; Mt: in Modularity_Type := Weighted_Signed; R: in Num := No_Resistance; Pc: in Num := 1.0);

  -- Purpose : Set the Resistance
  -- Note    : No-Resistance is not the same as Zero-Resistance for Unweighted Modularity Types
  --
  -- Mi      : The Modularity Info
  -- R       : The Resistance
  -- Mt      : The Modularity Type
  -- raises  : Uninitialized_Modularity_Info_Error
  procedure Set_Resistance(Mi: in Modularity_Info; R: in Num := No_Resistance; Mt: in Modularity_Type := Weighted_Signed);

  -- Purpose : Obtain the Resistance
  --
  -- Mi      : The Modularity Info
  -- return  : The Resistance
  -- raises  : Uninitialized_Modularity_Info_Error
  function Get_Resistance(Mi: in Modularity_Info) return Num;

  -- Purpose : Set the Penalty Coefficient
  --
  -- Mi      : The Modularity Info
  -- Pc      : The Penalty Coefficient
  -- raises  : Uninitialized_Modularity_Info_Error
  procedure Set_Penalty_Coefficient(Mi: in Modularity_Info; Pc: in Num := 1.0);

  -- Purpose : Obtain the Penalty Coefficient
  --
  -- Mi      : The Modularity Info
  -- return  : The Penalty Coefficient
  -- raises  : Uninitialized_Modularity_Info_Error
  function Get_Penalty_Coefficient(Mi: in Modularity_Info) return Num;

  -- Purpose : Update Modularity Info
  -- Note    : Only needed if Graph has changed
  -- Note    : Some modularity types need special initializations
  -- Note    : By default, only the minimum set of initializations is performed
  --
  -- Mi      : The Modularity Info
  -- Mt      : The Modularity Type
  -- raises  : Uninitialized_Modularity_Info_Error
  procedure Update_Graph(Mi: in Modularity_Info; Mt: in Modularity_Type := Weighted_Signed);

  -- Purpose : Initialization of special modularity information corresponding to certain modularity types
  -- Note    : Only needed if Graph or modularity type has changed
  --
  -- Mi      : The Modularity Info
  -- Mt      : The Modularity Type
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modularity_Type_Error
  procedure Special_Initializations(Mi: in Modularity_Info; Mt: in Modularity_Type);

  -- Purpose : Deallocate all the space used by Modularity Info
  --
  -- Mi      : The Modularity Info
  procedure Free(Mi: in out Modularity_Info);

  -- Purpose : Create a copy of a Modularity Info
  --
  -- Mi      : The Modularity Info
  -- return  : The Clone
  -- raises  : Uninitialized_Modularity_Info_Error
  function Clone(Mi: in Modularity_Info) return Modularity_Info;

  -- Purpose : Obtain the Number of Edges from a given Vertex
  --
  -- Mi      : The Modularity Info
  -- V       : The Vertex
  -- return  : The Number of Edges
  -- raises  : Uninitialized_Modularity_Info_Error
  function Degree_From(Mi: in Modularity_Info; V: in Vertex) return Natural;

  -- Purpose : Obtain the Number of Edges to a given Vertex
  --
  -- Mi      : The Modularity Info
  -- V       : The Vertex
  -- return  : The Number of Edges
  -- raises  : Uninitialized_Modularity_Info_Error
  function Degree_To(Mi: in Modularity_Info; V: in Vertex) return Natural;

  -- Purpose : Obtain the Total Degree
  --
  -- Mi      : The Modularity Info
  -- return  : The Total Degree
  -- raises  : Uninitialized_Modularity_Info_Error
  function Total_Degree(Mi: in Modularity_Info) return Natural;

  -- Purpose : Obtain the Number of Edges
  --
  -- Mi      : The Modularity Info
  -- return  : The Number of Edges
  -- raises  : Uninitialized_Modularity_Info_Error
  function Number_Of_Edges(Mi: in Modularity_Info) return Natural;

  -- Purpose : Obtain the Number of Self-Loops
  --
  -- Mi      : The Modularity Info
  -- return  : The Number of Self-Loops
  -- raises  : Uninitialized_Modularity_Info_Error
  function Number_Of_Self_Loops(Mi: in Modularity_Info) return Natural;

  -- Purpose : Obtain the Strength from a given Vertex
  --
  -- Mi      : The Modularity Info
  -- V       : The Vertex
  -- return  : The Strength
  -- raises  : Uninitialized_Modularity_Info_Error
  function Strength_From(Mi: in Modularity_Info; V: in Vertex) return Num;

  -- Purpose : Obtain the Strength to a given Vertex
  --
  -- Mi      : The Modularity Info
  -- V       : The Vertex
  -- return  : The Strength
  -- raises  : Uninitialized_Modularity_Info_Error
  function Strength_To(Mi: in Modularity_Info; V: in Vertex) return Num;

  -- Purpose : Obtain the Total Strength
  --
  -- Mi      : The Modularity Info
  -- return  : The Total Strength
  -- raises  : Uninitialized_Modularity_Info_Error
  function Total_Strength(Mi: in Modularity_Info) return Num;

  -- Purpose : To know whether a given Vertex has a Self-Loop
  --
  -- Mi      : The Modularity Info
  -- V       : The Vertex
  -- return  : True if Vertex has Self_Loop
  -- raises  : Uninitialized_Modularity_Info_Error
  function Has_Self_Loop(Mi: in Modularity_Info; V: in Vertex) return Boolean;

  -- Purpose : Obtain the Self-Loop of a given Vertex
  --
  -- Mi      : The Modularity Info
  -- V       : The Vertex
  -- return  : The Self-Loop
  -- raises  : Uninitialized_Modularity_Info_Error
  function Self_Loop(Mi: in Modularity_Info; V: in Vertex) return Num;

  -- Purpose : Obtain the Total Self-Loops Strength
  --
  -- Mi      : The Modularity Info
  -- return  : The Total Self-Loops Strength
  -- raises  : Uninitialized_Modularity_Info_Error
  function Total_Self_Loops_Strength(Mi: in Modularity_Info) return Num;

  -- Purpose : Obtain the Left Leading Eigenvector
  -- Note    : Returns null if not Weighted_Link_Rank initialization
  --
  -- Mi      : The Modularity Info
  -- return  : The Left Leading Eigenvector
  -- raises  : Uninitialized_Modularity_Info_Error
  function Left_Leading_Eigenvector(Mi: in Modularity_Info) return PNums;

  -- Purpose : Update the Modularity contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- Mt      : The Modularity Type
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Modularity(Mi: in Modularity_Info; L: in List; Mt: in Modularity_Type);

  -- Purpose : Update the Modularity contribution of the given Modules
  --
  -- Mi      : The Modularity Info
  -- Lol     : The Modules
  -- Mt      : The Modularity Type
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Modularity(Mi: in Modularity_Info; Lol: in List_Of_Lists; Mt: in Modularity_Type);

  -- Purpose : Calculate the Total Modularity from the given Modularity Info
  -- Note    : No update of the Modularity Info is performed
  --
  -- Mi      : The Modularity Info
  -- return  : The Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  function Total_Modularity(Mi: in Modularity_Info) return Num;

  -- Purpose : Calculate the Total Modularity from the given Modularity Info
  -- Note    : No update of the Modularity Info is performed
  --
  -- Mi      : The Modularity Info
  -- return  : The Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  function Total_Modularity(Mi: in Modularity_Info) return Modularity_Rec;

  -- Purpose : Calculate the Modularity Contribution of a Module from the given Modularity Info
  -- Note    : No update of the Modularity Info is performed
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- return  : The Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  function Partial_Modularity(Mi: in Modularity_Info; L: in List) return Num;

  -- Purpose : Calculate the Modularity Contribution of a Module from the given Modularity Info
  -- Note    : No update of the Modularity Info is performed
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- return  : The Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  function Partial_Modularity(Mi: in Modularity_Info; L: in List) return Modularity_Rec;

  -- Purpose : Calculate the Modularity Contribution of an Element from the given Modularity Info
  -- Note    : No update of the Modularity Info is performed
  --
  -- Mi      : The Modularity Info
  -- E       : The Element
  -- return  : The Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  function Element_Modularity(Mi: in Modularity_Info; E: in Element) return Num;

  -- Purpose : Calculate the Modularity Contribution of an Element from the given Modularity Info
  -- Note    : No update of the Modularity Info is performed
  --
  -- Mi      : The Modularity Info
  -- E       : The Element
  -- return  : The Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  function Element_Modularity(Mi: in Modularity_Info; E: in Element) return Modularity_Rec;

  -- Purpose : Calculate the Modularity of a Graph for the given Modules
  -- Note    : Modularity Info is updated
  --
  -- Mi      : The Modularity Info
  -- Lol     : The Modules
  -- Mt      : The Modularity Type
  -- return  : The Unweighted Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : Incompatible_Modules_Error
  function Modularity(Mi: in Modularity_Info; Lol: in List_Of_Lists; Mt: in Modularity_Type) return Num;

  -- Purpose : Calculate the Modularity of a Graph for the given Modules
  -- Note    : Modularity Info is updated
  --
  -- Mi      : The Modularity Info
  -- Lol     : The Modules
  -- Mt      : The Modularity Type
  -- return  : The Unweighted Modularity
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : Incompatible_Modules_Error
  function Modularity(Mi: in Modularity_Info; Lol: in List_Of_Lists; Mt: in Modularity_Type) return Modularity_Rec;

  -- Purpose : Calculate the Modularity of a Graph for the given Modules
  -- Note    : Modularity Info is lost
  -- Note    : Use Modularity_Info procedures for Modularity optimization or inspection
  --
  -- Gr      : The Graph
  -- Lol     : The Modules
  -- Mt      : The Modularity Type
  -- R       : The Resistance
  -- Pc      : The Penalty Coefficient
  -- return  : The Unweighted Modularity
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : Incompatible_Modules_Error
  function Modularity(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type; R: in Num := No_Resistance; Pc: in Num := 1.0) return Num;

  -- Purpose : Calculate the Modularity of a Graph for the given Modules
  -- Note    : Modularity Info is lost
  -- Note    : Use Modularity_Info procedures for Modularity optimization or inspection
  --
  -- Gr      : The Graph
  -- Lol     : The Modules
  -- Mt      : The Modularity Type
  -- R       : The Resistance
  -- Pc      : The Penalty Coefficient
  -- return  : The Unweighted Modularity
  -- raises  : Uninitialized_Graph_Error
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : Incompatible_Modules_Error
  function Modularity(Gr: in Graph; Lol: in List_Of_Lists; Mt: in Modularity_Type; R: in Num := No_Resistance; Pc: in Num := 1.0) return Modularity_Rec;


private

  ---------------------
  -- Modularity_Info --
  ---------------------

  No_Resistance: constant Num := Num'First;

  type Vertex_Info_Rec is record
    K: Natural := 0;
    W, W_Pos, W_Neg: Num := 0.0;
    Has_Self_Loop: Boolean := False;
    Self_Loop: Num := 0.0;
  end record;

  type Vertex_Info_Recs is array(Positive range <>) of Vertex_Info_Rec;
  type PVertex_Info_Recs is access Vertex_Info_Recs;

  type Modularity_Recs is array(Positive range <>) of Modularity_Rec;
  type PModularity_Recs is access Modularity_Recs;

  type Modularity_Info_Rec is record
    Gr: Graph;
    Size: Natural := 0;
    Directed: Boolean := True;
    Signed: Boolean := True;
    From: PVertex_Info_Recs;
    To: PVertex_Info_Recs;
    Lower_Q: Pmodularity_Recs;
    Two_M: Natural := 0;
    Two_W, Two_W_Pos, Two_W_Neg: Num := 0.0;
    Two_La: Num := 0.0;
    Two_Ula: Num := 0.0;
    Self_Loops: Num := 0.0;
    Self_Loops_N: Natural := 0;
    Gr_Trans: Graphs_Double.Graph;
    Eigenvec: PNums;
    Resistance: Num := No_Resistance;
    Penalty_Coefficient: Num := 1.0;
  end record;

  type Modularity_Info is access Modularity_Info_Rec;

  -- Purpose : Update the Unweighted Newman Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Unweighted_Newman(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Unweighted Uniform Nullcase Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Unweighted_Uniform_Nullcase(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Newman Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Newman(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Signed Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Signed(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Uniform Nullcase Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Uniform_Nullcase(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Local Average Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Local_Average(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Uniform Local Average Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Uniform_Local_Average(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Links Unweighted Nullcase Modularity Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Links_Unweighted_Nullcase(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted No Nullcase Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_No_Nullcase(Mi: in Modularity_Info; L: in List);

  -- Purpose : Update the Weighted Link Rank Contribution of a Module
  --
  -- Mi      : The Modularity Info
  -- L       : The Module
  -- raises  : Uninitialized_Modularity_Info_Error
  -- raises  : Incompatible_Modules_Error
  procedure Update_Weighted_Link_Rank(Mi: in Modularity_Info; L: in List);

  -- Purpose : Calculation of Random Walk Transitions Graph
  --
  -- Mi      : The Modularity Info
  -- raises  : Uninitialized_Modularity_Info_Error
  procedure Transitions_Graph(Mi: in Modularity_Info);

  -- Gr      : The Graph
  -- V       : The Vector
  -- return  : The Left Leading Eigenvector
  -- raises  : Uninitialized_Graph_Error
  procedure Left_Leading_Eigenvector(Gr: in Graphs_Double.Graph; Eigv: out PNums);

end Graphs.Modularities;
