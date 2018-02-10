-- Radalib, Copyright (c) 2018 by
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


-- @filename Finite_Disjoint_Lists-Algorithms.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 5/10/2005
-- @revision 19/12/2014
-- @brief Implementation of Lists of Lists Algorithms

package Finite_Disjoint_Lists.Algorithms is

  Unassigned_Elements_Error: exception;

  -- Types of Handle procedures
  type List_Of_Lists_Handler is access procedure(Lol: in List_Of_Lists);
  type List_Handler is access procedure(L: in List);

  -- Purpose : Sort the Elements of a List in increasing Index
  -- Note    : Current position and Saved positions are lost
  --
  -- L       : The List
  -- raises  : Not_A_List_Error
  procedure Sort_List(L: in List);

  -- Purpose : Sort the Elements of each List of a List of Lists in increasing Index
  -- Note    : Current position and Saved positions of each List are lost
  -- Note    : Unassigned List is also sorted
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Sort_Lists(Lol: in List_Of_Lists);

  -- Purpose : Sort the Lists of a List of Lists in decreasing Size
  -- Note    : Current position and Saved positions are lost
  --
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Sort_By_Size(Lol: in List_Of_Lists);

  -- Purpose : Obtain the Maximal Refinement of two List of Lists
  -- Note    : Any other possible Refinement is also a Refinement of the Maximal Refinement
  -- Note    : Input Lists of Lists must not have Unassigned Elements
  --
  -- Lol1    : The first List of Lists
  -- Lol2    : The second List of Lists
  -- return  : The Maximal Refinement List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  -- raises  : Incompatible_Lists_Of_Lists_Error
  -- raises  : Unassigned_Elements_Error
  function Maximal_Refinement(Lol1, Lol2: in List_Of_Lists) return List_Of_Lists;

  generic
    with procedure Handler(Lol: in List_Of_Lists) is <>;
  -- Purpose : Traverse and execute the Handler for all possible Partitions of given Number of Elements
  --
  -- Handler      : The Handler
  -- Num_Elements : The number of Elements
  procedure Generic_All_Partitions_Traversal(Num_Elements: in Positive);

  -- Purpose : Traverse and execute the Handler for all possible Partitions of given Number of Elements
  --
  -- Num_Elements : The number of Elements
  -- Handler      : The Handler
  procedure All_Partitions_Traversal(Num_Elements: in Positive; Handler: in List_Of_Lists_Handler);

  generic
    with procedure Handler(L: in List) is <>;
  -- Purpose : Traverse and execute the Handler for all possible Subsets of a Set with given Number of Elements
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Handler      : The Handler
  -- Num_Elements : The number of Elements
  procedure Generic_All_Subsets_Traversal(Num_Elements: in Positive);

  -- Purpose : Traverse and execute the Handler for all possible Subsets of a Set with given Number of Elements
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Num_Elements : The number of Elements
  -- Handler      : The Handler
  procedure All_Subsets_Traversal(Num_Elements: in Positive; Handler: in List_Handler);

  generic
    with procedure Handler(L: in List) is <>;
  -- Purpose : Traverse and execute the Handler for all Subsets up to a Maximum Size of a Set with given Number of Elements
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Handler      : The Handler
  -- Num_Elements : The number of Elements
  -- Max_Size     : The Maximun Size of the Subsets
  procedure Generic_Subsets_Traversal(Num_Elements: in Positive; Max_Size: in Natural);

  -- Purpose : Traverse and execute the Handler for all Subsets up to a Maximum Size of a Set with given Number of Elements
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Num_Elements : The number of Elements
  -- Max_Size     : The Maximun Size of the Subsets
  -- Handler      : The Handler
  procedure Subsets_Traversal(Num_Elements: in Positive; Max_Size: in Natural; Handler: in List_Handler);

  generic
    with procedure Handler(L: in List) is <>;
  -- Purpose : Traverse and execute the Handler for all possible Subsets of the Unassigned Elements in a List of Lists
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Handler : The Handler
  -- Lol     : The List of Lists
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Generic_All_Unassigned_Subsets_Traversal(Lol: in List_Of_Lists);

  -- Purpose : Traverse and execute the Handler for all possible Subsets of the Unassigned Elements in a List of Lists
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Lol     : The List of Lists
  -- Handler : The Handler
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure All_Unassigned_Subsets_Traversal(Lol: in List_Of_Lists; Handler: in List_Handler);

  generic
    with procedure Handler(L: in List) is <>;
  -- Purpose : Traverse and execute the Handler for all possible Subsets up to a Maximum Size of the Unassigned Elements in a List of Lists
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Handler : The Handler
  -- Lol     : The List of Lists
  -- Max_Size: The Maximun Size of the Subsets
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Generic_Unassigned_Subsets_Traversal(Lol: in List_Of_Lists; Max_Size: in Natural);

  -- Purpose : Traverse and execute the Handler for all possible Subsets up to a Maximum Size of the Unassigned Elements in a List of Lists
  -- Note    : The Subset passed to the Handler is the Current List in its List Of Lists
  --
  -- Lol     : The List of Lists
  -- Max_Size: The Maximun Size of the Subsets
  -- Handler : The Handler
  -- raises  : Uninitialized_List_Of_Lists_Error
  procedure Unassigned_Subsets_Traversal(Lol: in List_Of_Lists; Max_Size: in Natural; Handler: in List_Handler);

end Finite_Disjoint_Lists.Algorithms;
