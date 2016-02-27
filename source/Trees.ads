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


-- @filename Trees.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 23/02/2012
-- @revision 17/01/2016
-- @brief Tree data structure

with Linked_Lists;

generic
  type Node_Value is private;
  Default_Node_Value: Node_Value;
package Trees is

  -- Definitions
  type Node_Rec is private;
  type Node is access Node_Rec;
  subtype Tree is Node;
  subtype Subtree is Tree;

  package Nodes_Lists is new Linked_Lists(Node);
  use Nodes_Lists;

  subtype List_Of_Nodes is Linked_List;
  subtype List_Of_Trees is Linked_List;
  subtype List_Of_Subtrees is List_Of_Trees;
  subtype Forest is List_Of_Trees;

  type Node_Handler is access procedure(Nod: in Node);

  type Tree_Format is (Text_Tree, Newick_Tree, Json_Tree);
  Default_Tree_Format: constant Tree_Format := Text_Tree;

  Empty_Tree_Error: exception;
  No_Node_Error: exception;


  -- Purpose : Create a Tree formed by just one Node
  --
  -- Value   : The Root Value
  -- return  : The Tree
  function New_Tree(Value: in Node_Value := Default_Node_Value) return Tree;

  -- Purpose : Create a Node
  --
  -- Value   : The Node Value
  -- return  : The Node
  function New_Node(Value: in Node_Value := Default_Node_Value) return Node;

  -- Purpose : To know whether a Tree is Empty
  --
  -- T       : The Tree
  -- return  : True if Empty, False otherwise
  function Is_Empty(T: in Tree) return Boolean;

  -- Purpose : Obtain the Value of a Node
  --
  -- Nod     : The Node
  -- return  : The Value
  -- raises  : No_Node_Error
  function Value(Nod: in Node) return Node_Value;

  -- Purpose : Set the Value of a Node
  --
  -- Nod     : The Node
  -- Value   : The Value
  -- raises  : No_Node_Error
  procedure Set_Value(Nod: in Node; Value: in Node_Value := Default_Node_Value);

  -- Purpose : Remove a Tree
  -- Note    : All the Tree is removed even if the Tree is a Subtree
  --
  -- T       : The Tree
  procedure Free(T: in out Tree);

  -- Purpose : To know whether a Tree is the Subtree of a larger Tree
  -- Note    : In a Subtree the Root has a non null Parent
  --
  -- T       : The Tree
  -- return  : True if Subtree, False otherwise
  -- raises  : Empty_Tree_Error
  function Is_Subtree(T: in Tree) return Boolean;

  -- Purpose : Create a copy of a Subtree
  -- Note    : Current position and Saved positions on Children Lists are not cloned
  -- Note    : The Clone is an isolated Tree even if the original Tree is a Subtree
  --
  -- St      : The Subtree
  -- return  : The Clone
  -- raises  : Empty_Tree_Error
  function Clone(St: in Subtree) return Tree;

  -- Purpose : Obtain the Root of a Subtree
  -- Note    : In a Subtree, the returned Root Node may have a Parent
  --
  -- St      : The Subtree
  -- return  : The Root Node
  -- raises  : Empty_Tree_Error
  function Get_Root(St: in Subtree) return Node;

  -- Purpose : Obtain the Tree to which a Node belongs to
  --
  -- Nod     : The Node
  -- return  : The Tree
  -- raises  : No_Node_Error
  function Tree_Of(Nod: in Node) return Tree;

  -- Purpose : Obtain the Subtree which has a Node as Root
  --
  -- Nod     : The Subtree Root Node
  -- return  : The Subtree
  -- raises  : No_Node_Error
  function Subtree_Of(Nod: in Node) return Subtree;

  -- Purpose : To know whether a Node belongs to a Subtree
  --
  -- Nod     : The Node
  -- St      : The Subtree
  -- return  : True if Belongs, False otherwise
  -- raises  : No_Node_Error
  function Belongs_To(Nod: in Node; St: in Subtree) return Boolean;

  -- Purpose : To know whether a Node is Root
  -- Note    : A Node is Root if it has no Parent
  --
  -- Nod     : The Node
  -- return  : True if is Root, False otherwise
  -- raises  : No_Node_Error
  function Is_Root(Nod: in Node) return Boolean;

  -- Purpose : To know whether a Node is Internal
  -- Note    : A Node is Internal if it has at least one Child Node
  --
  -- Nod     : The Node
  -- return  : True if is Internal, False otherwise
  -- raises  : No_Node_Error
  function Is_Internal(Nod: in Node) return Boolean;

  -- Purpose : To know whether a Node is a Leaf
  -- Note    : A Node is a Leaf if it has no Child Node
  --
  -- Nod     : The Node
  -- return  : True if is a Leaf, False otherwise
  -- raises  : No_Node_Error
  function Is_Leaf(Nod: in Node) return Boolean;

  -- Purpose : To know whether a Node is Ancestor of another Node
  --
  -- Ancestor: The Ancestor Node
  -- Nod     : The Node
  -- return  : True if Node has Ancestor as ancestor, False otherwise
  -- raises  : No_Node_Error
  function Is_Ancestor(Ancestor: in Node; Nod: in Node) return Boolean;

  -- Purpose : To know whether two Nodes are Siblings
  -- Note    : Two Nodes are Siblings if they share the Parent
  --
  -- Nod1    : The first Node
  -- Nod2    : The second Node
  -- return  : True if Nodes are Siblings, False otherwise
  -- raises  : No_Node_Error
  function Are_Siblings(Nod1: in Node; Nod2: in Node) return Boolean;

  -- Purpose : To know whether two Nodes are Relatives
  -- Note    : Two Nodes are Relatives if they share the Root, i.e. they belong to the same Tree
  --
  -- Nod1    : The first Node
  -- Nod2    : The second Node
  -- return  : True if Nodes are Relatives, False otherwise
  -- raises  : No_Node_Error
  function Are_Relatives(Nod1: in Node; Nod2: in Node) return Boolean;

  -- Purpose : To know whether a Node has a Parent
  --
  -- Nod     : The Node
  -- return  : True if Has Parent, False otherwise
  -- raises  : No_Node_Error
  function Has_Parent(Nod: in Node) return Boolean;

  -- Purpose : Obtain the Parent of a Node
  --
  -- Nod     : The Node
  -- return  : The Parent
  -- raises  : No_Node_Error
  function Get_Parent(Nod: in Node) return Node;

  -- Purpose : To know whether a Node has Children
  --
  -- Nod     : The Node
  -- return  : True if Has Children, False otherwise
  -- raises  : No_Node_Error
  function Has_Children(Nod: in Node) return Boolean;

  -- Purpose : Obtain the Number of Children of a Node
  --
  -- Nod     : The Node
  -- return  : The Number of Children
  -- raises  : No_Node_Error
  function Number_Of_Children(Nod: in Node) return Natural;

  -- Purpose : Obtain the Children of a Node
  --
  -- Nod     : The Node
  -- return  : The Children
  -- raises  : No_Node_Error
  function Get_Children(Nod: in Node) return List_Of_Nodes;

  -- Purpose : Add a Child Node to a Parent Node
  -- Note    : The Child is added as the last Child in the Children List
  --
  -- Parent  : The Parent Node
  -- Value   : The Value of the Child Node
  -- raises  : No_Node_Error
  procedure Add_Child(Parent: in Node; Value: in Node_Value := Default_Node_Value);

  -- Purpose : Add a Child Node to a Parent Node
  -- Note    : The Child is added as the last Child in the Children List
  --
  -- Parent  : The Parent Node
  -- Value   : The Value of the Child Node
  -- return  : The Child Node
  -- raises  : No_Node_Error
  function Add_Child(Parent: in Node; Value: in Node_Value := Default_Node_Value) return Node;

  -- Purpose : Add a Child Node to a Parent Node
  -- Note    : The Child is added as the last Child in the Children List
  -- Note    : If the Child Node had a previous Parent, it is removed from its Children List
  --
  -- Parent  : The Parent Node
  -- Child   : The Child Node
  -- raises  : No_Node_Error
  procedure Add_Child(Parent: in Node; Child: in Node);

  -- Purpose : Add a Tree as Child of a Parent Node
  -- Note    : The Child Tree is added as the last Child in the Children List
  -- Note    : If the Tree is a Subtree, it is Extracted from the previous Tree
  --
  -- Parent  : The Parent Node
  -- Child   : The Child Tree
  -- raises  : No_Node_Error
  -- raises  : Empty_Tree_Error
  procedure Add_Tree(Parent: in Node; Child: in Tree);

  -- Purpose : Extract a Subtree from a Tree
  --
  -- St      : The Subtree
  procedure Extract_Subtree(St: in Tree);

  -- Purpose : Remove the Subtree under a certain Node
  --
  -- Nod     : The Node
  -- Included: True to Remove the given Node and descendants, False to Remove only its Children and descendants
  procedure Remove_Subtree(Nod: in out Node; Included: in Boolean := True);

  -- Purpose : Disconnect the Children of a Node
  -- Note    : Children Subtrees are not removed, just disconected from their Parent
  --
  -- Nod     : The Node
  procedure Disconnect_Children(Nod: in Node);

  -- Purpose : Disconnect a Node from its Children and Parent
  -- Note    : Children Subtrees and Parent are not removed, just disconected from the Node
  --
  -- Nod     : The Node
  procedure Isolate_Node(Nod: in Node);

  -- Purpose : Join two Subtrees in a new Tree
  -- Note    : The Subtrees are extracted from the previous Trees if needed
  -- Note    : Only non-empty Subtrees are joined
  --
  -- St1     : The First Subtree
  -- St2     : The Second Subtree
  -- Value   : The Root Value
  -- return  : The new Tree
  function Join_Subtrees(St1, St2: in Subtree; Value: in Node_Value := Default_Node_Value) return Tree;

  -- Purpose : Join Subtrees in a new Tree
  -- Note    : The Subtrees are extracted from the previous Trees if needed
  -- Note    : Only non-empty Subtrees are joined
  --
  -- Lst     : The List of Subtrees
  -- Value   : The Root Value
  -- return  : The new Tree
  function Join_Subtrees(Lst: in List_Of_Subtrees; Value: in Node_Value := Default_Node_Value) return Tree;

  -- Purpose : Obtain the Number of Nodes in a Subtree
  --
  -- St      : The Subtree
  function Number_Of_Nodes(St: in Subtree) return Natural;

  -- Purpose : Obtain the Number of Leaves in a Subtree
  --
  -- St      : The Subtree
  function Number_Of_Leaves(St: in Subtree) return Natural;

  -- Purpose : Obtain the Leaves in a Subtree
  --
  -- St      : The Subtree
  -- Leaves  : The Leaves
  procedure Get_Leaves(St: in Subtree; Leaves: in out List_Of_Nodes);

  -- Purpose : Obtain the Leaves in a Subtree
  --
  -- St      : The Subtree
  -- return  : The Leaves
  function Get_Leaves(St: in Subtree) return List_Of_Nodes;

  -- Purpose : Depth First Preorder Traversal of Tree with execution of Handler for all Nodes
  --
  -- Handler : The Handler
  -- T       : The Tree
  generic
    with procedure Handler(Nod: in Node) is <>;
  procedure Generic_Depth_First_Preorder_Traversal(T: in Tree);

  -- Purpose : Depth First Preorder Traversal of Tree with execution of Handler for all Nodes
  --
  -- T       : The Tree
  -- Handler : The Handler
  procedure Depth_First_Preorder_Traversal(T: in Tree; Handler: in Node_Handler);

  -- Purpose : Depth First Postorder Traversal of Tree with execution of Handler for all Nodes
  --
  -- Handler : The Handler
  -- T       : The Tree
  generic
    with procedure Handler(Nod: in Node) is <>;
  procedure Generic_Depth_First_Postorder_Traversal(T: in Tree);

  -- Purpose : Depth First Postorder Traversal of Tree with execution of Handler for all Nodes
  --
  -- T       : The Tree
  -- Handler : The Handler
  procedure Depth_First_Postorder_Traversal(T: in Tree; Handler: in Node_Handler);

  -- Purpose : Breadth First Traversal of Tree with execution of Handler for all Nodes
  --
  -- Handler : The Handler
  -- T       : The Tree
  generic
    with procedure Handler(Nod: in Node) is <>;
  procedure Generic_Breadth_First_Traversal(T: in Tree);

  -- Purpose : Breadth First Traversal of Tree with execution of Handler for all Nodes
  --
  -- T       : The Tree
  -- Handler : The Handler
  procedure Breadth_First_Traversal(T: in Tree; Handler: in Node_Handler);


private

  -----------
  -- Nodes --
  -----------

  type Node_Rec is record
    Value: Node_Value := Default_Node_Value;
    Parent: Node;
    Children: List_Of_Nodes;
  end record;

end Trees;
