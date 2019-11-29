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


-- @filename Trees.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 23/02/2012
-- @revision 17/01/2016
-- @brief Tree data structure

with Ada.Unchecked_Deallocation;
with Queues;

package body Trees is

  -------------
  -- Dispose --
  -------------

  procedure Dispose is new Ada.Unchecked_Deallocation(Node_Rec, Node);

  --------------
  -- New_Tree --
  --------------

  function New_Tree(Value: in Node_Value := Default_Node_Value) return Tree is
    T: Tree;
  begin
    T := new Node_Rec;
    T.Value := Value;
    T.Parent := null;
    Initialize(T.Children);
    return T;
  end New_Tree;

  --------------
  -- New_Node --
  --------------

  function New_Node(Value: in Node_Value := Default_Node_Value) return Node is
  begin
    return New_Tree(Value);
  end New_Node;

  --------------
  -- Is_Empty --
  --------------

  function Is_Empty(T: in Tree) return Boolean is
  begin
    if T = null then
      return True;
    end if;

    return not Is_Initialized(T.Children);
  end Is_Empty;

  -----------
  -- Value --
  -----------

  function Value(Nod: in Node) return Node_Value is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Nod.Value;
  end Value;

  ---------------
  -- Set_Value --
  ---------------

  procedure Set_Value(Nod: in Node; Value: in Node_Value := Default_Node_Value) is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    Nod.Value := Value;
  end Set_Value;

  ----------
  -- Free --
  ----------

  procedure Free(T: in out Tree) is

    procedure Recursive_Dispose(Nod: in out Node) is
      Ch: List_Of_Nodes;
      R: Node;
    begin
      if Has_Children(Nod) then
        Ch := Get_Children(Nod);
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          R := Next(Ch);
          Recursive_Dispose(R);
        end loop;
        Restore(Ch);
      else
        Free(Nod.Children);
        Dispose(Nod);
        Nod := null;
      end if;
    end Recursive_Dispose;

    Nod: Node;

  begin
    if not Is_Empty(T) then
      Nod := T;
      while Nod.Parent /= null loop
        Nod := Nod.Parent;
      end loop;
      Recursive_Dispose(Nod);
    end if;
    T := null;
  end Free;

  ----------------
  -- Is_Subtree --
  ----------------

  function Is_Subtree(T: in Tree) return Boolean is
  begin
    if Is_Empty(T) then
      raise Empty_Tree_Error;
    end if;

    return T.Parent /= null;
  end Is_Subtree;

  -----------
  -- Clone --
  -----------

  function Clone(St: in Subtree) return Tree is

    procedure Recursive_Clone(Ori, Cloned: in Node) is
      Nod_Ori, Nod_Cloned: Node;
      Ch: List_Of_Nodes;
    begin
      if Has_Children(Ori) then
        Ch := Get_Children(Ori);
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          Nod_Ori := Next(Ch);
          Nod_Cloned := New_Node(Nod_Ori.Value);
          Add_Child(Cloned, Nod_Cloned);
          Recursive_Clone(Nod_Ori, Nod_Cloned);
        end loop;
        Restore(Ch);
      end if;
    end Recursive_Clone;

    Cloned: Node;

  begin
    if Is_Empty(St) then
      raise Empty_Tree_Error;
    end if;

    Cloned := New_Node(St.Value);
    Recursive_Clone(St, Cloned);
    return Cloned;
  end Clone;

  --------------
  -- Get_Root --
  --------------

  function Get_Root(St: in Subtree) return Node is
  begin
    if Is_Empty(St) then
      raise Empty_Tree_Error;
    end if;

    return St;
  end Get_Root;

  -------------
  -- Tree_Of --
  -------------

  function Tree_Of(Nod: in Node) return Tree is
    T: Tree;
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    T := Nod;
    while T.Parent /= null loop
      T := T.Parent;
    end loop;
    return T;
  end Tree_Of;

  ----------------
  -- Subtree_Of --
  ----------------

  function Subtree_Of(Nod: in Node) return Subtree is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Nod;
  end Subtree_Of;

  ----------------
  -- Belongs_To --
  ----------------

  function Belongs_To(Nod: in Node; St: in Subtree) return Boolean is
    R: Node;
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    if Is_Empty(St) then
      return False;
    elsif Nod = St then
      return True;
    end if;
    R := Nod;
    while R.Parent /= null loop
      R := R.Parent;
      if R = St then
        return True;
      end if;
    end loop;
    return False;
  end Belongs_To;

  -------------
  -- Is_Root --
  -------------

  function Is_Root(Nod: in Node) return Boolean is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Nod.Parent = null;
  end Is_Root;

  -----------------
  -- Is_Internal --
  -----------------

  function Is_Internal(Nod: in Node) return Boolean is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Size(Nod.Children) > 0;
  end Is_Internal;

  -------------
  -- Is_Leaf --
  -------------

  function Is_Leaf(Nod: in Node) return Boolean is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Size(Nod.Children) = 0;
  end Is_Leaf;

  -----------------
  -- Is_Ancestor --
  -----------------

  function Is_Ancestor(Ancestor: in Node; Nod: in Node) return Boolean is
    R: Node;
  begin
    if Is_Empty(Ancestor) or Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    if Nod = Ancestor then
      return False;
    end if;
    R := Nod;
    while R.Parent /= null loop
      R := R.Parent;
      if R = Ancestor then
        return True;
      end if;
    end loop;
    return False;
  end Is_Ancestor;

  ------------------
  -- Are_Siblings --
  ------------------

  function Are_Siblings(Nod1: in Node; Nod2: in Node) return Boolean is
  begin
    if Is_Empty(Nod1) or Is_Empty(Nod2) then
      raise No_Node_Error;
    end if;

    return Nod1.Parent = Nod2.Parent;
  end Are_Siblings;

  -------------------
  -- Are_Relatives --
  -------------------

  function Are_Relatives(Nod1: in Node; Nod2: in Node) return Boolean is
  begin
    if Is_Empty(Nod1) or Is_Empty(Nod2) then
      raise No_Node_Error;
    end if;

    return Tree_Of(Nod1) = Tree_Of(Nod2);
  end Are_Relatives;

  ----------------
  -- Has_Parent --
  ----------------

  function Has_Parent(Nod: in Node) return Boolean is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Nod.Parent /= null;
  end Has_Parent;

  ----------------
  -- Get_Parent --
  ----------------

  function Get_Parent(Nod: in Node) return Node is
  begin
    if Is_Empty(Nod) or else Is_Empty(Nod.Parent) then
      raise No_Node_Error;
    end if;

    return Nod.Parent;
  end Get_Parent;

  ------------------
  -- Has_Children --
  ------------------

  function Has_Children(Nod: in Node) return Boolean is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Size(Nod.Children) > 0;
  end Has_Children;

  ------------------------
  -- Number_Of_Children --
  ------------------------

  function Number_Of_Children(Nod: in Node) return Natural is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Size(Nod.Children);
  end Number_Of_Children;

  ------------------
  -- Get_Children --
  ------------------

  function Get_Children(Nod: in Node) return List_Of_Nodes is
  begin
    if Is_Empty(Nod) then
      raise No_Node_Error;
    end if;

    return Nod.Children;
  end Get_Children;

  ---------------
  -- Add_Child --
  ---------------

  procedure Add_Child(Parent: in Node; Value: in Node_Value := Default_Node_Value) is
    Nod: Node;
  begin
    if Is_Empty(Parent) then
      raise No_Node_Error;
    end if;

    Nod := New_Node(Value);
    Nod.Parent := Parent;
    Add_Last(Nod, Parent.Children);
  end Add_Child;

  ---------------
  -- Add_Child --
  ---------------

  function Add_Child(Parent: in Node; Value: in Node_Value := Default_Node_Value) return Node is
    Nod: Node;
  begin
    if Is_Empty(Parent) then
      raise No_Node_Error;
    end if;

    Nod := New_Node(Value);
    Nod.Parent := Parent;
    Add_Last(Nod, Parent.Children);
    return Nod;
  end Add_Child;

  ---------------
  -- Add_Child --
  ---------------

  procedure Add_Child(Parent: in Node; Child: in Node) is
  begin
    if Is_Empty(Parent) or Is_Empty(Child) then
      raise No_Node_Error;
    end if;

    if Child.Parent /= null then
      Remove_All(Child, Child.Parent.Children);
    end if;
    Child.Parent := Parent;
    Add_Last(Child, Parent.Children);
  end Add_Child;

  --------------
  -- Add_Tree --
  --------------

  procedure Add_Tree(Parent: in Node; Child: in Tree) is
  begin
    if Is_Empty(Parent) then
      raise No_Node_Error;
    end if;
    if Is_Empty(Child) then
      raise Empty_Tree_Error;
    end if;

    if Child.Parent /= null then
      Remove_All(Child, Child.Parent.Children);
    end if;
    Child.Parent := Parent;
    Add_Last(Child, Parent.Children);
  end Add_Tree;

  ---------------------
  -- Extract_Subtree --
  ---------------------

  procedure Extract_Subtree(St: in Tree) is
  begin
    if not Is_Empty(St) then
      if St.Parent /= null then
        Remove_All(St, St.Parent.Children);
        St.Parent := null;
      end if;
    end if;
  end Extract_Subtree;

  --------------------
  -- Remove_Subtree --
  --------------------

  procedure Remove_Subtree(Nod: in out Node; Included: in Boolean := True) is
    Ch: List_Of_Nodes;
    R: Node;
  begin
    if not Is_Empty(Nod) then
      if Included then
        Extract_Subtree(Nod);
        Free(Nod);
      elsif Size(Nod.Children) > 0 then
        Ch := Nod.Children;
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          R := Get(Ch);
          Remove(Ch);
          R.Parent := null;
          Free(R);
        end loop;
        Restore(Ch);
      end if;
    end if;
  end Remove_Subtree;

  -------------------------
  -- Disconnect_Children --
  -------------------------

  procedure Disconnect_Children(Nod: in Node) is
    Ch: List_Of_Nodes;
    R: Node;
  begin
    if not Is_Empty(Nod) then
      if Size(Nod.Children) > 0 then
        Ch := Nod.Children;
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          R := Next(Ch);
          R.Parent := null;
        end loop;
        Restore(Ch);
        Clear(Ch);
      end if;
    end if;
  end Disconnect_Children;

  ------------------
  -- Isolate_Node --
  ------------------

  procedure Isolate_Node(Nod: in Node) is
  begin
    Disconnect_Children(Nod);
    Extract_Subtree(Nod);
  end Isolate_Node;

  -------------------
  -- Join_Subtrees --
  -------------------

  function Join_Subtrees(St1, St2: in Subtree; Value: in Node_Value := Default_Node_Value) return Tree is
    T: Tree;
  begin
    T := New_Tree(Value);
    if not Is_Empty(St1) then
      Add_Tree(T, St1);
    end if;
    if not Is_Empty(St2) then
      Add_Tree(T, St2);
    end if;
    return T;
  end Join_Subtrees;

  -------------------
  -- Join_Subtrees --
  -------------------

  function Join_Subtrees(Lst: in List_Of_Subtrees; Value: in Node_Value := Default_Node_Value) return Tree is
    T: Tree;
    St: Subtree;
  begin
    T := New_Tree(Value);
    if Is_Initialized(Lst) then
      Save(Lst);
      Reset(Lst);
      while Has_Next(Lst) loop
        St := Next(Lst);
        if not Is_Empty(St) then
          Add_Tree(T, St);
        end if;
      end loop;
      Restore(Lst);
    end if;
    return T;
  end Join_Subtrees;

  ---------------------
  -- Number_Of_Nodes --
  ---------------------

  function Number_Of_Nodes(St: in Subtree) return Natural is
    Count: Natural := 1;
    Ch: List_Of_Nodes;
  begin
    if Is_Empty(St) then
      return 0;
    end if;
    Ch := St.Children;
    Save(Ch);
    Reset(Ch);
    while Has_Next(Ch) loop
      Count := Count + Number_Of_Nodes(Next(Ch));
    end loop;
    Restore(Ch);
    return Count;
  end Number_Of_Nodes;

  ----------------------
  -- Number_Of_Leaves --
  ----------------------

  function Number_Of_Leaves(St: in Subtree) return Natural is
    Count: Natural := 0;
    Ch: List_Of_Nodes;
  begin
    if Is_Empty(St) then
      return 0;
    end if;
    Ch := St.Children;
    if Size(Ch) = 0 then
      return 1;
    end if;
    Save(Ch);
    Reset(Ch);
    while Has_Next(Ch) loop
      Count := Count + Number_Of_Leaves(Next(Ch));
    end loop;
    Restore(Ch);
    return Count;
  end Number_Of_Leaves;

  ----------------
  -- Get_Leaves --
  ----------------

  procedure Get_Leaves(St: in Subtree; Leaves: in out List_Of_Nodes) is
    Ch: List_Of_Nodes;
  begin
    if not Is_Initialized(Leaves) then
      Initialize(Leaves);
    end if;
    if not Is_Empty(St) then
      Ch := St.Children;
      if Size(Ch) = 0 then
        Add_Last(St, Leaves);
      else
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          Get_Leaves(Next(Ch), Leaves);
        end loop;
        Restore(Ch);
      end if;
    end if;
  end Get_Leaves;

  ----------------
  -- Get_Leaves --
  ----------------

  function Get_Leaves(St: in Subtree) return List_Of_Nodes is
    Leaves: List_Of_Nodes;
  begin
    Initialize(Leaves);
    Get_Leaves(St, Leaves);
    return Leaves;
  end Get_Leaves;

  --------------------------------------------
  -- Generic_Depth_First_Preorder_Traversal --
  --------------------------------------------

  procedure Generic_Depth_First_Preorder_Traversal(T: in Tree) is

    procedure Recursive_Traversal(Nod: in Node) is
      Ch: List_Of_Nodes;
    begin
      Handler(Nod);
      Ch := Nod.Children;
      Save(Ch);
      Reset(Ch);
      while Has_Next(Ch) loop
        Recursive_Traversal(Next(Ch));
      end loop;
      Restore(Ch);
    end Recursive_Traversal;

  begin
    if not Is_Empty(T) then
      Recursive_Traversal(T);
    end if;
  end Generic_Depth_First_Preorder_Traversal;

  ------------------------------------
  -- Depth_First_Preorder_Traversal --
  ------------------------------------

  procedure Depth_First_Preorder_Traversal(T: in Tree; Handler: in Node_Handler) is
    procedure Traversal is new Generic_Depth_First_Preorder_Traversal(Handler.all);
  begin
    Traversal(T);
  end Depth_First_Preorder_Traversal;

  ---------------------------------------------
  -- Generic_Depth_First_Postorder_Traversal --
  ---------------------------------------------

  procedure Generic_Depth_First_Postorder_Traversal(T: in Tree) is

    procedure Recursive_Traversal(Nod: in Node) is
      Ch: List_Of_Nodes;
    begin
      Ch := Nod.Children;
      Save(Ch);
      Reset(Ch);
      while Has_Next(Ch) loop
        Recursive_Traversal(Next(Ch));
      end loop;
      Restore(Ch);
      Handler(Nod);
    end Recursive_Traversal;

  begin
    if not Is_Empty(T) then
      Recursive_Traversal(T);
    end if;
  end Generic_Depth_First_Postorder_Traversal;

  -------------------------------------
  -- Depth_First_Postorder_Traversal --
  -------------------------------------

  procedure Depth_First_Postorder_Traversal(T: in Tree; Handler: in Node_Handler) is
    procedure Traversal is new Generic_Depth_First_Postorder_Traversal(Handler.all);
  begin
    Traversal(T);
  end Depth_First_Postorder_Traversal;

  -------------------------------------
  -- Generic_Breadth_First_Traversal --
  -------------------------------------

  procedure Generic_Breadth_First_Traversal(T: in Tree) is
    package Queues_Nodes is new Queues(Node); use Queues_Nodes;
    Q: Queue;
    Nod: Node;
    Ch: List_Of_Nodes;
  begin
    if not Is_Empty(T) then
      Initialize(Q);
      Enqueue(T, Q);
      while not Is_Empty(Q) loop
        Nod := Dequeue(Q);
        Handler(Nod);
        Ch := Nod.Children;
        Save(Ch);
        Reset(Ch);
        while Has_Next(Ch) loop
          Enqueue(Next(Ch), Q);
        end loop;
        Restore(Ch);
      end loop;
      Free(Q);
    end if;
  end Generic_Breadth_First_Traversal;

  -----------------------------
  -- Breadth_First_Traversal --
  -----------------------------

  procedure Breadth_First_Traversal(T: in Tree; Handler: in Node_Handler) is
    procedure Traversal is new Generic_Breadth_First_Traversal(Handler.all);
  begin
    Traversal(T);
  end Breadth_First_Traversal;

end Trees;
