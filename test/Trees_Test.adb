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


-- @filename Trees_Test.adb
-- @author Sergio Gomez
-- @version 1.0
-- @date 06/04/2012
-- @revision 01/02/2016
-- @brief Test of Trees packages

with Ada.Text_IO; use Ada.Text_IO;
with Trees_Float; use Trees_Float;
with Trees_Float_IO; use Trees_Float_IO;
with Trees_String; use Trees_String;
with Trees_String_IO; use Trees_String_IO;
with Utils; use Utils;
with Utils.IO; use Utils.IO;

procedure Trees_Test is

  Fn_Text      : constant String  := "trees_test-text.txt";
  Fn_Newick    : constant String  := "trees_test-newick.txt";
  Fn_Json      : constant String  := "trees_test-json.txt";
  Fn_Html      : constant String  := "trees_test-json.html";
  Fn_Html_Head : constant String  := "trees_test-json-html_head.txt";
  Fn_Html_Tail : constant String  := "trees_test-json-html_tail.txt";

  use Trees_Float.Nodes_Lists;
  use Trees_String.Nodes_Lists;


  procedure Remove_Value(Nod: in Trees_String.Node) is
  begin
    Set_Value(Nod, Null_Ustring);
  end Remove_Value;

  procedure Print_Node(Nod: in Trees_String.Node) is
  begin
    if Is_Leaf(Nod) then
      Put_Line("Leaf " & U2S(Value(Nod)));
    else
      Put_Line("Internal " & U2S(Value(Nod)) & " with " & I2S(Number_Of_Children(Nod)) & " children");
    end if;
  end Print_Node;

  procedure Depth_First_Preorder is new Trees_String.Generic_Depth_First_Preorder_Traversal(Print_Node);
  procedure Depth_First_Postorder is new Trees_String.Generic_Depth_First_Postorder_Traversal(Print_Node);
  procedure Breadth_First is new Trees_String.Generic_Breadth_First_Traversal(Print_Node);

  Tf: Trees_Float.Tree;
  Nf, Nf2: Trees_Float.Node;

  Ts: Trees_String.Tree;
  Ns, Ns1, Ns2: Trees_String.Node;
  Ln: Trees_String.List_Of_Nodes;

begin

  Delete_File(Fn_Text);
  Delete_File(Fn_Newick);
  Delete_File(Fn_Json);

  Put_Line("Is Tree initially empty? " & Capitalize(Boolean'Image(Is_Empty(Ts))));
  New_Line;

  Ts := New_Tree(S2U("The_root"));
  Ns1 := Add_Child(Ts, S2U("A"));
  Ns2 := Add_Child(Ts, S2U("B"));
  Add_Child(Ns1, S2U("a"));
  Add_Child(Add_Child(Ns1, S2U("b")), S2U("k"));
  Add_Child(Ns1, S2U("c"));
  Add_Child(Ns2, S2U("x"));
  Add_Child(Ns2, S2U("y"));
  Add_Child(Ns2, S2U("z"));

  Put_Line("Tree:");
  Put_Tree(Ts);
  Put_Tree(Ts, Format => Newick_Tree);
  Put_Tree(Ts, Format => Json_Tree);
  Put_Tree(Fn_Text, Ts);
  Put_Tree(Fn_Newick, Ts, Format => Newick_Tree);
  Put_Tree(Fn_Json, Ts, Format => Json_Tree);
  New_Line;
  Free(Ts);

  Put_Line("Read Tree in Text Format:");
  Get_Tree(Fn_Text, Ts);
  Put_Tree(Ts);
  New_Line;
  Free(Ts);

  Put_Line("Read Tree in Newick Format:");
  Get_Tree(Fn_Newick, Ts);
  Put_Tree(Ts);
  New_Line;
  Free(Ts);

  Put_Line("Read Tree in JSON Format:");
  Get_Tree(Fn_Json, Ts);
  Put_Tree(Ts);
  New_Line;
  Free(Ts);

  Ts := New_Tree;
  Ns1 := Add_Child(Ts);
  Ns2 := Add_Child(Ts);
  Add_Child(Ns1);
  Add_Child(Add_Child(Ns1));
  Add_Child(Ns1);
  Add_Child(Ns2);
  Add_Child(Ns2);
  Add_Child(Ns2);

  Put_Line("Tree without values:");
  Put_Tree(Ts);
  Put_Tree(Ts, Format => Newick_Tree);
  Put_Tree(Ts, Format => Json_Tree);
  Put_Tree(Fn_Text, Ts);
  Put_Tree(Fn_Newick, Ts, Format => Newick_Tree);
  Put_Tree(Fn_Json, Ts, Format => Json_Tree);
  New_Line;
  Free(Ts);

  Put_Line("Read Tree in Text Format:");
  Get_Tree(Fn_Text, Ts);
  Put_Tree(Ts);
  New_Line;
  Free(Ts);

  Put_Line("Read Tree in Newick Format:");
  Get_Tree(Fn_Newick, Ts);
  Put_Tree(Ts);
  New_Line;
  Free(Ts);

  Delete_File(Fn_Text);
  Delete_File(Fn_Newick);
  Delete_File(Fn_Json);

  Tf := New_Tree(0.0);
  Add_Child(Tf, 1.0);
  Add_Child(Tf, 2.0);
  Nf := Add_Child(Tf, 3.0);
  Add_Child(Nf, 31.0);
  Add_Child(Nf, 32.0);
  Add_Child(Nf, 33.0);
  Add_Child(Tf, 4.0);
  Nf := Get_First(Get_Children(Tf));
  Add_Child(Nf, 11.0);
  Nf2 := Add_Child(Nf, 12.0);
  Add_Child(Nf, 13.0);
  Nf2 := Add_Child(Nf2, 121.0);
  Nf2 := Add_Child(Nf2, 1211.0);
  Nf := Add_Child(Nf2, 12111.0);
  Nf := Add_Child(Nf, 121111.0);
  Add_Child(Nf2, 1212.0);

  Put_Line("Tree of Floats:");
  Put_Tree(Tf, 6);
  New_Line;
  Put_Tree(Tf, 1);
  New_Line;
  Free(Tf);

  Ts := New_Tree(S2U("Tree"));
  Add_Child(Ts, S2U("Child1"));
  Add_Child(Ts, S2U("Child2"));
  Ns := Add_Child(Ts, S2U("Child3"));
  Add_Child(Ns, S2U("Ch31"));
  Add_Child(Ns, S2U("Ch32"));
  Add_Child(Ns, S2U("Ch33"));
  Add_Child(Ts, S2U("Child4"));
  Ns := Get_First(Get_Children(Ts));
  Add_Child(Ns, S2U("Ch11"));
  Ns2 := Add_Child(Ns, S2U("Ch12"));
  Add_Child(Ns, S2U("Ch13"));
  Ns2 := Add_Child(Ns2, S2U("Sub121"));
  Ns2 := Add_Child(Ns2, S2U("SubSub1211"));
  Ns := Add_Child(Ns2, S2U("Deep12111"));
  Add_Child(Ns, S2U("VeryDeep121111"));
  Add_Child(Ns2, S2U("Deep12112"));

  Put_Line("Tree of Strings:");
  Put_Tree(Ts);
  Put_Tree(Ts, Format => Newick_Tree);
  -- Put_Tree(Ts, Format => Json_Tree);
  Put_String_Line(Fn_Text, "# Tree of Strings");
  Put_String_Line(Fn_Text, "");
  Put_Tree(Fn_Text, Ts, Mode => Append_File);
  Put_Tree(Fn_Newick, Ts, Mode => Out_File, Format => Newick_Tree);
  Put_Tree(Fn_Json, Ts, Mode => Out_File, Format => Json_Tree);
  New_Line;
  Free(Ts);

  Put_Line("Build HTML from Tree in JSON Format");
  Copy_File(Fn_Html_Head, Fn_Html);
  Append_File(Fn_Json, Fn_Html);
  Append_File(Fn_Html_Tail, Fn_Html);
  New_Line;

  Put_Line("Read Tree in Text Format:");
  Get_Tree(Fn_Text, Ts);
  Put_Tree(Ts);
  New_Line;
  Free(Ts);

  Put_Line("Read Tree in Newick Format:");
  Get_Tree(Fn_Newick, Ts);
  Put_Tree(Ts);
  New_Line;

  Put_Line("Subtree:");
  Ln := Get_Children(Get_Root(Ts));
  Ns := Get(3, Ln);
  Extract_Subtree(Ns);
  Put_Tree(Ns);
  New_Line;

  Put_Line("Move Subtree:");
  Ln := Get_Children(Get_Root(Ts));
  Ns2 := Get_First(Ln);
  Ln := Get_Children(Ns2);
  Ns2 := Get_First(Ln);
  Add_Tree(Ns2, Ns);
  Put_Tree(Ts);
  New_Line;

  Put_Line("Counters:");
  Ns := Add_Child(Ts, S2U("New5"));
  Ts := Tree_Of(Ns);
  Ln := Get_Children(Ts);
  Ns := Get_First(Ln);
  Ts := Subtree_Of(Ns);
  Ln := Get_Children(Ts);
  Ns := Get(2, Ln);
  Ts := Subtree_Of(Ns);
  Ns := Add_Child(Ts, S2U("New6"));
  Ts := Tree_Of(Ns);
  Put_Tree(Ts);
  Put_Line("Number of nodes : " & I2S(Number_Of_Nodes(Ts)));
  Put_Line("Number of leaves: " & I2S(Number_Of_Leaves(Ts)));
  New_Line;

  Put_Line("Depth first preorder:");
  Put_Tree(Ts);
  Depth_First_Preorder(Ts);
  New_Line;

  Put_Line("Depth first postorder:");
  Put_Tree(Ts);
  Depth_First_Postorder(Ts);
  New_Line;

  Put_Line("Breadth first:");
  Put_Tree(Ts);
  Breadth_First(Ts);
  New_Line;

  Put_Line("Leaves:");
  Initialize(Ln);
  Get_Leaves(Ts, Ln);
  Save(Ln);
  Reset(Ln);
  while Has_Next(Ln) loop
    Print_Node(Next(Ln));
  end loop;
  Restore(Ln);
  Free(Ln);
  New_Line;

  Free(Ts);

end Trees_Test;
