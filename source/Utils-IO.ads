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


-- @filename Utils-IO.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/05/2005
-- @revision 30/01/2016
-- @brief Several IO Utils

with Ada.Containers.Ordered_Maps;

package Utils.IO is

  package Ustring_Maps is new Ada.Containers.Ordered_Maps(Ustring, Ustring);
  use Ustring_Maps;

  subtype Properties is Map;
  subtype Word_Access is String_Access;


  -- Purpose : Open a File for Append or Create
  --
  -- Ft      : The File
  -- Fn      : The File Name
  procedure Open_Or_Create(Ft: out File_Type; Fn: in String);

  -- Purpose : To know if a File with a given Name Exists
  -- Note    : False if Name is a Directory
  --
  -- Name    : The File Name
  -- return  : True if File Exists
  function File_Exists(Name: in String) return Boolean;

  -- Purpose : To know if a Directory with a given Name Exists
  -- Note    : False if Name is an Ordinary File
  --
  -- Name    : The Directory Name
  -- return  : True if Directory Exists
  function Directory_Exists(Name: in String) return Boolean;

  -- Purpose : Rename a File
  -- Note    : No exception is raised
  --
  -- Old_Fn  : The Old File Name
  -- New_Fn  : The New File Name
  procedure Rename_File(Old_Fn: in String; New_Fn: in String);

  -- Purpose : Rename a File
  -- Note    : No exception is raised
  --
  -- Old_Fn  : The Old File Name
  -- New_Fn  : The New File Name
  -- return  : True if Rename successful
  function Rename_File(Old_Fn: in String; New_Fn: in String) return Boolean;

  -- Purpose : Copy a File
  -- Note    : If Destination File exists, it is overwritten
  -- Note    : No exception is raised
  --
  -- Ori_Fn  : The Origin File Name
  -- Dest_Fn : The Destination File Name
  -- Fn      : The File Name
  procedure Copy_File(Ori_Fn: in String; Dest_Fn: in String);

  -- Purpose : Copy a File
  -- Note    : If Destination File exists, it is overwritten
  -- Note    : No exception is raised
  --
  -- Ori_Fn  : The Origin File Name
  -- Dest_Fn : The Destination File Name
  -- return  : True if Copy successful
  function Copy_File(Ori_Fn: in String; Dest_Fn: in String) return Boolean;

  -- Purpose : Append a File
  -- Note    : If Destination File does not exist, it is created
  -- Note    : No exception is raised
  --
  -- Ori_Fn  : The Origin File Name
  -- Dest_Fn : The Destination File Name
  -- Fn      : The File Name
  procedure Append_File(Ori_Fn: in String; Dest_Fn: in String);

  -- Purpose : Append a File
  -- Note    : If Destination File does not exist, it is created
  -- Note    : No exception is raised
  --
  -- Ori_Fn  : The Origin File Name
  -- Dest_Fn : The Destination File Name
  -- return  : True if Append successful
  function Append_File(Ori_Fn: in String; Dest_Fn: in String) return Boolean;

  -- Purpose : Delete a File
  -- Note    : No exception is raised
  --
  -- Fn      : The File Name
  procedure Delete_File(Fn: in String);

  -- Purpose : Delete a File
  -- Note    : No exception is raised
  --
  -- Fn      : The File Name
  -- return  : True if Deletion successful
  function Delete_File(Fn: in String) return Boolean;

  -- Purpose : File Path
  -- Note    : Path refers to the Containing Directories
  --
  -- Fn      : The File Name
  -- return  : The File Path
  function File_Path(Fn: in String) return String;

  -- Purpose : File Name
  -- Note    : Name includes Base Name, Extension separator and Extension
  --
  -- Fn      : The File Name
  -- return  : The File Name
  function File_Name(Fn: in String) return String;

  -- Purpose : File Base Name
  --
  -- Fn      : The File Name
  -- return  : The File Base Name
  function File_Base_Name(Fn: in String) return String;

  -- Purpose : File Extension
  --
  -- Fn      : The File Name
  -- return  : The File Extension
  function File_Extension(Fn: in String) return String;

  -- Purpose : Full File Name
  -- Note    : Name includes Base Name, Extension separator and Extension
  --
  -- Fn      : The File Name
  -- return  : The File Name
  function Full_File_Name(Fn: in String) return String;

  -- Purpose : Compose a full File Name from Path, Name and Extension
  --
  -- Path    : The Path
  -- Name    : The Name
  -- Ext     : The Extension
  -- return  : The full File Name
  function Compose_File_Name(Path, Name: in String; Ext: in String := "") return String;

  -- Purpose : Skip Spaces in a Line from Current Input
  -- Note    : Current Line is not skipped
  --
  procedure Line_Spaces_Skip;

  -- Purpose : Skip Spaces in a Line from a File
  -- Note    : Current Line is not skipped
  --
  -- Ft      : The File
  procedure Line_Spaces_Skip(Ft: in File_Type);

  -- Purpose : Skip Comment in a Line from Current Input
  -- Note    : Current Line is not skipped
  --
  procedure Line_Comment_Skip;

  -- Purpose : Skip Comment in a Line from a File
  -- Note    : Current Line is not skipped
  --
  -- Ft      : The File
  procedure Line_Comment_Skip(Ft: in File_Type);

  -- Purpose : Skip Comments from Current Input
  -- Note    : End of Lines and Spaces are skipped
  --
  procedure Comments_Skip;

  -- Purpose : Skip Comments from a File
  -- Note    : End of Lines and Spaces are skipped
  --
  -- Ft      : The File
  procedure Comments_Skip(Ft: in File_Type);

  -- Purpose : Skip Separator in a Line from Current Input
  -- Note    : Spaces before and after a Separator are Skipped
  -- Note    : In Strict mode, Sep is the only possible Separator
  -- Note    : Otherwise, Sep is included as an additional Separator Character
  -- Note    : and one or more Spaces are also considered as a Separator
  --
  -- Sep     : Additional Separator Character
  -- Strict  : True if Sep must be found
  -- return  : True if Separator found and skipped
  function Separator_Skip(Sep: in Character := ' '; Strict: in Boolean := False) return Boolean;

  -- Purpose : Skip Separator in a Line from a File
  -- Note    : Spaces before and after a Separator are Skipped
  -- Note    : In Strict mode, Sep is the only possible Separator
  -- Note    : Otherwise, Sep is included as an additional Separator Character
  -- Note    : and one or more Spaces are also considered as a Separator
  --
  -- Ft      : The File
  -- Sep     : Additional Separator Character
  -- Strict  : True if Sep must be found
  -- return  : True if Separator found and skipped
  function Separator_Skip(Ft: in File_Type; Sep: in Character := ' '; Strict: in Boolean := False) return Boolean;

  -- Purpose : Skip Separator in a Line from Current Input
  -- Note    : Spaces before and after a Separator are Skipped
  -- Note    : In Strict mode, Sep is the only possible Separator
  -- Note    : Otherwise, Sep is included as an additional Separator Character
  -- Note    : and one or more Spaces are also considered as a Separator
  --
  -- Sep     : Additional Separator Character
  -- Strict  : True if Sep must be found
  procedure Separator_Skip(Sep: in Character := ' '; Strict: in Boolean := False);

  -- Purpose : Skip Separator in a Line from a File
  -- Note    : Spaces before and after a Separator are Skipped
  -- Note    : In Strict mode, Sep is the only possible Separator
  -- Note    : Otherwise, Sep is included as an additional Separator Character
  -- Note    : and one or more Spaces are also considered as a Separator
  --
  -- Ft      : The File
  -- Sep     : Additional Separator Character
  -- Strict  : True if Sep must be found
  procedure Separator_Skip(Ft: in File_Type; Sep: in Character := ' '; Strict: in Boolean := False);

  -- Purpose : Deallocate the space used by a Word
  --
  -- W       : The Word
  procedure Free_Word(W: in out Word_Access);

  -- Purpose : Skip Word from Current Input
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Sep     : Additional Separator Character
  -- return  : True if Word found and skipped
  function Word_Skip(Sep: in Character := ' ') return Boolean;

  -- Purpose : Skip Word from a File
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- Sep     : Additional Separator Character
  -- return  : True if Word found and skipped
  function Word_Skip(Ft: in File_Type; Sep: in Character := ' ') return Boolean;

  -- Purpose : Skip Word from Current Input
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Sep     : Additional Separator Character
  procedure Word_Skip(Sep: in Character := ' ');

  -- Purpose : Skip Word from a File
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- Sep     : Additional Separator Character
  procedure Word_Skip(Ft: in File_Type; Sep: in Character := ' ');

  -- Purpose : Obtain next Word from Current Input
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Us      : The Word
  -- Sep     : Additional Separator Character
  procedure Get_Word(Us: out Ustring; Sep: in Character := ' ');

  -- Purpose : Obtain next Word from a File
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- Us      : The Word
  -- Sep     : Additional Separator Character
  procedure Get_Word(Ft: in File_Type; Us: out Ustring; Sep: in Character := ' ');

  -- Purpose : Obtain next Word from Current Input
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  -- Note    : The output Word should be deallocated using Free_Word
  --
  -- W       : The Word
  -- Sep     : Additional Separator Character
  procedure Get_Word(W: out Word_Access; Sep: in Character := ' ');

  -- Purpose : Obtain next Word from a File
  -- Note    : The boundaries of a Word may be Sep, spaces, separators, comments or end of lines
  -- Note    : The output Word should be deallocated using Free_Word
  --
  -- Ft      : The File
  -- W       : The Word
  -- Sep     : Additional Separator Character
  procedure Get_Word(Ft: in File_Type; W: out Word_Access; Sep: in Character := ' ');

  -- Purpose : Obtain next Quoted Word from Current Input
  -- Note    : The boundaries of a Quoted Word must be matching open and close quotes
  --
  -- Us      : The Quoted Word without the Quotes
  -- Forbid_Quote_Mark_Inside: True to forbid quote marks different to the closing one inside the word
  -- raises  : Quotes_Error
  procedure Get_Quoted_Word(Us: out Ustring; Forbid_Quote_Mark_Inside: in Boolean := False);

  -- Purpose : Obtain next Quoted Word from a File
  -- Note    : The boundaries of a Quoted Word must be matching open and close quotes
  --
  -- Ft      : The File
  -- Us      : The Quoted Word without the Quotes
  -- Forbid_Quote_Mark_Inside: True to forbid quote marks different to the closing one inside the word
  -- raises  : Quotes_Error
  procedure Get_Quoted_Word(Ft: in File_Type; Us: out Ustring; Forbid_Quote_Mark_Inside: in Boolean := False);

  -- Purpose : Obtain next Quoted Word from Current Input
  -- Note    : The boundaries of a Quoted Word must be matching open and close quotes
  -- Note    : The output Word should be deallocated using Free_Word
  --
  -- W       : The Quoted Word without the Quotes
  -- Forbid_Quote_Mark_Inside: True to forbid quote marks different to the closing one inside the word
  -- raises  : Quotes_Error
  procedure Get_Quoted_Word(W: out Word_Access; Forbid_Quote_Mark_Inside: in Boolean := False);

  -- Purpose : Obtain next Quoted Word from a File
  -- Note    : The boundaries of a Quoted Word must be matching open and close quotes
  -- Note    : The output Word should be deallocated using Free_Word
  --
  -- Ft      : The File
  -- W       : The Quoted Word without the Quotes
  -- Forbid_Quote_Mark_Inside: True to forbid quote marks different to the closing one inside the word
  -- raises  : Quotes_Error
  procedure Get_Quoted_Word(Ft: in File_Type; W: out Word_Access; Forbid_Quote_Mark_Inside: in Boolean := False);

  -- Purpose : Obtain next Pair Key/Value from Current Input
  -- Note    : Sep Character must be between Key and Value
  -- Note    : The boundaries of each Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Key     : The Key
  -- Value   : The Value
  -- Sep     : The Separator Character
  procedure Get_Pair(Key, Value: out Ustring; Sep: in Character := ' ');

  -- Purpose : Obtain next Pair Key/Value from a File
  -- Note    : Sep Character must be between Key and Value
  -- Note    : The boundaries of each Word may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- Key     : The Key
  -- Value   : The Value
  -- Sep     : The Separator Character
  procedure Get_Pair(Ft: in File_Type; Key, Value: out Ustring; Sep: in Character := ' ');

  -- Purpose : Obtain next Pair Key/Value from Current Input
  -- Note    : Sep Character must be between Key and Value
  -- Note    : The boundaries of each Word may be Sep, spaces, separators, comments or end of lines
  -- Note    : The output Key and Value should be deallocated using Free_Word
  --
  -- Key     : The Key
  -- Value   : The Value
  -- Sep     : The Separator Character
  procedure Get_Pair(Key, Value: out Word_Access; Sep: in Character := ' ');

  -- Purpose : Obtain next Pair Key/Value from a File
  -- Note    : Sep Character must be between Key and Value
  -- Note    : The boundaries of each Word may be Sep, spaces, separators, comments or end of lines
  -- Note    : The output Key and Value should be deallocated using Free_Word
  --
  -- Ft      : The File
  -- Key     : The Key
  -- Value   : The Value
  -- Sep     : The Separator Character
  procedure Get_Pair(Ft: in File_Type; Key, Value: out Word_Access; Sep: in Character := ' ');

  -- Purpose : Obtain next Integer from Current Input
  -- Note    : The boundaries of the Integer may be Sep, spaces, separators, comments or end of lines
  --
  -- I       : The Integer
  -- Sep     : Additional Separator Character
  procedure Get_Integer(I: out Integer; Sep: in Character := ' ');

  -- Purpose : Obtain next Integer from a File
  -- Note    : The boundaries of the Integer may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- I       : The Integer
  -- Sep     : Additional Separator Character
  procedure Get_Integer(Ft: in File_Type; I: out Integer; Sep: in Character := ' ');

  -- Purpose : Obtain next Longint from Current Input
  -- Note    : The boundaries of the Longint may be Sep, spaces, separators, comments or end of lines
  --
  -- L       : The Longint
  -- Sep     : Additional Separator Character
  procedure Get_Longint(L: out Longint; Sep: in Character := ' ');

  -- Purpose : Obtain next Longint from a File
  -- Note    : The boundaries of the Longint may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- L       : The Longint
  -- Sep     : Additional Separator Character
  procedure Get_Longint(Ft: in File_Type; L: out Longint; Sep: in Character := ' ');

  -- Purpose : Obtain next Float from Current Input
  -- Note    : The boundaries of the Float may be Sep, spaces, separators, comments or end of lines
  --
  -- X       : The Float
  -- Sep     : Additional Separator Character
  procedure Get_Float(X: out Float; Sep: in Character := ' ');

  -- Purpose : Obtain next Float from a File
  -- Note    : The boundaries of the Float may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- X       : The Float
  -- Sep     : Additional Separator Character
  procedure Get_Float(Ft: in File_Type; X: out Float; Sep: in Character := ' ');

  -- Purpose : Obtain next Double from Current Input
  -- Note    : The boundaries of the Float may be Sep, spaces, separators, comments or end of lines
  --
  -- X       : The Double
  -- Sep     : Additional Separator Character
  procedure Get_Double(X: out Double; Sep: in Character := ' ');

  -- Purpose : Obtain next Double from a File
  -- Note    : The boundaries of the Double may be Sep, spaces, separators, comments or end of lines
  --
  -- Ft      : The File
  -- X       : The Double
  -- Sep     : Additional Separator Character
  procedure Get_Double(Ft: in File_Type; X: out Double; Sep: in Character := ' ');

  -- Purpose : Append a String Line to a File
  -- Note    : File created if it does not exist
  --
  -- Fn      : The File Name
  -- S       : The String Line
  procedure Put_String_Line(Fn: in String; S: in String);

  -- Purpose : Print a Right Justified Word to Current Output
  --
  -- Us      : The Word
  -- Width   : The minimum Width
  procedure Put_Word(Us: in Ustring; Width: in Natural := Default_String_Width);

  -- Purpose : Append a Right Justified Word to a File
  -- Note    : File created if it does not exist
  --
  -- Fn      : The File Name
  -- Us      : The Word
  -- Width   : The minimum Width
  procedure Put_Word(Fn: in String; Us: in Ustring; Width: in Natural := Default_String_Width);

  -- Purpose : Print a Right Justified Word to a File
  --
  -- Ft      : The File
  -- Us      : The Word
  -- Width   : The minimum Width
  procedure Put_Word(Ft: in File_Type; Us: in Ustring; Width: in Natural := Default_String_Width);

  -- Purpose : Print a formatted Integer to Current Output
  --
  -- I       : The Integer
  -- Width   : The minimum Width
  procedure Put_Integer(I: in Integer; Width: in Field := Default_Integer_Width);

  -- Purpose : Append a formatted Integer to a File
  -- Note    : File created if it does not exist
  --
  -- Fn      : The File Name
  -- I       : The Integer
  -- Width   : The minimum Width
  procedure Put_Integer(Fn: in String; I: in Integer; Width: in Field := Default_Integer_Width);

  -- Purpose : Print a formatted Integer to a File
  --
  -- Ft      : The File
  -- I       : The Integer
  -- Width   : The minimum Width
  procedure Put_Integer(Ft: in File_Type; I: in Integer; Width: in Field := Default_Integer_Width);

  -- Purpose : Print a formatted Longint to Current Output
  --
  -- L       : The Longint
  -- Width   : The minimum Width
  procedure Put_Longint(L: in Longint; Width: in Field := Default_Longint_Width);

  -- Purpose : Append a formatted Longint to a File
  -- Note    : File created if it does not exist
  --
  -- Fn      : The File Name
  -- L       : The Longint
  -- Width   : The minimum Width
  procedure Put_Longint(Fn: in String; L: in Longint; Width: in Field := Default_Longint_Width);

  -- Purpose : Print a formatted Longint to a File
  --
  -- Ft      : The File
  -- L       : The Longint
  -- Width   : The minimum Width
  procedure Put_Longint(Ft: in File_Type; L: in Longint; Width: in Field := Default_Longint_Width);

  -- Purpose : Print a formatted Float to Current Output
  -- Note    : If Aft is zero an integer string is returned
  --
  -- X       : The Float
  -- Aft     : The number of digits after the decimal point
  procedure Put_Float(X: in Float; Aft: in Field := Default_Float_Aft);

  -- Purpose : Append a formatted Float to a File
  -- Note    : File created if it does not exist
  -- Note    : If Aft is zero an integer string is returned
  --
  -- Fn      : The File Name
  -- X       : The Float
  -- Aft     : The number of digits after the decimal point
  procedure Put_Float(Fn: in String; X: in Float; Aft: in Field := Default_Float_Aft);

  -- Purpose : Print a formatted Float to a File
  -- Note    : If Aft is zero an integer string is returned
  --
  -- Ft      : The File
  -- X       : The Float
  -- Aft     : The number of digits after the decimal point
  procedure Put_Float(Ft: in File_Type; X: in Float; Aft: in Field := Default_Float_Aft);

  -- Purpose : Print a formatted Double to Current Output
  -- Note    : If Aft is zero an integer string is returned
  --
  -- X       : The Double
  -- Aft     : The number of digits after the decimal point
  procedure Put_Double(X: in Double; Aft: in Field := Default_Double_Aft);

  -- Purpose : Append a formatted Double to a File
  -- Note    : File created if it does not exist
  -- Note    : If Aft is zero an integer string is returned
  --
  -- Fn      : The File Name
  -- X       : The Double
  -- Aft     : The number of digits after the decimal point
  procedure Put_Double(Fn: in String; X: in Double; Aft: in Field := Default_Double_Aft);

  -- Purpose : Print a formatted Double to a File
  -- Note    : If Aft is zero an integer string is returned
  --
  -- Ft      : The File
  -- X       : The Double
  -- Aft     : The number of digits after the decimal point
  procedure Put_Double(Ft: in File_Type; X: in Double; Aft: in Field := Default_Double_Aft);

  -- Purpose : Print a percentage both in Natural and Float formats to Current Output
  --
  -- Num     : The number of items
  -- Total   : The total number of items
  -- Aft     : The number of digits after the decimal point
  procedure Put_Percentage(Num: in Natural; Total: in Positive; Aft: in Field := Default_Float_Aft);

  -- Purpose : Append a percentage both in Natural and Float formats to a File
  -- Note    : File created if it does not exist
  --
  -- Fn      : The File Name
  -- Num     : The number of items
  -- Total   : The total number of items
  -- Aft     : The number of digits after the decimal point
  procedure Put_Percentage(Fn: in String; Num: in Natural; Total: in Positive; Aft: in Field := Default_Float_Aft);

  -- Purpose : Print a percentage both in Natural and Float formats to a File
  --
  -- Ft      : The File
  -- Num     : The number of items
  -- Total   : The total number of items
  -- Aft     : The number of digits after the decimal point
  procedure Put_Percentage(Ft: in File_Type; Num: in Natural; Total: in Positive; Aft: in Field := Default_Float_Aft);

  -- Purpose : Put a formatted Duration to Standard Output
  --
  -- D       : The Duration
  -- Aft     : The number of digits after the decimal point
  procedure Put_Duration(D: in Duration; Aft: in Field := Default_Float_Aft);

  -- Purpose : Append a formatted Duration to a File
  --
  -- Fn      : The File Name
  -- D       : The Duration
  -- Aft     : The number of digits after the decimal point
  procedure Put_Duration(Fn: in String; D: in Duration; Aft: in Field := Default_Float_Aft);

  -- Purpose : Put a formatted Duration to a File
  --
  -- Ft      : The File
  -- D       : The Duration
  -- Aft     : The number of digits after the decimal point
  procedure Put_Duration(Ft: in File_Type; D: in Duration; Aft: in Field := Default_Float_Aft);

  -- Purpose : Load a Properties file
  -- Note    : Standard separator between Names and Values is '=' but all separators are allowed
  -- Note    : Both sides of Names and Values are trimmed
  -- Note    : No exception is raised
  --
  -- Fn      : The File Name
  -- Props   : The Properties
  procedure Load_Properties(Fn: in String; Props: out Properties);

  -- Purpose : Save Properties to file
  --
  -- Fn      : The File Name
  -- Props   : The Properties
  procedure Save_Properties(Fn: in String; Props: in Properties);

  -- Purpose : Get the Value of a Property
  -- Note    : Default Property Value is returned if Property does not exists
  -- Note    : Both sides of Name and Default Value are trimmed
  -- Note    : Name and Default Value are case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Ustring := Null_Ustring) return Ustring;

  -- Purpose : Get the Value of a Property
  -- Note    : Default Property Value is returned if Property does not exists
  -- Note    : Both sides of Name and Default Value are trimmed
  -- Note    : Name and Default Value are case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in String := "") return Ustring;

  -- Purpose : Get the Value of a Property
  -- Note    : Default Property Value is returned if Property does not exists
  -- Note    : Both sides of Name and Default Value are trimmed
  -- Note    : Name and Default Value are case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in String := "") return String;

  -- Purpose : Get the Value of a Boolean Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Boolean
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Boolean := False) return Boolean;

  -- Purpose : Get the Value of a Boolean Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Boolean
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Boolean := False) return Boolean;

  -- Purpose : Get the Value of an Integer Property
  -- Note    : Default Property Value is returned if Property does not exists or is not an Integer
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Integer := 0) return Integer;

  -- Purpose : Get the Value of an Integer Property
  -- Note    : Default Property Value is returned if Property does not exists or is not an Integer
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Integer := 0) return Integer;

  -- Purpose : Get the Value of a Longint Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Longint
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Longint := 0) return Longint;

  -- Purpose : Get the Value of a Longint Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Longint
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Longint := 0) return Longint;

  -- Purpose : Get the Value of a Float Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Float
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Float := 0.0) return Float;

  -- Purpose : Get the Value of a Float Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Float
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Float := 0.0) return Float;

  -- Purpose : Get the Value of a Double Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Double
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in Ustring; Def_Val: in Double := 0.0) return Double;

  -- Purpose : Get the Value of a Double Property
  -- Note    : Default Property Value is returned if Property does not exists or is not a Double
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Def_Val : Default Value of the Property
  -- return  : The Value of the Property
  function Get_Property(Props: in Properties; Name: in String; Def_Val: in Double := 0.0) return Double;

  -- Purpose : Set the Value of a Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name and Value are trimmed
  -- Note    : Name and Value are case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name, Value: in Ustring);

  -- Purpose : Set the Value of a Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name and Value are trimmed
  -- Note    : Name and Value are case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name, Value: in String);

  -- Purpose : Set the Value of a Boolean Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Boolean);

  -- Purpose : Set the Value of a Boolean Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Boolean);

  -- Purpose : Set the Value of an Integer Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Integer);

  -- Purpose : Set the Value of an Integer Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Integer);

  -- Purpose : Set the Value of a Longint Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Longint);

  -- Purpose : Set the Value of a Longint Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Longint);

  -- Purpose : Set the Value of a Float Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The number of digits in the exponent
  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp);

  -- Purpose : Set the Value of a Float Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The number of digits in the exponent
  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp);

  -- Purpose : Set the Value of a Double Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The number of digits in the exponent
  procedure Set_Property(Props: in out Properties; Name: in Ustring; Value: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp);

  -- Purpose : Set the Value of a Double Property
  -- Note    : If the Property already exists, its Value is replaced with the new given Value
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  -- Value   : The Value of the Property
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The number of digits in the exponent
  procedure Set_Property(Props: in out Properties; Name: in String; Value: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp);

  -- Purpose : Remove a Property
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  procedure Remove_Property(Props: in out Properties; Name: in Ustring);

  -- Purpose : Remove a Property
  -- Note    : Both sides of Name are trimmed
  -- Note    : Name is case-sensitive
  -- Note    : No exception is raised
  --
  -- Props   : The Properties
  -- Name    : The Name of the Property
  procedure Remove_Property(Props: in out Properties; Name: in String);

  -- Purpose : Get the Number of Properties
  --
  -- Props   : The Properties
  -- return  : The Number of Properties
  function Number_Of_Properties(Props: in Properties) return Natural;

  -- Purpose : Get an array with all the Names of the Properties
  --
  -- Props   : The Properties
  -- return  : The Names of the Properties
  function Get_Property_Names(Props: in Properties) return PUstrings;

  -- Purpose : Get an array with all the Values of the Properties
  --
  -- Props   : The Properties
  -- return  : The Values of the Properties
  function Get_Property_Values(Props: in Properties) return PUstrings;

end Utils.IO;
