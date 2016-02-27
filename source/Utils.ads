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


-- @filename Utils.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 11/05/2005
-- @revision 19/02/2016
-- @brief Several Utils

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Utils_Generics; use Utils_Generics;
with Arrays_Float;
with Arrays_Double;

package Utils is

  -- Basic types

  subtype Double is Long_Float;
  subtype Longint is Long_Long_Integer;
  subtype Ustring is Unbounded_String;

  Null_Ustring: constant Ustring := Null_Unbounded_String;

  -- Types Id

  type Basic_Type_Id is (Integer_Id, Longint_Id, Float_Id, Double_Id, Boolean_Id, Character_Id, String_Id, Ustring_Id);
  subtype Numeric_Type_Id is Basic_Type_Id range Integer_Id..Double_Id;

  Unknown_Type_Id_Error: exception;

  -- One-dimensional arrays

  type Booleans is array (Integer range <>) of Boolean;
  type PBooleans is access Booleans;

  type Characters is array(Integer range <>) of Character;
  type PCharacters is access Characters;

  type Integers is array(Integer range <>) of Integer;
  type PIntegers is access Integers;

  type Longints is array(Integer range <>) of Longint;
  type PLongints is access Longints;

--  type Floats is array(Integer range <>) of Float;
  subtype Floats is Arrays_Float.Real_Vector;
  type PFloats is access Floats;

--  type Doubles is array(Integer range <>) of Double;
  subtype Doubles is Arrays_Double.Real_Vector;
  type PDoubles is access Doubles;

  type Ustrings is array(Integer range <>) of Ustring;
  type PUstrings is access Ustrings;


  -- Two-dimensional arrays

  type Booleanss is array(Integer range <>, Integer range <>) of Boolean;
  type PBooleanss is access Booleanss;

  type Characterss is array(Integer range <>, Integer range <>) of Character;
  type PCharacterss is access Characterss;

  type Integerss is array(Integer range <>, Integer range <>) of Integer;
  type PIntegerss is access Integerss;

  type Longintss is array(Integer range <>, Integer range <>) of Longint;
  type PLongintss is access Longintss;

--  type Floatss is array(Integer range <>, Integer range <>) of Float;
  subtype Floatss is Arrays_Float.Real_Matrix;
  type PFloatss is access Floatss;

--  type Doubless is array(Integer range <>, Integer range <>) of Double;
  subtype Doubless is Arrays_Double.Real_Matrix;
  type PDoubless is access Doubless;

  type Ustringss is array(Integer range <>, Integer range <>) of Ustring;
  type PUstringss is access Ustringss;


  -- Irregular arrays

  type PsBooleans is array (Integer range <>) of PBooleans;
  type PPsBooleans is access PsBooleans;

  type PsCharacters is array(Integer range <>) of PCharacters;
  type PPsCharacters is access PsCharacters;

  type PsIntegers is array(Integer range <>) of PIntegers;
  type PPsIntegers is access PsIntegers;

  type PsLongints is array(Integer range <>) of PLongints;
  type PPsLongints is access PsLongints;

  type PsFloats is array(Integer range <>) of PFloats;
  type PPsFloats is access PsFloats;

  type PsDoubles is array(Integer range <>) of PDoubles;
  type PPsDoubles is access PsDoubles;

  type PsUstrings is array(Integer range <>) of PUstrings;
  type PPsUstrings is access PsUstrings;


  -- Constants

  HTab  : constant Character := HT;
  NLine : constant Character := LF;

  Default_Round_Decimals: constant Natural := 0;

  Default_String_Width : constant Natural := 0;
  Default_Integer_Width: constant Field   := 0;
  Default_Longint_Width: constant Field   := 0;

  Default_Float_Aft: constant Field := Ada.Float_Text_IO.Default_Aft;
  Default_Float_Exp: constant Field := Ada.Float_Text_IO.Default_Exp;

  Default_Double_Aft: constant Field := Ada.Long_Float_Text_IO.Default_Aft;
  Default_Double_Exp: constant Field := Ada.Long_Float_Text_IO.Default_Exp;

  Standard_Spaces      : constant Characters(1..6) := (' ', HT, VT, FF, LF, CR);
  Standard_Comments    : constant Characters(1..1) := (1 => '#');
  Standard_Separators  : constant Characters(1..4) := (',', ';', ':', '|');
  Standard_Quotes_Open : constant Characters(1..1) := (1 => '"');
  Standard_Quotes_Close: constant Characters(1..1) := (1 => '"');

  Key_Value_Pair_Error: exception;
  Quotes_Error: exception;


  -- Purpose : Obtain a Type Id from its Name
  -- Note    : Possible Names (case-insensitive):
  -- Note    :    I | Integer   | Int
  -- Note    :    L | Longint   | Long
  -- Note    :    F | Float
  -- Note    :    D | Double
  -- Note    :    B | Boolean   | Bool
  -- Note    :    C | Character | Char
  -- Note    :    S | String    | Str
  -- Note    :    U | Ustring   | Ustr
  --
  -- Ht_Name : The Type Id Name
  -- return  : The Type Id
  -- raises  : Unknown_Type_Id_Error
  function To_Type_Id(S: in String) return Basic_Type_Id;

  -- Purpose : Return the Rounding a Float
  -- Note    : Cannot handle properly values above Floats resolution
  --
  -- F       : The Float
  -- return  : The Rounding
  function Round(F: in Float) return Integer;
  pragma Inline(Round);

  -- Purpose : Return the Rounding a Double
  -- Note    : Cannot handle properly values out of Integers range
  --
  -- D       : The Double
  -- return  : The Rounding
  function Round(D: in Double) return Integer;
  pragma Inline(Round);

  -- Purpose : Return the Rounding a Float
  -- Note    : Cannot handle properly values above Floats resolution
  --
  -- F       : The Float
  -- return  : The Rounding
  function Round(F: in Float; Decimals: in Natural := Default_Round_Decimals) return Float;
  pragma Inline(Round);

  -- Purpose : Return the Rounding a Double
  -- Note    : Cannot handle properly values out of Integers range
  --
  -- D       : The Double
  -- return  : The Rounding
  function Round(D: in Double; Decimals: in Natural := Default_Round_Decimals) return Double;
  pragma Inline(Round);

  -- Purpose : Return the Floor of a Float
  -- Note    : Cannot handle properly values above Floats resolution
  --
  -- F       : The Float
  -- return  : The Floor
  function Floor(F: in Float) return Float;

  -- Purpose : Return the Floor of a Float
  -- Note    : Cannot handle properly values above Floats resolution
  --
  -- F       : The Float
  -- return  : The Floor
  function Floor(F: in Float) return Integer;

  -- Purpose : Return the Floor of a Double
  -- Note    : Cannot handle properly values out of Integers range
  --
  -- D       : The Double
  -- return  : The Floor
  function Floor(D: in Double) return Double;

  -- Purpose : Return the Floor of a Double
  -- Note    : Cannot handle properly values out of Integers range
  --
  -- D       : The Double
  -- return  : The Floor
  function Floor(D: in Double) return Integer;

  -- Purpose : Return the Ceiling of a Float
  -- Note    : Cannot handle properly values above Floats resolution
  --
  -- F       : The Float
  -- return  : The Ceiling
  function Ceiling(F: in Float) return Float;

  -- Purpose : Return the Ceiling of a Float
  -- Note    : Cannot handle properly values above Floats resolution
  --
  -- F       : The Float
  -- return  : The Ceiling
  function Ceiling(F: in Float) return Integer;

  -- Purpose : Return the Ceiling of a Double
  -- Note    : Cannot handle properly values out of Integers range
  --
  -- D       : The Double
  -- return  : The Ceiling
  function Ceiling(D: in Double) return Double;

  -- Purpose : Return the Ceiling of a Double
  -- Note    : Cannot handle properly values out of Integers range
  --
  -- D       : The Double
  -- return  : The Ceiling
  function Ceiling(D: in Double) return Integer;

  -- Purpose : Return the Minimum of two Integer
  --
  -- Left    : The Left Integer
  -- Right   : The Right Integer
  -- return  : The Minimum Integer
  function Min(Left, Right: in Integer) return Integer;

  -- Purpose : Return the Minimum of two Longint
  --
  -- Left    : The Left Longint
  -- Right   : The Right Longint
  -- return  : The Minimum Longint
  function Min(Left, Right: in Longint) return Longint;

  -- Purpose : Return the Minimum of two Float
  --
  -- Left    : The Left Float
  -- Right   : The Right Float
  -- return  : The Minimum Float
  function Min(Left, Right: in Float) return Float;

  -- Purpose : Return the Minimum of two Double
  --
  -- Left    : The Left Double
  -- Right   : The Right Double
  -- return  : The Minimum Double
  function Min(Left, Right: in Double) return Double;

  -- Purpose : Return the Maximum of two Integer
  --
  -- Left    : The Left Integer
  -- Right   : The Right Integer
  -- return  : The Maximum Integer
  function Max(Left, Right: in Integer) return Integer;

  -- Purpose : Return the Maximum of two Longint
  --
  -- Left    : The Left Longint
  -- Right   : The Right Longint
  -- return  : The Minimum Longint
  function Max(Left, Right: in Longint) return Longint;

  -- Purpose : Return the Maximum of two Float
  --
  -- Left    : The Left Float
  -- Right   : The Right Float
  -- return  : The Maximum Float
  function Max(Left, Right: in Float) return Float;

  -- Purpose : Return the Maximum of two Double
  --
  -- Left    : The Left Double
  -- Right   : The Right Double
  -- return  : The Maximum Double
  function Max(Left, Right: in Double) return Double;

  -- Purpose : Allocate one-dimensional arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  function Alloc is new Alloc_1D_Array(Boolean  , Booleans  , PBooleans);
  function Alloc is new Alloc_1D_Array(Character, Characters, PCharacters);
  function Alloc is new Alloc_1D_Array(Integer  , Integers  , PIntegers);
  function Alloc is new Alloc_1D_Array(Longint  , Longints  , PLongints);
  function Alloc is new Alloc_1D_Array(Float    , Floats    , PFloats);
  function Alloc is new Alloc_1D_Array(Double   , Doubles   , PDoubles);
  function Alloc is new Alloc_1D_Array(Ustring  , Ustrings  , PUstrings);

  -- Purpose : Allocate two-dimensional squared arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  function Alloc is new Alloc_2D_Squared_Array(Boolean  , Booleanss  , PBooleanss);
  function Alloc is new Alloc_2D_Squared_Array(Character, Characterss, PCharacterss);
  function Alloc is new Alloc_2D_Squared_Array(Integer  , Integerss  , PIntegerss);
  function Alloc is new Alloc_2D_Squared_Array(Longint  , Longintss  , PLongintss);
  function Alloc is new Alloc_2D_Squared_Array(Float    , Floatss    , PFloatss);
  function Alloc is new Alloc_2D_Squared_Array(Double   , Doubless   , PDoubless);
  function Alloc is new Alloc_2D_Squared_Array(Ustring  , Ustringss  , PUstringss);

  -- Purpose : Allocate two-dimensional rectangular arrays
  --
  -- First1  : The First index of first dimension
  -- Last1   : The Last index of first dimension
  -- First2  : The First index of second dimension
  -- Last2   : The Last index of second dimension
  -- return  : The access to the array
  function Alloc is new Alloc_2D_Rectangular_Array(Boolean  , Booleanss  , PBooleanss);
  function Alloc is new Alloc_2D_Rectangular_Array(Character, Characterss, PCharacterss);
  function Alloc is new Alloc_2D_Rectangular_Array(Integer  , Integerss  , PIntegerss);
  function Alloc is new Alloc_2D_Rectangular_Array(Longint  , Longintss  , PLongintss);
  function Alloc is new Alloc_2D_Rectangular_Array(Float    , Floatss    , PFloatss);
  function Alloc is new Alloc_2D_Rectangular_Array(Double   , Doubless   , PDoubless);
  function Alloc is new Alloc_2D_Rectangular_Array(Ustring  , Ustringss  , PUstringss);

  -- Purpose : Allocate first step of two-dimensional irregular arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  function Alloc is new Alloc_1D_Array(PBooleans  , PsBooleans  , PPsBooleans);
  function Alloc is new Alloc_1D_Array(PCharacters, PsCharacters, PPsCharacters);
  function Alloc is new Alloc_1D_Array(PIntegers  , PsIntegers  , PPsIntegers);
  function Alloc is new Alloc_1D_Array(PLongints  , PsLongints  , PPsLongints);
  function Alloc is new Alloc_1D_Array(PFloats    , PsFloats    , PPsFloats);
  function Alloc is new Alloc_1D_Array(PDoubles   , PsDoubles   , PPsDoubles);
  function Alloc is new Alloc_1D_Array(PUstrings  , PsUstrings  , PPsUstrings);

  -- Purpose : Allocate irregular squared arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Boolean  , Booleans  , PBooleans  , PsBooleans  , PPsBooleans);
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Character, Characters, PCharacters, PsCharacters, PPsCharacters);
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Integer  , Integers  , PIntegers  , PsIntegers  , PPsIntegers);
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Longint  , Longints  , PLongints  , PsLongints  , PPsLongints);
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Float    , Floats    , PFloats    , PsFloats    , PPsFloats);
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Double   , Doubles   , PDoubles   , PsDoubles   , PPsDoubles);
  function Alloc_Squared is new Alloc_Irregular_Squared_Array(Ustring  , Ustrings  , PUstrings  , PsUstrings  , PPsUstrings);

  -- Purpose : Allocate irregular rectangular arrays
  --
  -- First1  : The First index of first dimension
  -- Last1   : The Last index of first dimension
  -- First2  : The First index of second dimension
  -- Last2   : The Last index of second dimension
  -- return  : The access to the array
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Boolean  , Booleans  , PBooleans  , PsBooleans  , PPsBooleans);
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Character, Characters, PCharacters, PsCharacters, PPsCharacters);
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Integer  , Integers  , PIntegers  , PsIntegers  , PPsIntegers);
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Longint  , Longints  , PLongints  , PsLongints  , PPsLongints);
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Float    , Floats    , PFloats    , PsFloats    , PPsFloats);
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Double   , Doubles   , PDoubles   , PsDoubles   , PPsDoubles);
  function Alloc_Rectangular is new Alloc_Irregular_Rectangular_Array(Ustring  , Ustrings  , PUstrings  , PsUstrings  , PPsUstrings);

  -- Purpose : Allocate upper triangular arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Boolean  , Booleans  , PBooleans  , PsBooleans  , PPsBooleans);
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Character, Characters, PCharacters, PsCharacters, PPsCharacters);
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Integer  , Integers  , PIntegers  , PsIntegers  , PPsIntegers);
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Longint  , Longints  , PLongints  , PsLongints  , PPsLongints);
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Float    , Floats    , PFloats    , PsFloats    , PPsFloats);
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Double   , Doubles   , PDoubles   , PsDoubles   , PPsDoubles);
  function Alloc_Upper is new Alloc_Upper_Triangular_Array(Ustring  , Ustrings  , PUstrings  , PsUstrings  , PPsUstrings);

  -- Purpose : Allocate lower triangular arrays
  --
  -- First   : The First index
  -- Last    : The Last index
  -- return  : The access to the array
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Boolean  , Booleans  , PBooleans  , PsBooleans  , PPsBooleans);
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Character, Characters, PCharacters, PsCharacters, PPsCharacters);
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Integer  , Integers  , PIntegers  , PsIntegers  , PPsIntegers);
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Longint  , Longints  , PLongints  , PsLongints  , PPsLongints);
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Float    , Floats    , PFloats    , PsFloats    , PPsFloats);
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Double   , Doubles   , PDoubles   , PsDoubles   , PPsDoubles);
  function Alloc_Lower is new Alloc_Lower_Triangular_Array(Ustring  , Ustrings  , PUstrings  , PsUstrings  , PPsUstrings);

  -- Purpose : Deallocate the space used by one-dimensional arrays
  --
  -- P       : The access to the array
  procedure Free is new Free_1D_Array(Boolean  , Booleans  , PBooleans);
  procedure Free is new Free_1D_Array(Character, Characters, PCharacters);
  procedure Free is new Free_1D_Array(Integer  , Integers  , PIntegers);
  procedure Free is new Free_1D_Array(Longint  , Longints  , PLongints);
  procedure Free is new Free_1D_Array(Float    , Floats    , PFloats);
  procedure Free is new Free_1D_Array(Double   , Doubles   , PDoubles);
  procedure Free is new Free_1D_Array(Ustring  , Ustrings  , PUstrings);

  -- Purpose : Deallocate the space used by two-dimensional arrays
  --
  -- P       : The access to the array
  procedure Free is new Free_2D_Array(Boolean  , Booleanss  , PBooleanss);
  procedure Free is new Free_2D_Array(Character, Characterss, PCharacterss);
  procedure Free is new Free_2D_Array(Integer  , Integerss  , PIntegerss);
  procedure Free is new Free_2D_Array(Longint  , Longintss  , PLongintss);
  procedure Free is new Free_2D_Array(Float    , Floatss    , PFloatss);
  procedure Free is new Free_2D_Array(Double   , Doubless   , PDoubless);
  procedure Free is new Free_2D_Array(Ustring  , Ustringss  , PUstringss);

  -- Purpose : Deallocate the space used by two-dimensional irregular arrays
  --
  -- P       : The access to the array
  procedure Free is new Free_2D_Irregular_Array(Boolean  , Booleans  , PBooleans  , PsBooleans  , PPsBooleans);
  procedure Free is new Free_2D_Irregular_Array(Character, Characters, PCharacters, PsCharacters, PPsCharacters);
  procedure Free is new Free_2D_Irregular_Array(Integer  , Integers  , PIntegers  , PsIntegers  , PPsIntegers);
  procedure Free is new Free_2D_Irregular_Array(Longint  , Longints  , PLongints  , PsLongints  , PPsLongints);
  procedure Free is new Free_2D_Irregular_Array(Float    , Floats    , PFloats    , PsFloats    , PPsFloats);
  procedure Free is new Free_2D_Irregular_Array(Double   , Doubles   , PDoubles   , PsDoubles   , PPsDoubles);
  procedure Free is new Free_2D_Irregular_Array(Ustring  , Ustrings  , PUstrings  , PsUstrings  , PPsUstrings);

  -- Purpose : Check if a String exactly represents an Integer value, either Integer or Longint
  -- Note    : It is not considered an Integer if only a prefix of it is an Integer
  --
  -- S       : The String
  -- return  : True if represents an Integer value
  function Is_Integer(S: in String) return Boolean;

  -- Purpose : Check if a String exactly represents an Integer value, either Integer or Longint
  -- Note    : It is not considered an Integer if only a prefix of it is an Integer
  --
  -- U       : The String
  -- return  : True if represents an Integer value
  function Is_Integer(U: in Ustring) return Boolean;

  -- Purpose : Check if a String exactly represents a Real value, either Float or Double
  -- Note    : It is not considered a Real if only a prefix of it is a Real
  -- Note    : Integers are also considered as Reals
  --
  -- S       : The String
  -- return  : True if represents a Real value
  function Is_Real(S: in String) return Boolean;

  -- Purpose : Check if a String exactly represents a Real value, either Float or Double
  -- Note    : It is not considered a Real if only a prefix of it is a Real
  -- Note    : Integers are also considered as Reals
  --
  -- U       : The String
  -- return  : True if represents a Real value
  function Is_Real(U: in Ustring) return Boolean;

  -- Purpose : Convert a Character to an Integer
  --
  -- C       : The Character
  -- return  : The Integer
  function To_Integer(C: in Character) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert a String to an Integer
  --
  -- S       : The String
  -- return  : The Integer
  function To_Integer(S: in String) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert a Ustring to an Integer
  --
  -- U       : The Ustring
  -- return  : The Integer
  function To_Integer(U: in Ustring) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert an Integer to an Integer
  --
  -- I       : The Integer
  -- return  : The Integer
  function To_Integer(I: in Integer) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert a Longint to an Integer
  --
  -- L       : The Longint
  -- return  : The Integer
  function To_Integer(L: in Longint) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert a Float to an Integer
  -- Note    : The Float is rounded to its nearest Integer
  --
  -- F       : The Float
  -- return  : The Integer
  function To_Integer(F: in Float) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert an Double to an Integer
  -- Note    : The Double is rounded to its nearest Integer
  --
  -- D       : The Double
  -- return  : The Integer
  function To_Integer(D: in Double) return Integer;
  pragma Inline(To_Integer);

  -- Purpose : Convert a Character to a Longint
  --
  -- C       : The Character
  -- return  : The Longint
  function To_Longint(C: in Character) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert a String to a Longint
  --
  -- S       : The String
  -- return  : The Longint
  function To_Longint(S: in String) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert a Ustring to a Longint
  --
  -- U       : The Ustring
  -- return  : The Longint
  function To_Longint(U: in Ustring) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert a Integer to a Longint
  --
  -- I       : The Integer
  -- return  : The Longint
  function To_Longint(I: in Integer) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert an Longint to a Longint
  --
  -- L       : The Longint
  -- return  : The Longint
  function To_Longint(L: in Longint) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert a Float to a Longint
  -- Note    : The Float is rounded to its nearest Longint
  --
  -- F       : The Float
  -- return  : The Longint
  function To_Longint(F: in Float) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert an Double to a Longint
  -- Note    : The Double is rounded to its nearest Longint
  --
  -- D       : The Double
  -- return  : The Longint
  function To_Longint(D: in Double) return Longint;
  pragma Inline(To_Longint);

  -- Purpose : Convert a Character to a Float
  --
  -- C       : The Character
  -- return  : The Float
  function To_Float(C: in Character) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert a String to a Float
  --
  -- S       : The String
  -- return  : The Float
  function To_Float(S: in String) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert a Ustring to a Float
  --
  -- U       : The Ustring
  -- return  : The Float
  function To_Float(U: in Ustring) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert an Integer to a Float
  --
  -- I       : The Integer
  -- return  : The Float
  function To_Float(I: in Integer) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert a Longint to a Float
  --
  -- L       : The Longint
  -- return  : The Float
  function To_Float(L: in Longint) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert a Float to a Float
  --
  -- F       : The Float
  -- return  : The Float
  function To_Float(F: in Float) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert an Double to a Float
  --
  -- D       : The Double
  -- return  : The Float
  function To_Float(D: in Double) return Float;
  pragma Inline(To_Float);

  -- Purpose : Convert a Character to a Double
  --
  -- C       : The Character
  -- return  : The Double
  function To_Double(C: in Character) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert a String to a Double
  --
  -- S       : The String
  -- return  : The Double
  function To_Double(S: in String) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert a Ustring to a Double
  --
  -- U       : The Ustring
  -- return  : The Double
  function To_Double(U: in Ustring) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert an Integer to a Double
  --
  -- I       : The Integer
  -- return  : The Double
  function To_Double(I: in Integer) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert a Longint to a Double
  --
  -- L       : The Longint
  -- return  : The Double
  function To_Double(L: in Longint) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert a Float to a Double
  --
  -- F       : The Float
  -- return  : The Double
  function To_Double(F: in Float) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert a Double to a Double
  --
  -- D       : The Double
  -- return  : The Double
  function To_Double(D: in Double) return Double;
  pragma Inline(To_Double);

  -- Purpose : Convert an Integer to a String
  -- Note    : Parameters as in Ada.Integer_Text_IO
  --
  -- I       : The Integer
  -- Base    : The Base parameter
  -- return  : The String
  function To_String(I: in Integer; Base: in Number_Base := Ada.Integer_Text_IO.Default_Base) return String;

  -- Purpose : Convert a Longint to a String
  -- Note    : Parameters as in Ada.Long_Long_Integer_Text_IO
  --
  -- L       : The Longint
  -- Base    : The Base parameter
  -- return  : The String
  function To_String(L: in Longint; Base: in Number_Base := Ada.Long_Long_Integer_Text_IO.Default_Base) return String;

  -- Purpose : Convert a Float to a String
  -- Note    : Parameters as in Float_IO
  -- Note    : If Aft and Exp are zero an integer string is returned
  --
  -- F       : The Float
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The number of digits in the exponent
  -- return  : The String
  function To_String(F: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) return String;

  -- Purpose : Convert a Double to a String
  -- Note    : Parameters as in Double_IO
  -- Note    : If Aft and Exp are zero an integer string is returned
  --
  -- D       : The Double
  -- Aft     : The number of digits after the decimal point
  -- Exp     : The number of digits in the exponent
  -- return  : The String
  function To_String(D: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) return String;

  -- Purpose : Convert a Duration to a String
  -- Note    : Parameters as in Float_IO
  -- Note    : If Aft is zero no fraction of seconds is returned
  --
  -- D       : The Duration
  -- Aft     : The number of digits after the decimal point
  -- return  : The String
  function To_String(D: in Duration; Aft: in Field := Default_Float_Aft) return String;

  -- Purpose : Convert a Calendar Time to a String
  --
  -- T       : The Calendar Time
  -- return  : The String
  function To_String(T: in Time) return String;

  -- Purpose : Convert a Float to a String
  -- Note    : If Aft is zero an integer string is returned
  --
  -- F       : The Float
  -- Aft     : The number of digits after the decimal point
  -- Short   : True to skip non-significant zeros
  -- return  : The String
  function To_String_Exp0(F: in Float; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String;

  -- Purpose : Convert a Double to a String
  -- Note    : If Aft is zero an integer string is returned
  --
  -- D       : The Double
  -- Aft     : The number of digits after the decimal point
  -- Short   : True to skip non-significant zeros
  -- return  : The String
  function To_String_Exp0(D: in Double; Aft: in Field := Default_Double_Aft; Short: in Boolean := False) return String;

  -- Purpose : Convert a Float to a String skipping non-significant zeros
  -- Note    : If Aft is zero an integer string is returned
  --
  -- F       : The Float
  -- Aft     : The number of digits after the decimal point
  -- return  : The String
  function To_String_Short(F: in Float; Aft: in Field := Default_Float_Aft) return String;

  -- Purpose : Convert a Double to a String skipping non-significant zeros
  -- Note    : If Aft is zero an integer string is returned
  --
  -- D       : The Double
  -- Aft     : The number of digits after the decimal point
  -- return  : The String
  function To_String_Short(D: in Double; Aft: in Field := Default_Double_Aft) return String;

  -- Purpose : Convert a Float to a formatted String
  -- Note    : If Aft is zero an integer string is returned
  --
  -- F       : The Float
  -- Aft     : The number of digits after the decimal point
  -- Short   : True to skip non-significant zeros
  -- return  : The String
  function To_String_Exp_Auto(F: in Float; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String;

  -- Purpose : Convert a Float to a formatted String
  -- Note    : If Aft is zero an integer string is returned
  --
  -- D       : The Double
  -- Aft     : The number of digits after the decimal point
  -- Short   : True to skip non-significant zeros
  -- return  : The String
  function To_String_Exp_Auto(D: in Double; Aft: in Field := Default_Double_Aft; Short: in Boolean := False) return String;

  -- Purpose : Justify a String to the Left
  --
  -- S       : The String
  -- Width   : The Width of the Justified String
  -- Pad     : The Padding Character
  -- return  : The Left Justified String
  function Left_Justify(S: in String; Width: in Natural; Pad: in Character := ' ') return String;

  -- Purpose : Justify a String to the Right
  --
  -- S       : The String
  -- Width   : The Width of the Justified String
  -- Pad     : The Padding Character
  -- return  : The Right Justified String
  function Right_Justify(S: in String; Width: in Natural; Pad: in Character := ' ') return String;

  -- Purpose : Translate characters in a String to new characters
  -- Note    : The translation strings are considered as lists of characters, not as whole words
  --
  -- S       : The String
  -- From    : The Characters to be substituted
  -- To      : The replacement Characters
  -- return  : The Translated String
  function Translate(S: in String; From, To: in String) return String;

  -- Purpose : Convert a String to Uppercase
  --
  -- S       : The String
  -- return  : The String in Uppercase
  function To_Uppercase(S: in String) return String;
  pragma Inline(To_Uppercase);

  -- Purpose : Convert a Ustring to Uppercase
  --
  -- U       : The Ustring
  -- return  : The Ustring in Uppercase
  function To_Uppercase(U: in Ustring) return Ustring;

  -- Purpose : Convert a String to Lowercase
  --
  -- S       : The String
  -- return  : The String in Lowercase
  function To_Lowercase(S: in String) return String;
  pragma Inline(To_Lowercase);

  -- Purpose : Convert a Ustring to Lowercase
  --
  -- U       : The Ustring
  -- return  : The Ustring in Lowercase
  function To_Lowercase(U: in Ustring) return Ustring;

  -- Purpose : Capitalize a String
  --
  -- S       : The String
  -- return  : The Capitalized String
  function Capitalize(S: in String) return String;

  -- Purpose : Capitalize a Ustring
  --
  -- U       : The Ustring
  -- return  : The Capitalized Ustring
  function Capitalize(U: in Ustring) return Ustring;

  -- Purpose : Trims both sides of a String
  -- Note    : All kinds of Spaces are Trimmed
  --
  -- S       : The String
  -- return  : The Trimmed String
  function Trim_Spaces(S: in String) return String;

  -- Purpose : Trims both sides of a Ustring
  -- Note    : All kinds of Spaces are Trimmed
  --
  -- U       : The Ustring
  -- return  : The Trimmed Ustring
  function Trim_Spaces(U: in Ustring) return Ustring;

  -- Purpose : Check if a Character is in a List of Characters
  --
  -- C       : The Character
  -- L       : The List of Characters
  -- return  : True if Character is in the List of Characters
  function Is_In_Characters(C: in Character; L: in Characters) return Boolean;

  -- Purpose : Check if a Character is in a List of Characters
  --
  -- C       : The Character
  -- L       : The List of Characters
  -- return  : True if Character is in the List of Characters
  function Is_In_Characters(C: in Character; L: in PCharacters) return Boolean;

  -- Purpose : Check if a Character is a Space
  --
  -- C       : The Character
  -- return  : True if Character is a Space
  function Is_Space(C: in Character) return Boolean;
  pragma Inline(Is_Space);

  -- Purpose : Check if a Character is a Comment
  --
  -- C       : The Character
  -- return  : True if Character is a Comment
  function Is_Comment(C: in Character) return Boolean;
  pragma Inline(Is_Comment);

  -- Purpose : Check if a Character is a Separator
  --
  -- C       : The Character
  -- return  : True if Character is a Separator
  function Is_Separator(C: in Character) return Boolean;
  pragma Inline(Is_Separator);

  -- Purpose : Check if a Character is an Open Quote Character
  --
  -- C       : The Character
  -- return  : True if Character is an Open Quote Character
  function Is_Open_Quote(C: in Character) return Boolean;
  pragma Inline(Is_Open_Quote);

  -- Purpose : Check if a Character is a Close Quote Character
  --
  -- C       : The Character
  -- return  : True if Character is a Close Quote Character
  function Is_Close_Quote(C: in Character) return Boolean;
  pragma Inline(Is_Close_Quote);

  -- Purpose : Obtain the Space Characters
  --
  -- return  : The Space Characters
  function Get_Spaces return Characters;

  -- Purpose : Obtain the Comment Characters
  --
  -- return  : The Comment Characters
  function Get_Comments return Characters;

  -- Purpose : Obtain the Separator Characters
  --
  -- return  : The Separator Characters
  function Get_Separators return Characters;

  -- Purpose : Obtain the Open Quote Characters
  --
  -- return  : The Open Quote Characters
  function Get_Quotes_Open return Characters;

  -- Purpose : Obtain the Close Quote Characters
  --
  -- return  : The Close Quote Characters
  function Get_Quotes_Close return Characters;

  -- Purpose : Set the Space Characters
  --
  -- L       : The Space Characters
  procedure Set_Spaces(L: in Characters := Standard_Spaces);

  -- Purpose : Set the Comment Characters
  --
  -- L       : The Comment Characters
  procedure Set_Comments(L: in Characters := Standard_Comments);

  -- Purpose : Set the Separator Characters
  --
  -- L       : The Separator Characters
  procedure Set_Separators(L: in Characters := Standard_Separators);

  -- Purpose : Set the Quote Characters
  --
  -- L_Open  : The Open Quote Characters
  -- L_Close : The Close Quote Characters
  -- raises  : Quotes_Error
  procedure Set_Quotes(L_Open: in Characters := Standard_Quotes_Open; L_Close: in Characters := Standard_Quotes_Close);

  -- Purpose : Add a Space Character
  --
  -- C       : The Space Character
  procedure Add_Space(C: in Character);

  -- Purpose : Add a Comment Character
  --
  -- C       : The Comment Character
  procedure Add_Comment(C: in Character);

  -- Purpose : Add a Separator Character
  --
  -- C       : The Separator Character
  procedure Add_Separator(C: in Character);

  -- Purpose : Add Quote Characters
  --
  -- C_Open  : The Open Quote Character
  -- C_Close : The Close Quote Character
  procedure Add_Quotes(C_Open, C_Close: in Character);

  -- Purpose : Dummy function for the addition of two Characters
  --
  -- Left    : The Left Character
  -- Right   : The Right Character
  -- return  : The largest Character
  function "+"(Left, Right: in Character) return Character;
  pragma Inline("+");

  -- Purpose : Dummy function for the product of two Characters
  --
  -- Left    : The Left Character
  -- Right   : The Right Character
  -- return  : The largest Character
  function "*"(Left, Right: in Character) return Character;
  pragma Inline("*");

  -- Purpose : Longest of two Unbounded Strings
  --
  -- Left    : The Left Unbounded String
  -- Right   : The Right Unbounded String
  -- return  : The longest Unbounded String
  function "*"(Left, Right: in Ustring) return Ustring;
  pragma Inline("*");

  -- Purpose : Power of a Character
  --
  -- C       : The Character
  -- Power   : The Power
  -- return  : The Power of the Character
  function "**"(C: in Character; Power: in Natural) return String;
  pragma Inline("**");

  -- Purpose : Power of a String
  --
  -- S       : The String
  -- Power   : The Power
  -- return  : The Power of the String
  function "**"(S: in String; Power: in Natural) return String;

  -- Purpose : Power of an Unbounded String
  --
  -- U       : The Unbounded String
  -- Power   : The Power
  -- return  : The Power of the Unbounded String
  function "**"(U: in Ustring; Power: in Natural) return Ustring;

  function "+"(Left: in Integer; Right: in Float) return Float;
  pragma Inline("+");
  function "+"(Left: in Float; Right: in Integer) return Float;
  pragma Inline("+");
  function "-"(Left: in Integer; Right: in Float) return Float;
  pragma Inline("-");
  function "-"(Left: in Float; Right: in Integer) return Float;
  pragma Inline("-");
  function "*"(Left: in Integer; Right: in Float) return Float;
  pragma Inline("*");
  function "*"(Left: in Float; Right: in Integer) return Float;
  pragma Inline("*");
  function "/"(Left: in Float; Right: in Integer) return Float;
  pragma Inline("/");

  function "+"(Left: in Integer; Right: in Double) return Double;
  pragma Inline("+");
  function "+"(Left: in Double; Right: in Integer) return Double;
  pragma Inline("+");
  function "-"(Left: in Integer; Right: in Double) return Double;
  pragma Inline("-");
  function "-"(Left: in Double; Right: in Integer) return Double;
  pragma Inline("-");
  function "*"(Left: in Integer; Right: in Double) return Double;
  pragma Inline("*");
  function "*"(Left: in Double; Right: in Integer) return Double;
  pragma Inline("*");
  function "/"(Left: in Double; Right: in Integer) return Double;
  pragma Inline("/");

  function Concat(Left, Right: in Unbounded_String) return Unbounded_String renames Ada.Strings.Unbounded."&";
  function Concat(Left: in Unbounded_String; Right: in String) return Unbounded_String renames Ada.Strings.Unbounded."&";
  function Concat(Left: in String; Right: in Unbounded_String) return Unbounded_String renames Ada.Strings.Unbounded."&";
  function Concat(Left: in Unbounded_String; Right: in Character) return Unbounded_String renames Ada.Strings.Unbounded."&";
  function Concat(Left : in Character; Right : in Unbounded_String) return Unbounded_String renames Ada.Strings.Unbounded."&";
  function Equal(Left, Right: in Unbounded_String) return Boolean renames Ada.Strings.Unbounded."=";
  function Equal(Left: in Unbounded_String; Right: in String) return Boolean renames Ada.Strings.Unbounded."=";
  function Equal(Left: in String; Right: in Unbounded_String) return Boolean renames Ada.Strings.Unbounded."=";

  function "+"(Left, Right: in Unbounded_String) return Unbounded_String renames Concat;
  function "+"(Left: in Unbounded_String; Right: in String) return Unbounded_String renames Concat;
  function "+"(Left: in String; Right: in Unbounded_String) return Unbounded_String renames Concat;
  function "+"(Left: in Unbounded_String; Right: in Character) return Unbounded_String renames Concat;
  function "+"(Left : in Character; Right : in Unbounded_String) return Unbounded_String renames Concat;

  function S2U(S: in String) return Unbounded_String renames To_Unbounded_String;
  function U2S(U: in Unbounded_String) return String renames To_String;

  function C2I(C: in Character) return Integer renames To_Integer;
  function S2I(S: in String) return Integer renames To_Integer;
  function U2I(U: in Ustring) return Integer renames To_Integer;
  function I2I(I: in Integer) return Integer renames To_Integer;
  function L2I(L: in Longint) return Integer renames To_Integer;
  function F2I(F: in Float) return Integer renames To_Integer;
  function D2I(D: in Double) return Integer renames To_Integer;
  function C2L(C: in Character) return Longint renames To_Longint;
  function S2L(S: in String) return Longint renames To_Longint;
  function U2L(U: in Ustring) return Longint renames To_Longint;
  function I2L(I: in Integer) return Longint renames To_Longint;
  function L2L(I: in Longint) return Longint renames To_Longint;
  function F2L(F: in Float) return Longint renames To_Longint;
  function D2L(D: in Double) return Longint renames To_Longint;
  function C2F(C: in Character) return Float renames To_Float;
  function S2F(S: in String) return Float renames To_Float;
  function U2F(U: in Ustring) return Float renames To_Float;
  function I2F(I: in Integer) return Float renames To_Float;
  function L2F(L: in Longint) return Float renames To_Float;
  function F2F(F: in Float) return Float renames To_Float;
  function D2F(D: in Double) return Float renames To_Float;
  function C2D(C: in Character) return Double renames To_Double;
  function S2D(S: in String) return Double renames To_Double;
  function U2D(U: in Ustring) return Double renames To_Double;
  function I2D(I: in Integer) return Double renames To_Double;
  function L2D(L: in Longint) return Double renames To_Double;
  function F2D(F: in Float) return Double renames To_Double;
  function D2D(D: in Double) return Double renames To_Double;
  function I2S(I: in Integer; Base: in Number_Base := Ada.Integer_Text_IO.Default_Base) return String renames To_String;
  function L2S(L: in Longint; Base: in Number_Base := Ada.Long_Long_Integer_Text_IO.Default_Base) return String renames To_String;
  function F2S(F: in Float; Aft: in Field := Default_Float_Aft; Exp: in Field := Default_Float_Exp) return String renames To_String;
  function D2S(D: in Double; Aft: in Field := Default_Double_Aft; Exp: in Field := Default_Double_Exp) return String renames To_String;
  function D2S(D: in Duration; Aft: in Field := Default_Float_Aft) return String renames To_String;
  function T2S(T: in Time) return String renames To_String;
  function F2Se0(F: in Float; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String renames To_String_Exp0;
  function D2Se0(D: in Double; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String renames To_String_Exp0;
  function F2Ss(F: in Float; Aft: in Field := Default_Float_Aft) return String renames To_String_Short;
  function D2Ss(D: in Double; Aft: in Field := Default_Float_Aft) return String renames To_String_Short;
  function F2Sea(F: in Float; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String renames To_String_Exp_Auto;
  function D2Sea(D: in Double; Aft: in Field := Default_Float_Aft; Short: in Boolean := False) return String renames To_String_Exp_Auto;

  function To_Num(C: in Character) return Integer renames To_Integer;
  function To_Num(S: in String) return Integer renames To_Integer;
  function To_Num(U: in Ustring) return Integer renames To_Integer;
  function To_Num(I: in Integer) return Integer renames To_Integer;
  function To_Num(L: in Longint) return Integer renames To_Integer;
  function To_Num(F: in Float) return Integer renames To_Integer;
  function To_Num(D: in Double) return Integer renames To_Integer;
  function To_Num(C: in Character) return Longint renames To_Longint;
  function To_Num(S: in String) return Longint renames To_Longint;
  function To_Num(U: in Ustring) return Longint renames To_Longint;
  function To_Num(I: in Integer) return Longint renames To_Longint;
  function To_Num(L: in Longint) return Longint renames To_Longint;
  function To_Num(F: in Float) return Longint renames To_Longint;
  function To_Num(D: in Double) return Longint renames To_Longint;
  function To_Num(C: in Character) return Float renames To_Float;
  function To_Num(S: in String) return Float renames To_Float;
  function To_Num(U: in Ustring) return Float renames To_Float;
  function To_Num(I: in Integer) return Float renames To_Float;
  function To_Num(L: in Longint) return Float renames To_Float;
  function To_Num(F: in Float) return Float renames To_Float;
  function To_Num(D: in Double) return Float renames To_Float;
  function To_Num(C: in Character) return Double renames To_Double;
  function To_Num(S: in String) return Double renames To_Double;
  function To_Num(U: in Ustring) return Double renames To_Double;
  function To_Num(I: in Integer) return Double renames To_Double;
  function To_Num(L: in Longint) return Double renames To_Double;
  function To_Num(F: in Float) return Double renames To_Double;
  function To_Num(D: in Double) return Double renames To_Double;

  function To_Value(C: in Character) return Integer renames To_Integer;
  function To_Value(S: in String) return Integer renames To_Integer;
  function To_Value(U: in Ustring) return Integer renames To_Integer;
  function To_Value(I: in Integer) return Integer renames To_Integer;
  function To_Value(L: in Longint) return Integer renames To_Integer;
  function To_Value(F: in Float) return Integer renames To_Integer;
  function To_Value(D: in Double) return Integer renames To_Integer;
  function To_Value(C: in Character) return Longint renames To_Longint;
  function To_Value(S: in String) return Longint renames To_Longint;
  function To_Value(U: in Ustring) return Longint renames To_Longint;
  function To_Value(I: in Integer) return Longint renames To_Longint;
  function To_Value(L: in Longint) return Longint renames To_Longint;
  function To_Value(F: in Float) return Longint renames To_Longint;
  function To_Value(D: in Double) return Longint renames To_Longint;
  function To_Value(C: in Character) return Float renames To_Float;
  function To_Value(S: in String) return Float renames To_Float;
  function To_Value(U: in Ustring) return Float renames To_Float;
  function To_Value(I: in Integer) return Float renames To_Float;
  function To_Value(L: in Longint) return Float renames To_Float;
  function To_Value(F: in Float) return Float renames To_Float;
  function To_Value(D: in Double) return Float renames To_Float;
  function To_Value(C: in Character) return Double renames To_Double;
  function To_Value(S: in String) return Double renames To_Double;
  function To_Value(U: in Ustring) return Double renames To_Double;
  function To_Value(I: in Integer) return Double renames To_Double;
  function To_Value(L: in Longint) return Double renames To_Double;
  function To_Value(F: in Float) return Double renames To_Double;
  function To_Value(D: in Double) return Double renames To_Double;


private

  Spaces       : PCharacters;
  Comments     : PCharacters;
  Separators   : PCharacters;
  Quotes_Open  : PCharacters;
  Quotes_Close : PCharacters;

end Utils;
