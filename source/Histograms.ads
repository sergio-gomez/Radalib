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


-- @filename Histograms.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 7/02/2005
-- @revision 08/04/2012
-- @brief Treatment of Histograms

package Histograms is

  -- Represents a Histogram
  type Histogram is private;

  -- Types of Accumulated Histograms
  type Accumulation_Type is (Add_Left_Bins, Add_Right_Bins);

  Uninitialized_Histogram_Error: exception;
  Incompatible_Parameters_Error: exception;
  Index_Out_Of_Bounds_Error: exception;
  Accumulated_Histogram_Error: exception;


  -- Purpose : Initialize a Histogram
  --
  -- H           : The Histogram
  -- Lower_Limit : The Lower Limit of the Histogram
  -- Upper_Limit : The Upper Limit of the Histogram
  -- Num_Bins    : The Number of Bins of the Histogram
  -- Logarithmic : True if Logarithmic Histogram
  -- raises      : Incompatible_Parameters_Error
  procedure Initialize(H: out Histogram; Lower_Limit, Upper_Limit: in Float; Num_Bins: in Positive; Logarithmic: Boolean := False);

  -- Purpose : Initialize a Histogram
  -- Note    : This Initialization is only for non Logarithmic Histograms
  --
  -- H           : The Histogram
  -- Lower_Limit : The Lower Limit of the Histogram
  -- Num_Bins    : The Number of Bins of the Histogram
  -- Bin_Size    : The Size of the Bins in the Histogram
  -- raises      : Incompatible_Parameters_Error
  procedure Initialize(H: out Histogram; Lower_Limit: in Float; Num_Bins: in Positive; Bin_Size: in Float);

  -- Purpose : Initialize a Histogram
  -- Note    : This Initialization is for Integer Values and non Logarithmic Histograms
  -- Note    : The Bins are adjusted to half-integer Limits
  --
  -- H           : The Histogram
  -- Lower_Value : The Lower Integer Value of the Histogram
  -- Num_Bins    : The Number of Bins of the Histogram
  -- Bin_Size    : The Size of the Bins in the Histogram
  procedure Initialize(H: out Histogram; Lower_Value: in Integer; Num_Bins, Bin_Size: in Positive);

  -- Purpose : Deallocate all the space used by a Histogram
  --
  -- H       : The Histogram
  procedure Free(H: in out Histogram);

  -- Purpose : To know if a Histogram is Logarithmic
  --
  -- H       : The Histogram
  -- return  : True if it is Logarithmic
  -- raises  : Uninitialized_Histogram_Error
  function Is_Logarithmic(H: in Histogram) return Boolean;

  -- Purpose : Get the Value of the Number of Bins of the Histogram
  --
  -- H       : The Histogram
  -- return  : The Number of Bins
  -- raises  : Uninitialized_Histogram_Error
  function Get_Num_Bins(H: in Histogram) return Positive;

  -- Purpose : Get the Value of Bin Limits of the Histogram
  -- Note    : The Index of the Limit has to be in the range 0..Num_Bins
  -- Note    : Index 0 corresponds to the Lower Limit
  -- Note    : Index Num_Bins corresponds to the Upper Limit
  --
  -- H       : The Histogram
  -- I       : The Index of the Bin
  -- return  : The Limit between Bins I and I+1
  -- raises  : Uninitialized_Histogram_Error
  -- raises  : Index_Out_Of_Bounds_Error
  function Get_Limit(H: in Histogram; I: in Natural) return Float;

  -- Purpose : Get the Value of the Lower Limit of the Histogram
  --
  -- H       : The Histogram
  -- return  : The Lower Limit
  -- raises  : Uninitialized_Histogram_Error
  function Get_Lower_Limit(H: in Histogram) return Float;

  -- Purpose : Get the Value of the Upper Limit of the Histogram
  --
  -- H       : The Histogram
  -- return  : The Upper Limit
  -- raises  : Uninitialized_Histogram_Error
  function Get_Upper_Limit(H: in Histogram) return Float;

  -- Purpose : Add a Value to the Histogram
  -- Note    : The Value is put into Bin(I) such that Limit(I-1) <= Value < Limit(I)
  -- Note    : Cannot Add Values to Accumulated Histograms
  --
  -- H       : The Histogram
  -- V       : The Value
  -- raises  : Uninitialized_Histogram_Error
  -- raises  : Accumulated_Histogram_Error
  procedure Add(H: in Histogram; V: in Float);

  -- Purpose : Add an Integer Value to the Histogram
  -- Note    : Procedure designed for Integer Histograms, but no check is done
  -- Note    : The Value is put into Bin(I) such that Limit(I-1) <= Value < Limit(I)
  -- Note    : Cannot Add Values to Accumulated Histograms
  --
  -- H       : The Histogram
  -- V       : The Integer Value
  -- raises  : Uninitialized_Histogram_Error
  -- raises  : Accumulated_Histogram_Error
  procedure Add(H: in Histogram; V: in Integer);

  -- Purpose : Get the number of Values in a Bin of the Histogram
  -- Note    : The Index of the Bin has to be in the range 1..Num_Bins
  --
  -- H       : The Histogram
  -- I       : The Index of the Bin
  -- return  : The number of Values in the Bin
  -- raises  : Uninitialized_Histogram_Error
  -- raises  : Index_Out_Of_Bounds_Error
  function Get_Bin(H: in Histogram; I: in Positive) return Natural;

  -- Purpose : Get the number of Values Under the Lower Limit in the Histogram
  --
  -- H       : The Histogram
  -- return  : The number of Values Under the Lower Limit
  -- raises  : Uninitialized_Histogram_Error
  function Get_Under_Lower_Limit(H: in Histogram) return Natural;

  -- Purpose : Get the number of Values Over the Upper Limit in the Histogram
  --
  -- H       : The Histogram
  -- return  : The number of Values Over the Upper Limit
  -- raises  : Uninitialized_Histogram_Error
  function Get_Over_Upper_Limit(H: in Histogram) return Natural;

  -- Purpose : Get the Number of Values counted by the Histogram
  --
  -- H       : The Histogram
  -- return  : The Number of Values
  -- raises  : Uninitialized_Histogram_Error
  function Get_Num_Values(H: in Histogram) return Natural;

  -- Purpose : Accumulate a Histogram
  --
  -- H       : The Histogram
  -- Acc     : The Accumulation Type
  -- return  : The Accumulated Histogram
  -- raises  : Uninitialized_Histogram_Error
  function Accumulate_Histogram(H: in Histogram; Acc: in Accumulation_Type) return Histogram;

  -- Purpose : To know if a Histogram is Accumulated
  --
  -- H       : The Histogram
  -- return  : True if it is Accumulated
  -- raises  : Uninitialized_Histogram_Error
  function Is_Accumulated(H: in Histogram) return Boolean;


private

  ---------------
  -- Histogram --
  ---------------

  type Bins is array(Positive range <>) of Natural;

  type Histogram_Rec(Num_Bins: Positive) is record
    Accumulated: Boolean;
    Logarithmic: Boolean;
    Lower_Limit: Float;
    Upper_Limit: Float;
    Bin: Bins(1..Num_Bins);
    Under_Lower_Limit: Natural;
    Over_Upper_Limit: Natural;
    Num_Values: Natural;
  end record;

  type Histogram is access Histogram_Rec;

end Histograms;
