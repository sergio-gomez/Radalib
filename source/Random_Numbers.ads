-- Radalib, Copyright (c) 2022 by
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


-- @filename Random_Numbers.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 02/03/2010
-- @revision 21/01/2018
-- @brief Several Random Generators

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Utils; use Utils;

package Random_Numbers is

  subtype Probability is Float range 0.0 .. 1.0;


  function Random_Uniform(G: in Generator) return Uniformly_Distributed renames Random;

  -- Purpose : Generate a Float Random Number from a Uniform Distribution
  --
  -- G       : The seed
  -- From    : The smallest (or largest) random value
  -- To      : The largest (or smallest) random value
  -- return  : The uniform random number
  function Random_Uniform(G: in Generator; From, To: in Float) return Float;

  -- Purpose : Generate an Integer Random Number from a Uniform Distribution
  --
  -- G       : The seed
  -- From    : The smallest (or largest) random value
  -- To      : The largest (or smallest) random value
  -- return  : The uniform random number
  function Random_Uniform(G: in Generator; From, To: in Integer) return Integer;

  -- Purpose : Generate a Random Number from a Bernoulli Distribution
  --
  -- G       : The seed
  -- P       : The probability of each trial
  -- return  : The bernoulli random variable
  function Random_Bernoulli(G: in Generator; P: in Probability) return Boolean;

  -- Purpose : Generate a Random Number from a Binomial Distribution
  --
  -- G       : The seed
  -- N       : The number of trials
  -- P       : The probability of each trial
  -- return  : The binomial random number
  function Random_Binomial(G: in Generator; N: in Natural; P: in Probability) return Natural;

  -- Purpose : Generate an Integer Random Number from a Weighted Distribution
  -- Note    : Index I is returned with probability Weights(I) / Sum(Weights)
  -- Note    : If Sum(Weights) <= 0, a Random Uniform is returned
  -- Note    : Weights cannot be negative, no check is performed
  --
  -- G       : The seed
  -- Weights : The weights
  -- return  : The weighted random number
  function Random_Weighted(G: in Generator; Weights: in Floats) return Integer;
  function Random_Weighted(G: in Generator; Weights: in PFloats) return Integer;

  -- Purpose : Generate a Random Permutation
  -- Note    : The Permutation consists of Integers in the same range as the array
  -- Note    : Durstenfeld version of Fisher & Yates algorithm
  -- Note    : The array must exist, no check is performed
  --
  -- G       : The seed
  -- Permutation : The Permutation
  procedure Random_Permutation(G: in Generator; Permutation: in out Integers);
  procedure Random_Permutation(G: in Generator; Permutation: in PIntegers);

end Random_Numbers;
