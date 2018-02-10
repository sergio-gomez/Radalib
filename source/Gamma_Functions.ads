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


-- @filename Gamma_Functions.ads
-- @author Sergio Gomez
-- @version 1.0
-- @date 29/11/2004
-- @revision 5/12/2004
-- @brief Calculation of Gamma Functions

package Gamma_Functions is

  Gamma_Error: exception;


  -- Purpose : Calculate the Gamma Function G(A)
  -- Note    : G(A) = Integrate[T^(A-1) Exp(-T), T=0..Infinity]
  -- Note    : A /= 0,-1,-2, ...
  --
  -- A       : The argument
  -- return  : The Gamma
  -- raises  : Gamma_Error
  function Gamma(A: in Float) return Float;

  -- Purpose : Calculate the Logarithm of the Gamma Function Ln(G(A))
  -- Note    : G(A) = Ln[G(A)]
  -- Note    : A > 0
  --
  -- A       : The argument
  -- return  : The Logarithm of the Gamma
  -- raises  : Gamma_Error
  function Ln_Gamma(A: in Float) return Float;

  -- Purpose : Calculate the Incomplete Gamma Function G(A, X)
  -- Note    : G(A, X) = Integrate[T^(A-1) Exp(-T), T=X..Infinity]
  -- Note    : X >= 0, A /= 0,-1,-2, ...
  --
  -- A       : The exponent argument
  -- X       : The integration limit argument
  -- return  : The Incomplete Gamma
  -- raises  : Gamma_Error
  function Incomplete_Uppercase_Gamma(A: in Float; X: in Float) return Float;

  -- Purpose : Calculate the Incomplete Gamma Function g(A, X)
  -- Note    : g(A, X) = Integrate[T^(A-1) Exp(-T), T=0..X]
  -- Note    : X >= 0, A /= 0,-1,-2, ...
  --
  -- A       : The exponent argument
  -- X       : The integration limit argument
  -- return  : The Incomplete Gamma
  -- raises  : Gamma_Error
  function Incomplete_Lowercase_Gamma(A: in Float; X: in Float) return Float;

  -- Purpose : Calculate the Incomplete Gamma Ratio Function Q(A, X)
  -- Note    : Q(A, X) = G(A, X) / G(A)
  -- Note    : X >= 0, A /= 0,-1,-2, ...
  --
  -- A       : The exponent argument
  -- X       : The integration limit argument
  -- return  : The Incomplete Gamma Ratio Q
  -- raises  : Gamma_Error
  function Incomplete_Gamma_Ratio_Q(A: in Float; X: in Float) return Float;

  -- Purpose : Calculate the Incomplete Gamma Ratio Function P(A, X)
  -- Note    : P(A, X) = g(A, X) / G(A)
  -- Note    : X >= 0, A /= 0,-1,-2, ...
  --
  -- A       : The exponent argument
  -- X       : The integration limit argument
  -- return  : The Incomplete Gamma Ratio P
  -- raises  : Gamma_Error
  function Incomplete_Gamma_Ratio_P(A: in Float; X: in Float) return Float;


private

  -- Purpose : Calculate the Incomplete Gamma Ratio Function P(A, X) by its series expansion
  -- Note    : X >= 0, A > 0
  --
  -- A       : The exponent argument
  -- X       : The integration limit argument
  -- Gam_P   : The Incomplete Gamma Ratio P
  -- Gam_Ln  : The Logarithm of G(A)
  -- raises  : Gamma_Error
  procedure Gamma_P_Series(A, X: in Float; Gam_P, Gam_Ln: out Float);

  -- Purpose : Calculate the Incomplete Gamma Ratio Function Q(A, X) by its continued fraction
  -- Note    : X >= 0, A > 0
  --
  -- A       : The exponent argument
  -- X       : The integration limit argument
  -- Gam_Q   : The Incomplete Gamma Ratio Q
  -- Gam_Ln  : The Logarithm of G(A)
  -- raises  : Gamma_Error
  procedure Gamma_Q_Continued_Fraction(A, X: in Float; Gam_Q, Gam_Ln: out Float);

end Gamma_Functions;

