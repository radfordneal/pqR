/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000--2016  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double rnbinom(double n, double p)
 *
 *  DESCRIPTION
 *
 *    Random variates from the negative binomial distribution.
 *
 *  NOTES
 *
 *    x = the number of failures before the n-th success
 *
 *  REFERENCE
 *
 *    Devroye, L. (1986).
 *    Non-Uniform Random Variate Generation.
 *    New York:Springer-Verlag.  Pages 488 and 543.
 *
 *  METHOD
 *
 *    Generate lambda as gamma with shape parameter n and scale
 *    parameter p/(1-p).  Return a Poisson deviate with mean lambda.
 */

#include "nmath.h"

double rnbinom(double size, double prob)
{
    if(!R_FINITE(prob) || ISNAN(size) || size <= 0 || prob <= 0 || prob > 1)
	/* prob = 1 is ok, PR#1218 */
	ML_WARN_return_NAN;
    if(!R_FINITE(size)) size = DBL_MAX / 2.; // '/2' to prevent rgamma() returning Inf
    return (prob == 1) ? 0 : rpois(rgamma(size, (1 - prob) / prob));
}

double rnbinom_mu(double size, double mu)
{
    if(!R_FINITE(mu) || ISNAN(size) || size <= 0 || mu < 0)
	ML_WARN_return_NAN;
    if(!R_FINITE(size)) size = DBL_MAX / 2.;
    return (mu == 0) ? 0 : rpois(rgamma(size, mu / size));
}
