c-----------------------------------------------------------------------
c
c  R : A Computer Language for Statistical Data Analysis
c  Copyright (C) 1977        B.D. Ripley
c  Copyright (C) 1999        the R Core Team
c
c  This program is free software; you can redistribute it and/or modify
c  it under the terms of the GNU General Public License as published by
c  the Free Software Foundation; either version 2 of the License, or
c  (at your option) any later version.
c
c  This program is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c  GNU General Public License for more details.
c
c  You should have received a copy of the GNU General Public License
c  along with this program; if not, a copy is available at
c  https://www.R-project.org/Licenses/
c
c-----------------------------------------------------------------------
c
      subroutine eureka (lr,r,g,f,var,a)
c
c      solves Toeplitz matrix equation toep(r)f=g(1+.)
c      by Levinson's algorithm
c      a is a workspace of size lr, the number
c      of equations
c
      integer lr,l,l1,l2,i,j,k
      double precision r(lr+1), g(lr+1), f(lr,lr), a(lr), var(lr)
      double precision v, d, q, hold
      v = r(1)
      d = r(2)
      a(1) = 1.0d0
      f(1,1) = g(2)/v
      q = f(1,1)*r(2)
      var(1) = (1 - f(1,1)*f(1,1))*r(1)
      if (lr .eq. 1) return
      do 60 l = 2, lr
        a(l) = -d/v
        if (l .gt. 2) then
          l1 = (l - 2)/2
          l2 = l1 + 1
          do 10 j = 2, l2
            hold = a(j)
            k = l - j + 1
            a(j) = a(j) + a(l)*a(k)
            a(k) = a(k) + a(l)*hold
   10       continue
          if (2*l1 .ne. l - 2) a(l2+1) = a(l2+1)*(1.0d0 + a(l))
        endif
        v = v + a(l)*d
        f(l,l) = (g(l+1) - q)/v
        do 40 j = 1, l-1
          f(l,j) = f(l-1, j) + f(l, l)*a(l-j+1)
   40     continue
c  estimate the innovations variance
        var(l) = var(l-1) * (1 - f(l,l)*f(l,l))
        if (l .eq. lr) return
        d = 0.0d0
        q = 0.0d0
        do 50 i = 1, l
          k = l-i+2
          d = d + a(i)*r(k)
          q = q + f(l,i)*r(k)
   50     continue
   60   continue
      return
      end
