source("time.r")
 
# Timing tests:

e1 <- test.cmp (function (a)
{
  i <- 0
  while (i < a)
  { i <- i + 1;
  }
  a
})

e2 <- test.cmp (function (a)
{
  i <- 0
  repeat
  { if (i==a) break;
    i <- i + 1;
  }
  a
})

e3 <- test.cmp (function (a)
{
  i <- 0
  repeat
  { if (i==a) return (a);
    i <- i + 1;
  }
})

test.name <- "relop-logic.three-loops"

sys.time({ for (i in 1:100000) e1(20) })
sys.time({ for (i in 1:100000) e2(20) })
sys.time({ for (i in 1:100000) e3(20) })

f1 <- test.cmp (function (a)
{
  j <- 1
  n <- length(a)

  while (j<=n)
  { if (a[j]>0.9) break
    else if (a[j]>0.1) a[j] <- 1
    j <- j + 1
  }

  sum(a)
})

f2 <- test.cmp (function (a)
{
  for (j in seq_along(a))
  { if (a[j]>0.9) break
    else if (a[j]>0.1) a[j] <- 1
  }

  sum(a)
})

test.name <- "relop-logic.two-loops"

a <- seq(0,1,length=200)

sys.time({ for (i in 1:3000) r <- f1(a) })
print(r)

sys.time({ for (i in 1:3000) r <- f2(a) })
print(r)

g <- test.cmp (function (n)
{
  s <- 0
  i <- 1
  while (i < n)
  { if (i<10000)
      s <- s + i
    else if (i<100000)
      s <- s + 1
    else
      s <- s + (i>110000)
    i <- i + 1
  }
  s
})

h <- test.cmp (function (n)
{
  for (k in 1:n)
  { s <- 0
    for (i in 1:100)
      for (j in 1:10)
      { if (i>10 && i<90 || !(j>1) )
          s <- s + i + j
        if (i==j)
          s <- 2 * s
      }
  }
  s
})

test.name <- "relop-logic.loop-if"

sys.time(r <- g(1000000))
print(r)

sys.time(r <- h(1000))
print(r)

# A few correctness tests:

print(c(10,20)<c(11,12))
print(c(-1,2,5)==1:3)
print(1:3<2:2)
