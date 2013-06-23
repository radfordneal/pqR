source("time.r")
 
test.name <- "for"

# Timing tests:

f <- test.cmp (function (n) 
  { a <- 0; for (j in 1:n) for (i in 1:n) a <- a+i/j; a })

sys.time(r <- f(2000))
print(r)

f <- test.cmp (function (n) 
  { a <- 0; for (k in 1:n) for (j in 1:n) for (i in 1:n) a <- a+k*i/j; a })

sys.time(r <- f(150))
print(r)

f <- test.cmp (function (n) 
  { a <- 0; 
    for (l in 1:n) for (k in 1:n) for (j in 1:n) for (i in 1:n) a <- a+l*k*i/j
    a
  })

sys.time(r <- f(40))
print(r)

g <- test.cmp (function (n) 
  { a <- 0; for (j in seq_len(n)) for (i in seq_len(n)) a <- a+i/j; a })

sys.time(r <- g(2000))
print(r)

f <- test.cmp (function (n) 
  { for (l in 1:n) for (k in 1:n) for (j in 1:n) for (i in 1:n) i })

sys.time(r <- f(70))
print(r)

v1n <- 1:70

f <- test.cmp (function () 
  { for (l in v1n) for (k in v1n) for (j in v1n) for (i in v1n) i })

sys.time(r <- f())
print(r)

f <- test.cmp (function () 
{ v1 <- 1:70; 
  for (l in v1) 
  { v2 <- 1:70
    for (k in v2) 
    { v3 <- 1:70
      for (j in v3) 
      { v4 <- 1:70
        for (i in v4) i 
      }
    }
  }
})

sys.time(r <- f())
print(r)


# A quick correctness check:

for (i in 1:2) { print(i); if (i==1) a <- i; print(a); }
cat("\n")
