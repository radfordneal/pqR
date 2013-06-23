source("time.r")
 
test.name <- "parens"

f.curly <- test.cmp (function (n) 
{ a <- 5.5; b <- 1.1; c <- 4.2; 
  for (i in 1:n) d <- {a+1}/{a*{b+c}}
})

sys.time(f.curly(1000000))

f.paren <- test.cmp (function (n) 
{ a <- 5.5; b <- 1.1; c <- 4.2; 
  for (i in 1:n) d <- (a+1)/(a*(b+c))
})

sys.time(f.paren(1000000))

# Test correctness (if that's the right word) of using ... in parens.

f <- function (...) { (...) }

a <- 5
print(f(a+100))

