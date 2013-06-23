source("time.r")
 
test.name <- "assign.simple"

f <- test.cmp (function (n,a,b) 
{ for (i in 1:n) 
  { w <- a+1
    x <- a+b
    y <- a-b
    z <- a*b
  }
  w+x+y+z
})

sys.time(r <- f(1000000,1.1,2.2))
print(r)


test.name <- "assign.vecmat"

f <- test.cmp (function (n,j,v) 
{ vec <- (1:100)/100
  for (i in 1:n) 
  { vec[j] <- v
  }
  sum(vec)
})

sys.time(r <- f(1000000,17,1.1))
print(r)

sys.time(r <- f(1000000,15:17,1.1))
print(r)


f <- test.cmp (function (n,j,v) 
{ vec <- (1:100)/100
  names(vec) <- paste(c("a","b","c","d","e","f","g","h","i","j"),1:100,sep="")
  for (i in 1:n) 
  { vec[j] <- v
  }
  sum(vec)
})

sys.time(r <- f(1000000,"g17",1.1))
print(r)

sys.time(r <- f(1000000,c("e15","f16","g17"),1.1))
print(r)


f <- test.cmp (function (n,j,k,v) 
{ mat <- matrix ((1:100)/100, 10, 10)
  for (i in 1:n) 
  { mat[j,k] <- v
  }
  sum(mat)
})

sys.time(r <- f(1000000,7,2,1.1))
print(r)

sys.time(r <- f(1000000,5:7,2,1.1))
print(r)


test.name <- "assign.largevec"

f <- test.cmp (function (n,j,v) 
{ vec <- (0:10000)/10000
  for (i in 1:n) 
  { vec[j] <- v
  }
  sum(vec)
})

sys.time(r <- f(30000,201:8000,1.1))
print(r)

sys.time(r <- f(30000,201:8000,(1:100)*1.1))
print(r)

sys.time(r <- f(30000,201:8000,(201:8000)*1.1))
print(r)


test.name <- "assign.list"

f <- test.cmp (function (n) 
{ lis <- list(ab=3,cd=4,ef=5,gh=6)
  for (i in 1:n) 
  { lis$ef <- 10
  }
  lis
})

sys.time(r <- f(1000000))
print(unlist(r))


f <- test.cmp (function (n,j) 
{ lis <- list(ab=3,cd=4,ef=5,gh=6)
  for (i in 1:n) 
  { lis[[j]] <- 10
  }
  lis
})

sys.time(r <- f(1000000,"ef"))
print(unlist(r))

sys.time(r <- f(1000000,3))
print(unlist(r))


f <- test.cmp (function (n,j) 
{ lis <- list(ab=3,cd=4,ef=5,gh=6)
  for (i in 1:n) 
  { lis[j] <- 10
  }
  lis
})

sys.time(r <- f(1000000,"ef"))
print(unlist(r))

sys.time(r <- f(1000000,3))
print(unlist(r))

sys.time(r <- f(1000000,c("ef","ab")))
print(unlist(r))

sys.time(r <- f(1000000,c(3,1)))
print(unlist(r))


test.name <- "assign.complicated"

f <- test.cmp (function (n,j,k,v) 
{ lis <- list (a=1, b=(1:11)/11, c=list(xa=10,ya=20,xb=30,yb=40))
  for (i in 1:n) 
  { lis[[j]][k] <- v
  }
  lis
})

sys.time(r <- f(500000,2,3,5))
print(unlist(r))

sys.time(r <- f(500000,"b",3,5))
print(unlist(r))


f <- test.cmp (function (n,k,v) 
{ lis <- list (a=1, b=(1:11)/11, c=list(xa=10,ya=20,xb=30,yb=40))
  for (i in 1:n) 
  { lis$b[k] <- v
  }
  lis
})

sys.time(r <- f(500000,3,5))
print(unlist(r))

sys.time(r <- f(500000,c(3,1),5))
print(unlist(r))


f <- test.cmp (function (n,v) 
{ lis <- list (a=1, b=(1:11)/11, c=list(xa=10,ya=20,xb=30,yb=40))
  for (i in 1:n) 
  { lis$c$xb <- v
  }
  lis
})

sys.time(r <- f(500000,5))
print(unlist(r))


f <- test.cmp (function (n,v) 
{ lis <- list (a=1, b=matrix(4,2,2), c=10)
  for (i in 1:n) 
  { diag(lis$b)[2] <- 5
  }
  lis
})

sys.time(r <- f(60000,5))
print(unlist(r))


test.name <- "assign.named"

f <- test.cmp (function (n) 
{ u <- numeric(1000)
  for (i in 1:n)
  { v <- u
    v <- 0
    u[10] <- 1
  }
})

sys.time(f(500000))

f <- test.cmp (function (n) 
{ u <- numeric(1000)
  w <- 1
  for (i in 1:n)
  { v <- w
    v <- 0
    u[10] <- 1
  }
})

sys.time(f(500000))
