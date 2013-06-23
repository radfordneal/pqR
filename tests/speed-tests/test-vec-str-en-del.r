source("time.r")
 
test.name <- "vec-str-en-del.enlarge"

f <- test.cmp (function (n,v)
{ m <- length (v) 
  v1 <- v[[1]]
  for (i in 1:n) v[[m+i]] <- v1
  v
})

v <- rep(list(1.2,4.9),10000)
sys.time(r <- f(10000,v))
print(c(length(r),sum(unlist(r))))

v <- rep(c("1.2","4.9"),10000)
sys.time(r <- f(10000,v))
print(c(length(r),sum(as.numeric(r))))


test.name <- "vec-str-en-del.delete"

f <- test.cmp (function (n,v)
{ for (i in 1:n) v[[100+i]] <- NULL
  v
})

v <- rep(list(1.2,4.9),10000)
sys.time(r <- f(8000,v))
print(c(length(r),sum(unlist(r))))

f <- test.cmp (function (n,v)
{ for (i in 1:n) v[100+i] <- NULL
  v
})

v <- rep(list(1.2,4.9),10000)
sys.time(r <- f(8000,v))
print(c(length(r),sum(unlist(r))))

v <- rep(list(1.2,4.9),10000)
names(v) <- rep("abc",10000)
sys.time(r <- f(8000,v))
print(c(length(r),sum(unlist(r))))

f <- test.cmp (function (n,v)
{ for (i in 1:n) v[1000:1002] <- NULL
  v
})

v <- rep(list(1.2,4.9),10000)
sys.time(r <- f(6000,v))
print(c(length(r),sum(unlist(r))))


test.name <- "vec-str-en-del.str-cat"

f <- test.cmp (function (n,s1,s2,s3)
{
  for (i in 1:n) v <- c(s1,s2,s3)
  v
})

s1 <- rep("1.2",1000)
s2 <- rep("3.4",1001)
s3 <- rep("5.6",1001)

sys.time(r <- f(50000,s1,s2,s3))
print(c(length(r),sum(as.numeric(r))))

# correctness checks.

a <- c("abc","def","ghi","xxx")
b <- a
a[[6]] <- "yyy"
print(c(a,b))

a <- list(a=3,b=T,c="aa",d=list(1),e=4L,f=3,g=90)
b <- a
b[["b"]] <- NULL
print(b)

b[2:3] <- NULL
print(b)

b[c(1,4)] <- NULL
print(b)
