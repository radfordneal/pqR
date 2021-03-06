####  eval / parse / deparse / substitute  etc

##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Subject: Re: source() / eval() bug ??? (PR#96)
##- Date: 20 Jan 1999 14:56:24 +0100
e1 <- parse(text='c(F=(f <- .3), "Tail area" = 2 * if(f < 1) 30 else 90)')[[1]]
e1
str(eval(e1))
mode(e1)

( e2 <- quote(c(a=1,b=2)) )
names(e2)[2] <- "a b c"
e2
parse(text=deparse(e2))

##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Date: 22 Jan 1999 11:47

( e3 <- quote(c(F=1,"tail area"=pf(1,1,1))) )
eval(e3)
names(e3)

names(e3)[2] <- "Variance ratio"
e3
eval(e3)


##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Date: 2 Sep 1999

## The first failed in 0.65.0 :
attach(list(x=1))
evalq(dim(x) <- 1,as.environment(2))
dput(get("x", envir=as.environment(2)), control="all")

e <- local({x <- 1;environment()})
evalq(dim(x) <- 1,e)
dput(get("x",envir=e), control="all")

### Substitute, Eval, Parse, etc

## PR#3 : "..." matching
## Revised March 7 2001 -pd
A <- function(x, y, ...) {
    B <- function(a, b, ...) { match.call() }
    B(x+y, ...)
}
(aa <- A(1,2,3))
all.equal(as.list(aa),
          list(as.name("B"), a = expression(x+y)[[1]], b = 3))
(a2 <- A(1,2, named = 3)) #A(1,2, named = 3)
all.equal(as.list(a2),
          list(as.name("B"), a = expression(x+y)[[1]], named = 3))

CC <- function(...) match.call()
DD <- function(...) CC(...)
a3 <- DD(1,2,3)
all.equal(as.list(a3),
          list(as.name("CC"), 1, 2, 3))

## More dots issues: March 19 2001 -pd
## Didn't work up to and including 1.2.2

f <- function(...) {
	val <- match.call(expand.dots=FALSE)$...
        x <- val[[1]]
	eval.parent(substitute(missing(x)))
}
g <- function(...) h(f(...))
h <- function(...) list(...)
k <- function(...) g(...)
X <- k(a=)
all.equal(X, list(TRUE))

## Bug PR#24
f <- function(x,...) substitute(list(x,...))
deparse(f(a, b)) == "list(a, b)" &&
deparse(f(b, a)) == "list(b, a)" &&
deparse(f(x, y)) == "list(x, y)" &&
deparse(f(y, x)) == "list(y, x)"

tt <- function(x) { is.vector(x); deparse(substitute(x)) }
a <- list(b=3); tt(a$b) == "a$b" # tends to break when ...


## Parser:
1 <
    2
2 <=
    3
4 >=
    3
3 >
    2
2 ==
    2
## bug till ...
1 !=
    3

all(NULL == NULL)

## PR #656 (related)
u <- runif(1);	length(find(".Random.seed")) == 1

MyVaR <<- "val";length(find("MyVaR")) == 1
rm(MyVaR);	length(find("MyVaR")) == 0


## Martin Maechler: rare bad bug in sys.function() {or match.arg()} (PR#1409)
callme <- function(a = 1, mm = c("Abc", "Bde")) {
    mm <- match.arg(mm); cat("mm = "); str(mm) ; invisible()
}
## The first two were as desired:
callme()
callme(mm="B")
mycaller <- function(x = 1, callme = pi) { callme(x) }
mycaller()## wrongly gave `mm = NULL'  now = "Abc"


## Test pqR fix to DispatchOrEval.

a <- 0
class(a) <- "fred"
seq.fred <- function (x, y) deparse(substitute(y))
seq(a,1+2)      # should both be "1 + 2"
seq.int(a,1+2)  # ...  but this one used to be "3"


## Tests of 'for', including 'along', mostly like in help("for"), but 
## here using stopifnot.

s <- character()
for (i in s) 0
print(i)
stopifnot(identical(i,NULL))

s <- character()
for (i along letters) s[i] <- paste0(".",letters[i])
print(s)
stopifnot(identical(s,paste0(".",letters)))
print(i)
stopifnot(identical(i,length(letters)))

M <- matrix(0,nrow=4,ncol=5)
for (i, j along M) M[i,j] <- i*j
print(M)
stopifnot(identical(M,outer(1:4,1:5)))
print(c(i,j))
stopifnot(identical(c(i,j),c(4L,5L)))

v <- c(7,0,-1,3,-2,0,1,-3,9)
for (i along v) {
    if (v[i] < 0) {
        v[i] <- -v[i]
        next
    }
    if (v[i] == 1)
        break
    v[i] <- v[i] + if (v[i] < 5) 2 else 1
}
print(v) 
stopifnot(identical(v,c(8,2,1,5,2,2,1,-3,9)))
print(i)
stopifnot(identical(i,7L))


## Special 'for' tests about when loop variables are modified, or removed,
## or have contents referenced from elsewhere.

for (i in 1:5) {
    print(i)
    i <- 10
}

for (i in 1:5) {
    print(i)
    if (i==3) j <- i
}

print(i)
print(j)

for (i in 1:5) {
    print(i)
    rm(i)
    i <- 10
}

print(i)

M <- matrix(0,3,2)

for (i,j along M) {
    print(c(i,j))
    i <- 1000
    j <- 2000
}

print(c(i,j))

for (i, j along M) {
    print(c(i,j))
    rm(i,j)
}

for (i, j along M) {
    print(c(i,j))
    if (i==2 && j==2) { a <- i; b <- j }
}

print(c(i,j))
print(c(a,b))


## Tests of internal DispatchOrEval procedure.

a <- 5
print(c(a,7))
print(c(a,quote(cat("Hi!\n"))))

a <- 5
class(a) <- "fred"
print(c(a,7))
print(c(a,quote(cat("Hi!\n"))))

c.fred <- function (...) 999
print(c(a,7))
print(c(7,a))

setClass ("bert",prototype=integer(1),contains="numeric")

a <- new("bert",5)

print(c(a,7))
print(c(a,quote(cat("Hi!\n"))))

f <- function (...) c(...,7)
print(f(a))
print(f({cat("Howdy!\n");a}))

setClass ("george",prototype=integer(1),contains="numeric")
setMethod ("c","george",function(x,y)777)

print(c(a,7))
print(c(a,quote(cat("Hi!\n"))))

f <- function (...) c(...,7)
print(f(a))
print(f({cat("Howdy!\n");a}))

b <- new("george",8)
print(c(b,12))


## Tests of 'missing'.

f <- function (x,y) c(missing(x),missing(y))
g <- function (a,b) f(a,b)

f(1,2)
f(,2)
f(1,)
f(_,2)
f(1,_)
f()
f(_,_)

g(1,2)
g(,2)
g(1,)
g(_,2)
g(1,_)
g()
g(_,_)

f <- function (x) c(missing(x),missing_from_underline(x))
g <- function (y) f(y)
h <- function (z) g(z)

f(1)
g(1)
h(1)

f()
g()
h()

f(x=)
g(y=)
h(z=)

f(_)
g(_)
h(_)

f <- function (x) { try(x); c(missing(x),missing_from_underline(x)) }
g <- function (y) f(y)
h <- function (z) g(z)

f(pi[1,1])
g(pi[1,1])
h(pi[1,1])

d <- function () { 
    delayedAssign("A",B); delayedAssign("B",C); delayedAssign("C",A);
    c(missing(A),missing_from_underline(A))
}

d()

e <- function (w=x,x=y,y=z,z=w) c(missing(w),missing_from_underline(w))

e()

f <- function (x=99) { print(missing(x)); x }
f(1)
f()
f(_)

h <- function (y=88) f(y)
h(1)
h()
h(_)

# make sure we get errors when we should, and not when we shouldn't.

f <- function (x) x
try(f())

f <- function (x,y) mget("x",sys.frame(sys.nframe()))[[1]]
xx <- f(,1)
g <- function (a) length(a)
try(g(xx))

h <- function (x) length(x)
j <- function (x) h(x)
try(j())

lapply(function(x)0,all.names)

M <- quote(function (x) 0)[[2]]
all.names(M[[1]])

F <- function (e) typeof(e)
lapply(M,F)
G <- function (e) invisible(e)
lapply(M,G)


# Test ops with zero-size matrix.

print (c(3,4) * matrix(0,2,0))
print (matrix(0,2,0) * c(3,4))
print (matrix(0,2,0) * matrix(0,2,0))

x <- matrix(0,2,0)

print (c(3,4) * x)
print (x * c(3,4))
print (x * x)

print (c(3,4) < matrix(0,2,0))
print (matrix(0,2,0) < c(3,4))
print (matrix(0,2,0) < matrix(0,2,0))

x <- matrix(0,2,0)

print (c(3,4) < x)
print (x < c(3,4))
print (x < x)

x <- matrix(logical(0),2,0)

print (c(TRUE,FALSE) & x)
print (x & c(TRUE,FALSE))
print (x & x)


# Test ops with vector and 1-by-1 matrix.

y <- matrix(23,1,1)
v <- c(20,10,30)

print(v+y)
print(y+v)

print(v<y)
print(y<v)

y <- matrix(TRUE,1,1)
v <- c(TRUE,FALSE,TRUE)

print(v&y)
print(y&v)


# Test zero-length ops.

y <- matrix(numeric(),nrow=4,ncol=0)
print(y+1)
print(y+c(2,3))
print(y==1)
print(y==c(2,3))

y <- matrix(logical(),nrow=4,ncol=0)
print(y&TRUE)
print(y&c(TRUE,FALSE))

y <- matrix(list(),nrow=4,ncol=0)
print(y==1)
print(y==c(2,3))
print(y==y)

print(baseenv()==NULL)


# test that argument values are not destroyed by later evaluation.

f <- function () {

    a <- c(10,20); print(a+(a[2]<-7))
    a <- c(10,20); print(a>(a[2]<-7))
    a <- c(10,20); print(c(a,a[2]<-7,a,a[1]<-8))

    M <- matrix(11:14,2,2); a <- c(2,2); print(M[a,a[2]<-1])
    M <- matrix(11:14,2,2); a <- c(2,2); M[a,a[2]<-1] <- 7; print(M)

    M <- matrix(11:14,2,2); a <- c(2); print(M[[a,a[1]<-1]])
    M <- matrix(11:14,2,2); a <- c(2); M[[a,a[1]<-1]] <- 7; print(M)

    p <- c(4L,2L,2L,3L,2L)
    p[p] <- 1L
    print(p)
}

f()


# Test that overflow of the scalar stack is handled properly.  Should continue
# to give the right answer, but be a bit slower. Note that currently 128 entries
# fit on the scalar stack.

f <- function (x) {
    if (x == 0) {
        # Enable statement below to see performance change when stack overflows
        # print(system.time(for (i in 1:2000000) r <- 3*4+(1+2)*(1*2+3*4+7*8)))
        0
    }
    else 
        2*x + f(x-1)
}

for (x in c(3,20,124,125,126,127,128,129,200,300,600)) {
    r <- f(x)
    print (c(x, r, x*(x+1)))
    stopifnot(r == x*(x+1))
    cat("\n")
}


# Test data.frame and as.data.frame as applied to lists.

L <- list(x=c(3,5),y=c(TRUE,FALSE),c("xx","yy"))
(df<-as.data.frame(L))
unlist(lapply(df,typeof))
(df<-data.frame(L))
unlist(lapply(df,typeof))
(df<-data.frame(L,stringsAsFactors=FALSE))
unlist(lapply(df,typeof))
(df<-data.frame(Q=L))
unlist(lapply(df,typeof))
(df<-data.frame(Q=L,stringsAsFactors=FALSE))
unlist(lapply(df,typeof))
