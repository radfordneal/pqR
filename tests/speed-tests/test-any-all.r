source("time.r")
 
a <- seq(0.001,1,length=1000)
anan <- a
anan[300] <- 0/0
d <- c(FALSE,TRUE)

test.name <- "any-all.any"

b <- rep(FALSE,1000)

f <- test.cmp (function () {for (i in 1:5000000) x <- any(d); x})
sys.time(x<-f()); print(x)

b[700] <- TRUE
f <- test.cmp (function () {for (i in 1:500000) x <- any(b); x})
sys.time(x<-f()); print(x)

b[700] <- NA
f <- test.cmp (function () {for (i in 1:500000) x <- any(b); x})
sys.time(x<-f()); print(x)

test.name <- "any-all.all"

b <- rep(T,1000)

f <- test.cmp (function () {for (i in 1:5000000) x <- all(d); x})
sys.time(x<-f()); print(x)

b[700] <- F
f <- test.cmp (function () {for (i in 1:500000) x <- all(b); x})
sys.time(x<-f()); print(x)

b[700] <- NA
f <- test.cmp (function () {for (i in 1:500000) x <- all(b); x})
sys.time(x<-f()); print(x)

test.name <- "any-all.relop-is"

f <- test.cmp (function () {for (i in 1:100000) x <- any(a>0.2); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:100000) x <- all(a<0.2); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:100000) x <- any(a>0.7); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:100000) x <- all(a<0.7); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:200000) x <- any(is.na(a)); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:200000) x <- any(is.nan(anan)); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:200000) x <- any(is.infinite(a)); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:200000) x <- all(is.finite(a)); x})
sys.time(x<-f()); print(x)

f <- test.cmp (function () {for (i in 1:200000) x <- all(is.na(a)); x})
sys.time(x<-f()); print(x)


# Correctness checks.

ar <- c(3,NA,0/0,4,1/0)
ai <- as.integer(ar)
ac <- ar+1i

print(rbind(ar=is.na(ar),ai=is.na(ai),ac=is.na(ac)))
print(rbind(ar=is.nan(ar),ai=is.nan(ai),ac=is.nan(ac)))
print(rbind(ar=is.finite(ar),ai=is.finite(ai),ac=is.finite(ac)))
print(rbind(ar=is.infinite(ar),ai=is.infinite(ai),ac=is.infinite(ac)))

print(c(ar=any(is.na(ar)),ai=any(is.na(ai)),ac=any(is.na(ac))))
print(c(ar=any(is.nan(ar)),ai=any(is.nan(ai)),ac=any(is.nan(ac))))
print(c(ar=any(is.finite(ar)),ai=any(is.finite(ai)),ac=any(is.finite(ac))))
print(
 c(ar=any(is.infinite(ar)),ai=any(is.infinite(ai)),ac=any(is.infinite(ac))))

print(c(ar=all(is.na(ar)),ai=all(is.na(ai)),ac=all(is.na(ac))))
print(c(ar=all(is.nan(ar)),ai=all(is.nan(ai)),ac=all(is.nan(ac))))
print(c(ar=all(is.finite(ar)),ai=all(is.finite(ai)),ac=all(is.finite(ac))))
print(
 c(ar=all(is.infinite(ar)),ai=all(is.infinite(ai)),ac=all(is.infinite(ac))))

bi <- 1:3
br <- bi + 0.1
bc <- br + 1i

print(c(br=any(is.na(br)),bi=any(is.na(bi)),bc=any(is.na(bc))))
print(c(br=any(is.nan(br)),bi=any(is.nan(bi)),bc=any(is.nan(bc))))
print(c(br=any(is.finite(br)),bi=any(is.finite(bi)),bc=any(is.finite(bc))))
print(
 c(br=any(is.infinite(br)),bi=any(is.infinite(bi)),bc=any(is.infinite(bc))))

print(c(br=all(is.na(br)),bi=all(is.na(bi)),bc=all(is.na(bc))))
print(c(br=all(is.nan(br)),bi=all(is.nan(bi)),bc=all(is.nan(bc))))
print(c(br=all(is.finite(br)),bi=all(is.finite(bi)),bc=all(is.finite(bc))))
print(
 c(br=all(is.infinite(br)),bi=all(is.infinite(bi)),bc=all(is.infinite(bc))))
