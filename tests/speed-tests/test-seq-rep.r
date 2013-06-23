source("time.r")
 
test.name <- "seq-rep.seq"

f <- test.cmp (function (){for (i in 1:100000) r <- seq(1,10); r})
sys.time(r<-f()); print(r)
f <- test.cmp (function (){for (i in 1:100000) r <- seq(1,100); r})
sys.time(r<-f()); print(r)

f <- test.cmp (function (){for (i in 1:30000) r <- seq(1,20,by=2); r})
sys.time(r<-f()); print(r)
f <- test.cmp (function (){for (i in 1:30000) r <- seq(1,200,by=2); r})
sys.time(r<-f()); print(r)

f <- test.cmp (function (){for (i in 1:1000000) r <- seq.int(1,10); r})
sys.time(r<-f()); print(r)
f <- test.cmp (function (){for (i in 1:1000000) r <- seq.int(1,100); r})
sys.time(r<-f()); print(r)

f <- test.cmp (function (){for (i in 1:1000000) r <- seq.int(1,20,by=2); r})
sys.time(r<-f()); print(r)
f <- test.cmp (function (){for (i in 1:1000000) r <- seq.int(1,200,by=2); r})
sys.time(r<-f()); print(r)

test.name <- "seq-rep.rep"

f <- test.cmp (function (){for (i in 1:1000000) r <- rep(1.1,length=10); r})
sys.time(r<-f()); print(r)
f <- test.cmp (function (){for (i in 1:500000) r <- rep(1,length=100); r})
sys.time(r<-f()); print(r)

f <- test.cmp (function (){for (i in 1:1000000) r <- rep(c(1.1,2.1),each=5); r})
sys.time(r<-f()); print(r)
f <- test.cmp (function (){for (i in 1:500000) r <- rep(c(1,2),each=50); r})
sys.time(r<-f()); print(r)
