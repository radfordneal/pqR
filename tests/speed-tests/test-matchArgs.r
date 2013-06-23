source("time.r")
 
test.name <- "matchArgs.2"

f <- test.cmp (function (abcd,wxyz) 10*abcd+wxyz)
f0 <- test.cmp (function (abcd,wxyz) abcd)

h <- test.cmp (function ()
  {for (i in 1:1000000) r <- f(1,2); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:1000000) r <- f(wxyz=2,abcd=1); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:1000000) r <- f(wx=2,ab=1); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:1000000) r <- f(wx=2,1); r})
sys.time(r<-h()); print(r)

h <- test.cmp (function ()
  {for (i in 1:3000000) r <- f0(1,2); r})
sys.time(r<-h()); print(r)

test.name <- "matchArgs.8"

g <- test.cmp (function (aa,bb,cc,dd,ww=5,xx=6,yy=7,zz=8)
  10*(10*(10*(10*(10*(10*(10*aa+bb)+cc)+dd)+ww)+xx)+yy)+zz)
g0 <- test.cmp (function (aa,bb,cc,dd,ww=5,xx=6,yy=7,zz=8) aa)

h <- test.cmp (function ()
  {for (i in 1:300000) r<-g(1,2,3,4); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-g(1,2,3,4,5,6,7,8); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-g(ww=5,aa=1,zz=8,bb=2,cc=3,xx=6,yy=7,dd=4); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-g(1,2,3,4,ww=5,zz=8,xx=6,yy=7); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-g(1,2,3,4,w=5,z=8,x=6,y=7); r})
sys.time(r<-h()); print(r)

h <- test.cmp (function ()
  {for (i in 1:2000000) r<-g0(1,2,3,4,5,6,7,8); r})
sys.time(r<-h()); print(r)

test.name <- "matchArgs.10"

e <- test.cmp (function (aa,bb,cc,dd,ee,vv=5,ww=6,xx=7,yy=8,zz=9)
  10*(10*(10*(10*(10*(10*(10*(10*(10*aa+bb)+cc)+dd)+ee)+vv)+ww)+xx)+yy)+zz)

h <- test.cmp (function ()
  {for (i in 1:300000) r<-e(0,1,2,3,4); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-e(0,1,2,3,4,5,6,7,8,9); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) 
     r<-e(vv=5,ww=6,ee=4,aa=0,zz=9,bb=1,cc=2,xx=7,yy=8,dd=3); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-e(0,1,2,3,4,xx=7,ww=6,zz=9,vv=5); r})
sys.time(r<-h()); print(r)
h <- test.cmp (function ()
  {for (i in 1:300000) r<-e(0,1,2,3,4,x=7,w=6,z=9,v=5); r})
sys.time(r<-h()); print(r)
