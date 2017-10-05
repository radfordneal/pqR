# Tests of c / unlist / rbind / cbind.

# Currently concentrates on handling of names in unlist.

# Added for pqR, 2017, Radford M. Neal.


unlist(list(FALSE,10,20,30L))
unlist(list(FALSE,10,list(20,21),30L))
unlist(list(FALSE,10,list(20,21i),30L))
unlist(list(FALSE,10,list(20,21i),30L,"xx"))

unlist(list(1,a=list(3,4)))
unlist(list(1,list(3,4),b=9))
unlist(list(a=list(x=1),b=list(3,4)))
unlist(list(a=list(x=1),b=list(x=3,4,5)))
unlist(list(a=list(x=1),b=list(x=3,4)))
unlist(list(a=list(x=1),b=list(3,y=4)))
unlist(list(a=list(3,list(c(x=1,y=2,z=3)),4,list(c(x=10)))))
unlist(list(3,xx=list(a="p","q","r"),yy=list(list(),4,5),zz=list(),5))

unlist (list(a=list(x=0,1,y=2,3)))
unlist (list(a=list(x=0,1,y=2)))

unlist(list(x=list(2,a=3)))
unlist(list(x=c(2,a=3)))

unlist(pairlist(a=3,b=pairlist(x="p","q",y="r"),c=pairlist(TRUE,FALSE)))

unlist(function (x) 0)
unlist(c(a=1,b=2),use.names=FALSE)

c(1,2,3)
c(1,c(x=3,y=4))
c(1,a=c(3,4))
c(x=c(2,a=3))

rbind(a=c(x=1,y=2),b=2:3)
cbind(a=c(x=1,y=2),b=2:3)
