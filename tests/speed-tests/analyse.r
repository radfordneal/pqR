# SUPPORT FOR ANALYSING TIMING TESTS.


# READ A FILE OF TEST TIMES.  Stores the times in the global variable "td".

read.times <- function (file="test-times")
{ td <<- read.table(file,head=FALSE)
  colnames(td) <<- 
    c("date","time","se","su","ss","t","n","conf","hlp","cmp","rep")
}


# SELECT A SUBSET OF TESTS.  The "td" argument is the data frame with
# the test results, defaulting to the global td variable defined above.
# Other arguments are vectors of values for columns of td (except for se,
# su, and ss, for which an exact match cannot be expected).  The "tpat"
# argument may be used to select only tests matching a regular expression.
# A data frame containing only the tests with values matching those specified 
# is returned.

sel <- function (conf, hlp, cmp, t, n, date, time, rep, tpat, td=get("td",1))
{
  w <- base::rep(TRUE,nrow(td))

  if (!missing(date))     w <- w & td$date %in% date
  if (!missing(time))     w <- w & td$time %in% time
  if (!missing(t))        w <- w & td$t %in% t
  if (!missing(n))        w <- w & td$n %in% n
  if (!missing(conf))     w <- w & td$conf %in% conf
  if (!missing(hlp))      w <- w & td$hlp %in% hlp
  if (!missing(cmp))      w <- w & td$cmp %in% cmp
  if (!missing(rep))      w <- w & td$rep %in% rep
  if (!missing(tpat))     w <- w & grepl(tpat,td$t)

  td[w,]
}


# PRINT NUMBER OF TEST RUNS BY CONF, CMP, and REP.

run.count <- function (...)
{ 
  td <- sel(...)
  table(td[,c("conf","cmp","rep")])
}


# PLOT TIME RATIOS.  

plot.ratios <- function (ts1, ts2, per.page=150, pages=NULL, title="", ...)
{
  n <- nrow(ts1)

  ratios <- calc.ratios(ts1,ts2)

  if (is.null(pages)) pages <- ceiling (n / per.page)
  per.page <- ceiling (n / pages)

  par(mar=c(5,2,4, if (per.page>20) 10 else 14))

  last <- floor(seq(0, n, length=pages+1))
  for (j in 2:length(last))
  {
    dev.hold()

    plot (NULL, type="n",
          xlab="Elapsed time ratios (log scale)", ylab="",
          xlim=range(ratios), ylim=c(1,per.page),
          yaxt="n", pch=20, log="x", ...) 
    abline(v=c(2,5,10,20,50,1/2,1/5,1/10,1/20,1/50),col="grey")
    abline(v=1)
    points(rev(ratios[(last[j-1]+1):last[j]]), 1:(last[j]-last[j-1]), 
          xlab="Elapsed time ratios (log scale)", ylab="",
          xlim=range(ratios), ylim=c(1,per.page),
          yaxt="n", pch=20, ...)

    for (i in (last[j-1]+1):last[j])
    { if (ts1$n[i]==1) 
      { mtext (paste(" ",ts1$t[i]), 4, at=last[j]+1-i, las=1, 
               cex = if (per.page>20) 0.5 else 1)
        abline(h=last[j]+1.5-i,col="gray")
      }
    }

    if (j==2) title (title, outer=TRUE, line=-2.5)

    dev.flush()
  }
}


# CALCULATE TIME RATIOS.  Attaches names for informative printing.

calc.ratios <- function (ts1, ...)
{
  n <- nrow(ts1)
  ratios <- matrix(NA,n,0)
  rownames(ratios) <- paste(ts1$t,":",ts1$n,sep="") 

  for (ts2 in list(...))
  {
    if (nrow(ts2)!=n)
    { stop("selections not the same size")
    }

    for (i in 1:n)
    { if (ts1$t[i]!=ts2$t[i] || ts1$n[i]!=ts2$n[i])
      { stop("selections don't match: ",ts1$t[i],ts1$n[i]," vs ",ts2$t[i],ts2$n[i])
      }
    }

    ratios <- cbind (ratios, round(ts1$se/ts2$se,3))
  }
  
  ratios
}
