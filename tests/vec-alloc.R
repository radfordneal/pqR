# Test possible bugs involving allocation and use of vectors, such as 
# incorrect computation of required memory.  Also tests allocation and
# reallocation for "c".  Finally, exercises .. and "along" for "for".
#
# Added for pqR, 2018, Radford M. Neal.


# Test of allocation.

lengths <- c(0..17, 28..33, 60..65, 124..129, 252..257)
types <- c("integer","numeric","complex","character","list")

R <- list()

for (r in 1..10) {

    cat("vector allocation test, repetition",r,"\n")

    for (i in 1..length(lengths)) {
        R[[i]] <- list()
        for (j in 1..length(types)) {
            V <- vector(types[j],lengths[i])
            for (k in 1..lengths[i]) V[k] <- as.integer(10000*i + 1000*j + k)
            R[[i]][[j]] <- V
        }
    }

    if (r == 1) { cat ("\n"); print(R[[3]]) }
    
    for (i along lengths) {
        for (j along types) {
            for (k in 1..lengths[i]) {
                stopifnot (as.integer(R[[i]][[j]][[k]])
                  == as.integer(10000*i + 1000*j + k))
                R[[i]][[j]][[k]] <- as.integer(R[[i]][[j]][[k]]) +
                         as.integer(1000000000*(i%%2) + 100000000*j + 300000*k)
            }
        }
    }

    if (r == 1) { cat ("\n"); print(R[[3]]) }
    
    for (i in 1..length(lengths)) {
        for (j in 1..length(types)) {
            for (k in 1..lengths[i]) {
                stopifnot (as.integer(R[[i]][[j]][[k]])
                  == as.integer(10000*i + 1000*j + k)
                      + as.integer(1000000000*(i%%2) + 100000000*j + 300000*k))
            }
        }
    }

    if (r == 1) { cat ("\n"); print(R[[3]]); print(lapply(R[[4]],typeof)) }
}


# Tests of "c".

for (n in c(10L,100L,1000L,10000L,1000000L)) {  # must all be multiples of 5

    cat("c test, with n =",n,"\n")

    ivals <- 1:n  # must not have dim attribute, so not ..
    rvals <- ivals + c(rep(0.1,n/5-1),rep(0,4*n/5+1))
    
    ivalsn <- ivals
    names(ivalsn) <- paste0("N",ivals)
    rvalsn <- rvals
    names(rvalsn) <- paste0("N",ivals)

    ivals2 <- 1:(n+2)
    rvals2 <- ivals2 + c(rep(0.1,n/5-1),rep(0,4*n/5+3))

    ivals2n <- ivals2
    names(ivals2n) <- paste0("N",ivals2)
    rvals2n <- rvals2
    names(rvals2n) <- paste0("N",ivals2)

    stopifnot (identical (c(ivals,n+1L,n+2L), ivals2))
    stopifnot (identical (c(ivals,c(n+1L,n+2L)), ivals2))
    stopifnot (identical (c(rvals,n+1,n+2L), rvals2))
    stopifnot (identical (c(rvals,c(n+1L,n+2)), rvals2))

    if (n == 100) {
        stopifnot (identical (c(ivalsn,N101=n+1L,N102=n+2L), ivals2n))
        stopifnot (identical (c(ivalsn,c(N101=n+1L,N102=n+2L)), ivals2n))
        stopifnot (identical (c(rvalsn,N101=n+1,N102=n+2), rvals2n))
        stopifnot (identical (c(rvalsn,c(N101=n+1L,N102=n+2L)), rvals2n))
    }

    stopifnot (identical (c(ivals,ivals), rep(ivals,times=2)))
    stopifnot (identical (c(rvals,rvals), rep(rvals,times=2)))
    stopifnot (identical (c(ivalsn,ivalsn), rep(ivalsn,times=2)))
    stopifnot (identical (c(rvalsn,rvalsn), rep(rvalsn,times=2)))
    
    stopifnot (identical (c(ivals,ivals,ivals), rep(ivals,times=3)))
    stopifnot (identical (c(rvals,rvals,rvals), rep(rvals,times=3)))
    stopifnot (identical (c(ivalsn,ivalsn,ivalsn), rep(ivalsn,times=3)))
    stopifnot (identical (c(rvalsn,rvalsn,rvalsn), rep(rvalsn,times=3)))
    
    stopifnot (identical (ivals, c(ivals[1..n/5],ivals[n/5+1..n])))
    stopifnot (identical (rvals, c(rvals[1..n/5],rvals[n/5+1..n])))
    stopifnot (identical (ivalsn, c(ivalsn[1..n/5],ivalsn[n/5+1..n])))
    stopifnot (identical (rvalsn, c(rvalsn[1..n/5],rvalsn[n/5+1..n])))

    stopifnot (identical (rvals, c(rvals[1..n/5],ivals[n/5+1..n])))
    stopifnot (identical (rvalsn, c(rvalsn[1..n/5],ivalsn[n/5+1..n])))
    
    stopifnot (identical (ivals, c(ivals[1..n/5-1],n%/%5L,ivals[n/5+1..n])))
    stopifnot (identical (rvals, c(rvals[1..n/5-1],n%/%5L,rvals[n/5+1..n])))
    
    if (n == 100) {
        stopifnot (
          identical (ivalsn, c(ivalsn[1..n/5-1],N20=n%/%5L,ivalsn[n/5+1..n])))
        stopifnot (
          identical (rvalsn, c(rvalsn[1..n/5-1],N20=n%/%5L,rvalsn[n/5+1..n])))
    }
    
    stopifnot (
      identical (ivals, 
                 c(ivals[1..n/5],ivals[n/5+1..2*n/5],ivals[2*n/5+1..n])))
    stopifnot (
      identical (rvals, 
                 c(rvals[1..n/5],rvals[n/5+1..2*n/5],rvals[2*n/5+1..n])))
    stopifnot (
      identical (ivalsn, 
                 c(ivalsn[1..n/5],ivalsn[n/5+1..2*n/5],ivalsn[2*n/5+1..n])))
    stopifnot (
      identical (rvalsn, 
                 c(rvalsn[1..n/5],rvalsn[n/5+1..2*n/5],rvalsn[2*n/5+1..n])))
}
