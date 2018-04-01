# Test possible bugs involving allocation and use of vectors, such as 
# incorrect computation of required memory.
#
# Added for pqR, 2018, Radford M. Neal.

lengths <- c(0..17, 28..33, 60..65, 124..129, 252..257)
types <- c("integer","numeric","complex","character","list")

R <- list()

for (r in 1..10) {

    cat("Repetition",r,"\n")

    for (i in 1..length(lengths)) {
        R[[i]] <- list()
        for (j in 1..length(types)) {
            V <- vector(types[j],lengths[i])
            for (k in 1..lengths[i]) V[k] <- as.integer(10000*i + 1000*j + k)
            R[[i]][[j]] <- V
        }
    }

    if (r == 1) { cat ("\n"); print(R[[3]]) }
    
    for (i in 1..length(lengths)) {
        for (j in 1..length(types)) {
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
