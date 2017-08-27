# Test the 'sample' function.  Results should be identical to those obtained
# with previous versions, for the sake of reproducibility.
#
# Added for pqR, 2017, Radford M. Neal.

set.seed(123)

# k = 0

sample(0,0)
sample(1,0)
sample(10,0)

# k = 1

sample(11,1)
sample(letters[1:12],1)

# k = 2

sample(13,2)
sample(letters[1:14],2)

# k > 2, small compared to n

sample(15,3)
sample(letters[1:16],3)

R <- matrix(0,100,10)
for (i in 1:100) R[i,] <- sample(70,10)
R

R <- matrix(0,100,10)
for (i in 1:100) R[i,] <- sample(7000,10)
R

R <- numeric(100)
for (i in 1:100) R[i] <- sum(sample(7000,3000))
R

R <- matrix(0,100,7)
for (i in 1:100) R[i,] <- sample(70000,7)
R

R <- numeric(100)
for (i in 1:100) R[i] <- sum(sample(70000,33333))
R

# k > 2, not small compared to n

sample(15)
sample(15,13)
sample(letters)
sample(letters,22)

R <- numeric(100)
for (i in 1:100) R[i] <- sum(sample(7000))
R

R <- numeric(100)
for (i in 1:100) R[i] <- sum(sample(7000,6500))
R

R <- numeric(100)
for (i in 1:100) R[i] <- sum(sample(7000,6999))
R


# with replacement

sample(letters,19,replace=TRUE)
sample(letters,50,replace=TRUE)


# with probabilities

sample(letters,18,prob=1:26)
sample(letters,19,prob=26:1)

sample(letters,29,prob=1:26,replace=TRUE)
sample(letters,29,prob=26:1,replace=TRUE)
sample(letters,100,prob=1:26,replace=TRUE)
sample(letters,100,prob=26:1,replace=TRUE)
