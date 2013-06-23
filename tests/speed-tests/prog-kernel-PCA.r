source("time.r")
 
# KERNEL PCA.  Finds the projections of test points onto the first m principal
# components found from training points, using the kernel exp(-rho^2*d^2),
# where d is Euclidean distance.

kpca <- test.cmp (function (Xtrn, Xtst, rho, m)
{
  n <- nrow(Xtrn)

  K <- matrix(NA,n,n)
  for (i in 1:n)
  { for (j in i:n)
    { K[i,j] <- K[j,i] <- exp(-rho^2*Xtrn[i,]%*%Xtrn[j,])
    }
  }

  M <- diag(n) - matrix(1/n,n,n)
  e <- eigen (M %*% K %*% M, symmetric=TRUE)

  prj <- matrix(NA,nrow(Xtst),m)
  Ks <- colSums(K)/n
  for (t in 1:nrow(Xtst))
  { k <- rep(NA,n)
    for (i in 1:n)
    { k[i] <- exp(-rho^2*Xtrn[i,]%*%Xtst[t,])
    }
    for (w in 1:m)
    { prj[t,w] <- (k - Ks) %*% M %*% e$vectors[,w] / sqrt(e$values[w])
    }
  }

  prj
})

# TEST OF KERNEL PCA FUNCTION.

n <- 100

X <- matrix(NA,n,2)
class <- rep(NA,n)

set.seed(1)

for (i in 1:(n))
{
  theta <- runif(1,0,2*pi)
  r <- runif(1,1,1.75)
  X[i,1] <- r*cos(theta)
  X[i,2] <- 1.1*r*sin(theta)

  class[i] <- as.numeric(runif(1)<0.5)
  if (class[i])
  { X[i,1] <- 0.8*X[i,1]+0.1
    X[i,2] <- 0.7*X[i,2]-0.2
  }
}

class.trn <- class[1:n]
class.tst <- class[(n+1):(n)]

test.name <- "kernel-PCA"

sys.time (for (i in 1:25) prj <- kpca (X, X, 1, 3))

print(coef(glm(class~prj,family="binomial")))
