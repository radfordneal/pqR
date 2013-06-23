source("time.r")
 
# FIT MLP NETWORK USING SIMPLE GRADIENT ASCENT.  The arguments are a vector
# of 0/1 responses for training cases, a matrix of inputs for training cases,
# two learning rates (for parameters on connections to hidden units and the
# output unit), the number of iterations of gradient ascent to do.  
#
# These arguments are followed by either an argument, q, giving the number 
# of hidden units, or a vector of initial values for parameters (from which q 
# can be deduced).  If no initial parameter values are supplied, values are 
# randomly chosen independently from Gaussian distributions with mean zero and
# standard deviation 0.01, except that the bias for the output unit is set to 
# the logit of the fraction of training cases that are in class 1.  
#
# The result returned is a list with element ll giving the log likelihood
# for each iteration, element p1 giving the matrix of probabilities for
# class 1 for each iteration (rows) and training case (columns), and element
# params giving the values of the network parameters for each iteration (rows).
# Note that these results are for states after each iteration; the initial 
# values are not included.
#
# Note that an initial value can be supplied via the init argument that is the
# params element from the result of a previous call of mlp.train.

mlp.train = test.cmp (function (y, X, eta1, eta2, iters, q=NULL, init=NULL)
{
  n = nrow(X)
  p = ncol(X)

  if (is.null(q))
  { q = as.integer((length(init)-1) / (p+2))
  }

  M = (q+1) + q*(p+1)

  if (is.null(init))
  { init = c (log(mean(y)/(1-mean(y))), rnorm(M-1,0,0.01) )
  }

  if (length(init)!=M)
  { stop("Initial parameter vector is the wrong length")
  }

  eta = c (rep(eta2,q+1), rep(eta1,q*(p+1)) )
  
  ll = rep(NA,iters)
  params = matrix(NA,iters,M)
  p1 = matrix(NA,iters,n)

  current = init

  fw = mlp.forward(X,q,current)

  for (iter in 1:iters)
  { 
    bk = mlp.backward(y,X,q,current,fw)
    gr = mlp.grad(X,q,fw,bk)

    current = current + eta*gr

    params[iter,] = current

    fw = mlp.forward(X,q,current)
    p1[iter,] = 1/(1+exp(-fw$o))
    ll[iter] = mlp.log.likelihood(y,fw$o)
  }
  
  list (ll=ll, p1=p1, params=params)
})


# FORWARD NETWORK COMPUTATIONS.  Given the matrix of inputs, the number of
# hidden units, and the network parameters, this function returns a list with
# a matrix of hidden unit inputs (s) for all training cases, a corresponding 
# matrix of hidden unit values (h), and a vector of output unit values (o).

mlp.forward = test.cmp (function (X, q, params)
{
  n = nrow(X)
  p = ncol(X)

  if (length(params) != (q+1) + q*(p+1)) 
  { stop("Parameter vector is the wrong length")
  }
 
  beta0 = params[1]
  beta = params[2:(q+1)]
  gamma0 = params[(q+2):(2*q+1)]
  gamma0m = matrix(gamma0,n,q,byrow=T)
  gamma = matrix(params[(2*q+2):length(params)],p,q)

  s = X %*% gamma + gamma0m
  h = tanh(s)
  o = h %*% beta + beta0

  list(s=s, h=h, o=o)
})


# BACKWARD NETWORK COMPUTATIONS.  Given the training targets and inputs, the 
# number of hidden units, the network parameters, and the results of the 
# forward computation, this function returns a list of vectors and matrices of
# partial derivatives with respect to unit values, for each training case.

mlp.backward = test.cmp (function (y, X, q, params, fw)
{
  beta = params[2:(q+1)]

  p1 = 1/(1+exp(-fw$o))

  dl.do = y-p1
  dl.dh = dl.do %*% beta
  dl.ds = (1-fw$h^2) * dl.dh

  list (dl.do=dl.do, dl.dh=dl.dh, dl.ds=dl.ds)
})


# COMPUTE GRADIENT OF LOG LIKELIHOOD WITH RESPECT TO PARAMETERS.  Takes
# as arguments the matrix of inputs, the number of hidden units, and the 
# results of the forward and backward computations.  Returns a vector of 
# partial derivatives of the log likelihood with respect to the parameters.

mlp.grad = test.cmp (function (X, q, fw, bk)
{
  p = ncol(X)

  dl.dbeta0 = sum (bk$dl.do)
  dl.dbeta = t(fw$h) %*% bk$dl.do
  dl.dgamma0 = apply (bk$dl.ds, 2, sum)
  dl.dgamma = matrix(NA,p,q)
  for (j in 1:p)
  { dl.dgamma[j,] = X[,j] %*% bk$dl.ds
  }

  c (dl.dbeta0=dl.dbeta0, dl.dbeta=dl.dbeta, dl.dgamma0=dl.dgamma0, 
     dl.dgamma=dl.dgamma)
})


# COMPUTE THE LOG LIKELIHOOD GIVEN THE NETWORK OUTPUTS.  

mlp.log.likelihood = test.cmp (function (y, o)
{
  sum(-log(1+exp(-(2*y-1)*o)))
})

set.seed(1)

X <- matrix(rnorm(12*200),200,12)
y <- 1 / (1 + exp(-(X[,1]-X[,2]^2-4*sin(2*X[,3]-0.3*X[,4]*X[,5]))))

test.name <- "mlp"

n.iters <- 1500

sys.time (res <- mlp.train (y, X, 0.004, 0.004, n.iters, 20))

print(round(res$ll[seq(1,n.iters,length=20)],2))
print(round(res$params[n.iters,],4))

