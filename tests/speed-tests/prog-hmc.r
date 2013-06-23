source("time.r")
 
# Hamiltonian Monte Carlo update.

HMC = test.cmp (function (U, grad_U, epsilon, L, current_q)
{
  q = current_q
  p = rnorm(length(q),0,1)  # independent standard normal variates
  current_p = p

  # Make a half step for momentum at the beginning

  p = p - epsilon * grad_U(q) / 2  

  # Alternate full steps for position and momentum

  for (i in 1:L)
  { 
    # Make a full step for the position

    q = q + epsilon * p     

    # Make a full step for the momentum, except at end of trajectory

    if (i!=L) p = p - epsilon * grad_U(q)
  }

  # Make a half step for momentum at the end.

  p = p - epsilon * grad_U(q) / 2  

  # Negate momentum at end of trajectory to make the proposal symmetric

  p = -p

  # Evaluate potential and kinetic energies at start and end of trajectory

  current_U = U(current_q)
  current_K = sum(current_p^2) / 2
  proposed_U = U(q)
  proposed_K = sum(p^2) / 2

  # Accept or reject the state at end of trajectory, returning either
  # the position at the end of the trajectory or the initial position

  if (runif(1) < exp(current_U-proposed_U+current_K-proposed_K)) 
  { 
    return (q)  # accept
  }
  else
  { 
    return (current_q)  # reject
  }
})


test_HMC1 = test.cmp (function (epsilon,L,n,seed=1)
{
  set.seed(seed)

  U = function (q) q^2/2 + sin(q)
  grad_U = function (q) q + cos(q)
  
  q = numeric(n+1)
  for (i in 1:n)
  { q[i+1] = HMC(U,grad_U,epsilon,L,q[i])
  }

  list (median=median(q), mean=mean(q), sd=sd(q))
})


test_HMC2 = test.cmp (function (epsilon,L,n,seed=1)
{
  set.seed(seed)

  s1 = 2; s2 = 3; r = 0.9
  inv_cov = solve(matrix(c(s1^2,s1*s2*r,s1*s2*r,s2^2),2,2))
  U = function (q) t(q) %*% inv_cov %*% q / 2
  grad_U = function (q) inv_cov %*% q
  
  q = matrix(0,n+1,2)
  for (i in 1:n)
  { q[i+1,] = HMC(U,grad_U,epsilon,L,q[i,])
  }

  list (mean=apply(q,2,mean), sd=apply(q,2,sd), cor=cor(q[,1],q[,2]))
})


test.name <- "hmc"

sys.time (r <- test_HMC1(0.8,10,50000))
print(r)

sys.time (r <- test_HMC1(1.1,1,100000))
print(r)

sys.time (r <- test_HMC2(0.8,10,25000))
print(r)

sys.time (r <- test_HMC2(1.1,1,50000))
print(r)
