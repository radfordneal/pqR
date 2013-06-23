source("time.r")
 
# DO Q-LEARNING IN SOME WORLD.  Arguments are a function generating an
# initial state of the world (init), the function defining the transitions
# and rewards (world), the discount rate for rewards (gamma), the learning 
# rate (alpha), the probability of taking a random action (epsilon), and the 
# number of steps to simulate (steps).  
#
# The global variables n.states and n.actions should contain the number of
# states (labelled from 1 up ) and the number of actions (labelled from 1 up).
#
# Returns a list containing a matrix with the history of the simulation, one 
# row per time step, and the final Q function table.

simulate = test.cmp (function (init, world, gamma, alpha, epsilon, steps)
{
  history = matrix(NA,steps,6)
  colnames(history) = c("t","s","a","r","rs","sn")

  Q = matrix(0,n.states,n.actions)

  s = init()

  for (t in 1:steps)
  { 
    if (runif(1)<epsilon)
    { a = sample(n.actions,1)
    }
    else
    { a = order(Q[s,])[n.actions]
    }

    w = world(s,a)
    r = w$r
    sn = w$s

    Q[s,a] = (1-alpha) * Q[s,a] + 
      alpha * (r + gamma * (epsilon*mean(Q[sn,]) + (1-epsilon)*max(Q[sn,])))

    history[t,"t"] = t
    history[t,"s"] = s
    history[t,"a"] = a
    history[t,"r"] = r
    history[t,"sn"] = sn

    s = sn
  }

  history[,"rs"] = history[,"r"]
  for (t in 2:steps)
  { history[t,"rs"] = 0.1*history[t,"rs"] + 0.9*history[t-1,"rs"]
  }

  list (history=history, Q=Q)
})


# DEMO OF Q LEARNING.
#
# In this demo, the state used isn't really the full state of the world.
# We see if something reasonable gets learned anyway, with the help of
# one bit of "memory" that can be stored in the state...


# FUNCTION TO GENERATE INITIAL STATE.  There is a hidden part of the
# state, in the global variable "marks".

init2m = test.cmp (function ()
{ 
  marks <<- rep(0,n.states)
  sample(n.states,1)
})


# FUNCTION TO GENERATE REWARDS AND TRANSITION TO NEXT STATE.  In the hidden
# part of the state, some marks are set to 1, producing future rewards.
# The action taken can toggle the bit of memory stored in the state.

world2m = test.cmp (function (s, a)
{
  bit = as.numeric(s>n.states/2)
  s = s - bit*n.states/2

  if (runif(1)<0.05)
  { s = sample(n.states/2,1)
  }
  else
  { if (a>3)
    { bit = 1-bit
      a = a-3
    }
    s = s + (a-2)
    if (s<1) s = n.states/2
    if (s>n.states/2) s = 1
  }

  r = marks[s] - 10*as.numeric(s==1)

  marks <<- as.numeric (marks>0 | (runif(n.states)<0.3))
  marks[s] <<- 0

  s = s + bit*n.states/2
 
  list (s=s, r=r)
})

test.name <- "Qlearn"

n.steps = 30000

gamma = 0.95
alpha = 0.015
epsilon = 0.1

n.states = 10*2	# Positions arranged in a circle, plus one bit of memory
n.actions = 3*2	# 1 = move to lower, 2 = stay in place, 3 = move to higher
                # 4, 5, 6 similar but also toggle bit

set.seed(1)

sys.time({
  result2m = simulate (init2m, world2m, gamma, alpha, epsilon, n.steps)
})

cat("Average reward:",mean(result2m$history[,"r"]),"\n")
