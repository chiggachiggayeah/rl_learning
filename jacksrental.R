
# --------------------TODO--------------------
# [] think of a good way to handle the inherent issue with 1-indexing
#    and lot totals, that should be allowed to go to zero. In some ways, our "zero"
#    in the current imp. should match with 1
# [] implement code, to hop between policy eval and policy improvement until convergence
# [] get rid of off by one hack in full backup
# [] hack in findMaxAction
# --------------------------------------------
# "transitions" in the jack's car rental problem
# are car totals after moving cars, thing is
# do you keep track of this in one thing, or separately
# for both lots

# "actions" are the number of cars moved at night

# states are the end of day car totals

# rewards, I assume are money earned and lost. These are going to be
# negative though, I believe, since the money you gain
# ($10) has nothing to do with you're actions directly. It's entirely
# based off the poisson and whether or not you have cars. Your actions can
# cost you money directly, since it's -$2/car moved.

# you have a policy -> states,actions to probs

# you have transition probabilities to next states

# you have immediate rewards on the transitions (cost of move)
# discounted rewards in future ()

# states <- matrix(rep(0,42),ncol=21)
# actions <- matrix(rep(0:5, 2), ncol=6, byrow=TRUE)
rentReward <- 10
moveCost <- -2
theta <- 10^-7
maxCars <- 20


lot1Total <- 20
lot2Total <- 20

returnedLambda1 <- 3
returnedLambda2 <- 2
requestedLambda1 <- 3
requestedLambda2 <- 4

genPois <- function(l) {
  return(rpois(1,l))
}


# there should be four-hundred states if they're a combo of the totals
# in either lot

initValueFunc <- function() {
  vf <- matrix(rep(0, (maxCars + 1) ^ 2), ncol=maxCars+1)
  return(vf)
}

initPolicy <- function() {
  p <- matrix(sample(-5:5, (maxCars + 1) ^ 2, replace=TRUE), ncol=maxCars+1)
  return(p)
}

fillProbsAndRewards <- function(lambdaReturn, lambdaRequest) {
  probMat <- matrix(rep(0, (maxCars + 1) ^ 2), ncol=maxCars+1)
  rewards <- rep(0, maxCars + 1)
  
  for(m in 1:(maxCars + 1)) {
    for(req in 1:(maxCars + 1)) {
      prob_req = dpois(req, lambdaRequest)
      if(prob_req <= theta) {break}
      for(ret in 1:(maxCars + 1)) {
        prob_ret = dpois(ret, lambdaReturn)
        if(prob_ret <= theta) {break}
        # rewards
        rewards[m] <- rewards[m] + rentReward * prob_ret * prob_req
        
        # probs
        newState <- m + ret - min(m, req)
        newState <- min(newState, maxCars + 1)
        # print(paste("New state: ", newState))
        probMat[m, newState] <- probMat[m, newState] + (prob_req * prob_ret)    
      }
    }
  }
  
  return(c(probMat, rewards))
}

fullBackup <- function(action, state, valueFunc, reward1, reward2, probs1, probs2, discount = 0.9) {
  # val <- moveCost * action
  val <- matrix(rep(0, 441), ncol=21)
  
  state[[1]] <- state[[1]] + action
  state[[2]] <- state[[2]] - action
  
  # throw away "extra" cars (although this feels like a hack)
  state[[1]] <- min(maxCars + 1, state[[1]])
  state[[2]] <- min(maxCars + 1, state[[2]])
  
  # print(paste("S1 after action: ", state[[1]]))
  # print(paste("S2 after action: ", state[[2]]))
  
  for(s1 in 1:(maxCars + 1)) {
    for(s2 in 1:(maxCars + 1)) {
      
      val[s1, s2] <- (probs1[state[[1]], s1] * 
           probs2[state[[2]], s2] * 
           (reward1[state[[1]]] * reward2[state[[2]]] + discount * valueFunc[s1, s2]))
    }
  }
  
  # print(paste("final val is: ", sum(val)))
  return(sum(val))
  # policy iteration
  # improve policy until it stops changing
}

testFullBackup <- function() {
  vf <- initValueFunc()
  pol <- initPolicy()
  pr1 <- fillProbsAndRewards(returnedLambda1, requestedLambda1)
  pr2 <- fillProbsAndRewards(returnedLambda2, requestedLambda2)
  p1 <- matrix(pr1[1:(maxCars + 1)^2], ncol=maxCars+1)
  p2 <- matrix(pr2[1:(maxCars + 1)^2], ncol=maxCars+1)
  r1 <- pr1[-(1:(maxCars + 1)^2)]
  r2 <- pr2[-(1:(maxCars + 1)^2)]
  action <- 2
  state = c(15, 5)
  
  # testing to make sure that all of the dimensionality is correct
  assertthat::are_equal(dim(p1), c(21, 21))
  assertthat::are_equal(dim(p2), c(21, 21))
  assertthat::are_equal(length(r1), 21)
  assertthat::are_equal(length(r2), 21)
  # end tests
  
  fullBackup(action, state, vf, r1, r2, p1, p2)
}

policyEval <- function(valueFunc, policy) {
  l1 <- fillProbsAndRewards(returnedLambda1, requestedLambda1)
  l2 <- fillProbsAndRewards(returnedLambda2, requestedLambda2)
  probs1 <- matrix(l1[1:(maxCars + 1)^2], ncol=maxCars+1)
  probs2 <- matrix(l2[1:(maxCars + 1)^2], ncol=maxCars+1)
  rew1 <- l1[-(1:(maxCars + 1)^2)]
  rew2 <- l2[-(1:(maxCars + 1)^2)]
  delta <- -1
  
  for(s1 in 1:(maxCars + 1)) {
    for(s2 in 1:(maxCars + 1)) {
      # initialize policy and value function
      delta <- 0
      # policy <- matrix(rep(sample(0:5, 1), (maxCars + 1) ^ 2), ncols=21) 
      # valueFunc <- matrix(rep(sample(0:5, 1), (maxCars + 1) ^ 2),ncols=21)
      theta <- 0.1
      # policy evaluation
      # improve the value function until improvement is less than delta
      v <- valueFunc[s1, s2]
      a <- policy[s1, s2]
      
      # offset by 1
      a <- max(a, -s1+1)
      a <- min(a, s2-1)
      a <- min(a, 5)
      a <- max(a, -5)
      
      # print(paste("Action: ", a))
      # print(paste("State: ", c(s1, s2)))
      
      valueFunc[s1, s2] <- fullBackup(a, c(s1, s2), valueFunc, rew1, rew2, probs1, probs2)
      
      delta <- max(delta, abs(v - valueFunc[s1, s2]))
      if(delta <= theta) {return(valueFunc)}
    }
  }
  
  return(valueFunc)
}

findMaxAction <- function(state, valueFunc) {
  a <- -5:5
  bestAction <- -Inf
  for (action in a){
    s1_prime <- state[[1]] + action
    s2_prime <- state[[2]] - action
    
    # print(paste("S1 prime is: ", s1_prime))
    # print(paste("S2 prime is: ", s2_prime))
    
    # hack on the less than or equal to here
    if(s1_prime <= 0 || s2_prime <= 0 || s1_prime > maxCars + 1 || s2_prime > maxCars + 1) {next}
    
    # is there a difference between just getting the value function at the states post-action
    # and doing a full backup, like the above? I mean, yes since, the value function is closer
    # to optimal, assuming you're doing policy eval -> policy improvement
    
    result <- valueFunc[s1_prime, s2_prime]
    if(result >= bestAction) {
      bestAction <- result
    } else {
      next
    }
  }
  
  return(bestAction)
}

policyImp <- function(policy, valueFunc) {
  policyStable <- TRUE
  for(s1 in 1:(maxCars + 1)) {
    for(s2 in 1:(maxCars + 1)) {
      # for every state pick the action that maximized that state-value function
      b <- policy[s1, s2]
      policy[s1, s2] <- findMaxAction(c(s1, s2), valueFunc)
      if(b > policy[s1, s2] + theta) {
        policyStable <- FALSE
      }
    }
  }
  
  # check here to see if the policy has reached stability
  # if not, further optimize the value func
  
  return(policy)
}

testPolicyImp <- function() {
  # init
  vf <- initValueFunc()
  pol <- initPolicy()
  # pr1 <- fillProbsAndRewards(returnedLambda1, requestedLambda1)
  # pr2 <- fillProbsAndRewards(returnedLambda2, requestedLambda2)
  # p1 <- matrix(pr1[1:(maxCars + 1)^2], ncol=maxCars+1)
  # p2 <- matrix(pr2[1:(maxCars + 1)^2], ncol=maxCars+1)
  # r1 <- pr1[-(1:(maxCars + 1)^2)]
  # r2 <- pr2[-(1:(maxCars + 1)^2)]
  ######
  
  # print(pol)
  # policy_eval
  valueFunc <- policyEval(vf, pol)
  ######
  
  policyImp(valueFunc, pol)
  # print(pol_prime)
}
# vf <- policyEval(valueFunc, policy)
# p <- policyImp(valyeFunc, policy)