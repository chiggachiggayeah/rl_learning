library(tidyverse)

bandit <- function(a, dists) {
  # generate a reward
  reward <- rnorm(1, mean = dists[[a]])
  return(reward)
}

updateValueInc <- function(Q, a, r, k) {
  Q[[a]] <- Q[[a]] + (1/(k + 1))*(r - Q[[a]])
  return(Q)
  # print(paste("Q: ", Q))
}

updateValueSamp <- function(reward_sums, a, r, k) {
  # assume that Q contains a sum of previous rewards for a
  reward_sums[[a]] <- reward_sums[[a]] + r
  # reward_sums <- sapply(reward_sums, function(x) x / k)
  return(reward_sums)
}

getActionGreedy <- function(Q, n, epsilon) {
  doExplore <- sample(0:1, 1, prob = c(1 - epsilon, epsilon))
  if(doExplore == 0) {
    max_action <- match(max(Q), Q)
    return(max_action)
  } else {
    # randomly select an action
    rand_action <- sample(1:n, 1)
    return(rand_action)
  }
}

getActionSoftmax <- function(Q, n, temp = 0.1){
  selected_action <- exp(Q[[a]]/temp) / exp(sum(Q)/temp)
  return(selected_action)
}

narmed <- function(n, steps, epsilon = 0.1, action_val = "incremental", action_sel = "eps_greedy") {
  # action value method can be incremental, sample
  # action selection method can be epsilon greedy or softmax
  
  dists <- c()
  Q <- rep(0, n)
  rewards <- rep(0, steps)
  reward_sums <- rep(0, n)
  avg_reward <- c(1:steps)
  
  
  for(action in 1:n){
    # set the actual values of the actions
    dists <- c(dists, rnorm(1, mean = 0))
  }
  
  for(k in 1:steps){
    cur_action <- c()
    if(action_sel == "eps_greedy") {
      cur_action <- getActionGreedy(Q, n, epsilon)
    } else if(action_sel == "softmax") {
      cur_action <- getActionSoftmax(Q, n, epsilon)
    }
    
    rew <- bandit(cur_action, dists)
    rewards <- c(rewards, rew)
    
    if(action_val == "incremental"){
      Q <- updateValueInc(Q, cur_action, rew, k)
    } else if(action_val == "sample"){
      reward_sums <- updateValueSamp(reward_sums, cur_action, rew, k)
      Q <- sapply(reward_sums, function(x) x / k)
    }
   
    avg_reward[[k]] <- mean(rewards)
  }
  
  print(paste("Inferred optimal action: ", match(max(Q), Q)))
  print(paste("Actual optimal action: ", match(max(dists), dists)))
  # print(paste("Actual Values: ", dists))
  # print(paste("Estimated Values: ", Q))
  
  return(avg_reward)
}

showLearning <- function(){
  ar <- narmed(10, 1000, action_val = "sample")
  dat <- data.frame(x = 1:1000, y = ar)
  dat %>%
    ggplot(aes(x, y)) +
      geom_line()
}



