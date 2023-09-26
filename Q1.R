###Problem 1


## Part a

#'  Create the function by using a loop over the die rolls.
#'  
#'  Identify cost, winning/losing rolling score and the single 6-sided die
#'  Generate the random rolling point by using function sample()
#'  Check the rolling point and give a score in each circumstances
#'  @param n an integer
#'  @return overall winning/losing money amount
play_dice_v1 <- function(n) {
  # Identify cost, winning/losing rolling score and the single 6-sided die
  score <- 0
  cost <- 2
  die <- c(1, 2, 3, 4, 5, 6)
  
  # Create the loop
  for(i in 1:n) {
    # Generate the random rolling point by using function sample()
    point <- sample(die, 1, replace = TRUE)
    # Check the rolling point and give a score in each circumstances
    if (point == 2) {
      score <- score - cost + 2
    } else if(point == 4) {
      score <- score - cost + 4
    } else if(point == 6) {
      score <- score - cost + 6
    } else {
      score <- score - cost
    }
  }
  
  return(score)
}

#  Create the function by using built-in R vectorized functions
play_dice_v2 <- function(n) {
  #' Identify the cost and the single 6-sided die
  #' 
  #' Generate the random rolling point by using function sample()
  #' Then calculate the score
  #' @param n an integer
  #' @return overall winning/losing money amount
  cost <- 2
  die <- c(1, 2, 3, 4, 5, 6)
  
  # Generate the random rolling point
  point <- sample(die, n, replace = TRUE)
  
  # Calculate the score
  score <- length(which(point == 2)) * 2 + length(which(point == 4)) * 4 + length(which(point == 6)) *
    6 - cost * n
  
  return(score)
}

#  Create the function by collapsing the die rolls into a single table()
play_dice_v3 <- function(n) {
  #' Identify the cost and the single 6-sided die
  #' 
  #' Generate the random rolling point by using function sample()
  #' Collapsing the rolling points into a table to display the frequency of each roll point
  #' Using factor() to fix the indices of the table
  #' Finally, calculate the score
  #' @param n an integer
  #' @return overall winning/losing money amount
  cost <- 2
  die <- c(1, 2, 3, 4, 5, 6)
  
  # Generate the random rolling point by using function sample()
  point <- sample(die, n, replace = TRUE)
  
  # Collapsing the rolling points into a table
  freq_table <- table(factor(point, levels = 1:6))  # Using factor() to fix the indices of the table
  
  # Calculate the score
  score <- sum(freq_table[c(2, 4, 6)] * c(2, 4, 6)) - n * cost
  
  return(score)
}

#  Create the function by using one of the ¡°apply¡± functions
play_dice_v4 <- function(n) {
  #' Identify the cost and the single 6-sided die
  #' 
  #' Generate the random rolling point by using function sample()
  #' Calculate the score by using an apply function
  #' @param n an integer
  #' @return overall winning/losing money amount
  cost <- 2
  die <- c(1, 2, 3, 4, 5, 6)
  
  # Generate the random rolling point by using function sample()
  point <- sample(die, n, replace = TRUE)
  point_l <- list(point)
  
  # Calculate the score
  score <- sapply(point_l, function(x) {
    return(length(which(x == 2)) * 2 + length(which(x == 4)) * 4 + 
             length(which(x == 6)) * 6 - cost * n)
    
  })
  return(score)
  
}


## Part b

#  Run each examples for 5 times to prove the functions work
small_results <- c()
for (i in 1:5) {
  small_results <- c(small_results, play_dice_v1(3), play_dice_v2(3),
                     play_dice_v3(3), play_dice_v4(3))
  
}

large_results <- c()
for (i in 1:5) {
  large_results <- c(large_results, play_dice_v1(3000), play_dice_v2(3000),
                     play_dice_v3(3000), play_dice_v4(3000))
}

print(small_results)
print(large_results)


## Part c

#  Set a random seed and run each version few times to see whether the results are the same.
#  First display the results for the input 3.
s_m_results <- c()
set.seed(7)
for (i in 1:5) {
  s_m_results <- c(s_m_results, play_dice_v1(3))
}
set.seed(7)
for (i in 1:5) {
  s_m_results <- c(s_m_results, play_dice_v2(3))
}
set.seed(7)
for (i in 1:5) {
  s_m_results <- c(s_m_results, play_dice_v3(3))
}
set.seed(7)
for (i in 1:5) {
  s_m_results <- c(s_m_results, play_dice_v4(3))
}
small_matrix_results <- matrix(s_m_results, nrow = 5)
colnames(small_matrix_results) <- c('version 1', 'version 2', 'version 3', 'version 4')
print(small_matrix_results)

#  Now display the results for the input 3000.
l_m_results <- c()
set.seed(7)
for (i in 1:5) {
  l_m_results <- c(l_m_results, play_dice_v1(3000))
}
set.seed(7)
for (i in 1:5) {
  l_m_results <- c(l_m_results, play_dice_v2(3000))
}
set.seed(7)
for (i in 1:5) {
  l_m_results <- c(l_m_results, play_dice_v3(3000))
}
set.seed(7)
for (i in 1:5) {
  l_m_results <- c(l_m_results, play_dice_v4(3000))
}
large_matrix_results <- matrix(l_m_results, nrow = 5)
colnames(large_matrix_results) <- c('version 1', 'version 2', 'version 3', 'version 4')
print(large_matrix_results)

#  Both inputs give exactly the same results from a random seed.


## Part d
#  Install the package
library(microbenchmark)
microbenchmark(play_dice_v1(100), play_dice_v2(100), 
               play_dice_v3(100), play_dice_v4(100))
#  When the input is low, using built-in vectorized functions is the fastest way, then is using apply().
#  the execution time of these two methods are closer compare with the other two methods. The slowest way
#  is using a loop, which the execution time is far greater other methods.

microbenchmark(play_dice_v1(10000), play_dice_v2(10000), 
               play_dice_v3(10000), play_dice_v4(10000))
#  When the input is high, the rank of operation speed does not change, but the difference between collapsing
#  a matrix and using a loop increases, which means, despite the inefficient of collapsing a matrix, it is 
#  at least better than using a loop.


## Part e
#  I believe this is a fair game. We can prove this by a Monte Carlo simulation. The expected value of 
#  income should equals zero, so that the game is fair. We can use a Monte Carlo simulation to check whether
#  the expected amount of money is zero or very close to zero in the long run.

#  Set a trail
trail <- c(rep.int(3000, 10000))

#  Run the version 2 function to obtain the simulation.
sim <- sapply(trail, play_dice_v2)

#  Show the distribution we drew by a histogram
hist(sim, breaks = 200, probability = TRUE)
#  By CLT, we can say that our simulation has a normal distribution with zero mean. We can see the pattern
#  from the graph.

#  Carry out an one-sample t-test to see whether the true mean is zero.
t.test(sim)

#  Since the p-value is large enough, we could say it is statistically significant. So the true mean is zero.
#  This proves that the game is fair.