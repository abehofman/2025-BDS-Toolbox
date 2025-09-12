# install.packages("lintr")
library(lintr)

# Configuration
track_player_1 <- TRUE
num_players <- 100
num_items <- 10
num_iterations <- 10000
learning_rate <- 0.1

# True ability and difficulty values
true_ability <- rnorm(num_players, mean = 0, sd = 2)
true_difficulty <- runif(num_items, min = -2, max = 2)

# Estimated ability and difficulty values
estimated_ability <- numeric(num_players)
estimated_difficulty <- numeric(num_items)

# Tracking learning of player 1
player_1_trajectory <- numeric()

# Simulation loop
for (iteration in 1:num_iterations) {
  item_id <- sample(1:num_items, 1)
  player_id <- sample(1:num_players, 1)
  
  # Generate response based on true parameters
  prob_correct <- plogis(true_ability[player_id] - true_difficulty[item_id])
  response <- sample(0:1, 1, prob = c(1 - prob_correct, prob_correct))
  
  # Compute prediction and error based on current estimates
  predicted_prob <- plogis(estimated_ability[player_id] 
                           - estimated_difficulty[item_id])
  error <- response - predicted_prob
  
  # Update estimates
  estimated_ability[player_id] <- estimated_ability[player_id] + learning_rate * error
  estimated_difficulty[item_id] <- estimated_difficulty[item_id] - learning_rate * error
  
  # Optionally track player 1's ability over time
  if (track_player_1 && player_id == 1) {
    player_1_trajectory <- c(player_1_trajectory, estimated_ability[player_id])
  }
}

# Plot estimated vs. true abilities
plot(true_ability, estimated_ability,
     main = "True vs Estimated Ability",
     xlab = "True Ability",
     ylab = "Estimated Ability",
     pch = 19, col = "blue")

# Plot learning curve for player 1
plot(player_1_trajectory,
     main = "Learning Curve for Player 1",
     xlab = "Iteration",
     ylab = "Estimated Ability of Player 1",
     pch = 19, col = "red")

# Lint the current script
lint("test1.R")
