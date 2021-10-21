library(R6)
library(tibble)

# Set Class for Arena
Arena <- R6Class(
  classname = "Arena", 
  public = list(
      data_A = tibble(
        players = c("Black", "Gray", "White"),
        hit_rate = c(1/3, 2/3, 3/3),
        turn = c(1,0,0)
      ),
      data_B = tibble(
        players = c("Black", "Gray", "White"),
        hit_rate = c(1/3, 2/3, 3/3),
        turn = c(0,1,0)
      ),
      arena = NULL,
      initialize = function(strategy){
        if(strategy=="normal") {self$arena <- self$data_A}
        if(strategy=="miss_accidentally") {self$arena <- self$data_B}
      },
      results = tibble(
        "Black" = 0,
        "Gray" = 0,
        "White" = 0
      )
  )
)

# add function to choose a shooter
Arena$set(
  "public", "choose_shooter", function(){
    # choose shooter
    shooter <- filter(self$arena, turn==1)
    # remove shooter from arena
    self$arena <- filter(self$arena, turn==0)
    #print(shooter[1,1])
    return(shooter)
  }
)

# add function to class to choose a target
Arena$set(
  "public", "choose_target", function(){
    # Choose target with highest hit prob
    self$arena <- arrange(self$arena, desc(hit_rate))
    target <- self$arena[1,]
    # remove target from arena
    self$arena <- self$arena[-1,]
    return(target)
  }
)

# add function to shoot at a target
Arena$set(
  "public", "shoot_target", function(shooter){
    # Extract shooter
    shooter <- as.character(shooter[1,1])
    # Set probability for shooter
    if (shooter == "Black") {
      prob <- c(1/3, 2/3)
    } else if (shooter == "Gray") {
      prob <- c(2/3, 1/3)
    } else if (shooter == "White") {
      prob <- c(1, 0)
    } 
    # take the shot!
    result_shot <- sample(x=c("Kill", "Miss"), replace = T, size = 1, prob = prob)
    return(result_shot)
  }
)

# update the arena of the truel
Arena$set(
  "public", "update_arena", 
    function(result_shot, target, shooter){
      # If the shooter missed, put the target back in the arena
      if(result_shot=="Miss"){
        self$arena <- rbind(self$arena, target)
      }
      # Put the shooter back in the arena
      self$arena <- rbind(self$arena, shooter)
    }
)

# Determine next shooter
next_shooter <- function(last_shooter){
  
  # Extract string out of dataframe
  if(last_shooter=="Black"){
    next_shooter <- "Gray"
  } else if(last_shooter=="Gray"){
    next_shooter <- "White"
  } else if(last_shooter=="White"){
    next_shooter <- "Black"
  }  
  return(next_shooter)
}

# Switch the turns of the players
Arena$set(
  "public", "switch_turn", function(shooter){
    
    # Switch shooter
    name_last_shooter <- shooter[1,1] %>% as.character()
    nxt_shooter <- next_shooter(name_last_shooter)
    
    # Check if next shooter is alive; if not, skip that shooters turn
    while(nrow(filter(self$arena, players %in% nxt_shooter))==0){
      nxt_shooter <- next_shooter(nxt_shooter)
    }
    # Reset turn counter
    m <- self$arena["players"]==nxt_shooter
    self$arena[,"turn"] <- m[,1] %>% as.numeric()
  }
)

# Set functions to update the scoreboard and reset the game
Arena$set(
  "public", "update_scoreboard", function(strategy){
    # Adjust scores
    if(nrow(self$arena)==1){
      if(nrow(filter(self$arena, players %in% "Black")) == 1) self$results[,1] <- self$results[,1] + 1
      if(nrow(filter(self$arena, players %in% "Gray")) == 1) self$results[,2] <- self$results[,2] + 1
      if(nrow(filter(self$arena, players %in% "White")) == 1) self$results[,3] <- self$results[,3] + 1
      
      # adjust arena
      if(strategy=="normal") {self$arena <- self$data_A}
      if(strategy=="miss_accidentally") {self$arena <- self$data_B}
    }
  }
)

simulate_truel <- function(strategy, k) {
  
  # initialize
  western_shootout <- Arena$new(strategy = strategy)
  
  for(i in 1:k){
    
    repeat({
      
      # take shooter
      shooter <- western_shootout$choose_shooter()
      
      # pick target
      target <- western_shootout$choose_target()
      
      # shoot target
      result_shot <- western_shootout$shoot_target(shooter)
      
      # update arena
      western_shootout$update_arena(result_shot, target, shooter)
      
      # switch turn
      western_shootout$switch_turn(shooter)
      
      # if only one alive, update scoreboard
      if(nrow(western_shootout$arena)==1){
        western_shootout$update_scoreboard(strategy)
        break
      }
    })
    
    # Poormans Counter
    if (i%%1==0) cat("Number of truel simulations:", i, "\r")
    
    # Print scores when done
    if(i == k){
      print(western_shootout$results/k)
    } 
  }
}

# run function
simulate_truel(strategy = "miss_accidentally", k = 10000)