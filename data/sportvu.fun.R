require(RCurl)
require(jsonlite)
require(dplyr)
require(plyr)
require(ggplot2)
require(png)

###################################################################################################
# HELPER FUNCTIONS
###################################################################################################
# Function to convert standard PBP game clock to SportVU time. 
minutesToInteger = function(time){
  time = unlist(strsplit(time, ":"))
  time = as.numeric(time[2])*60 + as.numeric(time[3]) 
  return(time)
}

# FUnction to measure distance between ball and player. 
distanceToPoint <- function(x1, y1, x2, y2){
  x = (x2 - x1)^2
  y = (y2 - y1)^2
  ans = sqrt(x + y)
  return(ans)
}

# Function to find the defender id and distance from shooter at time of shot. 
distanceNearDefender = function(defensive.players, shot.time, ball.x, ball.y, event.id){
  distance = 1000
  player.id = -1

  for(i in 1:length(defensive.players)){
    # Get defender position information. 
    player.position = subset(shot.data, event.id == event.id &
                               player_id == player.map[which(player.map$Player_Name %in% defensive.players[i]), 1])
    
    player.x = player.position$x_loc[min(which(player.position$game_clock == shot.time))]
    player.y = player.position$y_loc[min(which(player.position$game_clock == shot.time))]
    
    player.distance = distanceToPoint(ball.x, ball.y, player.x, player.y)
    
    if(distance > player.distance){
      distance = player.distance
      player.id = player.map[which(player.map$Player_Name %in% defensive.players[i]), 1]
    }
  }
  defender = data.frame(ID = player.id, Distance = distance)
  return(defender)
}

# Function to find the identity of the defenders given a known event. .
getDefenders = function(event.id){
  away.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(4, 5, 6, 7, 8)]))
  home.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(9, 10, 11, 12, 13)]))
  
  shooter = pbp.data$player[pbp.data$play_id == event.id]
  
  if(shooter %in% away.players){
    return(home.players)
  } else{
    return(away.players)
  }
}

# Function to find the identity of the offensive players given a known event.
getOffensivePlayers = function(event.id){
  away.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(4, 5, 6, 7, 8)]))
  home.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(9, 10, 11, 12, 13)]))
  
  shooter = pbp.data$player[pbp.data$play_id == event.id]
  
  if(shooter %in% away.players){
    return(away.players)
  } else{
    return(home.players)
  }
}

###################################################################################################
# READ IN SPORTVU AND PBP DATA FILES
###################################################################################################
# Load in the SportVU and play-by-play data. 
game.data = sportvu_convert_json('0021500514.json')
pbp.data = read.csv('[2016-01-04]-0021500514-ORL@DET.csv')

###################################################################################################
# DEFINE PLAYER ID AND NAME MAP FOR GAME
###################################################################################################
# Get all unique names in game.
all.names = paste(game.data$firstname, game.data$lastname, sep = ' ')
player.names = unique(all.names)

player.keys = unique(game.data$player_id)
player.map = data.frame(Player_ID = player.keys, Player_Name = player.names)

################################################################################################### 
# FIND ALL SHOT (MISS OR MAKE IN PBP DATA) - DEFINE KEY & FILTER DATA
###################################################################################################                        
# Pull all shot events from play-by-play data. 
shot.events = subset(pbp.data, event_type == 'shot' | event_type =='miss')
shot.events = subset(shot.events, shot_distance <= 30)

# Get the play ID from PBP data to filter SportVU data to shot events only.
# Event ID and Play ID are equivalent. 
event.key = shot.events$play_id

# Remove any instances where SportVU data does not match PBP data (errors in recorded Shots, missing Shot events).
all.events = unique(game.data$event.id)
shot.key = all.events[which(all.events %in% event.key)]

# Filter SportVU data to specific events. 
shot.data = subset(game.data, event.id %in% shot.key)

# Finalize PBP data to reflect matched shot events in SportVU data and PBP data. 
shot.events = subset(shot.events, play_id %in% shot.key)
write.csv(shot.events, file = 'shot.events.csv')

# Convert PBP shot event times to SportVU times. Create shot event time key. 
pbp.game.clock.atEvent = as.character(shot.events$remaining_time)

clock.eventKey = rep(0, length(pbp.game.clock.atEvent))
for(i in 1:length(clock.eventKey)){
  clock.eventKey[i] = minutesToInteger(pbp.game.clock.atEvent[i])
}

###################################################################################################
# Get identity of each shooter. 
shooter.key = shot.events$player
###################################################################################################
# Function takes in an event and standardizes the locations by uniform time by set interval. 
# Data return: Player, game clock, ball.x, ball.y, ball.radius. player.x, player.y, distance from player to ball.
# Takes about 10 minutes to run through whole game data.
matchDataToTime = function(ball.position, player.position, time.min, time.max, interval){
  # Get player's name
  player.name = paste(player.position$firstname[1], player.position$lastname[1], sep = ' ')
  
  # Get event ID
  event.id = player.position$event.id[1]
  
  # Set new time intervals to match all data to time points. 
  time.sequence = seq(time.min, time.max, by = interval)
  
  # Setup data.frame to collect data. 
  new.data = data.frame(matrix(nrow = (length(time.sequence) - 1), ncol = 9))
  
  # For each new time interval, average player (x,y) values. 
  for(j in 2:length(time.sequence)){
    # Filter the data for both the ball and the player to the new time interval. 
    ball.sub.position = subset(ball.position, game_clock >= time.sequence[j - 1] & game_clock <= time.sequence[j])
    player.sub.position = subset(player.position, game_clock >= time.sequence[j - 1] & game_clock <= time.sequence[j])
    
    # Set the event.id for the play. 
    new.data[j - 1, 1] = event.id 
    
    # Set the player name for the play data. 
    new.data[j - 1, 2] = player.name 
    
    # Set the game time of the new averaged data. 
    new.data[j - 1, 3] = time.sequence[j - 1]
    
    # Set the ball's x, y and radius averaged values for the time event. 
    new.data[j - 1, 4] = mean(ball.sub.position$x_loc)
    new.data[j - 1, 5] = mean(ball.sub.position$y_loc)
    new.data[j - 1, 6] = mean(ball.sub.position$radius)
    
    # Set the players x, y averaged values for the time event. 
    new.data[j - 1, 7] = mean(player.sub.position$x_loc)
    new.data[j - 1, 8] = mean(player.sub.position$y_loc)
    
    # Set distance between player and ball. 
    new.data[j - 1, 9] = distanceToPoint(mean(ball.sub.position$x_loc), mean(ball.sub.position$y_loc), 
                                         mean(player.sub.position$x_loc), mean(player.sub.position$y_loc))
  }
  return(na.omit(new.data))
}

# Set data collection column names. 
columns = c('event_id', 'Player', 'game_clock', 'ball.x', 'ball.y', 'ball.radius', 'player.x', 'player.y', 'distance_ball')

# Setup data.frame to capture player ball information for shot events when ball is less than 1.5 from player. 
summarized.shot.data = data.frame(matrix(nrow = 0, ncol = 9))
colnames(summarized.shot.data) = columns

start.time = Sys.time() 
for(i in 1:length(shot.key)){
  # Get the offensive players on the court for the event. 
  offensive.players = getOffensivePlayers(shot.key[i])
  
  # Get the ball location during the event. 
  ball.position = subset(game.data, event.id == shot.key[i] & player_id == -1)
  
  # Get the time range of the ball during the event, standardize time intervals for each event. 
  time.min = min(ball.position$game_clock)
  time.max = max(ball.position$game_clock)
  interval = 0.2
  
  # Set data.frame to store all player data for an event. 
  play.data = data.frame(matrix(nrow = 0, ncol = 9))
  colnames(play.data) = columns
  
  for(j in 1:length(offensive.players)){
    # Get player position data for the specific event for offensive player j. 
    player.position = subset(game.data, event.id == shot.key[i] & 
                               player_id == player.map[which(player.map$Player_Name %in% offensive.players[j]), 1])
    
    # Get standardized player and ball data at specified interval. 
    ball_player_data = matchDataToTime(ball.position, player.position, time.min, time.max, interval)
    colnames(ball_player_data) = columns
    
    # Get data points where the player is within 1.5 feet of the ball, implying he is in possession of the ball. 
    sub.data = subset(ball_player_data, distance_ball <= 1.50)
    play.data = rbind(play.data, sub.data)
  }
  
  # Sort total play data by game_clock, cut off at event time. 
  play.data = arrange(play.data, -game_clock)
  play.data = subset(play.data, game_clock >= clock.eventKey[i])
  
  # Attach data to main data.frame for total game data. 
  summarized.shot.data = rbind(summarized.shot.data, play.data)
  
}
Sys.time() - start.time

