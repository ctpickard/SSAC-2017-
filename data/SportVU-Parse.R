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
game.data = sportvu_convert_json('0021500601[CLE-HOU-1-15-16].json')
pbp.data = read.csv('[2016-01-15]-0021500601-CLE@HOU.csv')

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
# Pull all shot events from play-by-play data, exclude shots over 30 feet. 
shot.events = subset(pbp.data, event_type == 'shot' | event_type =='miss')
shot.events = subset(shot.events, shot_distance <= 30)

# Get the moment the shot was recorded based on play-by-play log.
shot.time.stamp = as.character(shot.events$remaining_time)

# Convert PBP time to SportVU time. 
clock.eventKey = rep(0, nrow(shot.events))
for(i in 1:length(clock.eventKey)){
  clock.eventKey[i] = minutesToInteger(as.character(shot.events$remaining_time[i]))
}

# Collect the players that were on offense and defense of each shot event from PBP. 
offensive.players = data.frame(matrix(nrow = 0, ncol = 5))
defensive.players = data.frame(matrix(nrow = 0, ncol = 5))

for(i in 1:length(clock.eventKey)){
  defense = getDefenders(shot.events$play_id[i])
  offense = getOffensivePlayers(shot.events$play_id[i])
  for(j in 1:5){
    defensive.players[ i, j] = defense[j]
    offensive.players[ i, j] = offense[j]
  }
}

# Set time interval and look back for a given shot event. 
look.back = 10
interval = 0.2

# Initialize index counter, data.frame to store information. 
shot.information = data.frame(matrix(nrow = 0, ncol = 6))
index = 1

start.time = Sys.time()
# Loop through every shot event, collect information on every player on court. 
for(i in 1:nrow(shot.events)){
  # Define shot event data dump object. 
  data.dump = data.frame(matrix(nrow = 11*((look.back + interval)/interval), ncol = 7))
  
  # Set time window by specifying period and initial time of shot event. 
  period = shot.events$period[i]
  time.initial = clock.eventKey[i]
  
  # Set new time intervals to match all data to time points (standardize locations). 
  time.sequence = seq(time.initial, (time.initial + look.back + interval), by = interval)
  
  # Subset total game.data to specified time window. 
  play.data = subset(game.data, quarter == period & 
                       game_clock >= time.initial & game_clock <= (time.initial + look.back + interval))
  if(nrow(play.data) == 0){
    # Error in data, do nothing. 
  } else{
    # For each new time interval, average player (x,y) values. 
    for(j in 2:length(time.sequence)){
      # Current time interval being processed. 
      current.time = time.sequence[j - 1]
      
      # Get ball location at time interval. 
      ball.position = subset(play.data, player_id == -1 & 
                               game_clock >= time.sequence[j - 1] & game_clock <= time.sequence[j])
      
      # Store ball location information.
      data.dump[index, 1] = 'Ball'
      data.dump[index, 2] = 'BALL'
      data.dump[index, 3] = mean(ball.position$x_loc)
      data.dump[index, 4] = mean(ball.position$y_loc)
      data.dump[index, 5] = mean(ball.position$radius)
      data.dump[index, 6] = i
      data.dump[index, 7] = current.time
      
      # Go to next index. 
      index = index + 1
      for(k in 1:5){
        offensive.player.id = as.character(player.map$Player_ID[which(player.map$Player_Name == unlist(offensive.players[i, ])[k])])
        defensive.player.id = as.character(player.map$Player_ID[which(player.map$Player_Name == unlist(defensive.players[i, ])[k])])
        
        offensive.player = subset(play.data, player_id == offensive.player.id & 
                                    game_clock >= time.sequence[j - 1] & game_clock <= time.sequence[j])
        
        defensive.player = subset(play.data, player_id == defensive.player.id & 
                                    game_clock >= time.sequence[j - 1] & game_clock <= time.sequence[j])
        
        # Get name and location (x, y) of offensive player. 
        data.dump[index, 1] = unlist(offensive.players[i, ])[k]
        data.dump[index, 2] = 'OFF'
        data.dump[index, 3] = mean(offensive.player$x_loc)
        data.dump[index, 4] = mean(offensive.player$y_loc)
        data.dump[index, 5] = mean(offensive.player$radius)
        data.dump[index, 6] = i
        data.dump[index, 7] = current.time
        
        index = index + 1
        
        # Get name and location (x, y) of defensive player. 
        data.dump[index, 1] = unlist(defensive.players[i, ])[k]
        data.dump[index, 2] = 'DEF'
        data.dump[index, 3] = mean(defensive.player$x_loc)
        data.dump[index, 4] = mean(defensive.player$y_loc)
        data.dump[index, 5] = mean(defensive.player$radius)
        data.dump[index, 6] = i
        data.dump[index, 7] = current.time
        
        index = index + 1
      }
    }
    # Set names of shot data-dump, sort by time event (decreasing)
    colnames(data.dump) = c('identity', 'team_type', 'x_loc', 'y_loc', 'radius', 'shot_event', 'game_clock')
    data.dump = arrange(data.dump, -game_clock)
    
    # Bind to total event information. 
    shot.information = rbind(shot.information, data.dump)
    
  }
  
}
Sys.time() - start.time
   