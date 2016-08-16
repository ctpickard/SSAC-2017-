require(RCurl)
require(jsonlite)
require(dplyr)
require(plyr)

###################################################################################################

# HELPER FUNCTIONS

###################################################################################################
# =================================================================================================
# Function: Convert PBP time events to SportVU time events format. 
# =================================================================================================
# INPUT: Game time of PBP data as a character. 
# RETURN: PBP game time converted to SportVU time. 
minutesToInteger = function(time){
  time = unlist(strsplit(time, ":"))
  return(as.numeric(time[2])*60 + as.numeric(time[3]))
}

# =================================================================================================
# Function: Return the distance between two points. 
# =================================================================================================
# INPUT: Two (x, y) points of an object on the court, regardless of object(s) identity. 
# RETURN: The distance between the two points in feet. 
distanceToPoint <- function(x1, y1, x2, y2){
  x = (x2 - x1)^2
  y = (y2 - y1)^2
  return(sqrt(x + y))
}

# =================================================================================================
# Function: Return offensive players on court given a shot event. 
# =================================================================================================
# INPUT: Shot event ID from common PBP and SportVU events. 
# OUTPUT: Offensive players on court for input shot event ID. 
getOffensive_Players = function(event.id){
  # Get the players on the court of the event. 
  away.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(4, 5, 6, 7, 8)]))
  home.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(9, 10, 11, 12, 13)]))
  
  # Get the shooter of the event. 
  shooter = pbp.data$player[pbp.data$play_id == event.id]
  
  # If shooter is in away team, return home team, else return away team. 
  if(shooter %in% away.players){
    return(away.players)
  } else{
    return(home.players)
  }
}

# =================================================================================================
# Function: Return defensive players on court given a shot event. 
# =================================================================================================
# INPUT: Shot event ID from common PBP and SportVU events. 
# OUTPUT: Defensive players on court for input shot event ID. 
getDefensive_Players = function(event.id){
  # Get the players on the court of the event. 
  away.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(4, 5, 6, 7, 8)]))
  home.players = as.character(unlist(pbp.data[pbp.data$play_id == event.id, c(9, 10, 11, 12, 13)]))
  
  # Get the shooter of the event. 
  shooter = pbp.data$player[pbp.data$play_id == event.id]
  
  # If shooter is in away team, return home team, else return away team. 
  if(shooter %in% away.players){
    return(home.players)
  } else{
    return(away.players)
  }
}

# =================================================================================================
# Function: Return the shot angle of a given shot. 
# =================================================================================================
# INPUT: Two (x, y, z) points of the ball over two time instances. Final time > initial time. 
# RETURN: Ball angle in degrees of shot. 
getShot_Angle = function(initial, final){
  tan = (final[3] - initial[3])/(distanceToPoint(initial[1], initial[2], final[1], final[2]))
  return(atan(tan)*(180/pi))
}

# =================================================================================================
# Function: Information about defensive positioning at time of shot.  
# =================================================================================================
# INPUT: SportVU data at instance of shot (based on SportVU Key), Shooter ID and Defensive players
#        ID on court at time of shot. 
# RETURN: A list with [[1]] = Distance, [[2]] = ID of Nearest, [[3]] = number of defenders w/n 4.0
getDefender_Information = function(event.data, shooter, defense){
  distance = 500
  player = ''
  number.defenders = 0

  for(i in 1:5){
    # Calculate distance between shooter and the ith defensive player.
    new.distance = distanceToPoint(event.data[which(event.data$player_id == shooter), ]$x_loc,
                                   event.data[which(event.data$player_id == shooter), ]$y_loc,
                                   
                                   event.data[which(event.data$player_id == defense[i]), ]$x_loc,
                                   event.data[which(event.data$player_id == defense[i]), ]$y_loc)
    
    # If distance is less than shortest distance, update distance and player ID. 
    if(new.distance < distance){
      distance = new.distance
      player = defense[i]
    }
    
    # If distance is less than 4.0, update number of defenders near shooter. 
    if(new.distance < 4.0){
      number.defenders = number.defenders + 1
    }
  }
  # Return as a list. 
  return(list(distance, player, number.defenders))
}

###################################################################################################
# READ IN PBP DATA FILES
###################################################################################################
# Load in the SportVU and play-by-play data. 
sportVU.file = '0021500514[ORL-DET-1-04-16].json'
pbp.data = read.csv('[2016-01-04]-0021500514-ORL@DET.csv')

# game.data should be the synthesized SportVU data file from Synthesize_SportVU_Data.R
game.information = read.csv('0021500514[ORL-DET-1-04-16]_tracking-data.csv')

###################################################################################################
###################################################################################################

# Filter down synthesized SportVU data, to relevant features for data collection. 

###################################################################################################
###################################################################################################

start.time = Sys.time()

###################################################################################################
# 1) CREATE SHOT EVENT TIME MAP FROM PBP DATA 
###################################################################################################
# Create a key to find shot events within SportVU data. 
# Filter only by shots (made or missed), were not blocked and less than 30 feet.     
shot.events = subset(pbp.data, event_type == 'miss' | event_type == 'shot')
shot.events = subset(shot.events, shot_distance <= 30)
shot.events = subset(shot.events, block == '') 

# Record time of shot event.
pbp.shot.time = shot.events$remaining_time

# Convert PBP standard time to SportVU standard time. 
convert.time = rep('', nrow(shot.events))
for(i in 1:length(convert.time)){
  convert.time[i] = minutesToInteger(as.character(pbp.shot.time[i]))
}

# Create map of shot events that specify game time and shooter. 
shot.pbp.key = data.frame(time = as.double(convert.time), 
                          quarter = shot.events$period, 
                          shooter = shot.events$player, 
                          event_id  = shot.events$play_id,
                          result = shot.events$points, 
                          shot_distance = shot.events$shot_distance, 
                          assister = shot.events$assist)


################################################################################################### 
# 2) RECORD PLAYERS ON OFFENSE AND DEFENSE FOR EACH SHOT EVENT
###################################################################################################
# Collect all players on offense and defense of each shot event from PBP. 
# Create a key that holds all offensive and defensive players per shot event.
offensive.players = data.frame(matrix(nrow = 0, ncol = 5))
defensive.players = data.frame(matrix(nrow = 0, ncol = 5))

# For each shot event, grab and separate players based on offense and defense.
for(i in 1:nrow(shot.pbp.key)){
  defense = getDefensive_Players(shot.pbp.key$event_id[i])
  offense = getOffensive_Players(shot.pbp.key$event_id[i])
  for(j in 1:5){
    defensive.players[i, j] = defense[j]
    offensive.players[i, j] = offense[j]
  }
}

###################################################################################################
# 3) CREATE SHOT EVENT TIME MAP FOR SPORTVU DATA
###################################################################################################
# INPUT: Each time (reamining in quarter (sec), quarter) a shot event occured in PBP. 
#        Filter out block shots. 
# OUTPUT: The exact time of the shot in SportVU data. 

# NOTE: The moment of a shot is based on the relationship of the distance between the ball and 
#       the shooter while the ball is at least 7.5 feet above the ground (z_loc). 
#       Key condition is when the ball is within a given distance_threshold while above a 
#       certain height.

# HELPERS
# Set PBP shot event look_back time. 
look_back = 7.0

# Set distance threshold between ball and shooter (measures in feet). 
distance_threshold = c(1.25, 1.75, 2.25, 3.6)

# Set up data.frame object to collect the moments. 
shot.sportvu.key = data.frame(matrix(nrow = nrow(shot.pbp.key), ncol = 7))

# FUNCTION/PROCESS
for(i in 1:nrow(shot.pbp.key)){
  # Filter game information specifically to recorded shot event + look_back time.
  event.data = filter(game.information, 
                      quarter == shot.pbp.key$quarter[i], 
                      game_clock >= shot.pbp.key$time[i], 
                      game_clock < shot.pbp.key$time[i] + look_back)
  
  # Check if shot event time frame exists in SportVU data (not all time is recorded). 
  if(nrow(event.data) == 0){
    # Mark that this shot was not recorded in SportVU with 0's. 
    shot.sportvu.key[i, 1] = 0
    shot.sportvu.key[i, 2] = 0
    
  } else{
    # Filter to ball and shooter location for given shot event.  
    ball.location = filter(event.data, position == 'BALL')
    shooter.location = filter(event.data, player_id == as.character(shot.pbp.key$shooter[i]))
    
    # Initialize index for moment of shot and default distance between player and ball. 
    index_adjust = 0
    distance_ball = 500
    
    # Start with best case threshold, keep iterating until moment is found. 
    for(j in 1:length(distance_threshold)){
      
      # Look at each instance in time to see if ball-shooter relationship is true. 
      for(k in 1:nrow(ball.location)){
        # Get distance between ball and shooter.  
        distance_ball = distanceToPoint(ball.location$x_loc[k], ball.location$y_loc[k], 
                                        shooter.location$x_loc[k], shooter.location$y_loc[k])
        #print(distance_ball)
        #print(ball.location$z_loc[j])
        #print(' ')
        
        # Store shot moment information if true and break out of loop. 
        if(distance_ball < distance_threshold[j] && ball.location$z_loc[k] > 7.5){
          index_adjust = k
          shot.sportvu.key[i, 1] = shooter.location$game_clock[index_adjust]
          shot.sportvu.key[i, 2] = shooter.location$quarter[index_adjust]
          shot.sportvu.key[i, 3] = distance_ball
          shot.sportvu.key[i, 4] = as.character(shot.pbp.key$shooter[i])
          shot.sportvu.key[i, 5] = shot.pbp.key$event_id[i]
          shot.sportvu.key[i, 6] = shot.pbp.key$result[i]
          shot.sportvu.key[i, 7] = shot.pbp.key$shot_distance[i]
          shot.sportvu.key[i, 8] = shot.pbp.key$assister[i]
          break
        }
      }
      
      # Break out of exterior loop if solution is found before all thresholds evaluated.
      if(index_adjust != 0){
        break
      }
    }
    
    # Mark that a moment was not found, could be a result of miss time data or error in location data.
    if(index_adjust == 0){
      shot.sportvu.key[i, 1] = 0
      shot.sportvu.key[i, 2] = 0
    }
  }
}
# Remove shots that had error in data. CLE - HOU had 89.3% accuracy
possible_shots = nrow(shot.sportvu.key)

shot.sportvu.key = na.omit(shot.sportvu.key)
colnames(shot.sportvu.key) = c('time', 'quarter', 'distance_ball', 'shooter', 'event_id', 
                               'result', 'Shot_Distance', 'assister')

# Report Accuracy
nrow(shot.sportvu.key)/possible_shots

###################################################################################################
# 4) FILTER OFFENSIVE AND DEFENSIVE PLAYER KEY BASED ON RECORDED SHOTS IN (3)
###################################################################################################
# Merge the unfiltered players list with the updated shot event key by rowname (these indices match)
# Re-order to put it in chronological order and then only select the players. 
offensive.players = merge(offensive.players, shot.sportvu.key, by.x = 0, by.y = 0)
offensive.players = offensive.players[order(as.integer(offensive.players$Row.names)), ]
offensive.players = select(offensive.players, 2:6)

defensive.players = merge(defensive.players, shot.sportvu.key, by.x = 0, by.y = 0)
defensive.players = defensive.players[order(as.integer(defensive.players$Row.names)), ]
defensive.players = select(defensive.players, 2:6)

colnames(offensive.players) = c('O1', 'O2', 'O3', 'O4', 'O5')
colnames(defensive.players) = c('D1', 'D2', 'D3', 'D4', 'D5')

###################################################################################################
# 5) COLLECT SHOOTER INFORMATION FOR EACH SHOT
###################################################################################################
# INPUT: Exact time of shot event from map (remaining in quarter (sec), quarter) a shot occured.  
#        Filtered out shots with incomplete data. 
# OUTPUT: Data regarding shooting information:
#         - Shooter ID, position, team 
#         - Shot location (x, y)
#         - Shot angle (degrees)
#         - Shot Distance (feet)
#         - Shot result (Pts. scored) = 0, 2, 3
#         - Nearest defender distance
#         - Number of defenders within 3.5 feet
#         - Nearest defender ID, position, team


# HELPERS
# Initialize data.frame to store shot information. 
shot.information = data.frame(matrix(nrow = nrow(shot.sportvu.key), ncol = 13))

for(i in 1:nrow(shot.sportvu.key)){
  # Get specific event data for players at time of shot. 
  event.data = filter(game.information, 
                      quarter == shot.sportvu.key$quarter[i],
                      game_clock == shot.sportvu.key$time[i])
  
  # Get specific data on ball after shot to calculate shot angle. 
  ball.data = filter(game.information,
                     quarter == shot.sportvu.key$quarter[i], 
                     player_id == 'NA ball',
                     game_clock <= shot.sportvu.key$time[i],
                     game_clock >= shot.sportvu.key$time[i] - 2.5)
  
  # A) Player_ID
  shot.information[i, 1] = shot.sportvu.key$shooter[i]
  
  # B) Player Position
  shot.information[i, 2] = as.character(event.data[which(event.data$player_id == shot.sportvu.key$shooter[i]), ]$position)
  
  # C) Player Team_ID
  shot.information[i, 3] = as.character(event.data[which(event.data$player_id == shot.sportvu.key$shooter[i]), ]$team_id)
  
  # D) Shot Location (X)
  shot.information[i, 4] = event.data[which(event.data$player_id == shot.sportvu.key$shooter[i]), ]$x_loc
  
  # E) Shot Location (Y)
  shot.information[i, 5] = event.data[which(event.data$player_id == shot.sportvu.key$shooter[i]), ]$y_loc
  
  # F) Shot Angle (Degrees)
  shot.information[i, 6] = getShot_Angle(c(ball.data$x_loc[1], ball.data$y_loc[1], ball.data$z_loc[1]), 
                                         
                                         c(ball.data$x_loc[which(ball.data$z_loc == max(ball.data$z_loc))], 
                                           ball.data$y_loc[which(ball.data$z_loc == max(ball.data$z_loc))],
                                           ball.data$z_loc[which(ball.data$z_loc == max(ball.data$z_loc))]))
  
  # G) Shot Distance (feet)
  shot.information[i, 7] = shot.sportvu.key$Shot_Distance[i]
  
  # H) Shot Result (0, 2, 3 Pts.)
  shot.information[i, 8] = shot.sportvu.key$result[i]
  
  # Collect information about defensive situation at time of shot. 
  defensive.information = getDefender_Information(event.data, shot.sportvu.key$shooter[i], as.character(defensive.players[i, ]))
  
  # I) Distance to nearest defender 
  shot.information[i, 9] = defensive.information[[1]]
  
  # J) Number of defenders within 4.0 feet of shot. 
  shot.information[i, 10] = defensive.information[[3]]
  
  # K) Defender ID
  shot.information[i, 11] = as.character(defensive.information[[2]])
  
  # L) Defender Team_ID
  shot.information[i, 12] = as.character(event.data[which(event.data$player_id == defensive.information[[2]]), ]$position) 
  
  # M) Defender Position
  shot.information[i, 13] = as.character(event.data[which(event.data$player_id == defensive.information[[2]]), ]$team_id) 
  
  
}

colnames(shot.information) = c('Shooter', 'Shooter_Position', 'Shooter_Team', 'Shot_X', 'Shot_Y', 'Shot_angle', 'Shot_Distance', 'Shot_Result', 
                               'Defender_Distance', 'Num_Defenders', 'Defender', 'Defender_Position', 'Defender_Team')
write.csv(shot.information, file = paste(strsplit(sportVU.file, '.json'), '_shot-data.csv', sep = ''))
Sys.time() - start.time


###################################################################################################
###################################################################################################

# IDENTIFY PASS DATA FOR EACH SHOT EVENT

###################################################################################################
###################################################################################################


start.time = Sys.time()
###################################################################################################
# 6) CREATE PASS EVENT TIME MAP FOR SPORTVU DATA
###################################################################################################
# INPUT: The exact time of a shot event in SportVU time that was captured in the shot information.  
#         
# OUTPUT: The exact time of the pass in SportVU data with additional features. 

# NOTE: The moment of a pass is based on the pass sequence, which is determined by the instances
#       the ball is within a certain distance of a player (1.5 Feet). By tracking when the ball is
#       only within this distance of an offensive player ONLY, the pass sequence can be determined.
#       Using the sequence, the only players relevant are the last two in the list. The pass is 
#       identified as the last moment the passer (second to last player) has the ball and the 
#       moment the shooter recieves the ball, which is based on the chronological sequence of the
#       play. 

# HELPERS
# Set PBP shot event look_back time. 
pass_look_back = 7.0

# Set up data.frame object to collect key information about each pass. 
# Information collected: Time (remaining time in quarter (sec), quarter), passer_id, 
# time of pass, shooter ID, time pass was received, time of shot, shot was made, if 
# marked as an assist in PBP and difference between time received and time of shot.  
pass.sportvu.key = data.frame(matrix(nrow = nrow(shot.sportvu.key), ncol = 9))
colnames(pass.sportvu.key) = c('passer_Id', 'time_pass', 'shooter', 'time_receive', 'time_shot', 'quarter', 
                               'shot_result', 'assist_status', 'time_to_shoot') 

# FUNCTION/PROCESS (For each shot event)
for(i in 1:nrow(shot.sportvu.key)){
  # Filter game information specifically to recorded shot event + look_back time.
  event.data = filter(game.information, 
                      quarter == shot.sportvu.key$quarter[i], 
                      game_clock >= shot.sportvu.key$time[i], 
                      game_clock < shot.sportvu.key$time[i] + pass_look_back)
  
  # Mark the offensive players on the court for this shot. 
  offense = as.character(offensive.players[i, ])
  
  # Filter to ball and offensive player locations for given shot event.  
  ball.location = filter(event.data, position == 'BALL')
  players.location = filter(event.data, player_id %in% offense)
  
  # Setup data.frame to collect time, player_id and distance for each event.
  possession.information = data.frame(matrix(nrow = 0, ncol = 5))
  colnames(possession.information) = c('game_clock', 'quarter', 'player_id', 'distance_ball', 'ball_height')
  
  counter = 1
  
  # For each time instance in shot event, calculate the distance between the ball and player.
  for(j in 1:nrow(ball.location)){
  
    # Filter down the offensive players locations at specific time of ball. 
    interval.data = filter(players.location, game_clock == ball.location$game_clock[j])
    
    # For each player in that time instance, record time (sec, quarter), player_id and distance to ball.
    for(k in 1:nrow(interval.data)){
      possession.information[counter, 1] = ball.location$game_clock[j]
      possession.information[counter, 2] = ball.location$quarter[j]
      possession.information[counter, 3] = as.character(interval.data$player_id[k])
      possession.information[counter, 4] = distanceToPoint(ball.location$x_loc[j], ball.location$y_loc[j], 
                                                     interval.data$x_loc[k], interval.data$y_loc[k])
      possession.information[counter, 5] = ball.location$z_loc[j]
      
      counter = counter + 1
    }
  }
  
  # Filter possesion information to when a player is less than 1.50 feet from ball.
  # This is a state when the player is considered to be controlling the ball. 
  ball.control.information = filter(possession.information, distance_ball < 1.50)
  
  # Identify the pass sequence of the possession. 
  pass.sequence = rep('', nrow(ball.control.information))
  counter = 1
  last.player = ''
  for(m in 1:nrow(ball.control.information)){
    if(last.player == as.character(ball.control.information$player_id[m])){
      # Do nothing.
    } else{
      pass.sequence[counter] = as.character(ball.control.information$player_id[m])
      last.player = as.character(ball.control.information$player_id[m])
      counter = counter + 1
    }
  }
  pass.sequence = pass.sequence[pass.sequence != '']
  
  # Check if there was a pass 7 seconds before the shot was taken. 
  if(length(pass.sequence) < 2){
    
    # If there were no passes in the possession, no assist occured. 
    # Marked as 'No Pass' with 0's in the remaining columns. 
    pass.sportvu.key[i, 1] = 'No Pass'
    pass.sportvu.key[i, 2] = 0
    pass.sportvu.key[i, 3] = 0
    pass.sportvu.key[i, 4] = 0
    pass.sportvu.key[i, 5] = 0
    pass.sportvu.key[i, 6] = 0
    pass.sportvu.key[i, 7] = 0
    pass.sportvu.key[i, 8] = 0
    pass.sportvu.key[i, 9] = 0
    
  } else {
    # Mark the passer as the second to last player in the pass sequence. 
    passer.identity = pass.sequence[length(pass.sequence) - 1]
    
    # Collect information about passer when he is in control of the ball. 
    passer.information = filter(ball.control.information, player_id == passer.identity)
    
    # Time of pass is the last moment the player is within 1.50 feet of the ball. 
    time.pass = passer.information$game_clock[nrow(passer.information)]
    
    # Time of pass received by shooter is the next recorded time interval in ball control data after the time of pass. 
    time.receive = ball.control.information$game_clock[which(ball.control.information$game_clock == time.pass) + 1]
    
    # Collect important information to establish a map for the given pass event. 
    pass.sportvu.key[i, 1] = passer.identity
    pass.sportvu.key[i, 2] = time.pass
    pass.sportvu.key[i, 3] = shot.sportvu.key$shooter[i]
    pass.sportvu.key[i, 4] = time.receive
    pass.sportvu.key[i, 5] = shot.sportvu.key$time[i]
    pass.sportvu.key[i, 6] = shot.sportvu.key$quarter[i]
    
    # Record if the shot was made or not. 
    if(shot.sportvu.key$result[i] == 0){
      pass.sportvu.key[i, 7] = 'MISS'
    } else{
      pass.sportvu.key[i, 7] = 'MAKE'
    }
    
    # Record if the shot was marked as an assist or not in PBP data. 
    if(shot.sportvu.key$assister[i] == ''){
      pass.sportvu.key[i, 8] = 'NO'
    } else{
      pass.sportvu.key[i, 8] = 'YES'
    }
    
    # Record time shooter held ball before shot. 
    pass.sportvu.key[i, 9] = time.receive - shot.sportvu.key$time[i]
  }
}
write.csv(pass.sportvu.key, file = paste(strsplit(sportVU.file, '.json'), '_pass-data.csv', sep = ''))
Sys.time() - start.time



