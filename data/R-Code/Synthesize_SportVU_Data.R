require(RCurl)
require(jsonlite)
require(dplyr)
require(plyr)

# NOTE: Import and load Read_SportVU_FUnction.R for this script to run. 

###################################################################################################
# READ IN SPORTVU AND PBP DATA FILES
###################################################################################################
# Load in the SportVU and play-by-play data. 
sportVU.file = '0021500514[ORL-DET-1-04-16].json'
game.data = sportvu_convert_json(sportVU.file)

###################################################################################################

# CONDENSE SPORTVU DATA

###################################################################################################
# Define column names for data.frame that will collect synthesized data.
column.names = c('player_id', 'position', 'team_id', 'x_loc', 'y_loc', 'z_loc', 'game_clock', 'quarter')

# Get number of quarters in game (potential OT games)
num.quarters = length(unique(game.data$quarter))

# Set interval
interval = 0.1
game.information = data.frame(matrix(nrow = 0, ncol = 8))
colnames(game.information) = column.names

start.time = Sys.time()
for(i in 1:num.quarters){
  # Filter to all data in quarter. 
  quarter.data = filter(game.data, quarter == i)
  
  # Setup time sequence for quarter to collect information. 
  time.sequence = seq(0, 720 + interval, by = interval)
  quarter.information = data.frame(matrix(nrow = 0, ncol = 8))
  
  for(j in 2:length(time.sequence)){
    # Filter to all data within time sequence. 
    sequence.data = filter(quarter.data, 
                           game_clock >= time.sequence[j - 1],
                           game_clock < time.sequence[j])
    
    # Collect all players on court during sequence (should be 11, including ball).
    players = unique(sequence.data$player_id)
    
    # Setup dump database to collect player_id, x, y, z, game_clock sequence, quarter
    sequence.information = data.frame(matrix(nrow = length(players), ncol = 8))
    
    if(length(players) == 0){
      # Do nothing, no data available for this sequence. 
    } else{
      for(k in 1:length(players)){
        # Filter information to each player on court during sequence. 
        player.data = filter(sequence.data, 
                             player_id == players[k])
        
        # Get player personal data. 
        sequence.information[k, 1] = paste(player.data$firstname[1], player.data$lastname[1], sep = ' ')
        
        # Specify specific ball data.
        if(is.na(player.data$position[1])){
          sequence.information[k, 2] = 'BALL'
          sequence.information[k, 3] = 'NEUTRAL'
          
        } else{
          sequence.information[k, 2] = player.data$position[1]
          sequence.information[k, 3] = player.data$team_id[1]
        }
        
        # Get player tracking data. 
        sequence.information[k, 4] = mean(player.data$x_loc)
        sequence.information[k, 5] = mean(player.data$y_loc)
        sequence.information[k, 6] = mean(player.data$radius)
      }
      
      # Associate quarter game_clock and quarter information to tracking data. 
      sequence.information[, 7] = time.sequence[j -1]
      sequence.information[, 8] = i
    }
    
    # Paste sequence specific information to quarter information. 
    quarter.information = rbind(quarter.information, sequence.information)
  }
  
  # Set colnames for synthesized quarter information, order chronologically.
  colnames(quarter.information) = column.names
  quarter.information = arrange(quarter.information, -game_clock)
  
  # Paste quarter information to total game information data.frame
  game.information = rbind(game.information, quarter.information)
}
Sys.time() - start.time
write.csv(game.information, file = paste(strsplit(sportVU.file, '.json'), '_tracking-data.csv', sep = ''))
