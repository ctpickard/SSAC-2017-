###################################################################################################
# SCRIPT: getNBA_Player_Data.R
###################################################################################################
# FUNCTIONALITY
# =============
# NOTE:
# First, make sure the data read input is pointing to the correct .xml file directory. 
# Second, data expected input is the .xml file from OFFICIAL STATS SportVU format.
#
# When run, this script will take in the STATS SportVU player information key in .xml format
# and will parse it into a .csv file that returns player features that includes:
# 
# Name
# Global and player ID codes
# Team name, abbreviation, code
# Position name and code
# Jersey number
# Birth, rookie year, years of experience
# College, height, weight
###################################################################################################
# Load required packages
require(XML)
require(methods)

# Import XML data file, convert to list. 
data = xmlParse('~/Desktop/NBA_ALL_ROSTER.XML')
data = xmlToList(data)

# Filter list down to team level. 
NBA_TEAM_DATA = data$`sports-rosters`$`nba-rosters`

# Create data.frame to store parsed data. 
league_data = data.frame(matrix(nrow = 0, ncol = 15))
columns = c('player', 'global_ID', 'player_ID', 'team', 'team_abr', 'team_code', 'position_name', 'position_code', 'jersey_number',
            'experience', 'rookie_year', 'birth_year', 'college' , 'height_inch', 'weight_lbs')
colnames(league_data) = columns

# For each team, get team information:
#   - Name
#   - Abbreviation
#   - Code (believed to be SportVU code)
for(i in 1:length(NBA_TEAM_DATA)){
  team_data = NBA_TEAM_DATA[i]$`nba-roster`
  team_name = paste(as.character(team_data$`team-city`[1]), team_data$`team-name`[1], sep = ' ')
  abbreviation = toupper(as.character(team_data$`team-name`[2]))
  SportVU_code = as.character(team_data$`team-code`[2])
  
  # NOTE: First 5 elements of team data are always, team/coach related. 
  
  # For each NBA team roster get player information:
  #   - Name, global and player ID code.
  #   - Primary position name and ID code.
  #   - Jersey number
  #   - Experience, birth year, rookie year
  #   - College, height, weight
  
  # Create local data.frame to store information. 
  team_roster = data.frame(matrix(nrow = (length(team_data) - 5), ncol = 15))
  colnames(team_roster) = columns
  for(j in 6:length(team_data)){
    # Get player data. 
    player_data = team_data[j]
    
    # Get player name, global and player ID codes. 
    player_name = as.character(player_data$`nba-player`$name[4])
    global_ID = as.character(player_data$`nba-player`$`player-code`[1])
    player_ID = as.character(player_data$`nba-player`$`player-code`[2])
    
    # Get player primary position and position code name. 
    position_ID = as.character(player_data$`nba-player`$`primary-position`[1])
    position_name = as.character(player_data$`nba-player`$`primary-position`[2])
    
    # Get player jersey number. 
    jersey_number = as.character(player_data$`nba-player`$`player-number`[1])
    
    # Get player birth year, rookie year and number of years of experience.
    birth_year = as.character(player_data$`nba-player`$`birth-date`[1])
    rookie_year = as.character(player_data$`nba-player`$`first-year`[1])
    experience = as.character(player_data$`nba-player`$experience[1])
    
    # Get player college (reports 'none' if foreign), height (inches) and weight (lbs.)
    college = as.character(player_data$`nba-player`$school[4])
    height = as.character(player_data$`nba-player`$height[1])
    weight = as.character(player_data$`nba-player`$weight[1])
    
    # Combine splits and insert into local team data.frame. 
    player_split = c(player_name, global_ID, player_ID, 
                     team_name, abbreviation, SportVU_code, 
                     position_name, position_ID, 
                     jersey_number, 
                     experience, rookie_year, birth_year, 
                     college, 
                     height, weight)
    names(player_split) = columns
    
    team_roster[j - 5, ] = player_split
  }
  
  # Bind team data to league data. 
  league_data = rbind(league_data, team_roster)
}
write.csv('~/Desktop/NBA_PLAYER_DATA.csv')
