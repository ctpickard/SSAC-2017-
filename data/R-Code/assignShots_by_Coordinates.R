# Load required packages. 
require(dplyr)

# Load data file. Expected data file is PBP from NBAstuffer.com 
file = read.csv('(10-20-2015)-(04-13-2016)-combined-stats.csv')
data = rename(select(filter(file, event_type == 'shot', converted_x != 'unknown', converted_y != 'unknown', shot_distance <= 30), c(4:13, 23, 32, 39, 42, 43, 33)), 
              shooter = player, x_loc = converted_x, y_loc = converted_y) 

# Must convert (x, y) coordinates to numerics, convert all shots taken to one side of court. 
data$x_loc = as.numeric(as.character(data$x_loc))
data$y_loc = as.numeric(as.character(data$y_loc))
data$y_loc[data$y_loc > 47.0] = 94.0 - data$y_loc[data$y_loc > 47.0]


###################################################################################################
# 
# DEFINE GEOMETRY/SHOT ZONES OF COURT. 
#
###################################################################################################
# Define (x, y) coordinates of three-point arc. 

# ARC ANGLES
# Arc angle for wing zone. 
theta_wing = seq(atan(-22.000/10.000), atan(-8.875/22.1875), length.out = 100)

# Arc angle for straight-away zone.
theta_straight = seq(atan(-8.875/22.1875), atan(8.875/22.1875), length.out = 100)

# ARC ZONE (X, Y) COORDINATES

# arc_one is far wing. 
# arc_two is straight-away. 
# arc_three is near wing.
arc_one = mutate(data.frame(x = -23.90*sin(theta_wing) + 25, 
                     y = 23.90*cos(theta_wing) + 4), 
                    distance = sqrt((x - 25)^2 + (y - 4)^2))

arc_two = mutate(data.frame(x = -23.90*sin(theta_straight) + 25, 
                     y = 23.90*cos(theta_straight) + 4), 
                     distance = sqrt((x - 25)^2 + (y - 4)^2))

arc_three = mutate(data.frame(x = 23.90*sin(rev(theta_wing)) + 25, 
                       y = 23.90*cos(rev(theta_wing)) + 4), 
                       distance = sqrt((x - 25)^2 + (y - 4)^2))

# Define (x, y) coordinates of the dividing lines for three-point shots. 
x_1 = seq(16.125, 10, length.out = 100)
x_2 = seq(33.875, 40, length.out = 100)

line_one = mutate(data.frame(x = x_1, 
                             y = -2.5*x_1 + 66.5),
                  distance = sqrt((x - 25)^2 + (y - 4)^2))

line_two = mutate(data.frame(x = x_2, y = 2.5*x_2 - 58.5),
                  distance = sqrt((x - 25)^2 + (y - 4)^2))

# Define (x, y) coordinates of the dividing lines for two-point shots. 
x_3 = seq(19.0, 16.125, length.out = 50)
x_4 = seq(31.0, 33.875, length.out = 50)

line_three = mutate(data.frame(x = x_3, 
                             y = -2.5*x_3 + 66.5),
                  distance = sqrt((x - 25)^2 + (y - 4)^2))

line_four = mutate(data.frame(x = x_4, y = 2.5*x_4 - 58.5),
                  distance = sqrt((x - 25)^2 + (y - 4)^2))
#test = rbind(arc_one, line_one[line_one$distance <= 30, ], arc_two, line_two[line_two$distance <= 30, ], arc_three, line_three, line_four)
#plot(test$x, test$y)

# Define column indices for x_loc, y_loc and shot result.
x_loc_col = 14
y_loc_col = 15
shot_result_col = 16

getShot_Zone = function(x){
  # First filter by shot result. 
  shot_result = x[shot_result_col]
  x_loc = as.double(x[x_loc_col])
  y_loc = as.double(x[y_loc_col])
  
  # ===============================================================================================
  # TWO-POINT SHOTS
  if(shot_result == 2){
    
    # =============================================================================================
    # 1) NEAR-SIDE SHOTS.
    if(x_loc < 19.0){
      
      # 1A) BASELINE 
      if(y_loc < 5.0){
        return('F')
        
        # 1B) MID-RANGE, OUTSIDE OF THE LANE.     
      } else{
        if(nrow(line_three[x_loc < line_three[, 1] & y_loc < line_three[, 2], ]) != 0){
          return('G')
        } else{
          return('H')
        }
      }
    } 
    
    # =============================================================================================
    # 2) FAR-SIDE SHOTS.
    else if(x_loc > 31.0){
      
      # 2A) BASELINE 
      if(y_loc < 5.0){
        return('J')
        
      # 2B) MID-RANGE, OUTSIDE OF THE LANE. 
      } else{
        if(nrow(line_four[x_loc < line_four[, 1] & y_loc > line_four[, 2], ]) != 0){
          return('H')
        } else{
          return('I')
        }
      }
      
    # =============================================================================================
    # 3) STRAIGHT-AWAY, WITHIN FREE-THROW LINES.
    } else{
      
      # 3A) BELOW THE FREE-THROW LINE.
      if(y_loc >= 15.0){
        return('H')
        
      } else{
        
        # 3A.1) IN THE PAINT, 4 FOOT RADIUS OF HOOP.
        if(sqrt((x_loc - 25)^2 + (y_loc - 4)^2) <= 4){
          return('K')
          
          # 3A.2) EVERYTHING ELSE.
        } else{
          return('L')
        }
      }
    }
  
  # ===============================================================================================
  # THREE-POINT SHOTS
  } else{
    
    # 4) FAR-SIDE, CORNER THREE.
    if(x_loc > 47.00 && y_loc <= 14.00){
      return('A')
      
    # 5) NEAR-SIDE, CORNER THREE.   
    } else if(x_loc < 3.00 && y_loc <= 14.00){
      return('E')
      
    # WING/STRAIGHT-AWAY THREE.     
    } else{
      
      # 6) FAR-SIDE WING OR STRAIGHT-AWAY THREE.  
      if(nrow(arc_one[x_loc > arc_one[, 1] & y_loc > arc_one[, 2], ]) != 0){
        
        # 6A) FAR-SIDE WING THREE. 
        if(nrow(line_two[ x_loc > line_two[, 1] & y_loc < line_two[, 2], ]) != 0){
          return('B')
          
        # 6B) STRAIGHT-AWAY THREE.
        } else{
          return('C')
        }
        
      # 7) STRAIGHT-AWAY THREE. 
      } else if(nrow(arc_two[x_loc > arc_two[, 1] & y_loc > arc_two[, 2], ]) != 0){
        return('C')
      
      # 8) NEAR-SIDE WING OR STRAIGHT-AWAY THREE.
      } else{
        
        # 8A) STRAIGHT-AWAY THREE.
        if(nrow(line_one[ x_loc > line_one[, 1] & y_loc > line_one[, 2], ]) != 0){
          return('C')
          
        # 8B) NEAR-SIDE WING THREE. 
        } else{
          return('D')
        }
      }
    }
  }
}

start.time = Sys.time()
shot.assignment = apply(data, 1, function(x) getShot_Zone(x))
Sys.time() - start.time

table(shot.assignment)
