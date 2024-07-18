data <- read.csv('~/Downloads/cfb_games.csv')

#cleaning data
library(dplyr)
data <- data %>%
  rename(points_winner = Pts,
         points_loser = Pts.1)

#removing rankings from dataset for consistent team name format
data$Winner <- gsub("\\(\\d+\\)", "", data$Winner)
data$Winner <- gsub("[^a-zA-Z0-9]+", " ", data$Winner)
data$Winner <- trimws(data$Winner)
data$Loser <- gsub("\\(\\d+\\)", "", data$Loser)
data$Loser <- gsub("[^a-zA-Z0-9]+", " ", data$Loser)
data$Loser <- trimws(data$Loser)

# Calculate each team's record in wins                                                            
record_win <- data |>                                                                             
  dplyr::group_by(team = Winner) |>                                                          
  dplyr::summarize(                                                                               
    wins = dplyr::n(),                                                                            
    losses = 0,                                                                                   
    points_scored = sum(points_winner),                                                           
    points_allowed = sum(points_loser)                                                            
  )                                                                                               

# Calculate each team's record in losses                                                          
record_loss <- data |>                                                                            
  dplyr::group_by(team = Loser) |>                                                           
  dplyr::summarize(                                                                               
    wins = 0,                                                                                     
    losses = dplyr::n(),                                                                          
    points_scored = sum(points_loser),                                                            
    points_allowed = sum(points_winner)                                                           
  )                                                                                               

# Calculate each team's record (combining their wins and their losses)                            
record <- dplyr::bind_rows(record_win, record_loss) |>                                            
  dplyr::group_by(team) |>                                                                        
  dplyr::summarize(                                                                               
    wins = sum(wins),                                                                             
    losses = sum(losses),                                                                         
    points_scored = sum(points_scored),                                                           
    points_allowed = sum(points_allowed)                                                          
  )                                                                                               

head(record)   


#calculate win percentage function based on alpha 
calculate_win_pct <- function(record, alpha) {                                                    
  
  win_pct <- record |>                                                                            
    dplyr::mutate(                                                                                
      games = wins + losses,                                                                      
      actual_win_pct = wins / games,                                                              
      pythag_win_pct = points_scored^alpha / (points_scored^alpha + points_allowed^alpha)         
    ) |>                                                                                          
    dplyr::select(team, games, wins, losses, actual_win_pct, pythag_win_pct)                      
  
  return(win_pct)                                                                                 
}

#find best value of alpha for the dataset
grid_alpha <- seq(from = 0, to = 10, by = 0.1)                                                    
error <- rep(NA, length(grid_alpha))                                                              

for (i in 1:length(grid_alpha)) {                                                                 
  
  win_pct <- calculate_win_pct(record, grid_alpha[i])                                             
  
  error[i] <- win_pct |>                                                                          
    dplyr::summarize(error = mean((actual_win_pct - pythag_win_pct)^2)) |>                        
    with(error)                                                                                   
}                                                                                                 

grid_alpha[which.min(error)]                                                                      
plot(grid_alpha, error)                                                                           

#calculate record function
calculate_record <- function(data) {                                                              
  
  record_win <- data |>                                                                           
    dplyr::group_by(team = Winner) |>                                                        
    dplyr::summarize(                                                                             
      wins = dplyr::n(),                                                                          
      losses = 0,                                                                                 
      points_scored = sum(points_winner),                                                         
      points_allowed = sum(points_loser)                                                          
    )                                                                                             
  
  record_loss <- data |>                                                                          
    dplyr::group_by(team = Loser) |>                                                         
    dplyr::summarize(                                                                             
      wins = 0,                                                                                   
      losses = dplyr::n(),                                                                        
      points_scored = sum(points_loser),                                                          
      points_allowed = sum(points_winner)                                                         
    )                                                                                             
  
  record <- dplyr::bind_rows(record_win, record_loss) |>                                          
    dplyr::group_by(team) |>                                                                      
    dplyr::summarize(                                                                             
      wins = sum(wins),                                                                           
      losses = sum(losses),                                                                       
      points_scored = sum(points_scored),                                                         
      points_allowed = sum(points_allowed)                                                        
    )                                                                                             
  
  return(record)                                                                                  
}                                                                                                 
#calculate win percentages for first half of season
win_pct_1 <- data |>                                                                              
  dplyr::filter(Wk <= 8) |>                                                           
  calculate_record() |>                                                                           
  calculate_win_pct(alpha = 3.1)                                                                  
#filter out FCS teams (who have less than 6 games in dataset)
win_pct_1 <- win_pct_1[win_pct_1$games>=6, ]

#calculate win percentages for second half of season
win_pct_2 <- data |>                                                                              
  dplyr::filter(Wk > 8) |>                                                          
  calculate_record() |>                                                                           
  calculate_win_pct(alpha = 3.1)         
#filter out FCS teams (who have less than 4 games in dataset)
win_pct_2 <- win_pct_2[win_pct_2$games>=4, ]

#join first and second half win percentages for plotting/correlation
win_pct <- win_pct_1 |>                                                                           
  dplyr::left_join(win_pct_2, by = "team", suffix = c("_1", "_2"))                                

#first half actual vs second half actual
with(win_pct, cor(actual_win_pct_1, actual_win_pct_2))                                            
with(win_pct, plot( xlim=c(0, 1), ylim =c(0, 1), actual_win_pct_1, actual_win_pct_2, asp=1,))
abline(a=0, b=1, col="cyan", lwd =3)

#first half pythag vs second half actual
with(win_pct, cor(pythag_win_pct_1, actual_win_pct_2))
with(win_pct, plot(pythag_win_pct_1, actual_win_pct_2))
abline(a=0, b=1, col="darkgreen", lwd=3)

#first half residual vs second half residual
with(win_pct, cor(actual_win_pct_1 - pythag_win_pct_1, actual_win_pct_2 - pythag_win_pct_2))
with(win_pct, plot(actual_win_pct_1 - pythag_win_pct_1, actual_win_pct_2 - pythag_win_pct_2, xlab="First-Half Residual", 
      ylab="Second-Half Residual"))
abline(a=0, b=1, col="tomato4", lwd=3)
