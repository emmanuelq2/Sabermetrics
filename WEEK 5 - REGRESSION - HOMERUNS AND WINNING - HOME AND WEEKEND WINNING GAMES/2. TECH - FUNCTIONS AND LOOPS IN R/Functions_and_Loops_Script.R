library(ggplot2)
#create square function
square <- function(number){
  return(number*number)
}
square(10)
square(9)

#create birthday function
birthday <- function(name,age){
  paste("Happy Birthday ",
        age,
        "th Birthday, ",
        name, 
        '!', sep = '')
}
birthday('Ethan',20)
birthday('Andy',70)
birthday('Morris',1)

#basic loops
for(i in 1:10){
  print(i*2)
}

#create random coordinates
test_data <- data.frame(
  num = c(1,2,3,4,5),
  x = rnorm(n = 5,mean = 5,sd = 2),
  y = rnorm(n = 5,mean = 5,sd = 2)
)

qplot(test_data$x,test_data$y,xlim = c(0,10),ylim = c(0,10))
with(test_data,cor(x,y))
#spurious correlation!
     
#print num, x, y, distance between points and (0,0)
for(i in 1:nrow(test_data)){
  with(test_data,print(
    c(num[i],x[i],y[i],sqrt(square(x[i]) + square(y[i])))))
}

retrosheet <- read.csv('retrosheet00-09.csv',stringsAsFactors = FALSE)

#create home_win variable
retrosheet$home_win <- ifelse(retrosheet$home_score > retrosheet$visitor_score,1,0)

#home winning percentage
sum(retrosheet$home_win)/nrow(retrosheet)


retrosheet$weekendsat <- ifelse(day == "Sat" | day = "Sun", 1,0)
retrosheet$weekendsun <- ifelse(retrosheet$day == "Sun", 1,0)

sum(retrosheet$weekendsat)/nrow(retrosheet)
sum(retrosheet$weekendsun)/nrow(retrosheet)

#create a function that returns W, G, and WP all time for a team
W_G_WP <- function(team,df){
  W <- 0
  G <- 0
  sub <- subset(df,home == team | visitor == team)
  for(i in 1:nrow(sub)){
    W <- W + ifelse(sub$home[i] == team, sub$home_win[i],
                    ifelse(sub$home_win[i] == 1,0,1))
    G <- G + 1
  }
  return(c(W,G,W/G))
}

W_G_WP("BOS",retrosheet)

#convert retrosheet factor to string

#create new data frame with results for ALL teams
#create new data frame with results for ALL teams
results_df <- data.frame(team = unique(retrosheet$home),
                         W = 0,
                         G = 0,
                         WP = 0,
                         stringsAsFactors = FALSE)
for(i in 1:nrow(results_df)){
    team <- results_df$team[i]
    results_df[i,] <- c(team,W_G_WP(team,retrosheet))
    print(c(team,W_G_WP(team,retrosheet)))
}





weekWP <- function(team,df){
    
    sub <- subset(df,home == team | visitor == team)
    
    weekend <- subset(sub,weekend == 1)
    
    week <- subset(sub,weekend == 0)
    
    weekWP <- W_G_WP(team,week)[3]
    
    weekendWP <- W_G_WP(team,weekend)[3]
    
    return(c(team,weekWP,weekendWP))
    
}
weekWP("BOS",retrosheet)


#create new data frame with results for ALL teams
results_df <- data.frame(team = unique(retrosheet$home),
                         W = 0,
                         G = 0,
                         WP = 0,
                         stringsAsFactors = FALSE)
for(i in 1:nrow(results_df)){
    team <- results_df$team[i]
    results_df[i,] <- c(team,weekWP(team,retrosheet))
    print(c(team,weekWP(team,retrosheet)))
}
