

#####################################

#                                   #

# SABR101x: Sabermetrics 101        #

# on the edX platform for           #

# Boston University                 #

# Andy Andres and Morris Greenberg  #

#                                   #

#####################################

BJamesData <- read.csv(file.choose())

# scatterplot of the data

plot(BJamesData$RunRatio, BJamesData$WinRatio, 

     main="Run Ratio v. Win Ratio")

#adding log(x) versions to our data frame

BJamesData$WinRatio_log <- with(BJamesData, log(WinRatio))

BJamesData$RunRatio_log <- with(BJamesData, log(RunRatio))

# scatterplot of the data

plot(BJamesData$RunRatio_log, BJamesData$WinRatio_log, 

     main="Log Runs Ratio v. Log Win Ratio")

#regression with intercept

BJames_exponent1 <- 

  lm(BJamesData$WinRatio_log ~ BJamesData$RunRatio_log)

BJames_exponent1

#regression without intercept

BJames_exponent2 <- 

  lm(BJamesData$WinRatio_log ~ BJamesData$RunRatio_log + 0)

BJames_exponent2
