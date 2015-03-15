stepCounting <- function(fileName){
     require("dplyr")
     require("lubridate")
     
     # load data and pre-process
     stepFile <- read.csv(fileName)
     stepFile$date <- ymd(stepFile$date)
     stepFile$time <- hm(paste( 
                      floor(stepFile$interval/100),
                      stepFile$interval %% 100,
                      sep=":"))
     stepFile$dayOfWeek <- wday(stepFile$date, label = TRUE)
     stepFile$weekEnd <- stepFile$dayOfWeek == "Sat" | stepFile$dayOfWeek == "Sun"
     
     # mean number of steps per day
     perDay <- group_by(select(stepFile, steps, date), date)
     sPD <- summarise(perDay, stepSum=sum(steps, na.rm=T))
     meanSPD <- mean(sPD$stepSum)
     print(paste("Mean Number of Steps per Day: ", meanSPD))
     medianSPD <- median(sPD$stepSum)
     print(paste("Median Number of Steps per Day: ", medianSPD))
     
     # plot histogram of steps per day
     png("RepData_PeerAssessment1//figure//stepsPerDay.png")
     hist(sPD$stepSum, 
          breaks=8, 
          main='Steps per Day',
          xlab='Steps per Day',
          ylab='Frequency')
     abline(v=meanSPD, col = "red", lwd=6)
     abline(v=medianSPD, col = "blue", lwd=6)
     legend("topright", 
            c("Mean Steps per Day", "Median Steps per Day"), 
            col=c("red", "blue"), lwd=6)
     dev.off()
     
     # average activity pattern per day
     perInt <- group_by(select(stepFile, steps, date, interval), interval)
     aPI <- summarise(perInt, intAvg=mean(steps, na.rm=T))
     maxNumOfSteps <- max(aPI$intAvg)
     print(paste("Maximum Number of Steps:", 
                 maxNumOfSteps))
     
     # plot steps per 5-min interval
     png("RepData_PeerAssessment1//figure//stepsPerInterval.png")
     plot(aPI$interval, aPI$intAvg, type = "l", 
          main = "Average Daily Activity Pattern", 
          xlab = "Time", 
          ylab = "Steps per 5-min Interval")
     dev.off()
     
     # impute missing values
     print(paste("Number of Missing Values for Steps: ", 
                 sum(is.na(stepFile$steps))))
     fixStepFile <- stepFile
     for(i in 1:length(fixStepFile$steps)){
          if(is.na(fixStepFile$steps[i])){
               fixStepFile$steps[i] <- round(aPI$intAvg[aPI$interval == fixStepFile$interval[i]])
          }
     }
     
     # mean number of steps per day after imputing missing values
     perDay <- group_by(select(fixStepFile, steps, date), date)
     sPD <- summarise(perDay, stepSum=sum(steps, na.rm=T))
     meanSPD <- mean(sPD$stepSum)
     medianSPD <- median(sPD$stepSum)
     
     # plot histogram of steps per day after imputing missing values
     png("RepData_PeerAssessment1//figure//fixedStepsPerDay.png")
     hist(sPD$stepSum, 
          breaks=8, 
          main='Steps per Day after imputing missing values',
          xlab='Steps per Day',
          ylab='Frequency')
     abline(v=meanSPD, col = "red", lwd=12)
     abline(v=medianSPD, col = "blue", lwd=6)
     legend("topright", 
            c("Mean Steps per Day", "Median Steps per Day"), 
            col=c("red", "blue"), lwd=6)
     dev.off()
     
}