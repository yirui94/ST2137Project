plot_qqplot = function(filename) {
  dataset = read.csv(filename, header = TRUE)

  #qqplot of weather indicators 2 by 2
  jpeg(paste("QQPlot of Weather Indicators.jpg"))
    par(mfrow=c(2,2))
    qqnorm(dataset$rtemp, main = paste("Normal Q-Q Plot for Temperature"))
    qqline(dataset$rtemp)

    qqnorm(dataset$ratemp, main = paste("Normal Q-Q Plot for Apparent Temperature"))
    qqline(dataset$ratemp)

    qqnorm(dataset$rhum, main = paste("Normal Q-Q Plot for Humidity"))
    qqline(dataset$rhum)

    qqnorm(dataset$rwindspeed, main = paste("Normal Q-Q Plot for Windspeed"))
    qqline(dataset$rwindspeed)
  dev.off()

  #qqplot of casual vs registered users 2 by 1
  jpeg(paste("QQPlot of Users.jpg"))
    par(mfrow=c(2,2))
    qqnorm(dataset$casual, main = paste("Normal Q-Q Plot for Casual Users"))
    qqline(dataset$casual)

    qqnorm(dataset$registered, main = paste("Normal Q-Q Plot for Registered Users"))
    qqline(dataset$registered)

    qqnorm(dataset$cnt, main = paste("Normal Q-Q Plot for Total Users"))
    qqline(dataset$cnt)
  dev.off()

  jpeg(paste("QQPlot of Users in Summer.jpg"))
    par(mfrow=c(2,2))
    qqnorm(dataset[dataset$season == "Summer",]$casual, main = paste("Normal Q-Q Plot for Casual Users"))
    qqline(dataset[dataset$season == "Summer",]$casual)

    qqnorm(dataset[dataset$season == "Summer",]$registered, main = paste("Normal Q-Q Plot for Registered Users"))
    qqline(dataset[dataset$season == "Summer",]$registered)

    qqnorm(dataset[dataset$season == "Summer",]$cnt, main = paste("Normal Q-Q Plot for Total Users"))
    qqline(dataset[dataset$season == "Summer",]$cnt)
  dev.off()

  jpeg(paste("QQPlot of Users in Fall.jpg"))
    par(mfrow=c(2,2))
    qqnorm(dataset[dataset$season == "Fall",]$casual, main = paste("Normal Q-Q Plot for Casual Users in Summer/Fall"))
    qqline(dataset[dataset$season == "Fall",]$casual)

    qqnorm(dataset[dataset$season == "Fall",]$registered, main = paste("Normal Q-Q Plot for Registered Users in Summer/Fall"))
    qqline(dataset[dataset$season == "Fall",]$registered)

    qqnorm(dataset[dataset$season == "Fall",]$cnt, main = paste("Normal Q-Q Plot for Total Users in Summer/Fall"))
    qqline(dataset[dataset$season == "Fall",]$cnt)
  dev.off()
}

plot_qqplot('bike_sharing_by_day.csv')
