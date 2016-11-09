plot_boxplot = function(filename) {
  dataset = read.csv(filename, header = TRUE)
  rentType = c("Casual", "Registered", "Total")

  # WORKING DAY
  for(i in 1:3) {
    jpeg(paste("Boxplot of", rentType[i], "Users against Working Days.jpg"))
    boxplot(dataset[,15+i]~dataset$workingday,
            main = paste("Boxplot of", rentType[i], "Users against Working Day"),
            xlab = "Working Day", ylab = paste("Number of", rentType[i], "Users"))
    dev.off()
  }

  # WEEK DAY
  for(i in 1:3) {
    jpeg(paste("Boxplot of", rentType[i], "Users against Week Days.jpg"))
    dataset$weekday = factor(dataset$weekday, c("Sunday", "Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday"))
    boxplot(dataset[,15+i]~dataset$weekday,
            main = paste("Boxplot of", rentType[i], "Users against Week Days"),
            xlab = "Week Day", ylab = paste("Number of", rentType[i], "Users"))
    dev.off()
  }

  # 24 HOUR
  for(i in 1:3) {
    jpeg(paste("Boxplot of", rentType[i], "Users against 24Hour.jpg"))
    boxplot(dataset[,15+i]~dataset$hr,
            main = paste("Boxplot of", rentType[i], "Users against 24Hour"),
            xlab = "Hour", ylab = paste("Number of", rentType[i], "Users"))
    dev.off()
  }

  # MONTH
  for(i in 1:3) {
    jpeg(paste("Boxplot of", rentType[i], "Users against Month.jpg"))
    dataset$mnth = factor(dataset$mnth, c("January", "February", "March", "April",
               "May", "June", "July", "August",
               "September", "October", "November",
               "December"))
    boxplot(dataset[,15+i]~dataset$mnth,
            main = paste("Boxplot of", rentType[i], "Users against Month"),
            xlab = "Month", ylab = paste("Number of", rentType[i], "Users"))
    dev.off()
  }

  # SEASON
  for(i in 1:3) {
    jpeg(paste("Boxplot of", rentType[i], "Users against Season.jpg"))
    dataset$season = factor(dataset$season, c("Spring", "Summer", "Fall", "Winter"))
    boxplot(dataset[,15+i]~dataset$season,
            main = paste("Boxplot of", rentType[i], "Users against Season"),
            xlab = "Season", ylab = paste("Number of", rentType[i], "Users"))
    dev.off()
  }

  # WEATHERSIT
  for(i in 1:3) {
    jpeg(paste("Boxplot of", rentType[i], "Users against Weather Type.jpg"))
    boxplot(dataset[,15+i]~dataset$weathersit,
            main = paste("Boxplot of", rentType[i], "Users against Weather Type"),
            xlab = "Weather Type", ylab = paste("Number of", rentType[i], "Users"))
    dev.off()
  }

}
plot_boxplot('bike_sharing_by_hour.csv')
