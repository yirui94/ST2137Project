plot_scatterplot = function(filename) {
  dataset = read.csv(filename, header = TRUE);
  rentType = c("Casual", "Registered", "Total")

  #Users against day, by year
  for(i in 1:3) {
    jpeg(paste("Scatterplot of ", rentType[i], "Users against Day number.jpg"))

    xrange = c(0,370)
    yrange = range(dataset[,14+i])

    plot(xrange, yrange, type="n",
          xlab="Day Number",
          ylab=paste("Number of ", rentType[i], " Users"))

    lines(dataset[dataset$yr=="2011",]$instant, dataset[dataset$yr=="2011",14+i],
          type = 'p', col = 1, pch = 21)

    lines(dataset[dataset$yr=="2012",]$instant-365, dataset[dataset$yr=="2012",14+i],
          type = 'p', col = 2, pch = 22)

    title(paste("Scatterplot of ", rentType[i], "Users against Day number by Year"))

    legend(xrange[1], yrange[2], c("2011","2012"), col=c(1,2),
           pch = c(21,22),
           title="Year")

    dev.off()
  }

  weather = c("Spring", "Summer", "Fall", "Winter")
  #Users against day, by type of User
  for(i in 1:4) {
    jpeg(paste("Scatterplot of Users against Day number in", weather[i], ".jpg"))

    xrange = range(dataset[dataset$season==weather[i],]$instant)
    yrange = range(dataset[,16])

    plot(xrange, yrange, type="n",
          xlab="Day Number",
          ylab=paste("Number of Users"))

    lines(dataset[dataset$season==weather[i], ]$instant,
          dataset[dataset$season==weather[i], 15],
          type = 'p', col = 1, pch = 21)

    lines(dataset[dataset$season==weather[i],]$instant,
          dataset[dataset$season==weather[i], 16],
          type = 'p', col = 2, pch = 22)

    title(paste("Scatterplot of Users against Day number in", weather[i]))

    legend(xrange[1], yrange[2], c("Casual","Registered"), col=c(1,2),
           pch = c(21,22),
           title="User Type")

    dev.off()
  }

  #Users against temperature, by year
  for(i in 1:3) {
    jpeg(paste("Scatterplot of ", rentType[i], "Users against Temperature by Year.jpg"))

    xrange = range(dataset$rtemp)
    yrange = range(dataset[,14+i])

    plot(xrange, yrange, type="n",
          xlab="Temperature (Celsius)",
          ylab=paste("Number of ", rentType[i], " Users"))

    lines(dataset[dataset$yr=="2011",]$rtemp, dataset[dataset$yr=="2011",14+i],
          type = 'p', col = 1, pch = 21)

    lines(dataset[dataset$yr=="2012",]$rtemp, dataset[dataset$yr=="2012",14+i],
          type = 'p', col = 2, pch = 22)

    title(paste("Scatterplot of ", rentType[i], "Users against Temperature"))

    legend(xrange[1], yrange[2], c("2011","2012"), col=c(1,2),
           pch = c(21,22),
           title="Year")

    dev.off()
  }

  #Users against Apparent Temperature, by year
  for(i in 1:3) {
    jpeg(paste("Scatterplot of ", rentType[i], "Users against Apparent Temperature by Year.jpg"))

    xrange = range(dataset$ratemp)
    yrange = range(dataset[,14+i])

    plot(xrange, yrange, type="n",
          xlab="Apparent Temperature (Celsius)",
          ylab=paste("Number of ", rentType[i], " Users"))

    lines(dataset[dataset$yr=="2011",]$ratemp, dataset[dataset$yr=="2011",14+i],
          type = 'p', col = 1, pch = 21)

    lines(dataset[dataset$yr=="2012",]$ratemp, dataset[dataset$yr=="2012",14+i],
          type = 'p', col = 2, pch = 22)

    title(paste("Scatterplot of ", rentType[i], "Users against Apparent Temperature"))

    legend(xrange[1], yrange[2], c("2011","2012"), col=c(1,2),
           pch = c(21,22),
           title="Year")

    dev.off()
  }

  #Users against humidity, by year
  for(i in 1:3) {
    jpeg(paste("Scatterplot of ", rentType[i], "Users against Humidity by Year.jpg"))

    xrange = range(dataset$rhum)
    yrange = range(dataset[,14+i])

    plot(xrange, yrange, type="n",
          xlab="Humidity (%)",
          ylab=paste("Number of ", rentType[i], " Users"))

    lines(dataset[dataset$yr=="2011",]$rhum, dataset[dataset$yr=="2011",14+i],
          type = 'p', col = 1, pch = 21)

    lines(dataset[dataset$yr=="2012",]$rhum, dataset[dataset$yr=="2012",14+i],
          type = 'p', col = 2, pch = 22)

    title(paste("Scatterplot of ", rentType[i], "Users against Humidity"))

    legend(xrange[1], yrange[2], c("2011","2012"), col=c(1,2),
           pch = c(21,22),
           title="Year")

    dev.off()
  }

  #Users against WindSpeed, by year
  for(i in 1:3) {
    jpeg(paste("Scatterplot of ", rentType[i], "Users against WindSpeed by Year.jpg"))

    xrange = range(dataset$rwindspeed)
    yrange = range(dataset[,14+i])

    plot(xrange, yrange, type="n",
          xlab="Windspeed (km/h)",
          ylab=paste("Number of ", rentType[i], " Users"))

    lines(dataset[dataset$yr=="2011",]$rwindspeed, dataset[dataset$yr=="2011",14+i],
          type = 'p', col = 1, pch = 21)

    lines(dataset[dataset$yr=="2012",]$rwindspeed, dataset[dataset$yr=="2012",14+i],
          type = 'p', col = 2, pch = 22)

    title(paste("Scatterplot of ", rentType[i], "Users against Windspeed"))

    legend(xrange[1], yrange[2], c("2011","2012"), col=c(1,2),
           pch = c(21,22),
           title="Year")

    dev.off()
  }

}

#need to update base of i from 14 to 15
plot_scatterplot('bike_sharing_by_day.csv')
