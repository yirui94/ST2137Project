plot_histogram = function(filename) {
        dataset = read.csv(filename, header = TRUE)

        #Histogram of Weather Type
        jpeg(paste("Histogram of Weather Type.jpg"))
          hist(dataset$weathersit, include.lowest = TRUE, freq=TRUE,
               col="grey", main = paste("Histogram of Weather Type"),
               xlab = "Weather Type", ylab="Number of Days",
               ylim = c(0, 500), axes = TRUE,
               breaks=seq(0,4,by=1))
        dev.off()

        #Histogram of the four weather indications 2 by 2
        jpeg(paste("Histogram of Weather Indicators.jpg"))
          par(mfrow=c(2,2))
          hist(dataset$rtemp, include.lowest = TRUE, freq=TRUE,
               col="grey", main = paste("Histogram of Temperature"),
               xlab = "Temperature (Celsius)", ylab="Number of Days",
               axes = TRUE, ylim = c(0, 200))
          xpt <- seq(-10,100,0.1)
          ypt <- dnorm(seq(-10,100,0.1), mean(dataset$rtemp), sd(dataset$rtemp))
          aypt <- ypt*length(dataset$rtemp)*5
          lines(xpt,aypt)

          hist(dataset$ratemp, include.lowest = TRUE, freq=TRUE,
              col="grey", main = paste("Histogram of Apparent Temperature"),
              xlab = "Apparent Temperature (Celsius)", ylab="Number of Days",
              axes = TRUE, ylim = c(0, 200))
          xpt <- seq(-10,100,0.1)
          ypt <- dnorm(seq(-10,100,0.1), mean(dataset$ratemp), sd(dataset$ratemp))
          aypt <- ypt*length(dataset$ratemp)*5
          lines(xpt,aypt)

          hist(dataset$rhum, include.lowest = TRUE, freq=TRUE,
               col="grey", main = paste("Histogram of Humidity"),
               xlab = "Humidity (%)", ylab="Number of Days",
               axes = TRUE, ylim = c(0,250))
           xpt <- seq(-10,100,0.1)
           ypt <- dnorm(seq(-10,100,0.1), mean(dataset$rhum), sd(dataset$rhum))
           aypt <- ypt*length(dataset$rhum)*10
           lines(xpt,aypt)

          hist(dataset$rwindspeed, include.lowest = TRUE, freq=TRUE,
              col="grey", main = paste("Histogram of Windspeed"),
              xlab = "Windspeed (km/h)", ylab="Number of Days",
              axes = TRUE, ylim = c(0, 300))
          xpt <- seq(-10,100,0.1)
          ypt <- dnorm(seq(-10,100,0.1), mean(dataset$rwindspeed), sd(dataset$rwindspeed))
          aypt <- ypt*length(dataset$rwindspeed)*5
          lines(xpt,aypt)


        dev.off()
}

plot_histogram('bike_sharing_by_day.csv')
