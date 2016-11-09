regression = function(filename) {
  dataset = read.csv(filename, header = TRUE)

  # Pearson Correlation Coefficient Matrix
  sink("correlation.txt")
    cat(cor(cbind(dataset$cnt, dataset$casual,
      dataset$registered, dataset$rhum,
      dataset$rwindspeed, dataset$rtemp,
      dataset$ratemp),
      method = "pearson"),"\n")
  sink()

  sink("correlation_casual.txt", append = TRUE)
    model1 = lm(dataset$casual~dataset$rtemp)
    summary(lm(dataset$casual~dataset$rtemp))
    confint(lm(dataset$casual~dataset$rtemp))
  sink()

  jpeg(paste("Scatter and Residual Plot for Casual Users Against Temperature.jpg"))
      par(mfrow=c(2,2))
      plot(dataset$casual~dataset$rtemp, pch = 16,
        xlab="Temperature", ylab="Number of Casual Users")
      model1 = lm(dataset$casual~dataset$rtemp)

      abline(model1, lty=2)
      title("Scatter plot and Regression Line")

      rs = model1$resid
      fv = model1$fitted
      plot(rs~dataset$rtemp, xlab="Temperature", ylab="Residuals")
      abline(h=0,lty=2)
      # Normal QQ plot
      qqnorm(rs, ylab="Residuals", xlab="Normal Quantiles")
      qqline(rs)
  dev.off()



  jpeg(paste("Scatter and Residual Plot for Registered Users Against Temperature.jpg"))
    par(mfrow=c(2,2))
    plot(dataset$registered~dataset$rtemp, pch = 16, xlab="Temperature", ylab="Number of Registered Users")
    model2 = lm(dataset$registered~dataset$rtemp)

    abline(model2, lty=2)
    title("Scatter plot and Regression Line")

    rs = model2$resid
    fv = model2$fitted
    plot(rs~dataset$rtemp, xlab="Temperature", ylab="Residuals")
    abline(h=0,lty=2)
    # Normal QQ plot
    qqnorm(rs, ylab="Residuals", xlab="Normal Quantiles")
    qqline(rs)

  dev.off()

  sink("correlation_registered.txt", append = TRUE)
    summary(lm(dataset$registered~dataset$rtemp))
    confint(lm(dataset$registered~dataset$rtemp))
  sink()
}
regression("bike_sharing_by_day.csv")
