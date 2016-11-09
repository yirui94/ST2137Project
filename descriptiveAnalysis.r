skew = function(x) {
  n = length(x)
  m3 = mean((x - mean(x))^3)
  m2 = mean((x - mean(x))^2)
  sk = m3 / m2 ^ (3/2) * sqrt(n * (n-1)) / (n-2)
  return(sk)
}

kurt = function(x){
  n <- length(x)
  m4 <- mean((x-mean(x))^4)
  m2 <- mean((x-mean(x))^2)
  kurt = (n-1)/((n-2)*(n-3))*((n+1)*m4/m2^2-3*(n-1))
  return(kurt)
}

descriptiveAnalysis = function(filename) {
  dataset = read.csv(filename, header = TRUE)
  sink("descriptive.txt")
  #Total 2011
  cat("Total Users descriptive statistics for 2011\n")
  cat(summary(dataset[dataset$yr=="2011",]$cnt), "\n")
  cat(mean(dataset[dataset$yr=="2011",]$cnt, trim = 0.2), "\n")
  cat(sd(dataset[dataset$yr=="2011",]$cnt), "\n")
  cat(mad(dataset[dataset$yr=="2011",]$cnt), "\n")
  cat(IQR(dataset[dataset$yr=="2011",]$cnt), "\n")

  #Total 2012
  cat("Total Users descriptive statistics for 2012\n")
  cat(summary(dataset[dataset$yr=="2012",]$cnt), "\n")
  cat(mean(dataset[dataset$yr=="2012",]$cnt, trim = 0.2), "\n")
  cat(sd(dataset[dataset$yr=="2012",]$cnt), "\n")
  cat(mad(dataset[dataset$yr=="2012",]$cnt), "\n")
  cat(IQR(dataset[dataset$yr=="2012",]$cnt), "\n")

  #Casual 2011
  cat("Casual Users descriptive statistics for 2011\n")
  cat(summary(dataset[dataset$yr=="2011",]$casual), "\n")
  cat(mean(dataset[dataset$yr=="2011",]$casual, trim = 0.2), "\n")
  cat(sd(dataset[dataset$yr=="2011",]$casual), "\n")
  cat(mad(dataset[dataset$yr=="2011",]$casual), "\n")
  cat(IQR(dataset[dataset$yr=="2011",]$casual), "\n")

  #Casual 2012
  cat("Casual Users descriptive statistics for 2012\n")
  cat(summary(dataset[dataset$yr=="2012",]$casual), "\n")
  cat(mean(dataset[dataset$yr=="2012",]$casual, trim = 0.2), "\n")
  cat(sd(dataset[dataset$yr=="2012",]$casual), "\n")
  cat(mad(dataset[dataset$yr=="2012",]$casual), "\n")
  cat(IQR(dataset[dataset$yr=="2012",]$casual), "\n")

  #Registered 2011
  cat("Registered Users descriptive statistics for 2011\n")
  cat(summary(dataset[dataset$yr=="2011",]$registered), "\n")
  cat(mean(dataset[dataset$yr=="2011",]$registered, trim = 0.2), "\n")
  cat(sd(dataset[dataset$yr=="2011",]$registered), "\n")
  cat(mad(dataset[dataset$yr=="2011",]$registered), "\n")
  cat(IQR(dataset[dataset$yr=="2011",]$registered), "\n")

  #Registered 2012
  cat("Registered Users descriptive statistics for 2012\n")
  cat(summary(dataset[dataset$yr=="2012",]$registered), "\n")
  cat(mean(dataset[dataset$yr=="2012",]$registered, trim = 0.2), "\n")
  cat(sd(dataset[dataset$yr=="2012",]$registered), "\n")
  cat(mad(dataset[dataset$yr=="2012",]$registered), "\n")
  cat(IQR(dataset[dataset$yr=="2012",]$registered), "\n")

  #Skew & Kurtosis of Humidity
  cat("Humidity Skew & Kurtosis\n")
  cat(skew(dataset$rhum), "\n")
  cat(kurt(dataset$rhum), "\n")
  #Windspeed
  cat("Windspeed Skew & Kurtosis\n")
  cat(skew(dataset$rwindspeed), "\n")
  cat(kurt(dataset$rwindspeed), "\n")
  #Temperature
  cat("Temperature Skew & Kurtosis\n")
  cat(skew(dataset$rtemp), "\n")
  cat(kurt(dataset$rtemp), "\n")
  #Apparent Temperature
  cat("Apparent Temperature Skew & Kurtosis\n")
  cat(skew(dataset$ratemp), "\n")
  cat(kurt(dataset$ratemp), "\n")

  #Casual
  cat("Casual Skew & Kurtosis\n")
  cat(skew(dataset$casual), "\n")
  cat(kurt(dataset$casual), "\n")

  #Registerd
  cat("Registered Skew & Kurtosis\n")
  cat(skew(dataset$registered), "\n")
  cat(kurt(dataset$registered), "\n")

  sink()
}

descriptiveAnalysis("bike_sharing_by_hour.csv")
