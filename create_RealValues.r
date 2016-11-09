for(i in 1:2) {
  # Calculating the actual, non-normalised values of
  # temperature (Celsius), apparent temperature (Celsius),
  # humidity (Percentage), WindSpeed (km/h)
  if(i == 1) {
    bike_sharing = read.csv('hour.csv', header = TRUE)
  } else {
    bike_sharing = read.csv('day.csv', header = TRUE)
  }
  bike_sharing$rtemp <- bike_sharing$temp * (39 - (-8)) + (-8)
  bike_sharing$ratemp <- bike_sharing$atemp * (50 - (-16)) + (-16)
  bike_sharing$rhum <- bike_sharing$hum * 100
  bike_sharing$rwindspeed <- bike_sharing$windspeed * 67

  # Label Seasons by String values
  # 1: Spring
  # 2: Summer
  # 3: Fall
  # 4: Winter
  bike_sharing$season = factor(bike_sharing$season,
  levels = c(1,2,3,4),
  labels = c("Spring", "Summer", "Fall", "Winter"))

  # Label Year by String values
  # 0: '2011'
  # 1: '2012'
  bike_sharing$yr = factor(bike_sharing$yr,
  levels = c(0,1),
  labels = c("2011", "2012"))

  # Label Month by String values
  # 1: January
  # 12: December
  bike_sharing$mnth = factor(bike_sharing$mnth,
  levels = seq(from=1, to=12, by=1),
  labels = c("January", "February", "March", "April",
             "May", "June", "July", "August",
             "September", "October", "November",
             "December"))

  # Label Weekday by string values
  # 0: Sunday
  # 6: Saturday
  bike_sharing$weekday = factor(bike_sharing$weekday,
  levels = seq(from=0, to=6, by=1),
  labels = c("Sunday", "Monday", "Tuesday", "Wednesday",
             "Thursday", "Friday", "Saturday"))
  if(i == 1) {
    write.csv(bike_sharing, "bike_sharing_by_hour.csv")
  }else {
    write.csv(bike_sharing, "bike_sharing_by_day.csv")
  }
}
