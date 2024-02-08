# turn monthly into seasonal averages
weather <- read.csv("/Users/lukebeebe/Documents/School/Rutgers/2024 Spring/Independent Study/HF_data.csv")
season <- c(4,4,1,1,1,2,2,2,3,3,3,4)
weather <- cbind(season, weather)
weather_season <- (weather[1,]+weather[2,])/2 # first winter only average of 2 months
weather <- weather[-c(1,2),]
for(m in 1:floor(length(weather$year)/3)){ # does not include last winter of 1 month
  season <- (weather[1,]+weather[2,]+weather[3,])/3
  weather_season <- rbind(weather_season, season)
  weather <- weather[-c(1,2,3),]
}
weather_season$year <- as.integer(ceiling(weather_season$year))
weather_season <- weather_season[,-c(3)]
dim(weather_season)
head(weather_season)