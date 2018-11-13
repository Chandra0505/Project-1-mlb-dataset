baseball_data = subset(Baseball, Baseball$at_bats>=100)
baseball_data
str(baseball_data)
baseball_data$batting_avg <- (baseball_data$hits / baseball_data$at_bats) 
plot(baseball_data$batting_avg,baseball_data$homeruns)
br <- lm(baseball_data$homeruns ~ baseball_data$batting_avg, data = baseball_data)
abline(br)
