Baseball_data <- subset(Baseball, Baseball$at_bats>=100)

plot (Baseball_data$bat_ave, Baseball_data$homeruns, xlab = "batting average", 
      ylab = "home runs")

df <- data.frame(Baseball_data$homeruns,Baseball_data$bat_ave)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$Baseball_data.homeruns, SplitRatio = 4/5)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
model = lm(formula = Baseball_data.homeruns ~ .,
             data = training_set)
model
summary(model)
abline(model)
# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = df$Baseball_data.bat_ave, y = df$Baseball_data.homeruns),
             colour = 'red') +
  geom_line(aes(x = df$Baseball_data.bat_ave, y = predict(model, newdata = df)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('batting avg') +
  ylab('homeruns')
res = residuals(model)
fit = fitted(model)
plot(fit,res)
df$log_homerun = log(df$Baseball_data.homeruns)
df1 = subset(df , df$log_homerun != -Inf)
model1 = lm(formula = df1$log_homerun ~ df1$Baseball_data.bat_ave,
            data = df1)
model1
summary(model1)
library(ggplot2)
ggplot() +
  geom_point(aes(x = df1$Baseball_data.bat_ave, y = df1$log_homerun),
             colour = 'red') +
  geom_line(aes(x = df1$Baseball_data.bat_ave, y = predict(model1, newdata = df1)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('batting avg') +
  ylab('log of homeruns')
res1 = residuals(model1)
fit1 = residuals(model1)
plot(fit1, res1)
model2 = lm(formula = homeruns ~ bat_ave,
           data = Baseball_data)
summary(model2)
summary(model)
# Predicting a new result with Linear Regression
predict(model, test_set)
summary(model1)
y = 13.6375*0.300-1.4386
y
