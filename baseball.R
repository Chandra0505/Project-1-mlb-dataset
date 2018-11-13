#subsetting dataset for players who had atleast 100 at bats
Baseball_data <- subset(Baseball, Baseball$at_bats>=100)

#scatterplot of homeruns versus batting average.
plot (Baseball_data$bat_ave, Baseball_data$homeruns, xlab = "batting average", 
      ylab = "home runs", main = "batting average vs home runs")

#applying linear regression model on population
Baseball_data <- subset(Baseball, Baseball$at_bats>=100)
model2 = lm(formula = homeruns ~ bat_ave,
            data = Baseball_data)
summary(model2)

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(Baseball_data$homeruns, SplitRatio = 4/5)
training_set = subset(Baseball_data, split == TRUE)
test_set = subset(Baseball_data, split == FALSE)

#Fitting linear regression model
model = lm(formula = homeruns ~ bat_ave,
             data = training_set)
summary(model)

# Predicting a new result with Linear Regression
predict(model, test_set)

#Normal probability plot of standardised residuals
standard_res = rstandard(model)
qqnorm(standard_res, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(standard_res)

# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$bat_ave, y = training_set$homeruns),
             colour = 'red') +
  geom_line(aes(x = training_set$bat_ave, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('batting avg') +
  ylab('homeruns')

#scatter plot of residuals versus fitted
res = residuals(model)
fit = fitted(model)
plot(fit,res)

#take log of homeruns
Baseball_data$log_homerun = log(Baseball_data$homeruns)

#checking for outliers
boxplot(Baseball_data$log_homerun)
#removing -Inf values
Baseball_data = subset(Baseball_data , 
                       Baseball_data$log_homerun != -Inf)
#fitting linear regression model
model1 = lm(formula = log_homerun ~ bat_ave,
            data = Baseball_data)
summary(model1)

# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = Baseball_data$bat_ave, y = Baseball_data$log_homerun),
             colour = 'red') +
  geom_line(aes(x = Baseball_data$bat_ave, y = predict(model1, newdata = Baseball_data)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('batting avg') +
  ylab('log of homeruns')

#Normal probability plot of standardised residuals
standard_res1 = rstandard(model1)
qqnorm(standard_res1, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(standard_res1)

#scatter plot of residuals versus fitted
res1 = residuals(model1)
fit1 = fitted(model1)
plot(fit1, res1)

#taking batting average coloumn(bat_ave) as a vector
bat_ave = Baseball_data$bat_ave
#predicting homeruns with bat_ave = 0.300
predict(model1, data.frame(bat_ave=c(0.300)))

#size of typical error in predicting
sqrt(sum(residuals(model)^2) / df.residual(model))

#percentage of variability
#it is the R-Squared value in summary of model

#Hypothesis test 
cor.test(Baseball_data$bat_ave, Baseball_data$log_homerun, 
         method = "pearson")

#95% confidence interval for the unknown true slope of the regression line. 
confint(model1, conf.level = 0.95)

# 95% confidence interval for the population correlation coefficient.  
c = cor.test(Baseball_data$bat_ave, Baseball_data$log_homerun, 
             method = "pearson", conf.level = 0.95)

#95% confidance interval formean of homeruns for players 
#with batting avg. of 0.300
#import.packages("Publish")
library(Publish)
ci.mean(subset(Baseball_data$homeruns, Baseball_data$bat_ave==0.300))

#choosing a random player with batting avg. = 0.300
random_player = sample(subset(Baseball_data$homeruns, Baseball_data$bat_ave==0.300),1)






