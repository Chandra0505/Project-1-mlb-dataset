# Libraries Used
# install.packages("ggplot2")
# install.packages('caTools')
# install.packages("Publish")

# Load the libraries

library(ggplot2)
library(caTools)
library(Publish)


# Subsetting dataset for players who had atleast 100 at bats
Baseball_data <- subset(Baseball, Baseball$at_bats>=100)

# Scatterplot of homeruns versus batting average.
plot (Baseball_data$bat_ave, Baseball_data$homeruns, xlab = "Batting Average -->", 
       ylab = "Home Runs -->", main = "Scatter Plot for Batting Average vs Home runs", pch=21,  bg="black")

# Applying linear regression model for population
Baseball_data <- subset(Baseball, Baseball$at_bats>=100)
model_pop = lm(formula = homeruns ~ bat_ave,
            data = Baseball_data)
summary(model_pop)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(Baseball_data$homeruns, SplitRatio = 4/5)
training_set = subset(Baseball_data, split == TRUE)
test_set = subset(Baseball_data, split == FALSE)

# Fitting linear regression model
model = lm(formula = homeruns ~ bat_ave,
           data = training_set)
summary(model)

# Predicting a new result with Linear Regression
predict(model, test_set)

# Density plot of standardised residuals
standard_res = rstandard(model)
plot(density(standard_res), main = "Density Plot of Standard Residuals")

# Normal probability plot of standardised residuals
qqnorm(standard_res, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(standard_res)


# Visualising the Linear Regression on training set of model
ggplot(data = training_set, aes(x = bat_ave, y = homeruns)) +
    geom_point()+
    geom_smooth(method = lm)+
    labs(title = "Scatter Plot for training set", 
         x = "Batting Average ---->", y = "Home Runs ---->", caption = "for 2002 season")+
    theme_bw()

# Scatter plot of residuals versus fitted
res = residuals(model)
fit = fitted(model)
plot(fit,res, main = "Residuals Vs Fitted", xlab = "Fitted Values", ylab = "Residuals",
        pch=21,  bg="black")
abline(a=0,b=0,col='blue')

# Tranformation of home runs using log transformation
Baseball_data$log_homerun = log(Baseball_data$homeruns)

# checking for outliers
boxplot(Baseball_data$log_homerun)

# removing Inf values
Baseball_data = subset(Baseball_data , 
                       Baseball_data$log_homerun != -Inf & Baseball_data$log_homerun != Inf)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(Baseball_data$log_homerun, SplitRatio = 4/5)
training_set_log = subset(Baseball_data, split == TRUE)
test_set_log = subset(Baseball_data, split == FALSE)

# Fitting linear regression model after transformation
model_log = lm(formula = log_homerun ~ bat_ave,
            data = training_set_log)
summary(model_log)

# Visualising the Linear Regression results
ggplot(data = training_set_log, aes(x = bat_ave, y = log_homerun)) +
    geom_point()+
    geom_smooth(method = lm)+
    labs(title = "Scatter Plot -> after transformaion", 
         x = "Batting Average ---->", y = "Home Runs ---->", caption = "for 2002 season")+
    theme_bw()

# Normal probability plot of standardised residuals
standard_res_log = rstandard(model_log)
plot(density(standard_res_log), main = "Density Plot -> after transformation")
qqnorm(standard_res_log, ylab="Standardized Residuals", xlab="Normal Scores")  # A quantile normal plot - good for checking normality
qqline(standard_res_log)

#scatter plot of residuals versus fitted
res_log = residuals(model_log)
fit_log = fitted(model_log)
plot(fit_log,res_log, main = "Residuals Vs Fitted -> after tranformation", xlab = "Fitted Values", ylab = "Residuals",
     pch=21,  bg="black")
abline(a=0,b=0,col='blue')

# taking batting average coloumn(bat_ave) as a vector
bat_ave = Baseball_data$bat_ave

# predicting homeruns with bat_ave = 0.300
predict(model, data.frame(bat_ave=c(0.300)))

# size of typical error in predicting
sqrt(sum(residuals(model_log)^2) / df.residual(model_log))

# percentage of variability
# it is the R-Squared value in summary of model

# Hypothesis test 
cor.test(Baseball_data$bat_ave, Baseball_data$log_homerun, 
         method = "pearson")

# 95% confidence interval for the unknown true slope of the regression line. 
confint(model_log, conf.level = 0.95)

# 95% confidence interval for the population correlation coefficient.  
cor.test(Baseball_data$bat_ave, Baseball_data$log_homerun, 
             method = "pearson", conf.level = 0.95)

# 95% confidance interval for mean of homeruns for players with batting avg. of 0.300
ci.mean(subset(Baseball_data$homeruns, Baseball_data$bat_ave==0.300))

#choosing a random player with batting avg. = 0.300
random_player = sample(subset(Baseball_data$homeruns, Baseball_data$bat_ave==0.300),1)
predict(model, data.frame(bat_ave=c(0.300)),
        conf.level = 0.95, interval = "prediction")





