library(stringr)
library(ggplot2)

# Read string data and split into data frame
df = readLines("Baseball.txt")
df = as.data.frame(do.call(rbind, strsplit(df, split=" {2,10}")), stringsAsFactors=FALSE)

df = df[-1, ]
names(df) = c("firstname", "lastname", "age", "team", "games", "at_bats","runs", "hits", "doubles", "triples", "home_runs", "RBIs","walks", "strikeouts","bat_ave", "on_base_pct", "slugging_pct", "stolen_bases", "caught_stealing")

# View(df)
# str(df)

df$at_bats = as.numeric(df$at_bats)
df_final <- df[df$at_bats>=100, ]


# No need to calculate this average because it is already there as bat_ave
#df$hits = as.numeric(df$hits)
# df_final$batting_avg <- (baseball_data$hits / baseball_data$at_bats)


df_final$home_runs = as.numeric(df_final$home_runs)
df_final$bat_ave = as.numeric(df_final$bat_ave)

# plot(df_final$bat_ave, df_final$home_runs, main="Scatterplot", 
#      xlab="Batting Average ---->", ylab="Home Runs ---->")

# coord_cartesian(xlim = c(0, 700), ylim = c(0, 50)) +

# ?theme_by()

## Q.1- Scattter Plot
ggplot(data = df_final, aes(x = bat_ave, y = home_runs)) +
    geom_point()+
    geom_smooth(method = lm)+
    labs(title = "Scatter Plot", subtitle = "Scatter Plot with linear regression", 
    x = "Batting Average ---->", y = "Home Runs ---->", caption = "for 2002 season")+
    theme_bw()

## Q.2- Yes there is a positive lineaer realtionship
## Q.3- The variablity of number of home runs is greater for those with higher batting averages.

## Q.4- Code for normal probablity plot to verify regression assumptions (RS and assumptions are violated)
model <- lm(home_runs ~ bat_ave, data=df_final)
model ## use it to write the equation of linear regression
summary(model)

model_residuals <- residuals(model) #List of residuals can be used resid(model)
standard_res = rstandard(model)
# install.packages('StMoSim')
library(StMoSim)
 
plot(density(standard_res))  #A density plot
qqnormSim(standard_res)  # A quantile normal plot - good for checking normality
qqline(standard_res)

## Q.5- Residuals vs fitted values curve which show violation of constant varience
plot(predict(model, newdata=df_final), model_residuals)
abline(a=0,b=0,col='blue')

## Q.6- Normality plot after transformation which accepts the normality and have no skewness
df_final$log_homeruns<-log(baseball_data$homeruns)
df_final1 <- subset(df_final, 
                    df_final$log_homeruns!=Inf & df_final$log_homeruns != -Inf & df_final$log_homeruns!=0)

model1 <- lm(log_homeruns ~ bat_ave, data=df_final1)
model1 ## use it to write the equation of linear regression
summary((model1))
model1_residuals <- residuals(model1) #List of residuals can be used resid(model)
standard_res = rstandard(model1)

plot(density(standard_res))  #A density plot
qqnormSim(standard_res)  # A quantile normal plot - good for checking normality
qqline(standard_res)

## Q.7- Plot of residual vs fitted after transformation which shows no violation
plot(predict(model1, newdata=df_final1), model1_residuals) ## fitte function can also be used
abline(a=0,b=0,col='blue')

## Q.12 Estimate the values by training and test data

## Q.13
str(df_final1$log_homeruns)
str(df_final$home_runs)

## Q.15
# ggplot(data = df_final1, aes(x = bat_ave, y = log_homeruns)) +
#     geom_point()+
#     geom_smooth(method = lm)+
#     labs(title = "Scatter Plot", subtitle = "Transformed Scatter Plot with linear regression", 
#          x = "Batting Average ---->", y = "Home Runs ---->", caption = "for 2002 season")+
#     theme_bw()

## Q.16
# confint(model, level = 0.95)
# confint(model1, level = 0.95)

# plot(df_final1$bat_ave, df_final1$log_homeruns)
# abline(model1)

# model <- lm(home_runs ~ 1, offset=bat_ave, data=df_final)
# model
# View(df)
# str(df)
View(df_final)
View(df_final1)
str(df_final)
rm(list = ls())

# plot(residuals(br)~fitted(br))
# baseball_data1 = subset(baseball_data, baseball_data$loghr!=Inf & baseball_data$loghr != -Inf)
# brl<- lm(baseball_data1$batting_avg ~ baseball_data1$loghr, data = baseball_data1)
# baseball_data$loghr<-log(baseball_data$homeruns)
# brl<- lm(baseball_data$batting_avg ~ baseball_data$loghr, data = baseball_data)
# plot(residuals(br),fitted(br))
