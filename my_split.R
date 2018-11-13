library(stringr)
library(ggplot2)

# Read string data and split into data frame
df = readLines("Baseball.txt")
df = as.data.frame(do.call(rbind, strsplit(df, split=" {2,10}")), stringsAsFactors=FALSE)
df = df[-1, ]
names(df) = c("firstname", "lastname", "age", "team", "games", "at_bats","runs", "hits", "doubles", "triples", "home_runs", "RBIs","walks", "strikeouts","bat_ave", "on_base_pct", "slugging_pct", "stolen_bases", "caught_stealing")

# View(df)
str(df)

df$at_bats = as.numeric(df$at_bats)
# df$bat_ave = as.numeric(df$bat_ave)
df_final <- df[df$at_bats>=100, ]

plot (df_final$bat_ave, df_final$home_runs, xlab = "batting average", 
      ylab = "home runs")

# df_model <- data.frame(df_final$home_runs,df_final$bat_ave)
# names(df_model) = c("home_runs", "bat_ave")
# str(df_model)

# df_model$home_runs = as.numeric(df_model$home_runs)
# df_model$bat_ave = as.numeric(df_model$bat_ave)

# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(df_final$home_runs, SplitRatio = 4/5)
training_set = subset(df_final, split == TRUE)
test_set = subset(df_final, split == FALSE)

training_set
str(training_set)



model <- lm(training_set$home_runs ~ training_set$bat_ave, data=training_set)
summary(model)
abline(model)

# ggplot() +
#     geom_point(aes(x = df_model$df_final.bat_ave, y = df_model$df_final.home_runs), color = 'red')+
#     geom_line(aes(x = df_model$df_final.bat_ave, y = predict(model, newdata = df_model)),
#               colour = 'blue')+
#     labs(title = "Scatter Plot", subtitle = "Scatter Plot with linear regression", 
#          x = "Batting Average ---->", y = "Home Runs ---->", caption = "for 2002 season")+
#     theme_bw()





