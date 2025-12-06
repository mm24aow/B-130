# Project Title: B-130 Group Project
# File Name: analysis.R
# Dataset:
# nba_2017_twitter_players.csv
# Sections:
# 1. Load dataset
data <- read.csv("nba_2017_twitter_players.csv")
head(data)
str(data)
summary(data)

x <- data$TWITTER_FAVORITE_COUNT
y <- data$TWITTER_RETWEET_COUNT

# Scatter plot
plot(x, y,
     main = "Twitter Favorites vs Retweets",
     xlab = "Favorite Count",
     ylab = "Retweet Count",
     pch = 19, col = "blue")

abline(lm(y ~ x), col = "red", lwd = 2)

hist(x,
     main = "Distribution of Twitter Favorite Counts",
     xlab = "Favorite Count")
