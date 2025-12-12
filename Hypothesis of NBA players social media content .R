#loading the dataset
data <- read.csv("nba_2017_twitter_players.csv")

#checking the columns
head(data)

#column names
names(data)

#renaming the columns
names(data)[1] <- "player"
names(data)[2] <- "favorites"
names(data)[3] <- "retweets"

#scatter plot
plot(
  data$favorites,
  data$retweets,
  main = "Favorites vs Retweets",
  xlab = "Favorite Count",
  ylab = "Retweet Count",
  pch = 19,
  col = "blue"
)

#adding a trending line between the datas
model <- lm(retweets ~ favorites, data = data)
abline(model, col = "red")

# Bar plot of favorites for each player (original order)
barplot(
  data$favorites,
  names.arg = data$player,
  las = 2,
  col = "lightblue",
  cex.names = 0.6,         # shrink label size
  main = "Bar Plot of Favorites (Original Data)",
  ylab = "Favorite Count"
)



#histogram of favourites
hist(
  data$favorites,
  main = "Histogram of Favorites",
  xlab = "Favorite Count",
  col = "lightblue",
  border = "black"
)

#histogram of retweets
hist(
  data$retweets,
  main = "Histogram of Retweet Counts",
  xlab = "Retweet Count",
  col = "lightgreen",
  border = "black"
)

#calculating correlation
correlation_value <- cor(data$favorites, data$retweets)
correlation_value
cor_matrix <- cor(data[, c("favorites", "retweets")])
cor_matrix

#making a correlation
data$favorites <- as.numeric(data$favorites)
data$retweets  <- as.numeric(data$retweets)

#calculating the correlation again
correlation_value <- cor(data$favorites, data$retweets)
correlation_value

print(paste("The correlation between favorites and retweets is:", correlation_value))

#as it is showing there is no correlation we have to clean the data 
clean_data <- na.omit(data[, c("favorites", "retweets")])
correlation_value <- cor(clean_data$favorites, clean_data$retweets)
correlation_value
print(paste("The correlation between favorites and retweets is:", correlation_value))



