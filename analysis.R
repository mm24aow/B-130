# Project: B-130 Group Project - NBA 2017 Twitter Analysis
# File: analysis.R
# Description: Analysis of NBA player Twitter engagement metrics
# Dataset: nba_2017_twitter_players.csv
# Authors:#Giridhar Reddy Bachireddy
          #John Wesly Ponakalla
          #Manohar Meka
          #Muhammad Umer
          #Shanjida Akter
# Load the dataset from CSV file into a data frame
data <- read.csv("/Users/saim/Desktop/B-130/B-130/nba_2017_twitter_players.csv")

# Displaying the first 6 rows to get a sense of the data structure.
head(data)

# Examine the dataset architecture, noting column names, data types, and representative sample values.
str(data)

# Generate descriptive statistics—including minimum, maximum, mean, median, and quartiles—for all numeric variables.
summary(data)

# Extract specific Twitter engagement metrics to facilitate downstream analysis.
x <- data$TWITTER_FAVORITE_COUNT
y <- data$TWITTER_RETWEET_COUNT

# Check for missing values in our variables of interest
cat("Missing values in Favorite Count:", sum(is.na(x)), "\n")
cat("Missing values in Retweet Count:", sum(is.na(y)), "\n")

complete_cases <- complete.cases(x, y)
x <- x[complete_cases]
y <- y[complete_cases]

# Create scatter plot to explore relationship between favorites and retweets
plot(x, y,
     main = "NBA Players: Twitter Favorites vs Retweets (2017)",
     xlab = "Favorite Count (Likes)",
     ylab = "Retweet Count",
     pch = 19,
     col = rgb(0,0,1,0.6),
     cex = 0.8,
     col.main = "darkblue",
     font.main = 2)
# Add linear regression line to show trend
abline(lm(y ~ x), 
       col = "red",
       lwd = 2)
# Add grid for better readability
grid()

# Add correlation coefficient as text on plot
correlation <- cor(x, y, use = "complete.obs")
text(x = max(x, na.rm = TRUE) * 0.7, 
     y = max(y, na.rm = TRUE) * 0.9,
     labels = paste("r =", round(correlation, 3)),
     col = "darkred",
     cex = 1.1)

# Create histogram to show distribution of Twitter favorite counts
hist(x,
     main = "Distribution of Twitter Favorite Counts for NBA Players",
     xlab = "Favorite Count (Likes)",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue",
     breaks = 20,        # Number of bins
     col.main = "darkblue",
     font.main = 2)

# Add vertical line for mean
abline(v = mean(x, na.rm = TRUE), 
       col = "red", 
       lwd = 2, 
       lty = 2)

# Add vertical line for median
abline(v = median(x, na.rm = TRUE), 
       col = "darkgreen", 
       lwd = 2, 
       lty = 2)

# Add legend
legend("topright", 
       legend = c(paste("Mean =", round(mean(x, na.rm = TRUE))),
                  paste("Median =", round(median(x, na.rm = TRUE)))),
       col = c("red", "darkgreen"), 
       lty = 2, 
       lwd = 2,
       cex = 0.8)

# Calculate basic statistics
cat("\n=== TWITTER ENGAGEMENT STATISTICS ===\n")
cat("Favorite Count - Mean:", mean(x, na.rm = TRUE), "\n")
cat("Favorite Count - Median:", median(x, na.rm = TRUE), "\n")
cat("Favorite Count - SD:", sd(x, na.rm = TRUE), "\n")
cat("Retweet Count - Mean:", mean(y, na.rm = TRUE), "\n")
cat("Retweet Count - Median:", median(y, na.rm = TRUE), "\n")
cat("Correlation (Favorites vs Retweets):", correlation, "\n")

# Optional: Boxplot to identify outliers
par(mfrow = c(1, 2))  # Set up 1x2 plotting area
boxplot(x, main = "Favorite Count", col = "lightblue", ylab = "Count")
boxplot(y, main = "Retweet Count", col = "lightgreen", ylab = "Count")
par(mfrow = c(1, 1))  # Reset to single plot

png("twitter_analysis_plots.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
# Plot 1: Scatter plot
plot(x, y, main = "Favorites vs Retweets", xlab = "Favorites", ylab = "Retweets")
# Plot 2: Histogram
hist(x, main = "Distribution of Favorites", xlab = "Favorite Count")
# Pearson correlation test
cor_test <- cor.test(x, y, method = "pearson")

# Print the results
print(cor_test)
cat("\nCorrelation coefficient (r):", cor_test$estimate, "\n")
cat("p-value:", cor_test$p.value, "\n")

dev.off()
