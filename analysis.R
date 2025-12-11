# Project: B-130 Group Project - NBA 2017 Twitter Analysis
# File: analysis.R
# Description: Analysis of NBA player Twitter engagement metrics
# Dataset: nba_2017_twitter_players.csv
# Authors: [Your Names/Team]
# Date: [Date]

# =============================================================================
# SECTION 1: LOAD AND EXPLORE DATASET
# =============================================================================

# Load the dataset from CSV file into a data frame
data <- read.csv("nba_2017_twitter_players.csv")

# Display first 6 rows to understand data structure
head(data)

# Examine data structure: column names, data types, and sample values
str(data)

# Generate summary statistics for all variables
# Shows min, max, mean, median, and quartiles for numeric columns
summary(data)

# =============================================================================
# SECTION 2: DATA PREPARATION
# =============================================================================

# Extract Twitter engagement metrics for analysis
# x: Number of favorites/likes received
# y: Number of retweets received
x <- data$TWITTER_FAVORITE_COUNT
y <- data$TWITTER_RETWEET_COUNT

# Check for missing values in our variables of interest
cat("Missing values in Favorite Count:", sum(is.na(x)), "\n")
cat("Missing values in Retweet Count:", sum(is.na(y)), "\n")

# Optional: Remove rows with missing values if needed
# complete_cases <- complete.cases(x, y)
# x <- x[complete_cases]
# y <- y[complete_cases]

# =============================================================================
# SECTION 3: VISUALIZATION - SCATTER PLOT
# =============================================================================

# Create scatter plot to explore relationship between favorites and retweets
plot(x, y,
     main = "NBA Players: Twitter Favorites vs Retweets (2017)",
     xlab = "Favorite Count (Likes)",
     ylab = "Retweet Count",
     pch = 19,           # Solid circle points
     col = "blue",       # Point color
     cex = 0.8,          # Point size
     col.main = "darkblue",  # Title color
     font.main = 2)      # Bold title

# Add linear regression line to show trend
abline(lm(y ~ x), 
       col = "red",      # Line color
       lwd = 2)          # Line width

# Add grid for better readability
grid()

# Add correlation coefficient as text on plot
correlation <- cor(x, y, use = "complete.obs")
text(x = max(x, na.rm = TRUE) * 0.7, 
     y = max(y, na.rm = TRUE) * 0.9,
     labels = paste("r =", round(correlation, 3)),
     col = "darkred",
     cex = 1.1)

# =============================================================================
# SECTION 4: VISUALIZATION - HISTOGRAM
# =============================================================================

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

# =============================================================================
# SECTION 5: ADDITIONAL ANALYSIS (Optional Enhancements)
# =============================================================================

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

# Optional: Save plots to files
# png("twitter_analysis_plots.png", width = 1000, height = 500)
# par(mfrow = c(1, 2))
# # Plot 1: Scatter plot
# plot(x, y, main = "Favorites vs Retweets", xlab = "Favorites", ylab = "Retweets")
# # Plot 2: Histogram
# hist(x, main = "Distribution of Favorites", xlab = "Favorite Count")
# dev.off()

# =============================================================================
# SECTION 6: CLEANUP AND SESSION INFO
# =============================================================================

# Remove temporary variables if desired
# rm(x, y, correlation)

# Display session information for reproducibility
cat("\n=== SESSION INFO ===\n")
sessionInfo()

# End of analysis.R
