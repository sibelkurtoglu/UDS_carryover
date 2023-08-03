
# calculate the confidence interval of the correlation coefficient
# Load the cars dataset (if not already loaded)
data("cars")

# Calculate the correlation coefficient
correlation <- cor(cars$dist, cars$speed)
cor_2 <- cor.test(cars$dist, cars$speed, method = "pearson")
cor_2$conf.int

# Sample size
n <- length(cars$dist)

# Confidence level (e.g., 95%)
confidence_level <- 0.95

# Degrees of freedom for t-distribution
df <- n - 2

# Calculate the critical value for t-distribution (two-tailed test)
critical_value <- qt(1 - (1 - confidence_level) / 2, df)

# Calculate the standard error of the correlation coefficient
standard_error <- 1 / sqrt(df)

# Calculate the margin of error
margin_of_error <- critical_value * standard_error

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- correlation - margin_of_error
upper_bound <- correlation + margin_of_error

# Print the results
cat("Correlation coefficient:", correlation, "\n")
cat("Confidence interval:", lower_bound, "-", upper_bound, "\n")

?cor.test
