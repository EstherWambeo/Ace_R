library(readxl)
library(ggplot2)
CPS12 <- read_xlsx("C:/Users/user/Desktop/Datasets/CPS12.xlsx")
str(CPS12)

#1
# Running the linear regression
model <- lm(ahe ~ age, data = CPS12)

# Displaying the summary of the regression model
summary(model)

# Load the necessary library for plotting
# install.packages("ggplot2") # Uncomment this if you don't have ggplot2 installed
library(ggplot2)

# Create a scatter plot with a regression line
ggplot(CPS12, aes(x = age, y = ahe)) +
  geom_point(color = "blue") +        # Scatter plot of the data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add the regression line
  labs(title = "Regression of Average Hourly Earnings on Age",
       x = "Age",
       y = "Average Hourly Earnings") +
  theme_minimal()





birthweight_smoking <- read_excel("C:/Users/user/Desktop/Datasets/birthweight_smoking.xlsx")
str(birthweight_smoking)

# Calculate the overall average birthweight
average_birthweight <- mean(birthweight_smoking$birthweight, na.rm = TRUE)
print(paste("Average Birthweight for All Mothers:", round(average_birthweight, 2), "grams"))

# Calculate the average birthweight for smoking mothers
average_birthweight_smokers <- mean(birthweight_smoking$birthweight[birthweight_smoking$smoker == 1], na.rm = TRUE)
print(paste("Average Birthweight for Smoking Mothers:", round(average_birthweight_smokers, 2), "grams"))

# Calculate the average birthweight for non-smoking mothers
average_birthweight_nonsmokers <- mean(birthweight_smoking$birthweight[birthweight_smoking$smoker == 0], na.rm = TRUE)
print(paste("Average Birthweight for Non-Smoking Mothers:", round(average_birthweight_nonsmokers, 2), "grams"))

library(dplyr)
# Summarize data by smoking status
summary_stats <- birthweight_smoking %>%
  group_by(smoker) %>%
  summarise(
    n = n(),
    mean_birthweight = mean(birthweight, na.rm = TRUE),
    sd_birthweight = sd(birthweight, na.rm = TRUE)
  )

# Display the summary statistics
print(summary_stats)

# Extract values for Non-Smokers (smoker == 0)
non_smokers <- summary_stats %>% filter(smoker == 0)
n1 <- non_smokers$n
mean1 <- non_smokers$mean_birthweight
sd1 <- non_smokers$sd_birthweight

# Extract values for Smokers (smoker == 1)
smokers <- summary_stats %>% filter(smoker == 1)
n2 <- smokers$n
mean2 <- smokers$mean_birthweight
sd2 <- smokers$sd_birthweight

diff_avg_birthweight <- mean1 - mean2
diff_avg_birthweight

# Calculate the standard error
SE_diff <- sqrt((sd1^2 / n1) + (sd2^2 / n2))

# Display the standard error
print(paste("Standard Error of the Difference in Means:", round(SE_diff, 2), "grams"))


# Difference in means
diff_means <- mean1 - mean2

# Degrees of freedom for the t-distribution
# Using the smaller of n1 - 1 and n2 - 1 for a conservative estimate
df <- min(n1 - 1, n2 - 1)

# Critical t-value for 95% confidence
t_critical <- qt(0.975, df)

# Calculate the confidence interval
CI_lower <- diff_means - t_critical * SE_diff
CI_upper <- diff_means + t_critical * SE_diff

# Display the confidence interval
print(paste("95% Confidence Interval for the Difference in Means: [", 
            round(CI_lower, 2), ", ", round(CI_upper, 2), "] grams", sep = ""))



# Run the linear regression
model_bw_smoker <- lm(birthweight ~ smoker, data = birthweight_smoking)

# Display the summary of the regression model
summary(model_bw_smoker)

# Calculate the 95% confidence interval for the slope (smoker)
confint(model_bw_smoker, "smoker", level = 0.95)

