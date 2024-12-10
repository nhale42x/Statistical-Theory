# Load necessary libraries
library(readr)
library(car)

# Load the data
file_path <- "C:/Users/Cipher/Desktop/StatsTheory_JAYBEZ/Alzheimers-Mice-Data.csv"
df <- read_csv(file_path)

str(df)
colnames(df) <- make.names(colnames(df))
colnames(df)

# Display the first few rows of the dataset
head(df)

#1. ================================================================================
# Perform 2-Factor ANOVA on 'Memory' as the dependent variable
anova_model <- aov(Memory ~ as.factor(AD_Status) * as.factor(Treatment), data = df)
summary(anova_model)

# Perform Type II ANOVA for detailed interaction effects
Anova(anova_model, type = "II")

#2. ================================================================================

# Check the assumptions underlying the said ANOVA type.

# Normality

# Check residuals for normality
residuals_model <- residuals(anova_model)
residuals_model

# Histogram
hist(residuals_model, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot
qqnorm(residuals_model)
qqline(residuals_model, col = "red")

# Shapiro-Wilk test
shapiro.test(residuals_model)


# Homogeneity of variances

leveneTest(Memory ~ as.factor(AD_Status) * as.factor(Treatment), data = df)


