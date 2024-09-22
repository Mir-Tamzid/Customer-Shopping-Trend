#Group members:
# GRACE LING KIAN HWAI MCS231026
# MIR TAMZID HASAN     A20EC4037
# YAP QI YUAN          MCS231025

# clean up the environment
rm(list = ls())


#2 Dataset
#2.1 Dataset Description (in report)

#2.2 Data Preprocessing 
#2.2.1 Read Dataset
shopping_data <-read.csv("C:/Users/grace/OneDrive/Desktop/20232024 Sem 2/MCSD1113 STATISTIC/Project/shopping_trends_updated.csv")
head(shopping_data)

#2.2.2 Check null and duplicate value
# Checking for missing values
is.na(shopping_data)  # Logical matrix of NA positions
sum(is.na(shopping_data))  # Total count of NA values
colSums(is.na(shopping_data))  # Count of NA values per column

# Checking for duplicate entries
duplicated(shopping_data)  # Logical vector of duplicated rows
sum(duplicated(shopping_data))  # Total count of duplicate rows

# Checking properties of the data
colnames(shopping_data)
str(shopping_data)


#3 Data Analysis
#3.1 Descriptive Analysis
#3.1.1 Bar Chart
#install.packages("ggplot2") 
library(ggplot2)

ggplot(shopping_data, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Purchases by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3.1.2 Pie Chart
attach(shopping_data)
gender.count<-table(Gender)
piepercent<- round(100*(gender.count/sum(gender.count)), 1)
pie(table(shopping_data$Gender),main='Gender Percentage (%)', x=gender.count, labels=paste(piepercent, "%", sep=" "), col = c('blue','red'))
legend("topright", c('Male','Female'), cex = 0.6, fill = c('blue','red'))

#3.1.3 Stem & Leaf
stem(Review.Rating, scale=2)

#3.1.4 Histogram
hist_PA <- hist(Purchase.Amount..USD., breaks = 10, main="Histogram of Purchase Amount (USD)", xlab="Purchase Amount (USD)", ylab="Frequency", ylim = c(0, max(Purchase.Amount..USD.) + 500))
text(hist_PA$mids, hist_PA$counts, labels=hist_PA$counts, pos=3, cex=0.8, col="black")

#3.1.5 Boxplot
boxplot(Age~Color, main='Boxplot of Age by Colour Selection', xlab='', ylab='Age(in Years)', names=c('Gray','Maroon','Turquoise','White','Charcoal','Silver','Pink','Purple','Olive','Gold','Violet','Teal','Lavender','Black','Green','Peach','Red','Cyan','Brown','Beige','Orange','Indigo','Yellow','Magenta','Blue'),col = c('gray','maroon','turquoise','white','#36454F','#C0C0C0','pink','purple','#808000','gold','violet','#008080','lavender','black','green','#FFE5B4','red','cyan','brown','beige','orange','#4B0082','yellow','magenta','blue', notch=T), names.arg = names(Color), las = 2, cex.names = 0.4)
boxplot(Age~Size, main='Boxplot of Age by Size Selection', ylab='Age(in Years)',xlab='Customer Preference Size', names=c('S','M','L','XL'),col = c('red','blue','green','orange'), notch=T)

#3.1.6 Descriptive Analysis (mean, median, mode, etc.)
summary(Age)
summary(Review.Rating)
summary(Purchase.Amount..USD.)


#3.2 Inferential Analysis
#3.2.1 Hypothesis Testing 1-sample or 2-sample
# one sample t test 
attach(shopping_data)
head(shopping_data)

# one sample t test with H1 is less than 44.07
t.test(Age, mu=44.07, alternative="less", conf.level = 0.95)

# two sample t test
# Filter only standard and express shipping types for comparison
shipping_filtered <- shopping_data %>% filter(Shipping.Type %in% c("Standard", "Express"))

# Two-sample T-test: Comparing the means of Purchase Amount (USD) by Shipping Type
t_test_results <- t.test(Purchase.Amount..USD. ~ Shipping.Type, data = shipping_filtered)
print(t_test_results)

#3.2.2 Goodness of fit test: Chi-Square Test on Categories
# Observed frequencies
observed_frequencies <- table(shopping_data$Category)

# Expected frequencies (assuming equal distribution for simplicity)
expected_frequencies <- rep(sum(observed_frequencies) / length(observed_frequencies), length(observed_frequencies))

# Perform Chi-Square Goodness-of-Fit Test
chi_square_test <- chisq.test(observed_frequencies, p = expected_frequencies / sum(expected_frequencies))
print(chi_square_test)

#3.2.3 Chi Square Test of independence
#install.packages("tidyverse")
library(tidyverse)

# Create a contingency table for Payment_Method and Discount_Applied
contingency_table <- table(shopping_data$Payment.Method, shopping_data$Discount.Applied)

# Perform Chi-Square Test of Independence
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

#3.2.4 Correlation
#install.packages("tidyverse")
library(tidyverse)

data_for_cor <- shopping_data %>%
  select(Age, Purchase.Amount..USD., Review.Rating, Previous.Purchases)
head(data_for_cor,5)
round(cor(data_for_cor),
      digits = 2 # rounded to 2 decimals
)

#install.packages("corrplot")
library(corrplot)

correlation_matrix <- cor(data_for_cor)
print(round(correlation_matrix,4))

dev.off()
plot.new()

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation_matrix, method = "color", type = "upper", 
         col = col(200), tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix", addCoef.col = "black", number.cex = 0.7, is.corr = TRUE, tl.cex = 0.8)


#3.2.5 Regression
#install.packages("dplyr")
#install.packages("ggpubr")
library(ggplot2)
library(dplyr)

ggplot(shopping_data, aes(x = Age, y = Previous.Purchases)) +
  geom_point() +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line
  labs(title = "Age vs. Previous Purchases",
       x = "Age",
       y = "Previous Purchases") +
  theme_minimal()

lm_model <- lm(Previous.Purchases ~ Age, shopping_data = shopping_data)
summary(lm_model)

#3.2.6 ANOVA test
anova_result <- anova(lm_model)
anova_result

