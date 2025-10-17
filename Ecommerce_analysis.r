# ECOMMERCE DATA ANALYSIS

# Intsalling all required packages 
install.packages(c("tidyverse", "readxl", "lubridate", "GGally", 
                   "corrplot", "caret", "cluster", "factoextra", "ggpubr"))

library(tidyverse)
library(readxl)
library(lubridate)
library(GGally)
library(corrplot)
library(caret)
library(cluster)
library(factoextra)
library(ggpubr)

# load ecommerce dataset 
data <- read_excel("Ecommerce data (1).xlsx", sheet = "Data")

# View data structure or data exploration
glimpse(data)
summary(data)

# cleaning the data 
data_clean <- data %>%
  drop_na(AvgPurchaseValue, TotalPurchases, Income, TimeOnSite) %>%
  mutate(
    Gender = as.factor(Gender),
    ChurnStatus = as.factor(ChurnStatus),
    LoyaltyProgramMember = as.factor(LoyaltyProgramMember),
    ReferralSource = as.factor(ReferralSource),
    DeviceUsed = as.factor(DeviceUsed)
  )
data_clean

# CHURN ANALYSIS - What drives customer churn?
churn_model <- glm(ChurnStatus ~ AvgPurchaseValue + TotalPurchases + 
                     TimeOnSite + DiscountUsage + DeviceUsed + 
                     LoyaltyProgramMember + Income + Age,
                   data = data_clean, family = "binomial")

summary(churn_model)

# Visualize churn rate by category and device
data_clean %>%
  group_by(DeviceUsed, ChurnStatus) %>%
  summarise(Count = n()) %>%
  ggplot(aes(DeviceUsed, Count, fill = ChurnStatus)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Churn Rate by Device Type", y = "Proportion")

# CUSTOMER SEGMENTATION - Behavior-based clustering
# Select numeric columns for clustering
cluster_data <- data_clean %>%
  select(TotalPurchases, AvgPurchaseValue, DiscountUsage, TimeOnSite, Income) %>%
  scale()

# Determine optimal number of clusters (elbow method)
fviz_nbclust(cluster_data, kmeans, method = "wss")

# Apply K-means with 4 clusters (example)
set.seed(123)
kmeans_model <- kmeans(cluster_data, centers = 4, nstart = 25)
data_clean$Cluster <- as.factor(kmeans_model$cluster)

# Visualize clusters
fviz_cluster(kmeans_model, data = cluster_data,
             geom = "point", ellipse.type = "norm",
             main = "Customer Segmentation")

# REVENUE DRIVERS - What factors influence spending?
# Correlation plot for numeric features
num_vars <- data_clean %>%
  select(AvgPurchaseValue, TotalPurchases, Income, Age, TimeOnSite, DiscountUsage)

corrplot(cor(num_vars), method = "circle")

# Regression model for AvgPurchaseValue
revenue_model <- lm(AvgPurchaseValue ~ Income + Age + TimeOnSite + LoyaltyProgramMember + DiscountUsage, 
                    data = data_clean)
summary(revenue_model)

# REFERRAL CHANNEL PERFORMANCE
referral_summary <- data_clean %>%
  group_by(ReferralSource) %>%
  summarise(
    AvgPurchase = mean(AvgPurchaseValue),
    Retention = mean(ChurnStatus == "Active"),
    TotalCustomers = n()
  ) %>%
  arrange(desc(AvgPurchase))

print(referral_summary)

ggplot(referral_summary, aes(ReferralSource, AvgPurchase, fill = ReferralSource)) +
  geom_col() +
  labs(title = "Average Purchase Value by Referral Source", y = "Avg Purchase Value")

#LOYALTY PROGRAM IMPACT
# Compare churn & engagement metrics
loyalty_summary <- data_clean %>%
  group_by(LoyaltyProgramMember) %>%
  summarise(
    AvgPurchaseValue = mean(AvgPurchaseValue),
    AvgTimeOnSite = mean(TimeOnSite),
    ChurnRate = mean(ChurnStatus == "Churned")
  )

print(loyalty_summary)

ggplot(loyalty_summary, aes(LoyaltyProgramMember, AvgPurchaseValue, fill = LoyaltyProgramMember)) +
  geom_bar(stat = "identity") +
  labs(title = "Loyalty Program Impact on Spending")

# T-test for difference in AvgPurchaseValue
t.test(AvgPurchaseValue ~ LoyaltyProgramMember, data = data_clean)




