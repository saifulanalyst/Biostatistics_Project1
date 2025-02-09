lnguser_data <- read.csv("/Users/mamoon/Desktop/Documents/Biostatistics_Project/2024Dec21_Contraceptive_Data/Dec_Contraceptive_Data.csv")
inspect(lnguser_data)
View(lnguser_data)
library(tidyverse)

summary(lnguser_data)

#2 c mean and sd

mean(lnguser_data$v12_ClientAge)
sd(agebirthwgt$v12_ClientAge)
mean(lnguser_data$Mean_BP)
sd(lnguser_data$Mean_BP)
mean(lnguser_data$v51_Weight)
sd(lnguser_data$v51_Weight)

hormon.counts <- table(lnguser_data$Contra_type)
proportions <- prop.table(table(lnguser_data$Contra_type)) *100



library(dplyr)
library(broom)

qqnorm(lnguser_data$v51_Weight)
qqline(lnguser_data$v51_Weight)

qqnorm(lnguser_data$Mean_BP)
qqline(lnguser_data$Mean_BP)


# Scatterplot
plot(lnguser_data$v51_Weight, lnguser_data$Mean_BP, main="Scatterplot")

# Pearson Correlation
cor_result <- cor.test(lnguser_data$v51_Weight, lnguser_data$Mean_BP)

# Print the results
print(cor_result)
# Two Independent Samples t-test

# Calculate group-wise statistics
group_stats <- lnguser_data %>%
  group_by(Contra_type) %>%
  summarise(
    Mean_BP = mean(Mean_BP, na.rm = TRUE),
    SD_BP = sd(Mean_BP, na.rm = TRUE)
  )


# Print the results
print(group_stats)


var.test(lnguser_data$Mean_BP ~ lnguser_data$Contra_type)

t.test(lnguser_data$Mean_BP ~ lnguser_data$Contra_type, var.equal=TRUE)

# h Using lm() function, fit a least-square regression model

lnguser_data$Contra_typeCate <- factor(lnguser_data$Contra_type)
lm(lnguser_data$v51_Weight ~ lnguser_data$Mean_BP + lnguser_data$Contra_typeCate)

regresult <- lm(lnguser_data$Mean_BP ~ lnguser_data$v51_Weight + lnguser_data$Contra_typeCate)
summary(regresult)

model1.anova <- model.matrix(regresult)
anova(update(regresult, .
             ~ model1.anova))
plot(regresult, which=1)


