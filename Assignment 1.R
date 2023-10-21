getwd()
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)

# Prints the number of variables(columns) within the dataset
print(length(titanic.train$Survived)) # Doing length of the titanic.train only returns the number of columns

print(sum(titanic.train$Survived == 1 & titanic.train$Pclass == 1 & titanic.train$Sex == "male"))
print(sum(titanic.train$Survived == 1 & titanic.train$Pclass == 1 & titanic.train$Sex == "female"))


# ---- Establishing Base survival rates based on passenger class
base_survival <- function() {
  png(file = "class-survival.png")
  survival_rates_c1 <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 1)
  survival_rates_c2 <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 2)
  survival_rates_c3 <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 3)
  survival_rate_csum <- sum(survival_rates_c1, survival_rates_c2, survival_rates_c3)
  # print(paste(survival_rates_c1,survival_rates_c2,survival_rates_c3))
  library(ggplot2)
  x_values <- c(survival_rates_c1, survival_rates_c2, survival_rates_c3)
  title_values <- c("1st Class", "2nd Class", "3rd Class")
  percentages <- c(sum(survival_rates_c1 / survival_rate_csum) * 100, sum(survival_rates_c2 / survival_rate_csum) * 100, sum(survival_rates_c3 / survival_rate_csum) * 100)
  pie(as.numeric(x_values), labels = percentages, main = "Survival Rate Percentages Based on Passenger Class", col = rainbow(length(x_values)), radius = 0.9)
  legend("topleft", title_values, cex = 0.8, fill = rainbow(length(x_values)))
}
base_survival()
dev.off()

# ---- Establishing survival rates based on class AND gender

gender_class_survival <- function() {
  png(file = "class-gender-survival.png")
  # - female
  survival_rates_c1_f <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 1 & titanic.train$Sex == "female")
  survival_rates_c2_f <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 2 & titanic.train$Sex == "female")
  survival_rates_c3_f <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 3 & titanic.train$Sex == "female")

  #- male
  survival_rates_c1_m <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 1 & titanic.train$Sex == "male")
  survival_rates_c2_m <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 2 & titanic.train$Sex == "male")
  survival_rates_c3_m <- sum(titanic.train$Survived == 1 & titanic.train$Pclass == 3 & titanic.train$Sex == "male")
  survival_rate_gender_csum <- sum(survival_rates_c1_f, survival_rates_c2_f, survival_rates_c3_f, survival_rates_c1_m, survival_rates_c2_m, survival_rates_c3_m)
  library(ggplot2)
  x_values <- c(survival_rates_c1_f, survival_rates_c2_f, survival_rates_c3_f, survival_rates_c1_m, survival_rates_c2_m, survival_rates_c3_m)
  title_values <- c("Women in 1st Class", "Women in 2nd Class", "Women in 3rd Class", "Men in 1st Class", "Men in 2nd Class", "Men in 3rd Class")
  percentages <- c(sum(survival_rates_c1_f / survival_rate_gender_csum) * 100, sum(survival_rates_c2_f / survival_rate_gender_csum) * 100, sum(survival_rates_c3_f / survival_rate_gender_csum) * 100, sum(survival_rates_c1_m / survival_rate_gender_csum) * 100, sum(survival_rates_c2_m / survival_rate_gender_csum) * 100, sum(survival_rates_c3_m / survival_rate_gender_csum) * 100)
  pie(as.numeric(x_values), labels = percentages, main = "Survival Percentages Based on Passenger Class and Gender", col = rainbow(length(x_values)), radius = 0.8)
  legend("left", title_values, cex = 0.8, fill = rainbow(length(x_values)))
}
gender_class_survival()
dev.off()


# ---- Establishing survival rates based on age

# - age ranges
# 0-18
# 19-38
# 39-58
# 59-78
age_survival <- function() {
  png(file = "age-survival.png")
  age_group_1 <- sum(titanic.train$Survived == 1 & titanic.train$Age >= 1 & titanic.train$Age <= 18) / sum(titanic.train$Age >= 1 & titanic.train$Age <= 18)
  age_group_2 <- sum(titanic.train$Survived == 1 & titanic.train$Age >= 19 & titanic.train$Age <= 38) / sum(titanic.train$Age >= 19 & titanic.train$Age <= 38)
  age_group_3 <- sum(titanic.train$Survived == 1 & titanic.train$Age >= 39 & titanic.train$Age <= 58) / sum(titanic.train$Age >= 39 & titanic.train$Age <= 58)
  age_group_4 <- sum(titanic.train$Survived == 1 & titanic.train$Age >= 59 & titanic.train$Age <= 78) / sum(titanic.train$Age >= 59 & titanic.train$Age <= 78)
  age_csum <- sum(titanic.train$Survived == 1)
  library(ggplot2)
  x_values <- c(age_group_1, age_group_2, age_group_3, age_group_4)
  title_values <- c("Ages 1-18", "Ages 19-38", "Ages 39-58", "Ages 59-74")
  percentages <- c(age_group_1 * 100, age_group_2 * 100, age_group_3 * 100, age_group_4 * 100)
  pie(as.numeric(x_values), labels = percentages, main = "Survival Rate Percentages Based on Age", col = rainbow(length(x_values)), radius = .70)
  legend("topleft", title_values, cex = 0.8, fill = rainbow(length(x_values)))
}
age_survival()
dev.off()


embarked_survival <- function() {
  png(file = "embarked-survival.png")
  group_C <- sum(titanic.train$Survived == 1 & titanic.train$Embarked == "C")
  group_Q <- sum(titanic.train$Survived == 1 & titanic.train$Embarked == "Q")
  group_S <- sum(titanic.train$Survived == 1 & titanic.train$Embarked == "S")
  group_csum <- sum(group_C, group_Q, group_S)
  library(ggplot2)
  x_values <- c(group_C, group_Q, group_S)
  title_values <- c("Depature Location C", "Departure Location Q", "Departure Location S")
  percentages <- c(sum(group_C / group_csum) * 100, sum(group_Q / group_csum) * 100, sum(group_S / group_csum) * 100)
  pie(as.numeric(x_values), labels = percentages, main = "Survival Rate Percentages Based on Embarkment Location", col = rainbow(length(x_values)), radius = .70)
  legend("topleft", title_values, cex = 0.8, fill = rainbow(length(x_values)))
}
# print(unique(titanic.train$Embarked))
embarked_survival()
dev.off()


sibling_survival <- function() {
  # print(unique(titanic.train$SibSp))
  png(file = "sibling-survival.png")
  sib_0 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 0)
  sib_1 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 1)
  sib_2 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 2)
  sib_3 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 3)
  sib_4 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 4)
  sib_5 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 5)
  sib_8 <- sum(titanic.train$Survived == 1 & titanic.train$SibSp == 8)
  sibling_csum <- sum(sib_0, sib_1, sib_2, sib_3, sib_4, sib_5, sib_8)
  library(ggplot2)
  library(ggrepel)
  x_values <- c(sib_0, sib_1, sib_2, sib_3, sib_4, sib_5, sib_8)
  title_values <- c("0 Sibling(s)", "1 Sibling(s)", "2 Sibling(s)", "3 Sibling(s)", "4 Sibling(s)", "5 Sibling(s)", "8 Sibling(s)")
  percentages <- c(sum(sib_0 / sibling_csum) * 100, sum(sib_1 / sibling_csum) * 100, sum(sib_2 / sibling_csum) * 100, sum(sib_3 / sibling_csum) * 100, sum(sib_4 / sibling_csum) * 100, sum(sib_5 / sibling_csum) * 100, sum(sib_8 / sibling_csum) * 100)
  pie(as.numeric(x_values), labels = percentages, main = "Survival Rate Percentages Based on Amount of Siblings", col = rainbow(length(x_values)), radius = .70)
  legend("topleft", title_values, cex = 0.8, fill = rainbow(length(x_values)))
}
# print(unique(titanic.train$Embarked))
sibling_survival()
dev.off()

print(max(unique(titanic.train$Age)))
