setwd("/Users/Brian/Desktop/ds-project/Heart-Failure-Prediction")
library(readr)
heart_failure <- read_csv("heart_failure_clinical_records_dataset.csv")
na_counts <- colSums(is.na(heart_failure))
print(na_counts)

View(heart_failure)
correlation_matrix <- cor(heart_failure)
print(correlation_matrix)
installed.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "color", type = "full", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.5, tl.cex = 0.8, 
         outline = TRUE)
# So as we can see from the matrix age, ejection_fraction, serum_creatine, and 
#time are all the highest correlated with death_event so its what we'll build our
#model off of
library(ggplot2)
summary(heart_failure$age)
heart_failure$DEATH_EVENT <- as.factor(heart_failure$DEATH_EVENT)
ggplot(heart_failure, aes(x = DEATH_EVENT, y = age)) +
  geom_boxplot() +
  labs(title = "Boxplots of Age for Different Death Events",
       x = "Death Event",
       y = "Age") +
  theme_minimal()

summary(heart_failure$ejection_fraction)
heart_failure |> 
  ggplot(aes(x = ejection_fraction)) +
  geom_bar()
heart_failure |> 
  ggplot(aes(x = ejection_fraction)) +
  geom_boxplot()
heart_failure |>
  ggplot(aes(x = ejection_fraction)) +
  geom_boxplot(aes(fill = DEATH_EVENT), position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Box plot of Ejection Fraction and Death Event", x = "Ejection Fraction", y = "Count")
heart_failure |>
  ggplot(aes(x = DEATH_EVENT, y = ejection_fraction, fill = DEATH_EVENT)) +
  geom_violin() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Violin plot of Ejection Fraction and Death Event", x = "Death Event", y = "Ejection Fraction")
# we do have outliers but our outliers have meaning so itll be good to keep them
# More alive people have the normal ejection fraction while more dead people have
#the abnormal values
summary(heart_failure$serum_creatinine)
heart_failure |> 
  ggplot(aes(x = serum_creatinine)) +
  geom_bar()
heart_failure |> 
  ggplot(aes(x = serum_creatinine)) +
  geom_boxplot()
heart_failure |>
  ggplot(aes(x = serum_creatinine)) +
  geom_boxplot(aes(fill = DEATH_EVENT), position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Box plot of Serum Creatinine and Death Event", x = "Serum Creatinine", y = "Count")
heart_failure |>
  ggplot(aes(x = DEATH_EVENT, y = serum_creatinine, fill = DEATH_EVENT)) +
  geom_violin() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Violin plot of Serum Creatinine and Death Event", x = "Death Event", y = "Serum Creatinine")
#more alive people are around 1 mg/dL and more dead people are 2-3 mg/DL
heart_failure$DEATH_EVENT <- factor(heart_failure$DEATH_EVENT, labels = c("Alive", "Dead"))
heart_failure |>
  ggplot(aes(x =time )) +
  geom_bar(aes(fill = DEATH_EVENT), position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) 
#more days of no checkup seems to mean that the patient is okay and therefore 
#doesnt need to be checked up right away 
heart_failure |> 
  ggplot(aes(x = time)) +
  geom_boxplot()
heart_failure |>
  ggplot(aes(x = time)) +
  geom_boxplot(aes(fill = DEATH_EVENT), position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Box plot of Time and Death Event", x = "Time", y = "Count")
heart_failure |>
  ggplot(aes(x = DEATH_EVENT, y = time, fill = DEATH_EVENT)) +
  geom_violin() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Violin plot of Time and Death Event", x = "Death Event", y = "Time")
