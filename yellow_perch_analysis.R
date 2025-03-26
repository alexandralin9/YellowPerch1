# Yellow Perch Length-Weight Analysis
library(readxl)
library(ggplot2)

perch_data <- read_excel("YellowPerchLW.xlsx")

#unit change
perch_data$length <- as.numeric(perch_data$length)
perch_data$weight <- as.numeric(perch_data$weight)

#change to log?
perch_data$log_length <- log(perch_data$length)
perch_data$log_weight <- log(perch_data$weight)

#linear fit
model <- lm(log_weight ~ log_length, data = perch_data)
summary(model)

a <- exp(coef(model)[1])
b <- coef(model)[2]
cat("Power function: Weight =", round(a, 7), "× Length^", round(b, 4), "\n")

#scatter plot with regression
ggplot(perch_data, aes(x = log_length, y = log_weight)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  theme_minimal() +
  labs(title = "Log-Log Plot of Length vs Weight", 
       x = "Log(Length)", 
       y = "Log(Weight)")

#makes the ggplot look nices
ggplot(perch_data, aes(x = length, y = weight)) + 
  geom_point() + 
  stat_function(fun = function(x) a * x^b, color = "blue") +
  theme_minimal() +
  labs(title = "Length-Weight Relationship for Yellow Perch",
       x = "Length", 
       y = "Weight")
