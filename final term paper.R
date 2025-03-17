install.packages(c("openxlsx", "zoo", "car", "readxl"))
library(readxl)
library(zoo)
library(openxlsx)
library(stats)
library(car)

data <- read.xlsx("Downloads/Term Paper.xlsx", na.strings = c("NA", "", " "))
data$Countries <- na.locf(data$Countries)

variables <- c("Life.Satisfaction", "GDP.per.capita", "Life.Expectancy", "Crime.Index")
for (var in variables) {
  sd_value <- sd(data[[var]])
  cat(paste0(var, " Standard Deviation: ", sd_value, "\n"))
}

model <- lm(Life.Satisfaction ~ `GDP.per.capita(.in.USD)` + `Life.Expectancy(.in.Years)` + Crime.Index, data = data)
summary(model)

correlation <- cor(data$`GDP.per.capita(.in.USD)`, data$`Life.Expectancy(.in.Years)`, use = "complete.obs")
cat("Correlation between GDP per Capita and Life Expectancy:", correlation, "\n")
correlation <- cor(data$`GDP.per.capita(.in.USD)`, data$Crime.Index, use = "complete.obs")
cat("Correlation between GDP per Capita and Crime Index:", correlation, "\n")
correlation <- cor(data$`Life.Expectancy(.in.Years)`, data$Crime.Index, use = "complete.obs")
cat("Correlation between Life.Expectancy and Crime Index:", correlation, "\n")

vif_values <- vif(model)
for (var in names(vif_values)) {
  cat("VIF for", var, ":", vif_values[var], "\n")
}

if (any(vif_values > 5)) {
  cat("Warning: Some variables have high VIF values indicating multicollinearity.\n")
} else {
  cat("No significant multicollinearity detected among all variables.\n")
}


plot(data$`GDP.per.capita(.in.USD)`, data$Crime.Index,
     xlab = "GDP per Capita (USD)",
     ylab = "Crime Index",
     main = "GDP per Capita vs Crime Index",
     pch = 19, col = "blue")


model1 <- lm(data$Crime.Index ~ data$`GDP.per.capita(.in.USD)`)
abline(model1, col = "red")


plot(data$`Life.Expectancy(.in.Years)`, data$Crime.Index,
     xlab = "Life Expectancy (Years)",
     ylab = "Crime Index",
     main = "Life Expectancy vs Crime Index",
     pch = 19, col = "green")


model2 <- lm(data$Crime.Index ~ data$`Life.Expectancy(.in.Years)`)
abline(model2, col = "red")


plot(data$`GDP.per.capita(.in.USD)`, data$`Life.Expectancy(.in.Years)`,
     xlab = "GDP per Capita (USD)",
     ylab = "Life Expectancy (Years)",
     main = "GDP per Capita vs Life Expectancy",
     pch = 19, col = "purple")


model3 <- lm(data$`Life.Expectancy(.in.Years)` ~ data$`GDP.per.capita(.in.USD)`)
abline(model3, col = "red")


restricted_model <- lm(Life.Satisfaction ~ Crime.Index, data = data)
unrestricted_model <- lm(Life.Satisfaction ~ `GDP.per.capita(.in.USD)` + `Life.Expectancy(.in.Years)` + Crime.Index, data = data)
f_test <- anova(restricted_model, unrestricted_model)
print(f_test)


n <- nrow(data)
k_unrestricted <- length(coef(unrestricted_model))
k_restricted <- length(coef(restricted_model))
q <- k_unrestricted - k_restricted
RSS_restricted <- sum(restricted_model$residuals^2)
RSS_unrestricted <- sum(unrestricted_model$residuals^2)
F_statistic <- ((RSS_restricted - RSS_unrestricted) / q) / (RSS_unrestricted / (n - k_unrestricted))
cat("F-statistic:", F_statistic, "\n")


critical_F <- qf(0.95, df1 = q, df2 = n - k_unrestricted)
if (F_statistic > critical_F) {
  cat("Reject the null hypothesis. GDP per capita and Life Expectancy are jointly significant.\n")
} else {
  cat("Fail to reject the null hypothesis. GDP per capita and Life Expectancy are not jointly significant.\n")
}
