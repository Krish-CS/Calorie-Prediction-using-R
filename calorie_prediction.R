# calories_project_with_plot_save.R

library(ggplot2)
library(randomForest)
library(caret)
library(dplyr)
library(corrplot)
library(showtext)

# -------------------- Global Font & Theme --------------------
showtext_auto()
font_add_google("Poppins")

# -------------------- Data Loading & Preprocessing --------------------
calories <- read.csv("calories.csv")
calories <- na.omit(calories)
calories$Gender <- as.factor(calories$Gender)
calories$BMI <- calories$Weight / ((calories$Height / 100)^2)
num_cols <- sapply(calories, is.numeric)
calories_num <- calories[, num_cols]

# -------------------- Train-Test Split --------------------
set.seed(123)
idx <- createDataPartition(calories$Calories, p = 0.8, list = FALSE)
train <- calories[idx, ]
test <- calories[-idx, ]

# -------------------- Models --------------------
cat("Training models, please wait...\n")
lm_model <- train(
  Calories ~ Gender + Age + Height + Weight + Duration + Heart_Rate + Body_Temp + BMI,
  data = train, method = "lm"
)
rf_model <- randomForest(
  Calories ~ Gender + Age + Height + Weight + Duration + Heart_Rate + Body_Temp + BMI,
  data = train, ntree = 80, importance = TRUE
)

lm_pred <- predict(lm_model, test)
rf_pred <- predict(rf_model, test)
lm_rmse <- RMSE(lm_pred, test$Calories)
rf_rmse <- RMSE(rf_pred, test$Calories)
best_model <- ifelse(lm_rmse < rf_rmse, "Linear Regression", "Random Forest")
cat("Models trained. Now, please enter your details:\n\n")

# -------------------- User Inputs --------------------
gender_prompt <- paste0("Gender (", paste(levels(calories$Gender), collapse = "/"), "): ")
gender <- readline(gender_prompt)

age <- as.numeric(readline("Age (10-90): "))
height <- as.numeric(readline("Height in cm (100-220): "))
weight <- as.numeric(readline("Weight in kg (30-150): "))
duration <- as.numeric(readline("Duration (min) (1-300): "))
heart_rate <- as.numeric(readline("Heart Rate (40-200): "))
body_temp <- as.numeric(readline("Body Temperature in °C (35-42): "))

bmi <- weight / ((height / 100)^2)
new_data <- data.frame(
  Gender = factor(gender, levels = levels(train$Gender)),
  Age = age,
  Height = height,
  Weight = weight,
  Duration = duration,
  Heart_Rate = heart_rate,
  Body_Temp = body_temp,
  BMI = bmi
)

lm_p <- predict(lm_model, new_data)
rf_p <- predict(rf_model, new_data)
pred <- if (best_model == "Linear Regression") lm_p else rf_p

cat("\nBest Model:", best_model, "\n")
cat("Predicted Calories Burnt:", round(pred, 2), "kcal\n\n")

# Create plots directory if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# -------------------- Visualization & Saving --------------------

# Correlation Heatmap
png("plots/corr_heatmap.png", width = 800, height = 600)
corrplot(cor(calories_num), method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, addCoef.col = "black", title = "Feature Correlation", mar = c(0, 0, 2, 0))
dev.off()
cat("Saved Correlation Heatmap to plots/corr_heatmap.png\n")
readline("Press [Enter] to display Correlation Heatmap...")
corrplot(cor(calories_num), method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, addCoef.col = "black", title = "Feature Correlation", mar = c(0, 0, 2, 0))

# Feature Importance
imp <- as.data.frame(importance(rf_model))
imp$Feature <- rownames(imp)
p_imp <- ggplot(imp, aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity, fill = IncNodePurity)) +
  geom_col() + coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "Feature", y = "Importance") +
  scale_fill_gradient(low = "lightblue", high = "darkred")
ggsave("plots/feature_importance.png", p_imp, width = 8, height = 6)
cat("Saved Feature Importance plot to plots/feature_importance.png\n")
readline("Press [Enter] to display Feature Importance plot...")
print(p_imp)

# Model Comparison
perf <- data.frame(Model = c("Linear Regression", "Random Forest"), RMSE = c(lm_rmse, rf_rmse))
p_perf <- ggplot(perf, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Performance (Lower is Better)") +
  scale_fill_manual(values = c("#00BFFF", "#32CD32"))
ggsave("plots/model_comparison.png", p_perf, width = 8, height = 6)
cat("Saved Model Comparison plot to plots/model_comparison.png\n")
readline("Press [Enter] to display Model Comparison plot...")
print(p_perf)

# Residual Distribution
res <- data.frame(
  Residuals_LM = test$Calories - lm_pred,
  Residuals_RF = test$Calories - rf_pred
)
p_resid <- ggplot(res) +
  geom_histogram(aes(x = Residuals_LM, fill = "Linear Regression"), alpha = 0.6, bins = 20) +
  geom_histogram(aes(x = Residuals_RF, fill = "Random Forest"), alpha = 0.4, bins = 20) +
  labs(title = "Residuals Distribution") +
  scale_fill_manual(values = c("Linear Regression" = "#FF5733", "Random Forest" = "#33C3FF"))
ggsave("plots/residual_distribution.png", p_resid, width = 8, height = 6)
cat("Saved Residual Distribution plot to plots/residual_distribution.png\n")
readline("Press [Enter] to display Residual Distribution plot...")
print(p_resid)

# Prediction vs Actual
df <- data.frame(
  Actual = test$Calories,
  Linear = lm_pred,
  RandomForest = rf_pred
)
p_pred_act <- ggplot(df) +
  geom_point(aes(x = Actual, y = Linear, color = "Linear Regression"), alpha = 0.6) +
  geom_point(aes(x = Actual, y = RandomForest, color = "Random Forest"), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Prediction vs Actual", x = "Actual", y = "Predicted") +
  scale_color_manual(values = c("Linear Regression" = "#FF5733", "Random Forest" = "#33C3FF"))
ggsave("plots/prediction_vs_actual.png", p_pred_act, width = 8, height = 6)
cat("Saved Prediction vs Actual plot to plots/prediction_vs_actual.png\n")
readline("Press [Enter] to display Prediction vs Actual plot...")
print(p_pred_act)

cat("\n✅ All visualizations displayed and saved in 'plots/' folder.\n")
