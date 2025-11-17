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

# ======================================================================
#      USER INSIGHT VISUALIZATION SECTION — MATCHING GRADIENT STYLE
# ======================================================================

cat("\n-------------------------------------------------------------\n")
cat(" 🌈  Creating Gradient-Based User Insight Visualizations...\n")
cat("-------------------------------------------------------------\n\n")

library(scales)
library(gridExtra)
library(grid)   # <-- REQUIRED for textGrob()


# Same gradient as scatter
grad_cols <- c("#ffdee9", "#b5f4fa", "#9bdcff", "#6ecbff", "#4aa3ff", "#1f7cff")


# ----------------------------------------------------------------------
# 1) CLEAN ELEGANT USER CARD (dual-tone gradient, not messy)
# ----------------------------------------------------------------------

user_card <- ggplot() +
  
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
           fill = alpha("#b5f4fa", 0.65), color = NA) +
  
  annotate("rect", xmin = 0.03, xmax = 0.97, ymin = 0.03, ymax = 0.97,
           fill = alpha("#e7f7ff", 0.85), color = NA) +
  
  annotate("text", x = 0.5, y = 0.89,
           label = "USER PROFILE SUMMARY",
           size = 7, fontface = "bold", color = "#004aad",
           hjust = 0.5) +
  
  # left column
  annotate("text", x = 0.10, y = 0.78, label = paste("Gender:", gender),
           hjust = 0, size = 4.8, color = "#003f5c") +
  annotate("text", x = 0.10, y = 0.70, label = paste("Age:", age),
           hjust = 0, size = 4.8, color = "#003f5c") +
  annotate("text", x = 0.10, y = 0.62, label = paste("Height:", height, "cm"),
           hjust = 0, size = 4.8, color = "#003f5c") +
  annotate("text", x = 0.10, y = 0.54, label = paste("Weight:", weight, "kg"),
           hjust = 0, size = 4.8, color = "#003f5c") +
  annotate("text", x = 0.10, y = 0.46, label = paste("BMI:", round(bmi, 2)),
           hjust = 0, size = 4.8, color = "#003f5c") +
  annotate("text", x = 0.10, y = 0.38, label = paste("Duration:", duration, "min"),
           hjust = 0, size = 4.8, color = "#003f5c") +
  annotate("text", x = 0.10, y = 0.30, label = paste("Heart Rate:", heart_rate),
           hjust = 0, size = 4.8, color = "#003f5c") +
  
  # prediction
  annotate("text", x = 0.75, y = 0.58,
           label = "Predicted Calories",
           size = 5.3, fontface = "bold", color = "#1f7cff") +
  annotate("text", x = 0.75, y = 0.43,
           label = paste0(round(pred, 1), " kcal"),
           size = 8.0, fontface = "bold", color = "#ff003c") +
  
  theme_void()

ggsave("plots/user_summary_card.png", user_card, width = 10, height = 6)
readline("Press Enter to view User Summary Card...")
print(user_card)



# ----------------------------------------------------------------------
# Helper: create exact gradient for lines
# ----------------------------------------------------------------------

gradient_line_df <- function(x, y, cols) {
  n <- length(x)
  col_interp <- colorRampPalette(cols)(n)
  data.frame(x = x, y = y, col = col_interp)
}



# ----------------------------------------------------------------------
# 2) PERFECT GRADIENT-LINE SAFE DENSITY PLOTS
# ----------------------------------------------------------------------

modern_gradient_density <- function(feature, label, user_value, filename) {
  
  dens <- density(calories[[feature]], na.rm = TRUE)
  df <- gradient_line_df(dens$x, dens$y, grad_cols)
  
  p <- ggplot(df, aes(x, y)) +
    geom_area(fill = alpha("#cfefff", 0.35)) +
    
    geom_line(aes(color = col), size = 1.4, show.legend = FALSE) +
    scale_color_identity() +
    
    annotate("segment",
             x = user_value, xend = user_value,
             y = 0, yend = max(df$y),
             color = "#ff003c", size = 1.3) +
    
    annotate("text", x = user_value, y = max(df$y),
             label = paste("You:", round(user_value,2)),
             color = "#ff003c", vjust = -1, size = 4.8,
             fontface = "bold") +
    
    labs(
      title = paste(label, "Distribution"),
      subtitle = "Gradient Styled Density (Matches Scatter Theme)",
      x = label, y = "Density"
    ) +
    theme_minimal(base_family = "Poppins")
  
  ggsave(paste0("plots/", filename), p, width = 9, height = 5)
  readline(paste("Press Enter to view", label, "density..."))
  print(p)
}

modern_gradient_density("Age", "Age", age, "insight_age_density.png")
modern_gradient_density("BMI", "BMI", bmi, "insight_bmi_density.png")
modern_gradient_density("Duration", "Duration", duration, "insight_duration_density.png")



# ----------------------------------------------------------------------
# 3) EXACT GRADIENT SCATTER (unchanged because it's perfect)
# ----------------------------------------------------------------------

scatter_user <- ggplot(calories, aes(Duration, Calories)) +
  geom_point(aes(color = Calories), size = 2.2, alpha = 0.7) +
  scale_color_gradientn(colors = grad_cols) +
  
  annotate("point", x = duration, y = pred, size = 7, color = "#ff003c") +
  annotate("text", x = duration, y = pred,
           label = paste("You:", round(pred,1), "kcal"),
           vjust = -1.3, color = "#ff003c", size = 5,
           fontface = "bold") +
  
  labs(title = "Your Predicted Calories in Dataset Context",
       subtitle = "Gradient Scatter — User Highlighted",
       x = "Duration (min)", y = "Calories Burnt") +
  theme_minimal(base_family = "Poppins")

ggsave("plots/user_scatter_highlight.png", scatter_user, width = 9, height = 7)
readline("Press Enter to view Gradient Scatter...")
print(scatter_user)



# ----------------------------------------------------------------------
# 4) SNAPSHOT HISTOGRAM GRID (gradient styled)
# ----------------------------------------------------------------------

plot_snap <- function(feature, label, user_value) {
  
  ggplot(calories, aes_string(x = feature)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 25,
                   fill = alpha(grad_cols[4], 0.8)) +
    
    annotate("segment",
             x = user_value, xend = user_value,
             y = 0, yend = Inf,
             color = "#ff003c", size = 1.2) +
    
    labs(title = label, x = "", y = "Density") +
    theme_minimal(base_family = "Poppins")
}

p1 <- plot_snap("Age", "Age", age)
p2 <- plot_snap("BMI", "BMI", bmi)
p3 <- plot_snap("Duration", "Duration", duration)
p4 <- plot_snap("Heart_Rate", "Heart Rate", heart_rate)

snap_grid <- grid.arrange(
  p1, p2, p3, p4,
  ncol = 2,
  top = textGrob("User vs Dataset — Snapshot Overview",
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

ggsave("plots/user_snapshots.png", snap_grid, width = 12, height = 8)
readline("Press Enter to view Snapshot Grid...")
grid.draw(snap_grid)



cat("\n🌈 Gradient User Insight Section Generated Successfully!\n")
cat("All graphs saved in the 'plots/' folder.\n\n")
