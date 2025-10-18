# calories_project.R
library(shiny)
library(shinythemes)
library(ggplot2)
library(randomForest)
library(caret)
library(dplyr)
library(corrplot)
library(plotly)
library(showtext)
# -------------------- Global Font & Theme --------------------
showtext_auto()
font_add_google("Poppins")
calories <- read.csv("calories.csv")
calories <- na.omit(calories)
calories$Gender <- as.factor(calories$Gender)
calories$BMI <- calories$Weight / ((calories$Height/100)^2)
num_cols <- sapply(calories, is.numeric)
calories_num <- calories[, num_cols]
set.seed(123)
idx <- createDataPartition(calories$Calories, p = 0.8, list = FALSE)
train <- calories[idx, ]
test  <- calories[-idx, ]
# -------------------- Models --------------------
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
# ===================== Shiny UI =====================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body {
        /* Blue–Sea Gradient */
        background: linear-gradient(180deg, #00d2ff 0%, #3a7bd5 100%);
        font-family: 'Poppins';
      }
      .nav-tabs > li > a {
        color: white; font-weight: bold;
        background-color: transparent;
      }
      .nav-tabs > li > a:hover {
        background-color: #4CAF50; color: white;
      }
      /* Center all plot outputs with padding from top */
      .tab-content .shiny-plot-output,
      .tab-content .plotly.html-widget {
        display: block;
        margin-left: auto;
        margin-right: auto;
        padding-top: 30px;
      }
      /* Prediction text styling */
      .big-text {
        font-size: 42px;
        font-weight: bold;
        color: #FFFFFF;     /* DodgerBlue */
        text-align: center;
        padding: 40px 10px;
        line-height: 1.4;
        white-space: pre-line;  /* Allows line breaks */
      }
  "))
  ),
  titlePanel("Calories Burn Prediction Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter Your Details"),
      selectInput("Gender", "Gender", choices = levels(calories$Gender)),
      numericInput("Age", "Age", value = 25, min = 10, max = 90),
      numericInput("Height", "Height (cm)", value = 170, min = 100, max = 220),
      numericInput("Weight", "Weight (kg)", value = 65, min = 30, max = 150),
      numericInput("Duration", "Duration (min)", value = 30, min = 1, max = 300),
      numericInput("Heart_Rate", "Heart Rate", value = 90, min = 40, max = 200),
      numericInput("Body_Temp", "Body Temp (°C)", value = 37, min = 35, max = 42),
      actionButton("predict", "🔮 Predict Calories", class = "btn btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction Result",
                 div(class="big-text", textOutput("predText"))),
        tabPanel("Correlation Heatmap",
                 plotOutput("corrPlot", height="500px")),
        tabPanel("Feature Importance",
                 plotlyOutput("featImpPlot", height="500px")),
        tabPanel("Model Comparison",
                 plotlyOutput("modelCompPlot", height="500px")),
        tabPanel("Residual Distribution",
                 plotlyOutput("residPlot", height="500px")),
        tabPanel("Prediction vs Actual",
                 plotlyOutput("predActualPlot", height="500px"))
      )
    )
  )
)
# ===================== Shiny Server =====================
server <- function(input, output, session) {
  observeEvent(input$predict, {
    bmi <- input$Weight / ((input$Height/100)^2)
    new_data <- data.frame(
      Gender = factor(input$Gender, levels = levels(train$Gender)),
      Age = input$Age,
      Height = input$Height,
      Weight = input$Weight,
      Duration = input$Duration,
      Heart_Rate = input$Heart_Rate,
      Body_Temp = input$Body_Temp,
      BMI = bmi
    )
    lm_p <- predict(lm_model, new_data)
    rf_p <- predict(rf_model, new_data)
    pred <- if (best_model == "Linear Regression") lm_p else rf_p
    output$predText <- renderText({
      paste("\u2728 Best Model \u2728 \n", best_model,
            "\n\n\u2728 Predicted Calories Burnt \u2728 \n", round(pred, 2), "kcal")
    })
  })
  output$corrPlot <- renderPlot({
    corr <- cor(calories_num)
    corrplot(corr, method = "color", type = "upper", tl.col = "black",
             tl.srt = 45, addCoef.col = "black",
             title = "Feature Correlation", mar = c(0,0,2,0))
  })
  output$featImpPlot <- renderPlotly({
    imp <- as.data.frame(importance(rf_model))
    imp$Feature <- rownames(imp)
    p <- ggplot(imp, aes(x = reorder(Feature, IncNodePurity),
                         y = IncNodePurity, fill = IncNodePurity)) +
      geom_col() + coord_flip() +
      labs(title = "Feature Importance (Random Forest)",
           x = "Feature", y = "Importance") +
      scale_fill_gradient(low = "lightblue", high = "darkred")
    ggplotly(p)
  })
  output$modelCompPlot <- renderPlotly({
    perf <- data.frame(Model = c("Linear Regression", "Random Forest"),
                       RMSE = c(lm_rmse, rf_rmse))
    p <- ggplot(perf, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity") +
      labs(title = "Model Performance (Lower is Better)") +
      scale_fill_manual(values = c("#00BFFF", "#32CD32"))
    ggplotly(p)
  })
  output$residPlot <- renderPlotly({
    res <- data.frame(
      Residuals_LM = test$Calories - lm_pred,
      Residuals_RF = test$Calories - rf_pred
    )
    p <- ggplot(res) +
      geom_histogram(aes(x = Residuals_LM, fill = "Linear Regression"),
                     alpha = 0.6, bins = 20) +
      geom_histogram(aes(x = Residuals_RF, fill = "Random Forest"),
                     alpha = 0.4, bins = 20) +
      labs(title = "Residuals Distribution") +
      scale_fill_manual(values = c("Linear Regression" = "#FF5733",
                                   "Random Forest" = "#33C3FF"))
    ggplotly(p)
  })
  output$predActualPlot <- renderPlotly({
    df <- data.frame(
      Actual = test$Calories,
      Linear = lm_pred,
      RandomForest = rf_pred
    )
    p <- ggplot(df) +
      geom_point(aes(x = Actual, y = Linear, color = "Linear Regression"),
                 alpha = 0.6) +
      geom_point(aes(x = Actual, y = RandomForest, color = "Random Forest"),
                 alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Prediction vs Actual", x = "Actual", y = "Predicted") +
      scale_color_manual(values = c("Linear Regression" = "#FF5733",
                                    "Random Forest" = "#33C3FF"))
    ggplotly(p)
  })
}
# ===================== Dual Mode =====================
if (interactive()) {
  # ---- Run as Shiny App ----
  shinyApp(ui, server)
} else {
  # ---- Run as Plain Script ----
  message("Running in script mode: displaying plots in RStudio…")
  # Correlation
  corrplot(cor(calories_num), method = "color", type = "upper",
           tl.col = "black", tl.srt = 45, addCoef.col = "black",
           title = "Feature Correlation", mar = c(0,0,2,0))
  readline("Press [Enter] for Feature Importance...")
  imp <- as.data.frame(importance(rf_model))
  imp$Feature <- rownames(imp)
  print(ggplot(imp, aes(x = reorder(Feature, IncNodePurity),
                        y = IncNodePurity, fill = IncNodePurity)) +
          geom_col() + coord_flip() +
          labs(title = "Feature Importance (Random Forest)") +
          scale_fill_gradient(low = "lightblue", high = "darkred"))
  readline("Press [Enter] for Model Comparison...")
  perf <- data.frame(Model = c("Linear Regression", "Random Forest"),
                     RMSE = c(lm_rmse, rf_rmse))
  print(ggplot(perf, aes(x = Model, y = RMSE, fill = Model)) +
          geom_bar(stat = "identity") +
          labs(title = "Model Performance (Lower is Better)") +
          scale_fill_manual(values = c("#00BFFF", "#32CD32")))
  readline("Press [Enter] for Prediction vs Actual...")
  df <- data.frame(Actual = test$Calories,
                   Linear = lm_pred,
                   RandomForest = rf_pred)
  print(ggplot(df) +
          geom_point(aes(x = Actual, y = Linear, color = "Linear Regression"), alpha = 0.6) +
          geom_point(aes(x = Actual, y = RandomForest, color = "Random Forest"), alpha = 0.6) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
          labs(title = "Prediction vs Actual", x = "Actual", y = "Predicted") +
          scale_color_manual(values = c("Linear Regression" = "#FF5733",
                                        "Random Forest" = "#33C3FF")))
  message("✅ All visualizations displayed in RStudio Plots.")
}