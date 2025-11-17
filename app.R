# calories_project_shiny_updated.R
# Run: shiny::runApp("calories_project_shiny_updated.R")

library(shiny)
library(shinythemes)
library(ggplot2)
library(randomForest)
library(caret)
library(dplyr)
library(corrplot)
library(plotly)
library(showtext)
library(grid)
library(gridExtra)
library(scales)

# -------------------- Global Font & Theme --------------------
showtext_auto()
font_add_google("Poppins")

# -------------------- Data & Models (preserved) --------------------
calories <- read.csv("calories.csv")
calories <- na.omit(calories)
calories$Gender <- as.factor(calories$Gender)
calories$BMI <- calories$Weight / ((calories$Height / 100)^2)
num_cols <- sapply(calories, is.numeric)
calories_num <- calories[, num_cols]

set.seed(123)
idx <- createDataPartition(calories$Calories, p = 0.8, list = FALSE)
train <- calories[idx, ]
test  <- calories[-idx, ]

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

# -------------------- Gradient palette (your requested palette) --------------------
# pink → aqua → soft blue → deep blue
grad_cols <- c("#ffdee9", "#b5f4fa", "#9bdcff", "#6ecbff", "#4aa3ff", "#1f7cff")

# Helper to compute gradient-for-line dataframe
gradient_line_df <- function(x, y, cols) {
  n <- length(x)
  col_interp <- colorRampPalette(cols)(n)
  data.frame(x = x, y = y, col = col_interp, stringsAsFactors = FALSE)
}

# -------------------- UI --------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      /* Background gradient (requested) */
      body { background: linear-gradient(180deg, #00d2ff 0%, #3a7bd5 100%); font-family: 'Poppins'; }

      /* Nav tabs/title look */
      .nav-tabs > li > a { color: white; font-weight: bold; background-color: transparent; }
      .nav-tabs > li > a:hover { background-color: #4CAF50; color: white; }

      /* Centralize plots */
      .tab-content .shiny-plot-output, .tab-content .plotly.html-widget {
        display: block; margin-left: auto; margin-right: auto; padding-top: 20px;
      }

      /* Prediction large text styling */
      .big-text {
        font-size: 42px; font-weight: bold; color: #FFFFFF;
        text-align: center; padding: 30px 10px; line-height: 1.2; white-space: pre-line;
      }

      /* Recommendation card header and card */
      .rec-title { font-size: 36px; color: #FFFFFF; font-weight: bold; padding-left: 8px; }
      .rec-card { background: rgba(255,255,255,0.95); border-radius: 8px; padding: 22px; color: #003f5c; margin-top: 10px; }

      /* Buttons: stacked, full-width and nicely spaced */
      .btn-stack { width: 100%; margin-bottom: 12px; display: block; }

      /* keep existing color styling for other elements */
      .plot-label { font-weight: 700; }
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
      
      # Buttons stacked with the requested look — use class btn-stack to ensure spacing/alignment
      actionButton("predict", "🔮 Predict Calories", class = "btn btn-primary btn-stack"),
      downloadButton("downloadReport", "📥 Download PDF Report", class = "btn btn-default btn-stack")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction Result", div(class="big-text", textOutput("predText"))),
        tabPanel("Correlation Heatmap", plotOutput("corrPlot", height="500px")),
        tabPanel("Feature Importance", plotlyOutput("featImpPlot", height="500px")),
        tabPanel("Model Comparison", plotlyOutput("modelCompPlot", height="500px")),
        tabPanel("Residual Distribution", plotlyOutput("residPlot", height="500px")),
        tabPanel("Prediction vs Actual", plotlyOutput("predActualPlot", height="500px")),
        
        # User Insights tab with nested sub-tabs (each dynamic)
        tabPanel("User Insights",
                 tabsetPanel(
                   tabPanel("Snapshot Overview", plotOutput("userSnapshots", height = "720px")),
                   tabPanel("Scatter Highlight", plotOutput("userScatter", height = "520px")),
                   tabPanel("Age Density", plotOutput("ageDensity", height = "520px")),
                   tabPanel("BMI Density", plotOutput("bmiDensity", height = "520px")),
                   tabPanel("Duration Density", plotOutput("durationDensity", height = "520px")),
                   tabPanel("Summary Card", plotOutput("summaryCard", height = "420px"))
                 )
        ),
        
        tabPanel("Recommendations",
                 h3(class="rec-title", "BMI & Fitness Recommendation"),
                 div(class="rec-card", htmlOutput("recommendationText", style = "font-size:18px;"))
        )
      )
    )
  )
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  
  # reactive storage
  rv <- reactiveValues(pred = NULL, newdata = NULL, bmi = NULL)
  
  # helper to compute label x offset (prevents overlap on right edge)
  user_label_x <- function(user_x) {
    max_x <- max(calories$Duration, na.rm = TRUE)
    if (is.null(user_x) || is.na(user_x)) return(max_x * 0.5)
    if (user_x > 0.85 * max_x) return(user_x - 0.08 * max_x) else return(user_x + 0.02 * max_x)
  }
  
  # Output prediction text (reactively depends on rv)
  output$predText <- renderText({
    if (is.null(rv$pred)) {
      return("Press the 'Predict Calories' button to generate a prediction and unlock User Insights.")
    }
    paste("\u2728 Best Model \u2728", "\n", best_model,
          "\n\n\u2728 Predicted Calories Burnt \u2728", "\n", round(rv$pred, 2), "kcal")
  })
  
  # Predict event: store new data & prediction into reactiveValues
  observeEvent(input$predict, {
    bmi_val <- input$Weight / ((input$Height / 100)^2)
    new_data <- data.frame(
      Gender = factor(input$Gender, levels = levels(train$Gender)),
      Age = input$Age,
      Height = input$Height,
      Weight = input$Weight,
      Duration = input$Duration,
      Heart_Rate = input$Heart_Rate,
      Body_Temp = input$Body_Temp,
      BMI = bmi_val
    )
    
    lm_p <- predict(lm_model, new_data)
    rf_p <- predict(rf_model, new_data)
    pred_val <- if (best_model == "Linear Regression") lm_p else rf_p
    
    rv$pred <- as.numeric(pred_val)
    rv$newdata <- new_data
    rv$bmi <- bmi_val
    
    # after prediction, switch to Prediction Result tab (optional UX improvement)
    # updateTabsetPanel(session, "tabs", selected = "Prediction Result")  # not used to keep behavior stable
  })
  
  # -------------------- Static (preserved) plots --------------------
  output$corrPlot <- renderPlot({
    corr <- cor(calories_num)
    corrplot(corr, method = "color", type = "upper", tl.col = "black",
             tl.srt = 45, addCoef.col = "black",
             title = "Feature Correlation", mar = c(0,0,2,0))
  })
  
  output$featImpPlot <- renderPlotly({
    imp <- as.data.frame(importance(rf_model))
    imp$Feature <- rownames(imp)
    p <- ggplot(imp, aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity, fill = IncNodePurity)) +
      geom_col() + coord_flip() +
      labs(title = "Feature Importance (Random Forest)", x = "Feature", y = "Importance") +
      scale_fill_gradient(low = "lightblue", high = "darkred")
    ggplotly(p)
  })
  
  output$modelCompPlot <- renderPlotly({
    perf <- data.frame(Model = c("Linear Regression", "Random Forest"), RMSE = c(lm_rmse, rf_rmse))
    p <- ggplot(perf, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity") +
      labs(title = "Model Performance (Lower is Better)") +
      scale_fill_manual(values = c("#00BFFF", "#32CD32"))
    ggplotly(p)
  })
  
  output$residPlot <- renderPlotly({
    res <- data.frame(Residuals_LM = test$Calories - lm_pred, Residuals_RF = test$Calories - rf_pred)
    p <- ggplot(res) +
      geom_histogram(aes(x = Residuals_LM, fill = "Linear Regression"), alpha = 0.6, bins = 20) +
      geom_histogram(aes(x = Residuals_RF, fill = "Random Forest"), alpha = 0.4, bins = 20) +
      labs(title = "Residuals Distribution") +
      scale_fill_manual(values = c("Linear Regression" = "#FF5733", "Random Forest" = "#33C3FF"))
    ggplotly(p)
  })
  
  output$predActualPlot <- renderPlotly({
    df <- data.frame(Actual = test$Calories, Linear = lm_pred, RandomForest = rf_pred)
    p <- ggplot(df) +
      geom_point(aes(x = Actual, y = Linear, color = "Linear Regression"), alpha = 0.6) +
      geom_point(aes(x = Actual, y = RandomForest, color = "Random Forest"), alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Prediction vs Actual", x = "Actual", y = "Predicted") +
      scale_color_manual(values = c("Linear Regression" = "#FF5733", "Random Forest" = "#33C3FF"))
    ggplotly(p)
  })
  
  # -------------------- Dynamic User Insights (depend on rv$newdata & rv$pred) --------------------
  
  output$userScatter <- renderPlot({
    validate(need(!is.null(rv$pred), "Press 'Predict Calories' to view this chart."))
    duration <- rv$newdata$Duration
    pred_val <- rv$pred
    
    p <- ggplot(calories, aes(Duration, Calories)) +
      geom_point(aes(color = Calories), size = 2.2, alpha = 0.75) +
      scale_color_gradientn(colors = grad_cols) +
      annotate("point", x = duration, y = pred_val, size = 7, color = "#ff003c") +
      annotate("text",
               x = user_label_x(duration),
               y = pred_val,
               label = paste0("You: ", round(pred_val, 1), " kcal"),
               vjust = 0.5,
               hjust = ifelse(duration > 0.85 * max(calories$Duration, na.rm = TRUE), 1, 0),
               color = "#ff003c", fontface = "bold", size = 5) +
      labs(title = "Your Predicted Calories in Dataset Context", x = "Duration (min)", y = "Calories Burnt") +
      theme_minimal(base_family = "Poppins") +
      theme(plot.title = element_text(size = 16, face = "bold"))
    print(p)
  })
  
  output$userSnapshots <- renderPlot({
    validate(need(!is.null(rv$pred), "Press 'Predict Calories' to view snapshots."))
    duration <- rv$newdata$Duration
    bmi <- rv$bmi
    heart_rate <- rv$newdata$Heart_Rate
    age <- rv$newdata$Age
    
    p_age <- ggplot(calories, aes(Age)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = grad_cols[4], alpha = 0.85) +
      geom_vline(xintercept = age, color = "#ff003c", size = 1) +
      labs(title = "Age") + theme_minimal(base_family = "Poppins")
    
    p_bmi <- ggplot(calories, aes(BMI)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = grad_cols[4], alpha = 0.85) +
      geom_vline(xintercept = bmi, color = "#ff003c", size = 1) +
      labs(title = "BMI") + theme_minimal(base_family = "Poppins")
    
    p_dur <- ggplot(calories, aes(Duration)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = grad_cols[4], alpha = 0.85) +
      geom_vline(xintercept = duration, color = "#ff003c", size = 1) +
      labs(title = "Duration") + theme_minimal(base_family = "Poppins")
    
    p_hr <- ggplot(calories, aes(Heart_Rate)) +
      geom_histogram(aes(y = after_stat(density)), bins = 25, fill = grad_cols[4], alpha = 0.85) +
      geom_vline(xintercept = heart_rate, color = "#ff003c", size = 1) +
      labs(title = "Heart Rate") + theme_minimal(base_family = "Poppins")
    
    grid.arrange(p_age, p_bmi, p_dur, p_hr, ncol = 2,
                 top = textGrob("User vs Dataset - Snapshot Overview", gp = gpar(fontsize = 16, fontface = "bold", family = "Poppins")))
  })
  
  output$ageDensity <- renderPlot({
    validate(need(!is.null(rv$pred), "Press 'Predict Calories' to view this chart."))
    dens <- density(calories$Age, na.rm = TRUE)
    df <- gradient_line_df(dens$x, dens$y, grad_cols)
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_area(fill = alpha("#cfefff", 0.25)) +
      geom_line(aes(color = col), size = 1.4, show.legend = FALSE) +
      scale_color_identity() +
      geom_vline(xintercept = rv$newdata$Age, color = "#ff003c", size = 1.2) +
      labs(title = "Age Distribution", x = "Age", y = "Density") +
      theme_minimal(base_family = "Poppins")
    print(p)
  })
  
  output$bmiDensity <- renderPlot({
    validate(need(!is.null(rv$pred), "Press 'Predict Calories' to view this chart."))
    dens <- density(calories$BMI, na.rm = TRUE)
    df <- gradient_line_df(dens$x, dens$y, grad_cols)
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_area(fill = alpha("#cfefff", 0.25)) +
      geom_line(aes(color = col), size = 1.4, show.legend = FALSE) +
      scale_color_identity() +
      geom_vline(xintercept = rv$bmi, color = "#ff003c", size = 1.2) +
      labs(title = "BMI Distribution", x = "BMI", y = "Density") +
      theme_minimal(base_family = "Poppins")
    print(p)
  })
  
  output$durationDensity <- renderPlot({
    validate(need(!is.null(rv$pred), "Press 'Predict Calories' to view this chart."))
    dens <- density(calories$Duration, na.rm = TRUE)
    df <- gradient_line_df(dens$x, dens$y, grad_cols)
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_area(fill = alpha("#cfefff", 0.25)) +
      geom_line(aes(color = col), size = 1.4, show.legend = FALSE) +
      scale_color_identity() +
      geom_vline(xintercept = rv$newdata$Duration, color = "#ff003c", size = 1.2) +
      labs(title = "Duration Distribution", x = "Duration (min)", y = "Density") +
      theme_minimal(base_family = "Poppins")
    print(p)
  })
  
  output$summaryCard <- renderPlot({
    validate(need(!is.null(rv$pred), "Press 'Predict Calories' to view the summary."))
    p <- ggplot() +
      annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = alpha("#b5f4fa", 0.65), color = NA) +
      annotate("rect", xmin = 0.03, xmax = 0.97, ymin = 0.03, ymax = 0.97, fill = alpha("#e7f7ff", 0.85), color = NA) +
      annotate("text", x = 0.5, y = 0.89, label = "USER PROFILE SUMMARY", fontface = "bold", size = 7, color = "#004aad", hjust = 0.5) +
      annotate("text", x = 0.10, y = 0.78, label = paste("Gender:", rv$newdata$Gender), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.10, y = 0.70, label = paste("Age:", rv$newdata$Age), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.10, y = 0.62, label = paste("Height:", rv$newdata$Height, "cm"), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.10, y = 0.54, label = paste("Weight:", rv$newdata$Weight, "kg"), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.10, y = 0.46, label = paste("BMI:", round(rv$bmi, 2)), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.10, y = 0.38, label = paste("Duration:", rv$newdata$Duration, "min"), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.10, y = 0.30, label = paste("Heart Rate:", rv$newdata$Heart_Rate), hjust = 0, size = 5, color = "#003f5c") +
      annotate("text", x = 0.75, y = 0.58, label = "Predicted Calories", size = 5.3, fontface = "bold", color = "#1f7cff") +
      annotate("text", x = 0.75, y = 0.43, label = paste0(round(rv$pred, 1), " kcal"), size = 8, fontface = "bold", color = "#ff003c") +
      theme_void()
    print(p)
  })
  
  # -------------------- Recommendations text --------------------
  output$recommendationText <- renderUI({
    bmi_val <- if (!is.null(rv$bmi)) rv$bmi else (input$Weight / ((input$Height / 100)^2))
    if (bmi_val < 18.5) {
      rec_html <- "<ul>
      <li><b>BMI:</b> Underweight (&lt; 18.5). Focus on nutrient-dense calorie intake and strength training to build healthy mass.</li>
      <li>Aim to add 300–500 kcal daily with balanced macros and progressive resistance training 2–3x/week.</li>
      <li>Monitor sleep and reduce excessive cardio until stable mass gained.</li>
      </ul>"
    } else if (bmi_val < 25) {
      rec_html <- "<ul>
      <li><b>BMI:</b> Normal (18.5 - 24.9). Keep up a balanced diet and a mix of cardio + strength training.</li>
      <li>Aim for at least 150 minutes of moderate aerobic activity weekly and 2 strength sessions weekly.</li>
      <li>Stay hydrated, prioritize sleep, and re-evaluate goals monthly.</li>
      </ul>"
    } else if (bmi_val < 30) {
      rec_html <- "<ul>
      <li><b>BMI:</b> Overweight (25 - 29.9). Focus on calorie deficit via diet + increased activity.</li>
      <li>Combine 150–300 minutes of moderate activity per week with resistance training and dietary adjustments.</li>
      <li>Consider consulting a health professional for personalized plan.</li>
      </ul>"
    } else {
      rec_html <- "<ul>
      <li><b>BMI:</b> Obese (&ge; 30). Start with low-impact cardio, structured diet modifications and supervised exercise.</li>
      <li>Gradually increase activity; aim for professional guidance if needed.</li>
      </ul>"
    }
    HTML(rec_html)
  })
  
  # -------------------- Improved PDF report download (cairo_pdf so Poppins renders) --------------------
  output$downloadReport <- downloadHandler(
    filename = function() paste0("calories_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      
      showtext_auto()
      cairo_pdf(file, width = 8.5, height = 11, family = "Poppins")
      
      margin <- unit(0.5, "in")
      
      draw_page_title <- function(title) {
        grid.newpage()
        grid.rect(gp = gpar(fill = "#00d2ff", col = NA))
        grid.text(title,
                  y = 0.92,
                  gp = gpar(fontsize = 22, fontface = "bold", col = "#ffffff"))
        grid.rect(y = 0.89, height = 0.005, gp = gpar(fill = "#3a7bd5", col = NA))
      }
      
      # ---------------- COVER PAGE ----------------
      grid.newpage()
      grid.rect(gp = gpar(fill = "#00d2ff", col = NA))
      
      grid.text("Calories Prediction Report",
                y = 0.70,
                gp = gpar(fontsize = 32, fontface = "bold", col = "#ffffff"))
      
      grid.text(
        paste("Generated on", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        y = 0.60,
        gp = gpar(fontsize = 12, col = "#ffffff")
      )
      
      grid.text(
        paste("Model Used:", best_model),
        y = 0.55,
        gp = gpar(fontsize = 14, fontface = "bold", col = "#ffffff")
      )
      
      grid.text(
        "This report includes model diagnostics,\ndataset insights, and user-specific analytics.",
        y = 0.45,
        gp = gpar(fontsize = 12, col = "#ffffff")
      )
      
      # ---------------- CORRELATION ----------------
      draw_page_title("Feature Correlation Matrix")
      corr <- cor(calories_num)
      corrplot(corr, method = "color", type = "upper",
               tl.col = "black", tl.srt = 45, addCoef.col = "black")
      
      # ---------------- FEATURE IMPORTANCE ----------------
      draw_page_title("Random Forest Feature Importance")
      imp <- as.data.frame(importance(rf_model))
      imp$Feature <- rownames(imp)
      p_imp <- ggplot(imp, aes(x = reorder(Feature, IncNodePurity),
                               y = IncNodePurity, fill = IncNodePurity)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "", y = "Importance") +
        scale_fill_gradient(low = "#00d2ff", high = "#3a7bd5") +
        theme_minimal(base_family = "Poppins")
      print(p_imp)
      
      # ---------------- MODEL PERFORMANCE ----------------
      draw_page_title("Model Comparison (RMSE)")
      perf <- data.frame(Model = c("Linear Regression", "Random Forest"),
                         RMSE = c(lm_rmse, rf_rmse))
      p_perf <- ggplot(perf, aes(Model, RMSE, fill = Model)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        scale_fill_manual(values = c("#3a7bd5", "#00d2ff")) +
        theme_minimal(base_family = "Poppins")
      print(p_perf)
      
      # ---------------- RESIDUAL DISTRIBUTION ----------------
      draw_page_title("Residual Distribution")
      res <- data.frame(
        LM = test$Calories - lm_pred,
        RF = test$Calories - rf_pred
      )
      p_resid <- ggplot(res) +
        geom_histogram(aes(x = LM, fill = "Linear Model"),
                       alpha = 0.6, bins = 25) +
        geom_histogram(aes(x = RF, fill = "Random Forest"),
                       alpha = 0.4, bins = 25) +
        scale_fill_manual(values = c("Linear Model"="#ff6bd6",
                                     "Random Forest"="#1f7cff")) +
        theme_minimal(base_family = "Poppins")
      print(p_resid)
      
      # ---------------- PREDICTION VS ACTUAL ----------------
      draw_page_title("Prediction vs Actual")
      df <- data.frame(
        Actual = test$Calories,
        Linear = lm_pred,
        RF = rf_pred
      )
      p_predact <- ggplot(df) +
        geom_point(aes(Actual, Linear, color = "LM"), alpha = 0.6) +
        geom_point(aes(Actual, RF, color = "RF"), alpha = 0.6) +
        geom_abline(slope = 1, linetype = "dashed") +
        scale_color_manual(values = c(LM="#ff6bd6", RF="#1f7cff")) +
        theme_minimal(base_family = "Poppins")
      print(p_predact)
      
      # ---------------- USER SUMMARY ----------------
      if (!is.null(rv$newdata) && !is.null(rv$pred)) {
        
        draw_page_title("User Profile Summary")
        
        user <- rv$newdata
        
        grid.text("User Summary",
                  y = 0.80,
                  gp = gpar(fontsize = 20, fontface = "bold"))
        
        grid.text(
          paste0(
            "Gender: ", user$Gender, "\n",
            "Age: ", user$Age, "\n",
            "Height: ", user$Height, " cm\n",
            "Weight: ", user$Weight, " kg\n",
            "BMI: ", round(rv$bmi, 2), "\n",
            "Duration: ", user$Duration, " min\n",
            "Heart Rate: ", user$Heart_Rate, "\n\n",
            "Predicted Calories Burnt: ", round(rv$pred, 2), " kcal"
          ),
          y = 0.48, gp = gpar(fontsize = 14)
        )
        
        # ---------------- SNAPSHOT CHARTS ----------------
        draw_page_title("Snapshot Overview")
        
        p_age <- ggplot(calories, aes(Age)) +
          geom_histogram(aes(y=after_stat(density)),
                         bins=25, fill="#3a7bd5", alpha=0.7) +
          geom_vline(xintercept = user$Age, color="#ff003c", size=1.2) +
          theme_minimal(base_family="Poppins")
        
        p_bmi <- ggplot(calories, aes(BMI)) +
          geom_histogram(aes(y=after_stat(density)),
                         bins=25, fill="#3a7bd5", alpha=0.7) +
          geom_vline(xintercept = rv$bmi, color="#ff003c", size=1.2) +
          theme_minimal(base_family="Poppins")
        
        grid.arrange(p_age, p_bmi, ncol = 2)
        
        # ---------------- SCATTER HIGHLIGHT ----------------
        draw_page_title("User Scatter Highlight")
        p_scatter <- ggplot(calories, aes(Duration, Calories)) +
          geom_point(aes(color = Calories), alpha = 0.7) +
          scale_color_gradientn(colors = grad_cols) +
          annotate("point", x = user$Duration, y = rv$pred,
                   color="#ff003c", size=6) +
          theme_minimal(base_family="Poppins")
        print(p_scatter)
        
        # ---------------- RECOMMENDATIONS ----------------
        draw_page_title("BMI & Fitness Recommendation")
        
        rec <- if (rv$bmi < 18.5)
          "Underweight — Increase calories, strength training recommended."
        else if (rv$bmi < 25)
          "Normal — Maintain balanced diet and consistent workouts."
        else if (rv$bmi < 30)
          "Overweight — Start calorie deficit and increased cardio."
        else
          "Obese — Begin supervised cardio and adjusted nutrition."
        
        grid.text(rec, gp = gpar(fontsize = 14), y = 0.70)
      }
      
      dev.off()
    }
  )
  
  
} # end server

# Run app
shinyApp(ui, server)
