# 📊 Calories Burnt Prediction using Machine Learning (R Project)

## 🧠 Project Overview
This project predicts the number of calories burnt during physical activities based on user input parameters such as gender, age, height, weight, duration, heart rate, and body temperature.  
It compares two machine learning models (**Linear Regression** & **Logistic Regression**) and visualizes the prediction results interactively using **Shiny App** and as a **standalone R Script**.

---

## 🚀 Features

✅ Complete data preprocessing (missing value handling, encoding, normalization)  
✅ Correlation Heatmap for feature relationships  
✅ Feature Importance Visualization  
✅ Residual Analysis & Distribution  
✅ Model Comparison (Linear vs Logistic Regression)  
✅ Interactive Shiny App Interface with gradient UI  
✅ User Input at Runtime in both terminal & app modes  
✅ Auto Visualizations in Plot Pane (when run as R script)

---

## 🧩 Modules

| No. | Module Name | Description |
|-----|--------------|-------------|
| 1 | **Data Loading** | Imports and reads the dataset using `read.csv()` |
| 2 | **Data Preprocessing** | Handles missing values, normalizes, and encodes gender |
| 3 | **Exploratory Data Analysis** | Displays data summary, structure, and statistical overview |
| 4 | **Correlation Analysis** | Visualizes correlation between numerical variables using `ggcorrplot` |
| 5 | **Model Training** | Trains Linear Regression and Logistic Regression models |
| 6 | **Model Evaluation** | Calculates accuracy and compares both models |
| 7 | **Prediction Module** | Takes runtime user inputs and predicts calories burnt |
| 8 | **Visualization Module** | Displays multiple visualizations including predictions and distributions |

---

## 🧰 Technologies Used

**Programming Language:** R  

**Libraries Used:**  
`ggplot2`, `caret`, `corrplot`, `ggcorrplot`, `shiny`, `dplyr`, `scales`, `gridExtra`

---

## ⚙️ Installation & Setup

### 1️⃣ Prerequisites
Ensure you have **R** and **RStudio** installed.  
Install required packages using:

```r
install.packages(c("ggplot2", "caret", "corrplot", "ggcorrplot", "shiny", "dplyr", "scales", "gridExtra"))
