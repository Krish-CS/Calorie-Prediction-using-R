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
```

---

### 2️⃣ Running in Terminal Mode

To run directly in **R Studio Terminal**, execute:

```r
source("calories_prediction.R")
```

➡️ **You’ll be prompted to enter:**

```mathematica
Enter Gender (Male/Female):
Enter Age:
Enter Height (in cm):
Enter Weight (in kg):
Enter Duration (in minutes):
Enter Heart Rate:
Enter Body Temperature (in °C):
```

📈 After input, all plots (heatmaps, importance charts, residuals, etc.) will automatically appear in the **Plots Pane**.

---

### 3️⃣ Running in Shiny App Mode

For interactive use, execute:

```r
shiny::runApp("calories_app.R")
```

🎨 **The app features:**
- Gradient **blue-to-sea** theme  
- Tabs for each visualization (Prediction Result, Correlation Heatmap, Feature Importance, Residual Distribution)  
- Enlarged & styled **prediction output**  

---

## 📸 Screenshots

Replace the below placeholders after adding your screenshots:

🟦 **Home Interface**  
<img width="1919" height="899" alt="Image" src="https://github.com/user-attachments/assets/0082f191-cf6b-4dff-93d1-f83dfda76de9" />

🟩 **Prediction Result**  
<img width="1919" height="894" alt="Image" src="https://github.com/user-attachments/assets/833fd076-a495-456a-919c-da2e914c8e7c" />

🟨 **Correlation Heatmap**  
<img width="1919" height="894" alt="Image" src="https://github.com/user-attachments/assets/884cad81-19d2-495b-bc22-bdb2dee36b4d" />

🟧 **Feature Importance Plot**  
<img width="1919" height="895" alt="Image" src="https://github.com/user-attachments/assets/1f1e9384-e6d2-4c64-a3b8-5e9b06743a98" />

🟪 **Residual Analysis Plot**  
<img width="1919" height="897" alt="Image" src="https://github.com/user-attachments/assets/bbde89ea-9485-4055-ad73-6a5b71a361d4" />

---

## 🔍 Visualization Outputs

This project automatically generates:

📊 Correlation Heatmap  
📈 Feature Importance Comparison  
📉 Residual Distribution  
📋 Model Comparison Bar Chart  
🎯 Final Predicted Result Visualization  

All graphs are **center aligned** and displayed with **attractive color gradients**.

---

## 🎯 Sample Output (Terminal Mode)

```
Enter Gender (Male/Female): Male
Enter Age: 25
Enter Height (in cm): 180
Enter Weight (in kg): 75
Enter Duration (in minutes): 60
Enter Heart Rate: 120
Enter Body Temperature (in °C): 37.5
```

**Predicted Calories Burnt:**  
`245.67 kcal (via Linear Regression)`

---

## 🧮 Model Comparison Results

| Model | Accuracy / RMSE | Remarks |
|--------|----------------|----------|
| **Linear Regression** | ✅ Best performance | More stable for continuous calorie values |
| **Logistic Regression** | Slightly less accurate | Useful for categorical outcomes |

---

## 🌈 Future Enhancements

🔹 Integration with **smartwatch data APIs**  
🔹 Multi-model **auto selection using cross-validation**  
🔹 Deployment on a **live Shiny Server**  
🔹 Export reports as **PDFs**

---

⭐ *If you like this project, consider giving it a star on GitHub!* ⭐
