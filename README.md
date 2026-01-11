# Player Performance Lab
click here to demo product: https://dgt-international.shinyapps.io/Player_Performance_Lab/

# Overview
This project is an interactive Shiny dashboard that analyzes Major League Soccer (MLS) player performance data and predicts player salaries using a trained Random Forest model. Users can analyze player statistics, rank players based on selected metrics, and view estimated salaries generated from a trained machine learning model.
The goal of this project is to support soccer recruiting and evaluation using exploratory data analysis, predictive modeling, and deployment into a single end-to-end data science application.

# Features
- Player Search with estimated salary predictions compared to actual salary
- Top 10 player rankings based on user-selected performance metrics
- Adjustable age range and metric filters
- Interactive visualizations comparing raw KPIs and normalized user-selected KPIs
- Salary prediction powered by a tuned Random Forest model

## Data
This project uses player-level Major League Soccer (MLS) data retrieved from the *ItsCalledSoccer* API. The dataset contains detailed performance metrics including minutes played, passing efficiency, shooting statistics, expected goals (xG), assists, and other advanced features commonly used in soccer analytics.
Player salary data is merged with performance metrics and used for both exploratory data analysis and supervised learning. The final modeling dataset is restricted to a single season to maintain alignment between observed performance and reported compensation.

# Exploratory Data Analysis
Key EDA steps included:
- Distribution analysis of salaries
- Scatter plots of salary vs. performance and usage metrics
- Boxplots of salary by player position
- Team-level salary comparisons
- Correlation analysis acrocc numeric features
Insights from EDA informed feature selection and model choice.

# Modeling
Multiple models were evaluated:
- Baseline: Linear Regression
- Tuned model (final): Random Forest
- Additional Model: Lasso Regression (for feature selection insights)

Target Transformation
- Salary was modeled on a log scale toa address skewness and heteroskedasticity
- Predictions are converted bask to dallar values in the Shiny app

Evaluation Metrics
- RMSE (primary): Measures typlical prediction error magnitude
- MAE: Provides an interpreteable average absolute error
- R² / Adjusted R²: Used for comparison and explanatory power
The Random Forest model achieved the best overall performance and was selected for deployment

# Shiny App
Better your own roster and scout players here: https://dgt-international.shinyapps.io/Player_Performance_Lab/

# Author
Daniel Tshiani  
https://dgt-international.com  
Email: Info@DGT-International.com  
