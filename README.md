# Cloud Capacity Forecasting ☁️📊

**A comprehensive time series analysis project for predicting cloud infrastructure demand.**

![R](https://img.shields.io/badge/Made%20with-R-blue.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)
![Status](https://img.shields.io/badge/Status-Complete-success.svg)

## 📖 Overview
This project focuses on **Demand Forecasting and Capacity Planning** for a cloud service provider. By analyzing hourly usage data across multiple regions and instance types, we aim to predict future demand to optimize resource allocation, reduce costs, and prevent service outages.

The core analysis uses a **hybrid modeling approach**, combining statistical baseline models with advanced stochastic methods to capture complex seasonality and trends.

## 🛠️ Key Technologies
*   **Language:** R
*   **Libraries:** `forecast` (Hyndman), `ggplot2` (Visualization), `dplyr` (Data Manipulation), `lubridate` (Time Parsing)
*   **Techniques:** ETS, ARIMA, STL Decomposition, TSLM (Time Series Linear Models)

## 📂 Project Structure
```text
.
├── 📁 data/          # Raw input files (CSV/Parquet) and random generation logs
├── 📁 docs/          # Project reports and reference documentation
├── 📁 archive/       # Previous iterations and experimental scripts
├── 📄 Project_*.R    # MAIN analysis script (Production Code)
└── 📄 README.md      # This file
```

## 🧠 Modeling Strategy
We explored and compared four distinct modeling approaches to ensure robust forecasts:

1.  **Holt-Winters (ETS)**:
    *   Captures level, trend, and seasonality exponentially.
    *   *Result:* Good baseline, but struggles with complex multi-seasonality.
2.  **TSLM (Linear Regression)**:
    *   Models explicit deterministic trends and seasonal dummies.
    *   *Result:* Excellent for capturing the weekly "heartbeat" of business usage.
3.  **ARIMA / Auto-ARIMA**:
    *   Stochastic modeling of autocorrelated errors.
    *   *Result:* Captures short-term dynamics well but less interpretable.
4.  **Hybrid Two-Level Model (Champion 🏆)**:
    *   **Level 1:** TSLM to remove deterministic trend & seasonality.
    *   **Level 2:** ARIMA to model the residuals (the "unexplained" noise).
    *   *Result:* Best overall accuracy by combining structural and stochastic strengths.

## 🚀 How to Run
1.  **Clone the repository**:
    ```bash
    git clone https://github.com/mohanasundaramm1/Cloud-Capacity-Forecasting.git
    ```
2.  **Open the Project**:
    *   Open `Cloud-Capacity-Forecasting` in RStudio.
3.  **Run the Main Script**:
    *   Open `Project_Mohanasundaram_Murugesan_Code.R`.
    *   Run line-by-line or source the file.
    *   *Note:* The data path is relative (`data/hourly_normalized.csv`), so ensure your working directory is the project root.

## 📊 Key Insights
*   **Weekly Seasonality**: Cloud usage shows a strong weekly cycle, peaking on weekdays and dipping on weekends.
*   **Trend Analysis**: A slight upward linear trend was detected, indicating organic growth in adoption.
*   **Residual Analysis**: The Hybrid model's residuals showed significantly less autocorrelation than single-stage models, validating the approach.

## 📄 License
This project is open-source and available under the [MIT License](LICENSE).
