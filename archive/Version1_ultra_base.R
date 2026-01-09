# ─── LIBRARIES ─────────────────────────────────────────────────────────────────
library(dplyr)      # data manipulation
library(readr)      # fast CSV reader
library(stringr)    # string utilities
library(lubridate)  # date-time parsing
library(ggplot2)    # plotting
library(forecast)

# Set working directory for locating files.

setwd("C:/Users/Admin/Desktop/BAN 673/Project")
# ─── 1. LOAD RAW CSV WITH STRICT TYPES ──────────────────────────────────────────
cloud.data <- read_csv(
  "hourly_normalized.csv",
  col_types = cols(
    USAGE_HOUR    = col_character(),
    REGION_NUM    = col_integer(),
    INSTANCE_TYPE = col_character(),
    NORM_USAGE    = col_double()
  )
)

# ─── 2. CLEAN & PARSE USAGE_HOUR ────────────────────────────────────────────────
cloud.data <- cloud.data %>%
  mutate(
    # Step 2a: collapse extra spaces and drop any millisecond suffix
    tmp = USAGE_HOUR %>%
      str_squish() %>%              # "01-02-2021  00:00:00.000" → "01-02-2021 00:00:00.000"
      str_remove("\\.\\d+$"),       # → "01-02-2021 00:00:00"
    
    # Step 2b: parse into POSIXct, handling both dmy and ymd forms
    USAGE_HOUR = parse_date_time(
      tmp,
      orders = c("dmy HM", "dmy HMS", "ymd HM", "ymd HMS"),
      tz     = "UTC"
    )
  ) %>%
  select(-tmp)                     # drop the helper column

# ─── 3. VERIFY PARSE SUCCESS ───────────────────────────────────────────────────
num_bad <- sum(is.na(cloud.data$USAGE_HOUR))
if (num_bad > 0) {
  stop("Still ", num_bad, " timestamps failed to parse!")
}
num_bad
# ─── 4. AGGREGATE TO HOURLY TOTAL ─────────────────────────────────────────────
data.all <- cloud.data %>%
  group_by(USAGE_HOUR) %>%
  summarise(
    total_usage = sum(NORM_USAGE, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(USAGE_HOUR)
data.all

# ─── 5. PLOT THE FULL HOURLY SERIES ────────────────────────────────────────────
ggplot(data.all, aes(x = USAGE_HOUR, y = total_usage)) +
  geom_line(color = "#3366CC", size = 0.15) +
  labs(
    title =   "Total Normalized Usage (All Regions & SKUs)",
    x     =   "Time (hourly)",
    y     =   "Normalized Usage"
  ) +
  theme_minimal()

# Step 1: Extract the numeric time series (total_usage)
autocor <- Acf(data.all$total_usage, lag.max = 24,
               main = "Autocorrelation for total usage")

# Step 2: Extract and print lag + ACF values
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)

data.frame(Lag, ACF)

# 1. Convert to time series object (ts) — hourly data with weekly seasonality
usage.ts <- ts(data.all$total_usage, frequency = 24)

# 2. Apply STL decomposition
usage.stl <- stl(usage.ts, s.window = "periodic")

# 3. Plot using autoplot
library(ggplot2)
autoplot(usage.stl) +
  ggtitle("Hourly Demand: STL Decomposition") +
  theme_minimal()
# 1. Convert your usage vector into a time series object
#usage.ts <- ts(data.all$total_usage, frequency = 24 * 7)
usage.ts <- ts(data.all$total_usage, frequency = 24)
# 2. Define hold-out sizes
nValid <- 8 * 7 * 24     # 1344 hours = 8 weeks
nTrain <- length(usage.ts) - nValid

# 3. Partition the data
train.ts <- usage.ts[1:nTrain]
valid.ts <- usage.ts[(nTrain + 1):(nTrain + nValid)]

# 4. Check lengths
length(train.ts)   # should be total - 1344
length(valid.ts)   # should be 1344


# 1. Convert ts objects back to data frame for plotting
train_df <- data.frame(
  Time = data.all$USAGE_HOUR[1:length(train.ts)],
  Usage = as.numeric(train.ts),
  Set = "Training"
)

valid_df <- data.frame(
  Time = data.all$USAGE_HOUR[(length(train.ts) + 1):(length(train.ts) + length(valid.ts))],
  Usage = as.numeric(valid.ts),
  Set = "Validation"
)

# 2. Combine both into one tidy data frame
plot_df <- bind_rows(train_df, valid_df)

# 3. Plot
ggplot(plot_df, aes(x = Time, y = Usage, color = Set)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Training vs Validation (Hourly Usage)",
    x = "Time (hourly)",
    y = "Total Normalized Usage"
  ) +
  scale_color_manual(values = c("Training" = "#3366CC", "Validation" = "#FF5733")) +
  theme_minimal()


# Fit on training partition
hw_model <- ets(train.ts, model = "ZZZ")
summary(hw_model)

# Predict for validation period
hw_valid_pred <- forecast(hw_model, h = length(valid.ts), level = 0)

# Print forecast
hw_valid_pred

# Accuracy on validation
round(accuracy(hw_valid_pred$mean, valid.ts), 3)

# 1. Create forecast time axis (same as validation period)
forecast_time <- data.all$USAGE_HOUR[(nTrain + 1):(nTrain + nValid)]

# 2. Data frame for actual training data
plot_train <- data.frame(
  Time = data.all$USAGE_HOUR[1:nTrain],
  Usage = as.numeric(train.ts),
  Set = "Training"
)

# 3. Data frame for forecast values only
plot_forecast <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(hw_valid_pred$mean),
  Set = "HW Forecast"
)

# 4. Combine only Training and Forecast for cleaner plot
plot_df <- bind_rows(plot_train, plot_forecast)

# 5. Plot
ggplot(plot_df, aes(x = Time, y = Usage, color = Set, linetype = Set)) +
  geom_line(linewidth = 0.2) +
  scale_color_manual(values = c("Training" = "black", "HW Forecast" = "red")) +
  scale_linetype_manual(values = c("Training" = "solid", "HW Forecast" = "solid")) +
  labs(
    title = "Holt-Winters Forecast on Validation Set (Hourly Cloud Usage)",
    x = "Time",
    y = "Normalized Usage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Fit on full data and forecast 2 weeks ahead
hw_full_model <- ets(usage.ts, model = "ZZZ")
summary(hw_full_model)
hw_future_pred <- forecast(hw_full_model, h = 24 * 14, level = 0)

# Print forecast
hw_future_pred
# 1. Create datetime axis for forecast
start_time <- tail(data.all$USAGE_HOUR, 1) + lubridate::hours(1)
forecast_time <- seq(from = start_time, by = "hour", length.out = 24 * 14)

# 2. Actual data (entire series)
actual_df <- data.frame(
  Time = data.all$USAGE_HOUR,
  Usage = data.all$total_usage
)

# 3. Forecast data (next 2 weeks)
forecast_df <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(hw_future_pred$mean)
)

# 4. Plot: actual in black, forecast in red
ggplot() +
  geom_line(data = actual_df, aes(x = Time, y = Usage), color = "black", size = 0.2) +
  geom_line(data = forecast_df, aes(x = Time, y = Usage), color = "red", size = 0.0001) +
  labs(
    title = "2-Week Forecast Using Holt-Winters (ETS)",
    x = "Time (Hourly)",
    y = "Normalized Cloud Usage"
  ) +
  theme_minimal()
# 1. Generate datetime index for 2-week forecast
start_time <- tail(data.all$USAGE_HOUR, 1) + hours(1)
forecast_time <- seq(from = start_time, by = "hour", length.out = 24 * 14)

# 2. Forecast dataframe
forecast_df <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(hw_future_pred$mean)
)

# 3. Plot only the forecast
ggplot(forecast_df, aes(x = Time, y = Usage)) +
  geom_line(color = "red", size = 0.7) +
  labs(
    title = "Holt-Winters 2-Week Forecast (Zoomed In)",
    x = "Time (Hourly)",
    y = "Forecasted Normalized Usage"
  ) +
  theme_minimal()

# Use Arima() function to fit AR(1) model for cloud data 
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
cloud.ar1<- Arima(usage.ts, order = c(1,0,0))
summary(cloud.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9887
s.e. <- 0.0009
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Create first differenced cloud usage/demand data using lag1.
diff.usage.ts <- diff(usage.ts, lag = 1)
#diff.usage.ts

# Use Acf() function to identify autocorrealtion for first differenced 
# cloud, and plot autocorrelation for different lags 
# (up to maximum of 24).
Acf(diff.usage.ts, lag.max = 24, 
    main = "Autocorrelation for Differenced cloud usage data")

train.ts <- ts(train.ts, frequency = 24)


# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred 

# 1. Create time axis for forecast (same as validation period)
forecast_time <- data.all$USAGE_HOUR[(nTrain + 1):(nTrain + nValid)]

# 2. Actual training data
train_df <- data.frame(
  Time = data.all$USAGE_HOUR[1:nTrain],
  Usage = as.numeric(train.ts),
  Set = "Training"
)

# 3. Forecasted values
forecast_df <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(train.lin.season.pred$mean),
  Set = "Forecast"
)

# 4. Combine
plot_df <- bind_rows(train_df, forecast_df)

# 5. Plot
ggplot(plot_df, aes(x = Time, y = Usage, color = Set, linetype = Set)) +
  geom_line(size = 0.2) +
  scale_color_manual(values = c("Training" = "black", "Forecast" = "blue")) +
  scale_linetype_manual(values = c("Training" = "solid", "Forecast" = "dashed")) +
  labs(
    title = "Linear Regression Forecast validation data(Trend + Seasonality)",
    x = "Time (Hourly)",
    y = "Normalized Usage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 8).
Acf(train.lin.season.pred$residuals, lag.max = 24, 
    main = "Autocorrelation for cloud usage Training data 
    Residuals after lin( trend+ seasonality )")


# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.season$fitted, 
                             train.lin.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Cloud Usage", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 24).
Acf(res.ar1$residuals, lag.max = 24, 
    main = "Autocorrelation for cloud usage Training Residuals of Residuals")


# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Cloud usage", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(usage.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future two weeks.  
lin.season.pred <- forecast(lin.season, h = 24*14, level = 0)
lin.season.pred
# 1. Create forecast time sequence (2 weeks ahead)
start_time <- tail(data.all$USAGE_HOUR, 1) + hours(1)
forecast_time <- seq(from = start_time, by = "hour", length.out = 24 * 14)

# 2. Actual data (full)
actual_df <- data.frame(
  Time = data.all$USAGE_HOUR,
  Usage = as.numeric(usage.ts),
  Set = "Actual"
)

# 3. Forecast data
forecast_df <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(lin.season.pred$mean),
  Set = "Forecast"
)

# 4. Combine
plot_df <- bind_rows(actual_df, forecast_df)

# 5. Plot full timeline
ggplot(plot_df, aes(x = Time, y = Usage, color = Set, linetype = Set)) +
  geom_line(size = 0.2) +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Forecast" = "dashed")) +
  labs(
    title = "Linear Regression Forecast: Full Series + 2-Week Outlook",
    x = "Time (Hourly)",
    y = "Normalized Usage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Just the 2-week forecast
ggplot(forecast_df, aes(x = Time, y = Usage)) +
  geom_line(color = "blue", size = 0.7) +
  labs(
    title = "Zoomed-In: Linear Model 2-Week Forecast",
    x = "Time (Hourly)",
    y = "Forecasted Usage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 24).
Acf(lin.season.pred$residuals, lag.max = 24, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")
# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future two weeks
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 24*14, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)


# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 24).
Acf(residual.ar1$residuals, lag.max = 24, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# Identify forecast for the future two week period as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for two week future periods,
# AR(1) model for residuals for two week future periods, and combined 
# two-level forecast for two week future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df
# 1. Create datetime index for forecast
start_time <- tail(data.all$USAGE_HOUR, 1) + hours(1)
forecast_time <- seq(from = start_time, by = "hour", length.out = 24 * 14)

# 2. Actual full series
actual_df <- data.frame(
  Time = data.all$USAGE_HOUR,
  Usage = as.numeric(usage.ts),
  Set = "Actual"
)

# 3. Final combined forecast (Regression + AR(1))
forecast_df <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(lin.season.ar1.pred),
  Set = "Two-Level Forecast"
)

# 4. Combine for plotting
plot_df <- bind_rows(actual_df, forecast_df)

# 5. Full series plot
ggplot(plot_df, aes(x = Time, y = Usage, color = Set, linetype = Set)) +
  geom_line(size = 0.1) +
  scale_color_manual(values = c("Actual" = "black", "Two-Level Forecast" = "forestgreen")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Two-Level Forecast" = "dashed")) +
  labs(
    title = "Two-Level Forecast (Regression + AR(1)) Overlaid on Full Series",
    x = "Time (Hourly)",
    y = "Normalized Usage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Zoomed-in plot of 2-week forecast only
ggplot(forecast_df, aes(x = Time, y = Usage)) +
  geom_line(color = "forestgreen", size = 0.7) +
  labs(
    title = "Zoomed-In: 2-Week Combined Forecast (Regression + AR(1))",
    x = "Time (Hourly)",
    y = "Forecasted Usage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

round(accuracy(lin.season$fitted + residual.ar1$fitted, usage.ts), 3)

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 24, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")



round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(usage.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future two week periods. 
auto.arima.pred <- forecast(auto.arima, h = 24*14, level = 0)
auto.arima.pred

# 1. Create time index for forecast horizon
start_time <- tail(data.all$USAGE_HOUR, 1) + hours(1)
forecast_time <- seq(from = start_time, by = "hour", length.out = 24 * 14)

# 2. Actual data
actual_df <- data.frame(
  Time = data.all$USAGE_HOUR,
  Usage = as.numeric(usage.ts),
  Set = "Actual"
)

# 3. Forecast data
forecast_df <- data.frame(
  Time = forecast_time,
  Usage = as.numeric(auto.arima.pred$mean),
  Set = "Forecast"
)

# 4. Combine
plot_df <- bind_rows(actual_df, forecast_df)

# 5. Plot full
ggplot(plot_df, aes(x = Time, y = Usage, color = Set, linetype = Set)) +
  geom_line(size = 0.1) +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "darkred")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Forecast" = "dashed")) +
  labs(
    title = "Auto ARIMA Forecast: Full Series with 2-Week Outlook",
    x = "Time (Hourly)",
    y = "Normalized Usage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Zoomed-in plot of just the forecast
ggplot(forecast_df, aes(x = Time, y = Usage)) +
  geom_line(color = "darkred", size = 0.7) +
  labs(
    title = "Zoomed-In: 2-Week Forecast (Auto ARIMA)",
    x = "Time (Hourly)",
    y = "Forecasted Usage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

round(accuracy((snaive(usage.ts))$fitted,usage.ts), 3)
round(accuracy(hw_future_pred$fitted, usage.ts), 3)
round(accuracy(lin.season.pred$fitted, usage.ts),3)
round(accuracy(lin.season$fitted + residual.ar1$fitted, usage.ts), 3)
round(accuracy(auto.arima.pred$fitted, usage.ts), 3)


# 1. Calculate mean usage over last 7 days of training data
end_train <- tail(as.numeric(train.ts), 24 * 7)
mean_train_usage <- mean(end_train)

# 2. Calculate mean forecasted usage over the 2-week forecast
mean_forecast_usage <- mean(hw_future_pred$mean)

# 3. Calculate % increase
growth_pct <- ((mean_forecast_usage - mean_train_usage) / mean_train_usage) * 100
round(growth_pct, 2)

