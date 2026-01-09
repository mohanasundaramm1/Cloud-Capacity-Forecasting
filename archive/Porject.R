library(forecast)
library(zoo)
library(dplyr)
library(lubridate)

# 1. Parse your datetime column
cloud.data$USAGE_HOUR <- parse_date_time(
  cloud.data$USAGE_HOUR,
  orders = c("dmy HMS", "dmy HM", "Ymd HMS"),
  tz = "UTC"
)

# 2. Pick one region–SKU to explore (e.g. region 1, type A)
sub <- cloud.data %>%
  filter(REGION_NUM == 1, INSTANCE_TYPE == "A") %>%
  arrange(USAGE_HOUR)

# 3. Build a zoo series and then a ts object with weekly frequency
z <- zoo(sub$NORM_USAGE, order.by = sub$USAGE_HOUR)
# 168 = 24 hrs × 7 days
ts_hourly <- ts(coredata(z), frequency = 168)

# 4. Decompose with STL (trend / seasonal / remainder)
stl_res <- stl(ts_hourly, s.window = "periodic")
plot(stl_res, main = "STL Decomposition of Hourly Usage\nRegion 1, SKU A")

# 5. Split into training + 4-week hold-out
h <- 4 * 7 * 24   # 4 weeks of hourly data
n <- length(ts_hourly)
train <- window(ts_hourly, end = n - h)
test  <- window(ts_hourly, start = n - h + 1)

# 6a. Fit an ARIMA model (auto-select orders)
fit_arima <- auto.arima(train, seasonal = TRUE)
# 6b. Fit an ETS (Holt-Winters) model
fit_ets   <- ets(train)

# 7. Forecast next 4 weeks
f_arima <- forecast(fit_arima, h = h)
f_ets   <- forecast(fit_ets,   h = h)

# 8. Plot actual vs ARIMA/ETS forecasts
autoplot(window(ts_hourly, start = n - 3*h)) +
  autolayer(f_arima, series = "ARIMA", PI = FALSE) +
  autolayer(f_ets,   series = "ETS",    PI = FALSE) +
  ggtitle("Region 1, SKU A: Actual vs 4-Week Forecasts") +
  ylab("Normalized Usage") + xlab("Time (hourly)")

# 9. Compute accuracy on the hold-out
acc_arima <- accuracy(f_arima, test)
acc_ets   <- accuracy(f_ets,   test)
print("ARIMA accuracy:")
print(acc_arima)
print("ETS accuracy:")
print(acc_ets)
