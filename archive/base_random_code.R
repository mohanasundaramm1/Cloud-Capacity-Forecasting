```{r agg-hourly-forecast, message=FALSE, warning=FALSE}
# ─── 0. PACKAGES ───────────────────────────────────────────────────────────────
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(forecast)
library(ggplot2)

# ─── 1. LOAD & PARSE RAW DATA ──────────────────────────────────────────────────
cloud.data <- read_csv(
  "hourly_normalized.csv",
  col_types = cols(
    USAGE_HOUR    = col_character(),
    REGION_NUM    = col_integer(),
    INSTANCE_TYPE = col_character(),
    NORM_USAGE    = col_double()
  )
)

# collapse spaces, drop any millisecond suffix, then parse
cloud.data <- cloud.data %>%
  mutate(
    ts_str = USAGE_HOUR %>%
      str_squish() %>%                       # collapse multiple spaces
      str_remove("\\.\\d{1,3}$")             # drop trailing .000, .123, etc.
  ) %>%
  mutate(
    USAGE_HOUR = parse_date_time(
      ts_str,
      orders = c("dmy HM","dmy HMS","ymd HM","ymd HMS"),
      tz     = "UTC"
    )
  ) %>%
  select(-ts_str)

# ─── 2. AGGREGATE TO A SINGLE HOURLY SERIES ─────────────────────────────────
data.all <- cloud.data %>%
  group_by(USAGE_HOUR) %>%
  summarise(
    total_usage = sum(NORM_USAGE, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(USAGE_HOUR)

# ─── 3. BUILD A 'ts' OBJECT (freq = 168 hrs = weekly) ─────────────────────────
z_all  <- zoo(data.all$total_usage, order.by = data.all$USAGE_HOUR)
ts_all <- ts(zoo::coredata(z_all), frequency = 24 * 7)

# ─── 4. SPLIT INTO TRAIN / TEST BY INDEX (last 8 weeks hold‐out) ──────────────
h     <- 8 * 7 * 24         # hours in 8 weeks
n     <- length(ts_all)
train <- ts_all[1:(n - h)]
test  <- ts_all[(n - h + 1):n]

# ─── 5. FIT & FORECAST FOUR MODELS ────────────────────────────────────────────
f_naive  <- naive(train,  h = h)
f_snaive <- snaive(train, h = h)
f_hw     <- hw(train, seasonal = "additive", h = h)
ets_mod  <- ets(train)
f_ets    <- forecast(ets_mod, h = h)

# ─── 6. CALCULATE ACCURACY ON THE HOLD‐OUT ───────────────────────────────────
acc_naive  <- accuracy(f_naive,  test)[, c("RMSE","MAE","MAPE")]
acc_snaive <- accuracy(f_snaive, test)[, c("RMSE","MAE","MAPE")]
acc_hw     <- accuracy(f_hw,     test)[, c("RMSE","MAE","MAPE")]
acc_ets    <- accuracy(f_ets,    test)[, c("RMSE","MAE","MAPE")]

acc_table <- rbind(
  Naive          = acc_naive,
  SeasonalNaive  = acc_snaive,
  HoltWinters    = acc_hw,
  ETS            = acc_ets
) |> round(3)

print(acc_table)

# ─── 7. PLOT ACTUAL vs ALL FORECASTS ─────────────────────────────────────────
start_plot <- n - 3*h + 1
autoplot(ts_all[start_plot:n], series = "Actual") +
  autolayer(f_naive,    series = "Naive") +
  autolayer(f_snaive,   series = "Seasonal Naive") +
  autolayer(f_hw,       series = "Holt–Winters") +
  autolayer(f_ets,      series = "ETS") +
  labs(
    title = "8-Week Ahead Forecasts (Aggregated Hourly Demand)",
    x     = "Time (hourly)",
    y     = "Total Normalized Usage"
  ) +
  scale_colour_brewer(palette="Dark2") +
  theme_minimal()