install.packages("arrow")  # uncomment if you haven't installed arrow yet

library(arrow)
library(dplyr)
library(ggplot2)

# 1) Read the Parquet file
cloud.parq <- read_parquet("hourly_normalized.parquet")

# 2) Verify that USAGE_HOUR is already a POSIXct / POSIXt
str(cloud.parq$USAGE_HOUR)
# You should see something like:  POSIXct[1:xxx], format: "2021-02-01 00:00:00"  

# 3) Aggregate across every region & SKU, by the hour
data.all <- cloud.parq %>%
  group_by(USAGE_HOUR) %>%
  summarize(total_usage = sum(NORM_USAGE, na.rm=TRUE), .groups="drop")

# 4) Plot the full‐granularity aggregated series
ggplot(data.all, aes(x = USAGE_HOUR, y = total_usage)) +
  geom_line(color = "#5471AB") +
  labs(
    title = "Total Normalized Usage (All Regions & SKUs)",
    x     = "Time (hourly)",
    y     = "Normalized Usage"
  ) +
  theme_minimal()
