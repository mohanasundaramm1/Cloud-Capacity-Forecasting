library(forecast)
library(zoo)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/Users/Admin/Desktop/BAN 673/Project")

# Create data frame.
cloud.data <- read.csv("hourly_normalized.csv")

head(cloud.data)