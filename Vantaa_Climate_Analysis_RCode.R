#-------------------------------
# Climate Data Analysis
# Author: Kamala Sharma
# Dataset: Vantaa Helsinki-Vantaa airport (2015-2020)
#-------------------------------

# 1. Setup
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

# 2. Load Data
Data <- read.csv("C:/Users/rabin/Downloads/ClimateData_VantaAirport.csv") 
head(Data)

# 3. Rename Columns
Data <- Data %>%
  rename(
    Station = Observation.station,
    Precip = Precipitation.amount..mm.,
    SnowDepth = Snow.depth..cm.,
    Tavg = Average.temperature...C.,
    TgroundMin = Minimum.ground.temperature...C.,
    Tmax = Maximum.temperature...C.,
    Tmin = Minimum.temperature...C.
  )

# 4. Create Date Column
Data$Date <- as.Date(paste(Data$Year, Data$Month, Data$Day, sep = "-"), format="%Y-%m-%d")

# 5. Convert numeric columns
Data<- Data%>%
  mutate(
    Precip= as.numeric(Precip), #convert precipitation to numeric
    SnowDepth= as.numeric(SnowDepth),#convert snowdepth to numeric
    Tavg= as.numeric(Tavg),#convert avg temp to numeric
    Tmax= as.numeric(Tmax),# convert max temp to numeric
    Tmin= as.numeric(Tmin),# convert min temp to numeric
    
  )
head(Data)

# 6. Check missing values & duplicates
colSums(is.na(Data))
sum(duplicated(Data))

 Data<- na.omit(Data) # remove the missing values
colSums(is.na(Data))

# 7. Monthly Summary
MonthlySummary <- Data %>%
  group_by(Year, Month) %>%
  summarise(
    AvgT = mean(Tavg, na.rm=TRUE),
    MaxT = max(Tmax, na.rm=TRUE),
    MinT = min(Tmin, na.rm=TRUE),
    TotalPrecip = sum(Precip, na.rm=TRUE),
    AvgSnow = mean(SnowDepth, na.rm=TRUE),
    .groups = "drop"
  )
view(MonthlySummary)

# 8. Yearly Summary
YearlySummary <- Data %>%
  group_by(Year) %>%
  summarise(
    AvgT = mean(Tavg, na.rm=TRUE),
    MaxT = max(Tmax, na.rm=TRUE),
    MinT = min(Tmin, na.rm=TRUE),
    TotalPrecip = sum(Precip, na.rm=TRUE),
    AvgSnow = mean(SnowDepth, na.rm=TRUE),
    .groups = "drop"
  )

#-------------------------------
# 9. Visualizations
#-------------------------------

# 9a. Monthly Trend Line Plot
ggplot(MonthlySummary, aes(x=Month, y=AvgT, group=Year, color=factor(Year))) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(title="Monthly Average Temperature (2015-2020)",
       x="Month", y="Average Temperature (°C)", color="Year") +
  scale_x_continuous(breaks=1:12, labels=month.abb) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12))

# 9b. Box Plot by Month
ggplot(MonthlySummary, aes(x=factor(Month), y=AvgT)) +
  geom_boxplot(fill="skyblue") +
  labs(title="Temperature Distribution by Month",
       x="Month", y="Average Temperature (°C)") +
  scale_x_discrete(labels=month.abb) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16))

# 9c. Scatter Plot Temp vs Precip
ggplot(Data, aes(x=Tavg, y=Precip)) +
  geom_point(alpha=0.5, color="blue") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Temperature vs Precipitation",
       x="Average Temperature (°C)", y="Precipitation (mm)") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16))

# 9d. Histogram of Tavg
ggplot(Data, aes(x=Tavg)) +
  geom_histogram(binwidth=2, fill="skyblue", color="black") +
  labs(title="Distribution of Daily Temperature",
       x="Average Temperature (°C)", y="Count") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16))

# 9e. Density Plot
ggplot(Data, aes(x=Tavg)) +
  geom_density(fill="lightgreen", alpha=0.5) +
  labs(title="Density of Daily Average Temperature",
       x="Average Temperature (°C)", y="Density") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16))

# 9f. Heatmap Month x Year
ggplot(MonthlySummary, aes(x=factor(Month), y=factor(Year), fill=AvgT)) +
  geom_tile(color="white") +
  scale_x_discrete(labels=month.abb) +
  labs(title="Heatmap: Avg Temperature Month x Year", x="Month", y="Year") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# 9g. Faceted Line Plots
ggplot(MonthlySummary, aes(x=factor(Month), y=AvgT, group=Year)) +
  geom_line(color="blue") +
  geom_point(color="red") +
  facet_wrap(~Year) +
  scale_x_discrete(labels=month.abb) +
  labs(title="Monthly Avg Temperature Faceted by Year", x="Month", y="Avg Temp (°C)") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16))

#-------------------------------
# 10. Time Series Analysis
#-------------------------------

# 10a. Prepare TS Data
TS_Data <- Data %>%
  select(Date, Tavg) %>%
  arrange(Date) %>%
  mutate(Year = year(Date),
         Month = month(Date))

# 10b. Daily Time Series Plot
ggplot(TS_Data, aes(x=Date, y=Tavg)) +
  geom_line(color="steelblue") +
  labs(title="Daily Average Temperature (2015-2020)",
       x="Date", y="Average Temperature (°C)") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12))

# 10c. Trend Analysis (Linear Regression)
model_temp <- lm(Tavg ~ Year, data=TS_Data)
summary(model_temp)

# Plot with trend line
ggplot(TS_Data, aes(x=Year, y=Tavg)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Trend of Temperature Over Years",
       x="Year", y="Average Temperature (°C)") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12))

# 10d. Seasonal (Monthly) Pattern
monthly_avg <- TS_Data %>%
  group_by(Month) %>%
  summarise(Avg_Temp = mean(Tavg, na.rm=TRUE))

#Average Monthly Temperature (2015–2020)

ggplot(monthly_avg, aes(x=factor(Month), y=Avg_Temp, group=1)) +
  geom_line(color="darkred", size=1.2) +
  geom_point(color="black", size=2) +
  scale_x_discrete(labels=month.abb) +
  labs(title="Average Monthly Temperature (2015-2020)",
       x="Month", y="Average Temperature (°C)") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12))
