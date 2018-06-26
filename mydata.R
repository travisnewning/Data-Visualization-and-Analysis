####Objective####
#Perform an in-depth analysis of electricity consumption dataset

####Check for already installed packages####
row.names(installed.packages())

####Install and Load Packages####
install.packages(c("tidyr", "dplyr"))
install.packages("skimr")
install.packages("lubridate")
install.packages("plotly")
install.packages("ggfortify")
install.packages("forecast")
install.packages("corrplot")
library(tidyr)
library(dplyr)
library(skimr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggfortify)
library(forecast)
library(corrplot)

####Import Data####
getwd()
mydata <- read.table("C:/Users/Saad/Desktop/Data Analytics - CPE/Course 5/household_power_consumption.txt", header=TRUE, sep=";")

####REVIEW COMMANDS: mydata####
summary(mydata)
class(mydata)
str(mydata)
View(mydata)



####Sub-Meter Areas####
#Sub-Meter #1: Dishwasher, oven, microwave
#Sub-Meter #2: Laundry room, washing-machine, tumble-drier, refrigerator, and light
#Sub-Meter #3: Electric water heater and air-conditioner


####New DateTime Column & data.frame####
mydatanew <-cbind(mydata,paste(mydata$Date,mydata$Time), stringsAsFactors=FALSE) #Combine date and time into one attribute
colnames(mydatanew)[10] <-"DateTime" #Rename newly added column
mydatanew <- mydatanew[,c(ncol(mydatanew), 1:(ncol(mydatanew)-1))] #Move the newly added column to the beginning of the dataset (i.e.: first column)


####REVIEW COMMANDS: mydatanew####
summary(mydatanew)
class(mydatanew)
skim <- skim(mydatanew)
sum(is.na(mydatanew$DateTime))
summary(mydatanew$Date)
skim(mydatanew)
str(mydatanew)
View(mydatanew)


####Change attribute types####
mydatanew$DateTime <- strptime(mydatanew$DateTime, "%d/%m/%Y %H:%M:%S") #Changes to POSIXlt
mydatanew$DateTime <- as.POSIXct(mydatanew$DateTime, "%H:%M:%S") #Change to POSIXct
mydatanew$Date <- as.Date(mydatanew$Date, "%d/%m/%Y") #Adjust format of Date
mydatanew$Global_active_power <- as.numeric(mydatanew$Global_active_power)
mydatanew$Global_intensity <- as.numeric(mydatanew$Global_intensity)
mydatanew$Global_reactive_power <- as.numeric(mydatanew$Global_reactive_power)
mydatanew$Sub_metering_1 <- as.numeric(mydatanew$Sub_metering_1)
mydatanew$Sub_metering_2 <- as.numeric(mydatanew$Sub_metering_2)
mydatanew$Voltage <- as.double(mydatanew$Voltage) #as.numeric is same as.double


####Convert applicable units to kwH####
mydatanew$Global_active_power <- mydatanew$Global_active_power*60
mydatanew$Global_reactive_power <- mydatanew$Global_reactive_power*60
mydatanew$Sub_metering_1 <- mydatanew$Sub_metering_1/1000
mydatanew$Sub_metering_2 <- mydatanew$Sub_metering_2/1000
mydatanew$Sub_metering_3 <- mydatanew$Sub_metering_3/1000

####Mutate()####
mydatanew <- mydatanew %>% #Select dataset
  select(DateTime:Sub_metering_3) %>% #Then select columns you want 
  mutate(Global_apparent_power = sqrt((Global_active_power^2)+(Global_reactive_power^2))) #Use mutate to create new attribute and what it's value should be

mydatanew <- mydatanew %>% 
  select(DateTime:Global_apparent_power) %>% 
  mutate(Global_power_factor = Global_active_power/Global_apparent_power)

mydatanew <- mydatanew %>%
  select(DateTime:Global_power_factor) %>% 
  mutate(Year = as.numeric(format(mydatanew$Date, format = "%Y")),
         Month = as.numeric(format(mydatanew$Date, format = "%m")),
         Day = as.numeric(format(mydatanew$Date, format = "%d")))

mydatanew <- mydatanew[,c(1,2,13,14,15,3:12)] #Re-order columns


mydatanew$Time2 <- mydatanew$Time #Duplicate Time attribute to so that I can have full time and split time all in same dataset
mydatanew <- mydatanew %>% separate(Time2, c("Hour", "Minute", "Second", sep=":")) #Separate data based on ':' and name each one as listed in c().
mydatanew <- mydatanew[-c(19)] #Extra non-data column that was created that needed to be removed
mydatanew <- mydatanew[,c(1:6,16:18,7:15)] #Re-order columns
mydatanew$Hour <- as.numeric(mydatanew$Hour) #Change newly created column to numeric
mydatanew$Minute <- as.numeric(mydatanew$Minute) #Change newly created column to numeric
mydatanew$Second <- as.numeric(mydatanew$Second) #Change newly created column to numeric

mydatanew <- mydatanew %>% 
  select(DateTime:Global_power_factor) %>% 
  mutate(DayofWeek = weekdays(as.Date(Date, '%Y-%m-%d')))

mydatanew <- mydatanew[,c(1,19,2:18)]



####CleanData####
CleanData <- filter(mydatanew, !is.na(mydatanew$Sub_metering_3)) #which Sub_metering_3 observations are not empty


####REVIEW COMMANDS: CleanData####
summary(CleanData)
str(CleanData)
View(CleanData)


####Example Filter####
#Example of how to filter by hour of day, from minute 0 to 59, and assigning it to it's own object
#AM00 <- subset(CleanData, format(DateTime, '%H') %in% c('00'))


####Plot_ly: 9 Jan 2008#####
#Subset the 9th day of January 2008 (all observations from that day)
Jan092008 <- filter(CleanData, Date>"2008-01-08" & Date<"2008-01-10")
#Plot sub-meter 1
plot_ly(Jan092008, x = ~Jan092008$DateTime, y = ~Jan092008$Sub_metering_1, type= 'scatter', mode='lines')


#Plot sub-meter 1, 2, and 3 with title, legend and labels (all observations from 9 Jan 2008)
plot_ly(Jan092008, x = ~Jan092008$DateTime, y =~Jan092008$Sub_metering_1, name = 'Kitchen', type='scatter', mode='lines') %>% 
  add_trace(y=~Jan092008$Sub_metering_2, name='Laundry Room', mode='lines') %>%
  add_trace(y=~Jan092008$Sub_metering_3, name='Water Heater & AC', mode='lines') %>% 
  layout(title="Power Consumption January 9th, 2008", xaxis=list(title="Time"), yaxis=list(title ="Power (kwH)"))
  

#Subsets observations to those recorded every 10 minutes starting at the top  of the hour (only those recorded every ten minutes are recorded; for example times that end in :09 or :11 are not included in new dataset)
Jan092008TenMin <- subset(Jan092008, format(DateTime, '%M') %in% c('0','10','20','30','40','50'))


#Plots Jan092008TenMin (data from 9 Jan 2008 taken every 10 minutes)
plot_ly(Jan092008TenMin, x =~Jan092008TenMin$DateTime, y=~Jan092008TenMin$Sub_metering_1, name='Kitchen', type='scatter', mode='lines') %>%
  add_trace(y=~Jan092008TenMin$Sub_metering_2, name= 'Laundry Room', mode = 'lines') %>%
  add_trace(y =~Jan092008TenMin$Sub_metering_3, name= "Water Heater & AC", mode='lines') %>%
  layout(title="Power Consumption January 9th, 2008", xaxis=list(title = "Time"), yaxis=list(title="Power (kwH)"))
  

####plot_ly: 13 Jul 2008####
#Data for week of 13 Jul 2008
July13to192008 <- filter(CleanData, Date>"2008-07-12" & Date<"2008-07-20")

#Subset 13 Jul 2018 to data from every four hours (i.e: 0:00 to 0:59, 4:00 to 4:59, etc.)
July13to192008FourHour <- subset(July13to192008, format(DateTime, '%H') %in% c('0','4','8','12','16','20'))

#Subset further to only every fifteen minutes of every four hours
July13to192008FourHour15Min <- subset(July13to192008FourHour, format(DateTime, '%M') %in% c('00','15', '30', '45'))

#Plot the 15 minute increments of every four hour period
plot_ly(July13to192008FourHour15Min, x=~July13to192008FourHour15Min$DateTime, y=~July13to192008FourHour15Min$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode='lines') %>% 
  add_trace(y=~July13to192008FourHour15Min$Sub_metering_2, name = 'Laundry Room', mode ='lines') %>% 
  add_trace(y=~July13to192008FourHour15Min$Sub_metering_3, name='Water Heater & AC', mode='lines') %>% 
  layout(title="Power Consumption July 13-19, 2008", xaxis=list(title='Time'), yaxis=list(title="Power (kwH)"))


####Weekday and Weekend Noon####
PM12 <- subset(CleanData, format(DateTime, '%H') %in% c('00'))

PM12Weekend <- filter(PM12, DayofWeek == "Sunday" | DayofWeek == "Saturday") #Weekend filter
PM12WeekendTopofHour <- filter(PM12Weekend, format(DateTime, '%M') %in% c('00')) #Weekend noon filter
PM12Weekday <- filter(PM12, DayofWeek %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) #Weekday noon filter


plot(PM12Weekend$Sub_metering_1)
plot(PM12Weekday$Sub_metering_1)
plot(PM12WeekendTopofHour$Sub_metering_1)


plot_ly(PM12WeekendTopofHour, x=~PM12WeekendTopofHour$DateTime, y=~PM12WeekendTopofHour$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode='lines') %>% 
  add_trace(y=~PM12WeekendTopofHour$Sub_metering_2, name = 'Laundry Room', mode ='lines') %>% 
  add_trace(y=~PM12WeekendTopofHour$Sub_metering_3, name='Water Heater & AC', mode='lines') %>% 
  layout(title="Power Consumption: Weekends @ Noon", xaxis=list(title='Time'), yaxis=list(title="Power (kwH)"))



####Time Series Object####
#Subset to one observation per week: Mondays at 8:01pm for all years; meaning one observation taken per week
house070809weekly <- filter(CleanData, DayofWeek=='Monday' & Hour==20 & Minute==1)
View(house070809weekly)


#Create TS object with submeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency = 52, start = c(2007,1)) #Time series object for the one weekly recorded object in house070809weekly
View(tsSM3_070809weekly)

#Plot sub-meter 3 with autoplot
autoplot(tsSM3_070809weekly)

#Plot sub-meter 3 with autoplot - add labels and color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "kwH", title="Sub-meter 3")

#Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

#Sub-meters 1 and 2 time series graphs
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency = 52, start = c(2007,1))
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency = 52, start = c(2007,1))

autoplot(tsSM1_070809weekly, ts.colour = 'green', xlab = "Time", ylab = "kwH", title="Sub-meter 1")
autoplot(tsSM2_070809weekly, ts.colour = 'blue', xlab = "Time", ylab = "kwH", title="Sub-meter 2")

plot.ts(tsSM1_070809weekly)
plot.ts(tsSM2_070809weekly)

####Forecasting: Sub-Meters 1,2, and 3####
#Apply time series linear regression to the sub-meters 1,2, and 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM1 <- tslm(tsSM1_070809weekly~trend + season)
summary(fitSM1)

#Create the forecast for sub-meter 1. Forecase ahead 20 time periods (weeks)
forecastfitSM1 <- forecast(fitSM1, h=20)
#Plot the forecast for sub-meter 1
plot(forecastfitSM1, ylim = c(0,0.07), xlab = "Year", ylab = 'kWh', main = "Forecast 20 Weeks Out: Sub-Meter 1")


#Create sub-meter 1 forecast with confidence levels of 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=20, level = c(80,90))
#Plot sub-meter 1 forecast, limit y-axis and add labels
plot(forecastfitSM1c, ylim = c(0,.05), ylab='kWh', xlab = "Time")


#Forecasting: Sub-Meter 2
fitSM2 <- tslm(tsSM2_070809weekly~trend + season)
summary(fitSM2)

#Create the forecase for sub-meter 2. Forecast ahead 20 time periods
forecastfitSM2 <- forecast(fitSM2, h=20)
#Plot the forecast for sub-meter 2
plot(forecastfitSM2, ylim = c(0,0.07), xlab = "Year", ylab = 'kWh', main = "Forecast 20 Weeks Out: Sub-Meter 2")


#Create sub-meter 2 forecast with confidence levels of 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=20, level = c(80,90))
#Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0,.05), ylab='kWh', xlab = "Time")


#Forecasting: Sub-Meter 3
fitSM3 <- tslm(tsSM3_070809weekly~trend + season)
summary(fitSM3)

#Create the forecase for sub-meter 3. Forecast ahead 20 time periods
forecastfitSM3 <- forecast(fitSM3, h=20)
#Plot the forecast for sub-meter 3
plot(forecastfitSM3, ylim = c(0,0.07), xlab = "Year", ylab = 'kWh', main = "Forecast 20 Weeks Out: Sub-Meter 3")


#Create sub-meter 3 forecast with confidence levels of 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level = c(80,90))
#Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0,.05), ylab='kWh', xlab = "Time")


####Decompose Time Series: Sub-Meters 1,2, and 3####
#Decompose sub-meter 1 into trend, seasonal, and remainder (random)
components070809SM1weekly <- decompose(tsSM1_070809weekly)
#Plot decomposed sub-meter 1
plot(components070809SM1weekly)
#Check summary statistics for decomposed sub-meter 1
summary(components070809SM1weekly)


#Decompose sub-meter 2 into trend, seasonal, and remainder (random)
components070809SM2weekly <- decompose(tsSM2_070809weekly)
#Plot decomposed sub-meter 2
plot(components070809SM2weekly)
#Check summary statistics for decomposed sub-meter 2
summary(components070809SM2weekly)


#Decompose sub-meter 3 into trend, seasonal, and remainder (random)
components070809SM3weekly <- decompose(tsSM3_070809weekly)
#Plot decomposed sub-meter 3
plot(components070809SM3weekly)
#Check summary statistics for decomposed sub-meter 3
summary(components070809SM3weekly)



####HoltWinters() Forecasting####
#Requires removing the seasonal component identified via decomposition
#Seasonal adjusting Sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)

#Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for seasonal
plot(decompose(tsSM1_070809Adjusted))

#Holt Winters Simple Exponential smoothing & plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM1_HW070809, ylim = c(0,.025))

#HoltWinters Forecast
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
tsSM1_HW070809for
plot(tsSM1_HW070809for, ylim = c(0,0.020), ylab = "Kilowatt-Hours", xlab = "Time - Sub-Meter 1")

#Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level = c(10,25))
#Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0,0.020), ylab = "Kilowatt-Hours", xlab = "Time - Sub-Meter 1", start(2010))



#Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)

#Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for seasonal
plot(decompose(tsSM2_070809Adjusted))

#Holt Winters Simple Exponential smoothing & plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta = FALSE, gamma = FALSE)
plot(tsSM2_HW070809, ylim = c(0,.025))

#HoltWinters Forecast
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
tsSM2_HW070809for
tsSM2_HW070809for$residuals
plot(tsSM2_HW070809for, ylim = c(0,0.020), ylab = "Kilowatt-Hours", xlab = "Time - Sub-Meter 2")

#Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level = c(10,25))
#Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0,0.020), ylab = "Kilowatt-Hours", xlab = "Time - Sub-Meter 2", start(2010))



#Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

#Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for seasonal
plot(decompose(tsSM3_070809Adjusted))
plot(tsSM3_070809Adjusted)

#Holt Winters Simple Exponential smoothing & plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta = FALSE, gamma = FALSE)
tsSM3_HW070809
tsSM3_HW070809$fitted
tsSM3_HW070809$SSE
plot(tsSM3_HW070809, ylim = c(0,.025))


#HoltWinters Forecast
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
tsSM3_HW070809for
plot(tsSM3_HW070809for, ylim = c(0,0.020), ylab = "Kilowatt-Hours", xlab = "Time - Sub-Meter 3")


#Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level = c(10,25))
#Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0,0.020), ylab = "Kilowatt-Hours", xlab = "Time - Sub-Meter 3", start(2010))
