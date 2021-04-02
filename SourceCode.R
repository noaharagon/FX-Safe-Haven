#Jonas Schmitten & Noah Angara
#Economics in Practice
#March 2021

#packages
library('readxl')

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])


# Data Cleaning -----------------------------------------------------------

#Load raw data
spot_rates = read_excel("FXData.xlsx", sheet='SpotRates')
stable_coins = read_excel("FXData.xlsx", sheet='StableCoins')
independent_vars = read_excel("FXData.xlsx", sheet='IndependentVars')
spreads = read_excel("FXData.xlsx", sheet='Spreads')

#Create DataFrame
spot_rates <- as.data.frame(spot_rates)
independent_vars <- as.data.frame(independent_vars)

#Dates and Rearranging
dates <- seq(as.Date("2000-03-17"), as.Date("2021-03-18"), by = "days")
spot_rates_cleaned <- data.frame(dates)
spot_rates[,seq(1, ncol(spot_rates), 2)] <- lapply(spot_rates[,seq(1, ncol(spot_rates), 2)], as.Date, format = "%y-%m-%d")

#reverse order of some columns to make chronologically
spot_rates[,22:ncol(spot_rates)] <- spot_rates[nrow(spot_rates):1, 22:ncol(spot_rates)]
independent_vars[,35:ncol(independent_vars)] <- independent_vars[nrow(independent_vars):1,35:ncol(independent_vars)]

#treating NA's
spot_rates[spot_rates == "NA"] <- NA


# Data Visualization ------------------------------------------------------


