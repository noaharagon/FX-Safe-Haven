#Jonas Schmitten & Noah Angara
#Economics in Practice
#March 2021

#packages
library("readxl")
library("dplyr")
library("purrr")
library(stringr)
library(xts)
library(ggplot2)
library(ggthemes)
library(zoo)

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
spot_rates[,23:ncol(spot_rates)] <- spot_rates[nrow(spot_rates):1, 23:ncol(spot_rates)]
independent_vars[,35:ncol(independent_vars)] <- independent_vars[nrow(independent_vars):1,35:ncol(independent_vars)]

#Merge all spot rates into one DataFrame with uniform dates
currencies <- colnames(spot_rates[, seq(2, ncol(spot_rates), 2)])
df_list <- list()
for (i in currencies) {
  curr <- paste("df", i, sep = ".")
  assign(curr, spot_rates[,(which(colnames(spot_rates)== i)-1):which(colnames(spot_rates)== i)])
  df_list[[curr]] <- curr
}
#Change Column Names to Dates
for (i in df_list){
  d=get(i)
  colnames(d)[1]= "dates"
  assign(i,d)
}

#Merge All Cols into One
spot_rates_merged <- lapply(df_list, get) %>% 
  reduce(left_join, by = "dates",na_matches="never")

rm(list=setdiff(ls(), c("spot_rates_merged",'spreads','stable_coins','independent_vars')))

#Crossrates
for (i in c("JAPANESE YEN TO US $ (WMR) - EXCHANGE RATE",'NORWEGIAN KRONE TO US $ (WMR) - EXCHANGE RATE',
            'BRAZILIAN REAL TO US $ (WMR) - EXCHANGE RATE',"INDIAN RUPEE TO US $ (WMR) - EXCHANGE RATE" )) { 
  spot_rates_merged[paste(str_split(i, "US",simplify = T)[1], "CHF (WMR) - EXCHANGE RATE", sep="")][1:which(grepl("2003-07-14", spot_rates_merged$dates)),] = 
    (1/spot_rates_merged["SWISS FRANC TO US $ (WMR) - EXCHANGE RATE"][1:which(grepl("2003-07-14", spot_rates_merged$dates)),])*
    (spot_rates_merged[i][1:which(grepl("2003-07-14", spot_rates_merged$dates)),])}
rm(i)
spot_rates_merged[,2:ncol(spot_rates_merged)] = sapply(spot_rates_merged[,2:ncol(spot_rates_merged)], as.numeric)

#fill in missing values with previous value
spot_rates_merged = na.locf(spot_rates_merged)




#split independent variables by time frequency
daily_independent_vars = independent_vars[,c(1:32,35:36,39:48,57:64)]


# Data Visualization ------------------------------------------------------

# US Recession Start and End as per FRED St. Louis
recessions.df = read.table(textConnection(
  "Peak, Trough
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01"), sep=',', colClasses=c('Date', 'Date'), header=TRUE)
recessions.trim = subset(recessions.df, Peak >= "2000-03-17")
rm(recessions.df)

# Loop to Create Plots of Spot Rates With Recession Shading
plot_list = list()
for (i in colnames(spot_rates_merged[2:19])) {
  z = zoo(spot_rates_merged[, i], order.by = spot_rates_merged$dates)
  g = ggplot(fortify(z,melt=T)) +
    geom_line(aes(x=Index,y=Value))+ 
    geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
    ylab("Spot Rate") +
    xlab("Date") +
    theme_economist_white() +
    ggtitle(paste("Spot Rate of", i, sep = " "))
  plot_list[[i]] = g
  ggsave(g, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}
rm(c(i,z))

# Finite Gaussian Mixture  ------------------------------------------------

difflog = function(x){
  diff(log(x))}
spot_rates_merged_returns = as.data.frame(sapply(spot_rates_merged[,2:ncol(spot_rates_merged)], difflog))



