#Jonas Schmitten & Noah Angara
#Economics in Practice
#March 2021

#packages
library("readxl")
library("dplyr")
library("purrr")
library("stringr")
library("xts")
library("ggplot2")
library("ggthemes")
library("zoo")
library("tibble")
library("mixtools")
library("stargazer")

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])


# Data Cleaning -----------------------------------------------------------

#Load raw data
spot_rates <- read_excel("FXData.xlsx", sheet='SpotRates')
stable_coins <- read_excel("FXData.xlsx", sheet='StableCoins')
independent_vars <- read_excel("FXData.xlsx", sheet='IndependentVars')
spreads <- read_excel("FXData.xlsx", sheet='Spreads')

#Create DataFrame
spot_rates <- as.data.frame(spot_rates)
independent_vars <- as.data.frame(independent_vars)

#Dates and Rearranging
spot_rates[,seq(1, ncol(spot_rates), 2)] <- lapply(spot_rates[,seq(1, ncol(spot_rates), 2)], as.Date, format = "%y-%m-%d")

#reverse order of some columns to make chronologically
spot_rates[,23:ncol(spot_rates)] <- spot_rates[nrow(spot_rates):1, 23:ncol(spot_rates)]
independent_vars[,c(35:64,69:70)] <- independent_vars[nrow(independent_vars):1,c(35:64,69:70)]

spot_rates = spot_rates[,1:22]

# Function to clean data
datacleanup <- function(data){
  vars <- colnames(data[, seq(2, ncol(data), 2)])
  df_list <- list()
  for (i in vars) {
    var <- paste("df", i, sep = ".")
    assign(var, data[,(which(colnames(data)== i)-1):which(colnames(data)== i)], envir = .GlobalEnv)
    df_list[[var]] <- var }
  for (i in df_list){
    d=get(i)
    colnames(d)[1]<- "dates"
    assign(i,d, envir = .GlobalEnv)}
  assign(paste(deparse(substitute(data)),"_merged",sep=""), lapply(df_list, get) %>% reduce(full_join, by = "dates",na_matches="never"))
}

spot_rates_merged <- datacleanup(spot_rates)

rm(list=setdiff(ls(), c("spot_rates_merged",'spreads','stable_coins','independent_vars', "datacleanup")))

#Crossrates
for (i in c("JAPANESE YEN TO US $ (WMR) - EXCHANGE RATE",'NORWEGIAN KRONE TO US $ (WMR) - EXCHANGE RATE',
            'BRAZILIAN REAL TO US $ (WMR) - EXCHANGE RATE',"INDIAN RUPEE TO US $ (WMR) - EXCHANGE RATE" )) { 
  spot_rates_merged[paste(str_split(i, "US",simplify = T)[1], "CHF (WMR) - EXCHANGE RATE", sep="")][1:which(grepl("2003-07-14", spot_rates_merged$dates)),] = 
    (1/spot_rates_merged["SWISS FRANC TO US $ (WMR) - EXCHANGE RATE"][1:which(grepl("2003-07-14", spot_rates_merged$dates)),])*
    (spot_rates_merged[i][1:which(grepl("2003-07-14", spot_rates_merged$dates)),])}
rm(i)
spot_rates_merged[,2:ncol(spot_rates_merged)] = sapply(spot_rates_merged[,2:ncol(spot_rates_merged)], as.numeric)

#fill in missing values with previous value
#spot_rates_merged <- na.locf(spot_rates_merged)

spreads = spreads[,1:29]

#Calculating Bid-Ask Spreads
spreads_clean <- select(spreads, -5)
spreads_clean <- datacleanup(spreads_clean)
date_purposes <- spreads_clean[1:5539,1]
spreads_clean <- spreads_clean %>% select(-contains("Date"))
spreads_clean <- spreads_clean %>% relocate("Ask Price CHF/INR", .after = "Bid Price CHF/INR")
bid_ask <- spreads_clean[, seq(2, ncol(spreads_clean), 2)] - spreads_clean[, seq(1, ncol(spreads_clean), 2)]
colnames(bid_ask)<-gsub("Ask Price","", colnames(bid_ask))
bid_ask <- bid_ask[rowSums(is.na(bid_ask)) != ncol(bid_ask),]
bid_ask$Date <- date_purposes

bid_ask = bid_ask[1:which(grepl(2000-03-17, bid_ask$Date)),]

#compute log returns of spot rates
difflog <- function(x){
  diff(log(x))}
spot_rates_merged_returns <- as.data.frame(sapply(spot_rates_merged[,2:ncol(spot_rates_merged)], difflog))
spot_rates_merged_returns$dates <- spot_rates_merged[2:nrow(spot_rates_merged), "dates"]

#split independent variables by time frequency
daily_independent_vars <- independent_vars[,c(1:32,35:36,39:48,57:64)]
weekly_independent_vars <- independent_vars[,c(37:38,69:70)]
monthly_independent_vars <- independent_vars[,c(49:56,65:68)]

#Replace Year Column of Geopolitical Risk with Date Column & Drop Months as Redundant
monthly_independent_vars[!is.na(monthly_independent_vars[,9]), 9] <- rev(seq(as.Date("1997-01-01"), as.Date("2021-01-01"), by = "months"))
monthly_independent_vars$Year...65 = sort(as.Date(monthly_independent_vars$Year...65), na.last = T)
monthly_independent_vars <- monthly_independent_vars[, c(1:9, 11)]

#Merge all daily independent variables into one DataFrame with uniform dates
daily_independent_vars_merged <- datacleanup(daily_independent_vars)
weekly_independent_vars_merged <- datacleanup(weekly_independent_vars)
monthly_independent_vars_merged <- datacleanup(monthly_independent_vars)
monthly_independent_vars_merged <- monthly_independent_vars_merged[order(monthly_independent_vars_merged$dates),]

#monthly_independent_vars <- datacleanup(monthly_independent_vars)
rm(list=setdiff(ls(),c('daily_independent_vars_merged', 'independent_vars','spot_rates_merged_returns', "spot_rates_merged",
                       'stable_coins', "datacleanup", "weekly_independent_vars_merged", "monthly_independent_vars_merged", "bid_ask")))

# Convert to numeric
daily_independent_vars_merged[,2:ncol(daily_independent_vars_merged)] <- sapply(daily_independent_vars_merged[,2:ncol(daily_independent_vars_merged)], as.numeric)
daily_independent_vars_merged <- select(daily_independent_vars_merged, -25)

#removes rows where NAs across all columns (no date etc.) 
daily_independent_vars_merged <- daily_independent_vars_merged[rowSums(is.na(daily_independent_vars_merged)) != ncol(daily_independent_vars_merged),]
weekly_independent_vars_merged = weekly_independent_vars_merged[apply(weekly_independent_vars_merged,1,function(x)any(!is.na(x))),]
monthly_independent_vars_merged <- monthly_independent_vars_merged[rowSums(is.na(monthly_independent_vars_merged)) != ncol(monthly_independent_vars_merged),]


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


# Data Exploration --------------------------------------------------------

#summary statistics
summary_stats <- summary(spot_rates_merged)

#histogram

#qq-plots


# Finite Gaussian Mixture  ------------------------------------------------

