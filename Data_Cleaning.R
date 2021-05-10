#Jonas Schmitten & Noah Angara
#Economics in Practice
#March 2021

#packages
library("readxl")
library("dplyr")
library("purrr")
library("stringr")
library("xts")
library("zoo")
library("tibble")
library("quantmod")

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice/Raw Data", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

# Data Cleaning -----------------------------------------------------------

#Load raw data
spot_rates <- read_excel("FXData.xlsx", sheet='SpotRates')
stable_coins <- read_excel("FXData.xlsx", sheet='StableCoins')
independent <- read_excel("FXData.xlsx", sheet='IndependentVars')
spreads <- read_excel("FXData.xlsx", sheet='Spreads')

#Load TED Spread
getSymbols( "TEDRATE" , src = "FRED", adjust = TRUE)
TEDRATE = TEDRATE["2000-03-17/2021-03-19"]

#Create DataFrame
spot_rates <- as.data.frame(spot_rates)
independent <- as.data.frame(independent)

#Dates and Rearranging
spot_rates[,seq(1, ncol(spot_rates), 2)] <- lapply(spot_rates[,seq(1, ncol(spot_rates), 2)], as.Date, format = "%y-%m-%d")

#reverse order of some columns to make chronologically
spot_rates[,23:ncol(spot_rates)] <- spot_rates[nrow(spot_rates):1, 23:ncol(spot_rates)]
independent[,c(35:64,69:70)] <- independent[nrow(independent):1,c(35:64,69:70)]

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

spot_rates <- datacleanup(spot_rates)

rm(list=setdiff(ls(), c("spot_rates",'spreads','stable_coins','independent', "datacleanup", "TEDRATE")))

#Crossrates
for (i in c("JAPANESE YEN TO US $ (WMR) - EXCHANGE RATE",'NORWEGIAN KRONE TO US $ (WMR) - EXCHANGE RATE',
            'BRAZILIAN REAL TO US $ (WMR) - EXCHANGE RATE',"INDIAN RUPEE TO US $ (WMR) - EXCHANGE RATE" )) { 
  spot_rates[paste(str_split(i, "US",simplify = T)[1], "CHF (WMR) - EXCHANGE RATE", sep="")][1:which(grepl("2003-07-14", spot_rates$dates)),] = 
    (1/spot_rates["SWISS FRANC TO US $ (WMR) - EXCHANGE RATE"][1:which(grepl("2003-07-14", spot_rates$dates)),])*
    (spot_rates[i][1:which(grepl("2003-07-14", spot_rates$dates)),])}
rm(i)
spot_rates[,2:ncol(spot_rates)] = sapply(spot_rates[,2:ncol(spot_rates)], as.numeric)

#fill in missing values with previous value
#spot_rates <- na.locf(spot_rates)

spreads = spreads[,1:29]

#Calculating Bid-Ask Spreads
spreads <- select(spreads, -5) #remove extra date column
spreads <- datacleanup(spreads)
date_purposes <- spreads[1:5539,1]
date_purposes <- as.Date(date_purposes$dates, format = "%Y-%m-%d")
spreads <- spreads %>% select(-contains("Date"))
spreads <- spreads %>% relocate("Ask Price CHF/INR", .after = "Bid Price CHF/INR") #reorder to calculate spreads
bid_ask <- spreads[, seq(2, ncol(spreads), 2)] - spreads[, seq(1, ncol(spreads), 2)]
colnames(bid_ask)<-gsub("Ask Price","", colnames(bid_ask))#give nicer column names
bid_ask <- bid_ask[rowSums(is.na(bid_ask)) != ncol(bid_ask),] #deal with NA's
bid_ask$Date <- date_purposes #add date column back to df
bid_ask <- bid_ask %>% relocate("Date", .before = colnames(bid_ask)[1]) #put date column first

bid_ask = bid_ask[5485:6,] #reverse order to make it chronological as other variables
bid_ask = na.locf(bid_ask)

bid_ask = bid_ask[2:nrow(bid_ask),]
row.names(bid_ask) <- NULL
colnames(bid_ask)[3] <- "X.CHF.EUR"
colnames(bid_ask)[5] <- "X.CHF.JPY"

#compute log returns of spot rates
#difflog <- function(x){
#  diff(log(x))}

diffsimple <- function(x){
  diff(x)/(x[-length(x)])}

#Take inverse to make CHF rates consistent
spot_rates[c(5,7,9,11)] = 1/spot_rates[c(5,7,9,11)]
#FX CHF/USD meaning how many CHF you can get for 1 USD, up means depreciate CHF (appreciate USD), down means appreciate CHF (depreciate USD)
names(spot_rates) = c('dates', 'CHF/USD', 'CHF/GBP', 'CHF/EUR', 'CHF/JPY', 'JPY/USD', 'CHF/NOK', 'NOK/USD', 'CHF/BRL', 'BRL/USD', 'CHF/INR', 'INR/USD')

spot_rates_returns <- as.data.frame(sapply(spot_rates[,2:ncol(spot_rates)], diffsimple))
spot_rates_returns = add_column(spot_rates_returns, dates = spot_rates[2:nrow(spot_rates), "dates"] ,.before = 1)

#split independent variables by time frequency
daily_independent <- independent[,c(1:32,35:36,39:48,57:64)]
weekly_independent <- independent[,c(37:38)]
monthly_independent <- independent[,c(49:56,65:70)]

#Replace Year Column of Geopolitical Risk with Date Column & Drop Months as Redundant & SNB Date Column
monthly_independent[!is.na(monthly_independent[,9]), 9] <- rev(seq(as.Date("1997-01-01"), as.Date("2021-01-01"),by="months")-1)
monthly_independent[!is.na(monthly_independent[,7]), 7] <-(seq(as.Date("2003-05-01"), as.Date("2021-04-01"),by="months")-1)
monthly_independent[!is.na(monthly_independent[,13]), 13] <-(seq(as.Date("2000-02-01"), as.Date("2021-03-01"),by="months")-1)
monthly_independent$Year...65 = sort(as.Date(monthly_independent$Year...65), na.last = T)
monthly_independent <- monthly_independent[, c(1:9, 11, 13:14)]

#Merge all daily independent variables into one DataFrame with uniform dates
daily_independent <- datacleanup(daily_independent)
daily_independent <- daily_independent[order(daily_independent$dates),]
colnames(weekly_independent)[1] <- "dates"
monthly_independent <- datacleanup(monthly_independent)
monthly_independent <- monthly_independent[order(monthly_independent$dates),]

#Remove Fund Flows
monthly_independent <- select(monthly_independent, -5) 

#monthly_independent <- datacleanup(monthly_independent)
rm(list=setdiff(ls(),c('daily_independent', 'independent','spot_rates_returns', "spot_rates",
                       'stable_coins', "datacleanup", "weekly_independent", "monthly_independent", "bid_ask", "diffsimple", "TEDRATE")))

# Convert to numeric and Remove CDX & Last Price
daily_independent[,2:ncol(daily_independent)] <- sapply(daily_independent[,2:ncol(daily_independent)], as.numeric)
daily_independent <- select(daily_independent, -25)
daily_independent <- select(daily_independent, -24)

#Trim Barclay's US Corp High Yield pre 2000-03-17
daily_independent <- daily_independent[3384:nrow(daily_independent), ]

#removes rows where NAs across all columns (no date etc.) 
daily_independent <- daily_independent[rowSums(is.na(daily_independent)) != ncol(daily_independent),]
weekly_independent = weekly_independent[apply(weekly_independent,1,function(x)any(!is.na(x))),]
monthly_independent <- monthly_independent[rowSums(is.na(monthly_independent)) != ncol(monthly_independent),]

#Check maximum NAs in sequence, -INF means no more than 1 NA in a row 
for (i in colnames(daily_independent)){
  x = rle(is.na(daily_independent[,i]))
  print(max(x$lengths[x$values][x$lengths[x$values] != max(x$lengths[x$values])]))}
rm(x,i)

daily_independent = na.locf(daily_independent, na.rm = F)

#replace ted spread with full time series since 2000
daily_independent$`TED SPREAD RATE - MIDDLE RATE` = TEDRATE$TEDRATE

#Create return datasets from independent variables
daily_independent_returns = as.data.frame(sapply(daily_independent[2:ncol(daily_independent)], diffsimple))
daily_independent_returns = add_column(daily_independent_returns, dates = daily_independent[2:nrow(daily_independent), "dates"] ,.before = 1)
daily_independent_returns[is.na(daily_independent_returns$`TED SPREAD RATE - MIDDLE RATE`), "TED SPREAD RATE - MIDDLE RATE"] = 0
names(daily_independent_returns)[2:ncol(daily_independent_returns)] = c('MSCI', 'SPY', 'US_2Y', 'US_10Y', 'US_3M', 'DE_10Y','DE_3M', 'DE_2Y', 'PUT/CALL', 'VIX','TED_SPREAD', 'MOVE_1M', 
                                    'MOVE_3M', 'MOVE_6M', 'VSTOXX', 'GOLD', 'CITI_ECONOMIC_SURPRISE', 'GS_COMMODITY_VOLA', 'JPM_GLOBAL_FX_VOLA', 'JPM_EM_FX_VOLA_1M',
                                    'JPM_G10_FX_VOLA_1M', '10Y_BREAKEVEN', 'CDX_EUROPE_IG_10Y', 'BARC_US_CORP_HY_10Y')
names(daily_independent)[2:ncol(daily_independent)] = c('MSCI', 'SPY', 'US_2Y', 'US_10Y', 'US_3M', 'DE_10Y','DE_3M', 'DE_2Y', 'PUT/CALL', 'VIX','TED_SPREAD', 'MOVE_1M', 
                                                                'MOVE_3M', 'MOVE_6M', 'VSTOXX', 'GOLD', 'CITI_ECONOMIC_SURPRISE', 'GS_COMMODITY_VOLA', 'JPM_GLOBAL_FX_VOLA', 'JPM_EM_FX_VOLA_1M',
                                                                'JPM_G10_FX_VOLA_1M', '10Y_BREAKEVEN', 'CDX_EUROPE_IG_10Y', 'BARC_US_CORP_HY_10Y')

weekly_independent_returns = as.data.frame(sapply(weekly_independent, diffsimple))
weekly_independent_returns$dates = weekly_independent[2:nrow(weekly_independent), "dates"]
names(weekly_independent_returns) = c('dates', 'JPM_GLOBAL_FORECAST_REVISION_INDEX')

#Adjust time frame to coincide with spot rates and daily independent variables
monthly_independent = monthly_independent[which(grepl("2000-03-31", monthly_independent$dates)):nrow(monthly_independent),]
monthly_independent_returns = as.data.frame(sapply(monthly_independent[2:ncol(monthly_independent)], diffsimple))
monthly_independent_returns = add_column(monthly_independent_returns, dates = monthly_independent[2:nrow(monthly_independent), "dates"] ,.before = 1)

names(monthly_independent_returns) = c('dates', 'IIF_EM_FLOWS', 'IIF_EM_DEBT_FLOWS', 'IIF_EM_EQUITY_FLOWS', 'GEOPOLITICAL_RISK', 'SNB_SIGHT_DEPOSITS')

#Change path here for new folder where clean data is saved
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice/Clean Data", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

write.csv(bid_ask, 'Bid_Ask_Clean.csv', row.names = FALSE)
write.csv(spot_rates, 'Spot_Rates_Clean.csv',row.names = FALSE)
write.csv(spot_rates_returns, 'Spot_Rates_Returns_Clean.csv', row.names = FALSE)
write.csv(daily_independent_returns, 'Daily_Independent_Returns_Clean.csv',row.names = FALSE)
write.csv(daily_independent, "Daily_Independent.csv", row.names = FALSE)
write.csv(weekly_independent_returns, 'Weekly_Independent_Returns_Clean.csv',row.names = FALSE)
write.csv(monthly_independent_returns, 'Monthly_Independent_Returns_Clean.csv', row.names = FALSE)



