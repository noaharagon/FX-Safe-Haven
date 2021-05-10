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
library("alphastable")
library("midasr")
library("data.table")

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice/Clean Data", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

#Data 
bid_ask = read.csv('Bid_Ask_Clean.csv')
spot_rates = read.csv('Spot_Rates_Clean.csv')
spot_rates$dates <- as.Date(spot_rates$dates)
spot_rates_returns = read.csv('Spot_Rates_Returns_Clean.csv')
spot_rates_returns$dates <- as.Date(spot_rates_returns$dates)
daily_independent_returns = read.csv('Daily_Independent_Returns_Clean.csv')

# Data Visualization ------------------------------------------------------

#setting new working directory for graphs
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice/Graphs", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

# US Recession Start and End as per FRED St. Louis
recessions.df = read.table(textConnection(
  "Peak, Trough, Type
1973-11-01, 1975-03-01, Recession
1980-01-01, 1980-07-01, Recession
1981-07-01, 1982-11-01, Recession
1990-07-01, 1991-03-01, Recession
2001-03-01, 2001-11-01, Recession
2007-12-01, 2009-06-01, Recession
2009-10-01, 2013-05-01, Euro-Crisis"), sep=',', colClasses=c('Date', 'Date', 'character'), header=TRUE)
recessions.trim = subset(recessions.df, Peak >= "2000-03-17")
rm(recessions.df)

# Loop to Create Plots of Spot Rates With Recession Shading
plot_list = list()
for (i in colnames(spot_rates[2:ncol(spot_rates)])) {
  z = zoo(spot_rates[, i], order.by = spot_rates$dates)
  g = ggplot(fortify(z,melt=T)) +
    geom_line(aes(x=Index,y=Value))+ 
    geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf, color = recessions.trim$Type, fill=recessions.trim$Type), alpha=0.2)+
    labs(y = "Spot Rate", x= "", color = "", fill = "") + theme(legend.position = "bottom") +
    theme_economist_white(gray_bg = F)
  plot_list[[i]] = g
  ggsave(g, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}
rm(i)
rm(z)


# Data Exploration --------------------------------------------------------

#summary statistics for spot returns
summary_spot <- summarise_all(spot_rates_returns[,c("CHF.USD", "CHF.GBP", "CHF.EUR", "CHF.JPY")],
                               funs(min, mean, max, count = length))
summary_spot_clean <- data.frame(min = c(summary_spot[1,1], summary_spot[1,2], summary_spot[1,3], summary_spot[1,4]),
                            mean = c(summary_spot[1,5], summary_spot[1,6], summary_spot[1,7], summary_spot[1,8]),
                            max = c(summary_spot[1,9], summary_spot[1,10], summary_spot[1,11], summary_spot[1,12]),
                            count = c(summary_spot[1,13], summary_spot[1,14], summary_spot[1,15], summary_spot[1,16]))
rownames(summary_spot_clean) <- c("CHF.USD", "CHF.GBP", "CHF.EUR", "CHF.JPY")
stargazer(summary_spot_clean, summary = F)

#summary statistics for daily independent variables
summary_independent <- summarise_all(daily_independent_returns[,c("PUT.CALL", "JPM_GLOBAL_FX_VOLA", "US_3M", "SPY")],
                                     funs(min, mean, max, count = length))
summary_independent_clean <- data.frame(min = c(summary_independent[1,1], summary_independent[1,2], summary_independent[1,3], summary_independent[1,4]),
                                 mean = c(summary_independent[1,5], summary_independent[1,6], summary_independent[1,7], summary_independent[1,8]),
                                 max = c(summary_independent[1,9], summary_independent[1,10], summary_independent[1,11], summary_independent[1,12]),
                                 count = c(summary_independent[1,13], summary_independent[1,14], summary_independent[1,15], summary_independent[1,16]))
rownames(summary_independent_clean) <- c("PUT.CALL", "JPM_VOLA", "US_3M", "SPY")
stargazer(summary_independent_clean, summary = F)


#histogram of FX returns
hist_plots = list()
for (i in colnames(spot_rates_returns[,2:ncol(spot_rates_returns)])) {
  n = ggplot(spot_rates_returns, aes(x=spot_rates_returns[,i])) +
  geom_histogram(color="black", fill="red", alpha = 0.5, bins = 100) +
    geom_vline(xintercept = mean(spot_rates_returns[,i])) + theme_economist_white(gray_bg = F) + xlab("")
  hist_plots[[i]] = n
  ggsave(n, file=paste0("hist_", i,".png"), width = 14, height = 10, units = "cm")
}

#qq-plots of FX returns
qq_plots = list()
for (i in colnames(spot_rates_returns[,2:ncol(spot_rates_returns)])) {
  j = ggplot(spot_rates_returns, aes(sample=spot_rates_returns[,i])) +
  stat_qq(color = "red", alpha = 0.5) + stat_qq_line() + theme_economist_white(gray_bg = F)
  qq_plots[[i]] <- j
  ggsave(j, file=paste0("qq_", i,".png"), width = 14, height = 10, units = "cm")
}

#plotting classification into "business as usual" and crisis
significant_dates <- read.table(textConnection(
  "Date, Event
2001-09-11, Terrorist Attack
2004-03-11, Terrorist Attack
2005-07-07, Terrorist Attack
2010-05-06, Flash Crash
2013-04-23, Flash Crash
2015-01-15, SNB Floor
2015-11-13, Terrorist Attack
2017-05-22, Terrorist Attack"), sep=',', colClasses = c("Date", "character"), header=TRUE)

mixture_plots <- list("CHF.USD", "CHF.GBP", "CHF.EUR")
mix_plots_list = list()
for (i in mixture_plots) {
  assign(paste0(i, "_mix"), normalmixEM(spot_rates_returns[1:5479, i], k = 2))
  assign(paste0(i, "_mix_df"), spot_rates_returns[,c(i, "dates")])
  toAssign <- paste0(i, "_mix_df", "[,'component']")
  str1 <- paste0(toAssign, "<-", "ifelse(eval(parse(text = paste0(i, '_mix', '$posterior[, 1]')))<0.5, '2', '1')")
  eval(parse(text=str1))
  mix_plot = ggplot(data = eval(parse(text = paste0(i, "_mix_df"))), aes(x = dates, y = eval(parse(text = paste0(i, "_mix_df[,1]"))))) +
    geom_point(color = factor(eval(parse(text = paste0(i, "_mix_df[,3]")))), size = 1)+ geom_vline(data = significant_dates, size = 1,alpha = 0.5, aes(xintercept = significant_dates$Date, color = significant_dates$Event)) +theme_economist_white(gray_bg = F)+
    ggtitle(" ") + labs(y = "Spot Rate Return", x= "", color = "") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) + scale_fill_economist()
  mix_plots_list[[i]] = mix_plot
  ggsave(mix_plot, file=paste0("mixplot_", i,".png"), width = 14, height = 10, units = "cm")
}

# Finite Gaussian Mixture  ------------------------------------------------

#state dependent regression models
for (i in c("CHF.EUR", "CHF.USD", "CHF.GBP", "CHF.JPY", "CHF.NOK", "CHF.INR", "CHF.BRL", "JPY.USD", "BRL.USD", "INR.USD")){
  #split data into regimes to run separate regressions
  normalmix = normalmixEM(spot_rates_returns[1:5479, i])
  segmented = as.data.frame(spot_rates_returns[1:5479, c(i, "dates")])
  segmented$component = ifelse(normalmix$posterior[,1]<0.5, "1", "2")
  segmented$matching_col = as.Date(ifelse(segmented$component== "1", segmented$dates, 0))
  
  reg1 = segmented[segmented$component == "1",]
  reg2 = segmented[segmented$component == "2",]
  independent_comp1 = daily_independent_returns[which(segmented$matching_col == head(daily_independent_returns$dates,5479)),]
  independent_comp2 = daily_independent_returns[which(segmented$matching_col != head(daily_independent_returns$dates,5479)),]
  
  if (i %in% c("CHF.EUR", "CHF.USD", "CHF.GBP", "CHF.JPY", "CHF.NOK", "CHF.INR", "CHF.BRL")){
    #add bid_ask to independent variables
    independent_comp1$Bid_ask = bid_ask[which(segmented$matching_col == bid_ask$Date), paste0("X.",i)]
    independent_comp2$Bid_ask = bid_ask[which(segmented$matching_col != bid_ask$Date), paste0("X.",i)]
  
  #run regressions on each data set
    assign(paste0(i,"reg_model1_2000"), lm(formula = reg1[,i] ~ MSCI  + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y + Bid_ask,  data = independent_comp1))
    assign(paste0(i, 'reg_model2_2000'), lm(formula = reg2[,i] ~ MSCI + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y + Bid_ask, data = independent_comp2))
  } else {
    assign(paste0(i,"reg_model1_2000"), lm(formula = reg1[,i] ~ MSCI  + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y ,  data = independent_comp1))
    assign(paste0(i, 'reg_model2_2000'), lm(formula = reg2[,i] ~ MSCI + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y , data = independent_comp2))
    } 
    
  rm(normalmix, segmented, independent_comp1, independent_comp2)
}

library(quantmod)
getSymbols( "TEDRATE" , src = "FRED", from = "2000-03-20" , to = "2021-03-18" , adjust = TRUE)


#state dependent regression models
for (i in c("CHF.EUR", "CHF.USD", "CHF.GBP", "CHF.JPY", "CHF.NOK", "CHF.INR", "CHF.BRL", "JPY.USD", "BRL.USD", "INR.USD")){
  #split data into regimes to run separate regressions
  normalmix = normalmixEM(spot_rates_returns[which(grepl("2006-07-19", spot_rates_returns$dates)):5479, i])
  segmented = as.data.frame(spot_rates_returns[which(grepl("2006-07-19", spot_rates_returns$dates)):5479, c(i, "dates")])
  segmented$component = ifelse(normalmix$posterior[,1]<0.5, "1", "2")
  segmented$matching_col = as.Date(ifelse(segmented$component== "1", segmented$dates, 0))
  
  reg1 = segmented[segmented$component == "1",]
  reg2 = segmented[segmented$component == "2",]
  independent_comp1 = daily_independent_returns [which(segmented$matching_col == daily_independent_returns$dates[which(grepl("2006-07-19", spot_rates_returns$dates)):5479]),]
  independent_comp2 = daily_independent_returns[which(segmented$matching_col != daily_independent_returns$dates[which(grepl("2006-07-19", spot_rates_returns$dates)):5479]),]
  bid_ask_2006 = bid_ask[which(grepl("2006-07-19", spot_rates_returns$dates)):5479,]
  
  if (i %in% c("CHF.EUR", "CHF.USD", "CHF.GBP", "CHF.JPY", "CHF.NOK", "CHF.INR", "CHF.BRL")){
    #add bid_ask to independent variables
    independent_comp1$Bid_ask = bid_ask_2006[which(segmented$matching_col == bid_ask_2006$Date), paste0("X.",i)]
    independent_comp2$Bid_ask = bid_ask_2006[which(segmented$matching_col != bid_ask_2006$Date), paste0("X.",i)]  
    colnames(daily_independent_returns)
    #run regressions on each data set
    assign(paste0(i,"reg_model1_2006"), lm(formula = reg1[,i] ~ MSCI  + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y + Bid_ask + TED_SPREAD  + GS_COMMODITY_VOLA + CDX_EUROPE_IG_10Y,  data = independent_comp1))
    assign(paste0(i, 'reg_model2_2006'), lm(formula = reg2[,i] ~ MSCI + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y + Bid_ask + TED_SPREAD  + GS_COMMODITY_VOLA + CDX_EUROPE_IG_10Y, data = independent_comp2))
    } else {
    assign(paste0(i,"reg_model1_2006"), lm(formula = reg1[,i] ~ MSCI  + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y + TED_SPREAD  + GS_COMMODITY_VOLA + CDX_EUROPE_IG_10Y,  data = independent_comp1))
    assign(paste0(i, 'reg_model2_2006'), lm(formula = reg2[,i] ~ MSCI + PUT.CALL  + MOVE_3M + VIX + VSTOXX + GOLD +JPM_GLOBAL_FX_VOLA + X10Y_BREAKEVEN + BARC_US_CORP_HY_10Y + TED_SPREAD  + GS_COMMODITY_VOLA + CDX_EUROPE_IG_10Y, data = independent_comp2))
    }
    rm(normalmix, segmented, independent_comp1, independent_comp2)
    }

#LaTeX Table Preparation
CHFEUR_LaTeX_Table = rbind(CHFEUR_reg_2000$lambda,CHFEUR_reg_2000$sigma, CHFEUR_reg_2000$beta)
rownames(CHFEUR_LaTeX_Table) = c('Lambda', 'Sigma', 'Intercept',colnames(daily_independent_returns[c(2:7,9:11,13,14,16,17,20,23,25)]),'Bid-Ask')
stargazer(CHFEUR_LaTeX_Table)
