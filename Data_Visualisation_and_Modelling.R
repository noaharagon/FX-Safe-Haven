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
daily_independent_returns = read.csv('Daily_Independent_Returns_Clean.csv')

# Data Visualization ------------------------------------------------------

#setting new working directory for graphs
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice/Graphs", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

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
for (i in colnames(spot_rates[2:ncol(spot_rates)])) {
  z = zoo(spot_rates[, i], order.by = spot_rates$dates)
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
rm(i)
rm(z)


# Data Exploration --------------------------------------------------------

#summary statistics
summary_stats <- summary(spot_rates)

#histogram of FX returns
for (i in colnames(spot_rates_returns[,2:ncol(spot_rates_returns)])) {
  hist(spot_rates_returns[,i], breaks = 200, main = i)
}

#qq-plots to justify selection of distribution
for (i in colnames(spot_rates_returns[,2:ncol(spot_rates_returns)])) {
  qqnorm(spot_rates_returns[,i], pch = 1, frame = FALSE, main = i)
  qqline(spot_rates_returns[,i], col = "steelblue", lwd = 2)
}


# Finite Gaussian Mixture  ------------------------------------------------

CHF_reg <- regmixEM(y = spot_rates_returns[1:5479, 2], x = as.matrix(cbind(daily_independent_returns[1:5479, c(2, 3, 5, 6, 7)], bid_ask[1:5479,2])), k = 2)

#for CHF/EURO take German yields instead of treasury?
EUR_reg <- regmixEM(y = spot_rates_returns[1:5479, 3], x = as.matrix(cbind(daily_independent_returns[1:5479, c(2, 3, 4, 5, 6, 7)], bid_ask[1:5479,3])), k = 2)


#plotting classification into "business as usual" and crisis
EUR_mix <- normalmixEM(spot_rates_returns[1:5479, 3], k = 2)    
EUR_mix_df <- spot_rates_returns[,c(3,12)]
#if posterior of component 1 < 0.5 then component 2
EUR_mix_df$component <- ifelse(EUR_mix$posterior[,1]<0.5, "2", "1")   
ggplot(EUR_mix_df, aes(x = dates, y = EUR_mix_df[,1])) + 
  geom_point(aes(colour = factor(component))) + theme_economist_white() + ggtitle("CHF/EUR: Business as Usual vs. Crisis") +
  ylab("Spot Returns") + xlab("Date") 
rm(EUR_mix_df)



