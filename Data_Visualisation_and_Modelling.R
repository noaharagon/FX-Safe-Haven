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

#reg mixture for CHF/USD with all daily variables beginning 2000
CHFUSD_reg_2000 <- regmixEM(y = spot_rates_returns[1:5479, "CHF.USD"], x = as.matrix(cbind(daily_independent_returns[1:5479, c(2, 3, 4, 5, 6, 7)], bid_ask[1:5479,2])), k = 2)

#reg mixture for CHF/EUR with all daily variables beginning 2000
CHFEUR_reg_2000 <- regmixEM(y = spot_rates_returns[1:5479, "CHF.EUR"], x = as.matrix(cbind(daily_independent_returns[1:5479, c(2, 3, 4, 5, 6, 7)], bid_ask[1:5479,3])), k = 2)

#reg mixture for CHF/GBP with all daily variables beginning 2000
CHFGBP_reg_2000 <- regmixEM(y = spot_rates_returns[1:5479, "CHF.GBP"], x = as.matrix(cbind(daily_independent_returns[1:5479, c(2, 3, 4, 5, 6, 7)], bid_ask[1:5479,4])), k = 2)

#reg mixture for USD/INR
USDINR_reg_2000 <- regmixEM(y = spot_rates_returns[1:5479, "IDR.USD"], x = as.matrix(daily_independent_returns[1:5479, c(2, 3, 4, 5, 6, 7)]), k = 2)

#plotting classification into "business as usual" and crisis
mixture_plots <- list("CHF.USD", "CHF.GBP", "CHF.EUR")
mix_plots_list = list()
for (i in mixture_plots) {
  assign(paste0(i, "_mix"), normalmixEM(spot_rates_returns[1:5479, i], k = 2))
  assign(paste0(i, "_mix_df"), spot_rates_returns[,c(i, "dates")])
  toAssign <- paste0(i, "_mix_df", "[,'component']")
  str1 <- paste0(toAssign, "<-", "ifelse(eval(parse(text = paste0(i, '_mix', '$posterior[, 1]')))<0.5, '2', '1')")
  eval(parse(text=str1))
  mix_plot = ggplot(data = eval(parse(text = paste0(i, "_mix_df"))), aes(x = dates, y = eval(parse(text = paste0(i, "_mix_df[,1]"))))) +
    geom_point(color = factor(eval(parse(text = paste0(i, "_mix_df[,3]"))))) +theme_economist_white()+
    ggtitle("Business as Usual vs. Crisis") + ylab("Spot Returns") + xlab("Date")
  mix_plots_list[[i]] = mix_plot
  ggsave(mix_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}