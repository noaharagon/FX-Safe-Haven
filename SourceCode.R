#Jonas Schmitten & Noah Angara
#Economics in Practice
#March 2021

#packages
library('readxl')

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Economics in Practice", "C://user/Wilma", "C://Some/other/path")
names(Paths) = c("jonasschmitten", "Wilma", "Guest")
setwd(Paths[Sys.info()[7]])


# Data Cleaning -----------------------------------------------------------

#load data
spot_rates = read_excel("FXData.xlsx", sheet='SpotRates')
stable_coins = read_excel("FXData.xlsx", sheet='StableCoins')

independent_vars = read_excel("FXData.xlsx", sheet='IndependentVars')
spreads = read_excel("FXData.xlsx", sheet='Spreads')
