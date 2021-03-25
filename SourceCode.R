#Jonas Schmitten & Noah Angara
#Economics in Practice
#March 2021

#packages


#setting working directory
Paths = c("C://user/Fred/", "C://user/Wilma", "C://Some/other/path")
names(Paths) = c("Fred", "Wilma", "Guest")
setwd(Paths[Sys.info()[7]])


# Data Cleaning -----------------------------------------------------------

#load data
