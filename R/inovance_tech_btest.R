#https://www.inovancetech.com/backtesttutorialr.html
library(RCurl)
library(quantmod)
#Install the libraries we need 

#Download Michael Kapler's “Systematic Investor Toolbox”, a powerful set of
# tools used to backtest and evaluate quantitative trading strategies
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 
                    binary=TRUE, 
                    followlocation = TRUE, 
                    ssl.verifypeer = FALSE)

con = gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)


#Create a new environment 
data <- new.env()

tickers <- spl('XBTEUR')
folder_root <- "/media/deckard/External/data/kraken"
dp_dir <- file.path(folder_root, in_curr, "data_products") 

system.time(four_hour <- readRDS(file.path(dp_dir, 
                                           "XBTEUR_OHLC_four_hour_xts.rds")))
names(four_hour) <- c("Open", "High", "Low", "Close", "Volume")

data <- four_hour

#Load and clean the data 
bt.prep(data, align='remove.na')

#Specify the prices and store our models 
prices = data$Close
models = list()

#Create our baseline “Buy and Hold” strategy 
data$weight = NA
data$weight = 1
models$buy.hold = bt.run.share(data, clean.signal=TRUE)

#Calculate the indicators we need for our strategy 
CCI20<-CCI(Close,20)
RSI3<-RSI(Close,3)
DEMA10<-DEMA(Close,n = 10, v = 1, wilder = FALSE)
DEMA10c<-Close - DEMA10
DEMA10c<-DEMA10c/.0001
