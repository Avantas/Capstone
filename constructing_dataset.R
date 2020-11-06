
# if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") 


# loading the packages
library(tidyverse)
library(caret)
library(data.table)
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

#Read the html webpage
coinmarket <- read_html("https://coinmarketcap.com/")
# And continuosly we save the date
date <- now()

class(coinmarket)

# I identify the CCS selector for take the Name of coin, price, and volume
market <- html_nodes(coinmarket, ".iTmTiC , .kqPMfR , td .hVAibX , .price___3rj7O .cmc-link , .coin-item-symbol")

length(market)

#we have the first 100 coin

market_text <- html_text(market)
market_text <- market_text %>% str_replace_all("[$]", "") #we take out the symbol "$"
market_text <- market_text %>% str_replace_all(",", "") # we take out the ","
market_text <- market_text %>% str_replace_all("\\s[A-Z]*", "") # we take out the name of V6
market_text

market_data <- matrix(market_text, 100, 6, byrow = TRUE)
market_data <- as.tibble(market_data)

#we add the column date
market_data <- market_data %>% mutate(date = date)
market_data


#then we can transform the class of the variables
market_data$V3 <- as.numeric(market_data$V3)
market_data$V4 <- as.numeric(market_data$V4)       
market_data$V5 <- as.numeric(market_data$V5)
market_data$V6 <- as.numeric(market_data$V6)

#Finally we change the column names
colnames(market_data) <- c("Name", "Key", "Price", "Market_Cap", "Vol24hs", "Circulating", "Date")

market_data



#and we add a new rows from the diferent time
market_data1 <- full_join(market_data, market_data1)
market_data1


############################ TIMER

Sys.sleep(60)

##############################################################################

#if we pretend obtain data of a week, we need to calculate the time for sleep and obtain at the finish 1.000.000 millons of data rows

1000000/300 #we obtain the steps "t" to repeat
1000000/300/7/24/60 #for 7 days to the week and 24 hs and 60 minutes, we need to set the sleep function in 33 seconds.


#We made the function ########################################################

datasetmakingfor <- function(m, s){
  i <- 1
  for(i in 1:m){
    Sys.sleep(s)
    coinmarket <- read_html("https://coinmarketcap.com/")
    date <- now()
    market <- html_nodes(coinmarket, ".iTmTiC , .kqPMfR , td .hVAibX , .price___3rj7O .cmc-link , .coin-item-symbol")
    market_text <- html_text(market)
    market_text <- market_text %>% str_replace_all("[$]", "") #we take out the symbol "$"
    market_text <- market_text %>% str_replace_all(",", "") # we take out the ","
    market_text <- market_text %>% str_replace_all("\\s[A-Z]*", "") # we take out the name of V6
    market_data <- matrix(market_text, 100, 6, byrow = TRUE)
    market_data <- as.tibble(market_data)
    market_data <- market_data %>% mutate(date = date)
    market_data$V3 <- as.numeric(market_data$V3)
    market_data$V4 <- as.numeric(market_data$V4)       
    market_data$V5 <- as.numeric(market_data$V5)
    market_data$V6 <- as.numeric(market_data$V6)
    colnames(market_data) <- c("Name", "Key", "Price", "Market_Cap", "Vol24hs", "Circulating", "Date")
    if(i == 1){market_data1 <- market_data}
    else market_data1 <- full_join(market_data, market_data1)
    i+1
    print(i/m)}
  market_data1
}


finaldataset <- datasetmakingfor(4, 33) #1000 step, sleep for 33 seconds
nrow(finaldataset)
finaldataset

###############################################analyse the data

finaldataset %>% group_by(Key) %>% filter(Vol24hs >= 10^9) %>% # we filter for Crypto who market cap is more than 100 millons
  ggplot(aes(Key, Price)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_y_log10() +
  xlab("Crypto Coin") + 
  ylab("Market price (Log 10 scale)") +
  ggtitle("Variation in prices in the dataset period")

finaldataset %>% filter(Vol24hs >= 10^9)  %>% group_by(Key) %>% summarize(dif = Price-mean(Price)) %>% arrange(desc(dif))

head(finaldataset)

# we observe the price fluctuations between diferents coins over the date 
finaldataset %>% group_by(Key) %>% filter(Vol24hs >= 10^9) %>%
  ggplot(aes(Date, Price, color = Key)) + geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Price of Coin")

#for observe the weight of the prices moviment, we create a relation between price and volumen
finaldataset %>% mutate(relation_vol_price = Vol24hs/Price) %>% 
  group_by(Key) %>% filter(Vol24hs >= 10^9) %>% 
  ggplot(aes(Date, relation_vol_price, color = Key)) + geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Price of Coin")
# In the visutalization, is posible to observe when all prices go up (by buy operations) or
# when some trade are moving between cryptocoins, the main usefull thing is to detect negative correlations
# In automatical small operations

#we tray again with all cryptocoins (we take out the filter and set for better visualization)
finaldataset %>% mutate(relation_vol_price = Vol24hs/Price) %>% 
  group_by(Key) %>% 
  ggplot(aes(Date, relation_vol_price, color = Key)) + 
  geom_line(show.legend = TRUE, position = "stack", size = 1) + 
  scale_y_log10() +
  xlab("Date") + 
  ylab("Relation Volumen - Price of Coin") +
  ggtitle("Relation prices-volumen in the time")

#we can see two big groups of coins moving together


#we tray again but we try to filter for sd
finaldataset %>% mutate(sd = sd(Vol24hs/Price)) %>% 
  filter (sd > 50)%>% 
  group_by(Key) %>% 
  ggplot(aes(Date, sd, color = Key)) + 
  geom_line(show.legend = FALSE, position = "stack", size = 1) + 
  scale_y_log10() +
  xlab("Date") + 
  ylab("SD Volumen - Price of Coin") +
  ggtitle("Standard Deviation of prices-volumen in the time")

#we observe al line, like a new coin that we have data for a short peroid of time, we investigate whats is it
finaldataset %>% group_by(Key, Vol24hs) %>% summarize(Price = mean(Price)) %>% arrange(desc(Vol24hs))
