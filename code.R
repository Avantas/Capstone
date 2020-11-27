
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
library(TTR)
library(knitr)

url <- "https://raw.githubusercontent.com/Avantas/Capstone/main/dset.csv"

dset <- read_csv(url)

#if you want to have a local copy
#download.file(url, "dataset.csv")

head(dset)

#We need to take out the column X1

dset$X1 <- NULL


###############################################analyse the data

dset %>% group_by(Key) %>% filter(Vol24hs >= 10^9) %>% # we filter for Crypto who market cap is more than 100 millons
  ggplot(aes(Key, Price)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_y_log10() +
  xlab("Crypto Coin") + 
  ylab("Market price (Log 10 scale)") +
  ggtitle("Variation in prices in the dataset period")

dset %>% filter(Vol24hs >= 10^9)  %>% group_by(Key) %>% summarize(dif = Price-mean(Price)) %>% arrange(desc(dif))

head(dset)

# we observe the price fluctuations between diferents coins over the date 
dset %>% group_by(Key) %>% filter(Vol24hs >= 10^9) %>%
  ggplot(aes(Date, Price, color = Key)) + geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Price of Coin (Log scale)") + ggtitle("Variation of the prices in the time")

#for observe the weight of the prices moviment, we create a relation between price and volumen
dset %>% mutate(relation_vol_price = Vol24hs/Price) %>% 
  group_by(Key) %>% filter(Vol24hs >= 10^9) %>% 
  ggplot(aes(Date, relation_vol_price, color = Key)) + geom_line() + scale_y_log10() +
  xlab("Date") + ylab("Price of Coin") + ggtitle("Relation Price-Volumen in the time")
# In the visutalization, is posible to observe when all prices go up (by buy operations) or
# when some trade are moving between cryptocoins, the main usefull thing is to detect negative correlations
# In automatical small operations

#we tray again with all cryptocoins (we take out the filter and set for better visualization)
dset %>% mutate(relation_vol_price = Vol24hs/Price) %>% 
  group_by(Key) %>% 
  ggplot(aes(Date, relation_vol_price, color = Key)) + 
  geom_line(show.legend = FALSE, position = "stack", size = 1) + 
  scale_y_log10() +
  xlab("Date") + 
  ylab("Relation Volumen - Price of Coin") +
  ggtitle("Relation prices-volumen in the time")

#we can see two big groups of coins moving together


#we tray again but we try to filter for sd
dset %>% mutate(sd = sd(Vol24hs/Price)) %>% 
  filter (sd > 50)%>% 
  group_by(Key) %>% 
  ggplot(aes(Date, sd, color = Key)) + 
  geom_line(show.legend = FALSE, position = "stack", size = 1) + 
  scale_y_log10() +
  xlab("Date") + 
  ylab("SD Volumen - Price of Coin") +
  ggtitle("Standard Deviation of prices-volumen in the time")

#we observe al line, like a new coin that we have data for a short peroid of time, we investigate whats is it
dset %>% group_by(Key, Vol24hs) %>% summarize(Price = mean(Price)) %>% arrange(desc(Vol24hs))

#we observe al line, like a new coin that we have data for a short peroid of time, we investigate whats is it
dset %>% group_by(Key, Vol24hs) %>% summarize(Price = mean(Price)) %>% arrange(desc(Vol24hs))

dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% 
  filter(Key %in% c("BTC", "LTC", "ETH"))%>% ggplot(aes(Date, dif, color = Key)) + 
  geom_line() + scale_y_log10() + ylab("Variation of the mean Prices (Log10 scale)") + 
  ggtitle("Moviment of prices from the mean")

dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% 
  filter(Key %in% c("BTC", "LTC", "ETH"))%>% 
  ggplot(aes(Date, dif, color = Key)) + geom_line() + 
  ylab("Variation of prices from the mean") + ggtitle("Change in the prices from the mean")

dset %>% group_by(Key) %>%  mutate(dif = (mean(Price)-Price)*10) %>% 
  ggplot(aes(Date, dif, color = Key)) + geom_line(show.legend =  FALSE) + 
  scale_y_log10() + ylab("Variation of prices from the mean (Log10 scale)") + 
  ggtitle("Variation of prices in all cryptocoin")

dset %>% group_by(Key) %>%  mutate(dif = (mean(Price)-Price)*10)

dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% 
  filter(Key %in% c("BTC", "LTC", "ETH", "USDT", "XRP", "LINK", "BCH", "BNB", "DOT", "ADA")) %>% 
  ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + 
  ylab("Variation of prices from the mean (Log10 scale)") + ggtitle("Top 10 by volumen variation prices")

top20 <- dset %>% group_by(Key) %>% summarize(sum = sum(Market_Cap)) %>% arrange(desc(sum))
top20 <- top20 %>% top_n(20)
top20


dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% 
  filter(Key %in% top20$Key) %>% ggplot(aes(Date, dif, color = Key)) + 
  geom_line() + scale_y_log10() + 
  ylab("Variation of prices from the mean (Log10 scale)") + ggtitle("Variation in the prices from mean, Top 20")

#We will see the price one by one

dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[1]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for BTC")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[2]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for ETH")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[3]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for USDT")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[4]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for XRP")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[5]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for LINK")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[6]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for BCH")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[7]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for BNB")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[8]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for LTC")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[9]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for DOT")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[10]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for ADA")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[11]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for BSV")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[12]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for USDC")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[13]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for EOS")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[14]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for XMR")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[15]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for WBTC")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[16]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for TRX")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[17]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for XLM")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[18]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for XTZ")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[19]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for CRO")
dset %>% group_by(Key) %>% mutate(dif = (mean(Price)-Price)*10) %>% filter(Key %in% top20$Key[20]) %>% ggplot(aes(Date, dif, color = Key)) + geom_line() + scale_y_log10() + ylab("Variation in prices from mean (Log10 scale)") + ggtitle("Variation in prices from mean for LEO")
top20
dset %>% group_by(Key) %>% filter(Key == top20$Key[1]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for BTC")
dset %>% group_by(Key) %>% filter(Key == top20$Key[2]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for ETH")
dset %>% group_by(Key) %>% filter(Key == top20$Key[3]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for USDT")
dset %>% group_by(Key) %>% filter(Key == top20$Key[4]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for XRP")
dset %>% group_by(Key) %>% filter(Key == top20$Key[5]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for LINK")
dset %>% group_by(Key) %>% filter(Key == top20$Key[6]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for BCH")
dset %>% group_by(Key) %>% filter(Key == top20$Key[7]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for BNB")
dset %>% group_by(Key) %>% filter(Key == top20$Key[8]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for LTC")
dset %>% group_by(Key) %>% filter(Key == top20$Key[9]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for DOT")
dset %>% group_by(Key) %>% filter(Key == top20$Key[10]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for ADA")
dset %>% group_by(Key) %>% filter(Key == top20$Key[11]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for BSV")
dset %>% group_by(Key) %>% filter(Key == top20$Key[12]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for USDC")
dset %>% group_by(Key) %>% filter(Key == top20$Key[13]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for EOS")
dset %>% group_by(Key) %>% filter(Key == top20$Key[14]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for XMR")
dset %>% group_by(Key) %>% filter(Key == top20$Key[15]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for WBTC")
dset %>% group_by(Key) %>% filter(Key == top20$Key[16]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for TRX")
dset %>% group_by(Key) %>% filter(Key == top20$Key[17]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for XLM")
dset %>% group_by(Key) %>% filter(Key == top20$Key[18]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for XTZ")
dset %>% group_by(Key) %>% filter(Key == top20$Key[19]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for CRO")
dset %>% group_by(Key) %>% filter(Key == top20$Key[20]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line() + ylab("Variation in prices from mean") + ggtitle("Variation in prices from mean for LEO")

dset %>% group_by(Key) %>% filter(Key == top20$Key[1]) %>% 
  ggplot(aes(Date, Price, col= Key)) + geom_line() + geom_smooth() +
  ggtitle("Smoothing BTC Price")

#################################### we load the historical BTC data

url <- "https://raw.githubusercontent.com/Avantas/Capstone/main/BTC_USD%20Binance%20Historical%20Data%20(2).csv"

historical <- read_csv(url)

historical$Date <- mdy(historical$Date)

historical %>% ggplot(aes(Date, Price)) + geom_line() + ggtitle("BTC Price from 2018 to 2020")

# we made the function of movil mean and set it in 21 days (Its knowed that there are patterns of moviments in 21 days)

library(zoo)
rollingmean <- rollmedian(historical$Price, 21)

rollingmean

rollingmean[1153:1172] <- 0

historical <- historical %>% mutate(ma = rollingmean)

historical %>% ggplot(aes(Date, Price)) + geom_line() + geom_line(aes(Date, ma), col = "green") + ggtitle("Price of BTC with Rolling mean (21 days)")
historical %>% filter(Date >= "2020-01-01") %>% ggplot(aes(Date, Price)) + geom_line() + geom_line(aes(Date, ma), col = "green")  + ggtitle("Price of BTC with Rolling mean (21 days) - zoom")

########################we calculate KDJ indicator

KDJ <- historical %>% select(High, Low, Price)
KDJ <- KDJ %>% stoch()
KDJ
KDJ_matrix_K <- matrix(KDJ[-1:-13,1],(839+333-13),1)
KDJ_matrix_D <- matrix(KDJ[-1:-15,2],(839+333-15),1)
KDJ_matrix_J <- matrix(KDJ[-1:-17,3],(839+333-17),1)
KDJ_matrix_D[1158:1159] <- 0 
KDJ_matrix_J[1156:1159] <- 0
KDJ_matrix_K[1160:1172] <- 0 
KDJ_matrix_J[1160:1172] <- 0
KDJ_matrix_D[1160:1172] <- 0 

KDJ_matrix <- matrix(c(KDJ_matrix_K, KDJ_matrix_D, KDJ_matrix_J), 1172,3)
KDJ_matrix

historical %>% mutate(K = KDJ_matrix[,1] , D = KDJ_matrix[,2], J = KDJ_matrix[,3]) %>% 
  filter(Date >= "2020-07-01") %>% ggplot(aes(Date, Price)) + geom_line() +
  geom_line(aes(Date, K*15000), col = "blue") + 
  geom_line(aes(Date, D*15000), col = "yellow") + 
  geom_line(aes(Date, J*15000), col = "black") + 
  ggtitle("BTC Price and KDJ indicator")
  

historical  %>% mutate(day = weekdays(historical$Date)) %>% 
  filter(Date >= "2020-01-01") %>% group_by(day) %>% 
  ggplot(aes(Date, Price)) + geom_boxplot() + 
  facet_grid(. ~ day) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  ggtitle("Distribution of BTC prices by day")

######## We explore the data set from kaggle

url1 <- "https://www.kaggle.com/mczielinski/bitcoin-historical-data?select=bitstampUSD_1-min_data_2012-01-01_to_2020-09-14.csv"
ds_k <- read_csv(url1)

write.csv(reduced_kaggle, "reduced_kaggle.csv")


ds_kaggle <- read_csv("bitstampUSD_1min_data_20120101_to_20200914.csv")
head(ds_kaggle)

ds_kaggle$Timestamp <- as.POSIXct(ds_kaggle$Timestamp, origin="1970-01-01")
ds_kaggle

min(ds_kaggle$Timestamp)
max(ds_kaggle$Timestamp)

#famous stock and flow?
ds_kaggle %>% ggplot(aes(Timestamp, Close)) + geom_line() + scale_y_log10() +
  ylab("Price (Log10 scale)") + ggtitle("BTC Price 2012 - 2020 (Log Scale)")


####### Machine Learning

reduced_kaggle <- ds_kaggle %>% filter(Timestamp >= "2018-01-01")

test_set <- reduced_kaggle %>% filter(Timestamp >= "2020-06-01", !is.na(Close))
train_set <- reduced_kaggle %>% filter(Timestamp < "2020-06-01", !is.na(Close))

test_set %>% ggplot(aes(Timestamp, Close)) + geom_line() + 
  ylab("Price") + xlab("Date") + ggtitle("BTC Price test set")

train_glm <- train(Close ~ ., method = "glm", data = train_set)
head(train_glm)

y_hat_glm <- predict(train_glm, test_set, type = "raw")

test_set %>% mutate(y_hat_glm = y_hat_glm) %>% filter(Timestamp >= "2020-09-01") %>% 
  ggplot(aes(Timestamp, Close)) + geom_line() + geom_line(aes(Timestamp, y_hat_glm),col = "green")  + 
  ylab("Price") + xlab("Date") + ggtitle("BTC price and prediction (Green)")
  
# the correlation between the Price of close with Open Price High and Low... is near to perfect, but the problem is we will have not all this information
#So we try to predict the high and low values with the Open Price.

train_glm_high <- train(High ~ Open, method = "glm", data = train_set)
train_glm_low <- train(Low ~ Open, method = "glm", data = train_set)
y_hat_glm_high <- predict(train_glm_high, test_set, type = "raw")
y_hat_glm_low <- predict(train_glm_low, test_set, type = "raw")
test_set %>% mutate(y_hat_glm_high = y_hat_glm_high, y_hat_glm_low = y_hat_glm_low) %>% 
  filter(Timestamp >= "2020-09-13 18:00") %>% ggplot(aes(Timestamp, High)) + 
  geom_line() + 
  geom_line(aes(Timestamp, y_hat_glm_high),col = "green") + 
  geom_line(aes(Timestamp, y_hat_glm_low),col = "red") + 
  geom_line(aes(Timestamp, Low), col ="black") + ggtitle("High and Low Prices prediction for BTC") + xlab("Date") + ylab("Price")

#the prediction looks sobreestimated. We will check if the Open Price is the same in all day.

test_set %>% filter(Timestamp >= "2020-09-13 18:00")

# Machine learning data set future

Price <- historical$Price
histor <- historical %>% mutate(tomorrow = NA)
histor$tomorrow[2:1172] <- Price[1:1171]

#precio futuro de tomorrow
histor
#now, we predict

test_set_histor <- histor %>% filter(Date >= "2020-06-01", !is.na(tomorrow)) %>% 
  select(Date, Price, Open, High, Low, tomorrow)
train_set_histor <- histor %>% filter(Date < "2020-06-01", !is.na(tomorrow)) %>% 
  select(Date, Price, Open, High, Low, tomorrow)

test_set_histor %>% ggplot(aes(Date, Price)) + geom_line() + ggtitle("BTC Price test set")

train_set_histor
test_set_histor

train_glm <- train(tomorrow ~ ., method = "glm", data = train_set_histor)
train_glm

y_hat_glm <- predict(train_glm, test_set_histor, type = "raw")

test_set_histor %>% mutate(y_hat_glm = y_hat_glm) %>% 
  filter(Date >= "2020-11-01") %>% ggplot(aes(Date, Price)) + 
  geom_line() + geom_line(aes(Date, y_hat_glm),col = "green") + ggtitle("BTC real price (black) and prediction (green)")

rmse_glm <- RMSE(y_hat_glm, test_set_histor$Price)

#we normalize the RMSE for have percent
rmse_glm/(max(test_set_histor$Price)-min(test_set_histor$Price))


# We go back to the minute by minute data set

dset %>% group_by(Key) %>% filter(Key == top20$Key[1]) %>% ggplot(aes(Date, Price, col= Key)) + geom_line()

# with the trend of the day, we can construct in a real time function when is moment 
#for buy and sell.

#we try again with other tecnics kNN

train_knn <- train(tomorrow ~ ., method = "knn", data = train_set_histor)
train_knn

y_hat_knn <- predict(train_knn, test_set_histor, type = "raw")

test_set_histor %>% mutate(y_hat_knn = y_hat_knn) %>% 
  filter(Date >= "2020-11-01") %>% ggplot(aes(Date, Price)) + 
  geom_line() + geom_line(aes(Date, y_hat_knn),col = "green") + 
  ggtitle("BTC real price (black) and prediction (green) by method KNN")

rmse_knn <- RMSE(y_hat_knn, test_set_histor$Price)
rmse_knn
#we normalize the RMSE for have percent
rmse_knn/(max(test_set_histor$Price)-min(test_set_histor$Price))

# random forest

train_rf <- train(tomorrow ~ ., method = "rf", data = train_set_histor)
train_knn

y_hat_rf <- predict(train_rf, test_set_histor, type = "raw")

test_set_histor %>% mutate(y_hat_rf = y_hat_rf) %>% filter(Date >= "2020-11-01") %>% 
  ggplot(aes(Date, Price)) + geom_line() + 
  geom_line(aes(Date, y_hat_rf),col = "green") + ggtitle("BTC real price (black) and prediction (green) by method Random Forest")

rmse_rf <- RMSE(y_hat_rf, test_set_histor$Price)
rmse_rf
#we normalize the RMSE for have percent
rmse_rf/(max(test_set_histor$Price)-min(test_set_histor$Price))

################## So, the method "glm" is the best to predict the close price for tomorow using like predictor
# the data from today. It is very good, knowing the close price of tomorrow, its permits 
# to aply bands for buy and sell in the lapsus of the day like we see in the daily data set.

# also is posible to ensemble all in a real time function. 
#is not posible to do this in this code for this work because we need to generate 
# the final and rmd document.
# but i construct it in another script in this GitHub repository.




day09 <- test_set_histor %>% filter(Date == "2020-11-09")
day09

dset %>% group_by(Key) %>% filter(Key == top20$Key[1]) %>% 
  ggplot(aes(Date, Price, col= Key)) + geom_line() + 
  geom_hline(aes(yintercept = day09$tomorrow), colour="#990000", linetype="dashed") +
  ggtitle("BTC real variation price in a day and the prediction of close price")

