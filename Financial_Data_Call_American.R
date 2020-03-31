
library(readr)
library(dplyr)
library(stringr)
library('ragtop')
# total_calls <- read_csv("~/projects/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Puts.xlsb.csv")
total_calls <- read_csv("C:/Users/CK/Desktop/CK/Duke/Honors Thesis/Github/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Calls.xlsb.csv")
total_calls <- total_calls[complete.cases(total_calls),]
total_calls$interest_rate <- total_calls$interest_rate/100
total_calls$strike_price <- total_calls$strike_price/1000

total_calls_American <- total_calls %>% 
  filter(str_detect(exercise_style,"A")) %>% 
  filter(volume > 50)

total_calls_American <- total_calls_American[(total_calls_American$time_to_exp<=3.712 & total_calls_American$time_to_exp >= 0.776), ]

training_start <- 1 #06/03
training_end <- 268 #06/07

# training_start <- 269 #06/10
# training_end <- 432 #06/14

N <- training_end - training_start + 1



x_1_bs <- as.numeric(total_calls_American$forward_price[training_start:training_end])
x_2_bs <- as.numeric(total_calls_American$strike_price[training_start:training_end])
x_3_bs <- as.numeric(total_calls_American$impl_volatility[training_start:training_end])
x_4_bs <- as.numeric(total_calls_American$time_to_exp[training_start:training_end])
x_5_bs <- as.numeric(total_calls_American$dividend_yield[training_start:training_end])
x_6_bs <- as.numeric(total_calls_American$interest_rate[training_start:training_end])

x_bs <- cbind(x_1_bs,x_2_bs,x_3_bs,x_4_bs,x_5_bs,x_6_bs)

blackscholes <- rep(NA,N)
for (row in 1:nrow(data.frame(x_bs))){
  blackscholes[row] <- as.numeric(blackscholes(1,S0=x_1_bs[row],K=x_2_bs[row],r=x_6_bs[row],t=x_4_bs[row],vola=x_3_bs[row],divrate=x_5_bs[row])$Price)
}
blackscholes



range01 <- function(x){(x-min(x))/(max(x)-min(x))} #Scaling
total_calls_American$strike_price_scaled <- range01(total_calls_American$strike_price)
total_calls_American$forward_price_scaled <- range01(total_calls_American$forward_price)
total_calls_American$time_to_exp_scaled <- range01(total_calls_American$time_to_exp)
total_calls_American$impl_volatility_scaled <- range01(total_calls_American$impl_volatility)
total_calls_American$dividend_yield_scaled <- range01(total_calls_American$dividend_yield)
total_calls_American$interest_rate_scaled <- range01(total_calls_American$interest_rate)


x_1 <- as.numeric(total_calls_American$forward_price_scaled[training_start:training_end])
x_2 <- as.numeric(total_calls_American$strike_price_scaled[training_start:training_end])
x_3 <- as.numeric(total_calls_American$impl_volatility_scaled[training_start:training_end])
x_4 <- as.numeric(total_calls_American$time_to_exp_scaled[training_start:training_end])
x_5 <- as.numeric(total_calls_American$dividend_yield_scaled[training_start:training_end])
x_6 <- as.numeric(total_calls_American$interest_rate_scaled[training_start:training_end])

x <- cbind(x_1,x_2,x_3,x_4,x_5,x_6)


y <- as.numeric(total_calls_American$mid_price[training_start:training_end])

variables <- 6

