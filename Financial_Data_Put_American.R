
library(readr)
library(dplyr)
library(stringr)
library('ragtop')
# total_puts <- read_csv("~/projects/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Puts.xlsb.csv")
total_puts <- read_csv("C:/Users/CK/Desktop/CK/Duke/Honors Thesis/Github/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Puts.xlsb.csv")
total_puts <- total_puts[complete.cases(total_puts),]
total_puts$interest_rate <- total_puts$interest_rate/100
total_puts$strike_price <- total_puts$strike_price/1000

total_puts_American <- total_puts %>% 
  filter(str_detect(exercise_style,"A")) %>% 
  filter(volume > 50)


# training_start <- 1 #06/03
# training_end <- 322 #06/07

training_start <- 323 #06/03
training_end <- 559 #06/07

N <- training_end - training_start + 1



x_1_bs <- as.numeric(total_puts_American$forward_price[training_start:training_end])
x_2_bs <- as.numeric(total_puts_American$strike_price[training_start:training_end])
x_3_bs <- as.numeric(total_puts_American$impl_volatility[training_start:training_end])
x_4_bs <- as.numeric(total_puts_American$time_to_exp[training_start:training_end])
x_5_bs <- as.numeric(total_puts_American$dividend_yield[training_start:training_end])
x_6_bs <- as.numeric(total_puts_American$interest_rate[training_start:training_end])

x_bs <- cbind(x_1_bs,x_2_bs,x_3_bs,x_4_bs,x_5_bs,x_6_bs)

blackscholes <- rep(NA,N)
for (row in 1:nrow(data.frame(x_bs))){
  blackscholes[row] <- as.numeric(blackscholes(-1,S0=x_1_bs[row],K=x_2_bs[row],r=x_6_bs[row],t=x_4_bs[row],vola=x_3_bs[row],divrate=x_5_bs[row])$Price)
}
blackscholes



range01 <- function(x){(x-min(x))/(max(x)-min(x))} #Scaling
total_puts_American$strike_price_scaled <- range01(total_puts_American$strike_price)
total_puts_American$forward_price_scaled <- range01(total_puts_American$forward_price)
total_puts_American$time_to_exp_scaled <- range01(total_puts_American$time_to_exp)
total_puts_American$impl_volatility_scaled <- range01(total_puts_American$impl_volatility)
total_puts_American$dividend_yield_scaled <- range01(total_puts_American$dividend_yield)
total_puts_American$interest_rate_scaled <- range01(total_puts_American$interest_rate)


x_1 <- as.numeric(total_puts_American$forward_price_scaled[training_start:training_end])
x_2 <- as.numeric(total_puts_American$strike_price_scaled[training_start:training_end])
x_3 <- as.numeric(total_puts_American$impl_volatility_scaled[training_start:training_end])
x_4 <- as.numeric(total_puts_American$time_to_exp_scaled[training_start:training_end])
x_5 <- as.numeric(total_puts_American$dividend_yield_scaled[training_start:training_end])
x_6 <- as.numeric(total_puts_American$interest_rate_scaled[training_start:training_end])

x <- cbind(x_1,x_2,x_3,x_4,x_5,x_6)


y <- as.numeric(total_puts_American$mid_price[training_start:training_end])

variables <- 6

