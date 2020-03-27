
library(readr)
library(dplyr)
library(stringr)
total_calls <- read_csv("~/projects/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Calls.xlsb.csv")
total_calls <- total_calls[complete.cases(total_calls),]
total_calls$strike_price <- total_calls$strike_price/1000
total_calls$interest_rate <- total_calls$interest_rate/100
range01 <- function(x){(x-min(x))/(max(x)-min(x))} #Scaling
total_calls$strike_price <- range01(total_calls$strike_price)
total_calls$forward_price <- range01(total_calls$forward_price)

total_calls_European <- total_calls %>% 
  filter(str_detect(exercise_style,"E")) %>% 
  filter(volume > 50) 


# total_calls_European <- total_calls[(total_calls$time_to_exp>=0.016), ] # Atleast have 5 business days
# total_calls <- total_calls[total_calls$dividend_yield > 0 & total_calls$dividend_yield < 0.015, ]

training_start <- 1 #06/03
training_end <- 2937 #06/07
# 
# training_start <- 417 #06/10
# training_end <- 635 #06/14

N <- training_end - training_start + 1

x_1 <- as.numeric(total_calls_European$forward_price[training_start:training_end])
x_2 <- as.numeric(total_calls_European$strike_price[training_start:training_end])
x_3 <- as.numeric(total_calls_European$impl_volatility[training_start:training_end])
x_4 <- as.numeric(total_calls_European$time_to_exp[training_start:training_end])
x_5 <- as.numeric(total_calls_European$dividend_yield[training_start:training_end])
x_6 <- as.numeric(total_calls_European$interest_rate[training_start:training_end])

x <- cbind(x_1,x_2,x_3,x_4,x_5,x_6)


y <- as.numeric(total_calls_European$mid_price[training_start:training_end])

variables <- 6

library('ragtop')
library('qrmtools')
library('jrvFinance')
blackscholes <- rep(NA,N)
for (row in 1:nrow(data.frame(x))){
  blackscholes[row] <- as.numeric(blackscholes(1,S0=x_1[row],K=x_2[row],r=x_6[row],t=x_4[row],vola=x_3[row],divrate=x_5[row]))
}
blackscholes

# 
# blackscholes <- rep(NA,N)
# for (row in 1:nrow(data.frame(x))){
#   blackscholes[row] <- Black_Scholes(0,x_1[row],x_6[row],x_3[row],x_2[row],x_4[row],"put")
# }
# blackscholes
