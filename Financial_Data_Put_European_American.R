
library(readr)
library(dplyr)
library(stringr)
# total_puts <- read_csv("~/projects/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Puts.xlsb.csv")
total_puts <- read_csv("C:/Users/CK/Desktop/CK/Duke/Honors Thesis/Github/Independent_Study/spy_spx_(2019.06.01~2019.06.30)_Puts.xlsb.csv")
total_puts <- total_puts[complete.cases(total_puts),]

total_puts$strike_price <- total_puts$strike_price/1000
total_puts$interest_rate <- total_puts$interest_rate/100
range01 <- function(x){(x-min(x))/(max(x)-min(x))} #Scaling
total_puts$strike_price <- range01(total_puts$strike_price)
total_puts$forward_price <- range01(total_puts$forward_price)

total_puts_American <- total_puts %>% 
  filter(str_detect(exercise_style,"A")) %>% 
  filter(volume > 50)

total_puts_European <- total_puts_European[(total_puts_European$time_to_exp<=6&total_puts_European$time_to_exp>=1.24), ]

total_puts_European_American <- rbind(total_puts_European,total_puts_American)
# total_puts <- total_puts[total_puts$dividend_yield > 0 & total_puts$dividend_yield < 0.015, ]

training_start_European <- 1 #06/03
training_end_European <- 144 #06/07
training_start_American <- 621 #06/03
training_end_American <- 942 #06/07

# training_start_European <- 145 #06/10
# training_end_European <- 252 #06/14
# training_start_American <- 943 #06/10
# training_end_American <- 1179 #06/14


N <- (training_end_European - training_start_European + 1) + (training_end_American - training_start_American + 1)

x_1 <- as.numeric(total_puts_European_American$forward_price[c(training_start_European:training_end_European,training_start_American:training_end_American)])
x_2 <- as.numeric(total_puts_European_American$strike_price[c(training_start_European:training_end_European,training_start_American:training_end_American)])
x_3 <- as.numeric(total_puts_European_American$impl_volatility[c(training_start_European:training_end_European,training_start_American:training_end_American)])
x_4 <- as.numeric(total_puts_European_American$time_to_exp[c(training_start_European:training_end_European,training_start_American:training_end_American)])
x_5 <- as.numeric(total_puts_European_American$dividend_yield[c(training_start_European:training_end_European,training_start_American:training_end_American)])
x_6 <- as.numeric(total_puts_European_American$interest_rate[c(training_start_European:training_end_European,training_start_American:training_end_American)])

x <- cbind(x_1,x_2,x_3,x_4,x_5,x_6)


y <- as.numeric(total_puts_European_American$mid_price[c(training_start_European:training_end_European,training_start_American:training_end_American)])

variables <- 6

library('ragtop')
library('qrmtools')
library('jrvFinance')
blackscholes <- rep(NA,N)
for (row in 1:nrow(data.frame(x))){
  blackscholes[row] <- as.numeric(blackscholes(-1,S0=x_1[row],K=x_2[row],r=x_6[row],t=x_4[row],vola=x_3[row],divrate=x_5[row]))
}
blackscholes

# 
# blackscholes <- rep(NA,N)
# for (row in 1:nrow(data.frame(x))){
#   blackscholes[row] <- Black_Scholes(0,x_1[row],x_6[row],x_3[row],x_2[row],x_4[row],"put")
# }
# blackscholes
