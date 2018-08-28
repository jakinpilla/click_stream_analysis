
setwd('C:/Users/Daniel/click_stream_analysis')
df <- read.csv('./data/ML_data_clickStreams.csv')
head(df)

df[1, ][, c(2 : 18)]
unnamed <- unname(df[1, ][, c(2 : 18)])
class(unnamed)
unnamed <- as.numeric(unnamed)
class(unnamed)
str(unnamed)
sum(unnamed)
sd(unnamed)
mean(unnamed)
sd(unnamed) / mean(unnamed)

colnames(df)
df['CT_PORTAL'] / df['DWELLTIME']

tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
head(tran, 50)
str(tran)
dim(tran)
head(tran)
str(tran)

# weekday binning 하기
tran$ymd <- as.Date(tran$ymd)
tran$weekday <- format(tran$ymd, '%a')
head(tran)
str(tran)
tran$weekday <- as.factor(tran$weekday)

head(tran, 30)

# install.packages('dummies')
library(dummies)

tran <- dummy.data.frame(tran, names=c('weekday'), sep='_')
head(tran)
tran <- tran[, c('custid', 'ymd', 'time', 'weekday_일', 'weekday_월', 'weekday_화', 'weekday_수', 
         'weekday_목', 'weekday_금', 'weekday_토', 'prod', 'amt')]
head(tran)

tran$hour <- substr(tran$time, 1, 2)
head(tran)
tran$hour <- as.numeric(tran$hour)
head(tran)
str(tran)

# hour binning 하기
library(tidyverse)
fivenum(tran$hour)
max(tran$hour)
min(tran$hour)
tran %>% mutate(time_bin = cut(hour, 
                               breaks = c(0, 6, 12, 18, 23),
                               include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                               labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran
head(tran)

# hour bin을 one-hot coding
tran <- dummy.data.frame(tran, names=c('time_bin'), sep='_')
head(tran,20)

# 고객별 방문 요일의 비율 구하기

tran %>% group_by(custid) %>% summarise(sum.weekday_일 = sum(weekday_일),
                                        sum.weekday_월 = sum(weekday_월),
                                        sum.weekday_화 = sum(weekday_화),
                                        sum.weekday_수 = sum(weekday_수),
                                        sum.weekday_목 = sum(weekday_목),
                                        sum.weekday_금 = sum(weekday_금),
                                        sum.weekday_토 = sum(weekday_토)) -> weekday_rate
head(weekday_rate)

weekday_rate %>% mutate(total_weekday = (sum.weekday_일 + sum.weekday_월 + sum.weekday_화 + 
                                           sum.weekday_수 + sum.weekday_목 + sum.weekday_금 + sum.weekday_토)) -> weekday_rate

weekday_rate %>% mutate(rate_sun = sum.weekday_일 / total_weekday,
                        rate_mon = sum.weekday_월 / total_weekday,
                        rate_tue = sum.weekday_화 / total_weekday,
                        rate_wed = sum.weekday_수 / total_weekday,
                        rate_thu = sum.weekday_목 / total_weekday,
                        rate_fri = sum.weekday_금 / total_weekday,
                        rate_sat = sum.weekday_토 / total_weekday,) -> weekday_rate

weekday_rate$rate_sun
head(weekday_rate[, c('custid', 'rate_sun', 'rate_mon', 'rate_tue', 'rate_wed', 'rate_thu', 'rate_fri', 'rate_sat')])
