
setwd('C:/Users/Daniel/click_stream_analysis')
df <- read.csv('./data/ML_data_clickStreams.csv')
head(df)

# CT_xxx :: 웹사이트 카테고리 별 체류시간 비율(제품 카테고리별 구매비용 비율)
# COVERAGE :: 서로 다른 웹 사이트에 얼마나 다양하게 접속했는지에 대한 비율
#             (서로 다른 카테고리 제품을 얼마나 다양하게 구매하였는지에 대한 비율, 구매 제품 카테고리수 / 전체 카테고리 수)
# G_DWELLTIME :: 총 체류시간(총 구매비용)
# PAGEVIEWS :: 총 페이지뷰(총 구매 제품 카테고리의 수)
# HF_xxx :: 시간대별(0-5시, 6-11시, 12-17시, 18-23시) 체류시간 비율(시간대별 매장 방문 비율)
# DF_xxx :: 요일별 체류시간 비율(-> 요일별 방문비율)
# VISITES :: 접속한 서로 다른 웹사이트의 수(구매한 서로 다른 상품 카테고리 수)
# SITECOV :: 웹사이트 카케고리 별 체류시간 변동계수(카테고리별 체류시간의 '표준편차/평균' 값) 
#            (제품 카테고별 구매비용 변동계수(카테고리별 구매비용의 표준편차 / 평균))
# VDAYS :: 총 접속일수(총 매장 방문일수)
# DAYTIME :: 하루 평균 체류시간(하루 평균 구매비용)
# DAYCOV :: 일별 변동계수(일일 체류시간의 '표준편차/평균' 값)
#           (일일 구매비용의 '표준편차/평균' 값)
# SCH_KEYWORDS :: 네이버에서 검색한 검색량
# SCH_TOPKEYWORD :: 네이버에서 가장 많이 검색한 검색어
# GENDER :: 고객성별(남자/여자). 예측하고자 하는 값

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
head(tran[, -1])
tran <- tran[, -1]
str(tran)
dim(tran)
head(tran)
str(tran)

tran$ymd <- as.Date(tran$ymd)
colnames(tran)[which(names(tran) == "time")] <- "hour"
head(tran)
tran$hour <- as.numeric(substr(tran$hour, 1, 2))
head(tran)
str(tran)

# 고객별 총 매장방문 횟수 구하기
tran %>% group_by(custid, ymd, time) %>%
  summarise(visit_count = n()) -> df_visit_count

df_visit_count %>% arrange(desc(visit_count)) # 가장 방문을 많이 한 고객순으로 보기기



# weekday binning 하기
tran$ymd <- as.Date(tran$ymd)
tran$wd <- format(tran$ymd, '%a')
head(tran)
str(tran)
tran$wd <- as.factor(tran$wd)
tran$wd <- factor(tran$wd, levels=c('일', '월', '화', '수', '목', '금', '토'))

head(tran, 30)
str(tran)

# install.packages('dummies')
library(dummies)

tran <- dummy.data.frame(tran, names=c('wd'), sep='_')
head(tran)
names(tran)
colnames(tran) <- c('ymd', 'time', 'custid', 'prod', 'amt', 'wd_sun', 'wd_mon', 
                    'wd_tue', 'wd_wed', 'wd_thu', 'wd_fri', 'wd_sat')
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
tran %>% mutate(h_bin = cut(hour, 
                               breaks = c(0, 6, 12, 18, 23),
                               include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                               labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran
head(tran)

# hour bin을 one-hot coding
tran <- dummy.data.frame(tran, names=c('h_bin'), sep='_')
head(tran,20)

# 고객별 방문 요일의 비율 구하기
tran %>% group_by(custid) %>% summarise(sum.wd_sun = sum(wd_sun),
                                        sum.wd_mon = sum(wd_mon),
                                        sum.wd_tue = sum(wd_tue),
                                        sum.wd_wed = sum(wd_wed),
                                        sum.wd_thu = sum(wd_thu),
                                        sum.wd_fri = sum(wd_fri),
                                        sum.wd_sat = sum(wd_sat)) -> wd_rate
head(wd_rate)
dim(wd_rate) # 고객수는 총 2089명
wd_rate %>% mutate(total.wd = rowSums(.[2:8])) -> df_1
head(df_1)
df_1$total.wd[1:5]

df_1 %>% mutate(rate_sun = sum.wd_sun / total.wd,
                rate_mon = sum.wd_mon / total.wd,
                rate_tue = sum.wd_tue / total.wd,
                rate_wed = sum.wd_wed / total.wd,
                rate_thu = sum.wd_thu / total.wd,
                rate_fri = sum.wd_fri / total.wd,
                rate_sat = sum.wd_sat / total.wd) -> df_2

head(df_2)
dim(df_2)
df_tmp <- df_2[, c(10:16)]
df_tmp[] <- lapply(df_tmp, function(x) if(is.numeric(x)) round(x, 3) else x)
head(df_tmp)
df_wd_ratio <- cbind(custid = df_2[, 1], df_tmp)
head(df_wd_ratio)
# fix(df_wd_ratio)
# View(head(df_2, 20))
# 요일별 변동계수(coefficient of variation, CV) = 표준편차 / 평균
install.packages('matrixStats')
library(matrixStats)
library(dplyr)


dim(df_wd_ratio)

df_wd_ratio[, -1] %>%
  mutate(row_std = round(rowSds(as.matrix(.[1:7])), 3),
         row_mean = round(rowMeans(as.matrix(.[1:7])), 3), 
         wd_cv = row_std/row_mean) %>%
  select(1:7, 10) -> wd_ratio_cv

head(df_wd_ratio)
head(wd_ratio_cv)
wd_df <- cbind(custid = df_wd_ratio[, 1], wd_ratio_cv)

# fix(wd_df)

# 구매 상품 종류별 지출비율
head(tran)
str(tran)
length(unique(tran$prod))
tran$prod
names(tran)

# 고객별&제품별 총 구매비용 구하기
tran %>% 
  group_by(custid, prod) %>%
  summarise(sum.amt = sum(amt)) -> df_3
head(df_3)

# pivotting 
# 목적 : 고객별 구매 상품종류에 대한 지출비용을 알아보기위해 실시

library(reshape2)
names(df_3)

melted <- melt(df_3, id.vars=c('custid', 'prod'), measure.vars = c('sum.amt'))
head(melted)

dcasted <- dcast(melted, custid ~ prod, value.var = 'value')
head(dcasted)

head(melted)
sample_dcasted <- dcasted[1:2, ]
# fix(sample_dcasted)

# NA를 0으로 채우기
dcasted %>% mutate_all(funs(ifelse(is.na(.), 0, .))) -> df_4
head(df_4)
dim(df_4)
names(df_4) ## 물품 종류의 수는 84 종류

# 고객별 총구매액(total.amt)에 대한 컬럼 만들기
df_4 %>% mutate(total.amt = rowSums(.[2:85])) -> df_5
head(df_5)

# 고객들의 상품 종류별 구매비율 구하기
df_6 <- df_5[, 2:85] / df_5$total.amt
df_6 %>% mutate(total.sum = rowSums(.)) # total.sum =1 이 되는지 확인

# 소수점 4째자리에서 반올림하여 수들을 정리
df_6[] <- lapply(df_6, function(x) if(is.numeric(x)) round(x, 3) else x)
head(df_6)
names(df_6) <- paste('ratio', names(df_6), sep='_')

head(df_6)

df_7 <- cbind(df_5, df_6)

# fix(df_7)
dim(df_7)
df_7[, c(1, 87:170)] -> df_8
# fix(df_8) # 이것이 고객별 구매비용의 비율!
dim(df_8)
head(df_8)
df_8$max_prod <- colnames(df_8[, c(2:85)])[apply(df_8[, c(2:85)], 1, which.max)]
fix(df_8)
table(df_8$max_prod)

df_8 %>% 
  drop_na %>%
  group_by(max_prod) %>%
  summarise(count = n(), percent = count / nrow(.) * 100) %>%
  arrange(desc(count)) -> df_9


# fix(df_9)

df_wd_ratio %>% left_join(df_8, by='custid') -> df_10
head(df_10)
# fix(df_10)

# prod_coverage
# 물품 종류의 갯수 : 84개
# 소수점 4째자리에서 반올림하여 수들을 정리
df_4[, -1]
m <- as.matrix(df_4[, -1])
m
m[m>0] <- 1
m
df_tmp <- as.data.frame(m)
prod_sparm <- cbind(custid = df_4$custid, df_tmp)
# fix(prod_sparm)

prod_sparm

prod_sparm[, -1] %>% 
  mutate(cover_num = rowSums(.)) %>%
  mutate(prod_coverage = round(cover_num / 84, 4)) -> df_prod_cover

cbind(custid = prod_sparm[, 1], df_prod_cover) -> df_prod_cover
head(df_prod_cover)
dim(df_prod_cover)

names(df_10)
names(wd_df)
df_11 <- cbind(wd_df, df_10[, 9:93], prod_coverage = df_prod_cover$prod_coverage)
fix(df_11)













