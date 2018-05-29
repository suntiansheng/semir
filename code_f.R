#library(xts)
library(readr)
library(lubridate)
library(dplyr)
library(stringr)
setwd("/media/sunao/新加卷1/host/t02/20180416")
file_name <- dir()
###read
pri <- data.frame(name = file_name,max = NA , min = NA)
#length(file_name)
for (i in 1 : length(file_name)){
  df <- read_csv(file_name[i],skip = 1,col_names = F)
  names(df) <- c('date','time','price','quantity','sum','limit','B1_p',
                 'B1_q','B2_p','B2_q','B3_p','B3_q','B4_p','B4_q','B5_p',"B5_q",'S1_p',
                 'S1_q','S2_p','S2_q','S3_p','S3_q','S4_p',"S4_q","S5_p",
                 "S5_q",'BS')
  df$date <- ymd(df$date)
  df$time <- hms(df$time)
  df_1 <- df[hour(df$time) == 9,]
  df_2 <- df_1[minute(df_1$time)>=30&minute(df_1$time)<=45,]
  pri[pri$name == file_name[i],2] = max(df_2$price)
  pri[pri$name == file_name[i],3] = min(df_2$price)
}

pri$rate <- (pri$max - pri$min)/pri$min
write_csv(pri,path = 'pri.csv')


########################


############################
set.seed(1234)
c1 <- runif(31 , min = 0, max = 0.001)

file_name <- dir()

name <- data.frame()

prob <- data.frame()

for(i in 1:length(file_name)){
  print(i)
  df <- read_csv(file_name[i],skip = 1,col_names = F)
  names(df) <- c('date','time','price','quantity','sum','limit','B1_p',
                 'B1_q','B2_p','B2_q','B3_p','B3_q','B4_p','B4_q','B5_p',"B5_q",'S1_p',
                 'S1_q','S2_p','S2_q','S3_p','S3_q','S4_p',"S4_q","S5_p",
                 "S5_q",'BS')
  df <- select(df,date,time,quantity)
  df$date <- ymd(df$date)
  df$time <- hms(df$time)
  df_1 <- df[hour(df$time) == 9,]
  df_2 <- df_1[minute(df_1$time)>=30&minute(df_1$time)<=45,]
  quan <- quantile(df_2$quantity, probs = seq(from = 0 , to = 1, length.out = 31))
  quan1 <- quan + c1
  des <- as.data.frame(table(cut(df_2$quantity,breaks =  quan1)))
  prob <- rbind(prob, t(des[,2]))
  name <- rbind(name,as.character(file_name[i]), stringsAsFactors = F)

}

names(name) <- 'stock'
re <- cbind(name,prob)

write_csv(re,path = 'result.csv')

#######

df <- read.csv(file = 'result.csv')
l <- apply(df[,-1],1,sum)
d1 <- df[,-1]/l
m1 <- d1[1,]
fun <- function(x){
  g = 1 - (2 * sum(x[1:29])+1)*(1/30)
 return(g)
}
d2 <- apply(d1,1,fun)
d2 <- cbind(df$stock,d2)
write.csv(d2, file = 'gini.csv',fileEncoding = 'UTF-8')

#######

gini <- read.csv(file = 'gini.csv')
pri <- read.csv(file = 'pri.csv')
tt1 <- data.frame(name = pri$name,rate = pri$rate,gini = gini$x)
tt1 <- tt1[order(tt1$rate,decreasing = T),]
#write.csv(tt1,file = 'tt.csv')
i = 1
k = 1
temp <- abs(mean(tt1$gini[1:i]) - mean(tt1$gini[i:100]))
for(i in 2 : 99){
  temp1 <- abs(mean(tt1$gini[1:i]) - mean(tt1$gini[i:100]))
  if(temp1 > temp){
    temp <- temp1
    k = i
  }
}


#######

x = seq(from = 0 , to = 1, length.out = 10)
y = seq(from = 0 , to = 1, length.out = 10)
plot(ecdf(df[1,-1]))



