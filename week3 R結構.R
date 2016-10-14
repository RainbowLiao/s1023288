setwd('D:/凱/生態資訊')

#指定變數
x <- 1
y <- "1"
LETTERS
LETTERS[25]
seq(1,50)

#匯率換算
USDTWD <- 31.53
# 15 USD = ? TWD
15*USDTWD

#換率換算funtion結構
USDTWDConv <- function(usd, USDTWD = 31.5){
  twd <- usd* USDTWD
  #return 代表傳回數值或資料
  return(twd)
}

#運算補充
## mean(), sd(), sqrt()
## 小數點後位數設定  option(digits = 20)


# R的資料結構 Vector, DATA FRAME, matrix
## vector 可用 c() 將資料聚合
# vector example 
x <- c(1, 3, 4, 5, 6, 2)
length(x)

# data. frame example 
x <- c(1, 3, 4, 5, 6, 2)
x.df <- data.frame(x)
x.df

# matrix example
x <- c(1, 3, 4, 5, 6, 2, 5, 3, 6)
matrix(x, nrow = 3, ncol = 3, byrow = FALSE)

#轉換資結構用 as.

# 索引 隨機給予50個數 抽出第x列y行 數值[x, ] [ ,m]
x <- sample(50)
x.m <- matrix(x, nrow = 10)
x.df <- data.frame(x.m)
x.m
x.df
log(x.m[ ,3])
View(x.m)
x.m[, 3]<- log10(x.m[, 3])
x.m
summary(x.m)
x.m[x.m >10] 


