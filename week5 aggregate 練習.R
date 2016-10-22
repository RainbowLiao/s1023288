##  week5 'aggregate'(聚合) reveiw and homework.

setwd('D:/凱/生態資訊/git')

# 輸入資料 (tab 可顯示list 路徑)
library(data.table)
cwb <- fread('D:/凱/生態資訊/data/raw/200601_auto_hr.txt', skip=74)

# 設定欄位名稱(colnames <- c(固定格式)
#              setnames(目標,欄位,colnames)

colnames <- c("stno", "yyyymmddhr","PS01", "TX01", "RH01","WD01","WD02", "PP01", 
              "SS01")

setnames(cwb,1:9,colnames)

# 時間格式化(strptime(目標,'格式'))
# 建立新欄位 cwb[, timestamps:=as.POSIXct(strptime(yyyymmddhr,'%Y%m%d%H'))]

strptime(cwb$yyyymmddhr,'%Y%m%d%H')

cwb[,timestamp:=as.POSIXct(strptime(yyyymmddhr-1,'%Y%m%d%H'))]
  

# 先去除怪怪值-9997,-9999  (or一開始在'輸入檔案'時就把怪值變成'NA') 
#          (,na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))

#colClass
cwb[cwb <= -9996] <- 'NA'



# 開始aggregate(表示式,資料集,函數) 

# 計算mean(去除'NA' = na.rm = TRUE)

mean_omit_na <- function(x){
  x <- as.numeric(x)
  return (mean(x, na.rm = T))
}

aggregate(iris$Petal.Length, by = list(iris$Species), FUN = mean)
C0M530 <- cwb[cwb$stno == 'C0M530']

mean_va <- aggregate(as.numeric(C0M530$TX01), by = list(C0M530$date), FUN = mean_omit_na)

C0M530[, date:= format.Date(timestamp, '%Y-%m-%d')]
format.Date(C0M530$timestamp, '%Y-%m-%d')


library(plyr)
#mean_value <- plyr::ddply(cwb, ~ 'C0M530', summarize,
#                  time = 
#                  tx = mean_omit_na(TX01),
#                  pp = mean_omit_na(PP01))





