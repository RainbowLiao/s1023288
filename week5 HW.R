
## list.files
## 題目：
### 1.計算C0M530(奮起湖)從2006~2015十年的
###   a.日均溫、b.日低溫、c.日高溫、d.月均溫、e.月累積降水


#### 2.計算最暖月每日最高均溫

##### 3.計算最冷月每日最低均溫

# load all of the txt


data_list <- list.files(path = "D:/凱/生態資訊/data/raw/" , pattern = "auto_hr")
substr(data_list,1,120)
filelist <- lapply(data_list, fread)

for (i in names) {
  filepath <- file.path('D:/凱/生態資訊/data/raw/')
  assign(i, read.delim(filepath))
  }


  

##  前置作業
{
setwd('D:/凱/生態資訊/git')

 
# 輸入資料 (tab 可顯示list 路徑)
library(data.table)
cwb <- fread('D:/凱/生態資訊/data/raw/200601_auto_hr.txt', skip=74 
             , na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))

# 設定欄位名稱(colnames <- c(固定格式)
#              setnames(目標,欄位,colnames)

colnames <- c("stno", "yyyymmddhr","PS01", "TX01", "RH01","WD01","WD02", "PP01", 
              "SS01")

setnames(cwb,1:9,colnames)

# 時間格式化(strptime(目標,'格式'))
# 建立新欄位 cwb[, timestamps:=as.POSIXct(strptime(yyyymmddhr,'%Y%m%d%H'))]

strptime(cwb$yyyymmddhr,'%Y%m%d%H')

cwb[,timestamp:=as.POSIXct(strptime(yyyymmddhr-1,'%Y%m%d%H'))]
}

## 開始aggregate(表示式,資料集,函數) 
{
  # 先抓出([ == ])目標測站'C0M530'資料，並整理時間標記(format.Date)去除hr

C0M530 <- cwb[cwb$stno == 'C0M530']

format.Date(C0M530$timestamp, '%Y-%m-%d')

C0M530[, date:= format.Date(timestamp, '%Y-%m-%d')]

# 設計funtion,計算mean(注意目前資料讀取為文字檔，需轉換成數值(as.numeric())，
#                                                   並去除'NA' = na.rm = TRUE)

mean_omit_na <- function(x){
  x <- as.numeric(x)
  return (mean(x, na.rm = T))
}

## DEMO: aggregate(iris$Petal.Length, by = list(iris$Species), FUN = mean)
}

### a.2006年每日日均溫daliy tmper (DTM)
{mean_DTM <- aggregate(as.numeric(C0M530$TX01), by = list(C0M530$date), 
                      FUN = mean_omit_na)
}

### b.2006年每日最低溫、最高溫Daliy Max/ min tmper (DMaxT/DminT)
{# 寫funtion 判定最大最小值

library(stats)
Max_T <- function(x){
    max(x, na.rm = TRUE)
}


min_T <-  function(x){
   min(x, na.rm = TRUE)
}

## use it

DMaxT <- aggregate(C0M530$TX01, by = list(C0M530$date), FUN = Max_T)

DminT <- aggregate(C0M530$TX01, by = list(C0M530$date), FUN = min_T)
}

## 

  