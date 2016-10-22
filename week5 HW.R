#  week5 HW aggregate 
#  all files open _tips: list.files

## 題目：
### 1.計算C0M530(奮起湖)從2006~2015十年的
###   a.日均溫、b.日低溫、c.日高溫、d.月均溫、e.月累積降水


#### 2.計算最暖月每日最高均溫

##### 3.計算最冷月每日最低均溫

# load all of the txt (non-finished)
{
library(data.table)
library(stats)

all_files <- list.files("D:/凱/生態資訊/data/raw/" , pattern = "auto_hr")

substr(all_files,1,120)

read_data <-  function(x){
  data_open_clean <- fread(x, skip = 74)
  return(data_open_clean)
}
      
for (i in all_files){
  temp.data <- fread(i)
  data <- data.table(rbind(data,temp.data))
}      

mylist <- lapply(all_files, read_data)
mydata <- do.call('rbind',mylist)   

}
---------------------
    

##  前置作業
{
library(data.table)
library(stats)  
setwd('D:/凱/生態資訊/git')

 
# 輸入資料 (tab 可顯示list 路徑)

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

### b.c.2006年每日最低溫、最高溫Daliy Max/ min tmper (DMaxT/DminT)
{# 寫funtion 判定最大最小值

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

### d.2006年1月均溫(month temper (MTM))
{
# 新增欄位"month"

C0M530[, month:= format.Date(timestamp, '%Y-%m')]  

#  同a.計算法
mean_MTM <- aggregate(as.numeric(C0M530$TX01), by = list(C0M530$month),
                      FUN = mean_omit_na)
}

### e.2006年1月累積降水(sum)
{
# 新增funtion(sum)

sum_omit_na <- function(x){
  x <- as.numeric(x)
  return (sum(x, na.rm = TRUE))
}


# 計算總和(sum month rain(SMR))

sum_SMR <- aggregate(as.numeric(C0M530$PP01), by = list(C0M530$month), 
                      FUN = sum_omit_na) 
}



  