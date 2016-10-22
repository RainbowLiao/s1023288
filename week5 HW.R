#  week5 HW -- aggregate 
#  all files open _tips: list.files
#  紅點點為執行提示

------------------目標-----------------------------------------
## 題目：
### 1.計算C0M530(奮起湖)從2006~2015十年的
###   a.日均溫、b.日低溫、c.日高溫、d.月均溫、e.月累積降水


#### 2.計算最暖月每日最高均溫

##### 3.計算最冷月每日最低均溫


setwd('D:/凱/生態資訊/git')

# load all of the txt (2016/10/22/ 11.30 am 終於成功!) 
 # 參考stack overflow上的各種網友模組
{
library(data.table)
library(stats)
library(readr)  

# 選擇所有檔案資料夾,設定資料外觀( ,full.name 才能加入路徑)  
  #注意!!! <讀取超久有當機可能>!!!
all_files <- list.files("D:/凱/生態資訊/data/raw/" , pattern = "auto_hr", full.names = TRUE)

data_menu <- substr(all_files,1,120)

read_data <-  function(x){
  for (i in all_files){
    temp.data <- fread(i)
    data <- data.table(rbind(data,temp.data))
    data_open_clean <- fread(x, skip = 74)
    return(data_open_clean)
  }
}      

file_list <- lapply(data_menu, fread, skip = 74, stringsAsFactors=FALSE, 
                     na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))

mydata <- do.call('rbind',file_list)   

}

# <注意>!!!!! 成功之後不要view  "mydata" 檔案太大打開會爆機

------------------答案檢視---------------------------------------
# <答案檢視>   
# 1. a. mean_DTM , b. DminT  , c. DMaxT , d. mean_MTM , e. sum_SMR
# 2. highest_T
# 3. lowest_T 
  
  
  
------------------計算方法---------------------------------------  

  ##  前置作業(整理表格)
{
# 設定欄位名稱(colnames <- c(固定格式)
#              setnames(目標,欄位,colnames)

colnames <- c("stno", "yyyymmddhr","PS01", "TX01", "RH01","WD01","WD02", "PP01", 
              "SS01")

setnames(mydata,1:9,colnames)


# <注意>!!!!! mydata 檔案太大，先把C0M530抓出來才能做欄位新增
# 先抓出([ == ])目標測站'C0M530'資料

C0M530 <- mydata[mydata$stno == 'C0M530']

# 時間格式化(strptime(目標,'格式'))

strptime(C0M530$yyyymmddhr,'%Y%m%d%H')


## 建立新欄位 C0M530[, timestamps:=as.POSIXct(strptime(yyyymmddhr,'%Y%m%d%H'))]

C0M530[,timestamp:=as.POSIXct(strptime(yyyymmddhr-1,'%Y%m%d%H'))]

#  整理時間標記(format.Date)去除hr

format.Date(C0M530$timestamp, '%Y-%m-%d')

C0M530[, date:= format.Date(timestamp, '%Y-%m-%d')]

-----------------------至此完成資料表格整理-------------------------------

}

## 開始aggregate(表示式,資料集,函數) 

## DEMO: aggregate(iris$Petal.Length, by = list(iris$Species), FUN = mean)


### 1.計算C0M530(奮起湖)從2006~2015十年的：
###  a.每年每日日均溫daliy tmper (DTM)
{
# 設計funtion,計算mean(注意目前資料讀取為文字檔，需轉換成數值(as.numeric())，
#                                                   並去除'NA' = na.rm = TRUE)
  
mean_omit_na <- function(x){
   x <- as.numeric(x)
  return (mean(x, na.rm = T))
}
  
  
mean_DTM <- aggregate(as.numeric(C0M530$TX01), by = list(C0M530$date), 
                      FUN = mean_omit_na)
}

 ### b.c.每年每日最低溫、最高溫Daliy Max/ min tmper (DMaxT/DminT)
{
# 寫funtion 判定最大最小值

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

 ### d.每年每月均溫(month temper (MTM))
{
# 新增欄位"month"

C0M530[, month:= format.Date(timestamp, '%Y-%m')]  

#  同a.計算法
mean_MTM <- aggregate(as.numeric(C0M530$TX01), by = list(C0M530$month),
                      FUN = mean_omit_na)
}

 ### e.每年每月累積降水(sum)
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

#### 2.計算最暖月每日最高均溫  (higest_T)
{
# 用order來排序mean_MTM 找出最暖月(Highest_Temper_of_month_peryear(HTOMY))，
#                         及最冷月(Lowest_Temper_of_month_peryear(LTOMY))

setorder(mean_MTM, x)

order_MTM <- mean_MTM[order(rank(x , y))]

# 從 'order_MTM' 可得 "最冷出現在'2009-01'" , "最熱出現在'2014-07'"

## 再從最暖月'2014-07'中找尋最高溫的一天，重複上步方法，排序'2014-07'找出MAX
## 先從C0M530將 '2014-07' 取出 再做排序

HTOMY <- C0M530[C0M530$month == '2014-07']

Max_2014.07 <- aggregate(as.numeric(HTOMY$TX01), by = list(HTOMY$date), FUN = mean_omit_na)

setorder(Max_2014.07, x)

order_Max.2014.07 <- Max_2014.07[order(rank(x , y))]

# 從 'order_Max.2014.07' 可得 "2014.07.31" 最熱
# 再將抓出該日

highest_T <- Max_2014.07[31,]
}
##### 3.計算最冷月每日最低均溫
{
# 根據162行可知"最冷出現在'2009-01'"
# 重複最暖月及最暖日找法

LTOMY <- C0M530[C0M530$month == '2009-01']

Min_2009.01 <- aggregate(as.numeric(LTOMY$TX01), by = list(LTOMY$date), FUN = mean_omit_na)

setorder(Min_2009.01, x)

order_Min_2009.01 <- Min_2009.01[order(rank(x , y))]

# 從 'order_Min_2009.01' 可得 "2009.01.11" 最冷
# 再將抓出該日  

lowest_T <- Min_2009.01[ 1 , ]
}

# 於2016.10.22 16.25 完成