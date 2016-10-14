## week4---資料輸出
##  政府資料庫 http://data.gov.tw/ , get some metadata(詮釋資料)

setwd('D:/凱/生態資訊')
library(data.table)
library(curl) 

# 將網址當成變數
np_raw_url <- 'http://statis.moi.gov.tw/micst/stmain.jsp?sys=220&kind=21&type=1&funid=c0820101&cycle=4&outmode=2&utf=1&compmode=0&outkind=1&fldspc=0,6,&codspc0=0,10,&codlst1=111&rdm=cbfWIc9m&ym=8900&ymt=10200'

# 使用 curl 來下載資料

curl::curl_download('np_raw_url','data/np_raw.csv' )
# 讀取資料
np_raw <- read.csv('D:/凱/生態資訊/np_raw.csv')

# 轉換西元年, 先以gsub取代('特徵', '取代成', '目標'), 再用as.numeric 將資料轉換成數字才能作運算
#  1.先將資料欄位中有年的文字取代掉
roc_yr <- gsub('年', '',np_raw[,1])

# 2.再轉換成西元年
as.numeric(roc_yr) + 1911

# 3.取代原資料內容
np_raw[,1] <- as.numeric(roc_yr) + 1911

np_raw
