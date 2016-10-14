# 建立一個複雜的判斷 天氣預報
forecast <- function(){
  # 建立一個「天氣」向量
  x <- c('下雨', '颱風','下雪','晴天','陰天','冰雹')
  action <- c('帶傘','宅在家','出門玩')
  # 隨機排列，並且取出第一個天氣現象
  # 命名為 weather(變數)
  weather <- sample(x)[1]
  
  if( weather == '冰雹' | weather == '颱風') {
    #宅在家
    print(paste('現在天氣是', weather,'所以', action[2]))
  }
  
  else if( weather == '下雨' | weather == '下雪' ){
    #帶傘
    print(paste('現在天氣是', weather,'所以', action[1]))        
  }
  
  else {
    #出門玩
    print( paste('現在天氣是', weather, '所以', action[3]))
  } 
}
forecast()



## 迴圈
print(1)
print(2)
print(3)
print(4)
print(5)
print(6)

# 以 for 完成迴圈
for (i in 1:6) {
  print(i)  
}
print(100)

# 以 while 完成迴圈
  # 必須設定起始點，否則形成無限迴圈
i<-1
while (i<= 6) {
  print(i)
  # 設定自動+1
  i<- i + 1 
}

## 迴圈應用 費氏數列
## 0 1 2 3 5 8 13 21 34 55
n = 15
fib <- numeric(n)
fib[1] <- 0
fib[2] <- 1
fib[3] <- 2
fib[4] <- 3

for (i in 4:n) {
  fib[i] <- fib[i-2] +fib[i-1]
  }

if (i <= 3) {
  return(2)}

if (i <= 2){
  return(1)}

if (i <= 1){
  return(0)}

fib
  
  
