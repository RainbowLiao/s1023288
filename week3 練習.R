# 天氣練習: function 試寫
'內心的宅宅說' <- function(){
  x <- c('晴天', '陰天', '雨天', '雪天', '颱風', '冰雹')
  wheather <- function()
  action <- c('該抓寶囉~', '還是宅在家看動畫好了', '帶傘出門玩')
  
  #將天氣向量x 抓一隻出來"[]"
  wheather <- sample(x)[1]
  
  #最後加上sep=""可去除空格
  
  if (wheather == '晴天' | wheather == '陰天') {
    print(paste('現在外頭是', wheather ,'所以', action[1], sep=""))
  }
  else if (wheather == '雨天' | wheather == '雪天') {
    print(paste('現在外頭是', wheather ,'所以', action[3], sep=""))
  }
  else{
    print(paste ('現在外頭是', wheather ,'所以', action[2], sep=""))
  }
}
# 要使用該function則輸入 "內心的宅宅說()"
內心的宅宅說()


##迴圈數列練習:費氏數列, for 應用 
{## 0 1 1 2 3 5 8 13 21 34
##先定義 n , 前四項,及費氏數列函數 fib 所需要之空格

n = 50
fib <- numeric(n)
fib[1] = 0
fib[2] = 1
fib[3] = 1  
fib[4] = 2

##定義方程式 "fib[i]" 以[]指定項定義
for (i in 3:n) {
  fib[i] = fib[i-1] + fib[i-2]
}

##求完善數列需補充前三項規則 以if定義條件

if (i <= 1){
  return(0)}

if (i <= 2){
  return(1)}

if (i <= 3){
  return(1)}
}
fib
