setwd('/Users/Eric/Desktop/凱/生態資訊')
# Week2 +-*/
## 矩陣定義 A<-matrix(c(x,y,z,...),n行,n列), B<-matrix(c(x,y,z,...),n行,n列)
## 矩陣運算"dot"  A%*%B
### ":" 用法, seq(from=x, to=y, by=z)
##"="or"<-"皆可做為定義

###小考Fibonacci數列
#
#}

## "[]" 可撈出數據or   ex: rnorm(10)
##                     rn=rnorm(10)
##                     rn[rn>1]
# 費氏數列
n <- 55
fib <- numeric(n)
fib[1] <- 1
fib[2] <- 1
for (x in 3:n) {
  fib[x]= fib[x-1] + fib[x-2]
}

fib

