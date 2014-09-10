# 1. work dir
setwd("F:/Allan/R Stuff/Data")
setwd("D:/Allan/DropBox/RWorkingDir/Trading/Data/pairs")
library(TTR)

# 2. read 2 x files
# 2a. change date format 
# create fnc
c_p <- function(p1, p2){
  
  pair1 <- read.csv(p1,stringsAsFactors=F)
  pair1$Date <- as.POSIXct(pair1$Date,format='%d/%m/%Y')
  #pair1$Date <- as.POSIXct(pair1$Date,format='%Y-%m-%d')
  
  pair2 <- read.csv(p2,stringsAsFactors=F)
  pair2$Date <- as.POSIXct(pair2$Date,format='%d/%m/%Y')
  #pair2$Date <- as.POSIXct(pair2$Date,format='%Y-%m-%d')
  
  pp <- merge(pair1,pair2, by='Date')
  return(pp)
}

pair1 <- read.csv('boeing.csv',stringsAsFactors=F)
tail(pair1)
pair1$Date <- as.POSIXct(pair1$Date,format='%Y-%m-%d')

pr <- c_p('boeing.csv','gen_dyn.csv')
tail(pr)

# 3. merge 


# 4. add atr

atr <- ATR(pr[c(3,4,5)], n = 14)
tail(atr)
tail(atr[,c(1,2)])
pr <- cbind(pr, atr[,c(1,2)])
tail(pr)

# 5. add diffs

pr$diff <- pr$Close.x - pr$Close.y
tail(pr)

# 6. add St values
# a. add min and max -> TTR
# b. add st val

pr$diff_mx <- runMax(pr$diff, n=5)
tail(pr)

pr$diff_mn <- runMin(pr$diff, n=5)
tail(pr)

pr$st5 <- round((pr$diff-pr$diff_mn) / (pr$diff_mx - pr$diff_mn) * 100,0)
colnames(pr)
tail(pr[c(1,13,17)],n=20)

# 7. add MA of Diff
# a. ma
# b. ma diff
pr$diff_MA <- round(runMean(pr$diff,n=5),2)
tail(pr)
pr$diff_MAD <- pr$diff - pr$diff_MA
tail(pr)
tail(pr[c(1,12,17)],n=20)

write.csv(pr, 'boeing_northrop.csv',row.names=F)
