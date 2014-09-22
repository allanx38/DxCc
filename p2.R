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

add_details <- function(pr, dy){
  atr <- ATR(pr[c(3,4,5)], n = 14)
  pr <- cbind(pr, atr[,c(1,2)])
  pr$diff <- pr$Close.x - pr$Close.y
  pr$diff_mx <- runMax(pr$diff, n=dy)
  pr$diff_mn <- runMin(pr$diff, n=dy)
  pr$st5 <- round((pr$diff-pr$diff_mn) / (pr$diff_mx - pr$diff_mn) * 100,0)
  pr$diff_MA <- round(runMean(pr$diff,n=dy),2)
  pr$diff_MAD <- pr$diff - pr$diff_MA
  return(pr)
}

pair1 <- read.csv('boeing.csv',stringsAsFactors=F)
tail(pair1)
pair1$Date <- as.POSIXct(pair1$Date,format='%Y-%m-%d')

pr <- c_p('boeing.csv','gen_dyn.csv')
tail(pr)

# --- OIL ---
setwd("D:/Allan/DropBox/DxCc/oil")
pr <- c_p('exxon.csv','total.csv')
pr <- add_details(pr)
tail(pr)
write.csv(pr, 'exxon_total.csv',row.names=F)

# --- banks ---
setwd("D:/Allan/DropBox/DxCc/banks")
pr <- c_p('hsbc.csv','barclays.csv')
pr <- add_details(pr, 10)
tail(pr)
write.csv(pr, 'hsbc_barclays_10.csv',row.names=F)


# --- YAHOO Downloader
add_ta <- function(pr,dy){
  #browser()
  atr <- ATR(pr[c(3,4,5)], n = 14)
  pr <- cbind(pr, atr[,c(1,2)])
  pr[6] <- round(pr[6],1)
  pr[7] <- round(pr[7],1)
  pr$MA <- round(runMean(pr$Close,n=dy),2)
  pr$diff <- pr$Close - pr$MA
  #colnames(pr) <- c('Date','Open','High','Low','Close','TR','ATR','MA','Diff')
  return(pr)
}

pr <- read.csv('D:/VM Share/ind2/TLPR.L.csv',stringsAsFactors=F)
pr <- pr[c(1,2,3,4,5)]
colnames(pr) <- c('Date','Open','High','Low','Close')
tail(pr)
pr <- add_ta(pr,10)
write.csv(pr, 'D:/VM Share/ind2/TLPR.L_ma.csv',row.names=F)


pr2 <- pr[pr$diff < 0,]
tail(pr2)
head(pr2, n=30)
sum(pr2$Close - pr2$Open, na.rm = T)

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

# ----------------------------------------------
# Quandl - auth: mW11caB1btTqNnBWGhtg

setwd("D:/Allan/DropBox/RWorkingDir/Trading/Data/stocks")
install.packages("Quandl")
library(Quandl)
library(TTR)

# Sometimes a more recent version of the Quandl package may be available on Github. 
# (CRAN packages often update with a lag). 
# To download and install the latest version from Github, enter this code snippet:
# install.packages("devtools")
# library(devtools)
# install_github('R-package','quandl')
# library(Quandl)

# enter auth code
Quandl.auth("mW11caB1btTqNnBWGhtg")

# The Quandl package is able to return data in 4 very usable formats: 
# data frame ("raw"), 
# ts ("ts"), 
# zoo ("zoo") and 
#xts ("xts"). The default is "raw". 

GetStock <- function(days,stock,ma){
  end <- format(Sys.Date(), format="%Y-%m-%d")
  start <- format((Sys.Date() - days), format="%Y-%m-%d")
  mkt <- Quandl(stock, trim_start=start, trim_end=end,sort = "asc")
  mkt <- mkt[c(1,2,3,4,5)]
  colnames(mkt) <- c('Date','Open','High','Low','Close')
  mkt <- na.omit(mkt)
  atr <- ATR(mkt[c(3,4,5)], n = 14)
  mkt <- cbind(mkt, atr[,c(1,2)])
  mkt$atr <- round(mkt$atr)
  mkt$MA <- round(runMean(mkt$Close,n=ma))
  mkt$diff <- round(mkt$Close - mkt$MA)
  mkt$OC <- round(mkt$Close - mkt$Open)
  mkt$Tog <- ifelse(mkt$diff>0,mkt$Low-mkt$MA,mkt$MA-mkt$High)
  return(mkt)
}

tail(st)

head(st)
st <- GetStock(100,"LSE/NXT", 10)
st <- GetStock(1000,"YAHOO/RBGPF", 10) #Reckitt Benckiser Group PLC
st <- GetStock(1000,"LSE/SHP", 10) #Shire PLC
st <- GetStock(1000,"LSE/AZN", 10)

st <- GetStock(1000,"YAHOO/L_NXT", 10)
st <- GetStock(1000,"YAHOO/RBGPF", 10) # ??
st <- GetStock(1000,"YAHOO/L_SHP", 10)
st <- GetStock(1000,"YAHOO/L_AZN", 10)
st <- GetStock(1000,"YAHOO/L_RRS", 10)
st <- GetStock(1000,"YAHOO/L_JMAT", 10)
st <- GetStock(1000,"YAHOO/L_WTB", 10)
st <- GetStock(1000,"YAHOO/L_BATS", 10)
st <- GetStock(1000,"YAHOO/L_SAB", 10)
st <- GetStock(1000,"YAHOO/L_WOS", 10)

'YAHOO/L_WTB','YAHOO/L_BATS','YAHOO/L_SAB','YAHOO/L_WOS',
'YAHOO/L_RIO','YAHOO/L_JMAT'

write.csv(st, 'next.csv',row.names=F)


st$pdiff <- c( NA, st$diff[ - length(st$diff) ] )
st$pOC <- c( NA, st$OC[ - length(st$OC) ] )
st$BS <- ifelse(st$pdiff<0 & st$pOC>0, -1, ifelse(st$pdiff>0 & st$pOC<0,1,0))
tail(st)

sum(st[st$BS==1,10], na.rm = T)
-sum(st[st$BS== -1,10], na.rm = T)

dn <- st[st$BS== -1,10]
sum(dn, na.rm = T)
length(dn)
head(dn, n=20)

length(st[st$BS== -1,10],na.rm = T)

sum(dn, na.rm = T)

write.csv(st, 'next2.csv',row.names=F)

end <- format(Sys.Date(), format="%Y-%m-%d")
start <- format((Sys.Date() - 50), format="%Y-%m-%d")
ydax = Quandl("LSE/NXT", start_date=start, end_date=end)
nxt <- Quandl("LSE/NXT", trim_start="2014-02-13", trim_end="2014-09-19")
nxt <- Quandl("YAHOO/L_AZN", trim_start=start, trim_end=end)
tail(nxt)
nxt2 <- nxt[c(1,2,3,4,6)]
class(nxt2)
colnames(nxt2) <- c('Date','Open','High','Low','Close')
tail(nxt2)
head(nxt2)

nxt2 <- na.omit(nxt2)

atr <- ATR(nxt2[c(3,4,5)], n = 14)
nxt2 <- cbind(nxt2, atr[,c(1,2)])

nxt2$MA <- round(runMean(nxt2$Close,n=10),2)
nxt2$diff <- nxt2$Close - nxt2$MA

write.csv(nxt2,"Next.csv", row.names=FALSE)