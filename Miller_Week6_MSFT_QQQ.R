# install.packages("quantmod")
library(quantmod)
#Load MSFT
getSymbols("MSFT", src = "yahoo", from = "2025-01-01")
getSymbols("QQQ", src = "yahoo", from = "2025-01-01")
# Plot Volume 
plot(MSFT$MSFT.Volume,
               type = "l",
               main = "MSFT Daily Volume (Jan 2025 - Feb 2026)")
#Deviation Analysis
firstDayCloseM <- as.numeric(MSFT["2025-01-02"]$MSFT.Close) #Find benchmark
MSFT$pChg <- (MSFT$MSFT.Close - firstDayCloseM) / firstDayCloseM * 100 #Set values to benchmark
# Plot Deviation Analysis
plot(MSFT$pChg,
               type = "l",
               main = "MSFT Deviation Analysis (Jan 2, 2025 Benchmark)",
               ylab = "Price Fluctuation (%)")
#Deviation Analysis for QQQ
firstDayCloseQ <- as.numeric(QQQ["2025-01-02"]$QQQ.Close) #Find benchmark
QQQ$pChg <- (QQQ$QQQ.Close - firstDayCloseQ) / firstDayCloseQ * 100 #Set values to benchmark

#Plot QQQ with MSFT 
plot(MSFT$pChg,
     type = "l",
     main = "MSFT (blue) vs. QQQ (red) (Jan 2, 2025 Benchmark)",
     ylab = "Share Price Fluctuation (%)",
     col = "blue",
     )
lines(QQQ$pChg, col = "red")
