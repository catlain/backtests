require(quantstrat)
require(IKTrading)
require(rlang)
require(dplyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(purrr)
# require(PerformanceAnalytics)
# require(FinancialInstrument)
# suppressWarnings(rm("order_book.highlow", pos = .strategy))
# suppressWarnings(rm("account.highlow", "portfolio.highlow", pos = .blotter))
# suppressWarnings(rm("account_st", "portfolio_st", "stock_str"))

rm(list = ls(.blotter), envir = .blotter)
rm(list = ls(.strategy), envir = .strategy)
source("highlow.R")

stock_str <- "601166.SS" # 需要回测的股票
init_date <- "2001-01-01"
# 从雅虎取得股票数据
# getSymbols(stock_str, from = init_date, index.class=c("POSIXt","POSIXct"))
# stock_df <- na.omit(adjustOHLC(get(stock_str), use.Adjusted = TRUE))
assign(stock_str, na.omit(adjustOHLC(get(stock_str), use.Adjusted = TRUE)))

# 设定货币为人民币
# rm_instruments()
# ls_instruments

currency("CNY")
Sys.setenv(TZ = "UTC")
stock(stock_str, currency = "CNY", multiplier = 1)

# 设定资金量及最大头寸
trade_size <- 100000
init_eq <- trade_size * length(stock_str)
pct_atr <- 0.2
n_atr <- 10
n_ema <- 5
period_highlow <- "day"
n_high <- 180
n_low <- 120
buyCost <- 0.001425
sellCost <- 0.004425

# 手续费
buyFee <- function(TxnQty, TxnPrice, Symbol, ...)
{
  abs(TxnQty) * TxnPrice * -buyCost
}

sellFee <- function(TxnQty, TxnPrice, Symbol, ...)
{
  abs(TxnQty) * TxnPrice * -sellCost
}

# 初始化
strategy_st <- portfolio_st <- account_st <- strat_st <-  "highlow"
rm.strat(portfolio_st)
rm.strat(strategy_st)
initPortf(portfolio_st, symbols = stock_str, initDate = init_date, currency = "CNY")
initAcct(account_st, portfolios = portfolio_st, init_eq = init_eq, initDate = init_date, currency = "CNY")
initOrders(portfolio = portfolio_st, initDate = init_date)

# 设定策略
strategy(strategy_st, store = TRUE)

# 指标
# TODO 使用 lagATR 进行仓位管理
# add.indicator(strategy_st, name = "ATR",
#               arguments = list(HLC = quote(HLC(mktdata)), n = n_atr),
#               label = "atr")

add.indicator(strategy_st, name = "EMA",
              arguments = list(x = quote(Cl(mktdata)), n = n_ema),
              label = "EMAL")

add.indicator(strategy_st, name = "highlow", 
              arguments = list(x = quote(Ad(mktdata)), period = period_highlow, n = n_high), 
              label = "High")

add.indicator(strategy_st, name = "highlow", 
              arguments = list(x = quote(Ad(mktdata)), period = period_highlow, n = n_low), 
              label = "Low")


# 信号

add.signal(strategy_st, name = "sigFormula",
           arguments = list(data = quote(mktdata),
                            formula = "EMA.EMAL > high_last_period.High"), 
           label = "BuySignal"
)

add.signal(strategy_st, name = "sigFormula",
           arguments = list(data = quote(mktdata),
                            formula = "EMA.EMAL < low_last_period.Low"), 
           label = "SellSignal"
)

# add.signal(strategy = strategy_st, name = "sigCrossover", 
#            arguments = list(columns = c("EMA.EMAL", "high_last_period.HighLow"), 
#                             relationship = "gt"), 
#            label = "BuySignal")
# 
# add.signal(strategy = strategy_st, name = "sigCrossover", 
#            arguments = list(columns = c("EMA.EMAL", "low_last_period.HighLow"), 
#                             relationship = "lt"),
#            label = "SellSignal")

# 进出场
add.rule(strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "BuySignal",
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = TRUE, #  replace = TRUE may choose just one rule
                          prefer = "Open", #  tomorrow because today has closed
                          osFUN = osMaxDollar, #  order size function
                          tradeSize = trade_size,
                          maxSize = trade_size,
                          pct_atr = pct_atr,
                          maxpct_atr = pct_atr, #  set an upper limit of orders
                          atrMod = "",
                          TxnFees = "buyFee"), #  atrx above
         type = "enter", path.dep = TRUE, label = "EnterRule")


add.rule(strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "SellSignal",
                          sigval = TRUE,
                          orderqty = "all", #  order quantity, in all and out all
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          TxnFees = "sellFee"),
         type = "exit", path.dep = TRUE, label = "ExitRule")




# add.rule(strategy = strategy_st, name = 'ruleSignal', 
#          arguments = list(sigcol = "BuySignal", 
#                           sigval = TRUE, 
#                           ordertype = 'market', 
#                           orderside = 'long', 
#                           orderqty = "all",
#                           prefer =  "Close"), 
#          type = 'enter', label = "EnterRule", enabled = T)

# add.rule(strategy = strategy_st, name = 'ruleSignal', 
#          arguments = list(sigcol = "SellSignal", 
#                           sigval = TRUE, 
#                           orderqty = 'all', 
#                           ordertype = 'market', 
#                           orderside = 'long', prefer = "Close"), 
#          type = 'exit', label = "ExitRule", enabled = T)

out <- applyStrategy(strategy = strategy_st, portfolios = portfolio_st, symbols = stock_str)

# 更新账户资料和权益金额
updatePortf(Portfolio = portfolio_st)
tradeDetails <- getPortfolio(portfolio_st)
posPL <- tradeDetails$symbols$tsmc$posPL
dateRange <- time(tradeDetails$summary)[-1]
updateAcct(portfolio_st, dateRange)
updateEndEq(account_st)

# TODO 检查没有成交的原因
# trend1_book <- getOrderBook(portfolio = strategy_st)
# trend1_book


# 統計資料
tStats <- tradeStats(Portfolios = portfolio_st, use = "trades", inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgwLR <- mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE))

# 畫k線圖、進出場flag、持有部位和累積報酬
myTheme <- chart_theme()
myTheme$col$dn.col<-'lightgray'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

chart.Posn(Portfolio = portfolio_st, Symbol = stock_str, theme = myTheme)
