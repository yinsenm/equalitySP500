## install packages: If installed, then loading
{
  list.of.packages <- c("scales", "reshape2", "xts", "ggplot2", "plyr", "lubridate",
                        "latex2exp", "quantmod", "xtable", "latex2exp", "ggthemes", 
                        "PerformanceAnalytics")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)
  
  setwd("C:/Users/yinse/Desktop/equalitySP500-master/")
  
  # A list of transformation to apply
  funcList = list(function(x) x, function(x) indict(x))
  funcStr  = c("MKC", "EQU")
  psntTransPerStock = 1
  
  load("./SP500TimeSeries.Rdata")
  
}


## Indicator Function: For EQU====
indict <- function(x){
  naidx = which(is.na(x))
  y = rep(1, length(x))
  y[naidx] = NA
  y
}

## Reccession Periods====
recessions.df = read.table(textConnection(
  "Peak, Trough
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

## dollarInflate function for computing money worth ====
dollarInflate = function(bgn, end, bgn_val, CPI = CPI) {
  bgn = as.Date(bgn)
  end = as.Date(end)
  bgnCPI = as.numeric(CPI[bgn])
  endCPI = as.numeric(CPI[end])
  bgn_val * (bgnCPI / endCPI)
} 


## Compute SPX and SPW Culmulative Returns
SPW = as.numeric(SP500["1958-01-02/2016-12-30","ewretd"])
SPX = as.numeric(SP500["1958-01-02/2016-12-30","vwretd"])

# Compute Cumulative Return
CulSPW = cumprod(1+SPW) * 10^5 
CulSPX = cumprod(1+SPX) * 10^5



## get return list for one strategy====
getReturn = function(FUNC = function(x) 1/x^2, PRCS, SHRS, RETS, PERIOD) {
  dataDate = index(PRCS)
  PRCM = t(as.matrix(PRCS))
  SHRM = t(as.matrix(SHRS))
  RETM = t(as.matrix(RETS))
  addNum = 0
  delNum = 0
  chgNum = 0
  money = 0
  nBalanced = 0
  WeightList   = list()
  BalancedDate = list()
  AdminList    = list()
  BidAskList   = list()
  TransacList  = list()
  CumuReturn   = matrix(0, nrow(PRCS), 1)
  DayReturn    = matrix(0, nrow(PRCS), 1)
  CumuReturn[1] = dollarInflate(dataDate[1], as.Date("1958-01-02"), 10^5, CPI = CPI)
  
  
  if(PERIOD == "Daily") {
    periodFunc = day
  } else if (PERIOD == "Monthly") {
    periodFunc = month
  } else if (PERIOD == "Quarterly") {
    periodFunc = quarter
  } else if (PERIOD == "Yearly") {
    periodFunc = year
  } else {
    stop("Period can only be chosen from Daily, Monthly Quaterly, Yearly")
  }
  
  PeriodADJ = periodFunc(dataDate[1]) + 1
  Weight = PRCM[,1] * SHRM[,1] / sum(PRCM[,1] * SHRM[,1], na.rm = T)
  NewWeight = FUNC(Weight) / sum(FUNC(Weight), na.rm = T)
  NewWeight[is.na(NewWeight)] = 0
  ## save NewWeight and rebalanced date
  nBalanced = nBalanced + 1
  WeightList[[nBalanced]] = NewWeight
  BalancedDate[[nBalanced]] = as.character(dataDate[1])
  

  for(i in c(2:nrow(PRCS))) {
    # print(i)  
    DayReturn[i] = sum(RETM[,i] * NewWeight, na.rm = T)
    
    ## account for deleted and added companies
    bgn = which(is.na(PRCM[, i-1]) == F)
    end = which(is.na(PRCM[, i]) == F)
    deleted = setdiff(bgn, end)
    added   = setdiff(end, bgn)
    
    if(length(deleted) != 0 & length(added) != 0) {
      chgNum = chgNum + 1;
    } else if (length(deleted) == 0 & length(added) != 0) {
      addNum = addNum + 1;
    } else if (length(deleted) != 0 & length(added) == 0) {
      delNum = delNum + 1;
    }
    
    if(length(deleted) != 0 | length(added) != 0) {
      Weight = PRCM[,i] * SHRM[,i] / sum(PRCM[,i] * SHRM[,i], na.rm = T)
      NewWeight = FUNC(Weight) / sum(FUNC(Weight), na.rm = T)
      NewWeight[is.na(NewWeight)] = 0
    }
    
    if(PERIOD == "Daily" | periodFunc(dataDate[i]) == PeriodADJ) {
      nBalanced = nBalanced + 1
      Weight = PRCM[,i] * SHRM[,i] / sum(PRCM[,i] * SHRM[,i], na.rm = T)
      NewWeight =  FUNC(Weight) / sum(FUNC(Weight), na.rm = T)
      NewWeight[is.na(NewWeight)] = 0
      WeightList[[nBalanced]] = NewWeight
      BalancedDate[[nBalanced]] = as.character(dataDate[i])
      prc = PRCM[,i]
      bidaskspd = PRCM[,i] / 2000
      money = CumuReturn[i-1]
      diffMoney = money  * (WeightList[[nBalanced - 1]]  - WeightList[[nBalanced]])
      
      adminFee      = -dollarInflate(dataDate[i], dataDate[length(dataDate)], psntTransPerStock, CPI = CPI)  * sum(!is.na(prc), na.rm = T)
      bidaskspdFee  = -sum((abs(diffMoney / prc) * bidaskspd), na.rm = T)
      transactionFee=  adminFee + bidaskspdFee
      CumuReturn[i] = (money + transactionFee) *  (1 + DayReturn[i])
      AdminList[[nBalanced]]   = adminFee
      BidAskList[[nBalanced]]  = bidaskspdFee
      TransacList[[nBalanced]] = transactionFee
    } else {
      CumuReturn[i] = CumuReturn[i-1] *  (1 + DayReturn[i])
    }  
    
    if(PERIOD != "Daily" & periodFunc(dataDate[i]) == PeriodADJ) {
      PeriodADJ = PeriodADJ + 1
      if(PERIOD == "Monthly" & PeriodADJ == 13) {
        PeriodADJ = 1
      } else if(PERIOD == "Quarterly" & PeriodADJ == 5) {
        PeriodADJ = 1
      }
    }      
  }
  list(CumuReturn = xts(CumuReturn, order.by = dataDate), 
       DayReturn = xts(DayReturn, order.by = dataDate), 
       nBalanced = nBalanced, 
       ChangeStat = c(addNum, delNum, chgNum),
       Admin= sum(unlist(AdminList)),
       BidAsk = sum(unlist(BidAskList)), Transaction = sum(unlist(TransacList)))
}


## get return mat for multiple strategies====
getReturnMat <- function(FROM, TO, PERIOD, funclist) {
  SHRSS = abs(SHRT[paste0(FROM,"/",TO)])
  PRCSS = abs(PRCT[paste0(FROM,"/",TO)])
  RETSS = RET[paste0(FROM,"/",TO)]
  
  ResultList = lapply(funcList, function(x) {
    print(x)
    getReturn(x, PRCSS, SHRSS, RETSS, PERIOD)
  })
  CumuList = lapply(ResultList, function(x) x$CumuReturn)
  CumuMat = Reduce(cbind, CumuList)
  colnames(CumuMat) = funcStr
  nBalanced = ResultList[[1]]$nBalanced
  ChangeStat = ResultList[[1]]$ChangeStat
  Admin = unlist(lapply(ResultList, function(x) x$Admin))
  BidAsk = unlist(lapply(ResultList, function(x) x$BidAsk))
  Transaction = unlist(lapply(ResultList, function(x) x$Transaction))
  list(CumuMat = CumuMat, nBalanced = nBalanced, ChangeStat = ChangeStat,
       Admin = Admin, BidAsk = BidAsk, Transaction = Transaction)
}

FROM = as.Date("1958-01-02")
TO = as.Date("2016-12-30")
CumuResultList = getReturnMat(FROM, TO, "Monthly", funcList)


## Cumulative Return for MKC and EQU
CumuResultList$CumuMat
CumuResultList$Admin
CumuResultList$BidAsk
CumuResultList$Transaction


FROM = as.Date("1957-03-02")
TO = as.Date("2016-12-30")
SHRS = abs(SHRT[paste0(FROM,"/",TO)])
PRCS = abs(PRCT[paste0(FROM,"/",TO)])
RETS = RET[paste0(FROM,"/",TO)]

## Compute Index Daily Return Rate
IndexRate = function(func = indict, SHRS, PRCS, RETS){
  DailyRate = sapply(2:nrow(SHRS), function(i){
    Weight = as.numeric(PRCS[i-1,]) * as.numeric(SHRS[i-1,]) / sum(as.numeric(PRCS[i-1,]) * as.numeric(SHRS[i-1,]), na.rm = T)
    NewWeight = func(Weight) / sum(func(Weight), na.rm = T)
    sum(RETS[i,] * NewWeight, na.rm = T)
  })
  c(0, DailyRate)
}

## Select 20 stocks use MaxMedian and then Compute MaxMedian Daily Return Rate
MaxMedianRate = lapply(1957:2015, function(Y) {
  idx = year(index(PRCS)) == Y
  SelPRCS = as.matrix(PRCS[idx,])
  selecTop <- function(SelPRCS, N = 20) {
    DayRatios = lapply(2:nrow(SelPRCS), function(i) SelPRCS[i,] / SelPRCS[i-1,])
    R = matrix(unlist(DayRatios), byrow = T, ncol = ncol(PRCS)) # daily Return
    
    # avoid 1
    Median = apply(R, 2, function(x) {
      tmp = x
      tmp[tmp == 1] = NA
      median(tmp, na.rm = T)
    })
    
    Median[is.na(SelPRCS[nrow(SelPRCS),]) & apply(SelPRCS, 2, function(x) sum(is.na(x))) > 10] = NA
    top = head(order(Median, decreasing = T), N)
    top
  }
  
  # select top 20 max-median
  top = selecTop(SelPRCS, N = 20)
  selidx = (year(index(PRCS)) == (Y+1))
  SHRSS = SHRS[selidx, top]
  PRCSS = PRCS[selidx, top]
  RETSS = RETS[selidx, top]
  funclist = list(function(x) indict(x))
  
  # Compute Daily Return Rate
  Rate = lapply(funclist, function(x) {
    IndexRate(func = x, SHRS = SHRSS, PRCS = PRCSS, RETS = RETSS)
  })
  RateMatrix = matrix(unlist(Rate), ncol = length(funclist))
  list(rate = RateMatrix, top = top, date = index(PRCSS)[1])
})


## compute MaxMedian Cumulative return
initMoney <<- dollarInflate(as.Date("1958-01-02"), as.Date("1958-01-02"), 10^5, CPI = CPI)
MaxMedianReturn = lapply(1:length(MaxMedianRate), function(x) {
  top  = MaxMedianRate[[x]]$top
  Year = MaxMedianRate[[x]]$date
  rate = MaxMedianRate[[x]]$rate
  bidaskspdFee   = dollarInflate(Year, as.Date("1958-01-02"), 200, CPI = CPI)
  adminFee       = dollarInflate(Year, as.Date("1958-01-02"), 20 * psntTransPerStock, CPI = CPI)
  transactionFee = bidaskspdFee + adminFee 
  money <<- (initMoney - transactionFee) * cumprod(1 + rate) 
  initMoney <<- money[length(money)]
  list(money = money, adminFee = adminFee, bidaskspdFee = bidaskspdFee)
})
  
MaxMedianCulmulative = unlist(lapply(MaxMedianReturn, function(x) x$money))
MaxMedianAdmin  = sum(unlist(lapply(MaxMedianReturn, function(x) x$adminFee)))
MaxMedianBidask = sum(unlist(lapply(MaxMedianReturn, function(x) x$bidaskspdFee)))



# ReturnSeries has MKC, EQU, MaxMedian, SPW and SPX
ReturnSeries = cbind(CumuResultList$CumuMat, MaxMedianCulmulative, CulSPW, CulSPX)
colnames(ReturnSeries) = c("MKC", "EQU", "MaxMedian", "SPW", "SPX")


getCumuPlt(Year = 5, ReturnSeries[, c("EQU", "MKC", "MaxMedian")], "MaxMedian")
getCumuPlt(Year = 5, ReturnSeries[, c("MKC", "EQU", "SPW", "SPX")], "Compare")
getCumuPlt(Year = 5, ReturnSeries[, c("EQU", "MKC")], "EQUMKC")



## Print Cumulative Return Figure ====
getCumuPlt = function(Year = 5, ReturnSeries, tagg) {
  FROM =  head(index(ReturnSeries), 1)
  TO   =  tail(index(ReturnSeries), 1)
  label = colnames(ReturnSeries)
  tag = paste(format(FROM, "%Y_%m"), format(TO, "%Y_%m"), tagg, sep = "_")
  ReturnMatrix = as.data.frame(ReturnSeries)
  ReturnMatrix$Datadate = index(ReturnSeries)
  df <- melt(ReturnMatrix, id.vars="Datadate", value.name = "values")
  ord = order(ReturnMatrix[nrow(ReturnMatrix),-ncol(ReturnMatrix)], decreasing = T)
  df$variable <- factor(df$variable,levels = levels(df$variable)[ord])
  recess = recessions.df[recessions.df$Peak > FROM, ]
  ## log base = 10
  plt = ggplot(df) + geom_line(aes(x = Datadate, y = log10(values),
                                   colour = variable), size = .4) + 
    geom_rect(data = recess, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill='red', alpha=0.1) +
    scale_x_date(breaks = date_breaks(paste(Year, "years")), labels = date_format("%Y")) +
    labs(title = paste("S&P 500 from", format(FROM, "%Y-%m-%d"), "to", 
                       format(TO, "%Y-%m-%d")), x = "Year", y = TeX("$\\log_{10}$(Dollars)")) +
    scale_color_discrete(name="Weighting Methods", breaks = levels(df$variable),labels = label[ord]) +
    scale_y_continuous(breaks = seq(1, round(log10(max(df$value)), digits = 3) + 0.25, by = 0.5)) +
    # scale_y_log10() +
    theme_bw() +
    # theme_minimal() +
    theme(legend.position="top", 
          plot.title = element_text(size = 20, face = "bold", hjust = 0),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15)) +
    guides(col = guide_legend(nrow = 1, byrow = T))
  plt
  dir.create(sprintf("./%s", tag), showWarnings = FALSE)
  ggsave(paste0("./", tag, "/", "Cumulative_", tag, "_Log10.pdf"), height = 6, width = 10)
  plt
}


getCumuPrt(ReturnSeries)
## Print Cumulative return Table ====
getCumuPrt = function(ReturnSeries) {
  FROM =  head(index(ReturnSeries), 1)
  TO   =  tail(index(ReturnSeries), 1)
  ReturnMatrix = as.data.frame(ReturnSeries)
  # ReturnMatrix$Datadate = index(ReturnSeries)
  tag = paste(format(FROM, "%Y_%m"), format(TO, "%Y_%m"), sep = "_")
  bgnInvest = ReturnMatrix[1,1]
  tempM = tail(ReturnMatrix, 10)
  # rownames(tempM) = index(ReturnSeries)
  caption = sprintf("The cumulative for MaxMedian, EQU and MKC portfolios from %s (assumed $%.3f invested) to %s.", 
                    as.character(FROM), bgnInvest, as.character(TO))
  print(xtable(tempM, caption = caption, digits = 2),
        file = paste0("./", tag, "/Cumulative_", tag, "_RET.tex"))
  print(xtable(tempM, caption = caption, digits = 2), type = "html",
        file = paste0("./", tag, "/Cumulative_", tag, "_RET.html"))
  print(xtable(tempM, display = rep("E", ncol(ReturnSeries) + 1), caption = caption, digits = 2), 
        file = paste0("./", tag, "/Cumulative_", tag, "_RETExp.tex"))
  print(xtable(tempM, display = rep("E", ncol(ReturnSeries) + 1), caption = caption, digits = 2), type = "html",
        file = paste0("./", tag, "/Cumulative_", tag, "_RETExp.html"))
  return(NULL)
}


## Compute annual performance and save them into local directory===
getPerformance = function(ReturnSeries) {
  FROM =  head(index(ReturnSeries), 1)
  TO   =  tail(index(ReturnSeries), 1)
  tag = paste(format(FROM, "%Y_%m"), format(TO, "%Y_%m"), sep = "_")
  
  ## Compute Table 14 & 15===
  YearReturn = vector("list", ncol(ReturnSeries))
  for(i in 1:length(YearReturn)) {
    tda = ReturnSeries[,i]
    YearReturn[[i]] = annualReturn(tda)
  }
  M = matrix(unlist(YearReturn), ncol = ncol(ReturnSeries))
  AnnualReturn = as.data.frame(M)
  colnames(AnnualReturn) = colnames(ReturnSeries)
  rownames(AnnualReturn) = unique(year(index(ReturnSeries)))
  dir.create(sprintf("./%s", tag), showWarnings = FALSE)
  print(xtable(AnnualReturn * 100, digits = 2), 
        file = paste0("./", tag, "/AnnualReturn", tag, ".tex"))
  print(xtable(AnnualReturn * 100, digits = 2), type = "html", 
        file = paste0("./", tag, "/AnnualReturn", tag, ".html"))
  
  funcstr = colnames(ReturnSeries)
  ## Compute Table 8, 9, 10 and Table 12, 13
  ## Compute return matrix based for different periods
  DailyReturnList = lapply(1:ncol(ReturnSeries), function(x) dailyReturn(ReturnSeries[,x]))
  DailyReturnMat  = Reduce(cbind, DailyReturnList)
  colnames(DailyReturnMat) = funcstr
  
  
  MonthlyReturnList = lapply(1:ncol(ReturnSeries), function(x) monthlyReturn(ReturnSeries[,x]))
  MonthlyReturnMat  = Reduce(cbind, MonthlyReturnList)
  colnames(MonthlyReturnMat) = funcstr
  
  YearlyReturnList = lapply(1:ncol(ReturnSeries), function(x) yearlyReturn(ReturnSeries[,x]))
  YearlyReturnMat  = Reduce(cbind, YearlyReturnList)
  colnames(YearlyReturnMat) = funcstr
  
  
  sharpRatios = SharpeRatio(YearlyReturnMat, Rf = 0.0175)[1,]
  
  dVAR = VaR(DailyReturnMat, p = 0.95, method = "historical")
  mVAR = VaR(MonthlyReturnMat, p = 0.95, method = "historical")
  yVAR = VaR(YearlyReturnMat, p = 0.95, method = "historical")
  dCVAR = CVaR(DailyReturnMat, p = 0.95, method = "historical")
  mCVAR = CVaR(MonthlyReturnMat, p = 0.95, method = "historical")
  yCVAR = CVaR(YearlyReturnMat, p = 0.95, method = "historical")
  
  arithMean = apply(YearlyReturnMat, 2, mean)
  geomMean = apply(YearlyReturnMat, 2, function(x) ((prod(x+1))^(1/length(x))) - 1)
  standSD = apply(YearlyReturnMat, 2, sd)
  
  
  AnnalStat = data.frame(geomMean  = geomMean, 
                         arithMean = arithMean, 
                         standSD   = standSD,
                         sharpRatios = as.numeric(sharpRatios),
                         yVAR = as.numeric(yVAR),
                         mVAR = as.numeric(mVAR),
                         dVaR = as.numeric(dVAR),
                         yCVAR = as.numeric(yCVAR),
                         mCVAR = as.numeric(mCVAR),
                         dCVaR = as.numeric(dCVAR))
  
  print(xtable(AnnalStat * 100, digits = 2), 
        file = paste0("./", tag, "/AnnualSummary", tag, ".tex"))
  print(xtable(AnnalStat * 100, digits = 2), type = "html", 
        file = paste0("./", tag, "/AnnualSummary", tag, ".html"))
  list(AnnalStat = AnnalStat, AnnualReturn = AnnualReturn)
}


getPerformance(ReturnSeries)

## Select top/mid/bottom 20 based market capital ranking====
FROM = as.Date("19578-01-02")
TO = as.Date("2016-12-30")
SHRS = abs(SHRT[paste0(FROM,"/",TO)])
PRCS = abs(PRCT[paste0(FROM,"/",TO)])
RETS = RET[paste0(FROM,"/",TO)]

MKC = PRCS * SHRS
top20 = lapply(1:nrow(MKC), function(i) {
  head(order(as.numeric(MKC[i,]), decreasing = T), 20)
})

mid20 = lapply(1:nrow(MKC), function(i) {
  setdiff(head(order(as.numeric(MKC[i,]), decreasing = T), 260), 
          head(order(as.numeric(MKC[i,]), decreasing = T), 240))
})

bom20 = lapply(1:nrow(MKC), function(i) {
  head(order(as.numeric(MKC[i,]), decreasing = F), 20)
})


head(SelMat)
## Select the stocks of our interest and set others to NAs
SelMatrix = function(PRCS, SHRS, RETS, Select) {
  SelPRCS = matrix(NA, ncol(PRCS), nrow(PRCS))
  SelSHRS = matrix(NA, ncol(PRCS), nrow(PRCS))
  SelRETS = matrix(NA, ncol(PRCS), nrow(PRCS))
  SelMat = t(Reduce(rbind, Select))
  for(i in c(1:ncol(SelMat))) {
    if(i %% 200 == 0) {
      print(i )  
    }
    idx = SelMat[,i]
    SelPRCS[idx, i] = PRCS[i, idx]
    SelSHRS[idx, i] = SHRS[i, idx]
    SelRETS[idx, i] = RETS[i, idx]
  }
  SelPRCS = xts(t(SelPRCS), order.by = index(PRCS))
  SelSHRS = xts(t(SelSHRS), order.by = index(PRCS))
  SelRETS = xts(t(SelRETS), order.by = index(PRCS))
  list(PRCS = SelPRCS, SHRS = SelSHRS, RETS = SelRETS)
}


## TOP 20
top20List = SelMatrix(PRCS, SHRS, RETS, top20)
mid20List = SelMatrix(PRCS, SHRS, RETS, mid20)
bom20List = SelMatrix(PRCS, SHRS, RETS, bom20)


## get Sub Return mat for multiple strategies (EQU & MKC)====
getSubReturnMat <- function(List, funclist) {
  SHRSS = abs(List$PRCS)
  PRCSS = abs(List$SHRS)
  RETSS = List$RETS
  
  ResultList = lapply(funcList, function(x) {
    print(x)
    getReturn(x, PRCSS, SHRSS, RETSS, "Monthly")
  })
  CumuList = lapply(ResultList, function(x) x$CumuReturn)
  CumuMat = Reduce(cbind, CumuList)
  colnames(CumuMat) = funcStr
  nBalanced = ResultList[[1]]$nBalanced
  ChangeStat = ResultList[[1]]$ChangeStat
  Admin = unlist(lapply(ResultList, function(x) x$Admin))
  BidAsk = unlist(lapply(ResultList, function(x) x$BidAsk))
  Transaction = unlist(lapply(ResultList, function(x) x$Transaction))
  list(CumuMat = CumuMat, nBalanced = nBalanced, ChangeStat = ChangeStat,
       Admin = Admin, BidAsk = BidAsk, Transaction = Transaction)
}



 
# EQU versus MKC of the top 20 tier
top20Return = getSubReturnMat(top20List, funcList)
# EQU versus MKC of the mid 20 tier
mid20Return = getSubReturnMat(mid20List, funcList)
# EQU versus MKC of the bottom 20 tier
bom20Return = getSubReturnMat(bom20List, funcList)

tail(top20Return$CumuMat)
tail(mid20Return$CumuMat)
tail(bom20Return$CumuMat)

tail(top20Return$CumuMat)[,2] / tail(top20Return$CumuMat)[,1]
tail(mid20Return$CumuMat)[,2] / tail(mid20Return$CumuMat)[,1]
tail(bom20Return$CumuMat)[,2] / tail(bom20Return$CumuMat)[,1]
