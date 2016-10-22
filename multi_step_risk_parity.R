# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ========  risk parity   ===============================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#初始化
# source("Default/Index.R")
setwd("~/Code/R/")
source('RiskParityModel/risk_parity_func.R',encoding='utf-8')
source('QuantFunction/basic_func.R',encoding='utf-8')
library(Rdonlp2)
library(timeSeries)
library(WindR)
library(xts)
library(TTR)
w.start()


# 1.  H11137.CSI      中国互联网(美元)             Equity    2007-07-01
# 2.  HSCEI.HI        恒生国企指数(港币)           Equity    2005-04-14
# 3.  000016.SH       上证50                       Equity    2004-01-01
# 4.  399006.SZ       创业板指                     Equity    2010-06-02
# 5.  000905.SH       中证500                      Equity    2005-01-04
# 6.  000300.SH       沪深300                      Equity    2002

# 14.  SPX.GI          标普500                     Equity    1966
# 15.  IXIC.GI         纳斯达克指数                Equity    1971
# 16.  GDAXI.GI        德国DAX                     Equity    1965

# 7.  H11017.CSI      中期国债                     Bond      2008-01-01
# 8.  H11078.CSI      中证中高信用                 Bond      2008-01-01
# 9.  0401E.CS        中债固定利率全价(7-10)指数   Bond      2006-11-20
# 17  070025.OF       嘉实信用A                    Bond      2011-09-14
# 10. 037.CS          中债总财富指数               Bond      2002-01-01

# 18. 000198.OF       天弘余额宝                   Cash      2013-06-03
# 19. 482002.OF       工银货币                     Cash      2006-03-26

# 11. CL.NYM NYMEX    原油(美元)                   Commodity 2011-12-21
# 12. AU9999.SGE      黄金                         Commodity 2004-01-01
# 13. CCFI.WI         WIND商品                     Commodity 2002-01-01


# 设置数据路径
path <- "RiskParityModel/input/data0906.csv"
# # 保存数据:只需运行一次
# 
# dataset <- get_data2(start='2002-01-01',end='2016-09-05',type='discrete',currency=T)
# write.csv(as.data.frame(dataset),"RiskParityModel/intput/data_d.csv")

设置日期
dt <- '2016-08-26'
# 模型1
#资产 恒生+300+商品+黄金
id1 <- c(2,6,13,12)
risk_weight1 <- c(1/4,1/4,1/4,1/4)
# 初始化
num <- length(id1)
u_limit <- rep(1,num)  # 资产权重上限 
l_limit <- rep(0,num)                  # 资产权重下限
u_sigma <- +Inf                  # 年化波动率上限
l_sigma <- -Inf                     #  年化波动率下限
currency <- FALSE                    # 是否进行货币转换
# 设置风险权重配比
# 输出某个时间点往前t天的波动率
asset_vol(id1,c(20,60,90,250),dt,path)

risk_parity_result1 <- risk_parity3(id1,risk_weight1,dt,u_limit,l_limit,u_sigma,l_sigma,currency,path)
ret1 <- risk_parity_result1$ret
w1 <- risk_parity_result1$weight

# 模型2
#资产 中期国债+中期高信用
id2 <- c(7,8)
risk_weight2 <- c(1/2,1/2)
# 初始化
num <- length(id2)
u_limit <- rep(1,num)  # 资产权重上限 
l_limit <- rep(0,num)                  # 资产权重下限
u_sigma <- +Inf                  # 年化波动率上限
l_sigma <- -Inf                     #  年化波动率下限
currency <- FALSE                    # 是否进行货币转换
# 设置风险权重配比
# 输出某个时间点往前t天的波动率
asset_vol(id2,c(20,60,90,250),dt,path)
risk_parity_result2 <- risk_parity3(id2,risk_weight2,dt,u_limit,l_limit,u_sigma,l_sigma,currency,path)
ret2 <- risk_parity_result2$ret
w2 <- risk_parity_result2$weight

# 模型3：结合1,2
ret3 <- as.xts(cbind(ret1,ret2))
# 设置风险权重配比
risk_weight3 <- c(3/4,1/4)
# 初始化
num <- 2
u_limit <- rep(1,num)  # 资产权重上限 
l_limit <- rep(0,num)                  # 资产权重下限
u_sigma <- +Inf                  # 年化波动率上限
l_sigma <- -Inf                     #  年化波动率下限
currency <- FALSE                    # 是否进行货币转换
risk_parity_result3 <- risk_parity4(ret3,risk_weight3,u_limit,l_limit,u_sigma,l_sigma,init=c(0.2,0.4))
w3 <- risk_parity_result3$weight

## 计算总权重
w <- c(w1*w3[1],w2*w3[2])
print(round(w,2))











