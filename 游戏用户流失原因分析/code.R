##  第四章源代码

# 读入CSV文件
dau <- read.csv(
  "dau.csv", 
  header=T, 
  stringsAsFactors=F
)
head(dau)

dauAgg = data.frame(table(dau$log_date))
colnames(dauAgg) <- c('date', 'dau')
plot(dauAgg)
lines(dauAgg$date, dauAgg$dau)
library(zoo)
dauAggRM = rollmean(dauAgg$dau, 8)
lines(
  data.frame(
    date=tail(dauAgg$date, length(dauAggRM)), 
    dau=dauAggRM
  )
)

user.info <- read.csv(
  "user_info.csv", 
  header=T, 
  stringsAsFactors=F
)
head(user.info)

# 合并DAU和user.info的数据
dau.user.info <- merge(
  dau, user.info, 
  by = c("user_id")
)
head(dau.user.info)

# 用户群分析（按性别统计）
dau.user.info$log_month <- substr(
  dau.user.info$log_date, 1, 7
)
table(
  dau.user.info[
    , 
    c("log_month", "gender")
  ]
)

# 用户群分析（按年龄段统计）
table(
  dau.user.info[
    , 
    c("log_month", "generation")
  ]
)

prop.table(
  table(
    dau.user.info[
      , 
      c("log_month", "generation")
    ]
  ), margin = 1
)
# 用户群分析（按性别×年龄段统计）
library(reshape2)
dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id",
length)

# 用户群分析（按设备统计）
table(
  dau.user.info[
    ,
    c("log_month","device_type")
  ]
)

# 用户群分析结果的可视化

# 按照日期和设备类型计算用户数
library(plyr)
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
# 变换日期类型
dau.user.info.device.summary$log_date <- as.Date(
  dau.user.info.device.summary$log_date
)
# 画出时序列趋势图
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
geom_line(lwd=1) +
geom_point(size=4) +
scale_y_continuous(label=comma, limits=limits)