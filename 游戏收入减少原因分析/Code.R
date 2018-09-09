# 读入CSV文件
dau <- read.csv(
  "dau.csv", 
  header = T, 
  stringsAsFactors = F
)
dpu <- read.csv(
  "dpu.csv", 
  header = T, 
  stringsAsFactors = F
)
registered <- read.csv(
  "registered.csv", 
  header = T, 
  stringsAsFactors= F
)

# 合并DAU和Registered的数据
dau.registered <- merge(
  dau, registered, 
  by = c("user_id")
)
# 合并上述数据和DPU数据
dau.registered.payment <- merge(
  dau.registered, dpu, 
  by = c("log_date", "user_id"), 
  all.x = T
)

# 将未消费用户的消费额设置为零
dau.registered.payment$payment[
  is.na(dau.registered.payment$payment)
] <- 0

# 按月统计
# 增加一列表示月份
dau.registered.payment$log_month <- substr(
  dau.registered.payment$log_date, 1, 6
)
dau.registered.payment$install_month <- substr(
  dau.registered.payment$install_date, 1, 6
)

#install.packages('plyr')

library(plyr)
# 求出每人每月的消费总额
mau.payment <- ddply(
  dau.registered.payment,
  .(log_month, user_id, install_month), # 分组
  summarize, # 汇总命令
  payment = sum(payment) # payment的总和
)

# 增加新的标记，识别新用户和已有用户
mau.payment$user.type <- ifelse(
  mau.payment$install_month == mau.payment$log_month,
  "install", 
  "existing"
)

mau.payment.summary <- ddply(
  mau.payment,
  .(log_month, user.type), # 分组
  summarize, # 汇总命令
  total.payment = sum(payment) # payment的总和
)

head(mau.payment) 
head(mau.payment.summary)

barplot(
  mau.payment.summary$total.payment,
  names.arg=paste(
    mau.payment.summary$log_month,
    mau.payment.summary$user.type
  )
)

install.packages('ggplot2')
# 数据可视化（将geom_bar()修正为geom_bar(stat="identity") 2014/08/22）
library(ggplot2)
library(scales)
ggplot(
  mau.payment.summary, 
  aes(
    x = log_month, 
    y = total.payment,
    fill = user.type
  )
) + geom_bar(
  stat="identity"
) + scale_y_continuous(
  label = comma
)

# old_theme = theme_update(
# axis.title.x = theme_text(family="HiraKakuProN-W3"),
# axis.title.y = theme_text(family="HiraKakuProN-W3", angle=90),
# plot.title = theme_text(family="HiraKakuProN-W3", size=14.4))

ggplot(
  mau.payment[
    mau.payment$payment > 0 
    & mau.payment$user.type == "install", 
  ], 
  aes(x = payment, fill = log_month)
) + geom_histogram(
  position = "dodge", binwidth = 2000
)
