# 读入数据

dau <- read.csv("dau.csv")
dpu <- read.csv("dpu.csv")
user.action <- read.csv("action.csv")

# 将DAU和DPU合并

# 合并消费额数据
daupu <- merge(
  dau, dpu, all.x=T,
  by=c("log_date", "user_id")
)
# 添加消费额标志位
daupu$is.payment <- ifelse(
  is.na(daupu$payment), 0, 1
)

# 将无消费记录的消费额设为0
daupu$payment <- ifelse(
  is.na(daupu$payment), 0, daupu$payment
)

# 按月统计
# 增加一列表示月份
daupu$log_month <- substr(daupu$log_date, 1, 7)
# 按月统计
mau <- ddply(
  daupu, .(log_month, user_id), 
  summarize, payment=sum(payment),
  access_days=length(log_date)
)

library(plyr)
library(scales)
library(ggplot2)
library(ykmeans)

# A47为排行榜得分
user.action2 <- ykmeans(
  user.action, "A47", "A47", 3
)
# 每个类的人数
table(user.action2$cluster)

# 排行榜得分的分布
ggplot(
  arrange(user.action2, desc(A47)),
  aes(
    x=1:length(user_id), y = A47,
    col=as.factor(cluster), 
    shape=as.factor(cluster))
) + geom_line() + xlab("用户排名") + ylab("游戏排行榜得分") + 
  scale_y_continuous(label=comma) + ggtitle("聚类效果") + 
  theme(legend.title=element_blank())

# 限定排名靠前的用户
user.action.h <- user.action2[
  user.action2$cluster >= 2,
  names(user.action)
]

library(caret)
# 删除建模用不上的user_id, log_date列
user.action.f <- subset(
  user.action.h, 
  select=-c(user_id, log_date)
)
# 把user_id用于行名，便于标记样本
row.names(user.action.f) <- user.action.h$user_id

# 删除那些方差接近于0的列
nzv <- nearZeroVar(user.action.f)
user.action.f.filterd <- user.action.f[
  , 
  -nzv
]
# 删除那些相关性高的变量
user.action.cor <- cor(
  user.action.f.filterd
)
highly.cor.f <- findCorrelation(
  user.action.cor, cutoff=.7
)
user.action.f.filterd <- user.action.f.filterd[
  , 
  -highly.cor.f
]

# 进行主成分分析
user.action.pca.base <- prcomp(
  user.action.f.filterd, scale=T
)

# 进行聚类
user.action.pca <- data.frame(
  user.action.pca.base$x
)
user.action.km <- ykmeans(
  user.action.pca, 
  names(user.action.pca), 
  "PC1", 3:6
)
table(user.action.km$cluster)

ggplot(
  user.action.km,
  aes(
    x=PC1, y=PC2, 
    col=as.factor(cluster), 
    shape=as.factor(cluster)
  )
) + geom_point() + 
theme(legend.title=element_blank())

# 计算每个类的平均值
user.action.f.filterd$cluster <- user.action.km$cluster
user.action.f.center <- ldply(
  lapply(
    sort(unique(user.action.f.filterd$cluster)),
    function(i) {
      x <- user.action.f.filterd[
        user.action.f.filterd$cluster==i,
        -ncol(user.action.f.filterd)
      ]
      apply(x, 2, function(d) mean(d))
    }
  )
)

# 生成用于雷达图的数据
library(fmsb)
# 对雷达图所需的数据进行整理的函数
createRadarChartDataFrame <- function(df) {
  df <- data.frame(df)
  dfmax <- apply(df, 2, max) + 1
  dfmin <- apply(df, 2, min) - 1
  as.data.frame(rbind(dfmax, dfmin, df))
}
# 排除相关性较高的变量
df <- user.action.f.center
df.cor <- cor(df)
df.highly.cor <- findCorrelation(
  df.cor, cutoff=0.91
)
# 手动调整使得数据易于解释
df.filterd <- df[, -df.highly.cor]
# 生成雷达图所需的数据
df.filterd <- createRadarChartDataFrame(
  scale(df.filterd)
)
names(df.filterd)

names(df.filterd) <- c(
  "级别", "救援他人的次数", "被救援的次数", 
  "对战敌方首领的次数", "排行榜分数",
  "参与战斗的次数", "参与游戏的次数"
)

radarchart(
  df.filterd, 
  seg = 5, plty = 1:5, 
  plwd = 4, pcol = rainbow(5)
)
legend(
  "topright", legend=1:5, 
  col=rainbow(5), lty=1:5
)

# 计算每个类的KPI

user.action.f.filterd$user_id <- as.numeric(
  rownames(user.action.f.filterd)
)
user.action.kpi <- merge(
  user.action.f.filterd, 
  mau, by="user_id"
)
ddply(
  user.action.kpi, 
  .(cluster), 
  summarize,
  arpu=round(mean(payment)),
  access_days=round(mean(access_days))
)
