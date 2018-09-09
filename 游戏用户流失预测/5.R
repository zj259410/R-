# 读入CSV文件
dau <- read.csv(
  "dau.csv", 
  header = T, 
  stringsAsFactors = F
)
dau <- dau[
  , 
  c(
    "region_month", "region_day", 
    "user_id", "device"
  )
]
dau[dau$device=='FP', "device"] <- 'App'
dau[dau$device=='SP', "device"] <- 'XYX'
write.csv(dau, "dau2.csv", row.names=FALSE, quote = FALSE)

# DAU数据
dau <- read.csv(
  "dau.csv", 
  header = T, 
  stringsAsFactors = F
)
# App MAU
app.mau <- unique(
  dau[
    dau$device=="App", 
    c("region_month", "device", "user_id")
  ]
)
# XYX MAU
xyx.mau <- unique(
  dau[
    dau$device=="XYX", 
    c("region_month", "device", "user_id")
  ]
)
# 分别获取1月份和2月份的数据
app.mau1 <- app.mau[
  app.mau$region_month == "2013-01", 
]
app.mau2 <- app.mau[
  app.mau$region_month == "2013-02", 
]
xyx.mau1 <- xyx.mau[
  xyx.mau$region_month == "2013-01", 
]
xyx.mau2 <- xyx.mau[
  xyx.mau$region_month == "2013-02", 
]

# 一月份活跃的所有用户
mau1 <- unique(
  dau[
    dau$region_month == "2013-01", 
    c("region_month", "device", "user_id")
  ]
)
# 二月份活跃的所有用户
mau2 <- unique(
  dau[
    dau$region_month == "2013-02", 
    c("region_month", "device", "user_id")
  ]
)

# 1月份的App用户在2月份的访问情况
mau2$is_access <- 1
app.mau1 <- merge(
  app.mau1, 
  mau2[
    , 
    c('user_id', 'is_access')
  ], 
  by="user_id", all.x = T
)
app.mau1$is_access[
  is.na(app.mau1$is_access)
] <- 0

# 1月份访问过游戏的App用户
# 在2月份是否是继续通过App来访问的
app.mau2$is_app <- 1
app.mau1 <- merge(
  app.mau1, 
  app.mau2[
    , 
    c("user_id", "is_app")
  ],
  by = "user_id",
  all.x = T
)
app.mau1$is_app[
  is.na(app.mau1$is_app)
] <- 0

# 1月份访问过游戏的App用户
# 在2月份是否是通过xyx来访问的
xyx.mau2$is_xyx <- 1
app.mau1 <- merge(
  app.mau1, 
  xyx.mau2[
    , 
    c("user_id", "is_xyx")
  ],
  by = "user_id", 
  all.x = T
)
app.mau1$is_xyx[
  is.na(app.mau1$is_xyx)
] <- 0

#二月App不活跃用户
nrow(
  app.mau1[
    app.mau1$is_access==0 & app.mau1$is_xyx==0
    , 
  ]
)
#二月App或者小游戏活跃用户
nrow(
  app.mau1[
    app.mau1$is_access==1 | app.mau1$is_xyx==1
    , 
  ]
)
#二月App活跃用户
nrow(
  app.mau1[
    app.mau1$is_access==1
    , 
  ]
)
#二月小游戏活跃用户
nrow(
  app.mau1[
    app.mau1$is_xyx==1
    , 
  ]
)


app.mau1.mau2 <- app.mau1[
  app.mau1$is_xyx==1
  , 
]

# 过滤出1月份通过App访问
# 但2月份没有访问或者通过xyx访问的用户
app.mau1 <- app.mau1[
  app.mau1$is_access==0 | app.mau1$is_xyx==1
  , 
]

# 关于是否每天访问游戏的数据的整理
library(reshape2)
app.dau1 <- dau[
  dau$device=="App" 
  & dau$region_month=="2013-01",
]
app.dau1$is_access <- 1
app.dau1.cast <- dcast(
  app.dau1, 
  user_id ~ region_day, 
  value.var="is_access", 
  function(x) {
    as.character(length(x))
  }
)
names(app.dau1.cast)[-1] <- paste0(
  "X", 1:31, "day"
)

# 把2月份是否小游戏活跃的标记关联上
app.dau1.cast <- merge(
  app.dau1.cast, 
  app.mau1[, c("user_id", "is_xyx")],
  by = "user_id"
)

table(app.dau1.cast$is_xyx)

# 基于逻辑回归分析建立模型
fit.logit <- step(
  glm(
    is_xyx ~ ., 
    data = app.dau1.cast[, -1],
    family = binomial
  )
)
summary(fit.logit)

# 利用生成的模型来进行预测
# App账号迁转设定的概率
app.dau1.cast$prob <- round(
  fitted(fit.logit), 2
)
# 预测在小游戏上
# 是否进行了账号迁转设定
app.dau1.cast$pred <- ifelse(
  app.dau1.cast$prob > 0.5, 
  1, 
  0
)
# 预测值和实际值
table(
  app.dau1.cast[
    , 
    c("is_xyx", "pred")
  ]
)

# 根据预测结果来推测用户群
app.dau1.cast1 <- app.dau1.cast[
  app.dau1.cast$is_sp == 1 & app.dau1.cast$pred == 1, 
]
head(
  fp.dau1.cast1[
    order(fp.dau1.cast1$prob, decreasing = T), 
  ]
)

fp.dau1.cast2 <- fp.dau1.cast[
  fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred == 1, 
]
head(
  fp.dau1.cast2[
    order(fp.dau1.cast2$prob, decreasing = T), 
  ]
)

fp.dau1.cast3 <- fp.dau1.cast[
  fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred == 0, 
]
head(fp.dau1.cast3[order(fp.dau1.cast3$prob), ])
