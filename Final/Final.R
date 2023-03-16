library(tidyverse)
library(GGally)
library(patchwork)
library(mclust)
library(caret)
library(pROC)
library(highcharter)
library(e1071)
library(randomForest)
library(smotefamily)
library(xgboost)
library(Matrix)
rm(list = ls())

data <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
summary(data)
data <- data[,-1] #删除ID列
table(data$Churn) #样本不均
data$Churn <- factor(data$Churn,labels = c("0","1")) #响应变量因子化
str(data)
sum(is.na(data))
data <- na.omit(data)

### 个别值处理
data$MultipleLines[data$MultipleLines == 'No phone service'] <- 'No'
data$OnlineSecurity[data$OnlineSecurity == 'No internet service'] <- 'No'
data$OnlineBackup[data$OnlineBackup == 'No internet service'] <- 'No'
data$DeviceProtection[data$DeviceProtection == 'No internet service'] <- 'No'
data$TechSupport[data$TechSupport == 'No internet service'] <- 'No'
data$StreamingTV[data$StreamingTV == 'No internet service'] <- 'No'
data$StreamingMovies[data$StreamingMovies == 'No internet service'] <- 'No'

### 数据类型处理
data <- data %>%
  mutate(SeniorCitizen = as.character(SeniorCitizen)) %>%
  mutate_if(is.character,factor,ordered=F)

sapply(data,class)

### 卡方检验 分类变量
# gender  不显著
pv1 <- chisq.test(table(data$gender,data$Churn))$p.value
p1 <- ggplot(data = data) + 
  geom_bar(mapping = aes(x = gender, fill = Churn), position = "fill")
pv$p.value

# SeniorCitizen
pv2 <- chisq.test(table(data$SeniorCitizen,data$Churn))$p.value
p2 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = SeniorCitizen, fill = Churn), position = "fill")

# Partner       
pv3 <- chisq.test(table(data$Partner,data$Churn))$p.value
p3 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = Partner, fill = Churn), position = "fill")

# Dependents
pv4 <- chisq.test(table(data$Dependents,data$Churn))$p.value
p4 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = Dependents, fill = Churn), position = "fill")

# PhoneService  不显著
pv5<-chisq.test(table(data$PhoneService,data$Churn))$p.value
p5 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = PhoneService, fill = Churn), position = "fill")

# MultipleLines  无意义
pv6<-chisq.test(table(data$MultipleLines,data$Churn))$p.value
p6 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = MultipleLines, fill = Churn), position = "fill")

# InternetService
pv7<-chisq.test(table(data$InternetService,data$Churn))$p.value
p7 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = InternetService, fill = Churn), position = "fill")

# OnlineSecurity
pv8<-chisq.test(table(data$OnlineSecurity,data$Churn))$p.value
p8 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = OnlineSecurity, fill = Churn), position = "fill")

# OnlineBackup
pv9<-chisq.test(table(data$OnlineBackup,data$Churn))$p.value
p9 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = OnlineBackup, fill = Churn), position = "fill")

# DeviceProtection
pv10<-chisq.test(table(data$DeviceProtection,data$Churn))$p.value
p10 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = DeviceProtection, fill = Churn), position = "fill")

# TechSupport
pv11<-chisq.test(table(data$TechSupport,data$Churn))$p.value
p11 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = TechSupport, fill = Churn), position = "fill")

# StreamingTV
pv12<-chisq.test(table(data$StreamingTV,data$Churn))$p.value
p12 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = StreamingTV, fill = Churn), position = "fill")

# StreamingMovies
pv13<-chisq.test(table(data$StreamingMovies,data$Churn))$p.value
p13 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = StreamingMovies, fill = Churn), position = "fill")

# Contract
pv14<-chisq.test(table(data$Contract,data$Churn))$p.value
p14 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = Contract, fill = Churn), position = "fill")

# PaperlessBilling
pv15<-chisq.test(table(data$PaperlessBilling,data$Churn))$p.value
p15 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = PaperlessBilling, fill = Churn), position = "fill")

# PaymentMethod
pv16<-chisq.test(table(data$PaymentMethod,data$Churn))$p.value
p16 <- ggplot(data=data) +
  geom_bar(mapping = aes(x = PaymentMethod, fill = Churn), position = "fill")

p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15+p16

### 单因素方差分析
# tenure
tenure.aov <- aov(data$tenure~data$Churn)
pv17<-summary(tenure.aov)[[1]][["Pr(>F)"]][1]

# MonthlyCharges
MonthlyCharges.aov <- aov(data$MonthlyCharges~data$Churn)
pv18<-summary(MonthlyCharges.aov)[[1]][["Pr(>F)"]][1]

# TotalCharges
TotalCharges.aov <- aov(data$TotalCharges~data$Churn)
pv19<-summary(TotalCharges.aov)[[1]][["Pr(>F)"]][1]

p17 <- ggplot(data=data) +
  geom_boxplot(mapping = aes(x = Churn, y = tenure, fill=Churn))
p18 <- ggplot(data=data) +
  geom_boxplot(mapping = aes(x = Churn, y = MonthlyCharges, fill=Churn))
p19 <- ggplot(data=data) +
  geom_boxplot(mapping = aes(x = Churn, y = TotalCharges, fill=Churn))

p17+p18+p19

# p-value plot
pv<- c(pv1,pv2,pv3,pv4,pv17,pv5,pv6,pv7,pv8,pv9,pv10,pv11,pv12,pv13,pv14,pv15,pv16,pv18,pv19)
p.frame <- data.frame(feature = names(data[-20]),P.value = pv)
p.frame <- p.frame[order(p.frame$P.value),]
p.frame$feature <- factor(p.frame$feature,levels = p.frame$feature)
ggplot(data = p.frame,aes(x = feature, y = P.value)) + 
  geom_pointrange(aes(ymin=0,ymax=P.value),
                  size = 0.5,
                  color = ifelse(p.frame$P.value>0.001,'#F8766D','grey'))+
  coord_flip()

### 剔除不显著
data <- data %>%
  dplyr::select(-gender,-PhoneService,-MultipleLines)

data2 <- data

ggplot(data=data) +
  geom_bar(mapping = aes(x = Contract, fill = InternetService), position = "fill")+
  scale_fill_manual('InternetService',values = c('Fiber optic'='#F8766D','DSL'='#00BF7D','No'='#00BFC4'))

ggplot(data=data) +
  geom_bar(mapping = aes(x = InternetService, fill = Contract), position = "fill")+
  scale_fill_manual('InternetService',values = c('Month-to-month'='#00BF7D','One year'='#F8766D','Two year'='#00BFC4'))

### 连续变量交互作用
dataCON <- data %>%
  dplyr::select(where(is.numeric),Churn)

ggpairs(dataCON[-4],aes(color=dataCON$Churn,alpha=0.75),lower = list(continuous="smooth"))+
  theme_bw()



### Logistic
### 划分数据集
set.seed(2020111142)
ind <- sample(1:nrow(data),size = 0.7*nrow(data))
train <- data[ind,]
test <- data[-ind,]

### 交互变量探索
#tenure vs MonthlyCharges
p20 <- ggplot(train,aes(x=tenure, y=MonthlyCharges, color=Churn))+
  geom_point()+
  theme_bw()
# train添加交互项
train$tenure_15_MC_75 <- factor(ifelse(train$tenure<=15 & train$MonthlyCharges>=75,'Yes','No'))
chisq.test(table(train$tenure_15_MC_75,train$Churn))
p21 <-ggplot(data=train) +
  geom_bar(mapping = aes(x = tenure_15_MC_75, fill = Churn), position = "fill")
p20+p21

# test添加交互项
test$tenure_15_MC_75 <- factor(ifelse(test$tenure<=15 & test$MonthlyCharges>=75,'Yes','No'))

#tenure vs MonthlyCharges
p22 <- ggplot(train,aes(x=TotalCharges, y=MonthlyCharges, color=Churn))+
  geom_point()+
  theme_bw()
# train添加交互项
train$TC_1250_MC_75 <- factor(ifelse(train$TotalCharges<=1250 & train$MonthlyCharges>=75,'Yes','No'))
chisq.test(table(train$TC_1250_MC_75,train$Churn))
p23 <- ggplot(data=train) +
  geom_bar(mapping = aes(x = TC_1250_MC_75, fill = Churn), position = "fill")
p22+p23

# test添加交互项
test$TC_1250_MC_75 <- factor(ifelse(test$TotalCharges<=1250 & test$MonthlyCharges>=75,'Yes','No'))

# 建模
fit.logit.inter <- glm(Churn~.,train,family = binomial(link = "logit"))
summary(fit.logit.inter)

# 系数可视化
coefs <- coef(fit.logit.inter)[-1]
coef.frame <- data.frame(coef = names(coefs),value = coefs)
coef.frame <- coef.frame[order(coef.frame$value),]
coef.frame$coef <- factor(coef.frame$coef,levels = coef.frame$coef)
ggplot(data = coef.frame,aes(x = coef, y = value)) + 
  geom_bar(stat = 'identity', 
           fill = ifelse(coef.frame$value>0,'#F8766D','#00BFC4'), # 根据y值的正负设置颜色
           width = 0.9)+
  coord_flip()


# 确定最佳阈值
pred.train.logit.inter.p <- predict(fit.logit.inter, train, type = 'response')
thresholds <- seq(0,1,by=0.01)
acc.test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL
for(i in 1:length(thresholds)){
  pred.train.logit.inter <- ifelse(pred.train.logit.inter.p<thresholds[i],"0","1")
  accuracy1 <- confusionMatrix(factor(pred.train.logit.inter,levels = c("0","1")),train$Churn)
  accuracy2[i] <- accuracy1$overall[1]
}
acc.test <- data.frame(p=thresholds,cnt=accuracy2)
opt.p <- subset(acc.test,cnt==max(cnt))[1,]
sub <- paste("num of parameter:",opt.p$p,"    accuracy:", opt.p$cnt)
sub

hchart(acc.test,'line',hcaes(p,cnt))%>%
  hc_title(text='Accuracy With Threshold(logit_all)')%>%
  hc_subtitle(text=sub)%>%
  hc_add_theme(hc_theme_google())%>%
  hc_xAxis(title=list(text = 'Number of Parameters'))%>%
  hc_yAxis(title=list(text = 'Accuracy'))

# 使用最优阈值分类
pred.logit.inter.p <- predict(fit.logit.inter,newdata = test,type = 'response')
pred.logit.inter <- ifelse(pred.logit.inter.p < opt.p$p,'0','1')
confusionMatrix(data = factor(pred.logit.inter,levels = c('0','1')),reference = test$Churn)

roc.logit.inter <- roc(test$Churn,pred.logit.inter.p,quiet=T)
plot(roc.logit.inter,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),max.auc.polygon=T,auc.polygon.col='skyblue',print.thres=T)


### 对训练集连续变量分箱
trainCON <- train %>%
  dplyr::select(where(is.numeric),Churn)

set.seed(2020111142)
tenure.clust <- Mclust(trainCON$tenure,G=4)  #(v,4)
summary(tenure.clust, parameters = T)
p24 <- ggplot(cbind(train,类别=factor(tenure.clust$classification)),aes(y=tenure,fill=类别))+
  geom_bar(position = 'fill')
train$tenure <- factor(tenure.clust$classification)
table(factor(tenure.clust$classification),train$Churn)%>%chisq.test()

MonthlyCharges.clust <- Mclust(trainCON$MonthlyCharges,G=4)  #(v,4)
summary(MonthlyCharges.clust, parameters = T)
p25 <- ggplot(cbind(train,类别=factor(MonthlyCharges.clust$classification)),aes(y=MonthlyCharges,fill=类别))+
  geom_bar(position = 'fill')
train$MonthlyCharges <- factor(MonthlyCharges.clust$classification)
table(factor(MonthlyCharges.clust$classification),train$Churn)%>%chisq.test()

TotalCharges.clust <- Mclust(trainCON$TotalCharges,G=4)  #(v,4)
summary(TotalCharges.clust, parameters = T)
train$TotalCharges <- factor(TotalCharges.clust$classification)
table(factor(TotalCharges.clust$classification),train$Churn)%>%chisq.test()

p24+p25

### 对测试集连续变量分箱
testCON <- test %>%
  dplyr::select(where(is.numeric),Churn)

set.seed(2020111142)
tenure.test.clust <- Mclust(testCON$tenure,G=4,modelNames = 'V')  #(v,4)
summary(tenure.test.clust, parameters = T)
test$tenure <- factor(tenure.test.clust$classification)
table(factor(tenure.test.clust$classification),test$Churn)%>%chisq.test()

MonthlyCharges.test.clust <- Mclust(testCON$MonthlyCharges,G=4,modelNames = 'V')  #(v,4)
summary(MonthlyCharges.test.clust, parameters = T)
test$MonthlyCharges <- factor(MonthlyCharges.test.clust$classification)
table(factor(MonthlyCharges.test.clust$classification),test$Churn)%>%chisq.test()

TotalCharges.test.clust <- Mclust(testCON$TotalCharges,G=4,modelNames = 'V')  #(v,4)
summary(TotalCharges.test.clust, parameters = T)
test$TotalCharges <- factor(TotalCharges.test.clust$classification)
table(factor(TotalCharges.test.clust$classification),test$Churn)%>%chisq.test()

#给数据框 WOE编码
#输入：
#data：数据框
#y:因变量列名
#test：测试集数据框
#输出： WOE编码后的数据框和 IV值
#若有 test则再返回 test的编码结果
#注： test和 data的列名需要完全相同
WOEML <- function(data,y,test=NULL){
  flag <- 1
  if(is.null(test)){
    flag<- 0
    test <- data
  }
  result <- data%>%dplyr::select(eval(y))%>%unlist
  data <- data%>%dplyr::select(-eval(y))
  retest <- test%>%dplyr::select(eval(y))%>%unlist
  test <- test%>%dplyr::select(-eval(y))
  name <- names(data)
  IV <- rep(0,length(name))
  names(IV) <- name
  N0 <- table(result)["0"]
  N1 <- table(result)["1"]
  for(ii in 1:length(name)){
    M0 <- tapply(unlist(result),list(data[,ii],unlist(result)),length)%>%as.matrix
    for(jj in 1:nrow(M0)){
      WOE <- log((M0[jj,"1"]/M0[jj,"0"])/(N1/N0))
      IV[ii] <- IV[ii]+(M0[jj,"1"]/N1-M0[jj,"0"]/N0)*WOE
      levels(data[,ii])[which(levels(data[,ii])==rownames(M0)[jj])] <- WOE
      levels(test[,ii])[which(levels(test[,ii])==rownames(M0)[jj])] <- WOE
    }
    data[,ii] <- as.numeric(as.character(data[,ii]))
    test[,ii] <- as.numeric(as.character(test[,ii]))
  }
  data <- cbind(data,y=result)
  test <- cbind(test,y=retest)
  IV <- sort(IV)
  if(flag){
    return(list(data,IV,test))
  }else{
    return(list(data,IV))
  }
}


train <- as.data.frame(train)
test <- as.data.frame(test)
WOE <- WOEML(train,y="Churn",test = test)
train.WOE <- WOE[[1]]
test.WOE <- WOE[[3]]
WOE[[2]]

IV.frame <- data.frame(coef = names(WOE[[2]]),value = WOE[[2]])
IV.frame <- IV.frame[order(IV.frame$value),]
IV.frame <- IV.frame[-c(15,11,6),]
IV.frame$coef <- factor(IV.frame$coef,levels = IV.frame$coef)
ggplot(data = IV.frame,aes(x = value, y = coef)) + 
  geom_bar(stat = 'identity', 
           fill = ifelse(IV.frame$value>0.3,ifelse(IV.frame$value>=0.5,'#F8766D','#00BFC4'),'grey'), # 根据y值的正负设置颜色
           width = 0.9)

# 相关性矩阵
ggcorr(train.WOE[-19],label = T,digits = 2,hjust=0.8)
train.WOE %>%
  dplyr::select(tenure,InternetService,StreamingTV,StreamingMovies,Contract,MonthlyCharges,TotalCharges)%>%
  ggcorr(label = T,digits = 2,hjust=0.8)

# 建模
# all train
train.WOE.pro <- train.WOE %>%
  select(-MonthlyCharges) # 高度相关 保留其一
fit.logit <- glm(y~.,train.WOE.pro,family = binomial(link = "logit"))
summary(fit.logit)
train.WOE.pro <- train.WOE.pro %>%
  select(-Partner,-tenure_15_MC_75) # 删除系数小于0的
fit.logit <- glm(y~.,train.WOE.pro,family = binomial(link = "logit"))
summary(fit.logit)

train.WOE.pro.fac <- train.WOE.pro%>%
  mutate_if(is.numeric,factor,ordered=F)
  
summary(train.WOE.pro.fac)
summary(train)
# 确定最佳阈值
pred.train.logit.p <- predict(fit.logit, train.WOE.pro, type = 'response')
thresholds <- seq(0,1,by=0.01)
acc.test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL
for(i in 1:length(thresholds)){
  pred.train.logit <- ifelse(pred.train.logit.p<thresholds[i],"0","1")
  accuracy1 <- confusionMatrix(factor(pred.train.logit,levels = c("0","1")),train.WOE.pro$y)
  accuracy2[i] <- accuracy1$overall[1]
}
acc <- data.frame(p=thresholds,cnt=accuracy2)
opt.p <- subset(acc,cnt==max(cnt))[1,]
sub <- paste("num of parameter:",opt.p$p,"    accuracy:", opt.p$cnt)
sub

hchart(acc,'line',hcaes(p,cnt))%>%
  hc_title(text='Accuracy With Threshold(logit_WOE)')%>%
  hc_subtitle(text=sub)%>%
  hc_add_theme(hc_theme_google())%>%
  hc_xAxis(title=list(text = 'Number of Parameters'))%>%
  hc_yAxis(title=list(text = 'Accuracy'))

# 使用最优阈值分类
test.WOE.pro <- test.WOE %>%
  select(-Partner,-MonthlyCharges,-tenure_15_MC_75)
pred.logit.p <- predict(fit.logit,newdata = test.WOE.pro,type = 'response')
pred.logit <- ifelse(pred.logit.p < opt.p$p,'0','1')
confusionMatrix(data = factor(pred.logit,levels = c('0','1')),reference = test.WOE.pro$y)

roc.logit <- roc(test.WOE.pro$y,pred.logit.p,quiet=T)
plot(roc.logit,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),max.auc.polygon=T,auc.polygon.col='skyblue',print.thres=T)



### SVM
# 划分数据集
set.seed(2020111142)
ind <- sample(1:nrow(data),size = 0.7*nrow(data))
train <- data[ind,]
test <- data[-ind,]

# 验证集
ind.val <- sample(1:nrow(train),size = 0.7*nrow(train))
val.train <- train[ind.val,]
val <- train[-ind.val,]

# 调参
gamma <- 2^(-5:5)
cost <- 2^(-5:5)
parms <- expand.grid(cost=cost,gamma=gamma)

acc.test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL

for (i in 1:NROW(parms)) {
  set.seed(2020111142)
  learn.svm <- svm(Churn~.,data = val.train,kernel = "radial",gamma=parms$gamma[i],cost=parms$cost[i])
  pre.svm <- predict(learn.svm, val[,-17])
  accuracy1 <- confusionMatrix(pre.svm,val$Churn)
  accuracy2[i] <- accuracy1$overall[1]
}

acc.test <- data.frame(p=seq(1,NROW(parms)),cnt=accuracy2)
opt.p <- subset(acc.test,cnt==max(cnt))[1,]
sub <- paste("num of parameter:",opt.p$p,"    accuracy:", opt.p$cnt)
sub

hchart(acc.test,'line',hcaes(p,cnt))%>%
  hc_title(text='Accuracy With Parameters(SVM)')%>%
  hc_subtitle(text=sub)%>%
  hc_add_theme(hc_theme_google())%>%
  hc_xAxis(title=list(text = 'Number of Parameters'))%>%
  hc_yAxis(title=list(text = 'Accuracy'))
parms$cost[opt.p$p]
parms$gamma[opt.p$p]

# 训练
learn.svm <- svm(Churn~.,train,cost=parms$cost[opt.p$p],gamma=parms$gamma[opt.p$p],probability = T,scale = T)
# 测试
pre.svm <- predict(learn.svm,test[,-17])
confusionMatrix(pre.svm,test$Churn)
pre.svm.p <- as.numeric(predict(learn.svm,newdata=test[,-17],probability=T,type='prob'))
roc.svm <- roc(test$Churn,pre.svm.p,quiet=T)
plot(roc.svm,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),max.auc.polygon=T,auc.polygon.col='skyblue',print.thres=T)

### 随机森林
# 划分数据集
set.seed(2020111142)
ind <- sample(1:nrow(data),size = 0.7*nrow(data))
train <- data[ind,]
test <- data[-ind,]

train <- as.data.frame(train)

# 验证集
ind.val <- sample(1:nrow(train),size = 0.7*nrow(train))
val.train <- train[ind.val,]
val <- train[-ind.val,]

# 调参
acc.test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL
for(i in 1:16){
  set.seed(2020111142)
  rf.train<-randomForest(Churn~.,data=val.train,mtry=i,ntree=1000)
  rf.pred <- predict(rf.train, val[,-17])
  accuracy1 <- confusionMatrix(rf.pred,val$Churn)
  accuracy2[i] <- accuracy1$overall[1]   
}
acc.test <- data.frame(p=1:16,cnt=accuracy2)
opt.p <- subset(acc.test,cnt==max(cnt))[1,]
sub <- paste("num of parameter:",opt.p$p,"    accuracy:", opt.p$cnt)
sub

hchart(acc.test,'line',hcaes(p,cnt))%>%
  hc_title(text='Accuracy With mtry(randomforest)')%>%
  hc_subtitle(text=sub)%>%
  hc_add_theme(hc_theme_google())%>%
  hc_xAxis(title=list(text = 'Number of Parameters'))%>%
  hc_yAxis(title=list(text = 'Accuracy'))

fit.rf <- randomForest(Churn~.,data = train,mtry=2,ntree=1000,proximity=T,importance=T)
plot(fit.rf,lwd=3,main = "Random Forest (Error Rate vs. Number of Trees)")
varImpPlot(fit.rf)

pred.rf <- predict(fit.rf,test[,-17])
confusionMatrix(pred.rf,test$Churn)

pre.rf.p <- predict(fit.rf,test[,-17],type="prob")[,1]
roc.rf <- roc(test$Churn,pre.rf.p,quiet=T)
plot(roc.rf,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),max.auc.polygon=T,auc.polygon.col='skyblue',print.thres=T)

###xgboost
# 划分数据集
set.seed(2020111142)
ind <- sample(1:nrow(data),size = 0.7*nrow(data))
train <- data[ind,]
test <- data[-ind,]
train$Churn <- as.character(train$Churn)
train$Churn <- as.numeric(train$Churn)
test$Churn <- as.character(test$Churn)
test$Churn <- as.numeric(test$Churn)
####训练集的数据预处理
# 将自变量转化为矩阵
traindata1 <- data.matrix(train[,-17]) 
# 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
traindata2 <- Matrix(traindata1,sparse=T) 
traindata3 <- train$Churn
# 将自变量和因变量拼接为list
traindata4 <- list(data=traindata2,label=traindata3) 
# 构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 


testdata1 <- data.matrix(test[,-17]) 
testdata2 <- Matrix(testdata1,sparse=T) 
testdata3 <- test$Churn
testdata4 <- list(data=testdata2,label=testdata3) 
dtest <- xgb.DMatrix(data = testdata4$data, label = testdata4$label) 


xgb.fit <- xgboost(data = dtrain,max_depth=2, eta=0.5,  objective='binary:logistic', nround=15)
imp.xgb <- xgb.importance(colnames(data[,-17]),model=xgb.fit)
xgb.plot.importance(imp.xgb)

xgb.imp.frame <- data.frame(coef = imp.xgb$Feature,value = imp.xgb$Importance)
xgb.imp.frame <- xgb.imp.frame[order(xgb.imp.frame$value),]
xgb.imp.frame$coef <- factor(xgb.imp.frame$coef,levels = xgb.imp.frame$coef)
ggplot(data = xgb.imp.frame,aes(x = value, y = coef)) + 
  geom_bar(stat = 'identity', 
           fill = ifelse(xgb.imp.frame$value>0.1,'#F8766D','grey'), 
           width = 0.9)

xgb.cov.frame <- data.frame(coef = imp.xgb$Feature,value = imp.xgb$Cover)
xgb.cov.frame <- xgb.cov.frame[order(xgb.cov.frame$value),]
xgb.cov.frame$coef <- factor(xgb.cov.frame$coef,levels = xgb.cov.frame$coef)
ggplot(data = xgb.cov.frame,aes(x = value, y = coef)) + 
  geom_bar(stat = 'identity', 
           fill = ifelse(xgb.cov.frame$value>0.1,'#F8766D','grey'), 
           width = 0.9)

pred.train.xgb.p <- predict(xgb.fit,dtrain)

# 确定最佳阈值
thresholds <- seq(0,1,by=0.01)
acc.test <- numeric()
accuracy1 <- NULL
accuracy2 <- NULL
for(i in 1:length(thresholds)){
  pred.train.xgb <- ifelse(pred.train.xgb.p<thresholds[i],"0","1")
  accuracy1 <- confusionMatrix(factor(pred.train.xgb,levels = c("0","1")),as.factor(train$Churn))
  accuracy2[i] <- accuracy1$overall[1]
}
acc.test <- data.frame(p=thresholds,cnt=accuracy2)
opt.p <- subset(acc.test,cnt==max(cnt))[1,]
sub <- paste("num of parameter:",opt.p$p,"    accuracy:", opt.p$cnt)
sub

hchart(acc.test,'line',hcaes(p,cnt))%>%
  hc_title(text='Accuracy With Threshold(xgboost)')%>%
  hc_subtitle(text=sub)%>%
  hc_add_theme(hc_theme_google())%>%
  hc_xAxis(title=list(text = 'Number of Parameters'))%>%
  hc_yAxis(title=list(text = 'Accuracy'))

pred.test.xgb.p <- predict(xgb.fit,dtest)
pred.xgb <- ifelse(pred.test.xgb.p < opt.p$p,'0','1')
confusionMatrix(data = factor(pred.xgb,levels = c('0','1')),as.factor(test$Churn))
roc.xgb <- roc(as.factor(test$Churn),pred.test.xgb.p,quiet=T)
plot(roc.xgb,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),max.auc.polygon=T,auc.polygon.col='skyblue',print.thres=T)




