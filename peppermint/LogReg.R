library(data.table)
library(dplyr)
library(car)
library(caret)
library(moonBook)
library(pROC)
library(reshape2)
library(ggplot2)
library(mice)
library(boot)
library(rms)

setwd("/state/partition1/home/")

####################### 데이터 불러오기 #########
for(i in 2010:2013){
  assign(paste0("jk", i), fread(paste0("./nhis2/01.jk/nhid_jk_", i,".txt")))
  assign(paste0("gj", i), fread(paste0("/state/partition1/home/yd0011/DataCampus/dat/nhid_gj_", i,".txt")))
  assign(paste0("gy", i), fread(paste0("./nhis2/02.T120/nhid_gy20_t1_", i,".txt")))
}

################################################

## 질병코드 저장

hepaSeq <- c("K70", "K700", "K701", "K702", "K703", 
           "K704", "K705", "K706", "K707", "K708", 
           "K709")

diaSeq1 <- paste0("E", seq(10, 14, 1))
diaSeq2 <- paste0("E", seq(100, 149, 1))
diaSeq3 <- paste0("E", seq(1000, 1499, 1))

kidneySeq1 <- paste0("N", seq(170, 199, 1))
kidneySeq2 <- paste0("N", seq(17, 19, 1))
kidneySeq3 <- c("N990", "O084", "O904", "P960", "I120", "I129", "I131", "I132")

## 건강검진 데이터, 진료 데이터, 자격 데이터 병합
data2010 <- left_join(gj2010, gy2010, by="PERSON_ID")
data2010 <- left_join(data2010, jk2010, by="PERSON_ID")

data2011 <- left_join(gj2011, gy2011, by="PERSON_ID")
data2011 <- left_join(data2011, jk2011, by="PERSON_ID")

data2012 <- left_join(gj2012, gy2012, by="PERSON_ID")
data2012 <- left_join(data2012, jk2011, by="PERSON_ID")

data2013 <- left_join(gj2013, gy2013, by="PERSON_ID")
data2013 <- left_join(data2013, jk2013, by="PERSON_ID")

## 질병 유무
for (i in 2010:2013) {
  assign("data", get(paste0("data", i)))
  
  data$BMI <- (data$WEIGHT/(data$HEIGHT*data$HEIGHT)*10000)
  
  data$IS_HEPA <- 0
  data$IS_HEPA <- ifelse( data$MAIN_SICK %in% hepaSeq |
                            data$SUB_SICK %in% hepaSeq , 1, 0)
  
  data$IS_OBES <- 0
  data$IS_OBES <- ifelse(data$BMI>= 25,1,0)
  
  data$IS_BP <- 0
  data$IS_BP <- ifelse(data$BP_HIGH >=140| data$BP_LWST >=90, 1,0)
  
  data$IS_LIP <- 0
  data$IS_LIP <-  ifelse(data$TOT_CHOLE>=230 | data$TRIGLYCERIDE>=200 | data$LDL_CHOLE>=150, 1, 0)
  
  data$IS_DIA <- 0
  data$IS_DIA <- ifelse(data$MAIN_SICK %in% diaSeq1 |
                       data$SUB_SICK %in% diaSeq1 |
                       data$MAIN_SICK %in% diaSeq2 |
                       data$SUB_SICK %in% diaSeq2 |
                       data$MAIN_SICK %in% diaSeq3 |
                       data$SUB_SICK %in% diaSeq3 , 1, 0)
  
  data$IS_KID <- 0
  data$IS_KID <- ifelse(data$MAIN_SICK %in% kidneySeq1 |
                          data$SUB_SICK %in% kidneySeq1 |
                          data$MAIN_SICK %in% kidneySeq2 |
                          data$SUB_SICK %in% kidneySeq2 | 
                          data$MAIN_SICK %in% kidneySeq3 |
                          data$SUB_SICK %in% kidneySeq3, 1, 0) 
  
  data <- data[order(data$IS_HEPA, decreasing = T),]
  y <- data[!duplicated(data$PERSON_ID),]
  assign(paste0("y",i), y)
}

## 필요 변수 선별

y2013 <- y2013[,c(2, 4:41, 69, 70, 81:87)]
y2013 <- y2013[,c(1, 40:48, 2:39)]
y2010 <- y2010[, colnames(y2013)]
y2011 <- y2011[, colnames(y2013)]
y2012 <- y2012[, colnames(y2013)]

## 데이터 병합 및 중복 제거
y <- rbind(y2010, y2011, y2012, y2013)
y <- y[order(y$IS_HEPA, y$IS_KID, y$IS_OBES, y$IS_LIP, y$IS_DIA, decreasing=T),]
y <- y[!duplicated(y$PERSON_ID),]


## 결측치 대체
y[,27:38] <- sapply(y[,27:38], function(x) ifelse(is.na(x), 0, x))

y$SMK_STAT_TYPE_RSPS_CD[is.na(y$SMK_STAT_TYPE_RSPS_CD)] <- 1
y$DRNK_HABIT_RSPS_CD[is.na(y$DRNK_HABIT_RSPS_CD)] <- 1
y$PAST_SMK_TERM_RSPS_CD[is.na(y$PAST_SMK_TERM_RSPS_CD)] <- 0
y$PAST_DSQTY_RSPS_CD[is.na(y$PAST_DSQTY_RSPS_CD)] <- 0
y$CUR_SMK_TERM_RSPS_CD[is.na(y$CUR_SMK_TERM_RSPS_CD)] <- 0
y$CUR_DSQTY_RSPS_CD[is.na(y$CUR_DSQTY_RSPS_CD)] <- 0
y$TM1_DRKQTY_RSPS_CD[is.na(y$TM1_DRKQTY_RSPS_CD)] <- 0

## 변수 타입 변경
for(i in 27:38){
  y[,i] <- factor(y[,i])
}
y$SMK_STAT_TYPE_RSPS_CD <- as.factor(y$SMK_STAT_TYPE_RSPS_CD)
y$SEX <- as.factor(y$SEX)

## 결측치 제거
yy <- y[complete.cases(y), ]

## 이상치 조절
for(i in c("TRIGLYCERIDE", "SGOT_AST", "SGPT_ALT", "GAMMA_GTP" )){
  yy[,i] <- ifelse(yy[,i]>=300, 300, yy[,i])
  }
yy$LDL_CHOLE <- ifelse(yy$LDL_CHOLE>=300, 300, yy$LDL_CHOLE)
yy$HDL_CHOLE <- ifelse(yy$HDL_CHOLE>=300, 300, yy$HDL_CHOLE)
yy$BLDS <- ifelse(yy$BLDS>=300, 300, yy$BLDS)


##### 전처리 완료 데이터 : yy

# correlation, hist (상관계수 계산 및 그래프)
library(PerformanceAnalytics)
chart.Correlation(yy[,c(6:21)], histogram=TRUE, col="grey10", pch=1)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
usr <- par("usr"); on.exit(par(usr)) #on.exit() par()함수 인자가 있으면 실해
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y)) #상관계수 절대값
txt <- format(c(r, 0.123456789), digits=digits)[1] #상관계수 자릿수 지정
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex.cor * r) #상관계수 크기에 비례하게 글자지정
}
pairs(~., data=yy[,3:15, 37],
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20)


table(yy$IS_BP)
table(yy$IS_DIA)
table(yy$IS_OBES)
table(yy$IS_HEPA)
table(yy$IS_LIP)
table(yy$IS_KID)

### 샘플링
set.seed(1000)
mydata <- yy


## hepa, kid 는 9:1로 샘플링
pidx_hepa <- sample(yy[yy$IS_HEPA==0, "PERSON_ID"], 41742, replace =F)
pidx_kid <- sample(yy[yy$IS_KID==0, "PERSON_ID"], 10872, replace =F)

tmp_hepa <- yy[yy$PERSON_ID %in% pidx_hepa,]
tmp_kid <- yy[yy$PERSON_ID %in% pidx_kid,]

mydata_hepa <- rbind(tmp_hepa, yy[yy$IS_HEPA==1,])
mydata_kid <- rbind(tmp_kid, yy[yy$IS_KID==1,])


table(mydata_hepa$IS_HEPA)
table(mydata_kid$IS_KID)


## train-test 7:3 구분
inTrain <- createDataPartition(mydata$IS_BP, p=0.7, list=FALSE)
y_train <- yy[inTrain,]
y_test <- yy[-inTrain,]

inTrain <- createDataPartition(mydata$IS_DIA, p=0.7, list=FALSE)
y_train <- yy[inTrain,]
y_test <- yy[-inTrain,]

inTrain <- createDataPartition(mydata_hepa$IS_HEPA, p=0.7, list=FALSE)
y_train1 <- mydata_hepa[inTrain,]
y_test1 <- mydata_hepa[-inTrain,]

inTrain <- createDataPartition(mydata_kid$IS_KID, p=0.7, list=FALSE)
y_train2 <- mydata_kid[inTrain,]
y_test2 <- mydata_kid[-inTrain,]



###### 1차 변수 선택 t.test
t.test_p.value_df <- data.frame() # blank data.frame for saving
y_names <- colnames(mydata[,-c(1:4)])
y_names

for (i in 1:17) {
  t.test_p.value <- t.test(mydata[,y_names[i]] ~ mydata$IS_HEPA, alternative = "less", var.equal = TRUE)$p.value
  t.test_p.value_df[i,1] <- y_names[i]
  t.test_p.value_df[i,2] <- t.test_p.value
}
drnk.t.test <-  t.test(mydata$DRNK_HABIT_RSPS_CD ~ mydata$IS_HEPA, var.equal = TRUE)$p.value
t.test_p.value_df[18,1] <- "DRNK_HABIT_RSPS_CD"
t.test_p.value_df[18,2] <- drnk.t.test
t.test_p.value_df <- t.test_p.value_df[order(t.test_p.value_df$V2),]
t.test_p.value_df

y_names_sorted <- t.test_p.value_df$V1

# boxplot
y_sorted <- data.frame(mydata[, y_names_sorted])
tbox <- data.frame(mydata$IS_HEPA, y_sorted)
t_box <- melt(tbox, id.var = "mydata.IS_HEPA")

ggplot(data = t_box, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=as.factor(mydata.IS_HEPA))) +
  theme_bw() + # white background
  coord_flip() # flip the x & y-axis


##### modeling

# model_1 
colnames(y_train1)
model <- y_train1[,-c(1:4, 6:10)] ### - LDL, HDL, Height
colnames(model)
fit_full <- glm(formula = IS_HEPA ~ ., 
                family = "binomial", data = y_train)
colnames(model)

res1 <- c()
res2 <- c()
for (i in 1:39) {
  fit <- glm(IS_HEPA ~ model[,i] + SEX + AGE_GROUP + GAMMA_GTP + SGPT_ALT + 
               SGOT_AST + DRNK_HABIT_RSPS_CD + TM1_DRKQTY_RSPS_CD + WAIST + HCHK_HPLPDM_PMH_YN ,
             family = "binomial", data = y_train1)
  result1 <- summary(fit)
  res1 <- list(res1, result1)

  x2= 2*(logLik(fit_full)-logLik(fit)) # log-likelihood ratio test statistic
  as.numeric(x2)
  pval=1-pchisq(x2,7)
  as.numeric(pval)

  result2 <- c(result1$coefficients[2,1],
               result1$coefficients[2,2],
               result1$coefficients[2,3],
               result1$coefficients[2,4],
               result1$aic,
               result1$deviance,
               result1$df.residual,
               pval)
  res2 <- rbind(res2, result2)
}
df.res <- as.data.frame(res2, row.names = F)
df.res$names <- colnames(model)[1:39]
df.res <- df.res[,c(9, 1:8)]
colnames(df.res) <- c("variable", "estimate", "std.error", "z.value",
                      "p.value", "aic", "deviance", "df.residual", "pval")
df.res[,2:5] <- round(df.res[,2:5], digits=3)
df.res1 <- df.res
df.res1[order(abs(df.res1$estimate), decreasing = T),]

# model_2
colnames(y_train2)
model <- y_train2[,-c(1:4, 6:10)]
colnames(model)
res1 <- c()
res2 <- c()
for (i in 1:39) {
  fit <- glm(IS_KID ~ model[,i] + SEX + AGE_GROUP + HCHK_DIABML_PMH_YN + TOT_CHOLE + FMLY_DIABML_PATIEN_YN +
               OLIG_PROTE_CD + CREATININE + HMG + BMI + BP_HIGH,
             data=y_train2, family="binomial")
  result1 <- summary(fit)
  res1 <- list(res1, result1)
  
  x2= 2*(logLik(fit_full)-logLik(fit)) # log-likelihood ratio test statistic
  as.numeric(x2)
  pval=1-pchisq(x2,11)
  as.numeric(pval)
  
  result2 <- c(result1$coefficients[2,1],
               result1$coefficients[2,2],
               result1$coefficients[2,3],
               result1$coefficients[2,4],
               result1$aic,
               result1$deviance,
               result1$df.residual,
               pval)
  res2 <- rbind(res2, result2)
}
df.res <- as.data.frame(res2, row.names = F)
df.res$names <- colnames(model)[1:39]
df.res <- df.res[,c(9, 1:8)]
colnames(df.res) <- c("variable", "estimate", "std.error", "z.value",
                      "p.value", "aic", "deviance", "df.residual", "pval")
df.res[,2:5] <- round(df.res[,2:5], digits=3)
df.res2 <- df.res
df.res2[order(abs(df.res2$estimate), decreasing = T),]


### model validation

## model_bp
set.seed(1000)
inTrain <- createDataPartition(mydata$IS_BP, p=0.7, list=FALSE)
y_train <- yy[inTrain,]
y_test <- yy[-inTrain,]
# 
# colnames(y_train)
# y_train <- y_train[,-c(1,4,6,8,9,10, 27:48)]
# 
# cv_folds <- createMultiFolds(y_train$IS_BP, k = 5, times = 3)
# cv_ctrl <- trainControl(method = "cv", number = 5,
#                         repeats = 3, 
#                         index = cv_folds, 
#                         verboseIter = TRUE)
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
# 
# system.time( cv_bp <- train(IS_BP~., data=y_train, method="glm", trControl = ctrl, family="binomial"))


fit_bp <- glm(formula = IS_BP ~ SEX + AGE_GROUP + BP_HIGH + HCHK_HPRTS_PMH_YN + 
                WAIST + HMG + HCHK_DIABML_PMH_YN + OLIG_PROTE_CD + WLK30_WEK_FREQ_ID + CUR_DSQTY_RSPS_CD, 
              family = "binomial", data = y_train)

ORplot(fit_bp)
vif(fit_bp)
head(yy,)
pred_bp <- predict(fit_bp, newdata = y_test, type = "response")

hist(pred_bp)
summary(pred_bp)

pred_logit <- log(pred_bp/(1-pred_bp))
summary(pred_logit)


nom <- nomogram(fit_bp$fitted.values, fun=function(x)1/(1+exp(-x)),  # or fun=plogis
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Risk of Death")

plogis(fit_bp$fitted.values)
summary(fit_bp$fitted.values)

## model_dia
set.seed(1000)
inTrain <- createDataPartition(mydata$IS_DIA, p=0.7, list=FALSE)
y_train <- yy[inTrain,]
y_test <- yy[-inTrain,]
fit_dia <- glm(formula = IS_DIA ~ SEX + AGE_GROUP + BLDS + HCHK_DIABML_PMH_YN + TOT_CHOLE +
                 FMLY_DIABML_PATIEN_YN + BP_HIGH + SGPT_ALT + MOV30_WEK_FREQ_ID + OLIG_PROTE_CD + TRIGLYCERIDE,
               family = "binomial", data = y_train)
vif(fit_dia)
summary(fit_dia)
ORplot(fit_dia)
pred_dia <- predict(fit_dia, newdata = y_train, type = "response")

pred_logit <- log(pred_dia/(1-pred_dia))
summary(pred_logit)

## model_obes
set.seed(1000)
inTrain <- createDataPartition(mydata$IS_OBES, p=0.7, list=FALSE)
y_train <- yy[inTrain,]
y_test <- yy[-inTrain,]
fit_obes <- glm(IS_OBES ~ WEIGHT + SEX + WAIST + AGE_GROUP + SGPT_ALT + 
                  BP_HIGH + TRIGLYCERIDE + TOT_CHOLE + BP_LWST + HMG ,
                data=y_train, family="binomial")
vif(fit_obes)
ORplot(fit_obes)
pred_obes <- predict(fit_obes, newdata = yy, type = "response")

pred_logit <- log(pred_obes/(1-pred_obes))
summary(pred_logit)

## model_hepa
set.seed(1000)
pidx_hepa <- sample(yy[yy$IS_HEPA==0, "PERSON_ID"], 41742, replace =F)
tmp_hepa <- yy[yy$PERSON_ID %in% pidx_hepa,]
mydata_hepa <- rbind(tmp_hepa, yy[yy$IS_HEPA==1,])

inTrain <- createDataPartition(mydata_hepa$IS_HEPA, p=0.7, list=FALSE)
y_train1 <- yy[inTrain,]
y_test1 <- yy[-inTrain,]

fit_hepa <- glm(formula = IS_HEPA ~ SEX + AGE_GROUP + GAMMA_GTP + SGPT_ALT + HCHK_ETCDSE_PMH_YN +
                  SGOT_AST + DRNK_HABIT_RSPS_CD + TM1_DRKQTY_RSPS_CD + WAIST + HCHK_HPLPDM_PMH_YN,
                family = "binomial", data = y_train1)
summary(fit_hepa)
ORplot(fit_hepa)
vif(fit_hepa)

pred_hepa <- predict(fit_hepa, newdata = yy, type = "response")
pred_logit <- log(pred_hepa/(1-pred_hepa))
summary(pred_logit)


## model_lip
set.seed(1000)
inTrain <- createDataPartition(mydata$IS_LIP, p=0.7, list=FALSE)
y_train <- yy[inTrain,]
y_test <- yy[-inTrain,]
fit_lip <- glm(IS_LIP~ TOT_CHOLE + HDL_CHOLE + GAMMA_GTP + BLDS + BP_LWST + HCHK_HPLPDM_PMH_YN +
                 DRNK_HABIT_RSPS_CD + CUR_DSQTY_RSPS_CD + SEX + HCHK_HDISE_PMH_YN + HMG +
               WLK30_WEK_FREQ_ID + AGE_GROUP + BMI, data=y_train, family="binomial")
summary(fit_lip)
ORplot(fit_lip)
vif(fit_lip)
pred_lip <- predict(fit_lip, newdata = yy, type = "response")
pred_logit <- log(pred_lip/(1-pred_lip))
summary(pred_logit)

## model_kid
set.seed(1000)
pidx_kid <- sample(yy[yy$IS_KID==0, "PERSON_ID"], 10872, replace =F)
tmp_kid <- yy[yy$PERSON_ID %in% pidx_kid,]
mydata_kid <- rbind(tmp_kid, yy[yy$IS_KID==1,])

inTrain <- createDataPartition(mydata_kid$IS_KID, p=0.7, list=FALSE)
y_train2 <- yy[inTrain,]
y_test2 <- yy[-inTrain,]

fit_kid <- glm(formula = IS_KID ~ SEX + AGE_GROUP + HCHK_DIABML_PMH_YN + TOT_CHOLE + FMLY_DIABML_PATIEN_YN +
                OLIG_PROTE_CD + CREATININE + BMI,
               family = "binomial", data = y_train2)
table(mydata_kid$IS_KID)
summary(fit_kid)
ORplot(fit_kid)

pred_kid <- predict(fit_kid, newdata = yy, type = "response")
pred_logit <- log(pred_kid/(1-pred_kid))
summary(pred_logit)
hist(pred_logit)


## ROC Curve
ROC_bp = roc(y_test$IS_BP,pred_bp)
ROC_obes = roc(y_test$IS_OBES,pred_obes)
ROC_hepa = roc(y_test1$IS_HEPA,pred_hepa)
ROC_dia = roc(y_test$IS_DIA,pred_dia)
ROC_lip = roc(y_test$IS_LIP,pred_lip)
ROC_kid = roc(y_test2$IS_KID,pred_kid)

plot.roc(ROC_kid, 
         col="black",  
         print.auc=TRUE, 
         print.auc.cex = 2,
         asp=NA,
         max.auc.polygon=TRUE, 
         max.auc.polygon.col="white", 
         identity.col="darkgrey", identity.lty=3, identity.lwd=1, 
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "Dim gray",
         print.thres.cex=2,
         grid.col = "white",
         auc.polygon=TRUE, auc.polygon.col="LightPink")


### 모델링 beta 수치
est_bp <- c(-50.88, 0.2367, 0.2434, 0.04727, 0.2294, 0.4262, -0.08413, -0.03689, 0.0009016, -0.04478, 0.01402)
est_dia <- c(-7.2393024, -0.3474195, 0.1113002, 0.013037, 3.1471214, -0.0037631, 0.0329514, 0.2652377, 0.0034779, 0.0296456, 0.0359996, 0.0005807)
est_obes <- c(-36.49, 0.2816, 3.669, 0.1315, 0.2025, 0.009386, 0.007228, 0.001962, 0.001502, 0.006868, 0.01317)
est_hepa <- c(-5.3401212, -1.1277602, 0.1217728, 0.0086667, 0.0044314, 0.4019295, 0.0082866, 0.1225146, 0.0236187, 0.0098564, 0.6996902)
est_lip <- c(-13.98, 0.06456, -0.05503, 0.009454, 0.005597, 0.009672, 0.6347, 0.04529, 0.01103, -0.0439, 0.1949, 0.05179, -0.008044, 0.004203, 0.03885)
est_kid <- c(-5.208346, -0.5098, 0.23954, 0.682971, -0.011484, 0.3661, 0.955393, 0.445149, 0.023064)

## factor, offset 입력
f_bp <- -1.52
o_bp <- 54.61

f_dia <- -11.1694
o_dia <- 26.9742

f_obes <- -1.87744
o_obes <- 67.67047

f_hepa <- -10.4351456
o_hepa <- 47.22946885

f_lip <- -2.09338595
o_lip <- 59.2346581

f_kid <- -2.84276658
o_kid <- 82.94340052

summary(score_bp)

## 데이터 수치 변경

yy$SEX[yy$SEX==1] <- 0
yy$SEX[yy$SEX==2] <- 1

for(i in 27:38){
  yy[,i] <- as.numeric(yy[,i])
}


## 스코어 계산
score <- data.frame(ID=yy$PERSON_ID)

score$bp <- ((-5.088e+01) + yy$BP_HIGH*(2.367e-01) + yy$BP_LWST*(2.434e-01) + yy$AGE_GROUP*(4.727e-02) +
  yy$SEX*(2.294e-01) + yy$HCHK_DIABML_PMH_YN*(4.262e-01) + yy$HMG *(-8.413e-02) +
  yy$MOV20_WEK_FREQ_ID*(-3.689e-02) + yy$TRIGLYCERIDE*(9.016e-04) + yy$BMI*(-4.478e-02) +
    yy$WAIST*(1.402e-02))*f_bp+o_bp

score$dia <- ((-7.2393024)+ yy$SEX*(-0.3474195) + yy$AGE_GROUP*0.1113002 + yy$BLDS*0.0130370 + yy$HCHK_DIABML_PMH_YN*3.1471214 +
                yy$TOT_CHOLE*(-0.0037631) + yy$BMI*0.0329514 + yy$FMLY_DIABML_PATIEN_YN*0.2652377 + yy$SGPT_ALT*0.0034779 +
            yy$MOV30_WEK_FREQ_ID*0.0296456 + yy$OLIG_PROTE_CD*0.0359996 + yy$TRIGLYCERIDE*0.0005807)*f_dia + o_dia

score$obes <- ((-36.490000) + yy$WEIGHT*0.281600 + yy$SEX*3.669000 + yy$WAIST*0.131500 +
                 yy$AGE_GROUP*0.202500 + yy$SGPT_ALT*0.009386 + yy$BP_HIGH*0.007228 +
                 yy$TRIGLYCERIDE*0.001962 + yy$TOT_CHOLE*0.001502 + yy$BP_LWST*0.006868 + yy$HMG*0.013170)*f_obes + o_obes

score$hepa <- ((-5.3401212) + yy$SEX*(-1.1277602) + yy$AGE_GROUP*0.1217728 + yy$GAMMA_GTP*0.0086667 +
                 yy$SGPT_ALT*0.0044314 + yy$HCHK_ETCDSE_PMH_YN*0.4019295 + yy$SGOT_AST*0.0082866 +
                 yy$DRNK_HABIT_RSPS_CD*0.1225146 + yy$TM1_DRKQTY_RSPS_CD*0.0236187 + yy$WAIST*0.0098564 +
                 yy$HCHK_HPLPDM_PMH_YN * 0.6996902)* f_hepa + o_hepa

score$lip <- ((-13.980000) + yy$TOT_CHOLE*0.064560 + yy$HDL_CHOLE*(-0.055030) + yy$GAMMA_GTP*0.009454 +
                yy$BLDS*0.005597 + yy$BP_LWST*0.009672 + yy$HCHK_HPLPDM_PMH_YN * 0.634700 + 
                yy$DRNK_HABIT_RSPS_CD*0.045290 + yy$CUR_DSQTY_RSPS_CD*0.011030 + yy$SEX*(-0.043900) + 
                yy$HCHK_HDISE_PMH_YN*0.194900 + yy$HMG*0.051790 + yy$WLK30_WEK_FREQ_ID*(-0.008044) +
                yy$AGE_GROUP*0.004203 + yy$BMI*0.038850)*f_lip + o_lip

score$kid <- ((-5.208346) + yy$SEX*(-0.509800) + yy$AGE_GROUP*0.239540 + yy$HCHK_DIABML_PMH_YN*0.682971 +
              yy$TOT_CHOLE*(-0.011484) + yy$FMLY_DIABML_PATIEN_YN*0.366100 + yy$OLIG_PROTE_CD*0.955393 + 
              yy$CREATININE*0.445149 + yy$BMI*0.023064)*f_kid + o_kid


summary(score)
## score 그래프
data_frame(score_bp = score$bp) %>%
  ggplot(., aes(score_bp)) + 
  geom_density(color="red", fill="pink", alpha=0.5) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(score_bp)),
           color="black", linetype="dashed", size=2) +
  geom_vline(aes(xintercept=quantile(score_bp, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(score_bp, 0.75)),
             color="blue", linetype="dashed", size=1)

data_frame(score_dia = score$dia) %>%
  ggplot(., aes(score_dia)) + 
  geom_density(color="red", fill="pink", alpha=0.5) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(score_dia)),
             color="black", linetype="dashed", size=2) +
  geom_vline(aes(xintercept=quantile(score_dia, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(score_dia, 0.75)),
             color="blue", linetype="dashed", size=1)

data_frame(score_obes = score$obes) %>%
  ggplot(., aes(score_obes)) + 
  geom_density(color="red", fill="pink", alpha=0.5) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(score_obes)),
             color="black", linetype="dashed", size=2)+
  geom_vline(aes(xintercept=quantile(score_obes, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(score_obes, 0.75)),
             color="blue", linetype="dashed", size=1)

data_frame(score_hepa = score$hepa) %>%
  ggplot(., aes(score_hepa)) + 
  geom_density(color="red", fill="pink", alpha=0.5) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(score_hepa)),
             color="black", linetype="dashed", size=2)+
  geom_vline(aes(xintercept=quantile(score_hepa, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(score_hepa, 0.75)),
             color="blue", linetype="dashed", size=1)

data_frame(score_lip = score$lip) %>%
  ggplot(., aes(score_lip)) + 
  geom_density(color="red", fill="pink", alpha=0.5) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(score_lip)),
             color="black", linetype="dashed", size=2)+
  geom_vline(aes(xintercept=quantile(score_lip, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(score_lip, 0.75)),
             color="blue", linetype="dashed", size=1)

data_frame(score_kid = score$kid) %>%
  ggplot(., aes(score_kid)) + 
  geom_density(color="red", fill="pink", alpha=0.5) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(score_kid)),
             color="black", linetype="dashed", size=2)+
  geom_vline(aes(xintercept=quantile(score_kid, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(score_kid, 0.75)),
             color="blue", linetype="dashed", size=1)

data_frame(tot = score$tot) %>%
  ggplot(., aes(tot)) + 
  geom_density(color="red", fill="pink", alpha=0.75) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=median(tot)),
             color="yellow", linetype="solid", size=2) +
  geom_vline(aes(xintercept=quantile(tot, 0.25)),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(tot, 0.75)),
             color="Green Yellow", linetype="dashed", size=1)


data_frame(tot = score$tot) %>%
  ggplot(., aes(tot)) + 
  geom_density(color="white", fill="Light Pink 1", alpha=0.75) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=66.0),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=69.5),
             color="yellow", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=72.7),
             color="green", linetype="dashed", size=1)

names(score) <- c("ID", "bp", "dia", "obes", "hepa", "lip", "kid")


## 가중치 삽입
weight <- c(0.193, 0.201, 0.143, 0.153, 0.119, 0.191)

## 종합점수 계산
score$tot <- (score$bp*weight[1]+score$dia*weight[2]+score$obes*weight[3]+
                score$hepa*weight[4]+score$lip*weight[5]+score$kid*weight[6])

## 데이터 저장
write.csv(score, file = "score.csv", row.names = F)


######################## 정신건강
dat <- fread("~/DataCampus/dat/2015춘천조사자료.csv")
dat <- dat[-which(dat$sex %in% c("", 3)),]
dat <- dat[-which(dat$income %in% c("", 5)),]
dat <- dat[-which(dat$renthouse %in% c("")),]
dat <- dat[-which(dat$AUDID1 == 5),]
dat <- dat[complete.cases(dat),]
dat <- dat[,c(1, 42:50, 2:41)]

dat$sex[dat$sex=="male"] <- 0
dat$sex[dat$sex=="female"] <- 1
dat$sex <- as.numeric(dat$sex)
table(dat$SmartAG)
dat$AlNorm <- ifelse(dat$AlNorm == 3, 1, 0)
dat$GambDG <- ifelse(dat$GambDG %in% c("high risk"), 1, 0)
dat$SmartAG <- ifelse(dat$SmartAG %in% c("hig risk", "at risk"), 1, 0)
dat$religion[dat$religion==0] <- "none"
dat$marital[dat$marital==""] <- "single"
dat$income <- factor(dat$income, levels = c("below 1 million won", "101-300", "301-500", "exceed 500"))
dat$sex <- factor(dat$sex)
dat$marital <- factor(dat$marital)
dat$religion <- factor(dat$religion)
dat$occupation <- factor(dat$occupation)
dat$education <- factor(dat$education,
                        levels = c("none", "elementary", "middle", "high", "others", "university", "graduate"))
dat$renthouse <- factor(dat$renthouse, levels = c("other", "permanent rental", "monthly", "deposit", "own"))
dat$AUDID1 <- ifelse(dat$AUDID1 %in% c("1below 1/month", "no drinking"), 1, 
                     ifelse(dat$AUDID1 == "2-4/month", 2,
                            ifelse(dat$AUDID1 == 3, 3, 4)))

# dat$AlNorm <- factor(dat$AlNorm)
colnames(dat)

library(caret)
################ alcohol
set.seed(1000)
inTrain <- createDataPartition(dat$AlNorm, p=0.7, list=FALSE)
training <- dat[inTrain,]
testing <- dat[-inTrain,]

dat %>% group_by(AlNorm) %>% summarise(mean(AlcUseD))
str(dat)

fit_al_con <- glm(AlNorm~AUDID1+AUDIT2+AUDIT3+AUDIT4+AUDIT5+AUDIT6+AUDIT7+AUDIT8+AUDIT9+AUDIT10,
                  data = training, family = "binomial")
fit_al_full <- glm(AlNorm ~.-ID -AlcUseD , data=training, family="binomial")

fit_al <- step(fit_al_full, scope = list(lower = fit_al_con, upper = fit_al_full), direction="backward")

fit_al <- glm(AlNorm~AUDID1+AUDIT2+AUDIT3+AUDIT5+AUDIT7+AUDIT8+AUDIT9,
              data = training, family = "binomial")
summary(fit_al)


# modeling
colnames(training)
model <- as.data.frame(training[,-1])
colnames(model)
fit_full <- glm(AlNorm ~.-ID -AlcUseD , data=training, family="binomial")

res1 <- c()
res2 <- c()
for (i in 1:49) {
  fit <- glm(AlNorm ~ model[,i] + AUDID1+AUDIT2+AUDIT3+AUDIT5+AUDIT7+AUDIT8+AUDIT9+AUDIT3+AUDIT5+AUDIT6+
               SPHONE13,
             data = training, family = "binomial")
  result1 <- summary(fit)
  res1 <- list(res1, result1)
  
  x2= 2*(logLik(fit_full)-logLik(fit)) # log-likelihood ratio test statistic
  as.numeric(x2)
  pval=1-pchisq(x2,7)
  as.numeric(pval)
  
  result2 <- c(result1$coefficients[2,1],
               result1$coefficients[2,2],
               result1$coefficients[2,3],
               result1$coefficients[2,4],
               result1$aic,
               result1$deviance,
               result1$df.residual,
               pval)
  res2 <- rbind(res2, result2)
}
df.res <- as.data.frame(res2, row.names = F)
df.res$names <- colnames(model)[1:49]
df.res <- df.res[,c(9, 1:8)]
colnames(df.res) <- c("variable", "estimate", "std.error", "z.value",
                      "p.value", "aic", "deviance", "df.residual", "pval")
df.res[,2:5] <- round(df.res[,2:5], digits=3)
df.res1 <- df.res
df.res1[order(abs(df.res1$p.value)),]


fit_al <- glm(AlNorm~AUDID1+AUDIT2+AUDIT3+AUDIT5+AUDIT7+AUDIT8+AUDIT9+AUDIT3+AUDIT5+AUDIT6+
                SPHONE13,
              data = training, family = "binomial")
summary(fit_al)
vif(fit_al)
ORplot(fit_al)

pred_al <- predict(fit_al, newdata = testing, type = "response")
pred_logit <- log(pred_al/(1-pred_al))
summary(pred_logit)
hist(pred_logit)

ROC_al <- roc(testing$AlNorm, pred_al)

plot.roc(ROC_al, 
         col="black",  
         print.auc=TRUE, 
         print.auc.cex = 2,
         asp=NA,
         max.auc.polygon=TRUE, 
         max.auc.polygon.col="white", 
         identity.col="darkgrey", identity.lty=3, identity.lwd=1, 
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "Dim gray",
         print.thres.cex=2,
         grid.col = "white",
         auc.polygon=TRUE, auc.polygon.col="Lavender Blush 1")

############## gamble
set.seed(1000)
inTrain <- createDataPartition(dat$GambDG, p=0.6, list=FALSE)
training <- dat[inTrain,]
testing <- dat[-inTrain,]

dat %>% group_by(GambDG) %>% summarise(mean(GambD))
str(training)

fit_gamb_con <- glm(GambDG~1,
                  data = training, family = "binomial")
fit_gamb_full <- glm(GambDG~.-ID,
                     data = training, family = "binomial")

fit_gamb <- step(fit_gamb_full, scope = list(lower = fit_gamb_con, upper = fit_gamb_full), direction="backward")

summary(fit_gamb)

fit_gamb <- glm(formula = GambDG ~ GAMB2  + GAMB6 + GAMB7 + GAMB9, family = "binomial", data = training)

# modeling
colnames(training)
model <- as.data.frame(training[,-1])
colnames(model)
fit_full <- glm(GambDG ~.-ID , data=training, family="binomial")

res1 <- c()
res2 <- c()
for (i in 1:49) {
  fit <- glm(GambDG ~ model[,i]+GAMB6+GAMB7+GAMB9+GAMB2,
             family = "binomial", data = training)
  result1 <- summary(fit)
  res1 <- list(res1, result1)
  
  x2= 2*(logLik(fit_full)-logLik(fit)) # log-likelihood ratio test statistic
  as.numeric(x2)
  pval=1-pchisq(x2,7)
  as.numeric(pval)
  
  result2 <- c(result1$coefficients[2,1],
               result1$coefficients[2,2],
               result1$coefficients[2,3],
               result1$coefficients[2,4],
               result1$aic,
               result1$deviance,
               result1$df.residual,
               pval)
  res2 <- rbind(res2, result2)
}
df.res <- as.data.frame(res2, row.names = F)
df.res$names <- colnames(model)[1:49]
df.res <- df.res[,c(9, 1:8)]
colnames(df.res) <- c("variable", "estimate", "std.error", "z.value",
                      "p.value", "aic", "deviance", "df.residual", "pval")
df.res[,2:5] <- round(df.res[,2:5], digits=3)
df.res1 <- df.res
df.res1[order(abs(df.res1$p.value)),]


fit_gamb <- glm(GambDG ~ GAMB6+GAMB7+GAMB9+GAMB2,
                family = "binomial", data = training)
summary(fit_gamb)
vif(fit_gamb)
ORplot(fit_gamb)

pred_gamb <- predict(fit_gamb, newdata = testing, type = "response")
pred_logit <- log(pred_gamb/(1-pred_gamb))
summary(pred_logit)
hist(pred_logit)

ROC_gamb <- roc(testing$GambDG, pred_gamb)

plot.roc(ROC_gamb, 
         col="black",  
         print.auc=TRUE, 
         print.auc.cex = 2,
         asp=NA,
         max.auc.polygon=TRUE, 
         max.auc.polygon.col="white", 
         identity.col="darkgrey", identity.lty=3, identity.lwd=1, 
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "Dim gray",
         print.thres.cex=2,
         grid.col = "white",
         auc.polygon=TRUE, auc.polygon.col="Lavender Blush 1")

########## smartphone
set.seed(1000)
inTrain <- createDataPartition(dat$SmartAG, p=0.7, list=FALSE)
training <- dat[inTrain,]
testing <- dat[-inTrain,]

dat %>% group_by(SmartAG) %>% summarise(mean(SmartPUseD))
str(training)

fit_sphone_con <- glm(SmartAG~1,
                    data = training, family = "binomial")
fit_sphone_full <- glm(SmartAG~.-ID,
                     data = training, family = "binomial")

fit_sphone <- step(fit_sphone_full, scope = list(lower = fit_sphone_con, upper = fit_sphone_full), direction="backward")

summary(fit_sphone)

fit_sphone <- glm(SmartAG ~ SPHONE1 + SPHONE2 + SPHONE5 + SPHONE7 + SPHONE10 + SPHONE11 + SPHONE13,
                  family = "binomial", data = training)

# modeling
colnames(training)
model <- as.data.frame(training[,-1])
colnames(model)
fit_full <- glm(SmartAG ~.-ID , data=training, family="binomial")

res1 <- c()
res2 <- c()
for (i in 1:49) {
  fit <- glm(SmartAG ~ model[,i]+SPHONE2+SPHONE5+SPHONE7+SPHONE10+SPHONE13+SPHONE11,
             family = "binomial", data = training)
  result1 <- summary(fit)
  res1 <- list(res1, result1)
  
  x2= 2*(logLik(fit_full)-logLik(fit)) # log-likelihood ratio test statistic
  as.numeric(x2)
  pval=1-pchisq(x2,7)
  as.numeric(pval)
  
  result2 <- c(result1$coefficients[2,1],
               result1$coefficients[2,2],
               result1$coefficients[2,3],
               result1$coefficients[2,4],
               result1$aic,
               result1$deviance,
               result1$df.residual,
               pval)
  res2 <- rbind(res2, result2)
}
df.res <- as.data.frame(res2, row.names = F)
df.res$names <- colnames(model)[1:49]
df.res <- df.res[,c(9, 1:8)]
colnames(df.res) <- c("variable", "estimate", "std.error", "z.value",
                      "p.value", "aic", "deviance", "df.residual", "pval")
df.res[,2:5] <- round(df.res[,2:5], digits=3)
df.res1 <- df.res
df.res1[order(abs(df.res1$p.value)),]

fit_sphone <- glm(SmartAG~SPHONE2+SPHONE5+SPHONE7+SPHONE10+SPHONE13+SPHONE11+SPHONE15+SPHONE3,
                  family = "binomial", data = training)
summary(fit_sphone)
vif(fit_sphone)
ORplot(fit_sphone)

pred_sphone <- predict(fit_sphone, newdata = testing, type = "response")
pred_logit <- log(pred_sphone/(1-pred_sphone))

summary(pred_logit)
hist(pred_logit)

ROC_sphone <- roc(testing$SmartAG, pred_sphone)

plot.roc(ROC_sphone, 
         col="black",  
         print.auc=TRUE, 
         print.auc.cex = 2,
         asp=NA,
         max.auc.polygon=TRUE, 
         max.auc.polygon.col="white", 
         identity.col="darkgrey", identity.lty=3, identity.lwd=1, 
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "Dim gray",
         print.thres.cex=2,
         grid.col = "white",
         auc.polygon=TRUE, auc.polygon.col="Lavender Blush 1")

####### scoring

f_al <- -6.51306684034845
o_al <- 59.8219327525849

f_gamb <- -2.53164556962025
o_gamb <- 66.3291139240506

f_sphone <- -0.54054054054054
o_sphone <- 56.7567567567568

score2 <- data.frame(ID=dat$ID)
sf <- function(x) {dat %>% select(all_of(x))}

s <- c()
res <- c()
for (i in 2:length(fit_al$coefficients)) {
   s <- fit_al$coefficients[i]*sf(names(fit_al$coefficients[i]))
   res <- cbind(res,s)
}
score2$al <- (rowSums(res)+fit_al$coefficients[1])*f_al+o_al
summary(score2$al)

s <- c()
res <- c()
for (i in 2:length(fit_gamb$coefficients)) {
  s <- fit_gamb$coefficients[i]*sf(names(fit_gamb$coefficients[i]))
  res <- cbind(res,s)
}
score2$gamb <- (rowSums(res)+fit_gamb$coefficients[1])*f_gamb+o_gamb
summary(score2$gamb)


s <- c()
res <- c()
for (i in 2:length(fit_sphone$coefficients)) {
  s <- fit_sphone$coefficients[i]*sf(names(fit_sphone$coefficients[i]))
  res <- cbind(res,s)
}
score2$sphone <- (rowSums(res)+fit_sphone$coefficients[1])*f_sphone+o_sphone
summary(score2$sphone)

## 가중치 삽입
weight2 <- c(0.216008536454807, 0.213526442468987, 0.0804496960753899)

## 종합점수 계산
score2$sum <- (score2$al*weight2[1]+score2$gamb*weight2[2]+score2$sphone*weight2[3])

## 데이터 저장
write.csv(score2, file = "score2.csv", row.names = F)
summary(score2$sum)

data_frame(sum = score2$sum) %>%
  ggplot(., aes(sum)) + 
  geom_density(color="red", fill="pink", alpha=0.75) + 
  scale_x_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=25)) +
  geom_vline(aes(xintercept=mean(sum)),
             color="black", linetype="dashed", size=2) +
  geom_vline(aes(xintercept=quantile(sum, 0.25)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(sum, 0.75)),
             color="blue", linetype="dashed", size=1)
