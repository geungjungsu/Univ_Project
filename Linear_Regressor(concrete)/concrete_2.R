# 콘크리트 강도 예측 모형

# 패키지 불러오기 (*추가 패키지 : Car, MASS)
library(tidyverse)
library(GGally)
library(caret)
library(leaps)

# 데이터 로드
concrete_data <- read.csv("concrete_data.csv")
concrete <- as.data.frame(concrete_data)

# 데이터 확인
str(concrete)

# 산점도 행렬 1
ggpairs(concrete, lower=list(continuous="smooth"))

# 로그 변환(Blast.Furnace.Slag, Fly.Ash, Superplasticizer, Age)
concrete_ln <- concrete %>%
  mutate(ln_blast_furnace_slag=log(blast_furnace_slag),
         ln_fly.ash=log(fly.ash),
         ln_superplasticizer=log(superplasticizer),
         ln_age=log(age)) %>%
  dplyr::select(-blast_furnace_slag,-fly.ash,-superplasticizer,-age)

# 산점도 행렬 확인 2
ggpairs(concrete_ln, lower=list(continuous="smooth"))

# 로그 변환([ Blast.Furnace.Slag, Fly.Ash, Superplasticizer, Age ] + 1)
concrete_ln <- concrete %>%
  mutate(ln_blast_furnace_slag=log(blast_furnace_slag+1),
         ln_fly.ash=log(fly.ash+1),
         superplasticizer=log(superplasticizer+1),
         ln_age=log(age)) %>%
  dplyr::select(-blast_furnace_slag,-fly.ash,-superplasticizer,-age)

# 산점도 행렬 확인 3
ggpairs(concrete_ln, lower=list(continuous="smooth"))

# 최종 로그 변환 
# log(Blast.Furnace.Slag, Superplasticizer) : +1 / log(Age)
concrete_ln <- concrete %>%
  mutate(ln_blast_furnace_slag=log(blast_furnace_slag+1),
         ln_superplasticizer=log(superplasticizer+1),
         ln_age=log(age)) %>%
  dplyr::select(-blast_furnace_slag,-superplasticizer,-age)

# 최종 산점도 행렬 확인
ggpairs(concrete_ln, lower=list(continuous="smooth"))

# 상관관계 히트맵 확인
ggcorr(concrete_ln, label=TRUE, label_round=2)

# 데이터 분리 (train : 0.8 / test : 0.2)
x.id <- createDataPartition(concrete_ln$Strength, p=0.8,list=FALSE)
train_c   <- concrete_ln %>% slice(x.id)
test_c    <- concrete_ln %>% slice(-x.id)

# 선형회귀모델 생성
## 절편만 포함 (Null 모델)  
fit_null  <- lm(Strength~1, train_c)
## 모든 독립 변수 사용(Full 모델)
fit_full  <- lm(Strength~.,train_c)

# AIC 전진선택법(fit_null 기준)
MASS::stepAIC(fit_null,
              scope=list(lower=fit_null, upper=fit_full),
              trace=FALSE)
# AIC 후진소거법(fit_full 기준)
MASS::stepAIC(fit_full,
              direction="both", trace=FALSE)

# BIC 전진선택법(fit_null 기준)
MASS::stepAIC(fit_null,scope=list(lower=fit_null, upper=fit_full),
              k=log(nrow(concrete_ln)),
              trace=FALSE)
# BIC 후진소거법(fit_full 기준)
MASS::stepAIC(fit_full,
              direction="both",
              k=log(nrow(concrete_ln)),trace=FALSE)

# 변수 선택
fits <- regsubsets(Strength~., train_c)

# 그래프 확인
## 수정된 결정계수 기준
plot(fits,scale="adjr2")
## 변수 선택 패턴 확인
plot(fits)

# 변수 선택 (AIC 단계적 변수 선택)
fit1 <- MASS::stepAIC(fit_full, direction="both", trace=FALSE)

# fit1 회귀 모형 요약 통계표
summary(fit1)

# 다중공선성 확인
car::vif(fit1)

# AIC값, BIC값 확인
AIC(fit1)
BIC(fit1)

# 그래프 프레임 배치 조정(행,열)
par(mfrow=c(2,2))

# 회귀 모델 진단 그래프(1. Residuals vs Fitted, 2. Normal Q-Q, 3. Scale-Location, 4. Residuals vs Leverage)
# 잔차 분석과 모델의 가정 확인(선형성, 독립성, 정규성, 등분산성,..)
plot(fit1)

# 그래프 프레임 배치 조정(행,열)
par(mfrow=c(1,1))

# 조건부 회귀 플롯(독립변수와 종속변수의 관계 시각화)
car::crPlots(fit1)

# 영향력 플롯(Influence Plot) : Cook's distance, Leverage를 통한 이상치 확인
car::influencePlot(fit1)

# 예측
pred_c <- predict(fit1,newdata=test_c)

# 성능 지표 확인(RMSE, MAE, R-squared)
defaultSummary(data.frame(obs=test_c$strength, pred=pred_c))

# 잔차 표준 오차 및 결정계수 확인
summary(fit1)$sigma
summary(fit1)$r.squared

# 독립변수 행렬 생성(x_tr), (x_te)
x_tr <- as.matrix(train_c %>% select(-strength))
x_te <- as.matrix(test_c %>% select(-Strength))

# 교차검증을 통한 Lasso 모형 생성
cvfit_la <- cv.glmnet(x_tr, train_c$Strength)

# Lasso 모형 람다 선택 그래프
plot(cvfit_la)

# Lasso 모형 회귀계수 추출
## λ = lambda.1se
coef(cvfit_la, s="lambda.1se")
## λ = lambda.min
coef(cvfit_la, s="lambda.min")

# 예측
pred_la <- predict(cvfit_la, newx=x_te)

# 성능 지표 확인(RMSE, MAE, R-squared)
defaultSummary(data.frame(obs=test_c$strength, pred=as.numeric(pred_la)))

# 실제값과 예측값을 합친 df 생성
fc <- cbind(test_c, pred=pred_c) %>%
  rownames_to_column(var="ID")

# 예측값과 실제값 사이 산점도
fc %>%
  ggplot(aes(x=strength, y=pred))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1))+
  geom_text(data=slice_max(fc,abs(strength-pred),n=1),              
            aes(label=ID), nudge_y=0.3)+
  labs(y="Predicted values", x="Observed values")
