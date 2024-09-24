# 패키지 불러오기 (*추가 패키지 : Car, MASS)
> library(tidyverse)
> library(GGally)
> library(caret)
> library(leaps)

# 데이터 로드
> concrete_data <- read.csv("concrete_data.csv")
> concrete <- as.data.frame(concrete_data)

# 데이터 확인
> str(concrete)

# 산점도 행렬 1
> ggpairs(concrete, lower=list(continuous="smooth"))

# 로그 변환(Blast.Furnace.Slag, Fly.Ash, Superplasticizer, Age)
> concrete_ln <- concrete %>%
    + mutate(ln_Blast.Furnace.Slag=log(Blast.Furnace.Slag),
    + ln_Fly.Ash=log(Fly.Ash),
    + ln_Superplasticizer=log(Superplasticizer),
    + ln_Age=log(Age)) %>%
    + dplyr::select(-Blast.Furnace.Slag,-Fly.Ash,-Superplasticizer,-Age)

# 산점도 행렬 확인 2
> ggpairs(concrete_ln, lower=list(continuous="smooth"))

# 로그 변환([ Blast.Furnace.Slag, Fly.Ash, Superplasticizer, Age ] + 1)
> concrete_ln <- concrete %>%
    + mutate(ln_Blast.Furnace.Slag=log(Blast.Furnace.Slag+1),
    + ln_Fly.Ash=log(Fly.Ash+1),
    + ln_Superplasticizer=log(Superplasticizer+1),
    + ln_Age=log(Age)) %>%
    + dplyr::select(-Blast.Furnace.Slag,-Fly.Ash,-Superplasticizer,-Age)

# 산점도 행렬 확인 3
> ggpairs(concrete_ln, lower=list(continuous="smooth"))

# 최종 로그 변환 
# log(Blast.Furnace.Slag, Superplasticizer) : +1 / log(Age)
> concrete_ln <- concrete %>%
    + mutate(ln_Blast.Furnace.Slag=log(Blast.Furnace.Slag+1),
    + ln_Superplasticizer=log(Superplasticizer+1),
    + ln_Age=log(Age)) %>%
    + dplyr::select(-Blast.Furnace.Slag,-Superplasticizer,-Age)

# 최종 산점도 행렬 확인
> ggpairs(concrete_ln, lower=list(continuous="smooth"))

# 상관관계 히트맵 확인
> ggcorr(concrete_ln, label=TRUE, label_round=2)

# 데이터 분리 (train : 0.8 / test : 0.2)
x.id <- createDataPartition(concrete_ln$Strength, p=0.8,list=FALSE)
> train_c   <- concrete_ln %>% slice(x.id)
> test_c    <- concrete_ln %>% slice(-x.id)

# 선형회귀모델 생성
  ## 절편만 포함 (Null 모델)  
  > fit_null  <- lm(Strength~1, train_c)
  ## 모든 독립 변수 사용(Full 모델)
  > fit_full  <- lm(Strength~.,train_c)

# AIC 전진선택법(fit_null 기준)
> MASS::stepAIC(fit_null,
    +scope=list(lower=fit_null, upper=fit_full),
    +trace=FALSE)
# AIC 후진소거법(fit_full 기준)
> MASS::stepAIC(fit_full,
    +direction="both", trace=FALSE)

# BIC 전진선택법(fit_null 기준)
> MASS::stepAIC(fit_null,scope=list(lower=fit_null, upper=fit_full),
    +k=log(nrow(concrete_ln)),
    +trace=FALSE)
# BIC 후진소거법(fit_full 기준)
> MASS::stepAIC(fit_full,
    +direction="both",
    +k=log(nrow(concrete_ln)),trace=FALSE)

# 변수 선택
> fits <- regsubsets(Strength~., train_c)
