install.packages("dplyr")
install.packages("PerformanceAnalytics")
install.packages("magrittr")
install.packages("rsq")

library(dplyr)
library(PerformanceAnalytics)
library(magrittr)
library(rsq)

# 데이터 불러오기+확인
data <- read.csv("data/survey lung cancer.csv")

head(data)

names(data)

str(data)

ncol(data)

nrow(data)


boxplot(data$AGE)

is.null(data)

# 수치형으로 변환
data[data$LUNG_CANCER == "NO", "LUNG_CANCER"] = 0
data[data$LUNG_CANCER == "YES", "LUNG_CANCER"] = 1
data[data$GENDER == "M", "GENDER"] = 0
data[data$GENDER == "F", "GENDER"] = 1
data$GENDER = as.numeric(data$GENDER)
data$LUNG_CANCER = as.numeric(data$LUNG_CANCER)
str(data)


# 데이터 개수 맞추어 샘플 추출
length(which(data$LUNG_CANCER == 0))
length(which(data$LUNG_CANCER == 1))

s = data[data$LUNG_CANCER == 1,]
head(s)
s1 = sample_n(s, 40, replace = F)
s2 = data[data$LUNG_CANCER == 0,]
S = rbind(s1, s2)

# 상관분석
S_cor = cor(S)
round(S_cor, 2)
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(S_cor)

# 상관계수 +- 0.01 변수 제거
D = subset(S, select = -c(SMOKING, ANXIETY, CHRONIC.DISEASE))
round(cor(D), 2)
ggcorrplot(cor(D))

# 데이터 분할
train = sample_frac(D, size = 0.7)
test = sample_frac(D, size = 0.3)

# 변수 선택
model = glm(LUNG_CANCER ~ 1, data = train)
ss = step(model, direction = "both", scope = LUNG_CANCER ~ GENDER + 
            ALLERGY + YELLOW_FINGERS + SWALLOWING.DIFFICULTY + 
            PEER_PRESSURE + FATIGUE + AGE + SHORTNESS.OF.BREATH + 
            WHEEZING + ALCOHOL.CONSUMING + COUGHING + CHEST.PAIN)
summary(ss)

# 로지스틱 회귀 모델
result = glm(LUNG_CANCER ~ ALLERGY + COUGHING + FATIGUE, data = train, family = binomial())
train %>% select_at(vars(ALLERGY, COUGHING, FATIGUE, LUNG_CANCER)) %>% 
  chart.Correlation(histogram = TRUE, pch = 20)

rsq(result)

# 성능 확인
coef(result)

Train = train[c("ALLERGY", "COUGHING", "FATIGUE", "LUNG_CANCER")]
Test = test[C("ALLERGY", "COUGHING", "FATIGUE", "LUNG_CANCER")]
SSS = D[c("ALLERGY", "COUGHING", "FATIGUE", "LUNG_CANCER")]

# 오버/언더피팅 확인
p1 <- predict(result, newdata = Train, type = "response")
round(p1)
table(round(p1), Train$LUNG_CANCER)

p2 <- predict(result, newdata = Test, type = "response")
round(p2)
table(round(p2), Test$LUNG_CANCER)

# 예측 예시
man1 = data.frame(rbind(c(1,1,1)))
names(man1) = names(SSS)[1:3]
pred = predict(result, man1, type = "response")
pred

man2 = data.frame(rbind(c(1,2,1)))
names(man2) = names(SSS)[1:3]
pred = predict(result, man2, type = "response")
pred

man3 = data.frame(rbind(c(2,1,2)))
names(man3) = names(SSS)[1:3]
pred = predict(result, man3, type = "response")
pred

man4 = data.frame(rbind(c(2,2,2)))
names(man4) = names(SSS)[1:3]
pred = predict(result, man4, type = "response")
pred