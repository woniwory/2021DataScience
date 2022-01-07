# 분석하기 위해 ggplot패키지를 설치, 데이터 프레임과 패키지 연결을 위해 dqlyr패키지를 설치 
#install.packages("ggplot2")
#install.packages("dplyr")
install.packages("tidyverse")

# 패키지 라이브러리에 추가 
library(ggplot2)
library(dplyr)
library(tidyr)

titanic = read.csv(file="C:/Users/woniw/OneDrive/문서/titanic.csv")

# read_csv는 Character로 불러오기 때문에
View(titanic)
str(titanic)
summary(titanic)

# 평균 생존율 구하기
SurvivalRate = mean(titanic$Survived)
print(SurvivalRate)


titanic <- titanic %>% # ticket과 cabin은 파생변수 생성을 위해 문자열로 놔둠
  mutate(Survived = factor(Survived),
         Pclass   = factor(Pclass, ordered = T),
         Sex      = factor(Sex),
         Embarked = factor(Embarked))

titanic$Name <- as.character(titanic$Name)

str(titanic)
summary(titanic)

# 데이터 결측값 확인
for (i in 1:ncol(titanic)) 
{this.na<-is.na(titanic[,i])
cat(colnames(titanic)[i], 
    '\t', sum(this.na), '\n')}

titanic <- titanic %>% 
  drop_na()



# 평균 생존율 구하기
SurvivalRate = mean(titSurvived)
print(SurvivalRate)


# 선실별 탑승객 구하고 시각화하기
pclasstbl <- table(titanic$Pclass)
sum(pclasstbl)


barplot(pclasstbl, main = '선실별 탑승객',
        xlab = '선실 등급',
        ylab = '탑승객수')

# 선실별 탑승객의 생존율 살펴보기
frame1 <- data.frame(titanic$Survived, titanic$Pclass)
View(frame1)
frame1.table<-table(frame1)
#column이 1이면 생존자
survived<-frame1.table[1,1]+frame1.table[2,1]+frame1.table[3,1]



training_set <- training_set %>% 
  mutate(Ages = case_when(
    Age < 10 ~ "Under 10",
    Age < 20 ~ "10 ~ 20",
    Age < 30 ~ "20 ~ 30",
    Age < 40 ~ "30 ~ 40",
    Age < 50 ~ "40 ~ 50",
    Age < 60 ~ "50 ~ 60",
    TRUE ~ "over 60"
  )) 

titanic$Ages <- 
  factor(titanic$Ages,
         levels = c("Under 10", "10 ~ 20", "20 ~ 30", "30 ~ 40", "40 ~ 50", "50 ~ 60", "over 60"))

plot_1 <- ggplot(titanic, aes(x=Survived, fill = Sex)) +
  geom_bar() +
  ggtitle("Survived by Sex") +
  theme(legend.position="bottom")

plot_2 <- ggplot(titanic, aes(x = Survived, fill = Pclass)) +
  geom_bar() +
  ggtitle("Survived by Pclass") +
  theme(legend.position="bottom") 

plot_3 <- titanic %>% 
  ggplot(aes(x = Survived, fill = Ages)) +
  geom_bar() +
  ggtitle("Survived by Ages") +
  theme(legend.position="bottom") 

plot_4 <- titanic %>% 
  ggplot(aes(x = Survived, fill = Embarked)) +
  geom_bar() +
  ggtitle("Survived by Embarked") +
  theme(legend.position="bottom") 

plot_1
plot_2
plot_3
plot_4



