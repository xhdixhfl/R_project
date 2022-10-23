# 원본데이터가 1000명의 사람들의 결제정보들을 담고 있어서 1000명의 유저정보만 모아둔 데이터프레임
# 필요 라이브러리 로딩
library(ggplot2) # ggplot()
library(dplyr) # 파이프연산자, summarise, names등
# 유저들의 정보를 담을 새로운 df(userInfo)
userInfo<- data.frame(user_code=0:999)
## 성별
### 성별 판별하는 함수 정의
genf<- function(i){
  rqData %>%
    filter(user_code == i-1) %>%    # 유저코드가 0부터 시작해서 
    select(gender) %>%
    summarise(sum_gen = sum(gender))
}
genf(1) # 확인
### 원본 데이터의 고객수 만큼 반복하는 작업
x<- c() # 함수 안에 담기 전에 정의를 해둬야 작업이 가능해짐.
for(i in 1:1000){
  x<- c(x,genf(i))
}
head(x) # 확인, 리스트로 값이 출력됨
newg<- unlist(x) # 벡터값으로 변환
### 데이터프레임의 변수에 추가하는 작업
userInfo$user_gender<- ifelse(newg == 0,'F','M')

## 세대
### 세대를 판별하는 함수
agef<- function(i){
  rqData %>%
    filter(user_code == i-1) %>%
    select(group)
}
agef(1) # 확인
### 고객수 만큼 반복하는 작업
age <- c()
for(i in 1:1000){
  age<- c(age,agef(i))
}
### 반복 값을 제거하는 구문
###+ 결제횟수만큼 동일한 세대값을 가지기 떄문에 한 고객당 결제횟수만큼 세대값이 생김 그걸 제거
uniq_A<- c()
for(i in 1:1000){
  uniq_A <- c(uniq_A,unique(age[[i]]))
}
## 데이터 프레임에 변수 추가
userInfo$user_AG <- uniq_A 

## 총소비액
### 소비액 판별에 사용할 함수 정의
totalf<- function(i){
  rqData %>%
    filter(user_code == i-1) %>%
    select(total) %>%
    summarise(user_t = sum(total))
}
totalf(3) # 확인
### 고객수 만큼 반복하는 구문
u_total<- c()
for(i in 0:999){
  u_total<- c(u_total, totalf(i))
}  # 리스트로 저장됨
ut<- unlist(u_total) # 벡터화
### 데이터 프레임에 추가하는 작업
userInfo$user_total<- round(ut/1000) # 액수가 커서 모두가 동일한 1000원 단위를 나눠줌

## 소비건수
### 고객당 소비한 횟수를 판별하는 함수
cntf<- function(i){
  rqData %>%
    filter(user_code == i-1) %>%
    select(total) %>%
    summarise(cnt=n())
}
cntf(2)
### 반복문
r<- c()
for(i in 1:1000){
  r<- c(r,cntf(i))
}
cnt<- unlist(r)
userInfo$user_cnt <- cnt 
