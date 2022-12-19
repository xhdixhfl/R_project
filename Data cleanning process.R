# 결제 데이터 불러오기(csv파일 불러오기)
rqData<- read.csv('req_data.csv')
str(rqData) # 구조 확인하는 구문
View(rqData) # 새로운창으로 해당 파일을 열어서 보여주는 구문

# 필요한 데이터만 뽑은 DF
rqData<- rqData[,-c(1,9)] # 행은 전부를 열은 1,9번째 속성만 제외한 모든 열
head(rqData) # 해당 데이터프레임의 처음부터 6번째 값을 보여주는 구문

# 변수 변경 rename(new=old)
rqData <- rename(rqData, user_code = person_, age_G = age_group, gender = person_gender,
                 p_date = pay_date, p_day = pay_day_of_week, p_time = pay_hour_group,
                 total = amount, l_cate = large_category, m_cate = mid_category)
names(rqData) # 변수명을 불러오는 함수

# 결제 데이터내 세대그룹 차지 비율
### 시각화
class(rqData$group) 
table(rqData$group) ->gg# 문자형이여서 table로 빈도수 체크
names(gg)
pie3D(gg, main = '결제데이터 내 세대별 차지 비율', 
      labels = c('MZ \n 54.68%','OPAL \n 45.06%','Silver \n 0.25%'),
      labelcex = 1.2, explode = 0.05, col = rainbow(3),theta=pi/4,
      shade=0.8)

for(i in 1:3){
 gG<- round(gg[i]/sum(gg)*100,2)
 print(gG)
}   # 파이 수치 계산

# 데이터 전처리
## 데이터가 어떤 자료형으로 이루어졌는지와 결측값의 유무등을 알아보는 과정
### 나이
summary(rqData$age_G)
table(is.na(rqData$age_G)) # 결측값 없음

### category 분야
summary(rqData$l_cate)
table(is.na(rqData$l_cate)) # 결측값 없음
rqData$l_cate <- as.factor(rqData$l_cate) # 범주형으로 변경
head(rqData$l_cate)

summary(rqData$m_cate)
table(is.na(rqData$m_cate)) # 결측값 없음
rqData$m_cate <- as.factor(rqData$m_cate) # 범주형으로 변경
head(rqData$m_cate)

# 필요한 변수 만들어서 추가(group)
#+ OPAL세대: 40~64, MZ세대: 20~39, Silver세대: 65<=
## OPAL세대가 주요 분석대상이여서 고객의 나이 속성을 이용하여 세대그룹을 나눠주기
rqData$group<- ifelse(rqData$age_G<40,'MZ',
                      ifelse(rqData$age_G<65,'OPAL','Silver'))
head(rqData$group) # 확인 
