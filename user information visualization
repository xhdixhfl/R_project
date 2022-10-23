library(ggplot2) # 필요한 라이브러리 로딩

# 카드결제 데이터 내 성별 분포 
barplot(table(userInfo$user_gender), main='결제데이터 내 성별분포',
        font.main = 4, cex.main = 2,col = 'green',names=c('여성','남성'),
        xlab = '성별의 분포', ylab = '개수', ylim=c(0,600), las=1, space = 0.5,
        width = c(6,5), cex.lab = 1.5, font.lab = 1) 

# 고객들의 세대비율
pie3D(table(userInfo$user_AG),labels = c('MZ \n 57.4%','OPAL \n 42.4%','Silver \n 0.02%'),
    labelcex = 1.2, explode = 0.05, col = rainbow(3),theta=pi/4,
    shade=0.8, main= '이용고객의 세대비율')
table(userInfo$user_AG)

# 세대별 소비건수
## 박스플롯
### 소비건수를 불러오는 작업
a<- userInfo %>% filter(user_AG=='MZ') %>% select(user_cnt)
b<- userInfo %>% filter(user_AG=='OPAL') %>% select(user_cnt)
c<- userInfo %>% filter(user_AG=='Silver') %>% select(user_cnt)
### 시각화
#1) 전체
boxplot(a,b,c, col=rainbow(3), main= '세대별 소비건수', ylab='건수', ylim=c(200,2500), las=1)
#2) 파트별
boxplot(a,col='cyan', xlab= 'MZ 소비건수', ylab='건수', ylim=c(200,2500), las=1, cex.lab =2)
boxplot(b,col='magenta', xlab= 'OPAL 소비건수', ylab='건수', ylim=c(200,2500), las=1, cex.lab =2)
boxplot(c,col='green', xlab= 'Silver 소비건수', ylab='건수', ylim=c(200,2500), las=1, cex.lab =2)
#3) 실제 사용한 박스플롯
boxplot(a,col='cyan', xlab= 'MZ 소비건수', ylab='건수', ylim=c(450,1450), las=1, cex.lab =2) 
boxplot(b,col='magenta', xlab= 'OPAL 소비건수', ylab='건수', ylim=c(450,1450), las=1, cex.lab =2)
