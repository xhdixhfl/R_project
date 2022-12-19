#1. 라인그래프를 위한 데이터 정제
## MZ 데이터 정리
line_mz<- data.frame(a)
a<- rqData %>%
  filter(group == 'MZ') %>%
  group_by(l_cate) %>%
  summarise(cnt =n())

line_mz$pct<- round(line_mz$cnt/sum(line_mz$cnt)*100,2)
## Opal 세대
b<- rqData %>%
  filter(group == 'OPAL') %>%
  group_by(l_cate) %>%
  summarise(cnt =n())
line_op<-data.frame(b)

line_op$pct <- round(line_op$cnt/sum(line_op$cnt)*100,2)

line_op
## 실버
c<- rqData %>%
  filter(group == 'Silver') %>%
  group_by(l_cate) %>%
  summarise(cnt =n())
line_sv <- data.frame(c)

line_sv$pct <- round(line_sv$cnt/sum(line_sv$cnt)*100,2)

line_sv
## 전체 카테고리 수
table(rqData$l_cate) # 16개

#2. 각세대별 카테고리 소비율(%) 비교
## 그래프 그리기
plot(line_mz$pct, type = 'o',cex=2, pch = 16, col='cyan', ylim=c(1,30), xlab = '소비 카테고리',
cex.lab=1.5,ylab = '비율(%)',axes = F) # 기본 틀과 첫번째 라인 그래프
axis(1, at=1:16, labels = line_mz$l_cate) # x축 옵션
axis(2, at=1:30, las=1) # y축 옵션
lines(line_sv$pct, type = 'o', cex=1, pch = 15, col='grey')
lines(line_op$pct, type='o', cex=2, pch = 17, col='magenta')
legend(15,30, colnames(lineH),cex = 0.8, pch = c(16,17,15), col= c('cyan','magenta','grey'))
##+ 범례추가
lineH<- line_mz[,c(3)]
class(lineH) # 자료형 확인
lineH<-cbind(MZ=lineH, OPAL=line_op[,c(3)], Silver=line_sv[,c(3)])

#3. MZ, OPAL세대의 소비 건수 비교
plot(line_mz$cnt, type = 'o', cex=2, pch = 16, col='cyan', xlab = '소비 카테고리',
cex.lab=1.5,ylab = '건수',axes = F)
axis(1, at=1:16, labels = line_mz$l_cate)
axis(2, ylim=c(min=4000,max=3100000))
lines(line_op$cnt, type='o', cex=2, pch = 17, col='magenta')
legend(15,3400000, colnames(lineH)[-3],cex = 0.8, pch = c(16,17),
       col= c('cyan','magenta'))
