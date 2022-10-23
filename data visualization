library(plotrix) # 3D원그래프
# 세대별 주요소비 탑 10 시각화
## 다른 팝업창에서 차트를 보기위한 작업
windows()
par(mfrow=c(1,3), fig=c(0,0,0,0)) 

## MZ
### group, l_cate상관 관계 확인; 
MZ_top10<- rqData %>%
  filter(group == 'MZ') %>%
  group_by(l_cate) %>%
  summarise(cnt =n()) %>%
  arrange(desc(cnt)) %>%
  head(10)
MZ_top10
mz_top10<- as.data.frame(MZ_top10) # 나오는 데이터형을 보고 변화 필요하면 as함수 사용하기
mz_top10$nn <- round(MZ_top10$cnt/sum(MZ_top10$cnt)*100,2) # 건수 범위가 너무 커서 축약

### 시각화
pie3D(mz_top10$nn, labels = c('생활/마트\n26.47%','온라인쇼핑\n20.47%',
                              '식사\n17.15%','카페/간식\n11.15%',
                              '교통/차량\n9.00%','의료/건강\n5.31%',
                            '백화점/패션\n3.68%','주거/통신\n2.57%',
                            '문화/예술\n2.36%','금융/보험\n1.79%'),
      labelcex = 0.9, cex.main = 1.7,
      main = 'MZ세대의 top10 소비 카테고리(%)', col = rainbow(10),theta=pi/4, shade=0.8)
      
 ## OPAL
 ### group, l_cate상관 관계
 OP_top10<- rqData %>%
  filter(group == 'OPAL') %>%
  group_by(l_cate) %>%
  summarise(cnt =n()) %>%
  arrange(desc(cnt)) %>%
  head(10)
OP_top10
op_top<- as.data.frame(OP_top10) 
op_top$nn <- round(op_top$cnt/sum(op_top$cnt)*100,2)
### 시각화
pie3D(op_top$nn, labels = c('생활/마트\n28.76%','온라인쇼핑\n17.66%','식사\n16.56%',
                            '교통/차량\n9.20%','카페/간식\n9.18%','의료/건강\n6.69%',
                          '백화점/패션\n3.91%','주거/통신\n3.46%','금융/보험\n2.75%',
                          '문화/예술\n1.77%'), labelcex = 0.9, cex.main = 1.7,
    main = 'OPAL세대의 top10 소비 카테고리(%)', col = rainbow(10),theta=pi/4, shade=0.8)

## Silver
### group, l_cate상관 관계 
SV_top10<- rqData %>%
  filter(group == 'Silver') %>%
  group_by(l_cate) %>%
  summarise(cnt =n()) %>%
  arrange(desc(cnt)) %>%
  head(10)
SV_top10
sv_top10<- as.data.frame(SV_top10)
sv_top10$nn <- round(SV_top10$cnt/sum(SV_top10$cnt)*100, 2)
### 시각화
pie3D(sv_top10$nn, labels = c('생활/마트\n28.97%','온라인쇼핑\n15.74%','식사\n14.45%',
                              '의료/건강\n12.70%','교통/차량\n10.42%','카페/간식\n5.95%',
                              '주거/통신\n4.47%','백화점/패션\n3.31%','금융/보험\n2.91%',
                              '스포츠/레저\n1.04%'), labelcex = 0.9, cex.main = 1.7,
      main = 'Silver세대의 top10 소비 카테고리(%)', col = rainbow(10),theta=pi/4, shade = 0.8)
