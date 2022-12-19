library(ggplot2)
library(dplry)
# 시각화 창 뛰우기
windows()
par(mfrow(2,2)

#1. 세대별 전체 카테고리 건수 비교
## 데이터 정제
ll<- rqData %>% group_by(group,l_cate) %>% summarise(N=n())
## 시각화
ggplot(ll, aes(x='', y= N, fill=l_cate)) + facet_grid(facets = .~group) +
  geom_bar(stat = 'identity', width = 1) + coord_polar(theta = 'y')
  
#2. 주요세대 더 자세히 건수 비교(파이차트 로즈다이어그램 형식)
## MZ
ggplot(line_mz, aes(x=l_cate,y=pct)) + geom_col(aes(fill=l_cate), width = 0.92)+
    coord_polar() + theme_minimal(12) + labs(title = 'MZ세대 소비비율') +
  theme(legend.position = 'right', text = element_text(family = 'Courier'),
        axis.text.x = element_text(size=13, hjust = 0.1, face = 'bold'),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank())
        
## OPAL
ggplot(line_op, aes(x=l_cate,y=pct)) + geom_col(aes(fill=l_cate), width = 0.92)+
  coord_polar() + theme_minimal(12) + labs(title = 'OPAL세대 소비비율') +
  theme(legend.position = 'right', text = element_text(family = 'Courier'),
        axis.text.x = element_text(size=13, hjust = 0.1, face = 'bold'),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank())
