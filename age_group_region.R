install.packages("foreign")     # foreign 패키지 설치

library(foreign)                # SPSS 파일 불러오기
library(dplyr)                  # 전처리
library(ggplot2)                # 시각화
library(readxl)                 # 엑셀 파일 불러오기

# 데이터 불러오기
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T) 
# 'to.data.frame = T' 코드는 SPSS 파일을 데이터 프레임으로 변환!

# 복사본 만들기
welfare <- raw_welfare

# 분석에 사용하기 편하게 변수명 바꾸기
welfare <- rename(welfare,
                  sex = h10_g3,            # 성별
                  birth = h10_g4,          # 출생 년도
                  marriage = h10_g10,      # 혼인 상태
                  religion = h10_g11,      # 종교
                  income = p1002_8aq1,     # 월급
                  code_job = h10_eco9,     # 직업 코드
                  code_region = h10_reg7)  # 지역 코드

# 'birth'를 통해 'age' 파생 변수 만들기
welfare$age <- 2015 - welfare$birth + 1   # 2015년에 조사 진행

# 'age'를 통해 'age_group' 파생 변수 만들기
welfare <- welfare %>% 
  mutate(age_group = ifelse(age<30, "young",
                            ifelse(age<60, "middle", "old")))
# 지역 변수 전처리
# 지역 코드 목록 만들기
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))

# 지역명 변수 추가 -> 두 df 합치기
welfare <- left_join(welfare, list_region, id = "code_region")

# 지역별 연령대 비율표 만들기
region_age_group <- welfare %>%
  group_by(region,age_group) %>%
  summarise(n = n()) %>%
  mutate(sum_group = sum(n)) %>%
  mutate(ratio = round(n/sum_group*100,2))

# 노년층 비율 내림차순 정렬
list_order_old <- region_age_group %>%
  filter(age_group == "old") %>%
  arrange(ratio)

# 지역명 순서 변수 만들기
order <- list_order_old$region

# level 지정
region_age_group$age_group <- factor(region_age_group$age_group,
                                     level = c("old","middle","young"))
# 쉽게 말해서 old -> middle -> young 순으로 정렬
levels(region_age_group$region)

# 그래프로 표현
ggplot(data = region_age_group, aes(x = region, y = ratio, fill = age_group)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)