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

# 종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, "YES", "NO")

# 혼인 상태 전처리 -> 이혼 했는지 안했는지만 알려주는 변수 생성
welfare$divorce <- ifelse(welfare$marriage == 1, "Marriage",
                         ifelse(welfare$marriage == 3, "Divorce", NA))

# 종교별 이혼율 변수 생성
religion_divorce <- welfare %>%
  filter(!is.na(divorce)) %>%        # 이혼 변수 결측치 제거
  group_by(religion, divorce) %>%    # 종교와 이혼 여부로 그룹화
  summarise(n = n()) %>%             # 몇 명인지로 요약
  mutate(sum_group = sum(n)) %>%     # 각 그룹이 몇명인지 계산
  mutate(divorce_ratio = round(n/sum_group*100,1)) # 이혼율 구하기

# 'birth'를 통해 'age' 파생 변수 만들기
welfare$age <- 2015 - welfare$birth + 1   # 2015년에 조사 진행

# 'age'를 통해 'age_group' 파생 변수 만들기
welfare <- welfare %>% 
  mutate(age_group = ifelse(age<30, "young",
                            ifelse(age<60, "middle", "old")))

# 연령대, 종교, 결혼 상태 비율표 만들기
age_religion_marriage <- welfare %>%
  filter(!is.na(divorce) & age_group != "young") %>%
  group_by(age_group, religion, divorce) %>%
  summarise(n = n()) %>%
  mutate(sum_group = sum(n)) %>%
  mutate(ratio = round(n/sum_group*100,1 ))

age_religion_marriage

# 연령대 및 종교 유무별 이혼율 표 만들기
df_divorce <- age_religion_marriage %>%
  filter(divorce == "Divorce") %>%       # 이혼에 관한 정보만 뽑음
  select(age_group, religion, ratio)      # 연령대, 종교, 비율만 뽑음

# 그래프로 표현
ggplot(data = df_divorce, aes(x=age_group, y=ratio, fill=religion)) +
  geom_col(position = "dodge")   # 막대 두개로 분리
