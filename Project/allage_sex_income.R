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
# 나이 변수 전처리
# 이상치 확인
summary(welfare$birth)

# 결측치 확인
table(is.na(welfare$birth))

# 'birth'를 통해 'age' 파생 변수 만들기
welfare$age <- 2015 - welfare$birth + 1   # 2015년에 조사 진행

# 'age'를 통해 'age_group' 파생 변수 만들기
welfare <- welfare %>% 
  mutate(age_group = ifelse(age<30, "young",
                            ifelse(age<60, "middle", "old")))

# 남성은 male로, 여성은 female로 변환
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")

# 성별에 따른 각 나이별 월급 차이 분석
allage_sex_income <- welfare %>%            
  filter(!is.na(income)) %>%              # 결측치 제거
  group_by(age, sex) %>%                  # 각 성별의 나이별 월급
  summarise(mean_income = mean(income))   # 평균 월급

# 그래프로 만들기
ggplot(data = allage_sex_income, aes(x = age, y = mean_income, 
                                     col = sex)) +
  geom_line()
  

