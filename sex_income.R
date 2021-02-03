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

# 성별 변수를 1 -> male , 2 -> female 로 바꿔주기
welfare$sex <- ifelse(welfare$sex == 1 , "male", "female")

# income 변수가 어떻게 구성되어 있는지 요약
summary(welfare$income)

# 이상치를 NA로!
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA,
                         welfare$income)
# 0이나 9999면 이상치! 
# 결측치가 얼마나 있을까?
table(is.na(welfare$income))

# 두 변수관의 관계 분석!
sex_income <- welfare %>%
  filter(!is.na(income)) %>%             # 수입이 결측치가 아닐때 
  group_by(sex) %>%                      # 성별로 묶어보자
  summarise(mean_income = mean(income))  # 수입의 평균을 요약

sex_income

# 그래프로 분석!
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()