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

# 직업 변수 전처리
# codebook에서 직업 코드 가져오기
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T,sheet=2)

# left_join 함수를 통해 list_job과 합치기
welfare <- left_join(welfare, list_job, id = "code_job")

# 직업별 월급 구하기
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>% # job, income 결측치 제거
  group_by(job) %>%                        # 직업별로 묶음
  summarise(mean_income = mean(income))    # 평균 월급 구하기

# 월급 상위 10개 직업 추출
top10 <- job_income %>%
  arrange(desc(mean_income)) %>%    # 평균 월급 기준 내림차순 정렬
  head(10)                          # 상위 10개의 데이터만 추출

top10

ggplot(data = top10, aes(x = mean_income, y = reorder(job, mean_income)))+
  geom_col()
