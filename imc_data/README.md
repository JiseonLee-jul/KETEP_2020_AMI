# imcdata

### 목적
- 전력 데이터 특성 파악을 위한 사전분석

---
### 데이터 설명
- 전국의 아파트 206개 단지의 127,839개 가구의 1시간 단위 AMI 데이터
- 수집기간 : 2016년 9월 ~ 2019년 12월(75개월)
- 타입 : csv
- 6 columns data(long data) : DB_code, Apt, House, Date, Hour, KW

---
### 분석 내용
#### 1. gthr_n_smpl_data.R
- 각 단지별로 따로 저장된 데이터를 하나의 list로 변환
- 예시 데이터로 사용할 100개의 가구를 샘플링하여 dataframe으로 저장

#### 2. add_variables.R
- seq.POSIXt를 사용하여 추후 분석에 사용할 시계열 변수 추가
- 불필요한 변수(ex. DB code) 제거
- 중복 데이터 제거

#### 3. outlier_qunatile.R
- quantile method를 사용한 이상치 탐지

#### 4. tmsris_plots.R
- 7가지 타입의 전력사용량 시계열 plot 생성
