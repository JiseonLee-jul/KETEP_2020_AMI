# jbj_data

### 목적
- 프로젝트 결과 도출

### 시기
- 2021

---
### 데이터 설명
#### 1. APT
- 전국의 아파트 5개 단지의 251개 가구의 id 정보, 설문지 응답 여부 정보 등
- 수집기간 : 2017년 10월 ~ 2020년 12월(27개월)
- 타입 : csv
- 11 columns data : No, _id, address, apartment, dong_ho, listID, question, region, serial number, state

#### 2. Power_DB
- 전국의 아파트 5개 단지의 251개 가구의 1시간 단위 AMI 데이터
- 수집기간 : 2017년 10월 ~ 2020년 12월(27개월)
- 타입 : csv
- 32 columns data : 0 ~ 23, DCU, HCU, _id, buildingID, date, dong, ho, serial number

#### 3. Survey_DB
- 전국의 아파트 5개 단지의 251개 가구의 1시간 단위 AMI 데이터
- 수집기간 : 2017년 10월 ~ 2020년 12월(27개월)
- 타입 : csv
- 32 columns data : 0 ~ 23, DCU, HCU, _id, buildingID, date, dong, ho, serial number

---
### 분석 내용
#### 1. wide_to_long_sample.R
- 샘플로 설정한 2개 단지에 속한 각 가구별로 분할된 전력사용량 데이터를 하나로 병합(foreach)
- 중복 가구 제거하기
- 데이터 공백 채우기
- wide data -> long data 형태 변경

#### 2. outlier_na_cumsum.R
- 이상치 제거(z-score method)
- 선형보간을 이용한 na 처리
- cumulatvie sum function을 통한 전력사용 데이터 값 변경

#### 3. plot_outlier_pattern_sample.R
- na구간의 패턴을 보기 위해 시각화

#### 4. clustering.R
- 시간 단위 전력사용량 데이터에 Two-step clustering 적용
- 클러스터링 결과 시계열 그래프로 시각화하여 패턴 분석

#### 5. clustering_day.R
- 일 단위 전력사용량 데이터에 Two-step clustering 적용
- 클러스터링 결과 시계열 그래프로 시각화하여 패턴 분석

#### 6. survey_summary.R
- survey data 수치형 변수 단위 통일
- 설문지 항목으로부터 변수 생성
- clustering.R의 클러스터링 결과를 토대로 그룹별 설문항목 분석

#### 7. survey_summary_day.R
- survey data 수치형 변수 단위 통일
- 설문지 항목으로부터 변수 생성
- clustering_day.R의 클러스터링 결과를 토대로 그룹별 설문항목 분석

---
### Two-step clustering method
[자세한 설명은 링크를 참조하세요]()

