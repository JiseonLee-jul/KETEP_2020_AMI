# jbj_data

### 목적
- 전력 데이터 특징 파악을 위한 사전분석, 개인 논문 연구 목적

### 시기
- 2020 ~ 2021

---
### 데이터 설명
- 전국의 아파트 32개 단지의 19,442개 가구의 15분 단위 누적 AMI 데이터
- 수집기간 : 2019년 9월 ~ 2020년 8월(12개월)
- 타입 : csv
- 5 columns data(long data) : House, Date, Hour, KW, V5

---
### 분석 내용
#### 1. binding_folders.R
- 각 단지별로 따로 저장된 데이터를 하나의 list와 dataframe으로 변환
- 병렬처리를 위해 foreach 사용(doParallel)

#### 2. norm_outlier
- 데이터 정규화(Yeo-Johnson transforamtion)
- 이상치 제거(z-score method)

#### 3. dscrb_data.R
- 데이터 기초분석

#### 4. elec_fee_ftn.R
- 시계열 변수 생성
- 3가지 요금제 계산 함수 생성
- 시계열, 요금 변수 포함한 dataframe 생성

#### 5. graph_tmsris_smpl.R
- 예시 데이터 샘플링
- 5개 종류의 시계열 그래프 생성 후 전력 사용 패턴 파악

---
### Two-step clustering
#### 목적
- 주택용 전력 사용 가구의 전력사용량을 변수로 설정하여 가구를 분류
- 일반적인 clsutering 방법을 적용 시 총 전력사용량만을 기준으로 가구가 분류되는 문제 해결
<img src="https://user-images.githubusercontent.com/102127170/163716650-a5636d54-feac-47e1-84e7-f70390ec6cf2.png" width="700" height="370">

#### 방법
1. first step
- cumulative sum function을 통해 전력사용량 데이터를 누적 전력사용량 데이터로 변환
- clustering 적용하여 총 전력사용량을 기준으로 가구를 분류

![](https://user-images.githubusercontent.com/102127170/163718248-e2b93f55-583d-4a1c-ad52-960db14ac382.png)

2. second step
- min-max scailing을 통해 전력사용량 정보를 최소화
- clustering 적용하여 전력사용 패턴을 기준으로 가구를 분류

![](https://user-images.githubusercontent.com/102127170/163718266-825d9757-1228-4118-9f7f-76a8728cff70.png)
