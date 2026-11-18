# Bae 등 (2023). Associations between obesity parameters and hyperuricemia by sex, age, and diabetes mellitus: A nationwide study in Korea

- 서지: Bae J, Park KY, Son S, Huh Y, Nam GE. Associations between obesity parameters and hyperuricemia by sex, age, and diabetes mellitus: A nationwide study in Korea. Obes Res Clin Pract. 2023;17(5):405-410.
- DOI: https://doi.org/10.1016/j.orcp.2023.09.007
- PubMed: https://pubmed.ncbi.nlm.nih.gov/37739857/
- 원문 파일: 2023, Bae, [보정변수-BMI] 비만 지표와 고요산혈증(KNHANES).md (`full text/`에 있으나 원문이 아니라 초록 기반 요약이다. 원문 PDF는 없음)
- 요약 근거: 초록(`full text/`의 .md에 옮긴 초록 내용). 원문 전문은 읽지 못함
- 쓰임: 보정변수 근거 (체질량지수), 고요산혈증 기준(남 ≥7.0, 여 ≥6.0 mg/dL)의 근거

## 연구 목적

한국 성인에서 비만 지표와 고요산혈증의 연관을 성별, 연령, 당뇨병 유무에 따라 보고자 했다.

## 대상과 자료

2016–2018년 국민건강영양조사 성인 17,753명이다.

## 방법

BMI, 허리둘레, 전신비만, 복부비만과 고요산혈증(남성 ≥7.0, 여성 ≥6.0 mg/dL)의 연관을 다변량 로지스틱 회귀로 보았다. 비만 기준값, 보정변수, 복합표본설계 반영 여부는 원문에 없음(초록에 없음).

## 주요 결과

- 고요산혈증 12.7%(2,256명).
- BMI·허리둘레가 클수록 연령 보정 평균 요산과 고요산혈증 OR이 커졌다(추세 P < 0.001).
- 전신비만 OR: 남성 1.98(95% CI 1.69–2.32), 여성 3.26(95% CI 2.60–4.09).
- 복부비만 OR: 남성 2.21, 여성 3.43(95% CI는 원문에 없음).
- 연관은 여성에서 더 뚜렷했다.

## 저자가 밝힌 한계

원문에 없음(초록에 명시되지 않음).

## 보정변수 근거로서의 쓰임

- BMI가 남녀 모두에서 고요산혈증과 강하게, 단계적으로 관련된다는 근거다. 우리 결과 정의(남 ≥7.0, 여 ≥6.0 mg/dL)의 근거이기도 하다.
- 추세가 단계적이므로 `HE_BMI` 연속형으로 넣는 계획이 맞다.
- 음주는 열량을 통해 BMI를 높일 수 있어, BMI는 음주→요산의 매개변수일 수 있다. 그래서 주 모형(M2)이 아닌 M3 민감도 분석에 넣는다.
- 여성에서 연관이 더 커서, 여성 모형에서 M2와 M3의 차이가 더 클 수 있다.
