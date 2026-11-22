# code/ 안내

분석 코드가 모두 들어 있는 폴더입니다. 파일이 쌓인 순서대로 번호가 붙어 있어서, 번호만 봐서는 무엇이 최신인지 알기 어렵습니다. 이 안내에서는 파일을 **역할**에 따라 묶었습니다.

작성: 2026-11-18 · team06donga. 생성형 AI(Claude)로 각 파일의 머리 주석과 출력 경로를 읽어 정리했습니다. 파일은 옮기거나 지우지 않았습니다.

## 먼저 볼 것

| 파일 | 하는 일 | 결과 폴더 |
|---|---|---|
| **`final_analysis.R`** | **최종 분석 코드(임시본).** `분석계획서.md`의 규칙을 그대로 따른다. 파일 하나로 자료 통합부터 표 저장까지 끝난다. 주분석은 음주 기준이 확정될 때까지 꺼 두었다(`RUN_MAIN <- FALSE`) | `output/final_analysis/` |
| `../.claude/skills/knhanes_ptest/scripts/ptest.R` | `/knhanes_ptest` 스킬. 음주 기준·보정 묶음을 바꿔 가며 주질문·부질문의 교호작용 p를 빠르게 구한다 | `output/21_ptest/` |

실행 방법은 모두 프로젝트 폴더에서 `Rscript --vanilla code/파일이름.R`입니다. 원시자료(`data/`)는 저장소에 없으므로 각자 PC에 두어야 합니다.

## 역할별 목록

상태 표시: **최종** = 지금 기준 · **도구** = 다른 결과를 정리·변환함 · **점검** = 결정을 돕는 확인용 · **기록** = 이전 설계의 분석, 참고로만 봄 · **다른 주제** = 이 과제와 관계없음

### 1. 자료 읽기

| 파일 | 상태 | 하는 일 | 결과 폴더 |
|---|---|---|---|
| `01_read_basic_db.R` | 기록 | 기본DB(`hnXX_all`)를 읽어 연도별 행·변수 수를 확인 | `output/01_basic_db_overview/` |

### 2. 다른 주제: 식사–취침 간격 (이 과제와 관계없음)

| 파일 | 상태 | 하는 일 | 결과 폴더 |
|---|---|---|---|
| `02_meal_bedtime_cardiometabolic.R` | 다른 주제 | 청년층 마지막 식사–취침 간격과 심혈관대사 위험 | `output/02_meal_bedtime/` |
| `07_meal_bedtime_figures.R`, `08_meal_bedtime_report.R` | 다른 주제 | 위 결과의 그림·보고서 | 같은 폴더 |

### 3. 1차 분석: 현재 홍조 2군 (연구 질문 개정 전)

| 파일 | 상태 | 하는 일 | 결과 폴더 |
|---|---|---|---|
| `03_flushing_hyperuricemia.R` | 기록 | 처음 설계(현재 홍조 2군, 남녀 공통 기준)의 분석 | `output/03_flushing_hyperuricemia/` |
| `04_flushing_figures.R`, `05_flushing_report.R`, `06_flushing_report_docx.py` | 기록 | 위 결과의 그림, 보고서(md), Word 변환 | 같은 폴더 |

### 4. V3 설계: 홍조 이력 3군, 대상자 정의

| 파일 | 상태 | 하는 일 | 결과 폴더 |
|---|---|---|---|
| `09_part2_descriptives.R`, `10_part2_prep_figures.R`, `11_cutoff_options.R` | 기록 | 연구보고서 V3 2부(대상·음주량 정의) 집계와 그림, 남녀 공통 절단점 후보 비교 | `output/09_part2_prep/` |
| `12_v3_summary_figures.R` | 기록 | V3 개념도 | `output/10_v3_summary/` |
| **`13_v3_main_analysis.R`** | 기록(일부 최종) | V3 본 분석. **0–3절(변수·대상·설계)은 `ptest`가 그대로 불러 쓴다.** 그래서 이 부분을 고치면 ptest 결과도 바뀐다 | `output/13_v3_main_analysis/` |
| `14_v3_results_figures.R`, `15_v3_results_report.R` | 기록 | V3 결과 그림·보고서 | 같은 폴더 |
| `16_report_md_to_pdf.py` | 도구 | Markdown 보고서 → PDF(Edge 헤드리스) | — |

### 5. 음주 절단점 탐색 (11/14–11/16)

| 파일 | 상태 | 하는 일 | 결과 폴더 |
|---|---|---|---|
| `17_cutpoint_exploration_subq1.R`, `18_cutpoint_5_15_30.R` | 기록 | 절단점을 격자로 바꿔 보거나 5·15·30 g/일로 분석 | `output/15_cutpoint_exploration/` |
| `19_continuous_main.R`, `20_continuous_subq.R` | 기록 | 음주량·혈청 요산을 연속형으로 둔 분석 | `output/16_continuous_main/` |
| `21_cutpoint_5_15_main_sub.R` | 기록 | 5·15잔 기준 주질문·부질문 | `output/17_cutpoint_5_15/` |
| `22_main_5_15_precise.R`, `23_report_5_15.R` | 기록 | 5·15잔 기준 정밀 분석과 보고서 | `output/18_main_5_15/` |
| `24_tertile_cutpoints.R` | 기록 | 성별 가중 삼분위 절단점 찾기(계획서 후보 S1의 출처) | `output/19_tertile_cutpoints/` |
| `25_pvalues_by_criteria.R` | 기록 | 기준을 넣으면 질문별 p를 구하는 첫 도구(ptest의 전신) | `output/20_pvalues_by_criteria/` |

### 6. ptest 결과 정리 도구

| 파일 | 상태 | 하는 일 | 결과 |
|---|---|---|---|
| `26_ptest_summary_G.R` | 도구 | G01–G16의 질문별 p 요약 | `output/21_ptest/G_요약_질문별p.*` → `Result/2026-11-16_음주기준_전체_질문별p.*` |
| `28_ptest_run_catalog.R` | 도구 | 실행 46회의 조건 목록 | `Result/2026-11-18_ptest_실행목록.*` |
| `29_ptest_full_table.R` | 도구 | 실행 × 결과 × 보정 묶음 248줄 표 | `Result/2026-11-18_ptest_전체표_조건과p.*` |
| `32_ptest_table_xlsx.py` | 도구 | 위 표의 엑셀본(시트 7개) | `Result/2026-11-18_ptest_전체표_조건과p.xlsx` |
| `34_trend_vs_categorical.py` | 도구 | 주질문·부질문 1의 범주별 검정 vs 경향성 검정 p 비교(기준 11개) | `output/21_ptest/경향성비교_주질문_부질문1.*` |
| `35_subq23_all_criteria.py` | 도구 | 부질문 2·3(경향성)의 p를 기준 16개 × 보정 묶음 6개 × 결과 2개로 정리 | `output/21_ptest/부질문2_3_전체기준.*` |

### 7. 결정을 돕는 점검

| 파일 | 상태 | 하는 일 | 결과 |
|---|---|---|---|
| `C06_sex_specific_binary_cutoffs.R` | 점검 | 남녀별 2단계 기준 후보의 칸 인원(화면에 출력만 함) | — |
| `C07_meno_covariate_check.R` | 점검 | 폐경 상태를 보정할 때 빠지는 여성 수(화면에 출력만 함) | `Result/2026-11-16_보정변수_설정안_검증.md`에 인용 |
| `C08_절단점_경계_점검.R` | 점검 | 주당 잔 수가 실제로 가질 수 있는 값, 폭음 문항 교차표 | `output/C08_cutpoint_boundaries/` → `Result/2026-11-18_폭음문항_사용여부.md` |
| `C09_여성_칸_인원_점검.R` | 점검 | 후보 기준별 여자 × 홍조 3군 × 음주 수준 칸 인원 | `output/C09_women_cell_check/` → `Result/2026-11-18_여성_칸_인원_점검.md` |
| `30_repr_value_bounds_check.R` | 점검 | 음주 대표값을 범주 하한·상한으로 바꿀 때의 분포와 재분류 | `output/22_repr_sensitivity/` → `Result/2026-11-18_음주대표값_민감도분석_계획.md` |

### 8. 문서 만들기 도구

| 파일 | 하는 일 | 결과 |
|---|---|---|
| `25_adjusted_or_table_pdf.ps1` | V3 보정 결과표 PDF | `Result/2026-11-16_보정변수_보정결과표(V3_이전기준).pdf` |
| `28_shell_tables.ps1` | 결과표 빈 틀(표 0·1·2·3) PDF·엑셀 | `Result/2026-11-18_결과표_빈틀.*` |
| `31_reference_overview.R` | 논문 요약 100편 한눈 표 | `Result/2026-11-18_논문별_요약_한눈표.md` |
| `33_repo_overview_pdf.py` | 저장소 전체 정리 PDF(11/18) | `Result/2026-11-18_저장소_전체_정리.pdf` |
| `36_activity_report_pdf.py` | 활동 전체 정리 통합 PDF(11/22, Git 기록·결과 CSV·README·분석계획서·점검 결과를 읽어 만듦) | `Result/2026-11-22_6조_활동_전체정리.pdf` |
| `37_meeting_agenda_pdf.py` | 다음 회의 자료 PDF(결정 사항·할 일·기록지) | `Result/2026-11-22_다음회의_결정사항과_할일.pdf` |

## 알아 둘 점

- **번호가 겹칩니다:** `25_`와 `28_`가 각각 두 파일입니다. 새 파일은 `33_`부터 쓰는 것이 좋습니다.
- **분석 규칙은 코드가 아니라 `분석계획서.md`가 기준입니다.** 규칙을 바꾸려면 계획서를 먼저 고치고, 그다음 `final_analysis.R`을 고칩니다.
- **`13_v3_main_analysis.R` 0–3절은 고칠 때 조심해야 합니다.** ptest가 이 부분을 그대로 불러 쓰므로, 고치면 ptest 결과도 바뀝니다.
- **문서 점검 반영:** 코드 관련 항목(경향성 검정의 † 표시, 효과표 비고, 칸 표의 가중 유병률 등)은 2026-11-18에 고쳤습니다(`Result/2026-11-18_문서점검_결과.md` 3·4·18·19·27번).
