# 08_meal_bedtime_report.R
# 목적: 02_meal_bedtime_cardiometabolic.R 의 결과(CSV)와 07_meal_bedtime_figures.R 의 그림으로
#       분석 보고서(Markdown)를 만든다. 숫자는 모두 결과 CSV에서 읽어 넣는다(손으로 옮기지 않는다).
# 실행: Rscript --vanilla code/08_meal_bedtime_report.R   (프로젝트 폴더에서)
# 결과: output/02_meal_bedtime/REPORT_식사취침간격_대사위험.md
#       (Word 변환: python code/06_flushing_report_docx.py output/02_meal_bedtime/REPORT_식사취침간격_대사위험.md)

suppressPackageStartupMessages(library(data.table))

in_dir <- file.path("output", "02_meal_bedtime")
out_md <- file.path(in_dir, "REPORT_식사취침간격_대사위험.md")
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")

esc <- function(x) gsub("\\|", "\\\\|", trimws(as.character(x)))
md_table <- function(dt, headers = names(dt)) {
  dt <- as.data.table(dt)[, lapply(.SD, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })]
  c(paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|"),
    apply(dt, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")), "")
}
# 신뢰구간에 음수가 있으면 줄표(–)가 부호(-)와 겹쳐 읽기 어려우므로 쉼표로 구분한다
fmt_ci <- function(e, l, h) ifelse(is.na(l), "기준", ifelse(l < 0 | h < 0, sprintf("%.2f (%.2f, %.2f)", e, l, h), sprintf("%.2f (%.2f–%.2f)", e, l, h)))
fmt_p  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
big    <- function(x) format(x, big.mark = ",", trim = TRUE)

# ---- 결과 읽기 ----
vd   <- rd("01_variable_definitions.csv")
flow <- rd("02_participant_flow.csv")
ex   <- rd("03_exposure_distribution.csv")
q    <- rd("03_exposure_quantiles_weighted.csv")
t1   <- rd("04_table1.csv")
oc   <- rd("05_official_check.csv")
m    <- rd("06_main_results.csv")
s    <- rd("07_sensitivity.csv")
inputs <- rd("_inputs.csv")
repro  <- if (file.exists(file.path(in_dir, "_reproducibility.txt"))) readLines(file.path(in_dir, "_reproducibility.txt"), encoding = "UTF-8") else character(0)
repro_res  <- sub("^result:\\s*", "", grep("^result:", repro, value = TRUE))
repro_time <- sub("^time:\\s*", "", grep("^time:", repro, value = TRUE))
sess <- readLines(file.path(in_dir, "_sessionInfo.txt"), encoding = "UTF-8", n = 1)

lv_int <- c("<1시간", "1-<2시간", "2-<3시간", "3시간 이상")
n_final <- flow[.N, n]
ex[, level := factor(level, lv_int)]; setorder(ex, level)
pct_of <- function(l) ex[level == l, weighted_pct]
m2 <- m[grepl("^모형2", model)]
g2 <- function(o, l) m2[outcome == o & level == l]
gs <- function(key, o) s[grepl(paste0("^", key), analysis) & outcome == o & level == "<1시간"]
ci_txt <- function(r) fmt_ci(r$est, r$ci_low, r$ci_high)

# ---- 본문 조립 ----
L <- c()
add <- function(...) L <<- c(L, ...)

add("# 청년층(19–39세)의 마지막 식사–취침 간격과 심혈관대사 위험 요인", "",
    "각자의 평소 취침 시각을 기준으로 잰 '마지막 식사–취침 간격'과 공복혈당·중성지방·허리둘레·대사증후군의 관련성 — 국민건강영양조사 2016–2018, 2021, 2024", "",
    sprintf("- 보고서 생성: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "- 분석 코드: `code/02_meal_bedtime_cardiometabolic.R` (본 분석), `code/07_meal_bedtime_figures.R` (그림), `code/08_meal_bedtime_report.R` (이 보고서)",
    sprintf("- 분석 환경: %s, survey 패키지(복합표본)", sess),
    sprintf("- 재현성 확인: **%s** (%s, 새 R 세션에서 2회 실행 후 결과 CSV 비교)", ifelse(length(repro_res), repro_res, "미실행"), ifelse(length(repro_time), repro_time, "")),
    "")

# 요약
w_glu <- g2("공복혈당(mg/dL)", "<1시간"); w_tg <- g2("중성지방", "<1시간"); w_wc1 <- g2("허리둘레(cm)", "<1시간"); w_wc2 <- g2("허리둘레(cm)", "1-<2시간"); w_ms <- g2("대사증후군", "<1시간")
wc_s2 <- gs("S2", "허리둘레(cm)"); wc_s5 <- gs("S5", "허리둘레(cm)")
add("## 요약", "",
    sprintf("- 분석 대상은 2016–2018·2021·2024년 만 19–39세 중 24시간 회상조사와 검진에 참여하고 취침 시각에 응답한 %s명이다(임신부, 당뇨병·고혈압·이상지질혈증 진단자 제외).", big(n_final)),
    sprintf("- 마지막 식사–취침 간격은 3시간 이상이 %s%%로 대부분이었고, 1시간 미만 %s%%, 1–2시간 미만 %s%%, 2–3시간 미만 %s%%였다(가중 %%, 괄호는 SE).",
            pct_of("3시간 이상"), pct_of("<1시간"), pct_of("1-<2시간"), pct_of("2-<3시간")),
    sprintf("- 보정 후 1시간 미만 군(vs 3시간 이상)은 공복혈당 차이 %s mg/dL, 중성지방 %s%%, 대사증후군 OR %s로 **뚜렷한 연관성이 없었다**(경향성 p = %s, %s, %s).",
            ci_txt(w_glu), ci_txt(w_tg), ci_txt(w_ms), fmt_p(w_glu$p_trend), fmt_p(w_tg$p_trend), fmt_p(w_ms$p_trend)),
    sprintf("- 허리둘레는 간격이 짧을수록 오히려 약간 작았다(1–2시간 미만 %s cm, 경향성 p = %s). 그러나 자정 이후 섭취자를 제외하면 %s cm로 사라지고, 평일 회상자만 보면 %s cm로 커지는 등 **가정에 민감하여** 근거로 삼기 어렵다.",
            ci_txt(w_wc2), fmt_p(w_wc2$p_trend), ci_txt(wc_s2), ci_txt(wc_s5)),
    "- 1시간 미만 군은 남성·현재흡연자·고에너지 섭취자가 많고, 취침 시각은 오히려 이른 편이어서 '늦게 먹는 것'과 '일찍 자는 것'이 섞인 지표일 수 있다.",
    "")

# 1. 자료
add("## 1. 자료와 복합표본설계", "",
    "- 자료: 국민건강영양조사 2016·2017·2018년(제7기), 2021년(제8기), 2024년(제9기)의 기본DB(`hnYY_all`)와 식품섭취조사(`hnYY_24rc`). 평일·주말 취침 시각 문항(BP16_11~24)이 있는 연도만 썼다.",
    "- 가중치: 검진(혈액·계측) 변수와 영양조사(24시간 회상) 변수를 함께 쓰므로 연관성분석 가중치 `wt_tot`(제7기 지침서 PDF p.38 / 인쇄 p.30, 제8기 PDF p.40 / 인쇄 p.32, 제9기 PDF p.42 / 인쇄 p.34). 프로젝트 규칙의 `wt_itvex`는 민감도 분석 S1로 함께 보고한다.",
    "- 통합비율: 서로 다른 기수를 합치므로 조사기간 비례(제7기 PDF p.35 / 인쇄 p.27 표 8, 제9기 PDF p.47 / 인쇄 p.39 표 15). 5개 연도 모두 1년 조사로 보아 각 1/5(가정).",
    "- 층 `kstrata`, 집락 `psu`. 층 안에 집락이 1개뿐인 경우와 연도 간 (층, 조사구) 번호가 겹치는 경우는 없었다.",
    "- 모든 추정은 `svydesign` 설계 객체를 만든 뒤 `subset`으로 분석 대상을 지정해 계산했다(행 삭제 없음).",
    "",
    "입력 파일 지문:", "",
    md_table(inputs[, .(파일 = basename(file), 크기 = big(size), md5)]))

# 2. 변수 정의
add("## 2. 변수 정의 (이용지침서 쪽수)", "",
    "변수의 의미와 코드값은 분석 연도가 속한 제7·8·9기 원시자료 이용지침서에서 확인했다. 9·99·999(모름·무응답)는 결측으로, 8·88·888(비해당)은 지침서의 의미를 보고 처리했다.", "",
    md_table(vd[, .(변수 = var, 파일 = file, 설명 = label, 코드 = levels, 결측코드 = miss, 비해당처리 = na8, 근거 = source)]),
    "### 2-1. 노출 변수(마지막 식사–취침 간격) 만들기", "",
    "1. **회상한 날의 요일**: `N_DAY`는 영양조사를 실시한 요일이므로(제7기 PDF p.299, 제8기 PDF p.287, 제9기 PDF p.234), 24시간 회상의 준거일인 조사 1일 전(2016–2018·2021, 제7기 PDF p.13, 제8기 PDF p.13) 또는 2일 전(2024, 제9기 PDF p.14·p.23)으로 되돌렸다.",
    "2. **취침 시각**: 회상한 날 밤이 일–목요일이면 주중 취침 시각(BP16_11·12), 금·토요일이면 주말 취침 시각(BP16_21·22)을 썼다. 지침서 규칙대로 1–12시는 24를 더했다(제7기 PDF p.46 / 인쇄 p.38).",
    "3. **마지막 섭취 시각**: 24시간 회상 자료에서 식이보충제(N_MEAL = 5, 2016–2017년만)와 시각을 모르는 기록(N_MEAL_T 8881–8883)을 빼고, 에너지가 0보다 큰 끼니 중 가장 늦은 시각.",
    "4. **가정 A**: 00:00–04:59에 기록된 섭취는 지침서에 하루 경계 정의가 없어 확인할 수 없었다. 자료에서 이 시간대 기록 다수가 '저녁'·'간식'으로 분류되어 있어, 아침(N_MEAL = 1)이 아니면 회상일 밤 자정 이후의 섭취로 보고 24시간을 더했다. 영향은 민감도 분석 S2로 확인했다.",
    "5. **간격** = 취침 시각 − 마지막 섭취 시각. 1시간 미만 / 1–2시간 미만 / 2–3시간 미만 / 3시간 이상(기준)으로 나누고, 경향성 검정에는 0–3점 점수를 썼다.",
    "",
    "### 2-2. 결과 변수와 공변량", "",
    "- 공복혈당 `HE_glu`: 공복 8시간 이상(`HE_fst`)인 사람만(제7기 PDF p.49 / 인쇄 p.41). 중성지방 `HE_TG`: 공복 12시간 이상만(제7기 PDF p.50 / 인쇄 p.42), 로그 변환하여 % 차이로 보고. 허리둘레 `HE_wc`(cm).",
    "- 대사증후군: 지침서에 정의가 없어 문헌 기준(NCEP-ATP III 개정 기준, 허리둘레는 한국인 기준 남 90·여 85 cm)을 적용했다(가정). 진단자를 제외했으므로 약물 기준은 쓰지 않았고, 중성지방 조건 때문에 공복 12시간 이상인 사람만 판정했다.",
    "- 공변량: 나이, 성별, 조사연도(모형 1); 소득 4분위, 교육(중졸 이하/고졸/대졸 이상), 현재흡연·월간음주(지침서 주요지표 정의, 제7기 PDF p.46 / 인쇄 p.38), 유산소 신체활동, 수면시간(회상일 유형 기준), 하루 에너지 섭취량(모형 2).",
    "")

# 3. 흐름
add("## 3. 대상자 선정 흐름", "",
    md_table(flow[, .(단계 = step, 남은_n = big(n), 제외 = ifelse(is.na(excluded), "", big(excluded)))], c("단계", "남은 n", "제외")),
    "![대상자 선정 흐름](fig/fig1_participant_flow.png)", "")

# 4. 노출 분포
q[, 변수 := c(interval_h = "마지막 식사–취침 간격(시간)", bed_m = "취침 시각(자정 이후는 24+시)", last_t = "마지막 섭취 시각(자정 이후는 24+시)")[variable]]
add("## 4. 노출 분포", "",
    md_table(ex[, .(간격 = as.character(level), 비가중_n = big(n), 가중퍼센트 = weighted_pct)], c("마지막 식사–취침 간격", "비가중 n", "가중 % (SE)")),
    "가중 사분위수:", "",
    md_table(q[, .(변수, p25 = sprintf("%.1f", p25), p50 = sprintf("%.1f", p50), p75 = sprintf("%.1f", p75))], c("변수", "25%", "50%(중앙값)", "75%")),
    "![노출 분포](fig/fig2_exposure_distribution.png)", "")

# 5. Table 1
grp <- c("전체", "<1시간", "1-<2시간", "2-<3시간", "3시간 이상")
t1m <- t1[, .(항목 = ifelse(nzchar(level), paste0(label, ": ", level), label), 통계 = stat)]
for (g in grp) {
  n_col <- paste0(g, ": n"); e_col <- paste0(g, ": 추정치")
  t1m[, (g) := ifelse(t1[[n_col]] == 0, "–", paste0(t1[[e_col]], " (", big(t1[[n_col]]), ")"))]
}
setnames(t1m, "3시간 이상", "3시간 이상(기준)")
add("## 5. 대상자 특성 (Table 1): 마지막 식사–취침 간격 범주별", "",
    "각 칸은 **가중 추정치 (비가중 n)** 이다. 범주형은 가중 % (SE), 연속형은 가중 평균 (SE).", "",
    md_table(t1m),
    "주: 연도 2016–2018·2021·2024, 가중치 wt_tot × 1/5, 층 kstrata, 집락 psu. n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치. 공복혈당은 공복 8시간 이상, 중성지방·대사증후군은 공복 12시간 이상인 사람만.", "")

# 6. 본 분석
mo <- copy(m)
mo[, level := factor(level, c("3시간 이상(기준)", lv_int[1:3]))]
mo[, outcome := factor(outcome, c("공복혈당(mg/dL)", "중성지방", "허리둘레(cm)", "대사증후군"))]
setorder(mo, outcome, model, level)
add("## 6. 본 분석 (복합표본 선형·로지스틱 회귀)", "",
    "모형 1: 나이·성별·조사연도 보정. 모형 2: 모형 1 + 소득·교육·현재흡연·월간음주·유산소 신체활동·수면시간·하루 에너지. 기준은 3시간 이상. 중성지방은 로그 변환 후 % 차이로, 대사증후군은 오즈비로 보고한다.", "",
    md_table(mo[, .(결과 = as.character(outcome), 척도 = scale, 모형 = model, 간격 = as.character(level), 추정치 = fmt_ci(est, ci_low, ci_high), p = fmt_p(p),
                    해당_n = n_unweighted, 모형_n = n_model, 경향성_p = fmt_p(p_trend))],
             c("결과", "척도", "모형", "간격", "추정치 (95% CI)", "p", "해당 n", "모형 n", "경향성 p")),
    "![본 분석](fig/fig3_forest_main.png)", "")

# 7. 민감도
so <- copy(s)
so[, outcome := factor(outcome, c("공복혈당(mg/dL)", "중성지방", "허리둘레(cm)", "대사증후군", "대사증후군(혈압 전환식)"))]
setorder(so, outcome, analysis)
add("## 7. 민감도 분석 (1시간 미만 vs 3시간 이상, 모형 2)", "",
    md_table(so[, .(분석 = analysis, 결과 = as.character(outcome), 척도 = scale, 추정치 = fmt_ci(est, ci_low, ci_high), p = fmt_p(p), 해당_n = n_unweighted, 모형_n = n_model, 경향성_p = fmt_p(p_trend))],
             c("분석", "결과", "척도", "추정치 (95% CI)", "p", "해당 n", "모형 n", "경향성 p")),
    "- S1: 가중치를 프로젝트 규칙의 `wt_itvex` × 1/5로 교체. S2: 자정 이후(00–04시) 섭취자 제외(가정 A의 영향). S3: 주간 외 근무자(`EC_wht_5` = 2–7) 제외. S4: 2021·2024년 혈압을 지침서 전환식으로 청진형 기준으로 바꿔 대사증후군 재판정(제9기 PDF p.185 / 인쇄 p.177). S5: 평일(일–목) 회상자만.",
    "- 공복혈당·중성지방·대사증후군은 모든 민감도 분석에서 유의하지 않았다. 허리둘레는 S2에서 사라지고 S5에서 커져 가정 A에 민감하다.", "",
    "![민감도 분석](fig/fig4_forest_sensitivity.png)", "")

# 8. 공식 통계·재현성
add("## 8. 공식 통계 대조와 재현성", "",
    "단년도(2018) `wt_itvex`로 만 19세 이상 현재흡연율(조율)을 재현했다. 공식 조율값은 「2018 국민건강통계」에서 표 번호·쪽과 함께 확인해 `official` 열에 넣어야 한다(아직 비어 있음).", "",
    md_table(oc[, .(지표 = indicator, 비가중_n = n_unweighted, 추정치 = sprintf("%.2f", estimate), SE = sprintf("%.2f", se), CI = sprintf("%.2f–%.2f", ci_low, ci_high),
                    공식값 = ifelse(is.na(official), "미확인", sprintf("%.2f", official)), CI포함 = ifelse(is.na(within_ci), "", ifelse(within_ci, "예", "아니오")), 출처 = source)],
             c("지표", "비가중 n", "추정치(%)", "SE", "95% CI", "공식값", "CI 포함", "출처")),
    sprintf("재현성 확인: **%s** — 새 R 세션에서 분석 스크립트를 2회 실행해 결과 CSV의 md5가 모두 같았다(%s).", ifelse(length(repro_res), repro_res, "미실행"), ifelse(length(repro_time), repro_time, "")), "")

# 9. 해석
add("## 9. 해석", "",
    "1. **청년층에서 마지막 식사–취침 간격이 짧은 것과 공복혈당·중성지방·대사증후군 사이에 뚜렷한 연관성은 없었다.** 보정하지 않은 값에서는 1시간 미만 군의 중성지방과 대사증후군이 높았지만, 성별·흡연·에너지 섭취 차이로 대부분 설명되었다.",
    "2. **허리둘레의 역방향 연관성은 가정 A에 민감하다.** 자정 이후 섭취를 어느 날로 볼지에 따라 결과가 사라지거나 커지므로, 이 결과를 근거로 쓰기는 어렵다.",
    "3. **1시간 미만 군의 취침 시각이 오히려 이르다는 점**(Table 1)은 이 노출이 '늦게 먹는 것'뿐 아니라 '일찍 자는 것'을 함께 담고 있음을 뜻한다. 시계 시각 기준(21시 이후 식사)과의 설명력 비교는 아직 하지 않았다.",
    "4. 19–39세는 대사증후군 유병률이 낮아 이분형 결과의 검정력이 부족할 수 있다. 연속형 지표(혈당·중성지방 수치)에서도 연관성이 없었다는 점이 더 안정적인 근거다.",
    "")

# 10. 한계
add("## 10. 한계와 가정", "",
    "- 횡단 연구이므로 인과관계를 말할 수 없다.",
    "- 취침 시각은 '평소' 습관(설문)이고 마지막 식사 시각은 특정 하루(24시간 회상)의 기록이라, 간격에 측정 시점 불일치로 인한 오차가 있다. 이 오차는 연관성을 약하게 만드는 쪽으로 작용할 가능성이 크다.",
    "- 가정 A(자정 이후 섭취 처리), 하루 에너지 500–5000 kcal 제외 기준, 수면시간 이상값(0 이하 또는 16시간 초과) 결측 처리, 통합비율(각 연도 1년)은 지침서가 아닌 가정이다.",
    "- 대사증후군 정의는 문헌 기준이며, 공복 12시간 이상인 사람만 판정할 수 있었다.",
    "- 2021·2024년은 성인 혈압계가 바뀌었다(제8기 PDF p.18–19, 제9기 PDF p.20). 본 분석은 연도를 보정 변수로 넣었고 전환식은 S4에서만 적용했다.",
    "- 2021·2024년은 조사가 화–목요일에 몰려 주말 회상이 거의 없어, 사실상 평일 위주의 분석이다.",
    "- 수면 시각과 식사 시각 모두 자기보고이며, 공식 통계 대조의 공식값이 아직 비어 있어 확보되기 전까지는 예비 결과로 본다.",
    "")

# 11. 파일
add("## 11. 결과 파일", "",
    "| 파일 | 내용 |", "|---|---|",
    "| `01_variable_definitions.csv` | 변수 정의와 지침서 쪽수 |",
    "| `02_participant_flow.csv` | 대상자 선정 흐름 |",
    "| `03_exposure_distribution.csv`, `03_exposure_quantiles_weighted.csv` | 노출 분포 |",
    "| `04_table1.csv`, `04_table1_footnote.txt` | Table 1 |",
    "| `05_official_check.csv` | 공식 통계 대조표 |",
    "| `06_main_results.csv` | 본 분석 |",
    "| `07_sensitivity.csv` | 민감도 분석 |",
    "| `fig/fig1~fig4*.png` | 그림 |",
    "| `_inputs.csv`, `_sessionInfo.txt`, `_reproducibility.txt` | 재현성 기록 |",
    "")

writeLines(L, con <- file(out_md, open = "w", encoding = "UTF-8")); close(con)
cat("보고서 저장:", out_md, "(", length(L), "줄 )\n")
