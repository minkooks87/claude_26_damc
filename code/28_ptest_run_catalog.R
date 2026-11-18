# 28_ptest_run_catalog.R
# 목적: /knhanes_ptest로 돌린 모든 실행을 한 줄씩 정리해, 각 실행이 어떤 절단점·음주량 형태·결과 변수·보정변수·대상으로
#       분석했는지 알기 쉽게 표로 만든다. (p값은 다루지 않는다. 질문별 p는 output/21_ptest/G_요약_질문별p.md)
# 입력: output/21_ptest/ptest_log.csv (ptest가 실행마다 누적 기록)
# 출력: output/21_ptest/ptest_실행목록.md, output/21_ptest/ptest_실행목록.csv
# 실행: Rscript --vanilla code/28_ptest_run_catalog.R

suppressPackageStartupMessages(library(data.table))
out <- file.path("output", "21_ptest")
lg <- fread(file.path(out, "ptest_log.csv"), encoding = "UTF-8")
if (!"gram" %in% names(lg)) lg[, gram := NA_integer_]

# ---- 1. 실행 단위로 묶기 (같은 시각·이름·음주량·대상이면 한 실행) ----
runs <- lg[, .(결과 = paste(intersect(c("고요산혈증", "혈청 요산"), unique(결과)), collapse = ", "),
               묶음목록 = list(unique(보정)), 주질문검정 = paste(unique(검정[구분 == "주질문"]), collapse = " | ")),
           by = .(run_time, 기준, 음주량, 대상군, gram)]
setorder(runs, run_time)
runs[, 번호 := sprintf("%02d", .I)]

# ---- 2. 음주량 설명 문자열에서 형태·절단점 읽기 ----
num_list <- function(s) if (is.na(s) || !nzchar(s)) "" else gsub(",", " · ", s)
runs[, 남절단 := sub(".*남 ([0-9.,]+) / 여.*", "\\1", 음주량)][!grepl("^범주", 음주량), 남절단 := NA]
runs[, 여절단 := sub(".*/ 여 ([0-9.,]+) .*", "\\1", 음주량)][!grepl("^범주", 음주량), 여절단 := NA]
runs[, 군수 := ifelse(is.na(남절단), NA_integer_, lengths(strsplit(남절단, ",")) + 1L)]
runs[, 음주량형태 := fcase(
  grepl("^범주", 음주량), paste0("범주형 ", 군수, "군"),
  grepl("스플라인", 음주량), "연속형: 제한 삼차 스플라인",
  grepl("log2", 음주량), "연속형: log2(잔/주 + 1)",
  grepl("^연속", 음주량), "연속형: 직선",
  default = "?")]
runs[, 연속단위 := fcase(
  grepl("스플라인", 음주량), "매듭 4개(성별 5·35·65·95 백분위), 선형항 10 g/일당",
  grepl("log2", 음주량), "잔수 두 배당",
  grepl("하루 1잔당", 음주량), "7 g/일당(하루 1잔당)",
  grepl("14.2857 g/일당", 음주량), "14.3 g/일당(= 100 g/주당)",
  grepl("[0-9.]+ g/일당", 음주량), paste0(sub(".*연속: ([0-9.]+) g/일당.*", "\\1", 음주량), " g/일당"),
  default = "")]
runs[, 남녀기준 := fcase(is.na(남절단), "", 남절단 == 여절단, "남녀 같음", default = "남녀 다름")]
runs[, 경향성 := ifelse(grepl("경향성", 음주량), "예", "")]
runs[, 한잔g := ifelse(is.na(gram), 7L, gram)]
runs[, 대상 := fcase(대상군 == "all", "전체 (남 5,411 · 여 5,368)", 대상군 == "premeno", "남자 전체 + 폐경 전 여자만",
                     대상군 == "nopreg", "임신·수유 여성 제외", default = 대상군)]

# ---- 3. 보정 묶음: 이름과 실제 변수 ----
cov_kor <- c(age = "연령", age10 = "연령(10세 범주)", f_edu = "교육수준", f_incm = "소득", f_year = "조사연도",
             f_smk = "흡연(2범주)", f_smk3 = "흡연(3범주)", f_pa = "신체활동", he_bmi = "BMI", f_htn = "고혈압",
             f_dm = "당뇨병", egfr10 = "eGFR", meno_cov = "폐경 상태(여)", f_htn_dx = "고혈압(의사진단)", f_dm_dx = "당뇨병(의사진단)")
order_vars <- names(cov_kor)
sets <- unique(lg[, .(보정, 보정변수)])
sets[, 변수 := sapply(보정변수, function(x) { v <- strsplit(x, "+", fixed = TRUE)[[1]]; v <- v[nzchar(v)]
  if (!length(v)) "없음" else paste(cov_kor[intersect(order_vars, v)], collapse = ", ") })]
sets[, 폐경 := ifelse(grepl("meno_cov", 보정변수), "포함", "미포함")]
# 같은 이름이라도 폐경 자동 포함 전후로 변수가 다르므로 (이름, 변수) 조합마다 짧은 이름을 붙인다
short <- function(nm, has_meno) {
  base <- fcase(nm %in% c("Model 2", "M2"), "M2", nm == "Model 3", "M3", nm == "Crude", "Crude",
                nm == "Model 2-f_smk+f_smk3", "M2S", nm == "Model 2-f_smk+f_smk3-age+age10", "M2A",
                nm == "Model 3-f_smk+f_smk3", "M3S", nm == "Model 2-meno", "M2(폐경 뺌)",
                nm == "Model 2+he_bmi-f_pa", "M2+BMI−신체활동", nm == "age10+f_smk3", "연령범주+흡연3", default = nm)
  ifelse(base %in% c("M2", "M3", "연령범주+흡연3") & !has_meno, paste0(base, "(폐경 전)"), base)
}
sets[, 약칭 := short(보정, 폐경 == "포함")]
sets <- unique(sets[, .(약칭, 보정, 변수, 폐경, 보정변수)])
sets_show <- sets[, .(`ptest 표기` = paste(unique(보정), collapse = " 또는 ")), by = .(약칭, 변수, 폐경)]
runs[, 보정묶음 := sapply(seq_len(.N), function(i) {
  nm <- 묶음목록[[i]]; bv <- unique(lg[run_time == runs$run_time[i] & 기준 == runs$기준[i], .(보정, 보정변수)])
  paste(sapply(nm, function(n) sets[보정 == n & 보정변수 == bv[보정 == n, 보정변수][1], 약칭][1]), collapse = ", ") })]
runs[, 폐경보정 := sapply(seq_len(.N), function(i) {
  bv <- unique(lg[run_time == runs$run_time[i] & 기준 == runs$기준[i], 보정변수]); bv <- bv[nzchar(bv)]
  if (runs$대상군[i] == "premeno") return("해당 없음(폐경 전 여자만)")
  if (!length(bv)) "" else if (all(grepl("meno_cov", bv))) "포함" else if (any(grepl("meno_cov", bv))) "일부 포함" else "미포함" })]

# ---- 4. 실행 성격 ----
runs[, 질문구조 := ifelse(grepl("현재 홍조", 주질문검정), "개정 전(주질문 = 현재 홍조 2군)", "개정 후(주질문 = 지속 홍조 vs 비홍조)")]
runs[, 단계 := fcase(grepl("^G[0-9]{2} ", 기준), "G: GitHub 기준 전체(최종)",
                     grepl("^C[0-9]{2} ", 기준), "C: 제안·문헌 기준(1차)",
                     default = "도구 검증·시험")]
runs[, 출처 := fcase(grepl("설정안", 기준), "보정변수 설정안 문서(조용찬)", grepl("\\[폴더\\]|\\[표\\]", 기준), "음주량선정기준 폴더의 문헌",
                     grepl("\\[제안|^C0[1-6] ", 기준), "사용자 제안 기준", default = "")]

cat_tab <- runs[, .(번호, 실행 = run_time, 이름 = 기준, 단계, 출처, 질문구조, 음주량형태,
                    `남 절단점(g/일)` = sapply(남절단, num_list), `여 절단점(g/일)` = sapply(여절단, num_list), 남녀기준,
                    `연속형 단위` = 연속단위, 경향성검정 = 경향성, `1잔(g)` = 한잔g, 결과변수 = 결과, 대상, 보정묶음, 폐경보정)]
fwrite(cat_tab, file.path(out, "ptest_실행목록.csv"), bom = TRUE)

# ---- 5. markdown ----
md_tab <- function(dt) { x <- as.data.table(lapply(dt, function(v) ifelse(is.na(v), "", as.character(v))))
  c(paste0("| ", paste(names(x), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|"),
    apply(as.matrix(x), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")), "") }
body_tab <- function(dt) dt[, .(번호, 이름 = sub("^(G|C)[0-9]{2} ", "", 이름), 출처, 음주량형태, `남 절단점(g/일)`, `여 절단점(g/일)`, `연속형 단위`,
                               경향성검정, 결과변수, 대상, 보정묶음)]
G <- cat_tab[startsWith(단계, "G")]; C <- cat_tab[startsWith(단계, "C")]; V <- cat_tab[단계 == "도구 검증·시험"]
md <- c("# /knhanes_ptest 실행 목록: 절단점·음주량 형태·결과 변수·보정변수",
        "", sprintf("- 생성: `code/28_ptest_run_catalog.R`, 입력 `output/21_ptest/ptest_log.csv`. 실행 %d개(최종 G %d개, 1차 C %d개, 도구 검증·시험 %d개).", nrow(cat_tab), nrow(G), nrow(C), nrow(V)),
        "- 자료: 국민건강영양조사 2019–2021, 최종 분석 대상 10,779명(남 5,411 · 여 5,368). 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu.",
        "- **절단점 단위는 g/일**이다. 국건영 1잔 ≈ 7 g이므로 g/일 = 주당 잔수와 같다(1잔을 10 g으로 본 실행만 따로 표시).",
        "- 절단점 `a · b`는 a 미만 / a 이상 b 미만 / b 이상. **남녀 다름**이면 남녀에게 다른 절단점을 썼다는 뜻이다.",
        "- 결과 변수: **고요산혈증**(남 7.0, 여 6.0 mg/dL 이상, 로지스틱 회귀) / **혈청 요산**(mg/dL, 연속형, 선형회귀).",
        "- 경향성검정 = 예: 범주별 g/일 중앙값을 점수로 넣은 1 df 경향성 검정을 함께 했다.",
        "- 보정묶음의 약칭은 3절 표에 정확한 변수 목록이 있다. 폐경 상태는 여자 모형에만 들어가고, 남자 모형에서는 자동으로 빠진다.", "",
        "## 1. 최종 분석: GitHub 기준 전체 (G01–G16, 2026-11-16)", "",
        "모든 실행이 **같은 조건**이다: 질문 구조 개정 후, 1잔 7 g, 대상 전체, 결과 변수 = 고요산혈증과 혈청 요산(연속형) 둘 다, 보정 묶음 6개 = Crude, M2, M2S, M2A, M3, M3S.", "",
        md_tab(G[, .(번호, 이름 = sub("^G[0-9]{2} ", "", 이름), 출처, 음주량형태, `남 절단점(g/일)`, `여 절단점(g/일)`, `연속형 단위`, 경향성검정)]),
        "## 2. 1차 분석: 제안·문헌 기준 (C01–C12, 2026-11-16)", "",
        "모든 실행이 **같은 조건**이다: 질문 구조 개정 후, 1잔 7 g, 대상 전체, 결과 변수 = **고요산혈증만**, 보정 묶음 2개 = M2, M3(폐경 포함). G 실행과 같은 기준은 G의 결과가 그대로 같다.", "",
        md_tab(C[, .(번호, 이름 = sub("^C[0-9]{2} ", "", 이름), 음주량형태, `남 절단점(g/일)`, `여 절단점(g/일)`, `연속형 단위`, 경향성검정)]),
        "## 3. 보정 묶음별 정확한 변수", "",
        md_tab(sets_show[order(match(약칭, c("Crude", "M2", "M2S", "M2A", "M3", "M3S")), 약칭), .(약칭, `ptest 표기`, 들어간변수 = 변수, `폐경 상태` = 폐경)]),
        "- **M2(주 모형):** 연령, 교육수준, 소득, 조사연도, 흡연(2범주), 신체활동, 폐경 상태(여). **M3:** M2 + BMI, 고혈압, 당뇨병, eGFR(매개 가능 변수라 민감도 분석).",
        "- **M2S / M3S:** 흡연을 3범주(비흡연·과거·현재)로 바꾼 것. **M2A:** M2S에서 연령을 10세 범주로 바꾼 것(연령의 U자 관계 점검).",
        "- 약칭에 '(폐경 전)'이 붙은 묶음은 폐경 상태를 항상 넣기로 정하기 **전**에 돌린 실행이라 폐경이 없다.", "",
        "## 4. 도구 검증·시험 실행 (결과 해석에 쓰지 않음)", "",
        "스킬을 만들고 고치면서 계산이 맞는지 확인하려고 돌린 실행이다. 결과를 해석하거나 보고서에 쓰지 않는다.", "",
        md_tab(V[, .(번호, 실행, 이름, 질문구조, 음주량형태, `남 절단점(g/일)`, `여 절단점(g/일)`, `연속형 단위`, 경향성검정, `1잔(g)`, 결과변수, 대상, 보정묶음, 폐경보정)]),
        "## 5. 변수 이름 풀이", "",
        md_tab(data.table(`ptest 이름` = names(cov_kor), 뜻 = unname(cov_kor),
                          `정의 (제8기 원시자료 이용지침서 PDF 쪽)` = c(
                            "age, 만나이(80 = 80세 이상) (p.91)", "age를 19–29, 30–39, …, 70+로 나눔 (p.91)",
                            "edu 4범주: 초졸 이하·중졸·고졸·대졸 이상 (p.92)", "incm 개인 소득 4분위 (p.91)", "조사연도 2019·2020·2021 (p.91)",
                            "현재흡연 vs 비흡연, BS1_1·BS3_1 (p.175, 정의 p.48)", "비흡연·과거흡연·현재흡연, BS1_1·BS3_1 (p.175)",
                            "pa_aerobic 유산소 신체활동 실천 (p.188)", "HE_BMI 체질량지수, 연속 (p.198)",
                            "HE_HP = 3 고혈압 유병 (p.196)", "HE_DM_HbA1c = 3 당뇨병 유병 (p.199)",
                            "HE_crea로 계산한 CKD-EPI 2009 eGFR, 10 단위 (p.200). 계산식은 지침서에 없는 문헌식",
                            "LW_ms 2·3·4 폐경 전, 5·6 폐경 후, 7 기타, 9 결측 (p.190)",
                            "DI1_dg 고혈압 의사진단 (p.100)", "DE1_dg 당뇨병 의사진단 (p.112)"))))
con <- file(file.path(out, "ptest_실행목록.md"), open = "w", encoding = "UTF-8"); writeLines(md, con); close(con)
options(width = 250, datatable.print.nrows = 100)
print(cat_tab[, .(번호, 이름, 단계, 음주량형태, `남 절단점(g/일)`, `여 절단점(g/일)`, `연속형 단위`, 경향성검정, `1잔(g)`, 결과변수, 대상군 = 대상, 보정묶음, 폐경보정)])
print(sets)
cat("저장:", file.path(out, "ptest_실행목록.md"), "\n")
