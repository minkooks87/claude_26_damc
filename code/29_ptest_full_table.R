# 29_ptest_full_table.R
# 목적: /knhanes_ptest로 돌린 모든 실행을 "실행 × 결과 변수 × 보정 묶음" 한 줄로 펼쳐,
#       조건(절단점, 음주량 형태, 결과 변수가 연속형인지, 보정변수)과 질문별 p값을 같은 줄에 보여 준다.
# 입력: output/21_ptest/ptest_log.csv (ptest가 실행마다 누적 기록)
# 출력: output/21_ptest/ptest_전체표_조건과p.md (최종 G·1차 C·검증 실행), output/21_ptest/ptest_전체표_조건과p.csv (모든 검정 열 포함)
# 실행: Rscript --vanilla code/29_ptest_full_table.R

suppressPackageStartupMessages(library(data.table))
out <- file.path("output", "21_ptest")
lg <- fread(file.path(out, "ptest_log.csv"), encoding = "UTF-8")
if (!"gram" %in% names(lg)) lg[, gram := NA_integer_]
if (!"효과" %in% names(lg)) lg[, 효과 := NA_character_]
if ("효과(1 df)" %in% names(lg)) lg[is.na(효과) | !nzchar(효과), 효과 := `효과(1 df)`]

# ---- 1. 실행 번호와 조건 ----
runs <- unique(lg[, .(run_time, 기준, 음주량, 대상군, gram)])
setorder(runs, run_time)
runs[, 번호 := sprintf("%02d", .I)]
runs[, 남절단 := ifelse(grepl("^범주", 음주량), sub(".*남 ([0-9.,]+) / 여.*", "\\1", 음주량), NA)]
runs[, 여절단 := ifelse(grepl("^범주", 음주량), sub(".*/ 여 ([0-9.,]+) .*", "\\1", 음주량), NA)]
runs[, 군수 := ifelse(is.na(남절단), NA_integer_, lengths(strsplit(남절단, ",")) + 1L)]
runs[, 음주량형태 := fcase(grepl("^범주", 음주량), paste0("범주형 ", 군수, "군"),
                           grepl("스플라인", 음주량), "연속형(스플라인)",
                           grepl("log2", 음주량), "연속형(log2)",
                           grepl("^연속", 음주량), "연속형(직선)", default = "?")]
runs[, 연속단위 := fcase(grepl("스플라인", 음주량), "매듭 4개, 직선부 10 g/일당", grepl("log2", 음주량), "잔수 두 배당",
                         grepl("하루 1잔당", 음주량), "7 g/일당", grepl("14.2857", 음주량), "100 g/주당",
                         grepl("[0-9.]+ g/일당", 음주량), paste0(sub(".*연속: ([0-9.]+) g/일당.*", "\\1", 음주량), " g/일당"), default = "")]
runs[, 경향성 := grepl("경향성", 음주량)]
runs[, 한잔g := ifelse(is.na(gram), 7L, gram)]
runs[, 대상 := fcase(대상군 == "all", "전체", 대상군 == "premeno", "남 전체+폐경 전 여", 대상군 == "nopreg", "임신·수유 제외", default = 대상군)]
runs[, 단계 := fcase(grepl("^G[0-9]{2} ", 기준), "G", grepl("^C[0-9]{2} ", 기준), "C", default = "검증")]
fmt_cut <- function(x) ifelse(is.na(x), "", gsub(",", " · ", x))
# 연구 질문 개정 전 실행: 주질문이 "음주 × 현재 홍조"(현재 홍조 2군)였다
old_q <- unique(lg[구분 == "주질문" & grepl("현재 홍조", 검정), run_time])
runs[, 질문구조 := ifelse(run_time %in% old_q, "개정 전(주 = 현재 홍조 vs 현재 비홍조)", "개정 후(주 = 지속 홍조 vs 비홍조)")]

# ---- 2. 보정 묶음: 약칭과 정확한 변수 ----
cov_kor <- c(age = "연령", age10 = "연령(10세 범주)", f_edu = "교육", f_incm = "소득", f_year = "조사연도",
             f_smk = "흡연(2범주)", f_smk3 = "흡연(3범주)", f_pa = "신체활동", he_bmi = "BMI", f_htn = "고혈압",
             f_dm = "당뇨병", egfr10 = "eGFR", meno_cov = "폐경(여)", f_htn_dx = "고혈압(의사진단)", f_dm_dx = "당뇨병(의사진단)")
kor_vars <- function(x) { v <- strsplit(x, "+", fixed = TRUE)[[1]]; v <- v[nzchar(v)]
  if (!length(v)) "없음" else paste(cov_kor[intersect(names(cov_kor), v)], collapse = ", ") }
short <- function(nm, bv) {
  m <- grepl("meno_cov", bv)
  base <- fcase(nm %in% c("Model 2", "M2"), "M2", nm == "Model 3", "M3", nm == "Crude", "Crude",
                nm == "Model 2-f_smk+f_smk3", "M2S", nm == "Model 2-f_smk+f_smk3-age+age10", "M2A",
                nm == "Model 3-f_smk+f_smk3", "M3S", nm == "Model 2-meno", "M2(폐경 뺌)",
                nm == "Model 2+he_bmi-f_pa", "M2+BMI−신체활동", nm == "age10+f_smk3", "연령범주+흡연3", default = nm)
  ifelse(base %in% c("M2", "M3", "연령범주+흡연3") & !m, paste0(base, "(폐경 없음)"), base)
}
lg[, 묶음 := short(보정, 보정변수)]
lg[, 보정변수목록 := vapply(보정변수, kor_vars, "")]

# ---- 3. 검정을 열로: 각 p값이 무엇인지 ----
lg[, 경향 := grepl("경향성", 검정)]
lg[, 열 := fcase(
  구분 == "주질문" & !경향 & 대상 == "남자", "주 남", 구분 == "주질문" & !경향 & 대상 == "여자", "주 여",
  구분 == "부질문 1" & !경향 & grepl("소실 vs 비홍조", 검정) & 대상 == "남자", "부1 남",
  구분 == "부질문 1" & !경향 & grepl("소실 vs 비홍조", 검정) & 대상 == "여자", "부1 여",
  구분 == "부질문 2" & !경향, "부2 성별", 구분 == "부질문 2(탐색)", "부2 폐경",
  구분 == "주질문" & 경향 & 대상 == "남자", "주 경향 남", 구분 == "주질문" & 경향 & 대상 == "여자", "주 경향 여",
  구분 == "부질문 1" & 경향 & 대상 == "남자", "부1 경향 남", 구분 == "부질문 1" & 경향 & 대상 == "여자", "부1 경향 여",
  구분 == "부질문 2" & 경향, "부2 경향 성별",
  구분 %in% c("부질문 1(탐색)") | (구분 == "부질문 1" & grepl("소실 vs 지속", 검정)), paste0("소실vs지속 ", substr(대상, 1, 1)),
  구분 == "탐색적 분석", paste0("탐색(홍조 체질) ", substr(대상, 1, 1)),
  구분 == "용량-반응" & grepl("비선형", 검정), paste0("비선형성 ", substr(대상, 1, 1)),
  구분 == "참고" & grepl("현재 홍조 vs", 검정), paste0("참고 현재홍조 ", substr(대상, 1, 1)),
  default = NA_character_)]
fp <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
lg[, 표시 := ifelse(grepl("분리", 비고), "무효", paste0(fp(p), ifelse(grepl("사례 10명", 비고), "†", ""), ifelse(!is.na(p) & p < 0.05 & !grepl("분리", 비고), "*", "")))]
x <- lg[!is.na(열)]
dup <- x[, .N, by = .(run_time, 기준, 결과, 묶음, 열)][N > 1]
if (nrow(dup)) x <- x[, .SD[1], by = .(run_time, 기준, 결과, 묶음, 열)]          # 같은 열에 두 검정이 걸리면 첫 번째만(로그 구조상 생기지 않아야 함)
w <- dcast(x, run_time + 기준 + 결과 + 묶음 + 보정변수목록 ~ 열, value.var = "표시")
nm <- dcast(x, run_time + 기준 + 결과 + 묶음 ~ 열, value.var = "모형 n", fun.aggregate = function(v) v[1])
w <- merge(runs, w, by = c("run_time", "기준"))
w[, 결과변수 := ifelse(결과 == "고요산혈증", "고요산혈증(예/아니오)", "혈청 요산(mg/dL, 연속형)")]
setorder(w, 번호, 결과, 묶음)
mord <- c("Crude", "M2", "M2S", "M2A", "M3", "M3S")
w[, mo := match(묶음, mord)][is.na(mo), mo := 99L]
setorder(w, 번호, 결과, mo)

pcols_core <- c("주 남", "주 여", "부1 남", "부1 여", "부2 성별", "부2 폐경")
pcols_all <- c(pcols_core, "주 경향 남", "주 경향 여", "부1 경향 남", "부1 경향 여", "부2 경향 성별",
               "소실vs지속 남", "소실vs지속 여", "탐색(홍조 체질) 남", "탐색(홍조 체질) 여", "비선형성 남", "비선형성 여",
               "참고 현재홍조 남", "참고 현재홍조 여")
for (c in setdiff(pcols_all, names(w))) w[[c]] <- ""
for (c in pcols_all) w[is.na(get(c)), (c) := ""]
full <- w[, c(list(번호 = 번호, 단계 = 단계, 기준 = 기준, 음주량형태 = 음주량형태, `남 절단점(g/일)` = fmt_cut(남절단), `여 절단점(g/일)` = fmt_cut(여절단),
                   `연속형 단위` = 연속단위, 경향성검정 = ifelse(경향성, "예", ""), `1잔(g)` = 한잔g, 대상 = 대상, 결과변수 = 결과변수,
                   보정묶음 = 묶음, 보정변수 = 보정변수목록, 질문구조 = 질문구조), mget(pcols_all))]
fwrite(full, file.path(out, "ptest_전체표_조건과p.csv"), bom = TRUE)

# ---- 4. markdown ----
md_tab <- function(dt) { y <- as.data.table(lapply(dt, function(v) ifelse(is.na(v), "", as.character(v))))
  c(paste0("| ", paste(names(y), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(y)), collapse = "|"), "|"),
    apply(as.matrix(y), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")), "") }
crit_name <- function(s) sub("^(G|C)[0-9]{2} ", "", s)
sec <- function(dt, extra = character(0)) {
  dt[, c(list(기준 = crit_name(기준), `남 절단점` = `남 절단점(g/일)`, `여 절단점` = `여 절단점(g/일)`, 음주량형태 = 음주량형태), mget(c(pcols_core, extra)))]
}
G <- full[단계 == "G"]; C <- full[단계 == "C"]; V <- full[단계 == "검증"]
sets_tab <- unique(lg[묶음 %in% mord, .(묶음, 보정변수목록)])[order(match(묶음, mord))]
legend <- data.table(
  열 = c("주 남 / 주 여", "부1 남 / 부1 여", "부2 성별", "부2 폐경", "주 경향 · 부1 경향", "부2 경향 성별", "소실vs지속", "탐색(홍조 체질)", "비선형성", "참고 현재홍조"),
  `무엇에 대한 p인가` = c(
    "**주질문.** 남자(여자)에서 음주량과 결과의 관계가 **지속 홍조군과 비홍조군 사이에 다른가** (음주 × 지속 홍조 상호작용)",
    "**부질문 1.** 남자(여자)에서 같은 관계가 **홍조 소실군과 비홍조군 사이에 다른가** (음주 × 홍조 소실 상호작용)",
    "**부질문 2.** 지속 홍조군과 비홍조군의 차이가 **남녀 사이에 다른가** (음주 × 홍조 × 성별 3원 상호작용)",
    "**부질문 2(탐색).** 같은 차이가 **여성의 폐경 전과 후에 다른가** (음주 × 홍조 × 폐경 3원 상호작용)",
    "주질문·부질문 1과 같은 질문을, 범주별 g/일 중앙값을 점수로 넣은 **1 df 경향성 검정**으로 본 p (범주형이고 경향성검정 = 예일 때만)",
    "부질문 2와 같은 질문의 1 df 경향성 검정 p",
    "홍조 소실군과 지속 홍조군 사이의 차이 (탐색)",
    "과거 또는 현재 홍조가 있는 사람 vs 한 번도 없는 사람 (계획서에 없던 탐색 분석)",
    "스플라인일 때, 음주량과 결과의 관계가 **직선에서 벗어나는가**",
    "계획 발표 때의 주질문: 현재 홍조군 vs 현재 비홍조군"))
md <- c("# /knhanes_ptest 전체 표: 조건(절단점·결과 변수·보정변수)과 질문별 p값", "",
  sprintf("- 생성: `code/29_ptest_full_table.R`, 입력 `output/21_ptest/ptest_log.csv`. 한 줄 = 실행 하나 × 결과 변수 하나 × 보정 묶음 하나 (%d줄).", nrow(full)),
  "- 자료: 국민건강영양조사 2019–2021, 최종 분석 대상 10,779명(남 5,411 · 여 5,368). 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu. p는 설계 기반 Wald F 검정.",
  "- **절단점 단위는 g/일**(1잔 ≈ 7 g이라 g/일 = 주당 잔수). `a · b`는 a 미만 / a 이상 b 미만 / b 이상. 남녀 값이 다르면 성별 기준.",
  "- **결과 변수:** 고요산혈증(남 7.0, 여 6.0 mg/dL 이상, 예/아니오) 또는 혈청 요산(mg/dL, **연속형**).",
  "- **표시:** `*` p < 0.05 · `†` 비교하는 칸에 고요산혈증 사례 10명 미만이라 해석하지 않음 · `무효` 칸이 비어 계산이 성립하지 않음 · 빈칸 해당 검정 없음.",
  "- 기준 16개 × 결과 2개 × 보정 묶음 6개를 함께 본 결과다. p < 0.05 하나를 확증으로 읽지 말고, 미리 정한 주분석 기준과 주 모형(M2)을 먼저 해석한다.", "",
  "## 1. 각 p값이 무엇인가", "", md_tab(legend),
  "## 2. 보정 묶음별 변수", "", md_tab(sets_tab[, .(보정묶음 = 묶음, 들어간변수 = 보정변수목록)]),
  "- 폐경(여)은 여자 모형에만 들어가고 남자 모형에서는 자동으로 빠진다. M2가 주 모형, M3은 매개 가능 변수까지 넣은 민감도 분석이다.", "",
  "## 3. 최종 분석 G01–G16 (대상 전체, 1잔 7 g)", "",
  "각 기준의 경향성 p, 탐색 분석 p 등 나머지 열은 CSV(`ptest_전체표_조건과p.csv`)에 있다.", "")
for (oc in c("고요산혈증(예/아니오)", "혈청 요산(mg/dL, 연속형)")) for (m in mord) {
  vars <- sets_tab[묶음 == m, 보정변수목록]
  md <- c(md, sprintf("### 결과 변수: %s · 보정 묶음: %s", oc, m), "", sprintf("보정변수: %s", vars), "",
          md_tab(sec(G[결과변수 == oc & 보정묶음 == m], if (oc == "고요산혈증(예/아니오)") c("주 경향 남", "부1 경향 남") else character(0))))
}
md <- c(md, "## 4. 1차 분석 C01–C12 (고요산혈증만, 대상 전체, 1잔 7 g)", "")
for (m in c("M2", "M3")) md <- c(md, sprintf("### 보정 묶음: %s", m), "", sprintf("보정변수: %s", sets_tab[묶음 == m, 보정변수목록]), "",
                                 md_tab(sec(C[보정묶음 == m])))
md <- c(md, "## 5. 도구 검증·시험 실행 (결과 해석에 쓰지 않음)", "",
        "스킬을 만들고 고치며 계산을 확인하려고 돌린 실행이다. 일부는 연구 질문 개정 전 구조(01–05: 주질문 = 현재 홍조 2군)이거나, 폐경 보정을 정하기 전이거나, 1잔을 10 g으로 본 실행이다.", "",
        md_tab(V[, c(list(번호 = 번호, 기준 = 기준, 음주량형태 = 음주량형태, `남 절단점` = `남 절단점(g/일)`, `여 절단점` = `여 절단점(g/일)`, `연속형 단위` = `연속형 단위`,
                     `1잔(g)` = `1잔(g)`, 대상 = 대상, 결과변수 = 결과변수, 보정변수 = 보정변수, 질문구조 = sub("\\(.*", "", 질문구조)), mget(pcols_core))]),
        "- 질문구조가 **개정 전**인 줄의 `주 남 / 주 여`는 지속 홍조가 아니라 **현재 홍조군 vs 현재 비홍조군** 비교의 p다.", "")
con <- file(file.path(out, "ptest_전체표_조건과p.md"), open = "w", encoding = "UTF-8"); writeLines(md, con); close(con)
cat("줄 수: 전체", nrow(full), "/ G", nrow(G), "/ C", nrow(C), "/ 검증", nrow(V), "\n")
if (nrow(dup)) cat("※ 같은 열에 검정이 둘 걸린 경우:", nrow(dup), "건\n")
cat("저장:", file.path(out, "ptest_전체표_조건과p.md"), "\n")
