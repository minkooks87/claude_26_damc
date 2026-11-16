# 03_flushing_hyperuricemia.R
# 주제: 성인 음주자에서 음주 관련 안면홍조가 음주와 고요산혈증의 연관성에 미치는 영향
#       - 성별 차이, 여성(폐경 전·후) 특징 중심
#       - 홍조 패턴: 과거 홍조(BD9_1) × 현재 홍조(BD9_2) → 비홍조 / 지속 / 소실 / 신규
# 자료: 국민건강영양조사 제8기 2019, 2020, 2021 기본DB(hnYY_all)
#       (홍조 문항은 2019-2021년에만 있음)
# 실행: Rscript --vanilla code/03_flushing_hyperuricemia.R   (프로젝트 폴더에서)
# 원칙: data 폴더는 읽기만 한다. 분석 대상은 행을 지우지 않고 inc(1/0)로 표시한다.
#       추정은 svydesign(가중치·층·집락) → subset 으로 한다. (knhanes-analysis 스킬)
# 쪽수 표기: "8기 PDF 쪽 / 인쇄 쪽" (제8기(2019-2021) 원시자료 이용지침서)

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
years   <- c(2019, 2020, 2021)
out_dir <- file.path("output", "03_flushing_hyperuricemia")
files   <- file.path("data", "국건영 조사결과", sprintf("hn%02d_all.sas7bdat", years %% 100))

# 가중치: 건강설문·검진 변수만 쓰므로 wt_itvex (8기 PDF p.92 / 인쇄 p.84; CLAUDE.md 규칙과 같음)
# 통합비율: 제8기는 예외적으로 조사구수 비율이 아니라 조사기간 비례 통합비율을 쓴다.
#   2020년은 코로나19로 192개 중 180개 조사구만 조사했지만 조사기간은 모두 1년 → 각 연도 값 1, 소계 3
#   근거: 8기 PDF p.36 / 인쇄 p.28 (표 7, 제8기 1차-3차(3년) 통합)
pool_ratio <- c("2019" = 1/3, "2020" = 1/3, "2021" = 1/3)
stopifnot(abs(sum(pool_ratio) - 1) < 1e-12)

# ==== 1. 변수 정의 (지침서에서 확인한 내용만 적는다) ========================
src_drk <- "8기 PDF p.161-162 / 인쇄 p.153-154"
src_ind <- "주요지표 SAS 프로그램: 8기 PDF p.48 / 인쇄 p.40"
var_dict <- rbindlist(list(
  list(var = "id",       levels = "", miss = "", na8 = "", label = "개인 아이디", source = "8기 PDF p.91 / 인쇄 p.83"),
  list(var = "year",     levels = "", miss = "", na8 = "", label = "조사연도", source = "8기 PDF p.91 / 인쇄 p.83"),
  list(var = "sex",      levels = "1=남자;2=여자", miss = "", na8 = "", label = "성별", source = "8기 PDF p.91 / 인쇄 p.83"),
  list(var = "age",      levels = "1~79=세;80=80세이상(탑코딩)", miss = "", na8 = "", label = "만나이", source = "8기 PDF p.91 / 인쇄 p.83"),
  list(var = "psu",      levels = "", miss = "", na8 = "", label = "조사구번호(집락)", source = "8기 PDF p.91 / 인쇄 p.83"),
  list(var = "kstrata",  levels = "", miss = "", na8 = "", label = "분산추정 층", source = "8기 PDF p.92 / 인쇄 p.84"),
  list(var = "wt_itvex", levels = "", miss = "", na8 = "", label = "건강설문-검진조사 가중치", source = "8기 PDF p.92 / 인쇄 p.84"),
  list(var = "incm",     levels = "1=하;2=중하;3=중상;4=상", miss = "", na8 = "", label = "소득 4분위수(개인)", source = "8기 PDF p.91 / 인쇄 p.83"),
  list(var = "edu",      levels = "1=초졸이하;2=중졸;3=고졸;4=대졸이상", miss = "", na8 = "", label = "교육수준 재분류", source = "8기 PDF p.92 / 인쇄 p.84"),
  list(var = "bd1",      levels = "1=술을 마셔 본 적 없음;2=있음", miss = "9", na8 = "8=결측(소아)", label = "평생음주경험", source = src_drk),
  list(var = "bd1_11",   levels = "1=최근1년 전혀 안 마심;2=월1회미만;3=월1회정도;4=월2-4회;5=주2-3회;6=주4회이상", miss = "9",
       na8 = "8=범주유지(평생 비음주, 문항1-①)", label = "1년간 음주빈도", source = src_drk),
  list(var = "bd2_1",    levels = "1=1-2잔;2=3-4잔;3=5-6잔;4=7-9잔;5=10잔 이상", miss = "9", na8 = "8=범주유지(최근1년 비음주)",
       label = "한 번에 마시는 음주량", source = src_drk),
  list(var = "bd2_14",   levels = "잔(10잔 이상 응답자만)", miss = "999", na8 = "888=결측(문항2-2 ①-④)", label = "한 번에 마시는 음주량(잔)", source = src_drk),
  list(var = "bd2_31",   levels = "1=전혀 없음;2=월1회미만;3=월1회정도;4=주1회정도;5=거의매일", miss = "9", na8 = "8=범주유지(지침서 고위험음주 정의에 8 포함)",
       label = "폭음 빈도(7잔 이상)", source = src_drk),
  list(var = "bd2_32",   levels = "1=전혀 없음;2=월1회미만;3=월1회정도;4=주1회정도;5=거의매일", miss = "9", na8 = "8=범주유지(지침서 고위험음주 정의에 8 포함)",
       label = "여자 폭음 빈도(5잔 이상)", source = src_drk),
  list(var = "bd9_1",    levels = "1=예;2=아니오", miss = "9", na8 = "8=결측(평생 비음주·청소년·소아)",
       label = "과거 알코올 홍조(처음 마시기 시작한 1-2년 사이 소량 음주로 금세 빨개짐)", source = "8기 PDF p.163-164 / 인쇄 p.155-156"),
  list(var = "bd9_2",    levels = "1=아니오;2=가끔 그렇다;3=자주 그렇다;4=늘 그렇다", miss = "9", na8 = "8=결측(최근1년 비음주·청소년·소아)",
       label = "현재 알코올 홍조", source = "8기 PDF p.163-164 / 인쇄 p.155-156"),
  list(var = "he_uacid", levels = "mg/dL", miss = "", na8 = "", label = "요산", source = "8기 PDF p.200 / 인쇄 p.192"),
  list(var = "he_uacid_etc", levels = "<1.0=1.0 mg/dL 미만", miss = "", na8 = "", label = "요산 장비분석범위(2017년부터 1.0 미만 표시)", source = "8기 PDF p.200 / 인쇄 p.192"),
  list(var = "he_crea",  levels = "mg/dL", miss = "", na8 = "", label = "혈중크레아티닌", source = "8기 PDF p.200 / 인쇄 p.192"),
  list(var = "dm8_dg",   levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년·소아)", label = "통풍 의사진단 여부", source = "8기 PDF p.137 / 인쇄 p.129"),
  list(var = "dm8_pt",   levels = "0=없음;1=있음", miss = "9", na8 = "8=범주유지(의사진단 받지 않음 등)", label = "통풍 치료", source = "8기 PDF p.137 / 인쇄 p.129"),
  list(var = "he_prg",   levels = "0=아니오;1=예", miss = "", na8 = "8=범주유지(비해당-남자)", label = "임신여부", source = "8기 PDF p.192 / 인쇄 p.184"),
  list(var = "lw_ms",    levels = "1=초경하기 전;2=월경중;3=임신중;4=출산후 수유 중;5=자연폐경;6=인공폐경;7=기타", miss = "9",
       na8 = "8=결측(만10세미만 여자)", label = "월경여부(여성)", source = "8기 PDF p.190 / 인쇄 p.182"),
  list(var = "he_bmi",   levels = "kg/m2", miss = "", na8 = "", label = "체질량지수", source = "8기 PDF p.198 / 인쇄 p.190"),
  list(var = "di1_dg",   levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년·소아)", label = "고혈압 의사진단", source = "8기 PDF p.100 / 인쇄 p.92"),
  list(var = "de1_dg",   levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년·소아)", label = "당뇨병 의사진단", source = "8기 PDF p.112 / 인쇄 p.104"),
  list(var = "bs1_1",    levels = "현재흡연 정의에만 사용", miss = "", na8 = "", label = "평생 흡연 여부", source = src_ind),
  list(var = "bs3_1",    levels = "현재흡연 정의에만 사용", miss = "", na8 = "", label = "현재 흡연 여부", source = src_ind),
  list(var = "pa_aerobic", levels = "0=미실천;1=실천", miss = "", na8 = "", label = "유산소 신체활동 실천", source = "8기 PDF p.188 / 인쇄 p.180")
))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(var_dict, file.path(out_dir, "01_variable_definitions.csv"), bom = TRUE)

# ---- 자료 읽기 ----
read_year <- function(f) {
  nms  <- names(read_sas(f, n_max = 0))
  want <- nms[tolower(nms) %in% var_dict$var]
  miss <- setdiff(var_dict$var, tolower(nms))
  if (length(miss)) message(basename(f), "에 없는 변수: ", paste(miss, collapse = ", "))
  x <- as.data.table(zap_labels(read_sas(f, col_select = all_of(want))))
  setnames(x, tolower(names(x)))
  x
}
d <- rbindlist(lapply(files, read_year), use.names = TRUE, fill = TRUE)
n_raw <- nrow(d)

# ---- 결측·비해당 코드 처리 (var_dict 기준) ----
split_codes <- function(s) if (nzchar(s)) trimws(strsplit(s, ";")[[1]]) else character(0)
for (i in seq_len(nrow(var_dict))) {
  v <- var_dict$var[i]
  if (!v %in% names(d) || !is.numeric(d[[v]])) next
  mc <- split_codes(var_dict$miss[i])
  if (length(mc)) d[get(v) %in% as.numeric(mc), (v) := NA]
  for (rule in split_codes(var_dict$na8[i])) {
    code <- as.numeric(sub("=.*", "", rule)); to <- sub("^[^=]*=", "", rule)
    if (startsWith(to, "결측")) d[get(v) == code, (v) := NA]
  }
}
stopifnot(nrow(d) == n_raw)

# ==== 1-2. 분석 변수 만들기 ==================================================
# (1) 홍조 패턴 (과거 × 현재)
d[, flush_grp := fcase(
  bd9_1 == 2 & bd9_2 == 1,       "비홍조",         # 과거 아니오, 현재 아니오
  bd9_1 == 1 & bd9_2 %in% 2:4,   "지속 홍조",      # 과거 예, 현재 가끔·자주·늘
  bd9_1 == 1 & bd9_2 == 1,       "홍조 소실",      # 과거 예, 현재 아니오
  bd9_1 == 2 & bd9_2 %in% 2:4,   "신규 홍조"       # 과거 아니오, 현재 예 (회상 오류 가능)
)]
d[, flush_grp := factor(flush_grp, c("비홍조", "지속 홍조", "홍조 소실", "신규 홍조"))]
d[, f_flush_now := factor(fifelse(bd9_2 %in% 2:4, "현재 홍조 있음", fifelse(bd9_2 == 1, "현재 홍조 없음", NA_character_)),
                         c("현재 홍조 없음", "현재 홍조 있음"))]

# (2) 음주 수준: 지침서 고위험음주율 정의(8기 PDF p.48 / 인쇄 p.40) 그대로
#     dr_high = 주2회 이상(BD1_11 5,6) & 1회 음주량 남 7잔 이상(BD2_1 4,5) / 여 5잔 이상(BD2_1 3,4,5)
d[, dr_high := NA_integer_]
d[(sex == 1 & bd2_31 %in% c(1, 2, 3, 4, 5, 8)) | (sex == 2 & bd2_32 %in% c(1, 2, 3, 4, 5, 8)),
  dr_high := as.integer((sex == 1 & bd1_11 %in% c(5, 6) & bd2_1 %in% c(4, 5)) |
                        (sex == 2 & bd1_11 %in% c(5, 6) & bd2_1 %in% c(3, 4, 5)))]
d[, drink3 := fcase(
  bd1_11 == 2,                                   "월1회 미만",
  bd1_11 %in% 3:6 & bd2_1 %in% 1:5 & dr_high == 0, "월1회 이상(고위험 아님)",
  bd1_11 %in% 3:6 & bd2_1 %in% 1:5 & dr_high == 1, "고위험음주"
)]
d[, drink3 := factor(drink3, c("월1회 미만", "월1회 이상(고위험 아님)", "고위험음주"))]
# (민감도) 주당 음주 잔 수 추정: 범주 중간값을 쓰는 가정 B (지침서 정의 아님)
freq_wk <- c(`2` = 0.5 / 4.33, `3` = 1 / 4.33, `4` = 3 / 4.33, `5` = 2.5, `6` = 5)
amt     <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
d[, drinks_wk := unname(freq_wk[as.character(bd1_11)]) *
                 fifelse(bd2_1 == 5, fifelse(!is.na(bd2_14), bd2_14, 10), unname(amt[as.character(bd2_1)]))]
d[, drinks_wk7 := drinks_wk / 7]                      # 7잔/주 단위

# (3) 요산·고요산혈증
#     1.0 미만(HE_Uacid_etc = "<1.0")은 2016년 표기 방식처럼 0.9로 둔다(가정, 8기 PDF p.200 각주 1)
d[, uacid := he_uacid]
d[is.na(uacid) & trimws(as.character(he_uacid_etc)) == "<1.0", uacid := 0.9]
#     고요산혈증 기준은 지침서에 없음 → 문헌 기준 남 ≥7.0, 여 ≥6.0 mg/dL 사용(가정)
d[, hua := as.integer(uacid >= fifelse(sex == 1, 7.0, 6.0))]
d[, f_hua := factor(hua, 0:1, c("없음", "있음"))]

# (4) 공변량
#     eGFR: 지침서에 계산식 없음 → 문헌 CKD-EPI 2021(인종 계수 없음) 식 사용(가정)
d[, `:=`(kap = fifelse(sex == 2, 0.7, 0.9), alp = fifelse(sex == 2, -0.241, -0.302))]
d[, egfr := 142 * pmin(he_crea / kap, 1)^alp * pmax(he_crea / kap, 1)^(-1.200) * 0.9938^age * fifelse(sex == 2, 1.012, 1)]
d[, egfr10 := egfr / 10]
d[, smk := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3, smk := as.integer(bs1_1 == 2 & bs3_1 %in% c(1, 2))]
d[, meno := fcase(lw_ms %in% c(5, 6), "폐경 후", lw_ms %in% c(2, 3, 4), "폐경 전")]   # 1(초경 전)·7(기타)은 결측
d[, `:=`(
  f_sex  = factor(sex, 1:2, c("남자", "여자")),
  f_year = factor(year),
  f_incm = factor(incm, 1:4, c("하", "중하", "중상", "상")),
  f_edu  = factor(edu, 1:4, c("초졸이하", "중졸", "고졸", "대졸이상")),
  f_smk  = factor(smk, 0:1, c("비흡연", "현재흡연")),
  f_pa   = factor(pa_aerobic, 0:1, c("미실천", "실천")),
  f_htn  = factor(di1_dg, 0:1, c("없음", "있음")),
  f_dm   = factor(de1_dg, 0:1, c("없음", "있음")),
  f_meno = factor(meno, c("폐경 전", "폐경 후")),
  sexflush = interaction(factor(sex, 1:2, c("남", "여")), flush_grp, sep = "·", lex.order = TRUE)
)]
stopifnot(nrow(d) == n_raw)

# ==== 2. 분석 대상 표시 (행 삭제 없음) ======================================
criteria <- list(
  "가중치 있음(검진·설문 참여, wt_itvex)"        = quote(!is.na(wt_itvex)),
  "만 19세 이상"                                 = quote(age >= 19),
  "최근 1년 음주자(BD1_11 = 2-6)"                = quote(bd1_11 %in% 2:6),
  "과거·현재 홍조 문항 응답"                     = quote(!is.na(flush_grp)),
  "음주 수준 분류 가능(1회 음주량·폭음 문항 응답)" = quote(!is.na(drink3)),
  "임신 중 아님"                                 = quote(he_prg %in% c(0, 8)),
  "요산 측정"                                    = quote(!is.na(uacid)),
  "통풍 의사진단 없음"                           = quote(dm8_dg == 0)
)
d[, inc := 1L]
flow <- data.table(step = "기본DB 전체(2019-2021)", n = nrow(d))
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N]))
}
flow[, excluded := shift(n) - n]
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
# 민감도 S1: 통풍 진단자 포함 대상 표시
base_ok <- Reduce(`&`, lapply(criteria[-length(criteria)], function(e) { x <- d[, eval(e)]; !is.na(x) & x }))
d[, inc_gout := as.integer(base_ok & !is.na(dm8_dg))]              # S1: 통풍 진단자(1)도 포함
d[, inc_gout9 := as.integer(base_ok & !(dm8_dg %in% 1))]            # S3: 통풍 문항 모름·무응답(9→결측)도 포함
cat("통풍 문항: 진단 있음", d[base_ok & dm8_dg %in% 1, .N], "/ 모름·무응답(결측)", d[base_ok & is.na(dm8_dg), .N], "\n")
stopifnot(nrow(d) == n_raw)

# ==== 3. 복합표본 설계 지정 → subset ========================================
d[, wt_analysis := wt_itvex * pool_ratio[as.character(year)]]
d[is.na(wt_analysis), wt_analysis := 0]
lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","),
                          " → options(survey.lonely.psu = ...) 선택 근거를 기록하세요.")
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = d)
sub <- subset(des, inc == 1)
d_sub <- d[inc == 1]

# ==== 4. Table 1: 성별 × 홍조 패턴, 비가중 n + 가중 추정치 ==================
fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
labs <- c(age = "나이", f_year = "조사연도", drink3 = "음주 수준(지침서 고위험음주 정의)", drinks_wk = "주당 음주 잔 수(추정, 가정 B)",
          f_smk = "현재흡연", f_incm = "소득 4분위(개인)", f_edu = "교육수준", f_pa = "유산소 신체활동",
          he_bmi = "체질량지수", egfr = "eGFR(CKD-EPI 2021)", f_htn = "고혈압 진단", f_dm = "당뇨병 진단",
          f_meno = "폐경(여성)", uacid = "요산(mg/dL)", f_hua = "고요산혈증")
table1_block <- function(design, dat, v) {
  if (is.factor(dat[[v]])) {
    lv  <- levels(dat[[v]])
    n   <- as.integer(table(factor(dat[[v]], levels = lv)))
    if (sum(n) == 0) return(data.table(variable = v, level = lv, n = n, estimate = "-", stat = "가중 % (SE)"))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    p   <- coef(est)[paste0(v, lv)]; se <- SE(est)[paste0(v, lv)]
    data.table(variable = v, level = lv, n = n, estimate = paste0(fmt(100 * p), " (", fmt(100 * se), ")"), stat = "가중 % (SE)")
  } else {
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    data.table(variable = v, level = "", n = sum(!is.na(dat[[v]])),
               estimate = paste0(fmt(coef(est)), " (", fmt(SE(est)), ")"), stat = "가중 평균 (SE)")
  }
}
table1_col <- function(design, dat, label, vars) {
  out <- rbindlist(lapply(vars, function(v) table1_block(design, dat, v)))
  setnames(out, c("n", "estimate"), paste0(label, c(": n", ": 추정치")))
  out
}
table1 <- NULL
for (s in c("남자", "여자")) {
  for (g in levels(d_sub$flush_grp)) {   # 남성의 폐경 행은 n = 0, 추정치 "-"로 나온다
    col <- table1_col(subset(sub, f_sex == s & flush_grp == g), d_sub[f_sex == s & flush_grp == g], paste(s, g, sep = "·"), names(labs))
    table1 <- if (is.null(table1)) col else merge(table1, col, by = c("variable", "level", "stat"), sort = FALSE)
  }
}
table1[, label := labs[variable]]
setcolorder(table1, c("label", "variable", "level", "stat"))
fwrite(table1, file.path(out_dir, "04_table1.csv"), bom = TRUE)
writeLines(c("연도: 2019, 2020, 2021 / 가중치: wt_itvex × 1/3(제8기 기수 내 조사기간 비례 통합비율), 층 kstrata, 집락 psu",
             "n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치",
             "고요산혈증: 남 ≥7.0, 여 ≥6.0 mg/dL(문헌 기준). 통풍 의사진단자 제외."),
           file.path(out_dir, "04_table1_footnote.txt"))

# ==== 4-2. 고요산혈증 유병률: 성별 × 홍조 패턴 × 음주 수준 =================
prev <- rbindlist(lapply(c("남자", "여자"), function(s) rbindlist(lapply(levels(d_sub$flush_grp), function(g)
  rbindlist(lapply(levels(d_sub$drink3), function(k) {
    dd <- d_sub[f_sex == s & flush_grp == g & drink3 == k]
    if (nrow(dd) < 2) return(data.table(sex = s, flush = g, drink = k, n = nrow(dd), n_hua = sum(dd$hua), pct = NA_real_, se = NA_real_))
    e <- svymean(~hua, subset(sub, f_sex == s & flush_grp == g & drink3 == k))
    data.table(sex = s, flush = g, drink = k, n = nrow(dd), n_hua = sum(dd$hua),
               pct = round(100 * coef(e), 1), se = round(100 * SE(e)[1], 1))
  }))))))
fwrite(prev, file.path(out_dir, "05b_prevalence_by_sex_flush_drink.csv"), bom = TRUE)

# ==== (본 분석) 회귀모형 =====================================================
covs <- c("age", "f_year", "he_bmi", "egfr10", "f_htn", "f_dm", "f_smk", "f_incm", "f_edu", "f_pa")
# 설계 객체 안의 변수로 완전 응답자를 정한 뒤 subset 해서 적합한다.
fit_svy <- function(design, form, fam) {
  vars <- all.vars(form)
  dv <- as.data.table(design$variables)
  design$variables$.cc <- complete.cases(dv[, vars, with = FALSE])
  ds <- subset(design, .cc)
  list(fit = svyglm(form, design = ds, family = fam), n = sum(design$variables$.cc), data = dv[design$variables$.cc])
}
tidy <- function(fit, pat, or = TRUE) {
  cf <- coef(fit); ci <- confint(fit); rows <- grep(pat, names(cf))
  tr <- if (or) exp else identity
  p  <- summary(fit)$coefficients[rows, ncol(summary(fit)$coefficients)]
  data.table(term = names(cf)[rows], est = tr(cf[rows]), ci_low = tr(ci[rows, 1]), ci_high = tr(ci[rows, 2]), p = p)
}
outs <- list(list(y = "hua", fam = quasibinomial(), or = TRUE, lab = "고요산혈증(OR)"),
             list(y = "uacid", fam = gaussian(), or = FALSE, lab = "요산(mg/dL 차이)"))

# (A) 성별·홍조 패턴별 층화: 음주 수준(기준 월1회 미만)의 연관성
resA <- rbindlist(lapply(outs, function(o) rbindlist(lapply(c("남자", "여자"), function(s) {
  cv <- if (s == "여자") c(covs, "f_meno") else covs
  rbindlist(lapply(levels(d_sub$flush_grp), function(g) {
    ds <- subset(sub, f_sex == s & flush_grp == g)
    r  <- fit_svy(ds, as.formula(paste(o$y, "~ drink3 +", paste(cv, collapse = " + "))), o$fam)
    tb <- tidy(r$fit, "^drink3", o$or)
    tb[, n_cat := as.integer(table(r$data$drink3)[sub("^drink3", "", term)])]
    cbind(outcome = o$lab, sex = s, flush = g, n_model = r$n,
          n_ref = as.integer(table(r$data$drink3)["월1회 미만"]), tb)
  }))
}))))
# 상호작용 검정(성별 내): 음주 수준 × 홍조 패턴
intA <- rbindlist(lapply(outs, function(o) rbindlist(lapply(c("남자", "여자"), function(s) {
  cv <- if (s == "여자") c(covs, "f_meno") else covs
  r  <- fit_svy(subset(sub, f_sex == s), as.formula(paste(o$y, "~ drink3 * flush_grp +", paste(cv, collapse = " + "))), o$fam)
  tt <- regTermTest(r$fit, ~ drink3:flush_grp, method = "Wald")
  data.table(outcome = o$lab, sex = s, test = "음주 수준 × 홍조 패턴", n_model = r$n, p_interaction = as.numeric(tt$p))
}))))
resA[, `:=`(term = sub("^drink3", "", term))]
fwrite(resA, file.path(out_dir, "06a_drink_by_sex_flush.csv"), bom = TRUE)

# (B) 성별 내 홍조 패턴 자체의 연관성 (음주 수준 보정, 기준 비홍조)
resB <- rbindlist(lapply(outs, function(o) rbindlist(lapply(c("남자", "여자"), function(s) {
  cv <- if (s == "여자") c(covs, "f_meno") else covs
  r  <- fit_svy(subset(sub, f_sex == s), as.formula(paste(o$y, "~ flush_grp + drink3 +", paste(cv, collapse = " + "))), o$fam)
  tb <- tidy(r$fit, "^flush_grp", o$or)
  tb[, n_cat := as.integer(table(r$data$flush_grp)[sub("^flush_grp", "", term)])]
  cbind(outcome = o$lab, sex = s, n_model = r$n, n_ref = as.integer(table(r$data$flush_grp)["비홍조"]), tb)
}))))
resB[, term := sub("^flush_grp", "", term)]
fwrite(resB, file.path(out_dir, "06b_flush_pattern_by_sex.csv"), bom = TRUE)

# (C) 성별 차이 검정: 홍조 패턴별로 음주 수준 × 성별 상호작용, 그리고 3원 상호작용
resC <- rbindlist(lapply(outs, function(o) {
  r3 <- fit_svy(sub, as.formula(paste(o$y, "~ drink3 * flush_grp * f_sex +", paste(covs, collapse = " + "))), o$fam)
  t3 <- regTermTest(r3$fit, ~ drink3:flush_grp:f_sex, method = "Wald")
  by_g <- rbindlist(lapply(levels(d_sub$flush_grp), function(g) {
    r <- fit_svy(subset(sub, flush_grp == g), as.formula(paste(o$y, "~ drink3 * f_sex +", paste(covs, collapse = " + "))), o$fam)
    tt <- regTermTest(r$fit, ~ drink3:f_sex, method = "Wald")
    data.table(outcome = o$lab, test = paste0("음주 수준 × 성별 (", g, ")"), n_model = r$n, p_interaction = as.numeric(tt$p))
  }))
  rbind(by_g, data.table(outcome = o$lab, test = "음주 수준 × 홍조 패턴 × 성별 (3원)", n_model = r3$n, p_interaction = as.numeric(t3$p)))
}))
resC <- rbind(intA[, .(outcome, test = paste0(sex, ": ", test), n_model, p_interaction)], resC)
fwrite(resC, file.path(out_dir, "06c_interaction_tests.csv"), bom = TRUE)

# (D) 여성 특징: 폐경 전·후 × 현재 홍조 유무별 음주 수준의 연관성
resD <- rbindlist(lapply(outs, function(o) rbindlist(lapply(levels(d_sub$f_meno), function(m)
  rbindlist(lapply(levels(d_sub$f_flush_now), function(fn) {
    ds <- subset(sub, f_sex == "여자" & f_meno == m & f_flush_now == fn)
    r  <- fit_svy(ds, as.formula(paste(o$y, "~ drink3 +", paste(covs, collapse = " + "))), o$fam)
    tb <- tidy(r$fit, "^drink3", o$or)
    tb[, n_cat := as.integer(table(r$data$drink3)[sub("^drink3", "", term)])]
    n_hua <- sum(r$data$hua)
    cbind(outcome = o$lab, menopause = m, flush_now = fn, n_model = r$n, n_hua_events = n_hua,
          n_ref = as.integer(table(r$data$drink3)["월1회 미만"]), tb)
  }))))))
resD[, term := sub("^drink3", "", term)]
fwrite(resD, file.path(out_dir, "06d_women_menopause.csv"), bom = TRUE)

# ==== 민감도 분석 ===========================================================
# S1 통풍 진단자 포함, S2 주당 음주 잔 수(7잔/주 단위, 가정 B)로 음주 노출 교체 → 성별·홍조 패턴별 OR
sub_g  <- subset(des, inc_gout == 1)
sub_g9 <- subset(des, inc_gout9 == 1)
sens <- rbindlist(lapply(c("남자", "여자"), function(s) {
  cv <- if (s == "여자") c(covs, "f_meno") else covs
  rbindlist(lapply(levels(d_sub$flush_grp), function(g) {
    r1 <- fit_svy(subset(sub_g, f_sex == s & flush_grp == g), as.formula(paste("hua ~ drink3 +", paste(cv, collapse = " + "))), quasibinomial())
    t1 <- tidy(r1$fit, "^drink3고위험", TRUE)
    r2 <- fit_svy(subset(sub, f_sex == s & flush_grp == g), as.formula(paste("hua ~ drinks_wk7 +", paste(cv, collapse = " + "))), quasibinomial())
    t2 <- tidy(r2$fit, "^drinks_wk7$", TRUE)
    r3 <- fit_svy(subset(sub_g9, f_sex == s & flush_grp == g), as.formula(paste("hua ~ drink3 +", paste(cv, collapse = " + "))), quasibinomial())
    t3 <- tidy(r3$fit, "^drink3고위험", TRUE)
    rbind(cbind(analysis = "S1 통풍 진단자 포함: 고위험음주 vs 월1회 미만", sex = s, flush = g, n_model = r1$n, t1[, -1]),
          cbind(analysis = "S2 주당 7잔 증가당(가정 B)", sex = s, flush = g, n_model = r2$n, t2[, -1]),
          cbind(analysis = "S3 통풍 문항 모름·무응답 포함: 고위험음주 vs 월1회 미만", sex = s, flush = g, n_model = r3$n, t3[, -1]))
  }))
}))
fwrite(sens, file.path(out_dir, "07_sensitivity.csv"), bom = TRUE)

for (x in c("resA", "resB", "resC", "resD", "sens")) {
  z <- copy(get(x)); num <- intersect(c("est", "ci_low", "ci_high"), names(z))
  z[, (num) := lapply(.SD, round, 2), .SDcols = num]
  if ("p" %in% names(z)) z[, p := signif(p, 3)]
  if ("p_interaction" %in% names(z)) z[, p_interaction := signif(p_interaction, 3)]
  assign(x, z)
}
fwrite(resA, file.path(out_dir, "06a_drink_by_sex_flush.csv"), bom = TRUE)
fwrite(resB, file.path(out_dir, "06b_flush_pattern_by_sex.csv"), bom = TRUE)
fwrite(resC, file.path(out_dir, "06c_interaction_tests.csv"), bom = TRUE)
fwrite(resD, file.path(out_dir, "06d_women_menopause.csv"), bom = TRUE)
fwrite(sens, file.path(out_dir, "07_sensitivity.csv"), bom = TRUE)
cat("\n[A]\n"); print(resA); cat("\n[B]\n"); print(resB); cat("\n[C]\n"); print(resC); cat("\n[D]\n"); print(resD); cat("\n[S]\n"); print(sens)

# ==== 5-1. 공식 통계 대조표 =================================================
# 단년도 wt_itvex로 만 19세 이상 고위험음주율(지침서 정의, 8기 PDF p.48 / 인쇄 p.40)을 재현한다.
# 공식 조율값은 출처를 찾지 못해 NA. 국민건강통계 2019 표 번호·쪽과 함께 채워야 한다.
# 주의: 언론에 나오는 "2019년 고위험음주율 14.1%"는 지역사회건강조사(표준화, 시군구 중앙값) 값이라 대조에 쓸 수 없다.
#       e-나라지표·PHWR의 값은 2005년 추계인구 기준 연령표준화 값이라 조율 추정치와 비교하면 안 된다.
official <- list(
  list(indicator = "만19세 이상 고위험음주율(%), 2019, 조율(crude), 전체", year = 2019, sex_sel = NULL,
       ind = quote(dr_high), scale = 100, official = NA_real_,
       source = "지표 정의: 8기 PDF p.48 / 인쇄 p.40. 공식값 출처 미확인(국민건강통계 2019 조율값 필요)"),
  list(indicator = "만19세 이상 고위험음주율(%), 2019, 조율(crude), 남자", year = 2019, sex_sel = 1,
       ind = quote(dr_high), scale = 100, official = NA_real_,
       source = "지표 정의: 8기 PDF p.48 / 인쇄 p.40. 공식값 출처 미확인"),
  list(indicator = "만19세 이상 고위험음주율(%), 2019, 조율(crude), 여자", year = 2019, sex_sel = 2,
       ind = quote(dr_high), scale = 100, official = NA_real_,
       source = "지표 정의: 8기 PDF p.48 / 인쇄 p.40. 공식값 출처 미확인")
)
check <- rbindlist(lapply(official, function(o) {
  dd <- d[year == o$year]
  dd[, `:=`(pop_flag = as.integer(age >= 19 & !is.na(wt_itvex) &
                                  (is.null(o$sex_sel) | sex %in% o$sex_sel)), ind_val = eval(o$ind))]
  dd[, w1 := fifelse(!is.na(wt_itvex), wt_itvex, 0)]
  dz <- subset(svydesign(ids = ~psu, strata = ~kstrata, weights = ~w1, nest = TRUE, data = dd), pop_flag == 1)
  est <- svymean(~ind_val, dz, na.rm = TRUE); ci <- confint(est)
  data.table(indicator = o$indicator, n_unweighted = dd[pop_flag == 1 & !is.na(ind_val), .N],
             estimate = o$scale * unname(coef(est)), se = o$scale * as.numeric(SE(est)),
             ci_low = o$scale * unname(ci[1]), ci_high = o$scale * unname(ci[2]),
             official = o$official, source = o$source)
}))
check[, diff := estimate - official][, within_ci := official >= ci_low & official <= ci_high]
print(check)
if (any(is.na(check$official))) warning("공식 통계값이 비어 있습니다. 출처를 찾아 채우세요.")
fwrite(check, file.path(out_dir, "05_official_check.csv"), bom = TRUE)

# ==== 5-2. 재현성 기록 ======================================================
fwrite(data.table(file = files, size = file.size(files), md5 = unname(tools::md5sum(files))),
       file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "_sessionInfo.txt"))
cat("\n완료:", out_dir, "\n")
