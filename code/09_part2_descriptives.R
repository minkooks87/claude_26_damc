# 09_part2_descriptives.R
# 목적: 연구보고서 V3(2부: 연구 대상·음주량 정의) 기준으로 발표 v5의 예비 집계를 재현하고,
#       2부 질문 대비용 기술 수치(대상자 흐름, 칸별 인원, 음주량 분포)를 만든다.
# 원칙: 노출(음주 수준)·효과수정변수(홍조)별 요산 결과는 계산하지 않는다.
#       (발표 v5: "음주·홍조별 요산 결과는 분석 전까지 보지 않음")
#       요산은 성별 전체 유병률만 계산한다(발표 v5에 이미 있는 값의 재현).
# 자료: 국민건강영양조사 제8기 2019-2021 기본DB(hnYY_all). data 폴더는 읽기만 한다.
# 실행: Rscript --vanilla code/09_part2_descriptives.R   (프로젝트 폴더에서)
# 쪽수 표기: "8기 PDF 쪽 / 인쇄 쪽" (제8기(2019-2021) 원시자료 이용지침서)

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

years   <- c(2019, 2020, 2021)
out_dir <- file.path("output", "09_part2_prep")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
files   <- file.path("data", "국건영 조사결과", sprintf("hn%02d_all.sas7bdat", years %% 100))

# ---- 읽을 변수와 결측 코드 (지침서 확인) ----
# 음주 BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162 / 인쇄 p.153-154
# 홍조 BD9_1·BD9_2: 8기 PDF p.163 / 인쇄 p.155
# 통풍 DM8_dg: 8기 PDF p.137 / 인쇄 p.129, 요산·크레아티닌: 8기 PDF p.200 / 인쇄 p.192
# 고혈압 유병 HE_HP: 8기 PDF p.196 / 인쇄 p.188, 당뇨병 유병 HE_DM_HbA1c: 8기 PDF p.199 / 인쇄 p.191
vars <- c("id", "year", "sex", "age", "psu", "kstrata", "wt_itvex", "incm", "edu",
          "bd1_11", "bd2_1", "bd2_14", "bd9_1", "bd9_2", "he_uacid", "he_uacid_etc", "he_crea",
          "dm8_dg", "he_bmi", "di1_dg", "de1_dg", "he_hp", "he_dm_hba1c", "bs1_1", "bs3_1", "pa_aerobic")
read_year <- function(f) {
  nms <- names(read_sas(f, n_max = 0))
  x <- as.data.table(zap_labels(read_sas(f, col_select = all_of(nms[tolower(nms) %in% vars]))))
  setnames(x, tolower(names(x)))
  x
}
d <- rbindlist(lapply(files, read_year), use.names = TRUE, fill = TRUE)
n_raw <- nrow(d)

# 9·99·999 = 모름·무응답 → 결측. 8·888 = 비해당은 변수별로 처리(아래 주석)
for (v in c("bd1_11", "bd2_1", "bd9_1", "bd9_2", "dm8_dg", "di1_dg", "de1_dg", "incm", "edu")) d[get(v) == 9, (v) := NA]
d[bd2_14 %in% c(888, 999), bd2_14 := NA]   # 888 = 비해당(10잔 미만 응답자), 999 = 모름
d[bd9_1 == 8 | bd9_2 == 8, `:=`(bd9_1 = NA, bd9_2 = NA)]   # 비해당(비음주·청소년)은 홍조 없음으로 보지 않는다

# ---- 노출: 주당 음주잔수 (발표 v5·V3 환산표) ----
# 빈도: 월1회미만 0.5회/월, 월1회 1회/월, 월2-4회 3회/월, 주2-3회 2.5회/주, 주4회이상 5.5회/주 (월→주: ×12/52)
# 1회량: 1-2잔 1.5, 3-4잔 3.5, 5-6잔 5.5, 7-9잔 8, 10잔 이상은 BD2_14 기입값
freq_wk <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
amt     <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
d[, per_occ := fifelse(bd2_1 == 5, as.numeric(bd2_14), unname(amt[as.character(bd2_1)]))]
d[, drinks_wk := unname(freq_wk[as.character(bd1_11)]) * per_occ]
d[, drink3 := cut(drinks_wk, c(-Inf, 3.5, 14, Inf), right = FALSE, labels = c("저음주", "중음주", "고음주"))]

# ---- 효과수정변수: 현재 홍조(가끔 이상) / 홍조 패턴 ----
d[, flush_now := factor(fcase(bd9_2 == 1, "현재 비홍조", bd9_2 %in% 2:4, "현재 홍조"), c("현재 비홍조", "현재 홍조"))]
d[, flush_grp := factor(fcase(
  bd9_1 == 2 & bd9_2 == 1,     "비홍조",
  bd9_1 == 1 & bd9_2 == 1,     "홍조 소실",
  bd9_1 == 1 & bd9_2 %in% 2:4, "지속 홍조",
  bd9_1 == 2 & bd9_2 %in% 2:4, "신규 홍조"), c("비홍조", "홍조 소실", "지속 홍조", "신규 홍조"))]

# ---- 결과: 고요산혈증 (V3: 남 ≥7.0, 여 ≥6.0 mg/dL; 지침서에 정의 없음 → 연구의 조작적 정의) ----
#   요산 1.0 미만(HE_Uacid_etc "<1.0", HE_Uacid 결측)은 발표 v5처럼 결측으로 둔다(8기 PDF p.200 / 인쇄 p.192).
#   0.9로 넣으면 4명이 늘어난다(아래 점검 출력) → 결정이 필요한 항목으로 문서에 적는다.
d[, uacid := he_uacid]
d[, uacid_lt1 := is.na(he_uacid) & trimws(as.character(he_uacid_etc)) == "<1.0"]
d[, hua := as.integer(uacid >= fifelse(sex == 1, 7.0, 6.0))]

# ---- 공변량 ----
# eGFR: V3는 CKD-EPI 2009식 (지침서에 계산식 없음, 문헌식)
d[, egfr09 := 141 * pmin(he_crea / fifelse(sex == 2, 0.7, 0.9), 1)^fifelse(sex == 2, -0.329, -0.411) *
              pmax(he_crea / fifelse(sex == 2, 0.7, 0.9), 1)^(-1.209) * 0.993^age * fifelse(sex == 2, 1.018, 1)]
# 현재흡연: 주요지표 SAS 정의 (8기 PDF p.48 / 인쇄 p.40)
d[, smk := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3, smk := as.integer(bs1_1 == 2 & bs3_1 %in% c(1, 2))]
cov_base <- c("age", "edu", "incm", "year", "smk", "pa_aerobic")
cov_dx   <- c(cov_base, "he_bmi", "di1_dg", "de1_dg", "egfr09")        # 가정 A: 고혈압·당뇨병 = 의사진단
cov_prev <- c(cov_base, "he_bmi", "he_hp", "he_dm_hba1c", "egfr09")    # 가정 B: 고혈압·당뇨병 = 국건영 유병 변수
complete <- function(cols) d[, Reduce(`&`, lapply(.SD, function(x) !is.na(x))), .SDcols = cols]

# ==== 1. 대상자 흐름 (행 삭제 없이 표시) ======================================
steps <- list(
  "2019-2021 기본DB 전체"                 = rep(TRUE, n_raw),
  "만 19세 이상"                          = d$age >= 19,
  "최근 1년 음주자(BD1_11 = 2-6)"         = d$bd1_11 %in% 2:6,
  "음주량·과거/현재 홍조·요산 확보"       = !is.na(d$drinks_wk) & !is.na(d$flush_grp) & !is.na(d$uacid),
  "통풍 의사진단 없음(진단·모름 제외)"    = d$dm8_dg %in% 0
)
keep <- rep(TRUE, n_raw); flow <- NULL
for (nm in names(steps)) {
  keep <- keep & !is.na(steps[[nm]]) & steps[[nm]]
  flow <- rbind(flow, data.table(step = nm, n = sum(keep), n_men = sum(keep & d$sex == 1), n_women = sum(keep & d$sex == 2)))
}
pre_gout <- Reduce(`&`, steps[1:4]); pre_gout[is.na(pre_gout)] <- FALSE
gout_dx  <- sum(pre_gout & d$dm8_dg %in% 1); gout_unk <- sum(pre_gout & is.na(d$dm8_dg))
base_keep <- keep
for (alt in list(list("보정변수 모두 있음(가정 A: 고혈압·당뇨병 의사진단)", cov_dx),
                 list("보정변수 모두 있음(가정 B: HE_HP·HE_DM_HbA1c 유병)", cov_prev))) {
  k <- base_keep & complete(alt[[2]])
  flow <- rbind(flow, data.table(step = alt[[1]], n = sum(k), n_men = sum(k & d$sex == 1), n_women = sum(k & d$sex == 2)))
}
flow[, excluded := c(NA, diff(-n))]
flow[6:7, excluded := flow$n[5] - n]
fwrite(flow, file.path(out_dir, "01_flow_v3.csv"), bom = TRUE)
cat("통풍 진단", gout_dx, "/ 통풍 모름·무응답", gout_unk, "\n")
print(flow)

# 주 분석 대상: 발표 v5 인원(10,779명)에 가까운 가정 B를 쓴다 (가정 A는 비교용)
d[, inc := as.integer(base_keep & complete(cov_prev))]
d[, incA := as.integer(base_keep & complete(cov_dx))]
# 발표 v5와 차이 점검: 요산 1.0 미만(HE_Uacid_etc "<1.0")을 0.9로 넣은 인원
cat("요산 <1.0 표기로 결측 처리된 인원(음주자·홍조 확보 단계):",
    d[age >= 19 & bd1_11 %in% 2:6 & !is.na(drinks_wk) & !is.na(flush_grp) & uacid_lt1 == TRUE, .N], "\n")

# ==== 2. 칸별 인원 (비가중 n, 요산 결과 없음) =================================
f_sex <- function(x) factor(x, 1:2, c("남", "여"))
cell_now <- dcast(d[inc == 1, .N, by = .(성별 = f_sex(sex), 현재홍조 = flush_now, drink3)], 성별 + 현재홍조 ~ drink3, value.var = "N", fill = 0)
cell_pat <- dcast(d[inc == 1, .N, by = .(성별 = f_sex(sex), 홍조패턴 = flush_grp, drink3)], 성별 + 홍조패턴 ~ drink3, value.var = "N", fill = 0)
pat_tab  <- dcast(d[inc == 1, .N, by = .(성별 = f_sex(sex), 홍조패턴 = flush_grp)], 홍조패턴 ~ 성별, value.var = "N")
fwrite(cell_now, file.path(out_dir, "02_cells_current_flush.csv"), bom = TRUE)
fwrite(cell_pat, file.path(out_dir, "03_cells_flush_pattern.csv"), bom = TRUE)
fwrite(pat_tab,  file.path(out_dir, "04_flush_pattern_n.csv"), bom = TRUE)
print(cell_now); print(cell_pat); print(pat_tab)

# 빈도 × 1회량 격자: 주당 잔수와 칸별 인원(남녀)
grid <- d[inc == 1, .(n = .N, drinks_wk = round(mean(drinks_wk), 2)), by = .(sex, bd1_11, bd2_1)]
fwrite(grid[order(sex, bd1_11, bd2_1)], file.path(out_dir, "05_grid_freq_amount.csv"), bom = TRUE)

# ==== 3. 가중 기술통계 (요산은 성별 전체 유병률만) ============================
d[, wt := wt_itvex / 3]   # 8기 3개년 통합: 각 연도 1/3 (8기 PDF p.36 / 인쇄 p.28)
d[is.na(wt), wt := 0]
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt, nest = TRUE, data = d)
s   <- subset(des, inc == 1)
res <- list()
for (sx in 1:2) {
  ss <- subset(s, sex == sx)
  m  <- svyby(~drinks_wk, ~flush_now, ss, svymean, na.rm = TRUE)
  res[[length(res) + 1]] <- data.table(성별 = c("남", "여")[sx], 지표 = paste0("주당 음주잔수 가중평균: ", m$flush_now),
                                       값 = round(coef(m), 1), SE = round(SE(m), 2),
                                       비가중n = as.vector(table(d[inc == 1 & sex == sx, flush_now])))
  for (lab in c("현재 홍조 %", "고음주(≥14잔) %", "고요산혈증 %")) {
    f <- switch(lab, "현재 홍조 %" = ~I(flush_now == "현재 홍조"), "고음주(≥14잔) %" = ~I(drink3 == "고음주"), "고요산혈증 %" = ~I(hua == 1))
    e <- svymean(f, ss, na.rm = TRUE)
    cnt <- switch(lab, "현재 홍조 %" = d[inc == 1 & sex == sx & flush_now == "현재 홍조", .N],
                  "고음주(≥14잔) %" = d[inc == 1 & sex == sx & drink3 == "고음주", .N],
                  "고요산혈증 %" = d[inc == 1 & sex == sx & hua == 1, .N])
    res[[length(res) + 1]] <- data.table(성별 = c("남", "여")[sx], 지표 = lab, 값 = round(100 * coef(e)[2], 1),
                                         SE = round(100 * SE(e)[2], 2), 비가중n = cnt)
  }
  q <- svyquantile(~drinks_wk, ss, c(0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  res[[length(res) + 1]] <- data.table(성별 = c("남", "여")[sx], 지표 = paste0("주당 음주잔수 가중 분위수 ", c("25%", "50%", "75%", "90%")),
                                       값 = round(as.vector(coef(q)), 2), SE = NA, 비가중n = d[inc == 1 & sex == sx, .N])
}
desc <- rbindlist(res)
fwrite(desc, file.path(out_dir, "06_weighted_descriptives.csv"), bom = TRUE)
print(desc)

# 음주량 분포(그림용, 노출만): 성별 × 현재 홍조별 주당 잔수의 가중 비율(구간)
brks <- c(0, 1, 2, 3.5, 7, 10.5, 14, 21, 28, 42, Inf)
d[, dbin := cut(drinks_wk, brks, right = FALSE)]
dist <- d[inc == 1, .(w = sum(wt), n = .N), by = .(sex, flush_now, dbin)][, pct := 100 * w / sum(w), by = .(sex, flush_now)]
fwrite(dist[order(sex, flush_now, dbin)], file.path(out_dir, "07_drinks_distribution.csv"), bom = TRUE)

writeLines(c(paste("실행:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), paste("R:", R.version.string),
             paste("입력 행 수:", n_raw), paste("최종 대상(가정 B, 주):", sum(d$inc), " / 가정 A:", sum(d$incA))),
           file.path(out_dir, "_run_info.txt"))
