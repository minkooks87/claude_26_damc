# 02_meal_bedtime_cardiometabolic.R
# 주제: 청년층(19-39세)에서 마지막 식사-취침 간격이 짧을수록 심혈관대사 위험 요인
#       (공복혈당, 중성지방, 허리둘레, 대사증후군)이 나쁜가
# 자료: 국민건강영양조사 2016, 2017, 2018(제7기), 2021(제8기), 2024(제9기)
#       기본DB(hnYY_all) + 식품섭취조사(hnYY_24rc)
# 실행: Rscript --vanilla code/02_meal_bedtime_cardiometabolic.R   (프로젝트 폴더에서)
# 원칙: data 폴더는 읽기만 한다. 분석 대상은 행을 지우지 않고 inc(1/0)로 표시한다.
#       추정은 svydesign(가중치·층·집락) → subset 으로 한다. (knhanes-analysis 스킬)
#
# 지침서 약칭: 7기 = 제7기(2016-2018) 이용지침서, 8기 = 제8기(2019-2021), 9기 = 제9기(2022-2024)
#              쪽수는 "PDF 쪽 / 인쇄 쪽"

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
years   <- c(2016, 2017, 2018, 2021, 2024)
out_dir <- file.path("output", "02_meal_bedtime")
dat_dir <- file.path("data", "국건영 조사결과")
files_all <- file.path(dat_dir, sprintf("hn%02d_all.sas7bdat",  years %% 100))
files_rc  <- file.path(dat_dir, sprintf("hn%02d_24rc.sas7bdat", years %% 100))

# 가중치: 검진(혈액·계측)과 영양조사(24시간 회상) 변수를 함께 쓰므로 연관성분석 가중치 wt_tot
#   근거: 7기 PDF p.38 / 인쇄 p.30, 8기 PDF p.40 / 인쇄 p.32, 9기 PDF p.42 / 인쇄 p.34
#   (프로젝트 규칙 CLAUDE.md의 wt_itvex는 민감도 분석 S1로 함께 보고한다)
# 통합비율: 서로 다른 기수를 합치므로 "조사기간 비례" 통합비율을 쓴다.
#   근거: 7기 PDF p.35 / 인쇄 p.27(표 8), 9기 PDF p.47 / 인쇄 p.39(표 15, 예: 3개년 통합 wt_itvex/3)
#   5개 연도 모두 1년 조사 → 각 연도 값 1, 소계 5 → 통합비율 1/5
#   (가정: 2021·2024년 조사기간을 1년으로 봄. 1년 미만이라는 기술은 지침서에서 찾지 못함)
pool_ratio <- c("2016" = 1/5, "2017" = 1/5, "2018" = 1/5, "2021" = 1/5, "2024" = 1/5)
stopifnot(abs(sum(pool_ratio) - 1) < 1e-12)

# 24시간 회상 준거일: 조사일(N_DAY) 기준 며칠 전 섭취인가
#   2016-2018: 조사 1일전 (7기 PDF p.13 / 인쇄 p.5), 2021: 조사 1일전 (8기 PDF p.13 / 인쇄 p.5)
#   2024: 조사 2일전, 제9기(2022)부터 1일전→2일전 변경 (9기 PDF p.14 / 인쇄 p.6, PDF p.23 / 인쇄 p.15)
recall_lag <- c("2016" = 1, "2017" = 1, "2018" = 1, "2021" = 1, "2024" = 2)

# ==== 1. 변수 정의 (지침서에서 확인한 내용만 적는다) ========================
src_basic <- "7기 PDF p.93-94 / 인쇄 p.85-86; 9기 PDF p.101-102 / 인쇄 p.93-94"
src_sleep <- "7기 PDF p.169 / 인쇄 p.161; 8기 PDF p.169 / 인쇄 p.161; 9기 PDF p.158 / 인쇄 p.150; 시각 변환 규칙 7기 PDF p.46 / 인쇄 p.38, 8기 PDF p.48 / 인쇄 p.40, 9기 PDF p.50 / 인쇄 p.42"
src_lab   <- "7기 PDF p.198 / 인쇄 p.190; 8기 PDF p.199 / 인쇄 p.191; 9기 PDF p.188 / 인쇄 p.180"
src_rc    <- "7기 PDF p.299 / 인쇄 p.291; 8기 PDF p.287 / 인쇄 p.279; 9기 PDF p.234 / 인쇄 p.226"
src_ind   <- "주요지표 SAS: 7기 PDF p.46 / 인쇄 p.38; 8기 PDF p.48 / 인쇄 p.40; 9기 PDF p.50 / 인쇄 p.42"
var_dict <- rbindlist(list(
  list(var = "id",        file = "all,24rc", levels = "", miss = "", na8 = "", label = "개인 아이디", source = src_basic),
  list(var = "year",      file = "all", levels = "", miss = "", na8 = "", label = "조사연도", source = src_basic),
  list(var = "sex",       file = "all", levels = "1=남자;2=여자", miss = "", na8 = "", label = "성별", source = src_basic),
  list(var = "age",       file = "all", levels = "1~79=세;80=80세이상(탑코딩)", miss = "", na8 = "", label = "만나이", source = src_basic),
  list(var = "psu",       file = "all", levels = "", miss = "", na8 = "", label = "조사구번호(집락)", source = src_basic),
  list(var = "kstrata",   file = "all", levels = "", miss = "", na8 = "", label = "분산추정 층", source = src_basic),
  list(var = "wt_tot",    file = "all", levels = "", miss = "", na8 = "", label = "건강설문-검진조사 & 영양조사 가중치(본 분석)", source = src_basic),
  list(var = "wt_itvex",  file = "all", levels = "", miss = "", na8 = "", label = "건강설문-검진조사 가중치(민감도 S1)", source = src_basic),
  list(var = "incm",      file = "all", levels = "1=하;2=중하;3=중상;4=상", miss = "", na8 = "", label = "소득 4분위수(개인)", source = src_basic),
  list(var = "edu",       file = "all", levels = "1=초졸이하;2=중졸;3=고졸;4=대졸이상", miss = "", na8 = "", label = "교육수준 재분류", source = src_basic),
  list(var = "he_prg",    file = "all", levels = "0=아니오;1=예", miss = "", na8 = "8=범주유지(비해당-남자)", label = "임신여부",
       source = "7기 PDF p.192 / 인쇄 p.184; 8기 PDF p.192 / 인쇄 p.184; 9기 PDF p.182 / 인쇄 p.174"),
  list(var = "de1_dg",    file = "all", levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년·소아 비해당, 성인엔 해당 없음)", label = "당뇨병 의사진단",
       source = "7기 PDF p.112 / 인쇄 p.104; 8기 PDF p.112 / 인쇄 p.104; 9기 PDF p.112-113 / 인쇄 p.104-105"),
  list(var = "di1_dg",    file = "all", levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년·소아 비해당)", label = "고혈압 의사진단",
       source = "7기 PDF p.103 / 인쇄 p.95; 8기 PDF p.100 / 인쇄 p.92; 9기 PDF p.110 / 인쇄 p.102"),
  list(var = "di2_dg",    file = "all", levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년·소아 비해당)", label = "이상지질혈증 의사진단",
       source = "7기 PDF p.104 / 인쇄 p.96; 8기 PDF p.101 / 인쇄 p.93; 9기 PDF p.111 / 인쇄 p.103"),
  list(var = "bp16_11",   file = "all", levels = "시", miss = "99", na8 = "88=결측(소아)", label = "주중 잠자리에 든 시각_시", source = src_sleep),
  list(var = "bp16_12",   file = "all", levels = "분", miss = "99", na8 = "88=결측(소아)", label = "주중 잠자리에 든 시각_분", source = src_sleep),
  list(var = "bp16_13",   file = "all", levels = "시", miss = "99", na8 = "88=결측(소아)", label = "주중 일어난 시각_시", source = src_sleep),
  list(var = "bp16_14",   file = "all", levels = "분", miss = "99", na8 = "88=결측(소아)", label = "주중 일어난 시각_분", source = src_sleep),
  list(var = "bp16_21",   file = "all", levels = "시", miss = "99", na8 = "88=결측(소아)", label = "주말 잠자리에 든 시각_시", source = src_sleep),
  list(var = "bp16_22",   file = "all", levels = "분", miss = "99", na8 = "88=결측(소아)", label = "주말 잠자리에 든 시각_분", source = src_sleep),
  list(var = "bp16_23",   file = "all", levels = "시", miss = "99", na8 = "88=결측(소아)", label = "주말 일어난 시각_시", source = src_sleep),
  list(var = "bp16_24",   file = "all", levels = "분", miss = "99", na8 = "88=결측(소아)", label = "주말 일어난 시각_분", source = src_sleep),
  list(var = "he_fst",    file = "all", levels = "시간", miss = "", na8 = "", label = "공복시간",
       source = "7기 PDF p.192 / 인쇄 p.184; 8기 PDF p.192 / 인쇄 p.184; 9기 PDF p.182 / 인쇄 p.174"),
  list(var = "he_glu",    file = "all", levels = "mg/dL", miss = "", na8 = "", label = "공복혈당(공복 8시간 이상만 사용: 7기 PDF p.49 / 인쇄 p.41)", source = src_lab),
  list(var = "he_tg",     file = "all", levels = "mg/dL", miss = "", na8 = "", label = "중성지방(공복 12시간 이상만 사용: 7기 PDF p.50 / 인쇄 p.42, 9기 PDF p.52 / 인쇄 p.44)", source = src_lab),
  list(var = "he_hdl_st2",file = "all", levels = "mg/dL", miss = "", na8 = "", label = "HDL-콜레스테롤(보정값)", source = src_lab),
  list(var = "he_wc",     file = "all", levels = "cm", miss = "", na8 = "", label = "허리둘레",
       source = "7기 PDF p.197 / 인쇄 p.189; 8기 PDF p.198 / 인쇄 p.190; 9기 PDF p.187 / 인쇄 p.179"),
  list(var = "he_sbp",    file = "all", levels = "mmHg", miss = "", na8 = "", label = "최종 수축기 혈압(2,3차 평균)",
       source = "7기 PDF p.196 / 인쇄 p.188; 8기 PDF p.196 / 인쇄 p.188; 9기 PDF p.185 / 인쇄 p.177; 2021년부터 성인 진동형 혈압계(8기 PDF p.18-19 / 인쇄 p.10-11, 9기 PDF p.20 / 인쇄 p.12)"),
  list(var = "he_dbp",    file = "all", levels = "mmHg", miss = "", na8 = "", label = "최종 이완기 혈압(2,3차 평균)",
       source = "7기 PDF p.196 / 인쇄 p.188; 8기 PDF p.196 / 인쇄 p.188; 9기 PDF p.185 / 인쇄 p.177"),
  list(var = "bs1_1",     file = "all", levels = "현재흡연 정의에만 사용", miss = "", na8 = "", label = "평생 흡연 여부", source = src_ind),
  list(var = "bs3_1",     file = "all", levels = "현재흡연 정의에만 사용", miss = "", na8 = "", label = "현재 흡연 여부", source = src_ind),
  list(var = "bd1",       file = "all", levels = "월간음주 정의에만 사용", miss = "", na8 = "", label = "평생 음주 경험", source = src_ind),
  list(var = "bd1_11",    file = "all", levels = "월간음주 정의에만 사용", miss = "", na8 = "", label = "1년간 음주 빈도", source = src_ind),
  list(var = "pa_aerobic",file = "all", levels = "0=미실천;1=실천", miss = "", na8 = "", label = "유산소 신체활동 실천",
       source = "7기 PDF p.185 / 인쇄 p.177; 8기 PDF p.188 / 인쇄 p.180; 9기 PDF p.177 / 인쇄 p.169"),
  list(var = "ec_wht_5",  file = "all", levels = "1=주간;2=저녁;3=밤;4=주야간 규칙적 교대;5=24시간 교대;6=분할;7=불규칙 교대;8=기타", miss = "99",
       na8 = "88=범주유지(최근 1년 일하지 않음)", label = "변형근로시간(민감도 S3)",
       source = "7기 PDF p.157 / 인쇄 p.149; 8기 PDF p.155 / 인쇄 p.147; 9기 PDF p.144 / 인쇄 p.136"),
  list(var = "n_day",     file = "24rc", levels = "월요일~일요일(문자)", miss = "", na8 = "", label = "영양조사를 실시한 요일", source = src_rc),
  list(var = "n_meal",    file = "24rc", levels = "1=아침;2=점심;3=저녁;4=간식;5=식이보충제(2016-2017만)", miss = "", na8 = "", label = "끼니 구분", source = src_rc),
  list(var = "n_meal_t",  file = "24rc", levels = "HHMM(24시간제)", miss = "8881;8882;8883", na8 = "",
       label = "식사 시간(8881 오전·8882 오후 시각 모름, 8883 모름)", source = src_rc),
  list(var = "nf_en",     file = "24rc", levels = "kcal", miss = "", na8 = "", label = "식품별 에너지 섭취량",
       source = "7기 PDF p.303 / 인쇄 p.295; 8기 PDF p.291 / 인쇄 p.283; 9기 PDF p.238 / 인쇄 p.230")
))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(var_dict, file.path(out_dir, "01_variable_definitions.csv"), bom = TRUE)

# ---- 자료 읽기: 필요한 변수만, 대소문자 무시 ----
read_cols <- function(f, vars) {
  nms  <- names(read_sas(f, n_max = 0))
  want <- nms[tolower(nms) %in% vars]
  miss <- setdiff(vars, tolower(nms))
  if (length(miss)) message(basename(f), "에 없는 변수: ", paste(miss, collapse = ", "))
  x <- as.data.table(zap_labels(read_sas(f, col_select = all_of(want))))
  setnames(x, tolower(names(x)))
  x
}
v_all <- var_dict[grepl("all", file), var]
v_rc  <- var_dict[grepl("24rc", file), var]
d <- rbindlist(lapply(files_all, read_cols, vars = v_all), use.names = TRUE, fill = TRUE)
r <- rbindlist(Map(function(f, y) cbind(read_cols(f, v_rc), year = y), files_rc, years), use.names = TRUE, fill = TRUE)
n_raw <- nrow(d)

# ---- 결측·비해당 코드 처리 (var_dict 기준, 기본DB 변수) ----
split_codes <- function(s) if (nzchar(s)) trimws(strsplit(s, ";")[[1]]) else character(0)
for (i in which(var_dict$var %in% names(d) & var_dict$file == "all")) {
  v <- var_dict$var[i]
  mc <- split_codes(var_dict$miss[i])
  if (length(mc)) d[get(v) %in% as.numeric(mc), (v) := NA]
  for (rule in split_codes(var_dict$na8[i])) {
    code <- as.numeric(sub("=.*", "", rule)); to <- sub("^[^=]*=", "", rule)
    if (startsWith(to, "결측")) d[get(v) == code, (v) := NA]
  }
}
stopifnot(nrow(d) == n_raw)

# ==== 1-2. 24시간 회상 → 사람별 마지막 섭취 시각 =============================
# (1) 식이보충제(N_MEAL=5, 2016-2017만) 제외 (7기 PDF p.299 / 인쇄 p.291)
r <- r[is.na(n_meal) | n_meal != 5]
r[, mt := suppressWarnings(as.integer(n_meal_t))]
r[, t_unknown := is.na(mt) | mt >= 8881]                 # 8881-8883: 시각 모름
r[, t_h := (mt %/% 100) + (mt %% 100) / 60]
# (2) 가정 A: 00:00-04:59 섭취는 지침서에 하루 경계 정의가 없어 확인 불가.
#     자료상 이 시간대 기록 다수가 '저녁'·'간식'으로 분류되어 있어, 아침(1)이 아니면
#     회상일 밤 자정 이후 섭취로 보고 24시간을 더한다. → 민감도 분석 S2에서 해당자 제외.
r[, after_mid := !t_unknown & t_h < 5 & n_meal != 1]
r[after_mid == TRUE, t_h := t_h + 24]
# (3) 끼니(같은 끼니·같은 시각) 단위 에너지 합. 에너지 0인 끼니(물 등)는 '섭취'로 보지 않는다.
occ <- r[, .(en = sum(nf_en, na.rm = TRUE), t_unknown = any(t_unknown), after_mid = any(after_mid)),
         by = .(id, year, n_meal, n_meal_t, t_h)]
per <- occ[, .(
  energy_kcal   = sum(en),
  any_t_unknown = any(t_unknown & en > 0),
  any_after_mid = any(after_mid & en > 0),
  last_t        = if (any(en > 0 & !t_unknown)) max(t_h[en > 0 & !t_unknown]) else NA_real_
), by = .(id, year)]
# 조사요일(문자) → 회상일 요일. 0=일 … 6=토
wd <- c("일요일" = 0, "월요일" = 1, "화요일" = 2, "수요일" = 3, "목요일" = 4, "금요일" = 5, "토요일" = 6)
dayinfo <- unique(r[, .(id, year, n_day = trimws(n_day))])[, .SD[1], by = .(id, year)]
dayinfo[, survey_wd := unname(wd[n_day])]
dayinfo[, recall_wd := (survey_wd - recall_lag[as.character(year)]) %% 7]
per <- merge(per, dayinfo[, .(id, year, recall_wd)], by = c("id", "year"), all.x = TRUE)
d <- merge(d, per, by = c("id", "year"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == n_raw)

# ==== 1-3. 노출·결과·공변량 만들기 ===========================================
# 시각 변환: 1-12시는 +24 (지침서 SAS 규칙, 7기 PDF p.46 / 인쇄 p.38)
to_clock <- function(h, m) ifelse(h >= 1 & h <= 12, h + 24, h) + m / 60
d[, `:=`(bed_wk = to_clock(bp16_11, bp16_12), wake_wk = to_clock(bp16_13, bp16_14),
         bed_we = to_clock(bp16_21, bp16_22), wake_we = to_clock(bp16_23, bp16_24))]
# 회상일 밤 취침: 일-목 밤 → 주중 취침, 금·토 밤 → 주말 취침
#   (주말 = "일하지 않는 날 전날" 정의: 8기 PDF p.169 / 인쇄 p.161, BP16_2 설명)
d[, recall_weekend := recall_wd %in% c(5, 6)]
d[, bed_m  := fifelse(recall_weekend, bed_we,  bed_wk)]
d[, wake_m := fifelse(recall_weekend, wake_we, wake_wk)]
d[, sleep_h := wake_m - bed_m]                            # 지침서 수면시간 계산과 같은 방식
d[sleep_h <= 0 | sleep_h > 16, sleep_h := NA]             # 계산상 불가능한 값은 결측(가정)
d[, interval_h := bed_m - last_t]                         # 마지막 섭취 → 취침 (시간)
d[, interval_cat := cut(interval_h, c(-Inf, 1, 2, 3, Inf), right = FALSE,
                        labels = c("<1시간", "1-<2시간", "2-<3시간", "3시간 이상"))]
d[, interval_cat := relevel(interval_cat, ref = "3시간 이상")]
d[, trend_score := c("3시간 이상" = 0, "2-<3시간" = 1, "1-<2시간" = 2, "<1시간" = 3)[as.character(interval_cat)]]

# 결과 변수 (공복 조건은 지침서 지표 정의를 따름)
d[, glu  := fifelse(he_fst >= 8,  he_glu, NA_real_)]      # 7기 PDF p.49 / 인쇄 p.41
d[, tg   := fifelse(he_fst >= 12, he_tg,  NA_real_)]      # 7기 PDF p.50 / 인쇄 p.42
d[, ln_tg := log(tg)]
d[, wc   := he_wc]
# 대사증후군: 지침서에 정의 없음 → 문헌 정의(NCEP-ATP III 개정, 허리둘레는 한국인 기준 남 90·여 85cm) 사용(가정)
#   분석 대상은 진단자를 제외했으므로 약물 기준은 쓰지 않는다. 공복 12시간 이상(TG 기준)만 판정.
mets_fun <- function(sex, wc, tg, hdl, sbp, dbp, glu12) {
  k <- (wc >= fifelse(sex == 1, 90, 85)) + (tg >= 150) + (hdl < fifelse(sex == 1, 40, 50)) +
       (sbp >= 130 | dbp >= 85) + (glu12 >= 100)
  as.integer(k >= 3)
}
d[, glu12 := fifelse(he_fst >= 12, he_glu, NA_real_)]
d[, mets := mets_fun(sex, he_wc, tg, he_hdl_st2, he_sbp, he_dbp, glu12)]
# 민감도 S4: 2021·2024년 성인 혈압을 청진형 기준으로 전환 (9기 PDF p.185 / 인쇄 p.177)
d[, pp := he_sbp - he_dbp]
d[, sbp_cv := fifelse(year %in% c(2021, 2024), 10.773 + 0.771 * he_sbp + 0.039 * age + 0.374 * pp, he_sbp)]
d[, dbp_cv := fifelse(year %in% c(2021, 2024), 13.480 + 0.952 * he_dbp - 0.051 * age - 0.098 * pp, he_dbp)]
d[, mets_cv := mets_fun(sex, he_wc, tg, he_hdl_st2, sbp_cv, dbp_cv, glu12)]

# 공변량: 현재흡연·월간음주는 지침서 주요지표 SAS 정의 그대로 (7기 PDF p.46 / 인쇄 p.38)
d[, smk := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3, smk := as.integer(bs1_1 == 2 & bs3_1 %in% c(1, 2))]
d[, drk := NA_integer_]
d[bd1 %in% c(1, 2) & bd1_11 %in% c(1, 2, 3, 4, 5, 6, 8), drk := as.integer(bd1 == 2 & bd1_11 %in% c(3, 4, 5, 6))]
d[, nonday_work := fifelse(ec_wht_5 %in% 2:7, 1L, fifelse(ec_wht_5 %in% c(1, 8, 88), 0L, NA_integer_))]
d[, energy100 := energy_kcal / 100]
d[, `:=`(
  f_sex  = factor(sex, 1:2, c("남자", "여자")),
  f_year = factor(year),
  f_incm = factor(incm, 1:4, c("하", "중하", "중상", "상")),
  f_edu  = factor(pmin(pmax(edu, 2), 4), 2:4, c("중졸이하", "고졸", "대졸이상")),   # 19-39세는 초졸이하가 드물어 중졸과 합침
  f_smk  = factor(smk, 0:1, c("비흡연", "현재흡연")),
  f_drk  = factor(drk, 0:1, c("월1회 미만", "월1회 이상")),
  f_pa   = factor(pa_aerobic, 0:1, c("미실천", "실천")),
  f_mets = factor(mets, 0:1, c("없음", "있음"))
)]
stopifnot(nrow(d) == n_raw)

# ==== 2. 분석 대상 표시 (행 삭제 없음) ======================================
criteria <- list(
  "연관성분석 가중치 있음(검진·설문·영양조사 참여, wt_tot)" = quote(!is.na(wt_tot)),
  "만 19-39세"                                              = quote(age >= 19 & age <= 39),
  "임신 중 아님"                                            = quote(he_prg %in% c(0, 8)),
  "당뇨병·고혈압·이상지질혈증 의사진단 없음"                = quote(de1_dg == 0 & di1_dg == 0 & di2_dg == 0),
  "하루 에너지 500-5000 kcal(가정)"                         = quote(energy_kcal >= 500 & energy_kcal <= 5000),
  "섭취 기록의 식사 시각 모두 확인(8881-8883 없음)"         = quote(!any_t_unknown & !is.na(last_t)),
  "회상일 요일 확인"                                        = quote(!is.na(recall_wd)),
  "회상일 유형(주중/주말)의 취침 시각 응답"                 = quote(!is.na(bed_m))
)
d[, inc := 1L]
flow <- data.table(step = "기본DB 전체(5개 연도)", n = nrow(d))
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N]))
}
flow[, excluded := shift(n) - n]
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
stopifnot(nrow(d) == n_raw)

# 결과변수별 분석 가능 여부 (공변량 완전 응답 포함, 모형 2 기준)
covs2 <- c("age", "f_sex", "f_year", "f_incm", "f_edu", "f_smk", "f_drk", "f_pa", "sleep_h", "energy100")
d[, cc_cov := complete.cases(.SD), .SDcols = covs2]
d[, `:=`(ok_glu = inc == 1 & !is.na(glu), ok_tg = inc == 1 & !is.na(ln_tg),
         ok_wc  = inc == 1 & !is.na(wc),  ok_mets = inc == 1 & !is.na(mets),
         ok_mets_cv = inc == 1 & !is.na(mets_cv))]
# 민감도 분석용 표시 (설계 객체를 만들기 전에 만들어 두어야 design$variables에 들어간다)
d[, `:=`(no_after_mid   = !(any_after_mid %in% TRUE),     # S2
         day_worker     = !(nonday_work %in% 1),          # S3
         weekday_recall = !(recall_weekend %in% TRUE))]   # S5

# 기수 간 통합 시 같은 층 번호 안에서 서로 다른 연도의 조사구 번호가 겹치는지 점검(보고용)
psu_overlap <- d[inc == 1, .(n_year = uniqueN(year)), by = .(kstrata, psu)][n_year > 1, .N]
cat("서로 다른 연도에서 같은 (층, 조사구) 번호가 쓰인 경우:", psu_overlap, "\n")

# ==== 3. 복합표본 설계 지정 → subset ========================================
make_design <- function(wvar) {
  dd <- copy(d)
  dd[, wt_analysis := get(wvar) * pool_ratio[as.character(year)]]
  dd[is.na(wt_analysis), wt_analysis := 0]       # 가중치 결측자는 inc = 0 으로 이미 제외
  lonely <- dd[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
  if (nrow(lonely)) warning("집락이 1개인 층(", wvar, "): ", paste(lonely$kstrata, collapse = ","))
  svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = dd)
}
des <- make_design("wt_tot")
sub <- subset(des, inc == 1)
d_sub <- d[inc == 1]

# 노출 분포 (비가중 n + 가중 %)
expo <- data.table(level = levels(d_sub$interval_cat),
                   n = as.integer(table(d_sub$interval_cat)))
pe <- svymean(~interval_cat, sub)
expo[, weighted_pct := sprintf("%.1f (%.1f)", 100 * coef(pe)[paste0("interval_cat", level)], 100 * SE(pe)[paste0("interval_cat", level)])]
q <- svyquantile(~interval_h + bed_m + last_t, sub, c(0.25, 0.5, 0.75), ci = FALSE)
expo_q <- rbindlist(lapply(names(q), function(v) data.table(variable = v, p25 = q[[v]][1], p50 = q[[v]][2], p75 = q[[v]][3])))
fwrite(expo, file.path(out_dir, "03_exposure_distribution.csv"), bom = TRUE)
fwrite(expo_q, file.path(out_dir, "03_exposure_quantiles_weighted.csv"), bom = TRUE)

# ==== 4. Table 1: 비가중 n + 가중 추정치 (마지막 식사-취침 간격 범주별) ======
fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
labs <- c(f_sex = "성별", age = "나이", f_year = "조사연도", f_incm = "소득 4분위(개인)", f_edu = "교육수준",
          f_smk = "현재흡연", f_drk = "월간음주", f_pa = "유산소 신체활동", sleep_h = "수면시간(시간, 회상일 유형)",
          energy_kcal = "하루 에너지(kcal)", bed_m = "취침 시각(24+시 표기)", last_t = "마지막 섭취 시각(24+시 표기)",
          glu = "공복혈당(mg/dL, 공복≥8h)", tg = "중성지방(mg/dL, 공복≥12h)", wc = "허리둘레(cm)", f_mets = "대사증후군(공복≥12h)")
table1_block <- function(design, dat, v) {
  if (is.factor(dat[[v]])) {
    lv  <- levels(dat[[v]])
    n   <- as.integer(table(factor(dat[[v]], levels = lv)))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    p   <- coef(est)[paste0(v, lv)]; se <- SE(est)[paste0(v, lv)]
    data.table(variable = v, level = lv, n = n, estimate = paste0(fmt(100 * p), " (", fmt(100 * se), ")"), stat = "가중 % (SE)")
  } else {
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    data.table(variable = v, level = "", n = sum(!is.na(dat[[v]])),
               estimate = paste0(fmt(coef(est)), " (", fmt(SE(est)), ")"), stat = "가중 평균 (SE)")
  }
}
table1_col <- function(design, dat, label) {
  out <- rbindlist(lapply(names(labs), function(v) table1_block(design, dat, v)))
  setnames(out, c("n", "estimate"), paste0(label, c(": n", ": 추정치")))
  out
}
table1 <- table1_col(sub, d_sub, "전체")
for (g in levels(d_sub$interval_cat)) {
  col_g <- table1_col(subset(sub, interval_cat == g), d_sub[interval_cat == g], g)
  table1 <- merge(table1, col_g, by = c("variable", "level", "stat"), sort = FALSE)
}
table1[, label := labs[variable]]
setcolorder(table1, c("label", "variable", "level", "stat"))
fwrite(table1, file.path(out_dir, "04_table1.csv"), bom = TRUE)
writeLines(c("연도: 2016, 2017, 2018, 2021, 2024 / 가중치: wt_tot × 1/5(기수 간 통합비율), 층 kstrata, 집락 psu",
             "n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치",
             "공복혈당은 공복 8시간 이상, 중성지방·대사증후군은 공복 12시간 이상인 사람만"),
           file.path(out_dir, "04_table1_footnote.txt"))

# ==== (본 분석) 회귀모형 =====================================================
outcomes <- list(
  list(y = "glu",   ok = "ok_glu",  fam = "gaussian",      lab = "공복혈당(mg/dL)",  scale = "차이(mg/dL)"),
  list(y = "ln_tg", ok = "ok_tg",   fam = "gaussian",      lab = "중성지방",          scale = "% 차이"),
  list(y = "wc",    ok = "ok_wc",   fam = "gaussian",      lab = "허리둘레(cm)",      scale = "차이(cm)"),
  list(y = "mets",  ok = "ok_mets", fam = "quasibinomial", lab = "대사증후군",        scale = "OR")
)
covs1 <- c("age", "f_sex", "f_year")
# design: subset(…, inc == 1)로 만든 설계 객체. 판정은 설계 객체 안의 변수로 해서 행이 어긋나지 않게 한다.
fit_one <- function(design, o, covs, extra_flag = NULL, ycol = o$y) {
  dat  <- as.data.table(design$variables)
  flag <- dat[[o$ok]] & complete.cases(dat[, c(ycol, covs, "interval_cat"), with = FALSE])
  if (!is.null(extra_flag)) flag <- flag & dat[[extra_flag]]
  design$variables$.fit_flag <- flag
  ds <- subset(design, .fit_flag)
  fam <- if (o$fam == "gaussian") gaussian() else quasibinomial()
  f  <- as.formula(paste(ycol, "~ interval_cat +", paste(covs, collapse = " + ")))
  ft <- svyglm(f, design = ds, family = fam)
  ftr <- svyglm(as.formula(paste(ycol, "~ trend_score +", paste(covs, collapse = " + "))), design = ds, family = fam)
  cf <- coef(ft); ci <- confint(ft); se <- SE(ft)
  rows <- grep("^interval_cat", names(cf))
  tr <- function(x) if (o$scale == "OR") exp(x) else if (o$scale == "% 차이") 100 * (exp(x) - 1) else x
  n_cat <- table(factor(dat$interval_cat[flag], levels = levels(dat$interval_cat)))
  res <- data.table(outcome = o$lab, scale = o$scale,
                    level = sub("^interval_cat", "", names(cf)[rows]),
                    est = tr(cf[rows]), ci_low = tr(ci[rows, 1]), ci_high = tr(ci[rows, 2]),
                    p = 2 * pt(-abs(cf[rows] / se[rows]), df = ft$df.residual))
  ref <- data.table(outcome = o$lab, scale = o$scale, level = "3시간 이상(기준)",
                    est = if (o$scale %in% c("OR")) 1 else 0, ci_low = NA_real_, ci_high = NA_real_, p = NA_real_)
  res <- rbind(ref, res)
  res[, n_unweighted := as.integer(n_cat[sub("\\(기준\\)", "", level)])]
  res[, `:=`(n_model = sum(flag),
             p_trend = summary(ftr)$coefficients["trend_score", ncol(summary(ftr)$coefficients)])]
  res
}
main <- rbindlist(lapply(outcomes, function(o) rbind(
  cbind(model = "모형1(나이·성별·연도)", fit_one(sub, o, covs1)),
  cbind(model = "모형2(+소득·교육·흡연·음주·신체활동·수면시간·에너지)", fit_one(sub, o, covs2))
)))
num <- c("est", "ci_low", "ci_high")
main[, (num) := lapply(.SD, round, 2), .SDcols = num][, p := signif(p, 3)][, p_trend := signif(p_trend, 3)]
fwrite(main, file.path(out_dir, "06_main_results.csv"), bom = TRUE)
cat("\n[본 분석]\n"); print(main)

# ==== 민감도 분석 (모형 2, <1시간 vs 3시간 이상만 요약) =======================
des_itv <- subset(make_design("wt_itvex"), inc == 1)
sens_def <- list(
  list(name = "S1 가중치 wt_itvex×1/5 (CLAUDE.md 규칙)", design = des_itv, flag = NULL),
  list(name = "S2 자정 이후(00-04시) 섭취자 제외(가정 A 영향)", design = sub, flag = "no_after_mid"),
  list(name = "S3 주간 외 근무자(EC_wht_5=2-7) 제외", design = sub, flag = "day_worker"),
  list(name = "S5 평일(일-목) 회상자만", design = sub, flag = "weekday_recall")
)
sens <- rbindlist(lapply(sens_def, function(s) rbindlist(lapply(outcomes, function(o) {
  cbind(analysis = s$name, fit_one(s$design, o, covs2, s$flag)[level == "<1시간"])
}))))
# S4: 대사증후군 혈압 전환식 적용
o_cv <- list(y = "mets_cv", ok = "ok_mets_cv", fam = "quasibinomial", lab = "대사증후군(혈압 전환식)", scale = "OR")
sens <- rbind(sens, cbind(analysis = "S4 2021·2024 혈압 전환식 적용", fit_one(sub, o_cv, covs2)[level == "<1시간"]))
sens[, (num) := lapply(.SD, round, 2), .SDcols = num][, p := signif(p, 3)][, p_trend := signif(p_trend, 3)]
fwrite(sens, file.path(out_dir, "07_sensitivity.csv"), bom = TRUE)
cat("\n[민감도 분석: <1시간 vs 3시간 이상, 모형 2]\n"); print(sens)

# ==== 5-1. 공식 통계 대조표 =================================================
# 단년도 wt_itvex로 만 19세 이상 지표를 재현한다. 공식값은 기억으로 채우지 않는다.
# (국민건강통계 조율값을 찾지 못해 official = NA. 보고서 표 번호·쪽과 함께 채워야 함)
official <- list(
  list(indicator = "만19세 이상 현재흡연율(%), 2018, 조율(crude)", year = 2018,
       ind = quote(smk), scale = 100, official = NA_real_,
       source = "지표 정의: 7기 PDF p.46 / 인쇄 p.38. 공식값 출처 미확인(국민건강통계 2018 조율값 필요)")
)
check <- rbindlist(lapply(official, function(o) {
  dd <- d[year == o$year]                     # 해당 연도 자료만으로 단년도 설계를 만든다
  dd[, `:=`(pop_flag = as.integer(age >= 19 & !is.na(wt_itvex)), ind_val = eval(o$ind))]
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
files <- c(files_all, files_rc)
fwrite(data.table(file = files, size = file.size(files), md5 = unname(tools::md5sum(files))),
       file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "_sessionInfo.txt"))
cat("\n완료:", out_dir, "\n")
