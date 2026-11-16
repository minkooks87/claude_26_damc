# 13_v3_main_analysis.R
# 주제: 한국 성인 현재 음주자에서 음주 관련 안면홍조 이력에 따른 음주량–고요산혈증 연관성의 차이:
#       성별 효과수정 분석, 국민건강영양조사 2019–2021
# 근거 문서: 연구 계획서(docx), 연구계획 발표 v5(pptx), 연구보고서 V3 정리본(output/10_v3_summary)
#   → 대상·노출(주당 잔수 3.5/14)·홍조(현재 2군, 이력 3군)·결과(남 ≥7.0, 여 ≥6.0)·보정변수(기본/확장)·
#     분석(성별 로지스틱, 음주×홍조 상호작용 Wald, 3원 상호작용, 여성 선형회귀, 폐경 탐색, 민감도)을 따른다.
# 참고 논문(세부 방법의 근거, output/13_v3_main_analysis/00_reference_methods.md):
#   Kang & Oh 2023(홍조군별 층화 OR + 상호작용 검정, 단계적 보정), Lee 2023(1회량 중앙값 환산),
#   Kang B 2024(통합가중치, 도메인 분석, Rao-Scott, 단순효과), So 2020(연속형 요산 + 로지스틱, 성별 상호작용),
#   Lee 2022(층별 P for trend), Kim 2018(성별 층화), Yook 2024(홍조 4군), Moore 2016(곱셈·덧셈(RERI) 상호작용 병기)
# 자료: 국민건강영양조사 제8기 2019, 2020, 2021 기본DB(hnYY_all)
# 실행: Rscript --vanilla code/13_v3_main_analysis.R   (프로젝트 폴더에서)
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
out_dir <- file.path("output", "13_v3_main_analysis")
files   <- file.path("data", "국건영 조사결과", sprintf("hn%02d_all.sas7bdat", years %% 100))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 가중치: 건강설문·검진 변수만 쓰므로 wt_itvex (8기 PDF p.92 / 인쇄 p.84)
# 통합비율: 제8기는 조사구수 비율이 아니라 조사기간 비례(각 연도 1, 소계 3) → 각 연도 1/3 (8기 PDF p.36 / 인쇄 p.28, 표 7)
pool_ratio <- c("2019" = 1/3, "2020" = 1/3, "2021" = 1/3)
stopifnot(abs(sum(pool_ratio) - 1) < 1e-12)
z975 <- qnorm(0.975)

# ==== 1. 변수 정의 (지침서에서 확인한 내용만 적는다) ========================
src_b1  <- "8기 PDF p.91 / 인쇄 p.83"
src_b2  <- "8기 PDF p.92 / 인쇄 p.84"
src_drk <- "8기 PDF p.161-162 / 인쇄 p.153-154"
src_fl  <- "8기 PDF p.163-164 / 인쇄 p.155-156"
src_ind <- "주요지표 SAS 프로그램: 8기 PDF p.48 / 인쇄 p.40"
src_lab <- "8기 PDF p.200 / 인쇄 p.192"
var_dict <- rbindlist(list(
  list(var = "id",       levels = "", miss = "", na8 = "", label = "개인 아이디", source = src_b1),
  list(var = "year",     levels = "", miss = "", na8 = "", label = "조사연도", source = src_b1),
  list(var = "sex",      levels = "1=남자;2=여자", miss = "", na8 = "", label = "성별", source = src_b1),
  list(var = "age",      levels = "1~79=세;80=80세이상(탑코딩)", miss = "", na8 = "", label = "만나이", source = src_b1),
  list(var = "psu",      levels = "", miss = "", na8 = "", label = "조사구번호(집락)", source = src_b1),
  list(var = "incm",     levels = "1=하;2=중하;3=중상;4=상", miss = "", na8 = "", label = "소득 4분위수(개인)", source = src_b1),
  list(var = "kstrata",  levels = "", miss = "", na8 = "", label = "분산추정 층", source = src_b2),
  list(var = "wt_itvex", levels = "", miss = "", na8 = "", label = "건강설문-검진조사 가중치", source = src_b2),
  list(var = "edu",      levels = "1=초졸이하;2=중졸;3=고졸;4=대졸이상", miss = "", na8 = "", label = "교육수준 재분류", source = src_b2),
  list(var = "bd1",      levels = "1=술을 마셔 본 적 없음;2=있음", miss = "9", na8 = "8=결측(소아)", label = "평생음주경험", source = src_drk),
  list(var = "bd1_11",   levels = "1=최근1년 전혀 안 마심;2=월1회미만;3=월1회정도;4=월2-4회;5=주2-3회;6=주4회이상", miss = "9",
       na8 = "8=범주유지(평생 비음주, 문항1-①⑧)", label = "1년간 음주빈도", source = src_drk),
  list(var = "bd2_1",    levels = "1=1-2잔;2=3-4잔;3=5-6잔;4=7-9잔;5=10잔 이상", miss = "9", na8 = "8=범주유지(최근1년 비음주, 문항2-1-①⑧)",
       label = "한 번에 마시는 음주량", source = src_drk),
  list(var = "bd2_14",   levels = "잔(10잔 이상 응답자만)", miss = "999", na8 = "888=결측(문항2-2-①②③④⑧: 10잔 미만)",
       label = "한 번에 마시는 음주량(잔)", source = src_drk),
  list(var = "bd2_31",   levels = "1=전혀 없음;2=월1회미만;3=월1회정도;4=주1회정도;5=거의매일", miss = "9",
       na8 = "8=범주유지(지침서 고위험음주 정의에 8 포함)", label = "폭음 빈도(7잔 이상)", source = src_drk),
  list(var = "bd2_32",   levels = "1=전혀 없음;2=월1회미만;3=월1회정도;4=주1회정도;5=거의매일", miss = "9",
       na8 = "8=범주유지(지침서 고위험음주 정의에 8 포함)", label = "여자 폭음 빈도(5잔 이상)", source = src_drk),
  list(var = "bd9_1",    levels = "1=예;2=아니오", miss = "9", na8 = "8=결측(문항1-① 평생 비음주, 청소년, 소아)",
       label = "과거 알코올 홍조(처음 마시기 시작한 1-2년 사이, 맥주 1잔 정도 소량 음주로 금세 빨개짐)", source = src_fl),
  list(var = "bd9_2",    levels = "1=아니오;2=가끔 그렇다;3=자주 그렇다;4=늘 그렇다", miss = "9",
       na8 = "8=결측(문항2-1-①⑧ 최근1년 비음주, 청소년, 소아)", label = "현재 알코올 홍조", source = src_fl),
  list(var = "dm8_dg",   levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년, 소아)", label = "통풍 의사진단 여부", source = "8기 PDF p.137 / 인쇄 p.129"),
  list(var = "di1_dg",   levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년, 소아)", label = "고혈압 의사진단 여부(민감도)", source = "8기 PDF p.100 / 인쇄 p.92"),
  list(var = "de1_dg",   levels = "0=없음;1=있음", miss = "9", na8 = "8=결측(청소년, 소아)", label = "당뇨병 의사진단 여부(민감도)", source = "8기 PDF p.112 / 인쇄 p.104"),
  list(var = "lw_ms",    levels = "1=초경하기 전;2=월경중;3=임신중;4=출산후 수유 중;5=자연폐경;6=인공폐경;7=기타", miss = "9",
       na8 = "8=결측(만10세미만 여자)", label = "월경여부(여성)", source = "8기 PDF p.190 / 인쇄 p.182"),
  list(var = "lw_mp_a",  levels = "세(80=80세이상 탑코딩)", miss = "999", na8 = "888=결측(만19세미만 여자, 문항1-①②③④⑦⑧)",
       label = "폐경연령(자연·인공 통합)", source = "8기 PDF p.190 / 인쇄 p.182"),
  list(var = "he_prg",   levels = "0=아니오;1=예", miss = "", na8 = "8=범주유지(비해당-남자)", label = "임신여부", source = "8기 PDF p.192 / 인쇄 p.184"),
  list(var = "he_hp",    levels = "1=정상;2=고혈압전단계;3=고혈압", miss = "", na8 = "", label = "고혈압 유병여부(19세이상 생성변수)", source = "8기 PDF p.196 / 인쇄 p.188"),
  list(var = "he_bmi",   levels = "kg/m2", miss = "", na8 = "", label = "체질량지수", source = "8기 PDF p.198 / 인쇄 p.190"),
  list(var = "he_dm_hba1c", levels = "1=정상;2=당뇨병 전단계;3=당뇨병", miss = "", na8 = "",
       label = "당뇨병 유병여부(19세이상, 비임신·8시간 이상 공복자만 생성)", source = "8기 PDF p.199 / 인쇄 p.191"),
  list(var = "he_uacid", levels = "mg/dL", miss = "", na8 = "", label = "요산", source = src_lab),
  list(var = "he_uacid_etc", levels = "<1.0=1.0 mg/dL 미만", miss = "", na8 = "", label = "요산 장비분석범위(2017년부터 1.0 미만 표시)", source = src_lab),
  list(var = "he_crea",  levels = "mg/dL", miss = "", na8 = "", label = "혈중크레아티닌", source = src_lab),
  list(var = "bs1_1",    levels = "1=5갑(100개비) 미만;2=5갑(100개비) 이상;3=피운 적 없음", miss = "", na8 = "",
       label = "평생 일반담배(궐련) 흡연 여부(8·9는 흡연 변수 정의식에서 결측 처리)",
       source = "8기 PDF p.175 / 인쇄 p.167; 현재흡연 정의 PDF p.48 / 인쇄 p.40, PDF p.182 / 인쇄 p.174"),
  list(var = "bs3_1",    levels = "1=매일피움;2=가끔피움;3=과거엔 피웠으나 현재 피우지 않음", miss = "", na8 = "",
       label = "현재 일반담배(궐련) 흡연 여부(8·9는 흡연 변수 정의식에서 처리)",
       source = "8기 PDF p.175 / 인쇄 p.167; 현재흡연 정의 PDF p.48 / 인쇄 p.40, PDF p.182 / 인쇄 p.174"),
  list(var = "pa_aerobic", levels = "0=미실천;1=실천", miss = "", na8 = "", label = "유산소 신체활동 실천율(생성변수)", source = "8기 PDF p.188 / 인쇄 p.180")
))
fwrite(var_dict, file.path(out_dir, "01_variable_definitions.csv"), bom = TRUE)

# ---- 자료 읽기: 필요한 변수만, 대소문자 무시 ----
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
# (1) 노출: 주당 음주잔수 = 빈도 대표값(회/주) × 1회량 대표값(잔)  [발표 v5·V3 환산표; 1회량 중앙값은 Lee 2023과 같다]
#     빈도: 월1회미만 0.5회/월, 월1회 1회/월, 월2-4회 3회/월, 주2-3회 2.5회/주, 주4회이상 5.5회/주 (월→주: ×12/52)
#     1회량: 1-2잔 1.5, 3-4잔 3.5, 5-6잔 5.5, 7-9잔 8, 10잔 이상은 BD2_14 기입값(없으면 결측)
freq_main  <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
freq_alt_b <- replace(freq_main, c("2", "6"), c(0.25 * 12 / 52, 4))   # 민감도 S3a: 열린 범주를 작게
freq_alt_c <- replace(freq_main, c("2", "6"), c(0.75 * 12 / 52, 7))   # 민감도 S3b: 열린 범주를 크게
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
d[, per_occ := fifelse(bd2_1 == 5, as.numeric(bd2_14), unname(amt[as.character(bd2_1)]))]
calc_wk <- function(freq) unname(freq[as.character(d$bd1_11)]) * d$per_occ
d[, `:=`(drinks_wk = calc_wk(freq_main), drinks_wk_b = calc_wk(freq_alt_b), drinks_wk_c = calc_wk(freq_alt_c))]
cut3 <- function(x) cut(x, c(-Inf, 3.5, 14, Inf), right = FALSE, labels = c("저음주", "중음주", "고음주"))
d[, `:=`(drink3 = cut3(drinks_wk), drink3_b = cut3(drinks_wk_b), drink3_c = cut3(drinks_wk_c))]
d[, drink_ord := as.numeric(drink3) - 1]                       # 추세 검정용 순서형(0·1·2)
# 민감도 S10: Kang & Oh 2023 방식의 빈도 2군(월1회 이하 vs 월1회 초과)
d[, drink_freq2 := factor(fcase(bd1_11 %in% 2:3, "월1회 이하", bd1_11 %in% 4:6, "월1회 초과"), c("월1회 이하", "월1회 초과"))]
# 민감도 S5: 폭음 빈도(남 7잔·여 5잔 이상 마신 횟수)
d[, binge := fifelse(sex == 1, bd2_31, bd2_32)]
d[, binge3 := factor(fcase(binge == 1, "없음", binge %in% 2:3, "월1회 이하", binge %in% 4:5, "주1회 이상"), c("없음", "월1회 이하", "주1회 이상"))]
conv <- data.table(항목 = c(rep("빈도(BD1_11)", 5), rep("1회량(BD2_1)", 5)),
                   코드 = c(2:6, 1:5),
                   응답 = c("월1회미만", "월1회정도", "월2-4회", "주2-3회정도", "주4회이상", "1-2잔", "3-4잔", "5-6잔", "7-9잔", "10잔 이상"),
                   주분석_대표값 = c(round(freq_main, 4), 1.5, 3.5, 5.5, 8, NA),
                   민감도_S3a = c(round(freq_alt_b, 4), 1.5, 3.5, 5.5, 8, NA),
                   민감도_S3b = c(round(freq_alt_c, 4), 1.5, 3.5, 5.5, 8, NA),
                   비고 = c("회/주", "회/주", "회/주", "회/주", "회/주", "잔", "잔", "잔", "잔", "BD2_14 기입값(잔)"))
fwrite(conv, file.path(out_dir, "03c_conversion_table.csv"), bom = TRUE)

# (2) 효과수정변수: 현재 홍조(주분석), 홍조 이력(부가), 4군(보조)
d[, flush_now := factor(fcase(bd9_2 == 1, "비홍조", bd9_2 %in% 2:4, "홍조"), c("비홍조", "홍조"))]
d[, flush_grp := factor(fcase(bd9_1 == 2 & bd9_2 == 1,     "비홍조",
                               bd9_1 == 1 & bd9_2 == 1,     "홍조 소실",
                               bd9_1 == 1 & bd9_2 %in% 2:4, "지속 홍조",
                               bd9_1 == 2 & bd9_2 %in% 2:4, "신규 홍조"),
                         c("비홍조", "홍조 소실", "지속 홍조", "신규 홍조"))]
d[, flush_hist := factor(fifelse(flush_grp == "신규 홍조", NA_character_, as.character(flush_grp)), c("비홍조", "홍조 소실", "지속 홍조"))]
# 민감도 S6: Kang & Oh 2023 방식 — '자주·늘'만 홍조, '아니오'는 비홍조, '가끔'은 제외
d[, flush_now_ko := factor(fcase(bd9_2 == 1, "비홍조", bd9_2 %in% 3:4, "홍조"), c("비홍조", "홍조"))]

# (3) 결과: 요산·고요산혈증
#     요산 1.0 미만("<1.0" 표기, HE_Uacid 결측)은 발표 v5·예비 집계와 같이 결측으로 둔다 (8기 PDF p.200 각주 1)
d[, uacid := he_uacid]
d[, uacid_lt1 := is.na(he_uacid) & trimws(as.character(he_uacid_etc)) == "<1.0"]
#     고요산혈증 기준은 지침서에 없음 → V3의 조작적 정의 남 ≥7.0, 여 ≥6.0 mg/dL (Bae 2023 등 국건영 논문과 같음)
d[, hua := as.integer(uacid >= fifelse(sex == 1, 7.0, 6.0))]

# (4) 공변량
#     현재흡연: 주요지표 SAS 정의 (8기 PDF p.48 / 인쇄 p.40)
d[, smk := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3, smk := as.integer(bs1_1 == 2 & bs3_1 %in% c(1, 2))]
#     흡연 3범주(진행 기록 문서의 규칙, 규칙 비교 R1·R3용): 0 비흡연, 1 과거흡연(5갑 이상 & 현재 안 피움), 2 현재흡연.
#     평생 5갑 미만은 지침서의 현재흡연 정의에 맞춰 비흡연으로 둔다 (BS1_1·BS3_1 코드: 8기 PDF p.175 / 인쇄 p.167)
d[, smk3 := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3,
  smk3 := fcase(bs1_1 == 2 & bs3_1 %in% c(1, 2), 2L, bs1_1 == 2 & bs3_1 == 3, 1L, default = 0L)]
#     고혈압·당뇨병: 국건영 유병 생성변수(HE_HP = 3, HE_DM_HbA1c = 3)를 유병으로, 1·2는 없음으로 (V3)
d[, htn := fcase(he_hp == 3, 1L, he_hp %in% 1:2, 0L)]
d[, dm  := fcase(he_dm_hba1c == 3, 1L, he_dm_hba1c %in% 1:2, 0L)]
#     eGFR: 지침서에 계산식 없음 → V3의 CKD-EPI 2009식(Levey 2009; 문헌식, 가정)
d[, egfr := 141 * pmin(he_crea / fifelse(sex == 2, 0.7, 0.9), 1)^fifelse(sex == 2, -0.329, -0.411) *
            pmax(he_crea / fifelse(sex == 2, 0.7, 0.9), 1)^(-1.209) * 0.993^age * fifelse(sex == 2, 1.018, 1)]
d[, egfr10 := egfr / 10]
#     폐경 상태(여성, 탐색): LW_ms 5·6 = 폐경 후, 2·3·4 = 폐경 전, 1(초경 전)·7(기타)은 결측. LW_mp_a는 폐경 연령 기술용
d[, meno := factor(fcase(lw_ms %in% 5:6, "폐경 후", lw_ms %in% 2:4, "폐경 전"), c("폐경 전", "폐경 후"))]
d[, `:=`(
  f_sex    = factor(sex, 1:2, c("남자", "여자")),
  f_year   = factor(year),
  f_incm   = factor(incm, 1:4, c("하", "중하", "중상", "상")),
  f_edu    = factor(edu, 1:4, c("초졸이하", "중졸", "고졸", "대졸이상")),
  f_smk    = factor(smk, 0:1, c("비흡연", "현재흡연")),
  f_smk3   = factor(smk3, 0:2, c("비흡연", "과거흡연", "현재흡연")),
  f_pa     = factor(pa_aerobic, 0:1, c("미실천", "실천")),
  f_htn    = factor(htn, 0:1, c("없음", "있음")),
  f_dm     = factor(dm, 0:1, c("없음", "있음")),
  f_htn_dx = factor(di1_dg, 0:1, c("없음", "있음")),
  f_dm_dx  = factor(de1_dg, 0:1, c("없음", "있음")),
  f_hua    = factor(hua, 0:1, c("없음", "있음"))
)]
d[, cov_complete    := !is.na(age) & !is.na(f_edu) & !is.na(f_incm) & !is.na(f_smk) & !is.na(f_pa) & !is.na(he_bmi) & !is.na(f_htn) & !is.na(f_dm) & !is.na(egfr)]
d[, cov_complete_dx := !is.na(age) & !is.na(f_edu) & !is.na(f_incm) & !is.na(f_smk) & !is.na(f_pa) & !is.na(he_bmi) & !is.na(f_htn_dx) & !is.na(f_dm_dx) & !is.na(egfr)]
stopifnot(nrow(d) == n_raw)

# ==== 2. 분석 대상 표시 (행 삭제 없음) ======================================
criteria <- list(
  "가중치 있음(검진·설문 참여, wt_itvex)"                       = quote(!is.na(wt_itvex)),
  "만 19세 이상"                                                = quote(age >= 19),
  "최근 1년 음주자(BD1_11 = 2-6)"                               = quote(bd1_11 %in% 2:6),
  "음주량 계산 가능·과거/현재 홍조 응답·요산 측정"              = quote(!is.na(drinks_wk) & !is.na(flush_grp) & !is.na(uacid)),
  "통풍 의사진단 없음(진단·모름·무응답 제외)"                   = quote(dm8_dg %in% 0),
  "보정변수 모두 있음(교육·소득·흡연·신체활동·BMI·고혈압·당뇨병 유병·eGFR)" = quote(cov_complete)
)
d[, inc := 1L]
flow <- data.table(step = "기본DB 전체(2019-2021)", n = nrow(d), n_men = d[sex == 1, .N], n_women = d[sex == 2, .N])
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N], n_men = d[inc == 1L & sex == 1, .N], n_women = d[inc == 1L & sex == 2, .N]))
}
flow[, excluded := shift(n) - n]
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
# 발표 v5 흐름(가중치 단계 없음)과 대조
steps_v5 <- list("2019-2021 기본DB 전체" = rep(TRUE, n_raw), "만 19세 이상" = d$age >= 19, "최근 1년 음주자" = d$bd1_11 %in% 2:6,
                 "음주량·과거/현재 홍조·요산 확보" = !is.na(d$drinks_wk) & !is.na(d$flush_grp) & !is.na(d$uacid),
                 "통풍 의사진단 없음" = d$dm8_dg %in% 0, "보정변수 모두 있음" = d$cov_complete)
v5_n <- c(22559, 18691, 12231, 12002, 11217, 10779)
keep <- rep(TRUE, n_raw); flow_v5 <- NULL
for (i in seq_along(steps_v5)) {
  s <- steps_v5[[i]]; keep <- keep & !is.na(s) & s
  flow_v5 <- rbind(flow_v5, data.table(step = names(steps_v5)[i], n = sum(keep), v5_presentation = v5_n[i], match = sum(keep) == v5_n[i]))
}
fwrite(flow_v5, file.path(out_dir, "02b_flow_vs_presentation_v5.csv"), bom = TRUE)
# 민감도용 대상 표시
base_ok <- Reduce(`&`, lapply(criteria[1:4], function(e) { x <- d[, eval(e)]; !is.na(x) & x }))
d[, elig      := as.integer(base_ok & dm8_dg %in% 0)]                           # 공변량 제외 전(포함/제외 비교용)
d[, inc_s1    := as.integer(base_ok & dm8_dg %in% c(0, 1) & cov_complete)]       # S1 통풍 진단자 포함(모름·무응답 제외)
d[, inc_s2    := as.integer(base_ok & !(dm8_dg %in% 1) & cov_complete)]          # S2 통풍 모름·무응답 포함(진단자 제외)
d[, inc_s8    := as.integer(base_ok & dm8_dg %in% 0 & cov_complete_dx)]          # S8 고혈압·당뇨병 의사진단 보정
d[, inc_no2020 := as.integer(inc == 1L & year != 2020)]                          # S4 2020년 제외
# 규칙 비교 R2·R3: 임신·수유 여성 제외(진행 기록 문서의 규칙). 월경여부 LW_ms 3(임신중)·4(출산후 수유 중) 또는 검진 당일 임신 HE_prg = 1
#   (LW_ms: 8기 PDF p.190 / 인쇄 p.182, HE_prg: 8기 PDF p.192 / 인쇄 p.184)
d[, preg_lact  := as.integer(lw_ms %in% 3:4 | he_prg %in% 1)]
d[, inc_nopreg := as.integer(inc == 1L & preg_lact == 0L)]
d[, incl := factor(fifelse(inc == 1L, "포함", "제외"), c("포함", "제외"))]
cat("통풍 문항(공변량 제외 전): 진단 있음", d[base_ok & dm8_dg %in% 1, .N], "/ 모름·무응답", d[base_ok & is.na(dm8_dg), .N],
    "/ 요산 '<1.0' 표기로 결측 처리된 음주자", d[!is.na(wt_itvex) & age >= 19 & bd1_11 %in% 2:6 & !is.na(drinks_wk) & !is.na(flush_grp) & uacid_lt1 == TRUE, .N],
    "/ 임신 중(보정변수 단계에서 제외됨)", d[elig == 1 & he_prg %in% 1, .N], "\n")
stopifnot(nrow(d) == n_raw)

# ==== 3. 복합표본 설계 지정 → subset ========================================
d[, wt_analysis := wt_itvex * pool_ratio[as.character(year)]]
d[is.na(wt_analysis), wt_analysis := 0]
lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","), " → options(survey.lonely.psu = ...) 선택 근거를 기록하세요.")
# (민감도 S7) 성별 가중 삼분위 절단점: 분석 대상 안에서 계산한 뒤 변수를 추가하고 설계 객체를 다시 만든다
# 설계 객체에는 data.table이 아닌 일반 data.frame을 넣는다 (survey 패키지의 내부 subset이 data.table 구조를 깨뜨릴 수 있음)
des0 <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = as.data.frame(d))
tert <- rbindlist(lapply(1:2, function(sx) {
  q <- svyquantile(~drinks_wk, subset(des0, inc == 1 & sex == sx), c(1/3, 2/3), na.rm = TRUE)
  data.table(sex = sx, q1 = as.numeric(coef(q))[1], q2 = as.numeric(coef(q))[2])
}))
fwrite(tert, file.path(out_dir, "03d_tertile_cutpoints.csv"), bom = TRUE)
d[, drink_t3 := NA_character_]
for (sx in 1:2) d[sex == sx, drink_t3 := as.character(cut(drinks_wk, c(-Inf, tert[sex == sx, q1], tert[sex == sx, q2], Inf), right = FALSE, labels = c("T1", "T2", "T3")))]
d[, drink_t3 := factor(drink_t3, c("T1", "T2", "T3"))]
des  <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = as.data.frame(d))
sdes <- subset(des, inc == 1)

# ---- 공용 도구 ----
dat_of <- function(design) as.data.table(design$variables)
dsub   <- function(design, keep) { keep <- keep & !is.na(keep); design$variables$.keep <- keep; subset(design, .keep) }
fmt    <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
fmt_p  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
fit_svy <- function(design, form, fam) {   # 모형 변수 완전 응답자를 설계 객체 안에서 subset 한 뒤 적합
  vars <- all.vars(form); dv <- dat_of(design)
  cc <- complete.cases(dv[, vars, with = FALSE])
  ds <- dsub(design, cc)
  for (v in vars) if (is.factor(ds$variables[[v]])) ds$variables[[v]] <- droplevels(ds$variables[[v]])
  fit <- svyglm(form, design = ds, family = fam)
  list(fit = fit, design = ds, data = dat_of(ds), n = sum(cc), y = vars[1], binary = identical(fam$family, "quasibinomial"))
}
term_test <- function(fit, term) {
  tt <- regTermTest(fit, as.formula(paste0("~", term)), method = "Wald")
  data.table(df = as.integer(tt$df), ddf = as.numeric(tt$ddf), F = as.numeric(tt$Ftest), p = as.numeric(tt$p))
}
contrast_test <- function(fit, L) {        # L: 계수 가중치(이름 있는 벡터)들의 list → 공동 Wald F 검정
  b <- coef(fit); V <- vcov(fit)
  Lm <- do.call(rbind, lapply(L, function(l) { v <- setNames(rep(0, length(b)), names(b)); v[names(l)] <- l; v }))
  est <- Lm %*% b; W <- as.numeric(t(est) %*% solve(Lm %*% V %*% t(Lm)) %*% est); q <- nrow(Lm); ddf <- fit$df.residual
  data.table(df = q, ddf = ddf, F = W / q, p = pf(W / q, q, ddf, lower.tail = FALSE))
}
tidy <- function(fit, pat, or = TRUE) {
  cf <- coef(fit); ci <- confint(fit); rows <- grep(pat, names(cf)); tr <- if (or) exp else identity
  p  <- summary(fit)$coefficients[rows, ncol(summary(fit)$coefficients)]
  data.table(term = names(cf)[rows], est = tr(cf[rows]), ci_low = tr(ci[rows, 1]), ci_high = tr(ci[rows, 2]), p = p)
}
# 결합효과·층화효과·RERI 표 (Knol & VanderWeele 2012 형식; Moore 2016 참고)
joint_table <- function(r, expo, mod, or = TRUE) {
  fit <- r$fit; dat <- r$data; des_m <- r$design; b <- coef(fit); V <- vcov(fit)
  ctab <- summary(fit)$coefficients
  e_lv <- levels(dat[[expo]]); m_lv <- levels(dat[[mod]])
  cn <- function(e, m) { v <- character(0)
    if (e != e_lv[1]) v <- c(v, paste0(expo, e)); if (m != m_lv[1]) v <- c(v, paste0(mod, m))
    if (e != e_lv[1] && m != m_lv[1]) v <- c(v, paste0(expo, e, ":", mod, m)); v }
  lin <- function(nm) {
    if (!length(nm)) return(c(0, 0))
    if (!all(nm %in% names(b)) || any(is.na(b[nm]))) return(c(NA_real_, NA_real_))
    l <- setNames(rep(0, length(b)), names(b)); l[nm] <- 1
    c(sum(l * b), sqrt(as.numeric(t(l) %*% V %*% l)))
  }
  tr <- if (or) exp else identity
  f3 <- function(x) c(est = tr(x[1]), lo = tr(x[1] - z975 * x[2]), hi = tr(x[1] + z975 * x[2]),
                      p = if (is.na(x[2]) || x[2] <= 0) NA_real_ else 2 * pnorm(-abs(x[1] / x[2])))
  rbindlist(lapply(m_lv, function(m) rbindlist(lapply(e_lv, function(e) {
    cell <- dat[[expo]] == e & dat[[mod]] == m
    n <- sum(cell); n_ev <- if (r$binary) sum(dat[[r$y]][cell]) else NA_integer_
    pv <- tryCatch({ est <- svymean(as.formula(paste0("~", r$y)), dsub(des_m, cell)); c(coef(est)[1], SE(est)[1]) },
                   error = function(err) c(NA_real_, NA_real_))
    j <- f3(lin(cn(e, m)))                                     # 결합효과: 기준 (e1, m1)
    s <- f3(lin(setdiff(cn(e, m), cn(e_lv[1], m))))             # 층화효과: m 안에서 e vs e1 (단순효과)
    f <- f3(lin(setdiff(cn(e, m), cn(e, m_lv[1]))))             # 수정변수 효과: e 안에서 m vs m1
    int_nm <- if (e != e_lv[1] && m != m_lv[1]) paste0(expo, e, ":", mod, m) else NA_character_
    p_int <- if (!is.na(int_nm) && int_nm %in% rownames(ctab)) ctab[int_nm, ncol(ctab)] else NA_real_
    rr <- rep(NA_real_, 4)
    if (!is.na(int_nm) && or && all(c(paste0(expo, e), paste0(mod, m), int_nm) %in% names(b))) {
      idx <- c(paste0(expo, e), paste0(mod, m), int_nm)
      o11 <- exp(sum(b[idx])); o10 <- exp(b[idx[1]]); o01 <- exp(b[idx[2]])
      rer <- o11 - o10 - o01 + 1; g <- c(o11 - o10, o11 - o01, o11)
      se <- sqrt(as.numeric(t(g) %*% V[idx, idx] %*% g))
      rr <- c(rer, rer - z975 * se, rer + z975 * se, 2 * pnorm(-abs(rer / se)))
    }
    data.table(modifier = m, exposure = e, n = n, n_event = n_ev, wt_mean = unname(pv[1]), wt_se = unname(pv[2]),
               joint_est = unname(j["est"]), joint_lo = unname(j["lo"]), joint_hi = unname(j["hi"]),
               strat_est = unname(s["est"]), strat_lo = unname(s["lo"]), strat_hi = unname(s["hi"]), strat_p = unname(s["p"]),
               mod_est = unname(f["est"]), mod_lo = unname(f["lo"]), mod_hi = unname(f["hi"]), mod_p = unname(f["p"]),
               p_product_term = p_int, reri = rr[1], reri_lo = rr[2], reri_hi = rr[3], reri_p = rr[4])
  }))))
}
# 모형식: 보정변수가 없어도(Crude) 되도록 항들을 이어 붙인다
mk_form <- function(y, terms, covs = character(0)) as.formula(paste(y, "~", paste(c(terms, covs), collapse = " + ")))
run_int <- function(design, y, expo, mod, covs, fam, or) {
  r <- fit_svy(design, mk_form(y, paste(expo, "*", mod), covs), fam)
  list(r = r, cells = cbind(n_model = r$n, joint_table(r, expo, mod, or)),
       test = cbind(n_model = r$n, term_test(r$fit, paste0(expo, ":", mod))))
}
# 층별 P for trend (음주 수준 순서형 0·1·2; Lee 2022 참고)
trend_table <- function(design, y, mod, covs, fam, or) {
  r <- fit_svy(design, mk_form(y, paste("drink_ord *", mod), covs), fam)
  dat <- r$data; m_lv <- levels(dat[[mod]]); b <- coef(r$fit); V <- vcov(r$fit); tr <- if (or) exp else identity
  out <- rbindlist(lapply(m_lv, function(m) {
    nm <- if (m == m_lv[1]) "drink_ord" else c("drink_ord", paste0("drink_ord:", mod, m))
    l <- setNames(rep(0, length(b)), names(b)); l[nm] <- 1
    est <- sum(l * b); se <- sqrt(as.numeric(t(l) %*% V %*% l))
    data.table(modifier = m, n = sum(dat[[mod]] == m), per_level_est = tr(est), ci_low = tr(est - z975 * se),
               ci_high = tr(est + z975 * se), p_trend = 2 * pnorm(-abs(est / se)))
  }))
  out[, `:=`(n_model = r$n, p_trend_x_modifier = term_test(r$fit, paste0("drink_ord:", mod))$p)]
  out[]
}

# ==== 4. 기술통계 ===========================================================
d_sub <- dat_of(sdes)
sexes <- c("남자", "여자")
# 4-1. 칸별 비가중 n·고요산혈증 사례 수·가중 유병률 (성별 × 홍조 × 음주 수준)
cell_tab <- function(gvar) rbindlist(lapply(sexes, function(s) rbindlist(lapply(levels(d_sub[[gvar]]), function(g)
  rbindlist(lapply(levels(d_sub$drink3), function(e) {
    k <- d_sub$f_sex == s & d_sub[[gvar]] == g & d_sub$drink3 == e
    pv <- if (sum(k) >= 2) { est <- svymean(~hua, dsub(sdes, k)); c(coef(est)[1], SE(est)[1]) } else c(NA_real_, NA_real_)
    data.table(sex = s, group_var = gvar, group = g, drink = e, n = sum(k), n_hua = sum(d_sub$hua[k]),
               hua_pct_wt = 100 * unname(pv[1]), hua_se_wt = 100 * unname(pv[2]))
  }))))))
cells <- rbind(cell_tab("flush_now"), cell_tab("flush_grp"))
fwrite(cells, file.path(out_dir, "03_cell_counts.csv"), bom = TRUE)

# 4-2. Table 1: 비가중 n + 가중 추정치, 집단 차이는 Rao-Scott χ²(범주) / 설계 기반 Wald(연속)
t1_labs <- c(age = "나이(세)", f_year = "조사연도", f_edu = "교육수준", f_incm = "소득 4분위(개인)", f_smk = "현재흡연",
             f_pa = "유산소 신체활동 실천", he_bmi = "체질량지수(kg/m2)", f_htn = "고혈압 유병(HE_HP=3)", f_dm = "당뇨병 유병(HE_DM_HbA1c=3)",
             egfr = "eGFR(CKD-EPI 2009, mL/min/1.73m2)", drinks_wk = "주당 음주잔수(잔)", drink3 = "음주 수준", binge3 = "폭음 빈도",
             uacid = "혈청 요산(mg/dL)", f_hua = "고요산혈증", meno = "폐경 상태(여성, LW_ms)", lw_mp_a = "폐경 연령(세, 여성 폐경 후, LW_mp_a)",
             f_sex = "성별", flush_now = "현재 홍조", flush_grp = "홍조 패턴(4군)")
t1_block <- function(design, dat, v) {
  if (is.factor(dat[[v]])) {
    lv <- levels(dat[[v]]); n <- as.integer(table(factor(dat[[v]], levels = lv)))
    if (sum(n) == 0) return(data.table(variable = v, level = lv, stat = "가중 % (SE)", n = n, estimate = "-"))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    p <- coef(est)[paste0(v, lv)]; se <- SE(est)[paste0(v, lv)]
    data.table(variable = v, level = lv, stat = "가중 % (SE)", n = n, estimate = paste0(fmt(100 * p), " (", fmt(100 * se), ")"))
  } else {
    nn <- sum(!is.na(dat[[v]]))
    if (nn == 0) return(data.table(variable = v, level = "", stat = "가중 평균 (SE)", n = 0L, estimate = "-"))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    data.table(variable = v, level = "", stat = "가중 평균 (SE)", n = nn, estimate = paste0(fmt(coef(est), 2), " (", fmt(SE(est), 2), ")"))
  }
}
t1_pvalue <- function(design, dat, v, g) {
  ok <- !is.na(dat[[v]]) & !is.na(dat[[g]])
  if (sum(ok) == 0 || length(unique(dat[[g]][ok])) < 2) return(NA_real_)
  dd <- dsub(design, ok)
  dd$variables[[g]] <- droplevels(dd$variables[[g]])
  if (is.factor(dd$variables[[v]])) dd$variables[[v]] <- droplevels(dd$variables[[v]])
  tryCatch({
    if (is.factor(dat[[v]])) {
      if (length(unique(dat[[v]][ok])) < 2) return(NA_real_)
      as.numeric(svychisq(as.formula(paste0("~", v, "+", g)), dd, statistic = "F")$p.value)
    } else {
      f <- svyglm(as.formula(paste0(v, "~", g)), dd)
      as.numeric(regTermTest(f, as.formula(paste0("~", g)), method = "Wald")$p)
    }
  }, error = function(err) NA_real_)
}
table1_group <- function(design, gvar, vars) {
  dat <- dat_of(design); lv <- levels(dat[[gvar]])
  blocks <- lapply(lv, function(g) {
    dg <- dsub(design, dat[[gvar]] == g)
    out <- rbindlist(lapply(vars, function(v) t1_block(dg, dat_of(dg), v)))
    setnames(out, c("n", "estimate"), paste0(g, c(": n", ": 추정치"))); out
  })
  tab <- blocks[[1]]
  for (b in blocks[-1]) tab <- merge(tab, b, by = c("variable", "level", "stat"), sort = FALSE)
  tab[, ord := match(paste(variable, level), paste(blocks[[1]]$variable, blocks[[1]]$level))]
  pv <- rbindlist(lapply(vars, function(v) data.table(variable = v, p_value = t1_pvalue(design, dat, v, gvar))))
  tab <- merge(tab, pv, by = "variable", sort = FALSE)
  setorder(tab, ord); tab[, ord := NULL]
  tab[, label := t1_labs[variable]]; setcolorder(tab, c("label", "variable", "level", "stat")); tab[]
}
t1_vars <- c("age", "f_year", "f_edu", "f_incm", "f_smk", "f_pa", "he_bmi", "f_htn", "f_dm", "egfr",
             "drinks_wk", "drink3", "binge3", "uacid", "f_hua", "meno", "lw_mp_a")
sdes_sex <- lapply(setNames(sexes, sexes), function(s) dsub(sdes, d_sub$f_sex == s))
table1_now  <- rbindlist(lapply(sexes, function(s) cbind(sex = s, table1_group(sdes_sex[[s]], "flush_now", t1_vars))))
table1_hist <- rbindlist(lapply(sexes, function(s) cbind(sex = s, table1_group(dsub(sdes_sex[[s]], !is.na(dat_of(sdes_sex[[s]])$flush_hist)), "flush_hist", t1_vars))))
fwrite(table1_now,  file.path(out_dir, "04_table1_by_sex_flushnow.csv"), bom = TRUE)
fwrite(table1_hist, file.path(out_dir, "04b_table1_by_sex_flushhist.csv"), bom = TRUE)
writeLines(c("연도: 2019, 2020, 2021 / 가중치: wt_itvex × 1/3(제8기 기수 내 조사기간 비례 통합비율, 8기 PDF p.36), 층 kstrata, 집락 psu",
             "n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치(SE)",
             "p_value: 범주형은 Rao-Scott 보정 카이제곱(F), 연속형은 복합표본 선형회귀 Wald 검정 (성별 안에서 홍조군 간 비교)",
             "고요산혈증: 남 ≥7.0, 여 ≥6.0 mg/dL(V3 조작적 정의). 통풍 의사진단자(모름·무응답 포함) 제외. 폐경 변수는 여성만."),
           file.path(out_dir, "04_table1_footnote.txt"))

# 4-3. 포함자 vs 공변량 결측으로 제외된 사람 비교 (선택편향 검토, V3 7-6)
d_elig <- dsub(des, dat_of(des)$elig == 1)
cmp <- table1_group(d_elig, "incl", c("age", "f_sex", "f_year", "drink3", "flush_now", "flush_grp", "uacid", "f_hua"))
fwrite(cmp, file.path(out_dir, "03b_included_vs_excluded.csv"), bom = TRUE)

# ==== 5. 본 분석 ============================================================
cov_age  <- c("age")
cov_base <- c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa")
cov_ext  <- c(cov_base, "he_bmi", "f_htn", "f_dm", "egfr10")
cov_ext_dx <- c(cov_base, "he_bmi", "f_htn_dx", "f_dm_dx", "egfr10")
models <- list("M0 연령보정" = cov_age, "M1 기본" = cov_base, "M2 확장" = cov_ext)
oc_bin <- list(y = "hua",   fam = quasibinomial(), or = TRUE,  lab = "고요산혈증(OR)")
oc_con <- list(y = "uacid", fam = gaussian(),      or = FALSE, lab = "혈청 요산(β, mg/dL)")

# 5-1. 주분석: 음주 수준 × 현재 홍조 (남녀 각각, 3개 모형) — 고요산혈증 OR; 여성은 요산 연속형도(남성은 보조)
main_cells <- list(); main_tests <- list(); main_trend <- list()
for (s in sexes) for (mdl in names(models)) {
  for (oc in list(oc_bin, oc_con)) {
    if (oc$y == "uacid" && s == "남자" && mdl != "M2 확장") next
    lab <- if (oc$y == "uacid" && s == "남자") paste0(oc$lab, " [보조: 계획은 여성만]") else oc$lab
    a <- run_int(sdes_sex[[s]], oc$y, "drink3", "flush_now", models[[mdl]], oc$fam, oc$or)
    main_cells[[length(main_cells) + 1]] <- cbind(outcome = lab, sex = s, model = mdl, a$cells)
    main_tests[[length(main_tests) + 1]] <- cbind(outcome = lab, sex = s, model = mdl, test = "음주 수준 × 현재 홍조 (공동, 2 df)", a$test)
    main_trend[[length(main_trend) + 1]] <- cbind(outcome = lab, sex = s, model = mdl, trend_table(sdes_sex[[s]], oc$y, "flush_now", models[[mdl]], oc$fam, oc$or))
  }
}
main_cells <- rbindlist(main_cells); main_tests <- rbindlist(main_tests); main_trend <- rbindlist(main_trend)

# 5-2. 부질문 1: 음주 수준 × 홍조 이력 3군(비홍조 기준) — 공동 검정, 소실 vs 비홍조, 지속 vs 비홍조, 소실 vs 지속(탐색)
hist_cells <- list(); hist_tests <- list(); hist_trend <- list()
for (s in sexes) for (mdl in c("M1 기본", "M2 확장")) for (oc in list(oc_bin, oc_con)) {
  if (oc$y == "uacid" && (s == "남자" || mdl != "M2 확장")) next
  a <- run_int(sdes_sex[[s]], oc$y, "drink3", "flush_hist", models[[mdl]], oc$fam, oc$or)
  hist_cells[[length(hist_cells) + 1]] <- cbind(outcome = oc$lab, sex = s, model = mdl, a$cells)
  f <- a$r$fit
  ct <- function(L, lab) cbind(outcome = oc$lab, sex = s, model = mdl, test = lab, n_model = a$r$n, contrast_test(f, L))
  hist_tests[[length(hist_tests) + 1]] <- rbind(
    cbind(outcome = oc$lab, sex = s, model = mdl, test = "음주 수준 × 홍조 이력 (공동, 4 df)", a$test),
    ct(list(c("drink3중음주:flush_hist홍조 소실" = 1), c("drink3고음주:flush_hist홍조 소실" = 1)), "소실 vs 비홍조: 음주 연관성 차이 (2 df)"),
    ct(list(c("drink3중음주:flush_hist지속 홍조" = 1), c("drink3고음주:flush_hist지속 홍조" = 1)), "지속 vs 비홍조: 음주 연관성 차이 (2 df)"),
    ct(list(c("drink3중음주:flush_hist홍조 소실" = 1, "drink3중음주:flush_hist지속 홍조" = -1),
            c("drink3고음주:flush_hist홍조 소실" = 1, "drink3고음주:flush_hist지속 홍조" = -1)), "소실 vs 지속: 음주 연관성 차이 (2 df, 탐색)"))
  hist_trend[[length(hist_trend) + 1]] <- cbind(outcome = oc$lab, sex = s, model = mdl, trend_table(sdes_sex[[s]], oc$y, "flush_hist", models[[mdl]], oc$fam, oc$or))
}
hist_cells <- rbindlist(hist_cells); hist_tests <- rbindlist(hist_tests); hist_trend <- rbindlist(hist_trend)
# 보조: 신규 홍조를 포함한 4군 (Yook 2024 분류)
grp4 <- rbindlist(lapply(sexes, function(s) {
  a <- run_int(sdes_sex[[s]], "hua", "drink3", "flush_grp", cov_ext, quasibinomial(), TRUE)
  cbind(outcome = "고요산혈증(OR)", sex = s, model = "M2 확장", p_interaction_6df = a$test$p, a$cells)
}))

# 5-3. 부질문 2: 성별 차이 — 3원 상호작용(음주 × 현재 홍조 × 성별)과 홍조군 안에서의 음주 × 성별
sexdiff <- rbindlist(lapply(c("M1 기본", "M2 확장"), function(mdl) rbindlist(lapply(list(oc_bin, oc_con), function(oc) {
  r3 <- fit_svy(sdes, as.formula(paste(oc$y, "~ drink3 * flush_now * f_sex +", paste(models[[mdl]], collapse = " + "))), oc$fam)
  out <- cbind(outcome = oc$lab, model = mdl, test = "음주 수준 × 현재 홍조 × 성별 (3원, 2 df)", n_model = r3$n, term_test(r3$fit, "drink3:flush_now:f_sex"))
  for (m in levels(d_sub$flush_now)) {
    r2 <- fit_svy(dsub(sdes, d_sub$flush_now == m), as.formula(paste(oc$y, "~ drink3 * f_sex +", paste(models[[mdl]], collapse = " + "))), oc$fam)
    out <- rbind(out, cbind(outcome = oc$lab, model = mdl, test = paste0("음주 수준 × 성별 (", m, " 안에서, 2 df)"), n_model = r2$n, term_test(r2$fit, "drink3:f_sex")))
  }
  out
}))))

# 5-4. 여성 폐경 전·후 (탐색, M2): 층별 음주 × 현재 홍조, 3원 상호작용(음주 × 홍조 × 폐경)
sdes_w <- sdes_sex[["여자"]]; dw <- dat_of(sdes_w)
meno_cells <- list(); meno_tests <- list()
for (oc in list(oc_bin, oc_con)) {
  for (m in levels(dw$meno)) {
    a <- run_int(dsub(sdes_w, !is.na(dw$meno) & dw$meno == m), oc$y, "drink3", "flush_now", cov_ext, oc$fam, oc$or)
    meno_cells[[length(meno_cells) + 1]] <- cbind(outcome = oc$lab, menopause = m, model = "M2 확장", a$cells)
    meno_tests[[length(meno_tests) + 1]] <- cbind(outcome = oc$lab, menopause = m, test = "음주 수준 × 현재 홍조 (2 df)", a$test)
  }
  r3 <- fit_svy(sdes_w, as.formula(paste(oc$y, "~ drink3 * flush_now * meno +", paste(cov_ext, collapse = " + "))), oc$fam)
  meno_tests[[length(meno_tests) + 1]] <- cbind(outcome = oc$lab, menopause = "폐경 변수 있는 여성 전체", test = "음주 수준 × 현재 홍조 × 폐경 (3원, 2 df)",
                                                n_model = r3$n, term_test(r3$fit, "drink3:flush_now:meno"))
}
meno_cells <- rbindlist(meno_cells); meno_tests <- rbindlist(meno_tests)

# ==== 6. 민감도 분석 (M2 확장, 현재 홍조, 남녀 각각) =========================
sens_specs <- list(
  list(id = "S0",  label = "주분석(M2 확장) — 비교 기준", inc = "inc", expo = "drink3", mod = "flush_now", covs = cov_ext),
  list(id = "S1",  label = "통풍 의사진단자 포함(모름·무응답은 제외)", inc = "inc_s1", expo = "drink3", mod = "flush_now", covs = cov_ext),
  list(id = "S2",  label = "통풍 문항 모름·무응답 포함(진단자는 제외)", inc = "inc_s2", expo = "drink3", mod = "flush_now", covs = cov_ext),
  list(id = "S3a", label = "대표값 변경: 월1회미만=월0.25회, 주4회이상=주4회", inc = "inc", expo = "drink3_b", mod = "flush_now", covs = cov_ext),
  list(id = "S3b", label = "대표값 변경: 월1회미만=월0.75회, 주4회이상=주7회", inc = "inc", expo = "drink3_c", mod = "flush_now", covs = cov_ext),
  list(id = "S4",  label = "2020년 제외", inc = "inc_no2020", expo = "drink3", mod = "flush_now", covs = cov_ext),
  list(id = "S5",  label = "노출 = 폭음 빈도(남 7잔·여 5잔 이상; 없음/월1회 이하/주1회 이상)", inc = "inc", expo = "binge3", mod = "flush_now", covs = cov_ext),
  list(id = "S6",  label = "현재 홍조 = '자주·늘'만, '가끔'은 제외 (Kang & Oh 2023 방식)", inc = "inc", expo = "drink3", mod = "flush_now_ko", covs = cov_ext),
  list(id = "S7",  label = "노출 = 성별 가중 삼분위(T1 기준; 계획서 방법론 문구)", inc = "inc", expo = "drink_t3", mod = "flush_now", covs = cov_ext),
  list(id = "S8",  label = "고혈압·당뇨병을 의사진단(DI1_dg·DE1_dg)으로 보정", inc = "inc_s8", expo = "drink3", mod = "flush_now", covs = cov_ext_dx),
  list(id = "S10", label = "노출 = 음주 빈도 2군(월1회 이하 vs 초과; Kang & Oh 2023 방식)", inc = "inc", expo = "drink_freq2", mod = "flush_now", covs = cov_ext)
)
dd_all <- dat_of(des)
sens <- rbindlist(lapply(sens_specs, function(sp) rbindlist(lapply(1:2, function(sx) {
  a <- run_int(dsub(des, dd_all[[sp$inc]] == 1 & dd_all$sex == sx), "hua", sp$expo, sp$mod, sp$covs, quasibinomial(), TRUE)
  cbind(id = sp$id, analysis = sp$label, sex = sexes[sx], p_interaction = a$test$p,
        a$cells[, .(n_model, modifier, exposure, n, n_event, strat_est, strat_lo, strat_hi, strat_p, reri, reri_lo, reri_hi)])
}))))
# 보조: 홍조군별로 완전히 분리한 모형(공변량 효과도 홍조군마다 다르게; Kang & Oh 2023의 층화 방식)
strat_supp <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(levels(d_sub$flush_now), function(m) {
  ds <- dsub(sdes_sex[[s]], dat_of(sdes_sex[[s]])$flush_now == m)
  r  <- fit_svy(ds, as.formula(paste("hua ~ drink3 +", paste(cov_ext, collapse = " + "))), quasibinomial())
  tb <- tidy(r$fit, "^drink3", TRUE); tb[, term := sub("^drink3", "", term)]
  tb[, n_cat := as.integer(table(r$data$drink3)[term])]
  cbind(sex = s, flush = m, model = "M2 확장(홍조군별 별도 모형)", n_model = r$n, n_ref = as.integer(table(r$data$drink3)["저음주"]), tb)
}))))

# ==== 6-2. 모형 구성(2026-11-09 회의 결정): Crude → Model 1 → Model 2 → Model 3 ==============
# Crude   = 음주 수준 + 현재 홍조 (+ 상호작용항)만, 보정변수 없음
# Model 1 = Crude + 연령                                   (앞의 M0 연령보정과 같은 모형)
# Model 2 = Model 1 + 교육·소득·조사연도·흡연·신체활동      (앞의 M1 기본과 같은 모형)
# Model 3 = Model 2 + 체질량지수·고혈압·당뇨병·eGFR         (앞의 M2 확장과 같은 모형)
# 네 모형 모두 같은 대상(보정변수가 모두 있는 최종 분석 대상)에 적합하므로, OR의 변화는 대상이 아니라 보정 때문이다.
models4 <- list("Crude" = character(0), "Model 1" = cov_age, "Model 2" = cov_base, "Model 3" = cov_ext)
or_ci <- function(fit, nm) {             # 계수 합의 OR, 95% CI, p
  b <- coef(fit); V <- vcov(fit); stopifnot(all(nm %in% names(b)))
  l <- setNames(rep(0, length(b)), names(b)); l[nm] <- 1
  est <- sum(l * b); se <- sqrt(as.numeric(t(l) %*% V %*% l))
  c(est = exp(est), lo = exp(est - z975 * se), hi = exp(est + z975 * se), p = 2 * pnorm(-abs(est / se)))
}
# 한 모형의 핵심 수치: 음주의 주효과 OR(상호작용항 없는 모형), 홍조군별 OR(상호작용 모형), 홍조의 주효과 OR, 상호작용 p
summ_models <- function(design, covs) {
  fm <- fit_svy(design, mk_form("hua", c("drink3", "flush_now"), covs), quasibinomial())
  fi <- fit_svy(design, mk_form("hua", "drink3 * flush_now", covs), quasibinomial())
  fl <- or_ci(fm$fit, "flush_now홍조")
  out <- rbindlist(lapply(c("중음주", "고음주"), function(e) {
    a  <- or_ci(fm$fit, paste0("drink3", e)); b0 <- or_ci(fi$fit, paste0("drink3", e))
    b1 <- or_ci(fi$fit, c(paste0("drink3", e), paste0("drink3", e, ":flush_now홍조")))
    data.table(exposure = e, main_or = a[["est"]], main_lo = a[["lo"]], main_hi = a[["hi"]], main_p = a[["p"]],
               nonflush_or = b0[["est"]], nonflush_lo = b0[["lo"]], nonflush_hi = b0[["hi"]],
               flush_or = b1[["est"]], flush_lo = b1[["lo"]], flush_hi = b1[["hi"]])
  }))
  cbind(n_model = fm$n, out, flush_main_or = fl[["est"]], flush_main_lo = fl[["lo"]], flush_main_hi = fl[["hi"]],
        p_interaction = term_test(fi$fit, "drink3:flush_now")$p)
}
# (1) 네 모형 비교
four <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(names(models4), function(mdl)
  cbind(sex = s, model = mdl, covariates = if (length(models4[[mdl]])) paste(models4[[mdl]], collapse = " + ") else "(없음)",
        summ_models(sdes_sex[[s]], models4[[mdl]]))))))
four[, `:=`(main_pct_vs_crude = 100 * (main_or / main_or[model == "Crude"] - 1),
            nonflush_pct_vs_crude = 100 * (nonflush_or / nonflush_or[model == "Crude"] - 1),
            flush_pct_vs_crude = 100 * (flush_or / flush_or[model == "Crude"] - 1)), by = .(sex, exposure)]
# (2) 상호작용의 두 척도(곱셈: OR의 비 = exp(상호작용 계수), 덧셈: RERI)를 네 모형에서 나란히
int4 <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(names(models4), function(mdl) {
  a <- run_int(sdes_sex[[s]], "hua", "drink3", "flush_now", models4[[mdl]], quasibinomial(), TRUE)
  cc <- a$cells; f <- a$r$fit; ci <- confint(f); ct <- summary(f)$coefficients
  rbindlist(lapply(c("중음주", "고음주"), function(e) {
    nm  <- paste0("drink3", e, ":flush_now홍조"); x11 <- cc[modifier == "홍조" & exposure == e]
    j10 <- cc[modifier == "비홍조" & exposure == e, joint_est]; j01 <- cc[modifier == "홍조" & exposure == "저음주", joint_est]
    data.table(sex = s, model = mdl, exposure = e, n_model = a$r$n,
               or10_nonflush_drink = j10, or01_flush_lowdrink = j01, or11_observed = x11$joint_est,
               expected_additive = j10 + j01 - 1, expected_multiplicative = j10 * j01,
               reri = x11$reri, reri_lo = x11$reri_lo, reri_hi = x11$reri_hi, reri_p = x11$reri_p,
               ratio_of_or = unname(exp(coef(f)[nm])), ratio_lo = unname(exp(ci[nm, 1])), ratio_hi = unname(exp(ci[nm, 2])),
               p_product_term = unname(ct[nm, ncol(ct)]), p_interaction_joint = a$test$p,
               strat_or_nonflush = j10, strat_or_flush = x11$strat_est, naive_diff_of_stratified = x11$strat_est - j10)
  }))
}))))
# (3) 변수를 하나씩 넣었을 때의 OR 변화 (change-in-estimate)
single_covs <- c("연령" = "age", "교육수준" = "f_edu", "소득" = "f_incm", "조사연도" = "f_year", "현재흡연" = "f_smk", "유산소 신체활동" = "f_pa",
                 "체질량지수" = "he_bmi", "고혈압 유병" = "f_htn", "당뇨병 유병" = "f_dm", "eGFR" = "egfr10")
stopifnot(setequal(unname(single_covs), cov_ext))
chg <- rbindlist(lapply(sexes, function(s) {
  ds <- sdes_sex[[s]]
  A <- rbindlist(c(list(cbind(series = "A. Crude + 변수 1개", step = "Crude(기준)", summ_models(ds, character(0)))),
                   lapply(names(single_covs), function(nm) cbind(series = "A. Crude + 변수 1개", step = paste0("+ ", nm), summ_models(ds, single_covs[[nm]])))))
  B <- rbindlist(c(list(cbind(series = "B. Model 2 + 임상 변수 1개", step = "Model 2(기준)", summ_models(ds, cov_base))),
                   lapply(names(single_covs)[7:10], function(nm) cbind(series = "B. Model 2 + 임상 변수 1개", step = paste0("+ ", nm),
                                                                       summ_models(ds, c(cov_base, single_covs[[nm]]))))))
  C <- rbindlist(lapply(0:length(single_covs), function(k) cbind(series = "C. 누적(제안 순서)", step = if (k == 0) "Crude(기준)" else paste0("+ ", names(single_covs)[k]),
                                                                  summ_models(ds, unname(single_covs[seq_len(k)])))))
  cbind(sex = s, rbind(A, B, C))
}))
chg[, `:=`(main_pct = 100 * (main_or / main_or[1] - 1), nonflush_pct = 100 * (nonflush_or / nonflush_or[1] - 1),
           flush_pct = 100 * (flush_or / flush_or[1] - 1)), by = .(sex, series, exposure)]
# 넣는 순서를 뒤집어도 최종 모형(Model 3)의 OR은 같다는 확인
ord_chk <- rbindlist(lapply(sexes, function(s) {
  fwd <- summ_models(sdes_sex[[s]], cov_ext); rev_ <- summ_models(sdes_sex[[s]], rev(cov_ext))
  stopifnot(isTRUE(all.equal(fwd$main_or, rev_$main_or, tolerance = 1e-8)), isTRUE(all.equal(fwd$flush_or, rev_$flush_or, tolerance = 1e-8)))
  data.table(sex = s, exposure = fwd$exposure, main_or_forward = fwd$main_or, main_or_reverse = rev_$main_or)
}))
# (4) 두 분석 경로의 규칙 차이가 결과에 주는 영향 (진행 기록 문서: 흡연 3범주, 임신·수유 여성 제외)
rule_specs <- list(
  list(id = "R0", label = "현재 분석: 현재흡연 2범주, 임신·수유 여성 포함", inc = "inc", smk = "f_smk"),
  list(id = "R1", label = "흡연 3범주(비흡연·과거·현재)", inc = "inc", smk = "f_smk3"),
  list(id = "R2", label = "임신·수유 여성 제외", inc = "inc_nopreg", smk = "f_smk"),
  list(id = "R3", label = "진행 기록 규칙: 흡연 3범주 + 임신·수유 제외", inc = "inc_nopreg", smk = "f_smk3"))
rules <- rbindlist(lapply(rule_specs, function(sp) rbindlist(lapply(1:2, function(sx) rbindlist(lapply(c("Model 2", "Model 3"), function(mdl) {
  covs <- sub("^f_smk$", sp$smk, models4[[mdl]])
  cbind(id = sp$id, rule = sp$label, sex = sexes[sx], model = mdl, summ_models(dsub(des, dd_all[[sp$inc]] == 1 & dd_all$sex == sx), covs))
}))))))
rule_counts <- rbind(
  rbindlist(lapply(sexes, function(s) {
    dat <- dat_of(sdes_sex[[s]]); est <- svymean(~f_smk3, sdes_sex[[s]]); lv <- levels(dat$f_smk3)
    data.table(item = "흡연 3범주(최종 분석 대상)", sex = s, category = lv, n = as.integer(table(dat$f_smk3)[lv]), wt_pct = 100 * unname(coef(est)[paste0("f_smk3", lv)]))
  })),
  data.table(item = "임신·수유(최종 분석 대상 여자)", sex = "여자",
             category = c("임신중(LW_ms = 3)", "출산후 수유 중(LW_ms = 4)", "검진 당일 임신(HE_prg = 1)", "제외 대상 합계", "제외 후 전체 분석 대상"),
             n = c(d_sub[f_sex == "여자", sum(lw_ms %in% 3)], d_sub[f_sex == "여자", sum(lw_ms %in% 4)], d_sub[f_sex == "여자", sum(he_prg %in% 1)],
                   d_sub[, sum(preg_lact)], d_sub[, sum(preg_lact == 0L)]), wt_pct = NA_real_))

# ---- 저장 (반올림) ----
round_dt <- function(dt) {
  dt <- copy(dt)
  for (nm in names(dt)) if (is.double(dt[[nm]])) dt[[nm]] <- if (grepl("(^p$|^p_|_p$|p_value|p_trend|p_interaction)", nm)) signif(dt[[nm]], 3) else round(dt[[nm]], 4)
  dt
}
outs <- list("05a_main_joint_effects.csv" = main_cells, "05b_main_interaction_tests.csv" = main_tests, "05c_main_trend.csv" = main_trend,
             "06a_history_joint_effects.csv" = hist_cells, "06b_history_tests.csv" = hist_tests, "06c_history_trend.csv" = hist_trend,
             "06d_history_4groups_supp.csv" = grp4, "07_sex_difference_tests.csv" = sexdiff,
             "08a_women_menopause_cells.csv" = meno_cells, "08b_women_menopause_tests.csv" = meno_tests,
             "09_sensitivity.csv" = sens, "10_supp_stratified_models.csv" = strat_supp,
             "12a_four_models.csv" = four, "12b_four_models_interaction_scales.csv" = int4, "13_change_in_estimate.csv" = chg,
             "13b_order_check.csv" = ord_chk, "14a_rule_variants.csv" = rules, "14b_rule_counts.csv" = rule_counts)
for (nm in names(outs)) fwrite(round_dt(outs[[nm]]), file.path(out_dir, nm), bom = TRUE)

# ==== 7-1. 공식 통계 대조표 =================================================
# 단년도(2019) wt_itvex로 만 19세 이상 고위험음주율·월간음주율(지침서 정의, 8기 PDF p.48 / 인쇄 p.40)을 재현한다.
# 공식 조율값은 출처를 찾지 못해 NA. 국민건강통계 2019의 표 번호·쪽과 함께 채워야 한다(2주차 과제).
# 주의: 언론의 "2019년 고위험음주율 14.1%"는 지역사회건강조사 값, e-나라지표·PHWR 값은 연령표준화 값이라 조율 추정치와 비교하면 안 된다.
d[, dr_high := NA_integer_]
d[(sex == 1 & bd2_31 %in% c(1:5, 8)) | (sex == 2 & bd2_32 %in% c(1:5, 8)),
  dr_high := as.integer((sex == 1 & bd1_11 %in% 5:6 & bd2_1 %in% 4:5) | (sex == 2 & bd1_11 %in% 5:6 & bd2_1 %in% 3:5))]
d[, dr_month := NA_integer_]
d[bd1 %in% 1:2 & bd1_11 %in% c(1:6, 8), dr_month := as.integer(bd1 == 2 & bd1_11 %in% 3:6)]
official <- list()
for (ind in list(list(nm = "고위험음주율", v = "dr_high"), list(nm = "월간음주율", v = "dr_month")))
  for (sx in list(list(lab = "전체", sel = NULL), list(lab = "남자", sel = 1), list(lab = "여자", sel = 2)))
    official[[length(official) + 1]] <- list(indicator = paste0("만19세 이상 ", ind$nm, "(%), 2019, 조율(crude), ", sx$lab), year = 2019,
                                             sex_sel = sx$sel, var = ind$v, scale = 100, official = NA_real_,
                                             source = "지표 정의: 8기 PDF p.48 / 인쇄 p.40. 공식 조율값 출처 미확인(국민건강통계 2019 필요)")
check <- rbindlist(lapply(official, function(o) {
  dd <- d[year == o$year]
  dd[, `:=`(pop_flag = as.integer(age >= 19 & !is.na(wt_itvex) & (is.null(o$sex_sel) | sex %in% o$sex_sel)), ind_val = get(o$var))]
  dd[, w1 := fifelse(!is.na(wt_itvex), wt_itvex, 0)]
  dz <- subset(svydesign(ids = ~psu, strata = ~kstrata, weights = ~w1, nest = TRUE, data = as.data.frame(dd)), pop_flag == 1)
  est <- svymean(~ind_val, dz, na.rm = TRUE); ci <- confint(est)
  data.table(indicator = o$indicator, n_unweighted = dd[pop_flag == 1 & !is.na(ind_val), .N],
             estimate = o$scale * unname(coef(est)), se = o$scale * as.numeric(SE(est)),
             ci_low = o$scale * unname(ci[1]), ci_high = o$scale * unname(ci[2]), official = o$official, source = o$source)
}))
check[, diff := estimate - official][, within_ci := official >= ci_low & official <= ci_high]
if (any(is.na(check$official))) warning("공식 통계값이 비어 있습니다. 국민건강통계 2019에서 조율값을 찾아 채우세요.")
fwrite(round_dt(check), file.path(out_dir, "11_official_check.csv"), bom = TRUE)

# ==== 7-2. 재현성 기록 ======================================================
fwrite(data.table(file = files, size = file.size(files), md5 = unname(tools::md5sum(files))), file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "_sessionInfo.txt"))

# ==== 8. 결과 요약(markdown) — 수치는 모두 위 객체에서 가져온다 =============
est_ci <- function(e, lo, hi, dgt = 2) ifelse(is.na(e), "", sprintf(paste0("%.", dgt, "f (%.", dgt, "f–%.", dgt, "f)"), e, lo, hi))
md_tab <- function(dt) {
  x <- copy(dt)
  for (nm in names(x)) {
    v <- x[[nm]]
    x[[nm]] <- if (is.numeric(v)) {
      if (grepl("(^p$|^p_|_p$|p_value|p_trend|p_interaction)", nm)) fmt_p(v)
      else if (is.integer(v) || isTRUE(all(v[!is.na(v)] == round(v[!is.na(v)])))) ifelse(is.na(v), "", trimws(format(v, big.mark = ",", scientific = FALSE)))
      else ifelse(is.na(v), "", formatC(v, format = "f", digits = 2))
    } else ifelse(is.na(v), "", as.character(v))
  }
  c(paste0("| ", paste(names(x), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|"),
    apply(as.matrix(x), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")), "")
}
kv_table <- function(cells, or = TRUE) {   # Knol & VanderWeele 형식의 축약 표 (기준 칸은 '기준'으로 표시)
  ref <- if (or) 1 else 0
  ec  <- function(e, lo, hi) ifelse(!is.na(e) & e == ref & lo == ref & hi == ref, "기준", est_ci(e, lo, hi))
  x <- cells[, .(홍조 = modifier, 음주 = exposure, n = n, 사례 = n_event,
                 `가중 유병률 %` = if (or) sprintf("%.1f", 100 * wt_mean) else sprintf("%.2f", wt_mean),
                 `결합효과(기준: 비홍조·저음주)` = ec(joint_est, joint_lo, joint_hi),
                 `층화효과(홍조군 안에서 vs 저음주)` = ec(strat_est, strat_lo, strat_hi),
                 `홍조 효과(음주 수준 안에서 vs 비홍조)` = ec(mod_est, mod_lo, mod_hi),
                 `RERI (95% CI)` = est_ci(reri, reri_lo, reri_hi))]
  if (!or) { x[, c("가중 유병률 %", "사례", "RERI (95% CI)") := NULL]
             setnames(x, c("결합효과(기준: 비홍조·저음주)", "층화효과(홍조군 안에서 vs 저음주)", "홍조 효과(음주 수준 안에서 vs 비홍조)"),
                      c("결합효과 β(기준: 비홍조·저음주)", "층화효과 β(홍조군 안에서 vs 저음주)", "홍조 효과 β(음주 수준 안에서 vs 비홍조)")) }
  x
}
md <- c("# 분석 결과 요약 — 음주 관련 안면홍조 이력에 따른 음주량–고요산혈증 연관성의 차이 (KNHANES 2019–2021)", "",
        "- 생성: code/13_v3_main_analysis.R (수치는 결과 CSV와 같은 객체에서 작성). 설계 근거: 00_reference_methods.md",
        "- 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu. n은 비가중 표본 수, %·평균·OR·β는 복합표본 가중 추정치.",
        "- OR은 단면자료의 유병 오즈비. RERI(덧셈 척도 상호작용) = OR11 − OR10 − OR01 + 1, 95% CI는 델타법. p는 양측.", "",
        "## 1. 대상자 선정 흐름", "", md_tab(flow), "발표 v5 흐름과 대조(가중치 단계 제외):", "", md_tab(flow_v5),
        "## 2. 칸별 비가중 n·고요산혈증 사례 수·가중 유병률", "", md_tab(cells[group_var == "flush_now", .(sex, 현재홍조 = group, 음주 = drink, n, n_hua, hua_pct_wt, hua_se_wt)]),
        md_tab(cells[group_var == "flush_grp", .(sex, 홍조패턴 = group, 음주 = drink, n, n_hua, hua_pct_wt, hua_se_wt)]),
        "## 3. 주분석: 음주 수준 × 현재 홍조 (고요산혈증 OR)", "")
for (s in sexes) for (mdl in names(models)) {
  md <- c(md, paste0("### ", s, " · ", mdl), "",
          md_tab(kv_table(main_cells[outcome == "고요산혈증(OR)" & sex == s & model == mdl])),
          paste0("- 음주 수준 × 현재 홍조 공동 Wald 검정 p = ", fmt_p(main_tests[outcome == "고요산혈증(OR)" & sex == s & model == mdl, p]),
                 " (n = ", main_tests[outcome == "고요산혈증(OR)" & sex == s & model == mdl, n_model], ")"),
          paste0("- P for trend(순서형 음주 수준, 한 단계당 OR): ",
                 paste(main_trend[outcome == "고요산혈증(OR)" & sex == s & model == mdl,
                                  paste0(modifier, " ", est_ci(per_level_est, ci_low, ci_high), ", p = ", fmt_p(p_trend))], collapse = " / "),
                 "; 추세 차이 p = ", fmt_p(main_trend[outcome == "고요산혈증(OR)" & sex == s & model == mdl, p_trend_x_modifier][1])), "")
}
md <- c(md, "## 4. 혈청 요산 연속형 (β, mg/dL)", "")
for (s in sexes) for (mdl in names(models)) {
  oc_lab <- if (s == "남자") "혈청 요산(β, mg/dL) [보조: 계획은 여성만]" else "혈청 요산(β, mg/dL)"
  z <- main_cells[outcome == oc_lab & sex == s & model == mdl]; if (!nrow(z)) next
  md <- c(md, paste0("### ", s, " · ", mdl, if (s == "남자") " (보조)" else ""), "", md_tab(kv_table(z, or = FALSE)),
          paste0("- 음주 수준 × 현재 홍조 공동 Wald 검정 p = ", fmt_p(main_tests[outcome == oc_lab & sex == s & model == mdl, p])), "")
}
md <- c(md, "## 5. 부질문 1: 홍조 이력 3군 (비홍조·소실·지속; 신규 제외)", "")
for (s in sexes) for (mdl in c("M1 기본", "M2 확장")) {
  z <- hist_cells[outcome == "고요산혈증(OR)" & sex == s & model == mdl]
  md <- c(md, paste0("### ", s, " · ", mdl, " (고요산혈증 OR)"), "", md_tab(kv_table(z)),
          md_tab(hist_tests[outcome == "고요산혈증(OR)" & sex == s & model == mdl, .(test, n_model, df, F, p)]),
          paste0("- P for trend: ", paste(hist_trend[outcome == "고요산혈증(OR)" & sex == s & model == mdl,
                                                     paste0(modifier, " ", est_ci(per_level_est, ci_low, ci_high), ", p = ", fmt_p(p_trend))], collapse = " / ")), "")
}
zw <- hist_cells[outcome == "혈청 요산(β, mg/dL)" & sex == "여자"]
if (nrow(zw)) md <- c(md, "### 여자 · M2 확장 (혈청 요산 β)", "", md_tab(kv_table(zw, or = FALSE)),
                      md_tab(hist_tests[outcome == "혈청 요산(β, mg/dL)" & sex == "여자", .(test, n_model, df, F, p)]))
md <- c(md, "### 보조: 신규 홍조를 포함한 4군 (M2, 고요산혈증 OR)", "",
        md_tab(grp4[, .(sex, 홍조패턴 = modifier, 음주 = exposure, n, 사례 = n_event, `층화 OR (95% CI)` = est_ci(strat_est, strat_lo, strat_hi), p_interaction_6df)]),
        "## 6. 부질문 2: 성별 차이", "", md_tab(sexdiff[, .(outcome, model, test, n_model, df, F, p)]),
        "## 7. 여성 폐경 전·후 (탐색, M2)", "")
for (oc in list(oc_bin, oc_con)) for (m in levels(dw$meno)) {
  z <- meno_cells[outcome == oc$lab & menopause == m]
  md <- c(md, paste0("### ", m, " · ", oc$lab), "", md_tab(kv_table(z, or = oc$or)),
          paste0("- 음주 수준 × 현재 홍조 공동 Wald 검정 p = ", fmt_p(meno_tests[outcome == oc$lab & menopause == m, p]), " (n = ", meno_tests[outcome == oc$lab & menopause == m, n_model], ")"), "")
}
md <- c(md, md_tab(meno_tests[grepl("3원", test), .(outcome, test, n_model, df, F, p)]),
        "## 8. 민감도 분석 (M2, 홍조군 안에서 저음주(기준) 대비 층화 OR)", "",
        md_tab(sens[exposure != levels(d_sub$drink3)[1] | id %in% c("S5", "S7", "S10"), .(id, analysis, sex, modifier, exposure, n, n_event,
                     `층화 OR (95% CI)` = est_ci(strat_est, strat_lo, strat_hi), strat_p, `RERI (95% CI)` = est_ci(reri, reri_lo, reri_hi), p_interaction)][!(exposure %in% c("저음주", "없음", "T1", "월1회 이하"))]),
        "## 9. 공식 통계 대조 (2019년 단년도, wt_itvex)", "", md_tab(check[, .(indicator, n_unweighted, estimate, se, ci_low, ci_high, official, within_ci)]),
        "## 10. 가정과 확인하지 못한 것", "",
        "- 고요산혈증 기준(남 ≥7.0, 여 ≥6.0)과 eGFR 식(CKD-EPI 2009)은 지침서에 없는 문헌 기준(V3 결정).",
        "- 음주 빈도·1회량의 대표값은 V3 환산표(열린 범주 대표값은 민감도 S3a·S3b로 확인).",
        "- 요산 '<1.0' 표기(HE_Uacid 결측)는 발표 v5처럼 결측 처리.",
        "- 공식 통계 조율값은 출처를 찾지 못해 비어 있음. 국민건강통계 2019에서 채워야 대조가 끝난다.",
        "- 여성 홍조·고음주, 홍조 소실·고음주 칸은 사례 수가 적어 신뢰구간이 넓다(탐색적 해석).")
con <- file(file.path(out_dir, "RESULTS_summary.md"), open = "w", encoding = "UTF-8"); writeLines(md, con); close(con)

# ==== 9. 모형 구성·보정 전후 비교·상호작용 방식·규칙 차이 검토 메모(markdown) ================
ci2 <- function(e, l, h) ifelse(is.na(e), "", ifelse(l < 0 | h < 0, sprintf("%.2f (%.2f, %.2f)", e, l, h), sprintf("%.2f (%.2f–%.2f)", e, l, h)))
pc  <- function(x) ifelse(is.na(x), "", sprintf("%+.1f%%", x))
big2 <- function(x) format(x, big.mark = ",", trim = TRUE)
chg_tab <- function(ser) chg[series == ser & exposure == "고음주",
  .(성별 = sex, 단계 = step, `주효과 OR (95% CI)` = ci2(main_or, main_lo, main_hi), `기준 대비 변화` = pc(main_pct),
    `비홍조군 OR` = ci2(nonflush_or, nonflush_lo, nonflush_hi), `비홍조 변화` = pc(nonflush_pct),
    `홍조군 OR` = ci2(flush_or, flush_lo, flush_hi), `홍조 변화` = pc(flush_pct), `상호작용 p` = fmt_p(p_interaction))]
top_mover <- function(ser) chg[series == ser & exposure == "고음주" & !grepl("기준", step), .SD[which.max(abs(main_pct))], by = sex]
tA <- top_mover("A. Crude + 변수 1개"); tB <- top_mover("B. Model 2 + 임상 변수 1개")
g4 <- function(s, m, e = "고음주") four[sex == s & model == m & exposure == e]
mm <- c("# 모형 구성, 보정 전후 OR 비교, 상호작용 방식, 규칙 차이 검토 (2026-11-09 회의 후속)", "",
        "- 생성: code/13_v3_main_analysis.R (수치는 12a·12b·13·14a·14b CSV와 같은 객체에서 작성)",
        "- 대상: 최종 분석 대상(보정변수가 모두 있는 사람). 네 모형이 모두 같은 사람으로 적합되므로 OR의 변화는 보정 때문이다.",
        "- OR은 저음주(<3.5잔/주) 대비 고요산혈증의 유병 오즈비, 괄호는 95% 신뢰구간. n은 비가중, 추정은 복합표본 가중(wt_itvex × 1/3, kstrata, psu).", "",
        "## 1. 모형 구성 (확정안)", "",
        md_tab(data.table(모형 = names(models4),
                          `넣는 변수` = c("음주 수준, 현재 홍조, 음주 × 홍조 상호작용항만 (보정변수 없음)", "Crude + 연령",
                                          "Model 1 + 교육수준, 소득, 조사연도, 현재흡연, 유산소 신체활동", "Model 2 + 체질량지수, 고혈압 유병, 당뇨병 유병, eGFR"),
                          `기존 보고서의 이름` = c("(없었음, 새로 추가)", "M0 연령보정", "M1 기본", "M2 확장"),
                          성격 = c("보정 전", "최소 보정", "교란변수 보정(주 모형 권고)", "임상 지표까지 보정(민감도 권고)"))),
        "## 2. 보정 전후 OR 비교", "",
        "주효과 OR은 상호작용항 없이 음주 수준과 홍조를 함께 넣은 모형의 값이고, 홍조군별 OR은 상호작용 모형에서 구한 값이다. 변화율은 Crude 대비 OR의 변화(%).", "",
        md_tab(four[, .(성별 = sex, 모형 = model, `음주 수준` = exposure, `주효과 OR (95% CI)` = ci2(main_or, main_lo, main_hi), `주효과 변화` = pc(main_pct_vs_crude),
                        `비홍조군 OR (95% CI)` = ci2(nonflush_or, nonflush_lo, nonflush_hi), `비홍조 변화` = pc(nonflush_pct_vs_crude),
                        `홍조군 OR (95% CI)` = ci2(flush_or, flush_lo, flush_hi), `홍조 변화` = pc(flush_pct_vs_crude), `상호작용 p` = fmt_p(p_interaction))]),
        "홍조 자체의 OR(비홍조 대비, 주효과 모형):", "",
        md_tab(unique(four[, .(성별 = sex, 모형 = model, `홍조 OR (95% CI)` = ci2(flush_main_or, flush_main_lo, flush_main_hi))])),
        sprintf("- 고음주의 주효과 OR은 남자 Crude %s에서 Model 3 %s로, 여자 Crude %s에서 Model 3 %s로 바뀌었다.",
                ci2(g4("남자", "Crude")$main_or, g4("남자", "Crude")$main_lo, g4("남자", "Crude")$main_hi), ci2(g4("남자", "Model 3")$main_or, g4("남자", "Model 3")$main_lo, g4("남자", "Model 3")$main_hi),
                ci2(g4("여자", "Crude")$main_or, g4("여자", "Crude")$main_lo, g4("여자", "Crude")$main_hi), ci2(g4("여자", "Model 3")$main_or, g4("여자", "Model 3")$main_lo, g4("여자", "Model 3")$main_hi)),
        sprintf("- 상호작용 p는 남자 %s, 여자 %s로(Crude, Model 1, Model 2, Model 3 순) 어느 모형에서도 0.05보다 작지 않았다.",
                paste(fmt_p(unique(four[sex == "남자", .(model, p_interaction)])$p_interaction), collapse = " · "),
                paste(fmt_p(unique(four[sex == "여자", .(model, p_interaction)])$p_interaction), collapse = " · ")), "",
        "## 3. 변수를 하나씩 넣었을 때 (고음주 OR의 변화)", "",
        "### A. Crude에 변수 하나만 넣기", "", md_tab(chg_tab("A. Crude + 변수 1개")),
        sprintf("- Crude의 고음주 주효과 OR을 가장 크게 움직인 변수: 남자 %s(%s), 여자 %s(%s).", sub("^\\+ ", "", tA[sex == "남자", step]), pc(tA[sex == "남자", main_pct]),
                sub("^\\+ ", "", tA[sex == "여자", step]), pc(tA[sex == "여자", main_pct])), "",
        "### B. Model 2에 임상 변수 하나만 넣기", "", md_tab(chg_tab("B. Model 2 + 임상 변수 1개")),
        sprintf("- Model 2의 고음주 주효과 OR을 가장 크게 움직인 임상 변수: 남자 %s(%s), 여자 %s(%s).", sub("^\\+ ", "", tB[sex == "남자", step]), pc(tB[sex == "남자", main_pct]),
                sub("^\\+ ", "", tB[sex == "여자", step]), pc(tB[sex == "여자", main_pct])), "",
        "### C. 제안 순서대로 하나씩 누적", "", md_tab(chg_tab("C. 누적(제안 순서)")),
        sprintf("- 넣는 순서를 뒤집어도 최종 모형의 OR은 같다(확인: 고음주 주효과 OR 남자 %.4f = %.4f, 여자 %.4f = %.4f). 하나씩 넣는 방식은 유의성을 높이지 않고, 어떤 변수가 OR을 움직이는지 보여 줄 뿐이다.",
                ord_chk[sex == "남자" & exposure == "고음주", main_or_forward], ord_chk[sex == "남자" & exposure == "고음주", main_or_reverse],
                ord_chk[sex == "여자" & exposure == "고음주", main_or_forward], ord_chk[sex == "여자" & exposure == "고음주", main_or_reverse]), "",
        "## 4. 상호작용의 두 척도 (고음주, 기준: 비홍조·저음주)", "",
        "OR10 = 비홍조·고음주, OR01 = 홍조·저음주, OR11 = 홍조·고음주의 결합효과 OR. 덧셈 기대값 = OR10 + OR01 − 1, 곱셈 기대값 = OR10 × OR01. RERI = OR11 − 덧셈 기대값, OR의 비 = OR11 ÷ 곱셈 기대값 = exp(상호작용 계수).", "",
        md_tab(int4[exposure == "고음주", .(성별 = sex, 모형 = model, OR10 = sprintf("%.2f", or10_nonflush_drink), OR01 = sprintf("%.2f", or01_flush_lowdrink),
                                            `관측 OR11` = sprintf("%.2f", or11_observed), `덧셈 기대값` = sprintf("%.2f", expected_additive), `곱셈 기대값` = sprintf("%.2f", expected_multiplicative),
                                            `RERI (95% CI)` = ci2(reri, reri_lo, reri_hi), `OR의 비 (95% CI)` = ci2(ratio_of_or, ratio_lo, ratio_hi),
                                            `상호작용항 p` = fmt_p(p_product_term), `공동 검정 p` = fmt_p(p_interaction_joint),
                                            `층화 OR의 단순 차이` = sprintf("%.2f", naive_diff_of_stratified))]),
        "- '층화 OR의 단순 차이'(홍조군 OR − 비홍조군 OR)는 두 OR의 기준 집단이 달라 덧셈 척도의 상호작용 지표가 아니다. RERI와 값이 다르다는 점을 보이려고 함께 적었다.", "",
        "## 5. 두 분석 경로의 규칙 차이", "",
        md_tab(rule_counts[, .(항목 = item, 성별 = sex, 범주 = category, n = big2(n), `가중 %` = ifelse(is.na(wt_pct), "", sprintf("%.1f", wt_pct)))]),
        md_tab(rules[exposure == "고음주", .(ID = id, 규칙 = rule, 성별 = sex, 모형 = model, `모형 n` = big2(n_model), `주효과 OR (95% CI)` = ci2(main_or, main_lo, main_hi),
                                             `비홍조군 OR (95% CI)` = ci2(nonflush_or, nonflush_lo, nonflush_hi), `홍조군 OR (95% CI)` = ci2(flush_or, flush_lo, flush_hi),
                                             `상호작용 p` = fmt_p(p_interaction))]))
# 권고안과 문헌 검색 단서 (문장 속 수치도 모두 위 객체에서 가져온다)
gA <- function(s, st) chg[series == "A. Crude + 변수 1개" & exposure == "고음주" & sex == s & step == st, main_pct]
gB <- function(s, st) chg[series == "B. Model 2 + 임상 변수 1개" & exposure == "고음주" & sex == s & step == st, main_pct]
gR <- function(i, s, m = "Model 2") rules[id == i & sex == s & model == m & exposure == "고음주"]
orR <- function(i, s) ci2(gR(i, s)$main_or, gR(i, s)$main_lo, gR(i, s)$main_hi)
p_all <- range(c(four$p_interaction, chg$p_interaction, rules$p_interaction))
# 진행 기록 문서에 적힌 흡연 3범주 인원(남 비흡연·과거·현재, 여 비흡연·과거·현재)과 대조
smk_doc <- c(1456L, 2136L, 1819L, 4695L, 326L, 347L)
smk_match <- identical(as.integer(rule_counts[item == "흡연 3범주(최종 분석 대상)", n]), smk_doc)
n_preg <- d_sub[, sum(preg_lact)]
mm <- c(mm,
  "## 6. 권고안 (팀 확정 필요)", "",
  "1. **모형 구성**: 1절의 표대로 Crude, Model 1, Model 2, Model 3을 쓴다. 결과를 본 뒤 구성을 바꾸지 않는다.",
  sprintf("2. **주 모형은 Model 2, Model 3은 민감도 분석.** Model 2에 임상 변수를 하나씩 넣었을 때 고음주 OR을 가장 크게 움직인 변수는 남자에서 eGFR이었다(남자 %s, 여자 %s). eGFR은 요산과 서로 영향을 주고받고 음주가 크레아티닌에 영향을 줄 수 있어 교란변수로 단정하기 어렵다. 체질량지수·고혈압·당뇨병도 음주가 요산에 이르는 중간 경로일 수 있다. 그래서 교란변수만 넣은 Model 2를 주 결과로, 임상 지표까지 넣은 Model 3을 보정을 더 해도 결론이 같은지 보는 용도로 쓴다.",
          pc(gB("남자", "+ eGFR")), pc(gB("여자", "+ eGFR"))),
  sprintf("3. **상호작용은 상호작용항 공동 검정을 주 검정으로, RERI를 보조로 쓴다.** 두 척도의 결론이 같다. 상호작용 p는 네 모형, 변수를 하나씩 넣은 모든 단계, 네 가지 규칙에서 %.2f–%.2f였다.", p_all[1], p_all[2]),
  sprintf("4. **흡연은 3범주(비흡연·과거·현재)로 바꾸기를 권한다.** 과거흡연을 비흡연과 나누면 Model 2의 고음주 주효과 OR이 남자 %s에서 %s로, 여자 %s에서 %s로 달라진다. 진행 기록 문서의 흡연 3범주 인원과 이 자료의 인원은 %s.",
          orR("R0", "남자"), orR("R1", "남자"), orR("R0", "여자"), orR("R1", "여자"), if (smk_match) "모두 일치한다" else "일치하지 않는다(확인 필요)"),
  sprintf("5. **임신·수유 여성 제외는 채택해도 결과가 거의 같다.** 해당자는 %d명이고, 여자 Model 2의 고음주 주효과 OR은 %s에서 %s가 된다. 채택하면 최종 인원은 %s명이다.",
          n_preg, orR("R0", "여자"), orR("R2", "여자"), big2(d_sub[, sum(preg_lact == 0L)])),
  "6. **보정변수 목록**: 기본 6개(연령, 교육수준, 소득, 조사연도, 흡연, 유산소 신체활동)와 임상 4개(체질량지수, 고혈압, 당뇨병, eGFR).", "",
  "권고 4·5를 채택하면 주 분석을 그 규칙으로 다시 돌리고, 보고서의 모형 이름도 Crude·Model 1–3으로 바꿔야 한다. 이 문서의 2–4절 수치는 현재 규칙(R0) 기준이다.", "",
  "## 7. 문헌 검색에 쓸 단서", "",
  sprintf("- 남자: 고음주 OR을 올린 변수는 연령(Crude 기준 %s)과 eGFR(Model 2 기준 %s)이다. 연령별 고요산혈증·음주량 분포, 음주와 eGFR의 관계를 다룬 논문을 찾는다.",
          pc(gA("남자", "+ 연령")), pc(gB("남자", "+ eGFR"))),
  sprintf("- 여자: 흡연을 넣으면 OR이 내려가고(Crude 기준 %s) eGFR을 넣으면 올라간다(Crude 기준 %s). 여성의 흡연과 고요산혈증, 음주와 흡연의 동반을 다룬 논문을 찾는다.",
          pc(gA("여자", "+ 현재흡연")), pc(gA("여자", "+ eGFR"))),
  "- 공통: 보정 전후 OR을 함께 제시한 음주–고요산혈증 논문과 크기·방향을 비교한다.")
con <- file(file.path(out_dir, "MODEL_BUILDING_모형구성_검토.md"), open = "w", encoding = "UTF-8"); writeLines(mm, con); close(con)
cat("\n[네 모형 비교]\n"); print(four[, .(sex, model, exposure, main_or = round(main_or, 2), nonflush_or = round(nonflush_or, 2), flush_or = round(flush_or, 2), p_int = signif(p_interaction, 3))])

cat("\n[주분석 상호작용 검정]\n"); print(main_tests[, .(outcome, sex, model, n_model, p = signif(p, 3))])
cat("\n[홍조 이력 검정]\n"); print(hist_tests[, .(outcome, sex, model, test, p = signif(p, 3))])
cat("\n[성별 차이]\n"); print(sexdiff[, .(outcome, model, test, p = signif(p, 3))])
cat("\n완료:", out_dir, "\n")
