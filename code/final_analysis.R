# final_analysis.R — 최종 분석 코드(임시본)
# 주제: 음주 관련 안면홍조 이력에 따른 음주량–고요산혈증 연관성의 차이, 국민건강영양조사 2019–2021
# 근거 문서: 분석계획서.md(저장소 맨 위 폴더). 이 코드는 계획서의 규칙을 그대로 옮긴 것이며,
#            규칙을 바꾸려면 계획서를 먼저 고치고 이 코드의 해당 줄을 고친다. 줄 옆의 [계획서 n장]이 대응 위치다.
# 임시본: 계획서의 ✅ 확정 규칙과, 🔍 항목은 현재 규칙(code/13_v3_main_analysis.R, README 4장)으로 짰다.
#         ⏳ 항목(주분석 음주 기준, 1잔의 g, 주 검정 형태 A·B안)은 아래 0절의 설정값을 채우면 된다. 공식값 대조(8.1)는 ✅.
# 2026-11-18 수정(문서 점검 3·4·18·27번): 경향성 검정에도 '사례 10명 미만' 표시, 효과표 비고 열, 칸 표 가중 유병률.
# 2026-11-22 수정(계획서 1장 개정): 경향성 검정을 부질문 2·3으로 이름 바꾸고 기울기의 비를 효과표에 추가, 성별 3원 검정 삭제.
# 2026-11-23 주분석 확정(김준현 요청): 질문 구조 확정, 음주 기준 S2(남 16 / 여 8 g/일, 2군), 1잔 7 g → RUN_MAIN = TRUE.
#            흡연 보정은 3범주(비·과거·현재)로 확정.
#            경향성 분석(부질문 2·3)은 하지 않기로 결정: 고요산혈증(절단점)을 주결과로, 혈청 요산(연속형)은 보조 다중회귀.
#            민감도는 1잔 8·10 g과 대푯값 전체 하한·상한(L·U)만 한다. 통풍·다른 음주 기준 등은 실행 목록에서 뺐다.
#            보정 모형은 Crude·M2S(주 모형)·M3S 세 가지만 쓴다(같은 날 결정). S = 흡연 3범주.
# 실행: 프로젝트 폴더에서  Rscript --vanilla code/final_analysis.R
# 재현성 확인(코드를 짜지 않은 조원):
#   Rscript --vanilla .claude/skills/knhanes-analysis/scripts/check_reproducibility.R code/final_analysis.R output/final_analysis
# 원칙: 원시자료는 읽기만 한다. 대상자는 행을 지우지 않고 inc(1/0)로 표시한 뒤, 설계 객체에서 subset으로 고른다.
# 쪽수 표기: "8기 PDF 쪽 / 인쇄 쪽" (『국민건강영양조사 제8기(2019-2021) 원시자료 이용지침서』)
# 작성: 2026-11-18, 생성형 AI(Claude) 도움을 받아 code/13과 knhanes_ptest의 내용을 합쳐 정리. 조원 검토 필요.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
# ---- 주분석 스위치 [계획서 9장] ----
# 주분석 음주 기준(계획서 4.1)이 ✅ 확정되기 전에는 FALSE로 둔다. FALSE이면 8절(주분석·부질문·민감도)을 실행하지 않는다.
RUN_MAIN <- TRUE              # ✅ 2026-11-23 주분석 기준 확정

# ---- 음주 기준 후보 [계획서 4.1] ----
# 단위는 g/일. 1잔 = 7 g이면 g/일 = 주당 잔 수다. 절단점 a, b는 a 미만 / a 이상 b 미만 / b 이상.
# 다른 후보(S1 성별 삼분위, S3 5·15, S4 3.5·14, S5 연속형)는 2026-11-23 결정으로 실행하지 않는다.
# 그 결과는 ptest 기록(output/21_ptest/ptest_log.csv)에 남아 있어 보고서 부록의 '시도한 기준'으로 밝힌다.
CANDIDATES <- list(
  S2 = list(label = "S2 절주 지침 16/8",    drink = "cut",    male = 16,         female = 8)
)
MAIN_NAME  <- "S2"            # ✅ 2026-11-23: 주분석 기준 S2(Lee 2019 비홍조군 적정 상한, 남 16 / 여 8 g/일)
GRAM_MAIN  <- 7               # ✅ 2026-11-23: 1잔 = 7 g(계획서 4.1, 10장 2번)
GRAM_SENS  <- c(8, 10)        #    민감도 분석에 쓰는 다른 1잔 g 값

# ---- 보정 묶음 [계획서 4.4] ----
# meno_cov(폐경 상태)는 여자 모형에만 들어간다. 남녀 통합 모형에서는 여자 전용 지시변수(meno_post, meno_other)로 넣는다.
# ✅ 2026-11-23: 흡연은 3범주(비흡연·과거흡연·현재흡연, f_smk3)로 보정한다(계획서 10장 13번). 2범주는 민감도.
# ✅ 2026-11-23: 보정 모형은 Crude, M2S, M3S 세 가지만 쓴다(S = 흡연 3범주, ptest 표의 묶음 이름과 같음).
#   Crude: 보정 없음(보정 전후 비교) · M2S: 주 모형 · M3S: M2S + 대사 변수(매개변수일 수 있어 참고로 해석)
COVSETS <- list(
  "Crude" = character(0),
  "M2S"   = c("age", "f_edu", "f_incm", "f_year", "f_smk3", "f_pa", "meno_cov"),
  "M3S"   = c("age", "f_edu", "f_incm", "f_year", "f_smk3", "f_pa", "meno_cov", "he_bmi", "f_htn", "f_dm", "egfr10")
)
MAIN_COVSET <- "M2S"          # ✅ 주 모형

# ---- 공식 통계값 [계획서 8.1] ----
# 2019년 만 19세 이상 조율(crude) 값과 응답자 수. 연령표준화 값이 아니다.
# 출처: KOSIS 국민건강영양조사(질병관리청) 월간 음주율 추이 DT_11702_N012, 고위험음주율 추이 DT_11702_N018, '19세이상' 행, 2026-11-18 조회.
#       자세한 내용: Result/2026-11-18_2019음주_공식통계값_KOSIS.md
OFFICIAL <- data.table(
  indicator  = c("월간음주율", "월간음주율", "월간음주율", "고위험음주율", "고위험음주율", "고위험음주율"),
  sex        = c("전체", "남자", "여자", "전체", "남자", "여자"),
  official   = c(57.7, 72.4, 43.2, 12.1, 18.7, 5.6),
  official_n = c(6195L, 2746L, 3449L, 6195L, 2746L, 3449L),
  source     = c(rep("KOSIS DT_11702_N012(월간 음주율 추이), 2019, 19세이상", 3), rep("KOSIS DT_11702_N018(고위험음주율 추이), 2019, 19세이상", 3))
)
# 판정: 공식값이 소수 첫째 자리까지 발표되므로, 재현값을 소수 첫째 자리로 반올림한 값과 응답자 수가 모두 같으면 '일치'
OFFICIAL_DIGITS <- 1

# ---- 경로 ----
years   <- c(2019, 2020, 2021)
data_candidates <- file.path("data", c("국건영 조사결과", "law data"))   # 조원마다 원시자료 폴더 이름이 다르다
data_dir <- data_candidates[file.exists(file.path(data_candidates, "hn19_all.sas7bdat"))][1]
if (is.na(data_dir)) stop("원시자료(hn19_all.sas7bdat)를 찾지 못했습니다. 다음 폴더 중 하나에 두세요: ", paste(data_candidates, collapse = ", "))
files   <- file.path(data_dir, sprintf("hn%02d_all.sas7bdat", years %% 100))
out_dir <- file.path("output", "final_analysis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 설계 [계획서 2장] ----
# 가중치 wt_itvex (8기 PDF p.92 / 인쇄 p.84). 제8기는 조사기간 비례 통합이므로 각 연도 1/3 (8기 PDF p.36 / 인쇄 p.28, 표 7)
pool_ratio <- c("2019" = 1/3, "2020" = 1/3, "2021" = 1/3)
stopifnot(abs(sum(pool_ratio) - 1) < 1e-12)
MIN_EV <- 10                  # 고요산혈증 사례가 이보다 적은 칸이 있으면 해석하지 않는다 [계획서 7장]

# ==== 1. 변수 사전 [계획서 4장] =============================================
# miss: 결측으로 바꾸는 코드(모름·무응답). na8: 비해당 코드의 처리("결측" 또는 "범주유지"). 쪽수는 지침서.
src_b1  <- "8기 PDF p.91 / 인쇄 p.83"
src_b2  <- "8기 PDF p.92 / 인쇄 p.84"
src_drk <- "8기 PDF p.161-162 / 인쇄 p.153-154"
src_fl  <- "8기 PDF p.163-164 / 인쇄 p.155-156"
src_lab <- "8기 PDF p.200 / 인쇄 p.192"
src_smk <- "8기 PDF p.175 / 인쇄 p.167; 현재흡연 정의 PDF p.48 / 인쇄 p.40"
var_dict <- rbindlist(list(
  list(var = "id",       miss = "", na8 = "", label = "개인 아이디", source = src_b1),
  list(var = "year",     miss = "", na8 = "", label = "조사연도", source = src_b1),
  list(var = "sex",      miss = "", na8 = "", label = "성별(1 남, 2 여)", source = src_b1),
  list(var = "age",      miss = "", na8 = "", label = "만나이(80 = 80세 이상)", source = src_b1),
  list(var = "psu",      miss = "", na8 = "", label = "조사구번호(집락)", source = src_b1),
  list(var = "incm",     miss = "", na8 = "", label = "소득 4분위수(개인)", source = src_b1),
  list(var = "kstrata",  miss = "", na8 = "", label = "분산추정 층", source = src_b2),
  list(var = "wt_itvex", miss = "", na8 = "", label = "건강설문-검진조사 가중치", source = src_b2),
  list(var = "edu",      miss = "", na8 = "", label = "교육수준 재분류(1 초졸 이하 ~ 4 대졸 이상)", source = src_b2),
  list(var = "bd1",      miss = "9", na8 = "8=결측", label = "평생음주경험(1 없음, 2 있음)", source = src_drk),
  list(var = "bd1_11",   miss = "9", na8 = "8=범주유지", label = "1년간 음주빈도(1 안 마심 ~ 6 주 4회 이상)", source = src_drk),
  list(var = "bd2_1",    miss = "9", na8 = "8=범주유지", label = "한 번에 마시는 음주량(1 1-2잔 ~ 5 10잔 이상)", source = src_drk),
  list(var = "bd2_14",   miss = "999", na8 = "888=결측(변수 값만 비움. 10잔 미만이라 비해당이며 그 사람은 BD2_1 대푯값을 쓰므로 대상에서 빠지지 않음)", label = "한 번에 마시는 음주량(잔, 10잔 이상만)", source = src_drk),
  list(var = "bd2_31",   miss = "9", na8 = "8=범주유지", label = "폭음 빈도(남 7잔 이상)", source = src_drk),
  list(var = "bd2_32",   miss = "9", na8 = "8=범주유지", label = "폭음 빈도(여 5잔 이상)", source = src_drk),
  list(var = "bd9_1",    miss = "9", na8 = "8=결측", label = "과거 알코올 홍조(1 예, 2 아니오)", source = src_fl),
  list(var = "bd9_2",    miss = "9", na8 = "8=결측", label = "현재 알코올 홍조(1 아니오, 2 가끔, 3 자주, 4 늘)", source = src_fl),
  list(var = "dm8_dg",   miss = "9", na8 = "8=결측", label = "통풍 의사진단 여부(0 없음, 1 있음)", source = "8기 PDF p.137 / 인쇄 p.129"),
  list(var = "lw_ms",    miss = "9", na8 = "8=결측", label = "월경여부(1 초경 전, 2 월경 중, 3 임신 중, 4 수유 중, 5 자연폐경, 6 인공폐경, 7 기타)", source = "8기 PDF p.190 / 인쇄 p.182"),
  list(var = "he_prg",   miss = "", na8 = "8=범주유지", label = "임신여부", source = "8기 PDF p.192 / 인쇄 p.184"),
  list(var = "he_hp",    miss = "", na8 = "", label = "고혈압 유병여부(3 고혈압)", source = "8기 PDF p.196 / 인쇄 p.188"),
  list(var = "he_bmi",   miss = "", na8 = "", label = "체질량지수(kg/m2)", source = "8기 PDF p.198 / 인쇄 p.190"),
  list(var = "he_dm_hba1c", miss = "", na8 = "", label = "당뇨병 유병여부(3 당뇨병)", source = "8기 PDF p.199 / 인쇄 p.191"),
  list(var = "he_uacid", miss = "", na8 = "", label = "혈청 요산(mg/dL)", source = src_lab),
  list(var = "he_uacid_etc", miss = "", na8 = "", label = "요산 장비분석범위 표기(<1.0)", source = src_lab),
  list(var = "he_crea",  miss = "", na8 = "", label = "혈중 크레아티닌(mg/dL)", source = src_lab),
  list(var = "bs1_1",    miss = "", na8 = "", label = "평생 일반담배 흡연 여부(1 5갑 미만, 2 5갑 이상, 3 피운 적 없음)", source = src_smk),
  list(var = "bs3_1",    miss = "", na8 = "", label = "현재 일반담배 흡연 여부(1 매일, 2 가끔, 3 과거엔 피웠으나 현재 안 피움)", source = src_smk),
  list(var = "pa_aerobic", miss = "", na8 = "", label = "유산소 신체활동 실천(생성변수)", source = "8기 PDF p.188 / 인쇄 p.180")
))
fwrite(var_dict, file.path(out_dir, "01_variable_definitions.csv"), bom = TRUE)

# ==== 2. 자료 읽기·통합 =====================================================
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

# ==== 3. 결측·비해당 처리 [계획서 5장] ======================================
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

# ==== 4. 분석 변수 만들기 [계획서 4장] =====================================
# (1) 노출: 주당 잔 수 = 주당 음주 횟수 × 1회 잔 수 [4.1]
#     빈도: 월1회미만 0.5회/월, 월1회 1회/월, 월2-4회 3회/월(월 → 주: × 12/52), 주2-3회 2.5회/주, 주4회이상 5.5회/주
#     1회 잔 수: 1-2잔 1.5, 3-4잔 3.5, 5-6잔 5.5, 7-9잔 8, 10잔 이상은 BD2_14 기입값(없으면 결측)
freq_main  <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
#     민감도 [6.5]: 대푯값 전체 하한(L)·전체 상한(U). 정의는 Result/2026-11-18_음주대표값_민감도분석_계획.md 4–5절.
#       빈도 하한: 월1회미만 연 1회(월 1/12), 월1회 1, 월2-4회 월 2, 주2-3회 주 2, 주4회이상 주 4
#       빈도 상한: 월1회미만 월 1, 월1회 1, 월2-4회 월 4, 주2-3회 주 3, 주4회이상 주 7
#       1회량 하한 1·3·5·7잔, 상한 2·4·6·9잔. 10잔 이상은 기입값이라 바꾸지 않는다.
freq_L <- c(`2` = (1 / 12) * 12 / 52, `3` = 1 * 12 / 52, `4` = 2 * 12 / 52, `5` = 2, `6` = 4)
freq_U <- c(`2` = 1 * 12 / 52,        `3` = 1 * 12 / 52, `4` = 4 * 12 / 52, `5` = 3, `6` = 7)
amt_L  <- c(`1` = 1, `2` = 3, `3` = 5, `4` = 7)
amt_U  <- c(`1` = 2, `2` = 4, `3` = 6, `4` = 9)
occ_of <- function(a) fifelse(d$bd2_1 == 5, as.numeric(d$bd2_14), unname(a[as.character(d$bd2_1)]))
d[, per_occ := occ_of(amt)]
calc_wk <- function(freq, a = amt) unname(freq[as.character(d$bd1_11)]) * occ_of(a)
d[, `:=`(drinks_wk = calc_wk(freq_main), drinks_wk_L = calc_wk(freq_L, amt_L), drinks_wk_U = calc_wk(freq_U, amt_U))]

# (2) 효과수정인자: 홍조 이력 [4.2]
d[, flush_now := factor(fcase(bd9_2 == 1, "비홍조", bd9_2 %in% 2:4, "홍조"), c("비홍조", "홍조"))]
d[, flush_grp := factor(fcase(bd9_1 == 2 & bd9_2 == 1,     "비홍조",
                               bd9_1 == 1 & bd9_2 == 1,     "홍조 소실",
                               bd9_1 == 1 & bd9_2 %in% 2:4, "지속 홍조",
                               bd9_1 == 2 & bd9_2 %in% 2:4, "신규 홍조"),
                         c("비홍조", "홍조 소실", "지속 홍조", "신규 홍조"))]
# 홍조 이력 3군: 신규 홍조는 이력 3군에 속하지 않으므로 NA → 이력 분석에서 빠진다 [3장]
d[, flush_hist := factor(fifelse(flush_grp == "신규 홍조", NA_character_, as.character(flush_grp)), c("비홍조", "홍조 소실", "지속 홍조"))]

# (3) 결과: 혈청 요산·고요산혈증 [4.3]
#     🔍(계획서 10장 6번) 현재 규칙: '<1.0' 표기(HE_Uacid 결측)는 결측으로 둔다 (8기 PDF p.200 각주 1)
d[, uacid := he_uacid]
d[, uacid_lt1 := is.na(he_uacid) & trimws(as.character(he_uacid_etc)) == "<1.0"]
#     고요산혈증: 남 7.0, 여 6.0 mg/dL 이상. 지침서에 기준이 없어 문헌 기준(Bae 2023 등)
d[, hua := as.integer(uacid >= fifelse(sex == 1, 7.0, 6.0))]

# (4) 보정변수 [4.4]
#     현재흡연 2범주: 주요지표 정의(8기 PDF p.48 / 인쇄 p.40). 평생 5갑 이상이면서 현재 매일·가끔 피움 = 현재흡연
d[, smk := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3, smk := as.integer(bs1_1 == 2 & bs3_1 %in% c(1, 2))]
#     흡연 3범주(주 모형, 2026-11-23 확정): 0 비흡연, 1 과거흡연(5갑 이상 & 현재 안 피움), 2 현재흡연. 평생 5갑 미만은 비흡연.
#     BS1_1·BS3_1 코드: 8기 PDF p.175 / 인쇄 p.167. code/13과 같은 정의
d[, smk3 := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3,
  smk3 := fcase(bs1_1 == 2 & bs3_1 %in% c(1, 2), 2L, bs1_1 == 2 & bs3_1 == 3, 1L, default = 0L)]
#     고혈압·당뇨병: 유병 생성변수 HE_HP = 3, HE_DM_HbA1c = 3을 유병으로, 1·2는 없음으로
d[, htn := fcase(he_hp == 3, 1L, he_hp %in% 1:2, 0L)]
d[, dm  := fcase(he_dm_hba1c == 3, 1L, he_dm_hba1c %in% 1:2, 0L)]
#     🔍(계획서 10장 7번) eGFR: 지침서에 식이 없음. 현재 규칙은 CKD-EPI 2009식(Levey 2009)
d[, egfr := 141 * pmin(he_crea / fifelse(sex == 2, 0.7, 0.9), 1)^fifelse(sex == 2, -0.329, -0.411) *
            pmax(he_crea / fifelse(sex == 2, 0.7, 0.9), 1)^(-1.209) * 0.993^age * fifelse(sex == 2, 1.018, 1)]
d[, egfr10 := egfr / 10]
#     폐경 상태(여자만): LW_ms 2·3·4 폐경 전, 5·6 폐경 후, 1·7 기타, 9 결측
#     🔍(계획서 10장 8번) 현재 규칙: 임신 중·수유 중 여성은 폐경 전으로 포함한다
d[, meno_cov := factor(fcase(sex != 2, NA_character_, lw_ms %in% 2:4, "폐경 전", lw_ms %in% 5:6, "폐경 후", lw_ms %in% c(1, 7), "기타"),
                       c("폐경 전", "폐경 후", "기타"))]
d[, meno_post  := fifelse(sex == 1, 0, fifelse(is.na(meno_cov), NA_real_, as.numeric(meno_cov == "폐경 후")))]
d[, meno_other := fifelse(sex == 1, 0, fifelse(is.na(meno_cov), NA_real_, as.numeric(meno_cov == "기타")))]
d[, `:=`(
  f_sex  = factor(sex, 1:2, c("남자", "여자")),
  f_year = factor(year),
  f_incm = factor(incm, 1:4, c("하", "중하", "중상", "상")),
  f_edu  = factor(edu, 1:4, c("초졸이하", "중졸", "고졸", "대졸이상")),
  f_smk  = factor(smk, 0:1, c("비흡연", "현재흡연")),
  f_smk3 = factor(smk3, 0:2, c("비흡연", "과거흡연", "현재흡연")),
  f_pa  = factor(pa_aerobic, 0:1, c("미실천", "실천")),
  f_htn  = factor(htn, 0:1, c("없음", "있음")),
  f_dm   = factor(dm, 0:1, c("없음", "있음")),
  f_hua  = factor(hua, 0:1, c("없음", "있음"))
)]
# 보정변수 완전 응답(폐경 결측 2명은 여기서 빼지 않고 모형 단계에서 맞춘다 [3장])
d[, cov_complete := !is.na(age) & !is.na(f_edu) & !is.na(f_incm) & !is.na(f_smk) & !is.na(f_pa) &
                    !is.na(he_bmi) & !is.na(f_htn) & !is.na(f_dm) & !is.na(egfr)]
stopifnot(nrow(d) == n_raw)

# ==== 5. 분석 대상 표시(행 삭제 없음) [계획서 3장] ==========================
criteria <- list(
  "가중치 있음(wt_itvex)"                                  = quote(!is.na(wt_itvex)),
  "만 19세 이상"                                           = quote(age >= 19),
  "최근 1년 음주자(BD1_11 = 2-6)"                          = quote(bd1_11 %in% 2:6),
  "음주량 계산 가능·과거/현재 홍조 응답·요산 측정"         = quote(!is.na(drinks_wk) & !is.na(flush_grp) & !is.na(uacid)),
  "통풍 의사진단 없음(진단·모름·무응답 제외)"              = quote(dm8_dg %in% 0),
  "보정변수 모두 있음"                                     = quote(cov_complete)
)
run_flow <- function(crit, start_label) {
  keep <- rep(TRUE, nrow(d))
  out <- data.table(단계 = start_label, n = nrow(d), 남 = d[sex == 1, .N], 여 = d[sex == 2, .N])
  for (nm in names(crit)) {
    ok <- d[, eval(crit[[nm]])]; keep <- keep & !is.na(ok) & ok
    out <- rbind(out, data.table(단계 = nm, n = sum(keep), 남 = sum(keep & d$sex == 1), 여 = sum(keep & d$sex == 2)))
  }
  out[, 제외 := shift(n) - n]
  list(flow = out, keep = keep)
}
fl <- run_flow(criteria, "2019-2021 기본DB 전체")
d[, inc := as.integer(fl$keep)]
# 🔍(계획서 10장 9번) README·발표의 흐름(가중치 단계 없이 센 수)도 함께 출력해 대조한다
fl_v5 <- run_flow(criteria[-1], "2019-2021 기본DB 전체")
fl_v5$flow[, README_n := c(22559, 18691, 12231, 12002, 11217, 10779)][, 일치 := n == README_n]
fwrite(fl$flow,    file.path(out_dir, "02a_participant_flow.csv"), bom = TRUE)
fwrite(fl_v5$flow, file.path(out_dir, "02b_flow_vs_README.csv"), bom = TRUE)
if (!all(fl_v5$flow$일치)) warning("대상자 흐름이 README의 인원과 다릅니다. 02b_flow_vs_README.csv를 확인하세요.")

# 민감도용 대상(통풍 제외 기준 바꾸기) [6.5]
base_ok <- Reduce(`&`, lapply(criteria[1:4], function(e) { x <- d[, eval(e)]; !is.na(x) & x }))
d[, step3 := as.integer(base_ok)]                                               # 3단계까지 통과
d[, inc_gout_dx   := as.integer(base_ok & dm8_dg %in% c(0, 1) & cov_complete)]   # 통풍 진단자 포함(모름·무응답은 제외)
d[, inc_gout_unk  := as.integer(base_ok & !(dm8_dg %in% 1) & cov_complete)]      # 통풍 모름·무응답 포함(진단자는 제외)
# 여자 모형 인원 맞추기: 폐경 결측 여자는 Crude 모형에서도 뺀다 [3장]
d[, meno_ok := as.integer(sex == 1 | !is.na(meno_cov))]

# ==== 6. 복합표본 설계 → subset [계획서 2장] =================================
d[, wt_analysis := wt_itvex * pool_ratio[as.character(year)]]
d[is.na(wt_analysis), wt_analysis := 0]
lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","), " → survey.lonely.psu 선택 근거를 기록하세요.")
# 설계 객체에는 data.table이 아닌 data.frame을 넣는다(survey의 내부 subset이 data.table 구조를 깨뜨릴 수 있음)
des  <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = as.data.frame(d))
sdes <- subset(des, inc == 1)

# ---- 공용 도구 ----
sexes   <- c("남자", "여자")
dat_of  <- function(design) as.data.table(design$variables)
dsub    <- function(design, keep) { keep <- keep & !is.na(keep); design$variables$.keep <- keep; subset(design, .keep) }
fmt     <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
fit_svy <- function(design, form, fam) {   # 모형 변수 완전 응답자를 설계 안에서 subset한 뒤 적합
  vars <- all.vars(form); dv <- dat_of(design)
  cc <- complete.cases(dv[, vars, with = FALSE])
  ds <- dsub(design, cc)
  for (v in vars) if (is.factor(ds$variables[[v]])) ds$variables[[v]] <- droplevels(ds$variables[[v]])
  list(fit = svyglm(form, design = ds, family = fam), design = ds, data = dat_of(ds), n = sum(cc))
}
mk_form <- function(y, terms, covs = character(0)) as.formula(paste(y, "~", paste(c(terms, covs), collapse = " + ")))
contrast_test <- function(fit, L) {        # L: 이름 있는 계수 가중치 벡터들의 list → 공동 Wald F 검정
  b <- coef(fit); V <- vcov(fit)
  if (!all(unlist(lapply(L, names)) %in% names(b))) return(NA_real_)
  Lm <- do.call(rbind, lapply(L, function(l) { v <- setNames(rep(0, length(b)), names(b)); v[names(l)] <- l; v }))
  est <- Lm %*% b; W <- as.numeric(t(est) %*% solve(Lm %*% V %*% t(Lm)) %*% est); q <- nrow(Lm)
  pf(W / q, q, fit$df.residual, lower.tail = FALSE)
}
lincomb <- function(fit, l, tr) {          # 계수 선형결합의 추정치와 95% CI(t 분포)
  b <- coef(fit); V <- vcov(fit)
  if (!all(names(l) %in% names(b))) return(c(est = NA, lo = NA, hi = NA))
  w <- setNames(rep(0, length(b)), names(b)); w[names(l)] <- l
  e <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w)); q <- qt(0.975, fit$df.residual)
  c(est = tr(e), lo = tr(e - q * se), hi = tr(e + q * se))
}
# 2026-11-24: 유효숫자 4자리로 저장하면 보고서에서 소수 둘째 자리로 다시 반올림할 때 값이 어긋난다
#             (예: OR 1.05518 → 저장 1.055 → 보고 1.05, 맞는 값은 1.06). 그래서 7자리로 저장한다.
round_dt <- function(x, digits = 7) { x <- copy(x); for (nm in names(x)) if (is.double(x[[nm]])) set(x, j = nm, value = signif(x[[nm]], digits)); x }

# ==== 7. 기술 통계 [계획서 6.1, 5장] ========================================
# 7-1. 대상자 특성표: 성별 × 홍조 패턴(4군), 비가중 n + 가중 %(SE) 또는 가중 평균(SE)
#      결과(요산·고요산혈증)는 주분석 전까지 홍조군별로 보지 않으므로 여기 넣지 않는다(7-2에서 성별로만 본다).
t1_vars <- c(age = "나이(세)", f_year = "조사연도", f_edu = "교육수준", f_incm = "소득 4분위", f_smk3 = "흡연 3범주",
             f_pa = "유산소 신체활동 실천", meno_cov = "폐경 상태(여자)", he_bmi = "체질량지수(kg/m2)",
             f_htn = "고혈압 유병", f_dm = "당뇨병 유병", egfr = "eGFR(CKD-EPI 2009)", drinks_wk = "주당 음주 잔 수")
t1_block <- function(design, v) {
  dat <- dat_of(design)
  if (is.factor(dat[[v]])) {
    lv <- levels(dat[[v]]); n <- as.integer(table(factor(dat[[v]], levels = lv)))
    if (sum(n) == 0) return(data.table(변수 = t1_vars[[v]], 수준 = lv, n = n, 추정치 = "-"))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    data.table(변수 = t1_vars[[v]], 수준 = lv, n = n,
               추정치 = paste0(fmt(100 * coef(est)[paste0(v, lv)]), " (", fmt(100 * SE(est)[paste0(v, lv)]), ")"))
  } else {
    nn <- sum(!is.na(dat[[v]]))
    if (nn == 0) return(data.table(변수 = t1_vars[[v]], 수준 = "평균", n = 0L, 추정치 = "-"))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    data.table(변수 = t1_vars[[v]], 수준 = "평균", n = nn, 추정치 = paste0(fmt(coef(est), 2), " (", fmt(SE(est), 2), ")"))
  }
}
d_sub <- dat_of(sdes)
table1 <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(c("전체", levels(d_sub$flush_grp)), function(g) {
  k <- d_sub$f_sex == s & (g == "전체" | d_sub$flush_grp == g)
  vv <- if (s == "남자") setdiff(names(t1_vars), "meno_cov") else names(t1_vars)
  cbind(성별 = s, 홍조패턴 = g, 집단n = sum(k), rbindlist(lapply(vv, function(v) t1_block(dsub(sdes, k), v))))
}))))
fwrite(table1, file.path(out_dir, "03_table1.csv"), bom = TRUE)
# 7-1b. 홍조 이력 3군(비홍조·홍조 소실·지속 홍조) 사이 비교 p [2026-11-25 추가, 논문 표 1]
#   범주형: Rao-Scott 보정 χ² 검정(svychisq, F 통계량), 연속형: 복합표본 선형회귀의 홍조 이력 공동 Wald 검정(regTermTest)
t1p_one <- function(D, v) {
  dv <- dat_of(D)
  if (all(is.na(dv[[v]]))) return(NA_real_)
  if (is.factor(dv[[v]])) {
    k <- !is.na(dv[[v]]); DD <- dsub(D, k); DD$variables[[v]] <- droplevels(DD$variables[[v]])
    if (nlevels(DD$variables[[v]]) < 2) return(NA_real_)
    as.numeric(svychisq(as.formula(paste0("~", v, " + flush_hist")), DD, statistic = "F")$p.value)
  } else {
    f <- svyglm(as.formula(paste0(v, " ~ flush_hist")), design = dsub(D, !is.na(dv[[v]])))
    as.numeric(regTermTest(f, ~flush_hist)$p)
  }
}
table1_p <- rbindlist(lapply(sexes, function(s) {
  D <- dsub(sdes, d_sub$f_sex == s & !is.na(d_sub$flush_hist))
  D$variables$flush_hist <- droplevels(D$variables$flush_hist)
  vv <- if (s == "남자") setdiff(names(t1_vars), "meno_cov") else names(t1_vars)
  rbindlist(lapply(vv, function(v) data.table(성별 = s, 변수 = t1_vars[[v]], p = t1p_one(D, v))))
}))
cat("※ 03_table1.csv: n은 비가중 표본 수, 추정치는 가중 %(SE) 또는 가중 평균(SE)\n")

# 7-2. 결과 변수 분포(성별만)
outcome_tab <- rbindlist(lapply(sexes, function(s) {
  ds <- dsub(sdes, d_sub$f_sex == s)
  e1 <- svymean(~hua, ds); e2 <- svymean(~uacid, ds)
  data.table(성별 = s, n = sum(d_sub$f_sex == s), 고요산혈증_n = sum(d_sub$hua[d_sub$f_sex == s]),
             고요산혈증_가중pct = 100 * unname(coef(e1)), SE = 100 * as.numeric(SE(e1)),
             요산_가중평균 = unname(coef(e2)), 요산_SE = as.numeric(SE(e2)))
}))
fwrite(round_dt(outcome_tab), file.path(out_dir, "04_outcome_by_sex.csv"), bom = TRUE)

# 7-3. 제외된 사람과 남은 사람 비교: 3단계 통과자 중 최종 포함 vs 제외(통풍·보정변수 결측) [5장]
d[, 포함 := factor(fifelse(inc == 1L, "포함", "제외"), c("포함", "제외"))]
des_cmp <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = as.data.frame(d))
s3 <- subset(des_cmp, step3 == 1)
cmp_vars <- c(age = "나이(세)", f_sex = "성별", f_year = "조사연도", drinks_wk = "주당 음주 잔 수", flush_grp = "홍조 패턴", uacid = "혈청 요산")
t1_vars <- c(t1_vars, cmp_vars[setdiff(names(cmp_vars), names(t1_vars))])
cmp_tab <- rbindlist(lapply(c("포함", "제외"), function(g) {
  k <- dat_of(s3)$포함 == g
  cbind(집단 = g, 집단n = sum(k), rbindlist(lapply(names(cmp_vars), function(v) t1_block(dsub(s3, k), v))))
}))
fwrite(cmp_tab, file.path(out_dir, "05_included_vs_excluded.csv"), bom = TRUE)

# ==== 7-4. 공식 통계 대조 [계획서 8.1] ======================================
# 2019년 단년도, wt_itvex 그대로, 만 19세 이상. 지표 정의: 주요지표 SAS(8기 PDF p.48 / 인쇄 p.40)
#   월간음주율: 최근 1년간 월 1회 이상 음주한 분율
#   고위험음주율: 1회 평균 음주량이 남 7잔(여 5잔) 이상이며 주 2회 이상 음주한 분율
# 주의: 언론의 고위험음주율 일부는 지역사회건강조사 값이고, e-나라지표 값은 연령표준화 값이라 조율 추정치와 바로 비교하면 안 된다.
d[, dr_month := NA_integer_]
d[bd1 %in% 1:2 & bd1_11 %in% c(1:6, 8), dr_month := as.integer(bd1 == 2 & bd1_11 %in% 3:6)]
d[, dr_high := NA_integer_]
d[(sex == 1 & bd2_31 %in% c(1:5, 8)) | (sex == 2 & bd2_32 %in% c(1:5, 8)),
  dr_high := as.integer((sex == 1 & bd1_11 %in% 5:6 & bd2_1 %in% 4:5) | (sex == 2 & bd1_11 %in% 5:6 & bd2_1 %in% 3:5))]
d19 <- d[year == 2019]
d19[, w1 := fifelse(is.na(wt_itvex), 0, wt_itvex)]
des19 <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~w1, nest = TRUE, data = as.data.frame(d19))
official_check <- rbindlist(lapply(seq_len(nrow(OFFICIAL)), function(i) {
  o <- OFFICIAL[i]; v <- if (o$indicator == "월간음주율") "dr_month" else "dr_high"
  k <- d19$age >= 19 & !is.na(d19$wt_itvex) & (o$sex == "전체" | d19$sex == match(o$sex, sexes)) & !is.na(d19[[v]])
  est <- svymean(as.formula(paste0("~", v)), dsub(des19, k)); ci <- confint(est)
  data.table(지표 = paste0("만19세 이상 ", o$indicator, "(%, 2019, 조율)"), 성별 = o$sex, n = sum(k), 공식n = o$official_n,
             재현값 = 100 * unname(coef(est)), SE = 100 * as.numeric(SE(est)), CI하한 = 100 * ci[1], CI상한 = 100 * ci[2],
             공식값 = o$official, 차이 = 100 * unname(coef(est)) - o$official, 출처 = o$source)
}))
official_check[, 판정 := fifelse(is.na(공식값), "공식값 미입력",
                          fifelse(round(재현값, OFFICIAL_DIGITS) == 공식값 & n == 공식n, "일치", "불일치"))]
fwrite(round_dt(official_check), file.path(out_dir, "06_official_check.csv"), bom = TRUE)
if (any(is.na(official_check$공식값))) message("※ 공식 통계값이 비어 있습니다. 0절 OFFICIAL에 국민건강통계 2019 값을 채우세요.")

# ==== 8. 주분석·부질문·민감도 [계획서 6.2–6.5] ==============================
if (!RUN_MAIN) {
  message("※ RUN_MAIN = FALSE: 주분석 음주 기준이 확정되지 않아 8절(주분석·부질문·민감도)을 실행하지 않았습니다.")
} else {
  if (is.na(MAIN_NAME) || !MAIN_NAME %in% names(CANDIDATES)) stop("RUN_MAIN = TRUE이면 MAIN_NAME에 CANDIDATES의 이름 하나를 넣으세요.")

  # 음주 변수 만들기: 성별 절단점 범주(drk) 또는 연속(drk), 범주형이면 경향성 점수(drk_sc)도 만든다
  make_drink <- function(v, crit, gram, wk_var) {
    v$gday <- v[[wk_var]] * gram / 7                   # g/일. 1잔 = 7 g이면 주당 잔 수와 같다
    if (crit$drink == "cut") {
      k <- length(crit$male); stopifnot(k == length(crit$female))
      lv <- if (k == 1) c("저음주", "고음주") else if (k == 2) c("저음주", "중음주", "고음주") else paste0("수준", seq_len(k + 1))
      v$drk <- factor(NA_character_, lv); v$drk_sc <- NA_real_
      for (s in sexes) {
        i <- v$f_sex == s; br <- if (s == "남자") crit$male else crit$female
        v$drk[i] <- as.character(cut(v$gday[i], c(-Inf, br, Inf), right = FALSE, labels = lv))
        ii <- i & v$inc == 1; md <- tapply(v$gday[ii], v$drk[ii], median)   # 경향성 점수: 대상자 안의 범주별 g/일 중앙값 ÷ 10
        v$drk_sc[i] <- md[as.character(v$drk[i])] / 10
      }
      attr(v, "levels_drk") <- lv
    } else {
      v$drk <- v$gday / crit$per; v$drk_sc <- NA_real_
      attr(v, "levels_drk") <- NULL
    }
    v
  }

  # 기준 하나 × 보정 묶음 하나 × 결과 하나를 실행해 효과·검정·칸 표를 돌려준다
  run_spec <- function(spec) {
    crit <- spec$crit; covs <- COVSETS[[spec$covset]]
    sd <- des; sd$variables <- make_drink(des$variables, crit, spec$gram, spec$wk_var)
    lv <- attr(sd$variables, "levels_drk"); is_cut <- crit$drink == "cut"
    base_terms <- if (is_cut) paste0("drk", lv[-1]) else "drk"
    # 대상: 주분석은 inc, 민감도는 바꾼 대상. 여자는 폐경 결측을 모든 모형에서 빼 모형끼리 인원을 맞춘다 [3장]
    sd <- dsub(sd, sd$variables[[spec$pop]] == 1 & sd$variables$meno_ok == 1)
    dd <- dat_of(sd)
    fam <- if (spec$y == "hua") quasibinomial() else gaussian(); tr <- if (spec$y == "hua") exp else identity
    eff <- list(); tst <- list(); cel <- list()
    add_test <- function(q, who, r, L, groups = NULL, gvar = "flush_hist", bysex = FALSE) {
      p <- tryCatch(contrast_test(r$fit, L), error = function(e) NA_real_)
      b <- coef(r$fit)[unique(unlist(lapply(L, names)))]
      sep <- any(is.na(b)) || (is_cut && max(abs(b), na.rm = TRUE) > 8)
      few <- FALSE
      # 사례 10명 미만 칸 확인 [계획서 7장]: 경향성 검정도 같은 범주 칸 기준, 부질문 2는 성별 × 홍조군 × 음주 칸 기준
      if (is_cut && spec$y == "hua" && !is.null(groups)) {
        cc <- if (bysex) r$data[get(gvar) %in% groups, .(ev = sum(hua)), keyby = .(s = f_sex, g = get(gvar), drk)]
              else r$data[get(gvar) %in% groups, .(ev = sum(hua)), keyby = .(g = get(gvar), drk)]
        few <- nrow(cc) < length(groups) * length(lv) * (if (bysex) length(sexes) else 1) || min(cc$ev) < MIN_EV
      }
      tst[[length(tst) + 1L]] <<- data.table(질문 = q, 대상 = who, df = length(L), p = p, 모형n = r$n,
                                             비고 = paste(c(if (few) "사례 10명 미만 칸 있음", if (sep) "분리(추정 불가)"), collapse = ", "))
    }
    for (s in sexes) {
      D <- dsub(sd, dd$f_sex == s); covs_s <- if (s == "남자") setdiff(covs, "meno_cov") else covs
      # 홍조 이력 3군 모형: 주질문(지속 vs 비홍조)과 부질문 1(소실 vs 비홍조)을 한 모형에서 본다 [6.2]
      r <- fit_svy(D, mk_form(spec$y, "drk * flush_hist", covs_s), fam)
      nh <- function(g) paste0(base_terms, ":flush_hist", g)
      add_test("주질문: 지속 홍조 vs 비홍조", s, r, lapply(nh("지속 홍조"), function(x) setNames(1, x)), c("비홍조", "지속 홍조"))
      add_test("부질문 1: 홍조 소실 vs 비홍조", s, r, lapply(nh("홍조 소실"), function(x) setNames(1, x)), c("비홍조", "홍조 소실"))
      add_test("부질문 1(탐색): 홍조 소실 vs 지속 홍조", s, r,
               Map(function(a, b) setNames(c(1, -1), c(a, b)), nh("홍조 소실"), nh("지속 홍조")), c("홍조 소실", "지속 홍조"))
      # 홍조군별 효과: 각 홍조군 안에서 저음주 대비 OR(범주형) 또는 단위당 OR(연속형)
      for (g in levels(dd$flush_hist)) for (t in base_terms) {
        l <- if (g == "비홍조") setNames(1, t) else setNames(c(1, 1), c(t, paste0(t, ":flush_hist", g)))
        e <- lincomb(r$fit, l, tr)
        # 이 OR을 만드는 두 칸(같은 홍조군의 저음주 칸과 해당 음주 칸) 중 사례 10명 미만이 있으면 해석하지 않는다 [계획서 7장]
        few_e <- if (is_cut && spec$y == "hua") {
          ev <- r$data[flush_hist == g & drk %in% c(lv[1], sub("^drk", "", t)), .(ev = sum(hua)), by = drk]
          nrow(ev) < 2 || min(ev$ev) < MIN_EV
        } else FALSE
        eff[[length(eff) + 1L]] <- data.table(성별 = s, 홍조이력 = g, 음주 = if (is_cut) sub("^drk", "", t) else paste0(crit$per, " g/일당"),
                                              추정치 = e[["est"]], CI하한 = e[["lo"]], CI상한 = e[["hi"]], 모형n = r$n,
                                              비고 = if (few_e) "사례 10명 미만 칸 있음(해석하지 않음)" else "")
      }
      # 주질문·부질문의 효과 크기: 교호작용 항 = 홍조군 OR ÷ 비홍조군 OR(혈청 요산은 β 차이)과 95% CI [6.2]
      # 2026-11-25 추가(forest plot용). 비고는 같은 질문 검정의 비고(사례 10명 미만·분리)를 그대로 쓴다.
      q_note <- c("지속 홍조" = tst[[length(tst) - 2L]]$비고, "홍조 소실" = tst[[length(tst) - 1L]]$비고)
      for (g in c("지속 홍조", "홍조 소실")) for (t in base_terms) {
        e <- lincomb(r$fit, setNames(1, paste0(t, ":flush_hist", g)), tr)
        eff[[length(eff) + 1L]] <- data.table(성별 = s, 홍조이력 = paste(g, "÷ 비홍조"),
                                              음주 = paste0(if (is_cut) sub("^drk", "", t) else paste0(crit$per, " g/일당"),
                                                            if (spec$y == "hua") ": OR의 비" else ": β 차이"),
                                              추정치 = e[["est"]], CI하한 = e[["lo"]], CI상한 = e[["hi"]], 모형n = r$n, 비고 = q_note[[g]])
      }
      if (is_cut) {                                     # 칸 표: 비가중 n·사례 수와 가중 유병률(%) [계획서 6.1, 7장]
        cn <- r$data[, .(n = .N, 고요산혈증_n = sum(hua)), keyby = .(성별 = f_sex, 홍조이력 = flush_hist, 음주 = drk)]
        wp <- as.data.table(svyby(~hua, ~flush_hist + drk, r$design, svymean, na.rm = TRUE, keep.var = FALSE))
        setnames(wp, c("flush_hist", "drk", "statistic"), c("홍조이력", "음주", "가중유병률"), skip_absent = TRUE)
        wp[, 가중유병률 := 100 * 가중유병률]
        cel[[s]] <- merge(cn, wp[, .(홍조이력 = as.character(홍조이력), 음주 = as.character(음주), 가중유병률)],
                          by.x = c("홍조이력", "음주"), by.y = c("홍조이력", "음주"), all.x = TRUE)
      }
      # 경향성 분석(이전 부질문 2·3)은 2026-11-23 결정으로 하지 않는다. 주분석이 2군이라 결과도 공동 검정과 같다.
      # 참고: 현재 홍조 2군(계획 발표 때의 주분석) [6.4]
      if (isTRUE(spec$extra)) {
        rn <- fit_svy(D, mk_form(spec$y, "drk * flush_now", covs_s), fam)
        add_test("참고: 현재 홍조 vs 현재 비홍조", s, rn, lapply(paste0(base_terms, ":flush_now홍조"), function(x) setNames(1, x)), c("비홍조", "홍조"), "flush_now")
      }
    }
    # 이전 부질문 2(성별 3원 교호작용)는 2026-11-22 계획서 개정으로 삭제했다 [계획서 6.3]
    lab <- c(기준 = crit$label, `1잔g` = spec$gram, 빈도대푯값 = spec$wk_var, 대상군 = spec$pop, 보정 = spec$covset,
             결과 = if (spec$y == "hua") "고요산혈증(OR)" else "혈청 요산(β, mg/dL)", 분석 = spec$role)
    wrap <- function(x) if (length(x)) cbind(as.data.table(as.list(lab)), rbindlist(x)) else NULL
    list(effects = wrap(eff), tests = wrap(tst), cells = wrap(cel))
  }

  # 실행 목록: 주분석 1개 + 민감도 [6.5]. 결과를 보기 전에 이 목록을 고정한다.
  main <- CANDIDATES[[MAIN_NAME]]
  sp <- function(crit, role, covset = MAIN_COVSET, gram = GRAM_MAIN, wk_var = "drinks_wk", pop = "inc", y = "hua", extra = FALSE)
    list(crit = crit, role = role, covset = covset, gram = gram, wk_var = wk_var, pop = pop, y = y, extra = extra)
  # 2026-11-23 확정: 주분석(고요산혈증, M2S) + 보조(혈청 요산 다중회귀, M2S) + 보정 모형 Crude·M3S + 민감도 4개(1잔 8·10 g, 대푯값 L·U, M2S)
  specs <- c(
    list(sp(main, "주분석"),
         sp(main, "보조: 혈청 요산(다중회귀)", y = "uacid"),
         sp(main, "비교: 보정 없음", covset = "Crude"),
         sp(main, "비교(혈청 요산): 보정 없음", covset = "Crude", y = "uacid"),
         sp(main, "추가 보정: M3S", covset = "M3S"),
         sp(main, "추가 보정(혈청 요산): M3S", covset = "M3S", y = "uacid")),
    lapply(GRAM_SENS, function(g) sp(main, paste0("민감도: 1잔 ", g, " g"), gram = g)),
    list(sp(main, "민감도: 대푯값 전체 하한(L)", wk_var = "drinks_wk_L"),
         sp(main, "민감도: 대푯값 전체 상한(U)", wk_var = "drinks_wk_U"))
  )
  res <- lapply(specs, run_spec)
  main_effects <- rbindlist(lapply(res, `[[`, "effects"), fill = TRUE)
  main_tests   <- rbindlist(lapply(res, `[[`, "tests"),   fill = TRUE)
  main_cells   <- rbindlist(lapply(res, `[[`, "cells"),   fill = TRUE)
  fwrite(round_dt(main_tests),   file.path(out_dir, "10_interaction_tests.csv"), bom = TRUE)
  fwrite(round_dt(main_effects), file.path(out_dir, "11_effects_by_flush.csv"), bom = TRUE)
  fwrite(main_cells,             file.path(out_dir, "12_cell_counts.csv"), bom = TRUE)
  cat("※ 10–12번 파일: 모형n·n은 비가중 표본 수, 효과는 복합표본 가중 추정치. '분리'나 '사례 10명 미만' 표시가 있는 칸은 해석하지 않는다.\n")

  # 13. 홍조 이력별 결과·음주 분포(주분석 대상): 비가중 n과 가중 추정치(SE) [계획서 6.1, 7장]
  sdm <- des; sdm$variables <- make_drink(des$variables, main, GRAM_MAIN, "drinks_wk")
  sdm <- dsub(sdm, sdm$variables$inc == 1 & sdm$variables$meno_ok == 1 & !is.na(sdm$variables$flush_hist))
  dm_ <- dat_of(sdm); hi <- attr(sdm$variables, "levels_drk")[2]
  sdm$variables$drk_hi <- as.integer(sdm$variables$drk == hi)
  flush_outcome <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(levels(dm_$flush_hist), function(g) {
    k <- dm_$f_sex == s & dm_$flush_hist == g; D <- dsub(sdm, k)
    e1 <- svymean(~hua, D); e2 <- svymean(~uacid, D); e3 <- svymean(~drk_hi, D); e4 <- svymean(~gday, D)
    data.table(성별 = s, 홍조이력 = g, n = sum(k), 고음주_n = sum(dm_$drk[k] == hi), 고음주_가중pct = 100 * unname(coef(e3)), 고음주_SE = 100 * as.numeric(SE(e3)),
               음주_g일_가중평균 = unname(coef(e4)), 음주_SE = as.numeric(SE(e4)),
               고요산혈증_n = sum(dm_$hua[k]), 고요산혈증_가중pct = 100 * unname(coef(e1)), 고요산혈증_SE = 100 * as.numeric(SE(e1)),
               요산_가중평균 = unname(coef(e2)), 요산_SE = as.numeric(SE(e2)))
  }))))
  fwrite(round_dt(flush_outcome), file.path(out_dir, "13_outcome_by_flush.csv"), bom = TRUE)
  # 표 1의 결과 행(고음주·고요산혈증·혈청 요산·하루 음주량)의 홍조 이력 3군 비교 p(모형 분석 대상 기준) [2026-11-25 추가]
  sdm$variables$f_drk_hi <- factor(sdm$variables$drk_hi, 0:1); sdm$variables$f_hua <- factor(sdm$variables$hua, 0:1)
  out_p <- rbindlist(lapply(sexes, function(s) {
    D <- dsub(sdm, dat_of(sdm)$f_sex == s); D$variables$flush_hist <- droplevels(D$variables$flush_hist)
    data.table(성별 = s, 변수 = c("고음주", "고요산혈증", "혈청 요산", "하루 음주량(g/일)"),
               p = c(t1p_one(D, "f_drk_hi"), t1p_one(D, "f_hua"), t1p_one(D, "uacid"), t1p_one(D, "gday")))
  }))
  fwrite(round_dt(rbind(table1_p, out_p)), file.path(out_dir, "14_table1_pvalues.csv"), bom = TRUE)
}

# ==== 9. 재현성 기록 [계획서 8.2] ===========================================
fwrite(data.table(file = basename(files), size = file.size(files), md5 = unname(tools::md5sum(files))), file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(c(paste("RUN_MAIN:", RUN_MAIN), paste("MAIN_NAME:", MAIN_NAME), paste("GRAM_MAIN:", GRAM_MAIN),
             capture.output(sessionInfo())), file.path(out_dir, "_sessionInfo.txt"))
cat("완료:", out_dir, "\n")
