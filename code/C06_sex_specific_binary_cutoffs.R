# C06_sex_specific_binary_cutoffs.R
# 목적: 남녀를 따로 분석하고 음주 수준을 남녀별 기준으로 저/고 2단계로 나눌 때, 후보 기준별 칸 인원(노출 분포)만 확인
#       - 고요산혈증·요산과의 교차는 보지 않음 (결과를 보고 기준을 고르지 않기 위함)
# 음주량: 주당 음주 잔 수(B02와 같음). 1잔 = 알코올 7 g(Jeon 외, 2022) → 주당 알코올 g = 잔 수 × 7
# 후보 (남 / 여, 주당 알코올 g → 국건영 잔):
#   A 미국 NIAAA 고위험(주 표준잔 14 / 7잔 초과, 1잔 14 g): 196 / 98 g 초과 → 28 / 14잔 초과
#   B 통풍 위험 15 g/일(Choi 2004, 남성) + 여성은 절반(미국 식생활지침 남 2잔·여 1잔 비율): 105 / 52.5 g 이상 → 15 / 7.5잔 이상
#   C 충남대 가정의학 제안 남성 주 8 표준잔(112 g) + 여성 절반(56 g, 여성 기준은 확인 안 됨) → 16 / 8잔 이상
#   D NAFLD '유의한 음주'(남 210 / 여 140 g/주 초과) → 30 / 20잔 초과
#   E 국건영 공식 고위험음주: 1회 남 7잔·여 5잔 이상을 주 2회 이상 (패턴 기준)
# 자료: knhanes_2019_2021.sav (읽기만 함). 대상자 정의는 B02/C02와 같음
# 작성: 생성형 AI(Anthropic Claude)가 코드 작성·실행. 조원 검토 필요.

suppressPackageStartupMessages({ library(foreign); library(data.table); library(survey) })
options(survey.lonely.psu = "adjust")
sav <- "C:/Users/travi/OneDrive/문서/카카오톡 받은 파일/knhanes_2019_2021.sav"
D <- as.data.table(suppressWarnings(read.spss(sav, to.data.frame = TRUE, use.value.labels = FALSE, reencode = "UTF-8")))
setnames(D, tolower(names(D)))
freq_wk <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
D[, f := freq_wk[as.character(bd1_11)]]
D[, a := fifelse(bd2_1 %in% 1:4, amt[as.character(bd2_1)],
          fifelse(bd2_1 == 5 & bd2_14 >= 10 & bd2_14 < 999, bd2_14, NA_real_))]
D[, dw := f * a]                                      # 주당 잔 수
D[, cur_flush := fcase(bd9_2 %in% 2:4, "홍조", bd9_2 == 1, "비홍조", default = NA_character_)]
D[, in_s := !is.na(wt_itvex) & age >= 19 & bd1 == 2 & bd1_11 %in% 2:6 & !is.na(dw) &
            bd9_1 %in% 1:2 & bd9_2 %in% 1:4 & !is.na(he_uacid) & dm8_dg %in% 0 &
            edu %in% 1:4 & incm %in% 1:4 & bs3_1 %in% c(1, 2, 3, 8) & pa_aerobic %in% c(0, 1) &
            !is.na(he_bmi) & he_hp %in% 1:3 & he_dm_hba1c %in% 1:3 & !is.na(he_crea)]
m <- D$sex == 1
D[, `:=`(
  A = fifelse(m, dw > 28, dw > 14),
  B = fifelse(m, dw >= 15, dw >= 7.5),
  C = fifelse(m, dw >= 16, dw >= 8),
  D_ = fifelse(m, dw > 30, dw > 20),
  # 1회 음주량 BD2_1: 3 = 5-6잔, 4 = 7-9잔, 5 = 10잔 이상 / 빈도 BD1_11: 5 = 주 2-3회, 6 = 주 4회 이상
  E = bd1_11 %in% 5:6 & fifelse(m, bd2_1 %in% 4:5, bd2_1 %in% 3:5))]
labs <- c(A = "A NIAAA (남 >28 / 여 >14잔)", B = "B 15 g/일·여성 절반 (남 ≥15 / 여 ≥7.5잔)",
          C = "C 충남대 제안·여성 절반 (남 ≥16 / 여 ≥8잔)", D_ = "D NAFLD (남 >30 / 여 >20잔)",
          E = "E 국건영 고위험음주 (패턴)")
D[, sexf := factor(sex, 1:2, c("남", "여"))]
des <- subset(svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex_3y, nest = TRUE,
                        data = D[!is.na(wt_itvex_3y)]), in_s)
A <- D[in_s == TRUE]
res <- rbindlist(lapply(names(labs), function(v) {
  n <- A[, .(고 = sum(get(v)), 저 = sum(!get(v))), by = .(sexf, cur_flush)]
  w <- svyby(as.formula(paste0("~as.numeric(", v, ")")), ~sexf + cur_flush, des, svymean)
  n[, 고_가중pct := round(100 * coef(w)[paste(sexf, cur_flush, sep = ".")], 1)]
  cbind(기준 = labs[[v]], n)
}))
setorder(res, 기준, sexf, cur_flush)
print(res, nrows = 100)
# 환산 민감도: 1잔 = 10 g(Hong 외, 2016, 국건영 2011-2013)으로 바꾸면 B 기준(남 ≥105 g, 여 ≥52.5 g/주)의 잔 수 경계는 10.5 / 5.25잔
A[, B10 := fifelse(sex == 1, dw >= 10.5, dw >= 5.25)]
cat("\n[환산 민감도] B 기준을 1잔 = 10 g으로 계산할 때 (고 / 저, 비가중 n)\n")
print(A[, .(고 = sum(B10), 저 = sum(!B10)), by = .(sexf, cur_flush)][order(sexf, cur_flush)])

# 기준끼리 고음주로 분류되는 사람이 얼마나 겹치는지 (E와 B)
cat("\nB와 E 일치도 (성별):\n"); print(A[, .N, by = .(sexf, B, E)][order(sexf, B, E)])
