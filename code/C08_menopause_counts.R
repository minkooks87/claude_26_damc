# C08_menopause_counts.R
# 목적: 최종 분석 대상 여성(분석계획서 3장, 5,368명)의 폐경 상태별 인원을 세고,
#       폐경 상태를 여성 보정변수로 넣을 만한지(범주별 인원·사례 수·나이와 겹치는 정도) 검토한다.
#       ※ 음주 수준 × 홍조와 고요산혈증의 연관(주분석 결과)은 보지 않는다. 고요산혈증은 범주별 사례 수만 센다.
# 변수 정의 (8기 지침서 PDF p.190 / 인쇄 p.182, LW_ms 월경여부):
#   1 초경하기 전, 2 월경중, 3 임신중, 4 출산후 수유 중, 5 자연폐경, 6 인공폐경, 7 기타, 8 비해당(만10세 미만 여자), 9 모름·무응답
#   분석계획서 4.4의 분류: 폐경 전 = 2·3·4, 폐경 후 = 5·6, 기타 = 1·7, 9 = 결측
#   LW_mp_a 폐경 연령(같은 쪽): 888 비해당, 999 모름·무응답 → 결측
# 자료: knhanes_2019_2021.sav (읽기만 함). 대상자 정의는 C07·분석계획서 3장과 같음
# 복합표본설계: wt_itvex × 1/3, 층 kstrata, 집락 psu. 전체 자료로 설계를 만든 뒤 subset()
# 작성: 생성형 AI(Anthropic Claude)가 코드 작성·실행 (2026-11-18). 조원 검토 필요.

suppressPackageStartupMessages({ library(foreign); library(data.table); library(survey) })
options(survey.lonely.psu = "adjust")
sav <- "C:/Users/travi/OneDrive/문서/카카오톡 받은 파일/knhanes_2019_2021.sav"
out_dir <- "output/C08_menopause"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

D <- as.data.table(suppressWarnings(read.spss(sav, to.data.frame = TRUE, use.value.labels = FALSE, reencode = "UTF-8")))
setnames(D, tolower(names(D)))

# ---- 1. 대상자 정의 (C07과 같음) ----
freq_wk <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
D[, f := freq_wk[as.character(bd1_11)]]
D[, a := fifelse(bd2_1 %in% 1:4, amt[as.character(bd2_1)], fifelse(bd2_1 == 5 & bd2_14 >= 10 & bd2_14 < 999, bd2_14, NA_real_))]
D[, dw := f * a]
D[, in_s := !is.na(wt_itvex) & age >= 19 & bd1 == 2 & bd1_11 %in% 2:6 & !is.na(dw) &
            bd9_1 %in% 1:2 & bd9_2 %in% 1:4 & !is.na(he_uacid) & dm8_dg %in% 0 &
            edu %in% 1:4 & incm %in% 1:4 & bs3_1 %in% c(1, 2, 3, 8) & pa_aerobic %in% c(0, 1) &
            !is.na(he_bmi) & he_hp %in% 1:3 & he_dm_hba1c %in% 1:3 & !is.na(he_crea)]
D[, women := in_s & sex == 2]

# ---- 2. 파생 변수 ----
D[, meno := factor(fcase(lw_ms %in% 2:4, "폐경 전", lw_ms %in% 5:6, "폐경 후", lw_ms %in% c(1, 7), "기타",
                         default = NA_character_), c("폐경 전", "폐경 후", "기타"))]
D[, flush := factor(fcase(bd9_1 == 2 & bd9_2 == 1, "비홍조", bd9_1 == 1 & bd9_2 == 1, "홍조 소실",
                          bd9_1 == 1 & bd9_2 %in% 2:4, "지속 홍조", bd9_1 == 2 & bd9_2 %in% 2:4, "신규 홍조"),
                    c("비홍조", "홍조 소실", "지속 홍조", "신규 홍조"))]
D[, hua := as.integer(he_uacid >= 6.0)]          # 여성 기준
D[, mp_age := fifelse(lw_mp_a >= 0 & lw_mp_a <= 80, as.numeric(lw_mp_a), NA_real_)]
D[, age_grp := cut(age, c(18, 39, 44, 49, 54, 59, 64, Inf), c("19–39", "40–44", "45–49", "50–54", "55–59", "60–64", "65+"))]
D[, wt := wt_itvex / 3]

W <- D[women == TRUE]
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt, data = D[!is.na(wt)], nest = TRUE)
dW <- subset(des, women)

wpct <- function(var, design) {
  p <- svymean(as.formula(paste0("~", var)), design, na.rm = TRUE)
  data.table(level = sub(paste0("^", var), "", names(coef(p))), w_pct = round(100 * coef(p), 1), se = round(100 * SE(p), 1))
}
sink(file.path(out_dir, "C08_menopause_counts_log.txt"), split = TRUE)
cat("실행:", format(Sys.time()), "\n최종 대상 여성(비가중 n):", nrow(W), "\n\n")

# ---- 3. LW_ms 원값 분포 ----
cat("[표 1] LW_ms 원값 분포 (비가중 n, 가중 %)\n")
W[, lw_ms_c := factor(fifelse(is.na(lw_ms), "NA", as.character(lw_ms)))]
dW <- update(dW, lw_ms_c = factor(fifelse(is.na(lw_ms), "NA", as.character(lw_ms))))
t1 <- merge(W[, .(n = .N), by = .(level = as.character(lw_ms_c))], wpct("lw_ms_c", dW), by = "level", all = TRUE)
print(t1)

# ---- 4. 폐경 상태 분류 ----
cat("\n[표 2] 폐경 상태 (분석계획서 4.4 분류; 9·NA는 결측)\n")
t2 <- W[, .(n = .N, 나이_중앙값 = as.double(median(age)), 나이_최소 = min(age), 나이_최대 = max(age),
            고요산혈증_사례 = sum(hua)), by = .(meno = fifelse(is.na(meno), "결측", as.character(meno)))]
t2 <- merge(t2, wpct("meno", dW)[, .(meno = level, w_pct, se)], by = "meno", all.x = TRUE)
print(t2)
cat("※ 가중 %는 결측을 뺀 분모 기준\n")

# ---- 5. 홍조 이력 × 폐경 상태 (주질문·부질문 1의 부분집단) ----
cat("\n[표 3] 홍조 이력 × 폐경 상태 (비가중 n)\n")
t3 <- dcast(W[!is.na(meno)], flush ~ meno, value.var = "age", fun.aggregate = length)
print(t3)
cat("\n[표 3b] 홍조 이력 × 폐경 상태: 고요산혈증 사례 수 (비가중; 연관 크기는 보지 않음)\n")
t3b <- dcast(W[!is.na(meno)], flush ~ meno, value.var = "hua", fun.aggregate = sum)
print(t3b)

cat("\n[표 3c] 홍조 이력별 폐경 후 비율 (가중 %)\n")
t3c <- rbindlist(lapply(levels(W$flush), function(g) {
  dd <- subset(dW, flush == g & !is.na(meno))
  p <- svymean(~I(meno == "폐경 후"), dd)
  data.table(flush = g, n = W[flush == g & !is.na(meno), .N], 폐경후_w_pct = round(100 * coef(p)[2], 1))
}))
print(t3c)

# ---- 6. 나이와 겹치는 정도 ----
cat("\n[표 4] 나이 구간 × 폐경 상태 (비가중 n) — 같은 나이대에 폐경 전·후가 함께 있어야 나이와 따로 효과를 추정할 수 있음\n")
t4 <- dcast(W[!is.na(meno)], age_grp ~ meno, value.var = "age", fun.aggregate = length)
print(t4)
cat("\n폐경 연령(LW_mp_a, 폐경 후 여성, 비가중): \n")
print(W[meno == "폐경 후", .(n_응답 = sum(!is.na(mp_age)), 중앙값 = as.double(median(mp_age, na.rm = TRUE)),
                            사분위_25 = as.double(quantile(mp_age, .25, na.rm = TRUE)), 사분위_75 = as.double(quantile(mp_age, .75, na.rm = TRUE)))])

# ---- 7. 기타(1·7) 범주 들여다보기 ----
cat("\n[표 5] 기타 범주의 나이 분포 (비가중 n)\n")
print(W[meno == "기타", .N, by = .(lw_ms, age_grp)][order(lw_ms, age_grp)])

# ---- 8. 미확정 #8: 임신·수유 여성 ----
cat("\n[표 6] 임신·수유 (분석계획서 10장 #8)\n")
print(W[, .(임신중_LW_ms3 = sum(lw_ms %in% 3), 수유중_LW_ms4 = sum(lw_ms %in% 4), 검진당일임신_HE_prg1 = sum(he_prg %in% 1),
            합계_중복제외 = sum(lw_ms %in% 3:4 | he_prg %in% 1))])
sink()

fwrite(t1, file.path(out_dir, "01_lw_ms_raw.csv"), bom = TRUE)
fwrite(t2, file.path(out_dir, "02_menopause_status.csv"), bom = TRUE)
fwrite(merge(t3, t3b, by = "flush", suffixes = c("_n", "_사례")), file.path(out_dir, "03_flush_by_menopause.csv"), bom = TRUE)
fwrite(t4, file.path(out_dir, "04_age_by_menopause.csv"), bom = TRUE)
