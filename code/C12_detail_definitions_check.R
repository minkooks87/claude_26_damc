# C12_detail_definitions_check.R
# 목적: 다음 회의 자료 결정 7(세부 정의 6가지, 분석계획서 10장 6–9·14번)의 근거 수치를 원자료로 다시 확인한다.
#   7-1 혈청 요산 '<1.0' 표기자 수와, 포함하면 늘어나는 대상자 수
#   7-2 eGFR CKD-EPI 2009 vs 2021 값 차이, FLUSH 자료의 egfr가 어느 식인지
#   7-3 임신·수유 여성 수
#   7-4 S1(성별 가중 삼분위) 실제 절단점과 반올림값의 분류 차이
#   7-5 폐경 전후 탐색 분석의 칸 인원(여성 × 홍조 이력 × 폐경 × 주분석 기준 S2)
#   7-6 대상자 흐름: 가중치 단계 없는 흐름 vs 가중치 있음 단계를 먼저 넣은 흐름
# 변수 정의(제8기 지침서): HE_Uacid·HE_Uacid_etc(PDF p.200/인쇄 p.192, '<1.0'은 각주 1), HE_crea(PDF p.200/인쇄 p.192),
#   LW_ms(PDF p.190/인쇄 p.182), HE_prg(PDF p.192/인쇄 p.184), sex·age·wt_itvex·kstrata·psu(PDF p.91–92/인쇄 p.83–84)
# 복합표본: wt_itvex × 1/3, 층 kstrata, 집락 psu. 전체 자료로 설계 후 subset(). 자료는 읽기만 한다.
# 작성: 생성형 AI(Anthropic Claude) (2026-11-23). 조원 검토 필요.

suppressPackageStartupMessages({ library(foreign); library(data.table); library(survey) })
options(survey.lonely.psu = "adjust")
rd <- function(f) { d <- as.data.table(suppressWarnings(read.spss(f, to.data.frame = TRUE, use.value.labels = FALSE, reencode = "UTF-8"))); setnames(d, tolower(names(d))); d }
D <- rd("C:/Users/travi/OneDrive/문서/카카오톡 받은 파일/knhanes_2019_2021.sav")
FL <- rd("C:/Users/travi/OneDrive/문서/카카오톡 받은 파일/KNHANES_2019_2021_FLUSH.sav")
out_dir <- "output/C12_detail_definitions"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
sink(file.path(out_dir, "C12_log.txt"), split = TRUE)

freq_wk <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
D[, dw := freq_wk[as.character(bd1_11)] * fifelse(bd2_1 %in% 1:4, amt[as.character(bd2_1)], fifelse(bd2_1 == 5 & bd2_14 >= 10 & bd2_14 < 999, bd2_14, NA_real_))]
D[, gwk := dw * 7]                                   # 주당 g(1잔 7 g)
D[, lt1 := is.na(he_uacid) & trimws(as.character(he_uacid_etc)) == "<1.0"]
cov_ok <- quote(edu %in% 1:4 & incm %in% 1:4 & bs3_1 %in% c(1, 2, 3, 8) & pa_aerobic %in% c(0, 1) & !is.na(he_bmi) & he_hp %in% 1:3 & he_dm_hba1c %in% 1:3 & !is.na(he_crea))
D[, drk := age >= 19 & bd1 == 2 & bd1_11 %in% 2:6]
D[, base := drk & !is.na(dw) & bd9_1 %in% 1:2 & bd9_2 %in% 1:4]
D[, tgt := base & !is.na(he_uacid) & dm8_dg %in% 0 & !is.na(wt_itvex) & eval(cov_ok)]
D[, wt := wt_itvex / 3]
cat("최종 대상:", D[tgt == TRUE, .N], "(남", D[tgt == TRUE & sex == 1, .N], "· 여", D[tgt == TRUE & sex == 2, .N], ")\n")

# ---- 7-1 ----
cat("\n[7-1] 혈청 요산 '<1.0' 표기\n")
cat("  전체 자료:", D[lt1 == TRUE, .N], "명 | 현재 음주 성인 중:", D[lt1 == TRUE & drk == TRUE, .N], "명\n")
add <- D[lt1 == TRUE & base == TRUE & dm8_dg %in% 0 & !is.na(wt_itvex) & eval(cov_ok)]
cat("  포함하면 최종 대상에 더해지는 인원:", nrow(add), "명 (남", add[sex == 1, .N], "· 여", add[sex == 2, .N], ") → 모두 고요산혈증 '아님'\n")

# ---- 7-2 ----
cat("\n[7-2] eGFR 식 비교 (최종 대상)\n")
egfr09 <- function(cr, age, sex) { k <- fifelse(sex == 2, 0.7, 0.9); a <- fifelse(sex == 2, -0.329, -0.411)
  141 * pmin(cr / k, 1)^a * pmax(cr / k, 1)^-1.209 * 0.993^age * fifelse(sex == 2, 1.018, 1) }
egfr21 <- function(cr, age, sex) { k <- fifelse(sex == 2, 0.7, 0.9); a <- fifelse(sex == 2, -0.241, -0.302)
  142 * pmin(cr / k, 1)^a * pmax(cr / k, 1)^-1.200 * 0.9938^age * fifelse(sex == 2, 1.012, 1) }
T <- D[tgt == TRUE]
T[, `:=`(e09 = egfr09(he_crea, age, sex), e21 = egfr21(he_crea, age, sex))]
cat(sprintf("  2009식 평균 %.1f, 2021식 평균 %.1f, 차이(2021−2009) 평균 %.1f (범위 %.1f – %.1f), 상관 %.4f\n",
            T[, mean(e09)], T[, mean(e21)], T[, mean(e21 - e09)], T[, min(e21 - e09)], T[, max(e21 - e09)], T[, cor(e09, e21)]))
cat("  eGFR < 60: 2009식", T[e09 < 60, .N], "명, 2021식", T[e21 < 60, .N], "명\n")
m <- merge(T[, .(id, e09, e21)], FL[, .(id, egfr)], by = "id")
cat(sprintf("  FLUSH 자료 egfr와의 최대 차이: 2009식 %.4f, 2021식 %.4f\n", m[, max(abs(egfr - e09))], m[, max(abs(egfr - e21))]))

# ---- 7-3 ----
cat("\n[7-3] 임신·수유 여성 (최종 대상)\n")
print(T[sex == 2, .(임신중_LW_ms3 = sum(lw_ms %in% 3), 수유중_LW_ms4 = sum(lw_ms %in% 4), 검진당일임신_HE_prg1 = sum(he_prg %in% 1),
                   합계 = sum(lw_ms %in% 3:4 | he_prg %in% 1), 그중_고요산혈증 = sum((lw_ms %in% 3:4 | he_prg %in% 1) & he_uacid >= 6))])

# ---- 7-4 ----
cat("\n[7-4] S1 성별 가중 삼분위 (주당 g, 1잔 7 g)\n")
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt, data = D[!is.na(wt)], nest = TRUE)
s1 <- rbindlist(lapply(1:2, function(s) {
  q <- svyquantile(~gwk, subset(des, tgt & sex == s), c(1/3, 2/3), ci = FALSE)
  q <- as.numeric(coef(q))
  rnd <- if (s == 1) c(2.4, 10) * 7 else c(0.4, 2.5) * 7
  x <- T[sex == s]
  cls <- function(v, c) findInterval(v, c)            # 0 저 / 1 중 / 2 고 (a 미만 / a 이상 b 미만 / b 이상)
  data.table(성별 = c("남자", "여자")[s], 실제_33 = round(q[1], 2), 실제_66 = round(q[2], 2),
             반올림_33 = rnd[1], 반올림_66 = rnd[2], 분류가_다른_인원 = sum(cls(x$gwk, q) != cls(x$gwk, rnd)))
}))
print(s1)
cat("  (참고) 실제 삼분위로 나눈 가중 분포(%):\n")
for (s in 1:2) { qq <- unlist(s1[s, .(실제_33, 실제_66)]); x <- T[sex == s]
  w <- x[, .(w = sum(wt)), by = .(g = findInterval(gwk, qq))][order(g)]; cat("   ", c("남자", "여자")[s], paste(round(100 * w$w / sum(w$w), 1), collapse = " / "), "\n") }

# ---- 7-5 ----
cat("\n[7-5] 폐경 전후 탐색 분석의 칸 (여성, 주분석 기준 S2: 여 주 56 g 이상 = 고음주)\n")
W <- T[sex == 2]
W[, flush := fcase(bd9_1 == 2 & bd9_2 == 1, "비홍조", bd9_1 == 1 & bd9_2 == 1, "홍조 소실", bd9_1 == 1 & bd9_2 %in% 2:4, "지속 홍조", default = "신규(제외)")]
W[, meno := fcase(lw_ms %in% 2:4, "폐경 전", lw_ms %in% 5:6, "폐경 후", lw_ms %in% c(1, 7), "기타", default = "결측")]
W[, s2 := fifelse(gwk >= 56, "고", "저")]
print(dcast(W[flush != "신규(제외)" & meno %in% c("폐경 전", "폐경 후")], flush + meno ~ s2, value.var = "he_uacid",
            fun.aggregate = function(v) sprintf("%d(%d)", length(v), sum(v >= 6))))
cat("  칸 표기: n(고요산혈증 사례)\n")

# ---- 7-6 ----
cat("\n[7-6] 대상자 흐름\n")
fA <- c(nrow(D), D[age >= 19, .N], D[drk == TRUE, .N])
fB <- c(nrow(D), D[!is.na(wt_itvex), .N], D[!is.na(wt_itvex) & age >= 19, .N], D[!is.na(wt_itvex) & drk == TRUE, .N])
cat("  가중치 단계 없음: 전체", fA[1], "→ 19세 이상", fA[2], "→ 음주자", fA[3], "\n")
cat("  가중치 단계 먼저: 전체", fB[1], "→ 가중치 있음", fB[2], "→ 19세 이상", fB[3], "→ 음주자", fB[4], "\n")
cat("  음주자 중 가중치 없는 사람:", D[drk == TRUE & is.na(wt_itvex), .N], "명\n")
sink()
fwrite(s1, file.path(out_dir, "7-4_S1_tertiles.csv"), bom = TRUE)
