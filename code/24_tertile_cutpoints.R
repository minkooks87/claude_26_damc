# 24_tertile_cutpoints.R
# 목적: 저·중·고음주가 각각 약 1/3이 되도록 주당 음주잔수의 절단점(삼분위)을 찾는다.
#       (가) 성별 절단점: 남녀 각각 안에서 1/3씩  (나) 남녀 공통 절단점: 전체에서 1/3씩(성별로는 균등하지 않음)
# 방법: 주당 잔수는 '빈도 대표값 × 1회량 대표값'이라 띄엄띄엄한 값만 가진다. 그래서 가능한 모든 경계값에 대해
#       '그 값 미만인 사람의 가중 분율'을 구하고, 33.3%와 66.7%에 가장 가까운 경계를 고른다.
#       분율은 복합표본 가중치(wt_itvex × 1/3) 기준이며, 비가중 인원도 함께 적는다.
# 변수: 음주 빈도 BD1_11·1회량 BD2_1·BD2_14 (8기 PDF p.161-162 / 인쇄 p.153-154), 대상·가중치·환산표는 code/13_v3_main_analysis.R과 같다.
# 주의: 삼분위는 이 표본의 분포로 정하는 자료 의존적 기준이다. 결과(요산)는 쓰지 않고 노출 분포만으로 정한다.
# 실행: Rscript --vanilla code/24_tertile_cutpoints.R   (프로젝트 폴더에서)
# 결과: output/19_tertile_cutpoints/

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")

out <- file.path("output", "19_tertile_cutpoints")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
lv3 <- c("저음주", "중음주", "고음주")
dsb <- dat_of(sdes)                                   # 최종 분석 대상(행 삭제 없이 subset한 설계의 자료)
dsb[, dw := round(drinks_wk, 4)]                      # 부동소수 오차로 같은 값이 갈라지지 않게
freq_lab <- c(`2` = "월 1회 미만", `3` = "월 1회 정도", `4` = "월 2-4회", `5` = "주 2-3회", `6` = "주 4회 이상")
amt_lab  <- c(`1` = "1-2잔", `2` = "3-4잔", `3` = "5-6잔", `4` = "7-9잔", `5` = "10잔 이상")

# ---- 1. 값별 분포와 '그 값 미만'의 누적 분율 ----
cumdist <- function(x, scope) {
  a <- x[, .(n = .N, w = sum(wt_analysis),
             pattern = { p <- sort(table(paste0(freq_lab[as.character(bd1_11)], " × ", amt_lab[as.character(bd2_1)])), decreasing = TRUE); names(p)[1] }),
         keyby = .(drinks_wk = dw)]
  a[, `:=`(scope = scope, wt_pct = 100 * w / sum(w), n_pct = 100 * n / sum(n))]
  a[, `:=`(below_wt_pct = cumsum(wt_pct) - wt_pct, below_n_pct = cumsum(n_pct) - n_pct)]   # 이 값을 경계로 삼으면 저음주(미만)가 되는 분율
  a[, prev_value := shift(drinks_wk)]
  a[, .(scope, drinks_wk, prev_value, most_common_response = pattern, n, wt_pct, below_wt_pct, below_n_pct)]
}
scopes <- list("남자" = dsb[f_sex == "남자"], "여자" = dsb[f_sex == "여자"], "남녀 전체(공통 기준)" = dsb)
cd <- rbindlist(lapply(names(scopes), function(s) cumdist(scopes[[s]], s)))

# ---- 2. 33.3%·66.7%에 가장 가까운 경계 고르기 (가중 기준, 비가중 기준은 참고) ----
pick <- function(a, target, col) a[which.min(abs(get(col) - target))]
cuts <- rbindlist(lapply(names(scopes), function(s) { a <- cd[scope == s]
  rbindlist(lapply(c("below_wt_pct", "below_n_pct"), function(col) {
    c1 <- pick(a, 100 / 3, col); c2 <- pick(a, 200 / 3, col)
    data.table(scope = s, basis = if (col == "below_wt_pct") "가중 분율" else "비가중 분율",
               cut1 = c1$drinks_wk, cut1_prev_value = c1$prev_value, cut1_below_pct = c1[[col]], cut1_response = c1$most_common_response,
               cut2 = c2$drinks_wk, cut2_prev_value = c2$prev_value, cut2_below_pct = c2[[col]], cut2_response = c2$most_common_response)
  }))
}))
# 후보 비교용: 목표 근처의 경계값 몇 개씩
near <- rbindlist(lapply(names(scopes), function(s) { a <- cd[scope == s]
  rbind(cbind(target = "저/중 경계(33.3%)", a[order(abs(below_wt_pct - 100 / 3))][1:4]), cbind(target = "중/고 경계(66.7%)", a[order(abs(below_wt_pct - 200 / 3))][1:4]))
}))

# ---- 3. 고른 절단점으로 나눈 군의 크기: 비가중 n + 가중 % (설계 지정 → subset) ----
cw <- cuts[basis == "가중 분율"]
cut_of <- function(s) c(cw[scope == s, cut1], cw[scope == s, cut2])
v <- sdes$variables
v$dw <- round(v$drinks_wk, 4)
v$t3_sex <- factor(NA_character_, lv3)
for (s in sexes) { k <- v$f_sex == s; v$t3_sex[k] <- as.character(cut(v$dw[k], c(-Inf, cut_of(s), Inf), right = FALSE, labels = lv3)) }
v$t3_common <- cut(v$dw, c(-Inf, cut_of("남녀 전체(공통 기준)"), Inf), right = FALSE, labels = lv3)
sd2 <- sdes; sd2$variables <- v; d2 <- dat_of(sd2)
grp <- rbindlist(lapply(c("t3_sex", "t3_common"), function(tv) rbindlist(lapply(sexes, function(s) {
  ds <- dsub(sd2, d2$f_sex == s); est <- svymean(as.formula(paste0("~", tv)), ds); dd <- d2[f_sex == s]
  mn <- sapply(lv3, function(e) unname(coef(svymean(~drinks_wk, dsub(sd2, d2$f_sex == s & d2[[tv]] == e)))[1]))
  data.table(scheme = if (tv == "t3_sex") "성별 삼분위" else "남녀 공통 삼분위", sex = s, level = lv3,
             cut_low = c(NA, cut_of(if (tv == "t3_sex") s else "남녀 전체(공통 기준)")), n = as.integer(table(dd[[tv]])[lv3]),
             n_pct = 100 * as.numeric(table(dd[[tv]])[lv3]) / nrow(dd),
             wt_pct = 100 * unname(coef(est)[paste0(tv, lv3)]), wt_se = 100 * unname(SE(est)[paste0(tv, lv3)]),
             drinks_min = as.numeric(tapply(dd$drinks_wk, dd[[tv]], min)[lv3]), drinks_max = as.numeric(tapply(dd$drinks_wk, dd[[tv]], max)[lv3]), drinks_mean_wt = mn)
}))))
# 칸별 인원(성별 × 음주 수준 × 현재 홍조·홍조 이력): 결과 변수는 쓰지 않는다
cells <- rbindlist(lapply(c("t3_sex", "t3_common"), function(tv) rbind(
  d2[, .(n = .N), keyby = .(sex = f_sex, level = get(tv), group = flush_now)][, `:=`(scheme = if (tv == "t3_sex") "성별 삼분위" else "남녀 공통 삼분위", group_var = "현재 홍조")],
  d2[, .(n = .N), keyby = .(sex = f_sex, level = get(tv), group = flush_grp)][, `:=`(scheme = if (tv == "t3_sex") "성별 삼분위" else "남녀 공통 삼분위", group_var = "홍조 패턴(4군)")])))

# ---- 3-2. 쓰기 쉬운 반올림 기준값: 정확한 경계값으로 나눈 것과 한 사람도 다르지 않은지 확인한다 ----
round_cut <- list("남자" = c(2.4, 10), "여자" = c(0.4, 2.4), "남녀 전체(공통 기준)" = c(1, 5.7))
chk <- rbindlist(lapply(names(round_cut), function(s) { x <- scopes[[s]]
  a <- cut(x$dw, c(-Inf, cut_of(s), Inf), right = FALSE, labels = lv3)
  b <- cut(x$drinks_wk, c(-Inf, round_cut[[s]], Inf), right = FALSE, labels = lv3)          # 반올림하지 않은 원래 변수에 적용
  data.table(scope = s, round_cut1 = round_cut[[s]][1], round_cut2 = round_cut[[s]][2], exact_cut1 = cut_of(s)[1], exact_cut2 = cut_of(s)[2],
             n_low = sum(b == lv3[1]), n_mid = sum(b == lv3[2]), n_high = sum(b == lv3[3]), identical_grouping = identical(a, b)) }))
stopifnot(all(chk$identical_grouping))
fwrite(round_dt(chk), file.path(out, "05_recommended_round_cutpoints.csv"), bom = TRUE)

# ---- 4. 저장 ----
fwrite(round_dt(cd),    file.path(out, "01_value_distribution.csv"), bom = TRUE)
fwrite(round_dt(cuts),  file.path(out, "02_tertile_cutpoints.csv"), bom = TRUE)
fwrite(round_dt(near),  file.path(out, "02b_candidates_near_target.csv"), bom = TRUE)
fwrite(round_dt(grp),   file.path(out, "03_group_sizes.csv"), bom = TRUE)
fwrite(cells,           file.path(out, "04_cell_counts_by_flush.csv"), bom = TRUE)

options(width = 220)
cat("\n\n================ 삼분위 절단점 ================\n")
print(cuts[, .(scope, basis, cut1 = round(cut1, 2), prev1 = round(cut1_prev_value, 2), below1 = round(cut1_below_pct, 1), resp1 = cut1_response,
               cut2 = round(cut2, 2), prev2 = round(cut2_prev_value, 2), below2 = round(cut2_below_pct, 1), resp2 = cut2_response)])
cat("\n[목표 근처의 후보 경계값(가중)]\n"); print(near[, .(scope, target, drinks_wk = round(drinks_wk, 2), prev = round(prev_value, 2), resp = most_common_response, n, below_wt = round(below_wt_pct, 1), below_n = round(below_n_pct, 1))])
cat("\n[군별 크기]\n"); print(grp[, .(scheme, sex, level, cut_low = round(cut_low, 2), n, n_pct = round(n_pct, 1), wt_pct = round(wt_pct, 1), wt_se = round(wt_se, 1),
                                         range = sprintf("%.2f-%.2f", drinks_min, drinks_max), mean = round(drinks_mean_wt, 1))])
cat("\n[칸별 인원: 성별 삼분위 × 현재 홍조]\n"); print(dcast(cells[scheme == "성별 삼분위" & group_var == "현재 홍조"], sex + group ~ level, value.var = "n"))
cat("\n[칸별 인원: 성별 삼분위 × 홍조 패턴]\n"); print(dcast(cells[scheme == "성별 삼분위" & group_var == "홍조 패턴(4군)"], sex + group ~ level, value.var = "n"))
cat("\n[칸별 인원: 공통 삼분위 × 현재 홍조]\n"); print(dcast(cells[scheme == "남녀 공통 삼분위" & group_var == "현재 홍조"], sex + group ~ level, value.var = "n"))
cat("\n기존 13번 스크립트의 성별 가중 삼분위(svyquantile):\n"); print(tert)
cat("\n완료:", out, "\n")
