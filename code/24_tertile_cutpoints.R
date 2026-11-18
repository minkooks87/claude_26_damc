# 24_tertile_cutpoints.R
# 목적: 저·중·고음주의 가중 분율이 각각 1/3에 가장 가깝도록 주당 음주잔수의 절단점을 찾는다.
#       (가) 성별 절단점: 남녀 각각 안에서 1/3씩  (나) 남녀 공통 절단점: 전체에서 1/3씩(성별로는 균등하지 않음)
# 방법: 주당 잔수는 '빈도 대표값 × 1회량 대표값'이라 띄엄띄엄한 값만 가진다. 그래서 정확히 1/3씩 나눌 수는 없다.
#       가능한 경계 조합(저/중 경계 < 중/고 경계)을 모두 살펴, 세 군의 가중 분율과 33.3%의 편차 제곱합이 가장 작은 조합을 고른다.
#       (최대 편차 기준으로 골라도 같은 조합인지 함께 확인한다.)
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
  a[, `:=`(below_wt_pct = cumsum(wt_pct) - wt_pct, below_n_pct = cumsum(n_pct) - n_pct)]   # 이 값을 경계로 삼으면 아랫군(미만)이 되는 분율
  a[, prev_value := shift(drinks_wk)]
  a[, .(scope, drinks_wk, prev_value, most_common_response = pattern, n, wt_pct, below_wt_pct, below_n_pct)]
}
scopes <- list("남자" = dsb[f_sex == "남자"], "여자" = dsb[f_sex == "여자"], "남녀 전체(공통 기준)" = dsb)
cd <- rbindlist(lapply(names(scopes), function(s) cumdist(scopes[[s]], s)))

# ---- 2. 세 군의 분율이 모두 1/3에 가장 가깝도록 경계 조합 고르기 ----
pairs_of <- function(a, col) {                        # 가능한 모든 경계 조합과 세 군의 분율·편차
  a <- a[!is.na(prev_value)]                          # 최솟값은 경계가 될 수 없다
  g <- CJ(i = seq_len(nrow(a)), j = seq_len(nrow(a)))[i < j]
  g[, `:=`(cut1 = a$drinks_wk[i], cut1_prev_value = a$prev_value[i], cut1_response = a$most_common_response[i],
           cut2 = a$drinks_wk[j], cut2_prev_value = a$prev_value[j], cut2_response = a$most_common_response[j],
           low_pct = a[[col]][i], mid_pct = a[[col]][j] - a[[col]][i], high_pct = 100 - a[[col]][j])]
  g[, `:=`(ss_dev = (low_pct - 100 / 3)^2 + (mid_pct - 100 / 3)^2 + (high_pct - 100 / 3)^2,
           max_dev = pmax(abs(low_pct - 100 / 3), abs(mid_pct - 100 / 3), abs(high_pct - 100 / 3)))]
  g[, c("i", "j") := NULL]
  g[order(ss_dev, cut1, cut2)]
}
cand <- rbindlist(lapply(names(scopes), function(s) rbindlist(lapply(c("below_wt_pct", "below_n_pct"), function(col) {
  g <- pairs_of(cd[scope == s], col); m <- g[order(max_dev, cut1, cut2)][1]
  cbind(scope = s, basis = if (col == "below_wt_pct") "가중 분율" else "비가중 분율", rank = 1:5, g[1:5],
        same_as_best_by_max_dev = c(m$cut1 == g$cut1[1] & m$cut2 == g$cut2[1], rep(NA, 4)))
}))))
cuts <- cand[rank == 1]

# ---- 3. 고른 절단점(가중 기준)으로 나눈 군의 크기: 비가중 n + 가중 % (설계 지정 → subset) ----
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
grp[, dev_from_third := wt_pct - 100 / 3]
# 칸별 인원(성별 × 음주 수준 × 현재 홍조·홍조 패턴): 결과 변수는 쓰지 않는다
cells <- rbindlist(lapply(c("t3_sex", "t3_common"), function(tv) rbind(
  d2[, .(n = .N), keyby = .(sex = f_sex, level = get(tv), group = flush_now)][, `:=`(scheme = if (tv == "t3_sex") "성별 삼분위" else "남녀 공통 삼분위", group_var = "현재 홍조")],
  d2[, .(n = .N), keyby = .(sex = f_sex, level = get(tv), group = flush_grp)][, `:=`(scheme = if (tv == "t3_sex") "성별 삼분위" else "남녀 공통 삼분위", group_var = "홍조 패턴(4군)")])))

# ---- 3-2. 쓰기 쉬운 반올림 기준값: 정확한 경계값으로 나눈 것과 한 사람도 다르지 않은지 확인한다 ----
round_cut <- list("남자" = c(2.4, 10), "여자" = c(0.4, 2.5), "남녀 전체(공통 기준)" = c(1, 5.7))
chk <- rbindlist(lapply(names(round_cut), function(s) { x <- scopes[[s]]
  a <- cut(x$dw, c(-Inf, cut_of(s), Inf), right = FALSE, labels = lv3)
  b <- cut(x$drinks_wk, c(-Inf, round_cut[[s]], Inf), right = FALSE, labels = lv3)          # 반올림하지 않은 원래 변수에 적용
  data.table(scope = s, round_cut1 = round_cut[[s]][1], round_cut2 = round_cut[[s]][2], exact_cut1 = cut_of(s)[1], exact_cut2 = cut_of(s)[2],
             n_low = sum(b == lv3[1]), n_mid = sum(b == lv3[2]), n_high = sum(b == lv3[3]), identical_grouping = identical(a, b)) }))
stopifnot(all(chk$identical_grouping))

# ---- 4. 저장 ----
fwrite(round_dt(cd),    file.path(out, "01_value_distribution.csv"), bom = TRUE)
fwrite(round_dt(cuts),  file.path(out, "02_tertile_cutpoints.csv"), bom = TRUE)
fwrite(round_dt(cand),  file.path(out, "02b_top5_cutpoint_pairs.csv"), bom = TRUE)
fwrite(round_dt(grp),   file.path(out, "03_group_sizes.csv"), bom = TRUE)
fwrite(cells,           file.path(out, "04_cell_counts_by_flush.csv"), bom = TRUE)
fwrite(round_dt(chk),   file.path(out, "05_recommended_round_cutpoints.csv"), bom = TRUE)
old <- file.path(out, "02b_candidates_near_target.csv"); if (file.exists(old)) file.remove(old)   # 이전 방식(경계별 근접)의 산출물은 이 스크립트가 만든 것이므로 정리

options(width = 220)
cat("\n\n================ 삼분위 절단점 (세 군 분율의 편차 제곱합 최소) ================\n")
print(cand[, .(scope, basis, rank, cut1 = round(cut1, 2), prev1 = round(cut1_prev_value, 2), cut2 = round(cut2, 2), prev2 = round(cut2_prev_value, 2),
               low = round(low_pct, 1), mid = round(mid_pct, 1), high = round(high_pct, 1), ss = round(ss_dev, 1), maxdev = round(max_dev, 1), same_as_best_by_max_dev)])
cat("\n[군별 크기]\n"); print(grp[, .(scheme, sex, level, cut_low = round(cut_low, 2), n, n_pct = round(n_pct, 1), wt_pct = round(wt_pct, 1), wt_se = round(wt_se, 1),
                                         dev = round(dev_from_third, 1), range = sprintf("%.2f-%.2f", drinks_min, drinks_max), mean = round(drinks_mean_wt, 1))])
cat("\n[반올림 기준값 확인]\n"); print(chk)
cat("\n[칸별 인원: 성별 삼분위 × 현재 홍조]\n"); print(dcast(cells[scheme == "성별 삼분위" & group_var == "현재 홍조"], sex + group ~ level, value.var = "n"))
cat("\n[칸별 인원: 성별 삼분위 × 홍조 패턴]\n"); print(dcast(cells[scheme == "성별 삼분위" & group_var == "홍조 패턴(4군)"], sex + group ~ level, value.var = "n"))
cat("\n[칸별 인원: 공통 삼분위 × 현재 홍조]\n"); print(dcast(cells[scheme == "남녀 공통 삼분위" & group_var == "현재 홍조"], sex + group ~ level, value.var = "n"))
cat("\n완료:", out, "\n")
