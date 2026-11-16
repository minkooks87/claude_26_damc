# 17_cutpoint_exploration_subq1.R
# 목적: 부질문 1(홍조 이력 3군)의 음주 수준 절단점(기본 3.5잔·14잔/주)을 격자로 바꿔 가며
#       상호작용 검정 p가 어떻게 달라지는지 본다 (절단점 민감도, specification curve).
# 주의: 사전 지정 분석이 아닌 사후 탐색이다. 여기서 p < 0.05인 절단점만 골라 주 결과로 보고하면 p-해킹이다.
#       격자 전체(모든 절단점의 p)를 그대로 보고하는 용도로만 쓴다. 주 결과는 계획서의 3.5·14잔이다.
# 실행: Rscript --vanilla code/17_cutpoint_exploration_subq1.R   (프로젝트 폴더에서, 약 4분)
# 결과: output/15_cutpoint_exploration/

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")   # 자료·변수·설계 객체·함수를 그대로 가져온다 (13의 결과 파일도 같은 값으로 다시 생성된다)

out2 <- file.path("output", "15_cutpoint_exploration")
dir.create(out2, showWarnings = FALSE, recursive = TRUE)
lows  <- c(1, 2, 3, 3.5, 4, 5, 7)          # 저/중 절단점(잔/주)
highs <- c(7, 10, 12, 14, 16, 21, 28)      # 중/고 절단점(잔/주)
grid  <- CJ(low = lows, high = highs)[low < high]
models_g <- list("Model 2" = cov_base, "Model 3" = cov_ext)
getn <- function(t, fh, dx, col) { v <- t[flush_hist == fh & drink3x == dx]; if (nrow(v)) v[[col]] else 0L }

run_cell <- function(s, mdl, low, high) {
  ds <- sdes_sex[[s]]
  ds$variables$drink3x <- cut(ds$variables$drinks_wk, c(-Inf, low, high, Inf), right = FALSE, labels = c("저", "중", "고"))
  dat <- dat_of(ds)[!is.na(flush_hist)]
  tab <- dat[, .(N = .N, ev = sum(hua)), by = .(flush_hist, drink3x)]
  res <- tryCatch({
    r <- fit_svy(ds, mk_form("hua", "drink3x * flush_hist", models_g[[mdl]]), quasibinomial())
    f <- r$fit
    o  <- or_ci(f, c("drink3x고", "drink3x고:flush_hist홍조 소실"))   # 소실군 안에서 고 vs 저
    o0 <- or_ci(f, "drink3x고")                                      # 비홍조군 안에서 고 vs 저
    data.table(n_model = r$n,
               p_joint_4df = term_test(f, "drink3x:flush_hist")$p,
               p_lost_vs_non = contrast_test(f, list(c("drink3x중:flush_hist홍조 소실" = 1), c("drink3x고:flush_hist홍조 소실" = 1)))$p,
               p_persist_vs_non = contrast_test(f, list(c("drink3x중:flush_hist지속 홍조" = 1), c("drink3x고:flush_hist지속 홍조" = 1)))$p,
               or_high_lost = o[["est"]], or_high_lost_lo = o[["lo"]], or_high_lost_hi = o[["hi"]], or_high_non = o0[["est"]])
  }, error = function(e) data.table(n_model = NA_integer_, p_joint_4df = NA_real_, p_lost_vs_non = NA_real_, p_persist_vs_non = NA_real_,
                                    or_high_lost = NA_real_, or_high_lost_lo = NA_real_, or_high_lost_hi = NA_real_, or_high_non = NA_real_))
  cbind(data.table(sex = s, model = mdl, low = low, high = high,
                   n_lost_mid = getn(tab, "홍조 소실", "중", "N"), ev_lost_mid = getn(tab, "홍조 소실", "중", "ev"),
                   n_lost_high = getn(tab, "홍조 소실", "고", "N"), ev_lost_high = getn(tab, "홍조 소실", "고", "ev"),
                   min_cell_n = min(tab$N), min_cell_events = min(tab$ev)), res)
}
res_all <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(names(models_g), function(mdl)
  rbindlist(lapply(seq_len(nrow(grid)), function(i) run_cell(s, mdl, grid$low[i], grid$high[i])))))))
res_all[, prespecified := low == 3.5 & high == 14]
res_all[, unstable := min_cell_events < 5]        # 어느 칸이든 사례 5명 미만이면 추정이 불안정하다
fwrite(round_dt(res_all), file.path(out2, "01_grid_results.csv"), bom = TRUE)

# 요약: 검정별로 p < 0.05인 절단점 수, 가장 작은 p
summ <- rbindlist(lapply(c("p_joint_4df", "p_lost_vs_non", "p_persist_vs_non"), function(tp) res_all[, {
  p <- get(tp); i <- which.min(p)
  .(test = tp, n_specs = .N, n_p_below_05 = sum(p < 0.05, na.rm = TRUE), n_p_below_05_stable = sum(p < 0.05 & !unstable, na.rm = TRUE),
    n_unstable = sum(unstable), min_p = min(p, na.rm = TRUE), min_p_low = low[i], min_p_high = high[i], min_p_unstable = unstable[i],
    p_prespecified = p[prespecified])
}, by = .(sex, model)]))
fwrite(round_dt(summ), file.path(out2, "02_summary.csv"), bom = TRUE)

# 그림: 소실 vs 비홍조 검정 p의 격자 (행 = 저/중 절단점, 열 = 중/고 절단점). 어두울수록 p가 작다.
fam <- "sans"; if (.Platform$OS.type == "windows") { windowsFonts(kr = windowsFont("Malgun Gothic")); fam <- "kr" }
ramp <- colorRampPalette(c("#0d366b", "#cde2fb"))(100)      # dataviz 참조 팔레트의 파란 순차 램프(진함 → 연함)
for (mdl in names(models_g)) {
  png(file.path(out2, paste0("fig_p_grid_", sub(" ", "", tolower(mdl)), ".png")), width = 11, height = 5.2, units = "in", res = 200, family = fam, bg = "white")
  par(mfrow = c(1, 2), mar = c(4.2, 4.5, 4.2, 1), oma = c(2.2, 0, 0, 0), family = fam)   # 아래 바깥 여백에 설명 한 줄
  for (s in sexes) {
    m <- matrix(NA_real_, length(lows), length(highs), dimnames = list(lows, highs))
    u <- matrix(FALSE, length(lows), length(highs))
    for (i in seq_along(lows)) for (j in seq_along(highs)) {
      r <- res_all[sex == s & model == mdl & low == lows[i] & high == highs[j]]
      if (nrow(r)) { m[i, j] <- r$p_lost_vs_non; u[i, j] <- r$unstable }
    }
    image(seq_along(lows), seq_along(highs), m, zlim = c(0, 1), col = ramp, axes = FALSE, xlab = "저/중 절단점 (잔/주)", ylab = "중/고 절단점 (잔/주)",
          main = sprintf("%s · %s: 소실 vs 비홍조 상호작용 p", s, mdl), cex.main = 0.95)
    axis(1, seq_along(lows), lows, col = NA, col.ticks = NA); axis(2, seq_along(highs), highs, col = NA, col.ticks = NA, las = 1)
    for (i in seq_along(lows)) for (j in seq_along(highs)) if (!is.na(m[i, j]))
      text(i, j, paste0(sprintf("%.2f", m[i, j]), ifelse(u[i, j], "*", "")), cex = 0.75, col = ifelse(m[i, j] < 0.35, "white", "#0b0b0b"))
    rect(which(lows == 3.5) - 0.5, which(highs == 14) - 0.5, which(lows == 3.5) + 0.5, which(highs == 14) + 0.5, border = "#eb6834", lwd = 2.5)
  }
  mtext("주황 테두리 = 계획서의 절단점(3.5·14잔). * = 어느 칸이든 사례 5명 미만(불안정). 회색 칸 없음 = 모형 적합 실패",
        side = 1, line = 0.8, outer = TRUE, cex = 0.75, col = "#52514e")
  dev.off()
}

# 요약 메모
mm2 <- c("# 부질문 1 절단점 격자 탐색 (사후 탐색, 주 결과 아님)", "",
         sprintf("- 격자: 저/중 절단점 %s, 중/고 절단점 %s → 절단점 조합 %d개 × 남녀 × 모형 2개.", paste(lows, collapse = "·"), paste(highs, collapse = "·"), nrow(grid)),
         "- 검정: 음주 수준 × 홍조 이력 공동 검정(4 df), 소실 vs 비홍조(2 df), 지속 vs 비홍조(2 df). 모두 설계 기반 Wald F.",
         "- 주의: 사전 지정 분석(3.5·14잔)이 아니라 절단점을 바꿔 본 사후 탐색이다. p < 0.05인 절단점만 골라 보고하면 p-해킹이다. 보고한다면 격자 전체를 보고한다.",
         sprintf("- 사례 수가 적은 칸(어느 칸이든 사례 5명 미만)이 있는 조합은 '불안정'으로 표시했다. 조합당 검정 3개를 %d개 조합에 적용했으므로, 차이가 전혀 없어도 우연히 p < 0.05가 몇 개 나오는 것이 정상이다.", nrow(grid)), "",
         "## 요약", "", md_tab(summ[, .(성별 = sex, 모형 = model, 검정 = test, `조합 수` = n_specs, `p<0.05 조합` = n_p_below_05, `그중 안정` = n_p_below_05_stable,
                                           `불안정 조합` = n_unstable, `최소 p` = fmt_p(min_p), `최소 p 절단점` = paste0(min_p_low, "·", min_p_high),
                                           `최소 p 불안정?` = ifelse(min_p_unstable, "예", "아니오"), `계획서 절단점 p` = fmt_p(p_prespecified))]),
         "## p < 0.05가 나온 조합 (검정별)", "",
         md_tab(res_all[p_lost_vs_non < 0.05 | p_joint_4df < 0.05 | p_persist_vs_non < 0.05,
                        .(성별 = sex, 모형 = model, `저/중` = low, `중/고` = high, `소실·고 n(사례)` = paste0(n_lost_high, " (", ev_lost_high, ")"),
                          `소실 고 OR` = est_ci(or_high_lost, or_high_lost_lo, or_high_lost_hi), `비홍조 고 OR` = sprintf("%.2f", or_high_non),
                          `공동 p` = fmt_p(p_joint_4df), `소실 vs 비홍조 p` = fmt_p(p_lost_vs_non), `지속 vs 비홍조 p` = fmt_p(p_persist_vs_non),
                          불안정 = ifelse(unstable, "예", ""))]),
         "![Model 2](fig_p_grid_model2.png)", "", "![Model 3](fig_p_grid_model3.png)", "")
con <- file(file.path(out2, "SUMMARY_절단점_탐색.md"), open = "w", encoding = "UTF-8"); writeLines(mm2, con); close(con)
cat("\n[요약]\n"); print(summ[, .(sex, model, test, n_specs, n_p_below_05, n_p_below_05_stable, min_p = signif(min_p, 2), min_p_low, min_p_high, p_pre = signif(p_prespecified, 2))])
cat("\n완료:", out2, "\n")
