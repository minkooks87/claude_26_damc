# 19_continuous_main.R
# 목적: 주질문(음주 × 현재 홍조)을 노출(주당 음주잔수)과 결과(혈청 요산)를 모두 연속형으로 두고 분석한다.
#       모형: 요산 ~ 음주량(잔/일) × 현재 홍조 + 보정변수 (복합표본 선형회귀, 남녀 각각, Crude·Model 1·2·3)
#       상호작용 p는 '음주량 × 홍조' 항 하나(1 df)의 설계 기반 Wald F 검정이다.
#       보조: 극단값 영향(99백분위 절단), log 변환, 비선형성(2차항), 고요산혈증 이분형 결과(연속 노출).
# 주의: 계획서의 주분석은 범주형(3.5·14잔)이며, 이 분석은 보조 분석이다. 여성의 요산 연속형 분석은 계획서에 있었다.
# 실행: Rscript --vanilla code/19_continuous_main.R   (프로젝트 폴더에서, 약 2분)
# 결과: output/16_continuous_main/

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")   # 자료·변수·설계 객체·함수를 그대로 가져온다

out4 <- file.path("output", "16_continuous_main")
dir.create(out4, showWarnings = FALSE, recursive = TRUE)
b_ci <- function(fit, nm) {              # 계수 합의 추정치, 95% CI, p (항등 척도)
  b <- coef(fit); V <- vcov(fit); stopifnot(all(nm %in% names(b)))
  l <- setNames(rep(0, length(b)), names(b)); l[nm] <- 1
  est <- sum(l * b); se <- sqrt(as.numeric(t(l) %*% V %*% l))
  c(est = est, lo = est - z975 * se, hi = est + z975 * se, p = 2 * pnorm(-abs(est / se)))
}
# ---- 노출 변수: 잔/일 (= 잔/주 ÷ 7). 1단위 = 하루 1잔 = 주 7잔 ----
dist <- list(); prep <- list()
for (s in sexes) {
  ds <- sdes_sex[[s]]
  q <- svyquantile(~drinks_wk, ds, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)
  qv <- as.numeric(coef(q))
  dist[[s]] <- data.table(sex = s, n = nrow(ds$variables), p25 = qv[1], p50 = qv[2], p75 = qv[3], p90 = qv[4], p95 = qv[5], p99 = qv[6],
                          max = max(ds$variables$drinks_wk), pct_over_28_unw = 100 * mean(ds$variables$drinks_wk >= 28))
  ds$variables$drinks7  <- ds$variables$drinks_wk / 7
  ds$variables$drinks7w <- pmin(ds$variables$drinks_wk, qv[6]) / 7          # 99백분위(가중)에서 절단
  ds$variables$logdrk   <- log2(ds$variables$drinks_wk + 1)                 # 두 배가 될 때마다의 변화
  ds$variables$drinks7sq <- ds$variables$drinks7^2
  prep[[s]] <- ds
}
dist <- rbindlist(dist)
fwrite(round_dt(dist), file.path(out4, "01_drinks_distribution.csv"), bom = TRUE)

# ---- 주분석: 요산(연속) ~ 잔/일 × 현재 홍조 + 보정변수 ----
slope_row <- function(fit, xvar, n, binary = FALSE) {
  tr <- if (binary) exp else identity
  b0 <- b_ci(fit, xvar); b1 <- b_ci(fit, c(xvar, paste0(xvar, ":flush_now홍조"))); d <- b_ci(fit, paste0(xvar, ":flush_now홍조"))
  f0 <- b_ci(fit, "flush_now홍조")
  data.table(n_model = n,
             nonflush_est = tr(b0[["est"]]), nonflush_lo = tr(b0[["lo"]]), nonflush_hi = tr(b0[["hi"]]), nonflush_p = b0[["p"]],
             flush_est = tr(b1[["est"]]), flush_lo = tr(b1[["lo"]]), flush_hi = tr(b1[["hi"]]), flush_p = b1[["p"]],
             diff_est = tr(d[["est"]]), diff_lo = tr(d[["lo"]]), diff_hi = tr(d[["hi"]]), p_interaction = term_test(fit, paste0(xvar, ":flush_now"))$p,
             flush_at_zero_est = tr(f0[["est"]]), flush_at_zero_lo = tr(f0[["lo"]]), flush_at_zero_hi = tr(f0[["hi"]]))
}
main <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(names(models4), function(mdl) {
  r <- fit_svy(prep[[s]], mk_form("uacid", "drinks7 * flush_now", models4[[mdl]]), gaussian())
  cbind(outcome = "혈청 요산(mg/dL)", exposure = "잔/일(연속)", sex = s, model = mdl, slope_row(r$fit, "drinks7", r$n))
}))))

# ---- 보조 분석 (Model 2·3) ----
supp <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(c("Model 2", "Model 3"), function(mdl) {
  covs <- models4[[mdl]]
  r1 <- fit_svy(prep[[s]], mk_form("uacid", "drinks7w * flush_now", covs), gaussian())
  r2 <- fit_svy(prep[[s]], mk_form("uacid", "logdrk * flush_now", covs), gaussian())
  r3 <- fit_svy(prep[[s]], mk_form("hua", "drinks7 * flush_now", covs), quasibinomial())
  r4 <- fit_svy(prep[[s]], mk_form("uacid", c("drinks7 * flush_now", "drinks7sq * flush_now"), covs), gaussian())
  rbind(cbind(outcome = "혈청 요산(mg/dL)", exposure = "잔/일, 99백분위 절단", sex = s, model = mdl, slope_row(r1$fit, "drinks7w", r1$n)),
        cbind(outcome = "혈청 요산(mg/dL)", exposure = "log2(잔/주 + 1): 두 배당", sex = s, model = mdl, slope_row(r2$fit, "logdrk", r2$n)),
        cbind(outcome = "고요산혈증(OR)", exposure = "잔/일(연속)", sex = s, model = mdl, slope_row(r3$fit, "drinks7", r3$n, binary = TRUE)),
        cbind(outcome = "혈청 요산(mg/dL)", exposure = "잔/일 + 2차항(비선형 확인)", sex = s, model = mdl,
              data.table(n_model = r4$n, nonflush_est = NA_real_, nonflush_lo = NA_real_, nonflush_hi = NA_real_, nonflush_p = NA_real_,
                         flush_est = NA_real_, flush_lo = NA_real_, flush_hi = NA_real_, flush_p = NA_real_,
                         diff_est = NA_real_, diff_lo = NA_real_, diff_hi = NA_real_,
                         p_interaction = term_test(r4$fit, "drinks7:flush_now")$p,
                         flush_at_zero_est = term_test(r4$fit, "drinks7sq")$p,                 # (임시 저장) 2차항 p
                         flush_at_zero_lo = term_test(r4$fit, "flush_now:drinks7sq")$p,        # (임시 저장) 2차항 × 홍조 p
                         flush_at_zero_hi = NA_real_)))
}))))
nonlin <- supp[grepl("2차항", exposure), .(sex, model, n_model, p_quadratic = flush_at_zero_est, p_quadratic_x_flush = flush_at_zero_lo, p_linear_interaction = p_interaction)]
supp <- supp[!grepl("2차항", exposure)]
fwrite(round_dt(main),   file.path(out4, "02_main_continuous.csv"), bom = TRUE)
fwrite(round_dt(supp),   file.path(out4, "03_supplementary.csv"), bom = TRUE)
fwrite(round_dt(nonlin), file.path(out4, "04_nonlinearity_check.csv"), bom = TRUE)

# ---- 요약 메모 ----
ci2 <- function(e, l, h, d = 3) ifelse(is.na(e), "", sprintf(paste0("%.", d, "f (%.", d, "f, %.", d, "f)"), e, l, h))
tab_md <- function(x, d = 3) md_tab(x[, .(성별 = sex, 모형 = model, `모형 n` = format(n_model, big.mark = ","),
                                          `비홍조군 기울기 (95% CI)` = ci2(nonflush_est, nonflush_lo, nonflush_hi, d), `p ` = fmt_p(nonflush_p),
                                          `홍조군 기울기 (95% CI)` = ci2(flush_est, flush_lo, flush_hi, d), p = fmt_p(flush_p),
                                          `차이 (95% CI)` = ci2(diff_est, diff_lo, diff_hi, d), `상호작용 p` = fmt_p(p_interaction),
                                          `홍조 효과(0잔에서)` = ci2(flush_at_zero_est, flush_at_zero_lo, flush_at_zero_hi, d))])
g <- function(s, m) main[sex == s & model == m]
mm <- c("# 주질문의 연속형 분석: 요산(mg/dL) ~ 음주량(잔/일) × 현재 홍조 — 보조 분석", "",
        "- 노출: 주당 음주잔수 ÷ 7 = 하루 잔수. 기울기 1단위 = 하루 1잔(주 7잔, 약 49 g/주) 늘 때 요산 변화(mg/dL).",
        "- 상호작용 p: 음주량 × 홍조 항 하나(1 df)의 설계 기반 Wald F 검정. '차이'는 홍조군 기울기 − 비홍조군 기울기.",
        "- '홍조 효과(0잔에서)'는 음주량이 0일 때 홍조군과 비홍조군의 요산 차이(외삽이므로 참고용).",
        "- 계획서의 주분석은 범주형이며 이 분석은 보조 분석이다. 여성 요산 연속형은 계획서에 있었다.", "",
        "## 1. 음주량 분포 (가중 백분위수, 잔/주)", "",
        md_tab(dist[, .(성별 = sex, n = format(n, big.mark = ","), p25 = sprintf("%.1f", p25), 중앙값 = sprintf("%.1f", p50), p75 = sprintf("%.1f", p75), p90 = sprintf("%.1f", p90),
                        p95 = sprintf("%.1f", p95), p99 = sprintf("%.1f", p99), 최댓값 = sprintf("%.0f", max), `28잔 이상 %(비가중)` = sprintf("%.1f", pct_over_28_unw))]),
        "## 2. 주분석: 하루 1잔당 요산 변화(mg/dL)", "", tab_md(main),
        sprintf("- Model 2: 하루 1잔당 요산은 남자 비홍조 %s, 홍조 %s mg/dL(차이 p = %s), 여자 비홍조 %s, 홍조 %s(차이 p = %s).",
                ci2(g("남자", "Model 2")$nonflush_est, g("남자", "Model 2")$nonflush_lo, g("남자", "Model 2")$nonflush_hi), ci2(g("남자", "Model 2")$flush_est, g("남자", "Model 2")$flush_lo, g("남자", "Model 2")$flush_hi), fmt_p(g("남자", "Model 2")$p_interaction),
                ci2(g("여자", "Model 2")$nonflush_est, g("여자", "Model 2")$nonflush_lo, g("여자", "Model 2")$nonflush_hi), ci2(g("여자", "Model 2")$flush_est, g("여자", "Model 2")$flush_lo, g("여자", "Model 2")$flush_hi), fmt_p(g("여자", "Model 2")$p_interaction)), "",
        "## 3. 보조: 극단값 절단, log 변환, 고요산혈증(OR, 하루 1잔당)", "",
        tab_md(supp[outcome == "혈청 요산(mg/dL)" & grepl("절단", exposure)]), tab_md(supp[grepl("log2", exposure)]), tab_md(supp[outcome == "고요산혈증(OR)"], d = 2),
        "## 4. 비선형성 확인 (요산 ~ 잔/일 + 잔/일² , 홍조와의 곱 포함)", "",
        md_tab(nonlin[, .(성별 = sex, 모형 = model, `모형 n` = format(n_model, big.mark = ","), `2차항 p` = fmt_p(p_quadratic), `2차항 × 홍조 p` = fmt_p(p_quadratic_x_flush), `1차 상호작용 p(2차항 포함 모형)` = fmt_p(p_linear_interaction))]),
        "- 2차항 p가 작으면 직선 관계가 아니라는 뜻이므로 범주형 또는 변환한 분석을 함께 봐야 한다.")
con <- file(file.path(out4, "SUMMARY_연속형_주질문.md"), open = "w", encoding = "UTF-8"); writeLines(mm, con); close(con)
cat("\n[주분석]\n"); print(main[, .(sex, model, nonflush = round(nonflush_est, 3), flush = round(flush_est, 3), diff = round(diff_est, 3), p_int = signif(p_interaction, 3))])
cat("\n[비선형성]\n"); print(nonlin)
cat("\n완료:", out4, "\n")
