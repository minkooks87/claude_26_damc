# 20_continuous_subq.R
# 목적: 부질문 1(홍조 이력 3군)과 부질문 2(성별·폐경)를 노출(하루 잔수)과 결과(혈청 요산)를 연속형으로 두고 분석한다.
#       19_continuous_main.R(주질문)과 같은 모형 구조. 보조로 고요산혈증(이분형) 결과, 극단값 절단, log 변환도 본다.
# 주의: 계획서의 분석은 범주형이며 이 분석은 보조 분석이다.
# 실행: Rscript --vanilla code/20_continuous_subq.R   (프로젝트 폴더에서, 약 2분)
# 결과: output/16_continuous_main/05_*, 06_*, SUMMARY_연속형_부질문.md

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")

out4 <- file.path("output", "16_continuous_main")
dir.create(out4, showWarnings = FALSE, recursive = TRUE)
lc_ci <- function(fit, w, tr = identity) {    # 계수의 가중합: 추정치, 95% CI, p
  b <- coef(fit); V <- vcov(fit); stopifnot(all(names(w) %in% names(b)))
  l <- setNames(rep(0, length(b)), names(b)); l[names(w)] <- w
  est <- sum(l * b); se <- sqrt(as.numeric(t(l) %*% V %*% l))
  c(est = tr(est), lo = tr(est - z975 * se), hi = tr(est + z975 * se), p = 2 * pnorm(-abs(est / se)))
}
add_x <- function(ds) {   # 노출 변수: 하루 잔수, 99백분위 절단, log2
  q99 <- as.numeric(coef(svyquantile(~drinks_wk, ds, 0.99, na.rm = TRUE)))
  v <- as.data.frame(ds$variables)          # data.table 참조 상태를 피하려고 일반 data.frame으로 바꿔 열을 추가한다
  v$drinks7  <- v$drinks_wk / 7
  v$drinks7w <- pmin(v$drinks_wk, q99) / 7
  v$logdrk   <- log2(v$drinks_wk + 1)
  ds$variables <- v
  ds
}
prep <- lapply(sdes_sex, add_x); sall <- add_x(sdes)
oc_list <- list(list(y = "uacid", fam = gaussian(), tr = identity, lab = "혈청 요산(mg/dL, 하루 1잔당 기울기)"),
                list(y = "hua",   fam = quasibinomial(), tr = exp, lab = "고요산혈증(하루 1잔당 OR)"))

# ==== 부질문 1: 홍조 이력 3군 =================================================
hist_rows <- function(fit, x, n, tr) {
  nm <- function(g) paste0(x, ":flush_hist", g)
  s0 <- lc_ci(fit, setNames(1, x), tr)
  s1 <- lc_ci(fit, setNames(c(1, 1), c(x, nm("홍조 소실"))), tr); s2 <- lc_ci(fit, setNames(c(1, 1), c(x, nm("지속 홍조"))), tr)
  d1 <- lc_ci(fit, setNames(1, nm("홍조 소실")), tr); d2 <- lc_ci(fit, setNames(1, nm("지속 홍조")), tr)
  d3 <- lc_ci(fit, setNames(c(1, -1), c(nm("홍조 소실"), nm("지속 홍조"))), tr)
  pj <- term_test(fit, paste0(x, ":flush_hist"))$p
  mk <- function(g, s, cl, d) data.table(group = g, slope = s[["est"]], slope_lo = s[["lo"]], slope_hi = s[["hi"]], slope_p = s[["p"]],
                                        contrast = cl, c_est = d[["est"]], c_lo = d[["lo"]], c_hi = d[["hi"]], c_p = d[["p"]])
  na4 <- c(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_)
  rbind(mk("비홍조", s0, "", na4), mk("홍조 소실", s1, "소실 vs 비홍조", d1), mk("지속 홍조", s2, "지속 vs 비홍조", d2),
        mk("", na4, "소실 vs 지속 (탐색)", d3))[, `:=`(n_model = n, p_joint_2df = pj)][]
}
sq1 <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(names(models4), function(mdl) rbindlist(lapply(oc_list, function(oc) {
  if (oc$y == "hua" && !mdl %in% c("Model 2", "Model 3")) return(NULL)
  r <- fit_svy(prep[[s]], mk_form(oc$y, "drinks7 * flush_hist", models4[[mdl]]), oc$fam)
  cbind(outcome = oc$lab, exposure = "잔/일(연속)", sex = s, model = mdl, hist_rows(r$fit, "drinks7", r$n, oc$tr))
}))))))
# 민감도(Model 2·3, 요산과 고요산혈증): 99백분위 절단, log 변환 — 극단적으로 많이 마시는 사람이 결과를 끌고 가는지 확인
sq1_sens <- rbindlist(lapply(sexes, function(s) rbindlist(lapply(c("Model 2", "Model 3"), function(mdl) rbindlist(lapply(oc_list, function(oc)
  rbindlist(lapply(list(c("drinks7w", "잔/일, 99백분위 절단"), c("logdrk", "log2(잔/주 + 1): 두 배당")), function(xv) {
    r <- fit_svy(prep[[s]], mk_form(oc$y, paste(xv[1], "* flush_hist"), models4[[mdl]]), oc$fam)
    cbind(outcome = oc$lab, exposure = xv[2], sex = s, model = mdl, hist_rows(r$fit, xv[1], r$n, oc$tr))
  }))))))))
# 보조: 신규 홍조 포함 4군(요산, Model 2)
sq1_4 <- rbindlist(lapply(sexes, function(s) {
  r <- fit_svy(prep[[s]], mk_form("uacid", "drinks7 * flush_grp", cov_base), gaussian()); f <- r$fit
  lv <- levels(r$data$flush_grp)
  rbindlist(lapply(lv, function(g) {
    w <- if (g == lv[1]) setNames(1, "drinks7") else setNames(c(1, 1), c("drinks7", paste0("drinks7:flush_grp", g)))
    sl <- lc_ci(f, w); d <- if (g == lv[1]) c(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_) else lc_ci(f, setNames(1, paste0("drinks7:flush_grp", g)))
    data.table(sex = s, model = "Model 2", group = g, n = sum(r$data$flush_grp == g), slope = sl[["est"]], slope_lo = sl[["lo"]], slope_hi = sl[["hi"]],
               diff_vs_nonflush = d[["est"]], diff_lo = d[["lo"]], diff_hi = d[["hi"]], diff_p = d[["p"]], p_joint_3df = term_test(f, "drinks7:flush_grp")$p, n_model = r$n)
  }))
}))
fwrite(round_dt(rbind(sq1, sq1_sens)), file.path(out4, "05_subq1_history_continuous.csv"), bom = TRUE)
fwrite(round_dt(sq1_4), file.path(out4, "05b_subq1_4groups_continuous.csv"), bom = TRUE)

# ==== 부질문 2: 성별 차이, 여성 폐경 ===========================================
sq2 <- rbindlist(lapply(names(models4), function(mdl) rbindlist(lapply(oc_list, function(oc) {
  if (oc$y == "hua" && !mdl %in% c("Model 2", "Model 3")) return(NULL)
  r3 <- fit_svy(sall, mk_form(oc$y, "drinks7 * flush_now * f_sex", models4[[mdl]]), oc$fam)
  out <- data.table(outcome = oc$lab, model = mdl, test = "음주량 × 현재 홍조 × 성별 (3원, 1 df)", n_model = r3$n, term_test(r3$fit, "drinks7:flush_now:f_sex"))
  for (m in levels(dat_of(sall)$flush_now)) {
    r2 <- fit_svy(dsub(sall, dat_of(sall)$flush_now == m), mk_form(oc$y, "drinks7 * f_sex", models4[[mdl]]), oc$fam)
    d <- lc_ci(r2$fit, setNames(1, "drinks7:f_sex여자"), oc$tr)
    out <- rbind(out, cbind(data.table(outcome = oc$lab, model = mdl, test = sprintf("음주량 × 성별 (%s 안에서, 1 df): 여자 − 남자 기울기 = %.3f (%.3f, %.3f)", m, d[["est"]], d[["lo"]], d[["hi"]]),
                                       n_model = r2$n), term_test(r2$fit, "drinks7:f_sex")))
  }
  out
}))))
dw <- dat_of(prep[["여자"]])
sq2_meno <- rbindlist(lapply(c("Model 2", "Model 3"), function(mdl) rbindlist(lapply(oc_list, function(oc) {
  rows <- rbindlist(lapply(levels(dw$meno), function(m) {
    r <- fit_svy(dsub(prep[["여자"]], !is.na(dw$meno) & dw$meno == m), mk_form(oc$y, "drinks7 * flush_now", models4[[mdl]]), oc$fam)
    s0 <- lc_ci(r$fit, setNames(1, "drinks7"), oc$tr); s1 <- lc_ci(r$fit, setNames(c(1, 1), c("drinks7", "drinks7:flush_now홍조")), oc$tr)
    d <- lc_ci(r$fit, setNames(1, "drinks7:flush_now홍조"), oc$tr)
    data.table(outcome = oc$lab, model = mdl, menopause = m, n_model = r$n, n_flush_high = sum(r$data$flush_now == "홍조" & r$data$drinks_wk >= 14),
               nonflush = s0[["est"]], nonflush_lo = s0[["lo"]], nonflush_hi = s0[["hi"]], flush = s1[["est"]], flush_lo = s1[["lo"]], flush_hi = s1[["hi"]],
               diff = d[["est"]], diff_lo = d[["lo"]], diff_hi = d[["hi"]], p_interaction = term_test(r$fit, "drinks7:flush_now")$p)
  }))
  r3 <- fit_svy(prep[["여자"]], mk_form(oc$y, "drinks7 * flush_now * meno", models4[[mdl]]), oc$fam)
  rbind(rows, data.table(outcome = oc$lab, model = mdl, menopause = "3원 상호작용(음주량 × 홍조 × 폐경, 1 df)", n_model = r3$n, n_flush_high = NA_integer_,
                         nonflush = NA_real_, nonflush_lo = NA_real_, nonflush_hi = NA_real_, flush = NA_real_, flush_lo = NA_real_, flush_hi = NA_real_,
                         diff = NA_real_, diff_lo = NA_real_, diff_hi = NA_real_, p_interaction = term_test(r3$fit, "drinks7:flush_now:meno")$p))
}))))
fwrite(round_dt(sq2), file.path(out4, "06_subq2_sex_continuous.csv"), bom = TRUE)
fwrite(round_dt(sq2_meno), file.path(out4, "06b_subq2_menopause_continuous.csv"), bom = TRUE)

# ==== 요약 메모 ================================================================
ci3 <- function(e, l, h, d = 3) ifelse(is.na(e), "", sprintf(paste0("%.", d, "f (%.", d, "f, %.", d, "f)"), e, l, h))
sq1_md <- function(x, d = 3) md_tab(x[, .(홍조군 = group, `기울기 (95% CI)` = ci3(slope, slope_lo, slope_hi, d), p = fmt_p(slope_p), 비교 = contrast,
                                           `차이 또는 비 (95% CI)` = ci3(c_est, c_lo, c_hi, d), `p ` = fmt_p(c_p))])
mm <- c("# 부질문의 연속형 분석 (보조 분석)", "",
        "- 노출 = 하루 잔수(주당 잔수 ÷ 7), 결과 = 혈청 요산(mg/dL) 또는 고요산혈증. 기울기 1단위 = 하루 1잔(주 7잔) 늘 때의 변화.",
        "- 요산 결과에서 '차이'는 기울기의 차(mg/dL per 잔/일), 고요산혈증 결과에서는 하루 1잔당 OR의 비. 공동 p = 홍조 이력 2개 상호작용항의 설계 기반 Wald F(2 df).",
        "- 여자 홍조 소실군은 고음주가 17명뿐이라 기울기 추정이 불안정하다.", "",
        "## 1. 부질문 1: 음주량 × 홍조 이력 (비홍조·소실·지속, 신규 제외)", "")
for (s in sexes) for (mdl in names(models4)) {
  x <- sq1[sex == s & model == mdl & outcome == oc_list[[1]]$lab]
  mm <- c(mm, sprintf("### %s · %s · 혈청 요산 (n = %s, 공동 p = %s)", s, mdl, format(x$n_model[1], big.mark = ","), fmt_p(x$p_joint_2df[1])), "", sq1_md(x))
  if (mdl %in% c("Model 2", "Model 3")) {
    y <- sq1[sex == s & model == mdl & outcome == oc_list[[2]]$lab]
    mm <- c(mm, sprintf("**%s · %s · 고요산혈증 OR(하루 1잔당)** (공동 p = %s)", s, mdl, fmt_p(y$p_joint_2df[1])), "", sq1_md(y, 2))
  }
}
mm <- c(mm, "### 민감도: 극단값 절단과 log 변환 (Model 2·3, 요산과 고요산혈증)", "")
for (s in sexes) for (mdl in c("Model 2", "Model 3")) for (oc in oc_list) for (ex in unique(sq1_sens$exposure)) {
  x <- sq1_sens[sex == s & model == mdl & outcome == oc$lab & exposure == ex]
  mm <- c(mm, sprintf("**%s · %s · %s · %s** (공동 p = %s)", s, mdl, oc$lab, ex, fmt_p(x$p_joint_2df[1])), "", sq1_md(x, if (oc$y == "hua") 2 else 3))
}
mm <- c(mm, "### 보조: 신규 홍조 포함 4군 (요산, Model 2)", "",
        md_tab(sq1_4[, .(성별 = sex, 홍조패턴 = group, n = format(n, big.mark = ","), `기울기 (95% CI)` = ci3(slope, slope_lo, slope_hi),
                         `비홍조와의 차이 (95% CI)` = ci3(diff_vs_nonflush, diff_lo, diff_hi), p = fmt_p(diff_p), `공동 p(3 df)` = fmt_p(p_joint_3df))]),
        "## 2. 부질문 2: 성별 차이", "",
        md_tab(sq2[, .(결과 = outcome, 모형 = model, 검정 = test, `모형 n` = format(n_model, big.mark = ","), F = sprintf("%.2f", F), p = fmt_p(p))]),
        "## 3. 부질문 2: 여성 폐경 전·후 (탐색)", "",
        md_tab(sq2_meno[, .(결과 = outcome, 모형 = model, 구분 = menopause, `모형 n` = format(n_model, big.mark = ","), `홍조·고음주(≥14잔) n` = ifelse(is.na(n_flush_high), "", as.character(n_flush_high)),
                            `비홍조 기울기` = ci3(nonflush, nonflush_lo, nonflush_hi), `홍조 기울기` = ci3(flush, flush_lo, flush_hi), `차이/비` = ci3(diff, diff_lo, diff_hi), `상호작용 p` = fmt_p(p_interaction))]))
con <- file(file.path(out4, "SUMMARY_연속형_부질문.md"), open = "w", encoding = "UTF-8"); writeLines(mm, con); close(con)
cat("\n[부질문 1: 요산, 공동 p]\n"); print(unique(sq1[outcome == oc_list[[1]]$lab, .(sex, model, n_model, p_joint = signif(p_joint_2df, 3))]))
cat("\n[부질문 2]\n"); print(sq2[, .(outcome, model, test = substr(test, 1, 40), p = signif(p, 3))])
cat("\n완료:", out4, "\n")
