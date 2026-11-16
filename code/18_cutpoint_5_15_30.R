# 18_cutpoint_5_15_30.R
# 목적: 선행연구 근거 절단점(5·15·30잔/주 = 5·15·30 g/일, Jeon 2022·Choi 2004)과 3군 축약(5·15잔)으로
#       주질문(음주 × 현재 홍조)과 부질문 1(음주 × 홍조 이력)을 다시 분석한다. 근거: output/15_cutpoint_exploration/04_절단점_근거_검토.md
# 주의: 계획서(3.5·14잔) 분석 결과를 본 뒤에 바꾼 절단점이므로 사후 분석이다. 보고할 때 계획서 기준 결과와 함께 싣고 변경 이유를 적는다.
# 실행: Rscript --vanilla code/18_cutpoint_5_15_30.R   (프로젝트 폴더에서, 약 2분)
# 결과: output/15_cutpoint_exploration/05_*.csv, SUMMARY_5_15_30.md

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")   # 자료·변수·설계 객체·함수(run_int, trend_table, contrast_test 등)를 그대로 가져온다

out3 <- file.path("output", "15_cutpoint_exploration")
dir.create(out3, showWarnings = FALSE, recursive = TRUE)
schemes <- list("5·15·30 (4군)" = list(br = c(5, 15, 30), labs = c("<5", "5-14.9", "15-29.9", ">=30")),
                "5·15 (3군)"    = list(br = c(5, 15),     labs = c("<5", "5-14.9", ">=15")))
cells <- list(); tests <- list(); trends <- list()
add_c <- function(x) cells[[length(cells) + 1]] <<- x
add_t <- function(x) tests[[length(tests) + 1]] <<- x
add_r <- function(x) trends[[length(trends) + 1]] <<- x
for (sc in names(schemes)) for (s in sexes) {
  ds <- sdes_sex[[s]]
  ds$variables$drink3   <- cut(ds$variables$drinks_wk, c(-Inf, schemes[[sc]]$br, Inf), right = FALSE, labels = schemes[[sc]]$labs)
  ds$variables$drink_ord <- as.numeric(ds$variables$drink3) - 1          # 추세 검정용 순서형
  lv <- schemes[[sc]]$labs[-1]
  for (mdl in names(models4)) {
    # 주질문: 음주 × 현재 홍조 (Crude, Model 1, 2, 3)
    a <- run_int(ds, "hua", "drink3", "flush_now", models4[[mdl]], quasibinomial(), TRUE)
    add_c(cbind(scheme = sc, sex = s, modifier_var = "현재 홍조", model = mdl, a$cells))
    add_t(cbind(scheme = sc, sex = s, modifier_var = "현재 홍조", model = mdl, test = sprintf("음주 × 현재 홍조 (공동, %d df)", length(lv)), a$test))
    add_r(cbind(scheme = sc, sex = s, modifier_var = "현재 홍조", model = mdl, trend_table(ds, "hua", "flush_now", models4[[mdl]], quasibinomial(), TRUE)))
    # 부질문 1: 음주 × 홍조 이력 (Model 2, 3)
    if (mdl %in% c("Model 2", "Model 3")) {
      b <- run_int(ds, "hua", "drink3", "flush_hist", models4[[mdl]], quasibinomial(), TRUE)
      f <- b$r$fit
      ct <- function(L, lab) cbind(scheme = sc, sex = s, modifier_var = "홍조 이력", model = mdl, test = lab, n_model = b$r$n, contrast_test(f, L))
      L_lost <- lapply(lv, function(e) setNames(1, paste0("drink3", e, ":flush_hist홍조 소실")))
      L_pers <- lapply(lv, function(e) setNames(1, paste0("drink3", e, ":flush_hist지속 홍조")))
      L_diff <- lapply(lv, function(e) setNames(c(1, -1), c(paste0("drink3", e, ":flush_hist홍조 소실"), paste0("drink3", e, ":flush_hist지속 홍조"))))
      add_c(cbind(scheme = sc, sex = s, modifier_var = "홍조 이력", model = mdl, b$cells))
      add_t(rbind(cbind(scheme = sc, sex = s, modifier_var = "홍조 이력", model = mdl, test = sprintf("음주 × 홍조 이력 (공동, %d df)", 2 * length(lv)), b$test),
                  ct(L_lost, sprintf("소실 vs 비홍조 (%d df)", length(lv))), ct(L_pers, sprintf("지속 vs 비홍조 (%d df)", length(lv))),
                  ct(L_diff, sprintf("소실 vs 지속 (%d df, 탐색)", length(lv)))))
      add_r(cbind(scheme = sc, sex = s, modifier_var = "홍조 이력", model = mdl, trend_table(ds, "hua", "flush_hist", models4[[mdl]], quasibinomial(), TRUE)))
    }
  }
}
cells <- rbindlist(cells); tests <- rbindlist(tests); trends <- rbindlist(trends)
cells[, unstable := n_event < 5]
fwrite(round_dt(cells),  file.path(out3, "05_cutpoint_5_15_30_cells.csv"), bom = TRUE)
fwrite(round_dt(tests),  file.path(out3, "05_cutpoint_5_15_30_tests.csv"), bom = TRUE)
fwrite(round_dt(trends), file.path(out3, "05_cutpoint_5_15_30_trend.csv"), bom = TRUE)

# ---- 요약 메모 ----
ci2 <- function(e, l, h) ifelse(is.na(e), "", ifelse(l < 0 | h < 0, sprintf("%.2f (%.2f, %.2f)", e, l, h), sprintf("%.2f (%.2f–%.2f)", e, l, h)))
strat_md <- function(x) {   # 홍조군별 층화 OR 표(기준: 각 군의 첫 수준)
  y <- x[exposure != levels(factor(exposure, unique(exposure)))[1]]
  md_tab(y[, .(홍조군 = modifier, `음주 수준` = exposure, `n (사례)` = sprintf("%s (%s)", format(n, big.mark = ","), n_event),
               `층화 OR (95% CI)` = ci2(strat_est, strat_lo, strat_hi), p = fmt_p(strat_p),
               `OR의 비 p` = fmt_p(p_product_term), `RERI (95% CI)` = ci2(reri, reri_lo, reri_hi), 불안정 = ifelse(unstable, "*", ""))])
}
mm <- c("# 선행연구 절단점(5·15·30잔/주, 5·15잔/주)으로 다시 본 결과 — 사후 분석", "",
        "- 절단점 근거: 04_절단점_근거_검토.md. 1잔 ≈ 7 g이므로 잔/주 = g/일.",
        "- 사후 분석이다. 계획서 기준(3.5·14잔) 결과를 함께 보고하고 변경 이유(단위 환산, 선행연구)를 밝힌다. 격자 탐색에서 28잔 부근에 신호가 있었으므로 ≥30 구간 결과는 탐색으로 해석한다.",
        "- 층화 OR = 같은 홍조군 안에서 기준(<5잔) 대비. 'OR의 비 p' = 그 수준에서 홍조군과 비홍조군의 OR이 같은지(상호작용항). * = 사례 5명 미만.", "")
for (sc in names(schemes)) {
  mm <- c(mm, sprintf("## %s", sc), "")
  for (s in sexes) {
    mm <- c(mm, sprintf("### %s · 주질문(현재 홍조)", s), "",
            md_tab(tests[scheme == sc & sex == s & modifier_var == "현재 홍조", .(모형 = model, 검정 = test, `모형 n` = format(n_model, big.mark = ","), F = sprintf("%.2f", F), p = fmt_p(p))]))
    for (mdl in c("Model 2", "Model 3")) mm <- c(mm, sprintf("**%s · %s**", s, mdl), "", strat_md(cells[scheme == sc & sex == s & modifier_var == "현재 홍조" & model == mdl]))
    tr <- trends[scheme == sc & sex == s & modifier_var == "현재 홍조" & model == "Model 2"]
    mm <- c(mm, sprintf("- P for trend(Model 2, 한 단계당 OR): %s; 추세 차이 p = %s", paste(tr[, sprintf("%s %s (p %s)", modifier, ci2(per_level_est, ci_low, ci_high), fmt_p(p_trend))], collapse = " / "), fmt_p(tr$p_trend_x_modifier[1])), "",
            sprintf("### %s · 부질문 1(홍조 이력, 신규 제외)", s), "",
            md_tab(tests[scheme == sc & sex == s & modifier_var == "홍조 이력", .(모형 = model, 검정 = test, `모형 n` = format(n_model, big.mark = ","), F = sprintf("%.2f", F), p = fmt_p(p))]))
    for (mdl in c("Model 2", "Model 3")) mm <- c(mm, sprintf("**%s · %s**", s, mdl), "", strat_md(cells[scheme == sc & sex == s & modifier_var == "홍조 이력" & model == mdl]))
  }
}
con <- file(file.path(out3, "SUMMARY_5_15_30.md"), open = "w", encoding = "UTF-8"); writeLines(mm, con); close(con)
cat("\n[상호작용 검정 p]\n"); print(tests[, .(scheme, sex, modifier_var, model, test, p = signif(p, 3))], nrows = 200)
cat("\n완료:", out3, "\n")
