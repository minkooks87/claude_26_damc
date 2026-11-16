# 22_main_5_15_precise.R
# 주제: 음주 관련 안면홍조 이력에 따른 음주량–고요산혈증 연관성의 차이(성별 효과수정), 국민건강영양조사 2019–2021
# 목적: 음주 수준 절단점을 주 5잔·15잔(저음주 5잔 미만, 중음주 5잔 이상 15잔 미만, 고음주 15잔 이상)으로 놓고
#       주질문과 부질문 1·2를 정밀하게 분석한다.
#   - 주질문  : 남녀 각각에서 음주 수준 × 현재 홍조
#   - 부질문 1: 음주 수준 × 홍조 이력(비홍조·홍조 소실·지속 홍조). 계획서의 우선 대비는 '소실 vs 비홍조'
#   - 부질문 2: 음주 수준 × 현재 홍조 × 성별(3원), 여성 폐경 전·후(탐색)
# 이전 분석(code/13 계획서 절단점, code/21 절단점만 바꾼 재분석)에 더한 것:
#   (1) 신뢰구간과 p값을 설계 기반 잔차 자유도의 t 분포로 통일  (2) Wald 검정에 Rao–Scott 작업 우도비 검정(LRT) 병기
#   (3) 부질문 1: 대비별 OR의 비, 추세 차이, Holm 보정, 칸별 사례 수 기준, 현재 비홍조자 제한 모형, 2문항 분류(과거 또는 현재 홍조), 4군
#   (4) 부질문 2: 성별 층화 모형과 정확히 대응하는 3원 검정(공변량 효과도 성별로 다르게), 추세·연속형 3원, 범주 안 실제 잔수
#   (5) 검출 가능한 최소 효과 크기  (6) 민감도 분석  (7) 공식 통계 대조  (8) 모형 수렴·경고 기록
# 변수 정의·결측 처리·대상 선정(inc 표시, 행 삭제 없음)·설계(wt_itvex × 1/3, kstrata, psu)는 code/13_v3_main_analysis.R을 그대로 쓴다.
#   음주 빈도·1회량 BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162 / 인쇄 p.153-154, 홍조 BD9_1·BD9_2: 8기 PDF p.163-164 / 인쇄 p.155-156
#   요산 HE_Uacid: 8기 PDF p.200 / 인쇄 p.192, 폐경 LW_ms: 8기 PDF p.190 / 인쇄 p.182, 통합가중치: 8기 PDF p.36 / 인쇄 p.28
# 주의: 계획서 절단점(3.5·14잔)의 결과를 본 뒤에 바꾼 절단점이므로 사후 분석이다(절단점 근거: output/15_cutpoint_exploration/04_절단점_근거_검토.md).
# 실행: Rscript --vanilla code/22_main_5_15_precise.R   (프로젝트 폴더에서, 약 5분)
# 결과: output/18_main_5_15/

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")   # 자료·변수·대상 표시·설계 객체(des, sdes)·함수(fit_svy, term_test, contrast_test, mk_form, table1_group, md_tab, kv_table 등)

out <- file.path("output", "18_main_5_15")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
options(warn = 1)
CUT  <- c(5, 15)
lv3  <- c("저음주", "중음주", "고음주")
rng3 <- c("주 5잔 미만", "주 5잔 이상 15잔 미만", "주 15잔 이상")
EST_MIN_EVENTS <- 10      # 칸별 고요산혈증 사례가 이보다 적으면 그 대비는 기술통계로만 제시(Peduzzi 1996의 변수당 사례 10건 기준을 준용)
EST_MIN_N      <- 30      # 연속형 결과(요산)에서의 칸별 최소 인원
z80 <- qnorm(0.975) + qnorm(0.80)   # 양측 5%, 검정력 80%에서 검출 가능한 최소 효과 = exp(z80 × SE) (연속형은 z80 × SE)
ocs <- list(oc_bin, oc_con)         # 고요산혈증(OR), 혈청 요산(β, mg/dL)
ref13 <- list(int4 = copy(int4), hist_tests = copy(hist_tests))   # 계획서 절단점 결과(나란히 비교용)

# ==== 1. 변수: 절단점과 보조 변수만 더한다 (행·가중치·층·집락은 그대로) ==========
add_vars <- function(v) {
  cut3n <- function(x) cut(x, c(-Inf, CUT, Inf), right = FALSE, labels = lv3)
  v$drink3    <- cut3n(v$drinks_wk)                     # 주 분석 노출
  v$drink3_b  <- cut3n(v$drinks_wk_b)                   # 민감도 S3a(열린 범주 대표값을 작게)
  v$drink3_c  <- cut3n(v$drinks_wk_c)                   # 민감도 S3b(열린 범주 대표값을 크게)
  v$drink_ord <- as.numeric(v$drink3) - 1               # 추세 검정용 순서형(0·1·2)
  v$drink2    <- factor(ifelse(v$drinks_wk >= CUT[1], "주 5잔 이상", "주 5잔 미만"), c("주 5잔 미만", "주 5잔 이상"))
  v$drinks7   <- v$drinks_wk / 7                        # 하루 잔수(연속형)
  v$logdrk    <- log2(v$drinks_wk + 1)
  # 과거 홍조(BD9_1: 1 예, 2 아니오)와 2문항 분류(과거 또는 현재 홍조 = 홍조 체질; Yokoyama 2003, Brooks 2009) — 문항: 8기 PDF p.163-164
  v$past_flush <- factor(ifelse(v$bd9_1 == 1, "과거 홍조 있음", ifelse(v$bd9_1 == 2, "과거 홍조 없음", NA)), c("과거 홍조 없음", "과거 홍조 있음"))
  v$flush_ever <- factor(ifelse(is.na(v$flush_grp), NA, ifelse(v$flush_grp == "비홍조", "홍조 없음", "과거 또는 현재 홍조")), c("홍조 없음", "과거 또는 현재 홍조"))
  v$lost   <- as.numeric(v$flush_hist == "홍조 소실")   # 이력 대비의 우도비 검정용 지시변수
  v$pers   <- as.numeric(v$flush_hist == "지속 홍조")
  v$ever_h <- v$lost + v$pers
  v$age10  <- cut(v$age, c(-Inf, 30, 40, 50, 60, 70, Inf), right = FALSE, labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70+"))
  v$inc_no2019 <- as.integer(v$inc == 1L & v$year != 2019)   # 조사연도를 하나씩 뺀 민감도 분석용 대상 표시(행은 지우지 않는다)
  v$inc_no2021 <- as.integer(v$inc == 1L & v$year != 2021)
  v
}
des5 <- des; des5$variables <- add_vars(des$variables)
sdes5 <- subset(des5, inc == 1)
d5  <- dat_of(sdes5); dd5 <- dat_of(des5)
stopifnot(nrow(d5) == nrow(d_sub), nrow(dd5) == n_raw)
D <- lapply(setNames(sexes, sexes), function(s) dsub(sdes5, d5$f_sex == s))

# ==== 2. 공용 도구 ===========================================================
warn_log <- data.table(where = character(0), message = character(0))
diag_log <- list()
logw <- function(expr, where) withCallingHandlers(expr, warning = function(w) {
  warn_log <<- rbind(warn_log, data.table(where = where, message = conditionMessage(w))); invokeRestart("muffleWarning") })
fit5 <- function(design, form, fam, where = "") {          # fit_svy(완전 응답자 subset 후 적합) + 경고·수렴 기록
  ftxt <- paste(deparse(form, width.cutoff = 500L), collapse = " ")
  r <- logw(fit_svy(design, form, fam), paste(where, "|", ftxt))
  b <- coef(r$fit); b0 <- b[names(b) != "(Intercept)"]
  diag_log[[length(diag_log) + 1L]] <<- data.table(where = where, formula = ftxt, n = r$n, n_coef = length(b), df_resid = r$fit$df.residual,
                                                   converged = isTRUE(r$fit$converged), n_na_coef = sum(is.na(b)), max_abs_coef = max(abs(b0), na.rm = TRUE))
  r
}
# 계수의 선형결합: 추정치, 95% CI, p (t 분포, 자유도 = 설계 기반 잔차 자유도)
lin_t <- function(fit, l, tr = identity) {
  if (!length(l)) return(c(est = tr(0), lo = tr(0), hi = tr(0), p = NA_real_, se = 0))
  b <- coef(fit); V <- vcov(fit); df <- fit$df.residual
  stopifnot(all(names(l) %in% names(b)), !anyNA(b[names(l)]))
  w <- setNames(rep(0, length(b)), names(b)); w[names(l)] <- l
  est <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w)); tq <- qt(0.975, df)
  c(est = tr(est), lo = tr(est - tq * se), hi = tr(est + tq * se), p = 2 * pt(-abs(est / se), df), se = se)
}
ones <- function(nm) setNames(rep(1, length(nm)), nm)
pick <- function(fit, vars) { nm <- names(coef(fit)); nm[Reduce(`&`, lapply(vars, function(v) grepl(v, nm, fixed = TRUE)))] }
term_label <- function(fit, vars) { tl <- attr(terms(fit), "term.labels"); tl[Reduce(`&`, lapply(vars, function(v) grepl(v, tl, fixed = TRUE)))] }
wald_names <- function(fit, nm) { stopifnot(length(nm) > 0); contrast_test(fit, lapply(nm, function(x) setNames(1, x))) }
# Rao–Scott 작업 우도비 검정: 같은 설계·같은 대상에 다시 적합한 모형에서 해당 항을 뺀 모형과 비교한다
refit <- function(r, form = formula(r$fit)) do.call(svyglm, list(formula = form, design = r$design, family = family(r$fit)))
lrt_p <- function(r, label, form = formula(r$fit)) logw(tryCatch(
  as.numeric(regTermTest(refit(r, form), as.formula(paste0("~", label)), method = "LRT")$p), error = function(e) NA_real_), paste("LRT", label))

# 결합효과·층화효과·수정변수 효과·OR의 비·RERI 표 (Knol & VanderWeele 2012 형식; code/13의 joint_table과 추정치는 같고 구간만 t 분포)
cells_t <- function(r, expo, mod, or = TRUE) {
  fit <- r$fit; dat <- r$data; b <- coef(fit); V <- vcov(fit); df <- fit$df.residual; tq <- qt(0.975, df)
  tr <- if (or) exp else identity
  e_lv <- levels(dat[[expo]]); m_lv <- levels(dat[[mod]])
  cn <- function(e, m) c(if (e != e_lv[1]) paste0(expo, e), if (m != m_lv[1]) paste0(mod, m), if (e != e_lv[1] && m != m_lv[1]) paste0(expo, e, ":", mod, m))
  one <- function(nm) lin_t(fit, ones(nm), tr)
  rbindlist(lapply(m_lv, function(m) rbindlist(lapply(e_lv, function(e) {
    cell <- dat[[expo]] == e & dat[[mod]] == m
    pv <- tryCatch({ est <- svymean(as.formula(paste0("~", r$y)), dsub(r$design, cell)); c(coef(est)[1], SE(est)[1]) }, error = function(err) c(NA_real_, NA_real_))
    j <- one(cn(e, m)); s <- one(setdiff(cn(e, m), cn(e_lv[1], m))); f <- one(setdiff(cn(e, m), cn(e, m_lv[1])))
    is_int <- e != e_lv[1] && m != m_lv[1]
    ri <- if (is_int) one(paste0(expo, e, ":", mod, m)) else c(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, se = NA_real_)
    rr <- rep(NA_real_, 4)
    if (is_int && or) {                        # RERI = OR11 − OR10 − OR01 + 1, 델타법
      idx <- c(paste0(expo, e), paste0(mod, m), paste0(expo, e, ":", mod, m))
      o11 <- exp(sum(b[idx])); o10 <- exp(b[idx[1]]); o01 <- exp(b[idx[2]])
      rer <- o11 - o10 - o01 + 1; g <- c(o11 - o10, o11 - o01, o11)
      se <- sqrt(as.numeric(t(g) %*% V[idx, idx] %*% g))
      rr <- c(rer, rer - tq * se, rer + tq * se, 2 * pt(-abs(rer / se), df))
    }
    data.table(modifier = m, exposure = e, n = sum(cell), n_event = if (r$binary) as.integer(sum(dat[[r$y]][cell])) else NA_integer_,
               wt_mean = unname(pv[1]), wt_se = unname(pv[2]),
               joint_est = j[["est"]], joint_lo = j[["lo"]], joint_hi = j[["hi"]], joint_p = j[["p"]],
               strat_est = s[["est"]], strat_lo = s[["lo"]], strat_hi = s[["hi"]], strat_p = s[["p"]],
               mod_est = f[["est"]], mod_lo = f[["lo"]], mod_hi = f[["hi"]], mod_p = f[["p"]],
               ratio_est = ri[["est"]], ratio_lo = ri[["lo"]], ratio_hi = ri[["hi"]], ratio_p = ri[["p"]],
               ratio_mde80 = if (!is_int) NA_real_ else if (or) exp(z80 * ri[["se"]]) else z80 * ri[["se"]],
               reri = rr[1], reri_lo = rr[2], reri_hi = rr[3], reri_p = rr[4])
  }))))
}
# 노출 × 수정변수 모형: 칸 표 + 공동 Wald F + Rao–Scott LRT
run2 <- function(design, y, expo, mod, covs, fam, or, where, lrt = TRUE) {
  r <- fit5(design, mk_form(y, paste(expo, "*", mod), covs), fam, where)
  tst <- cbind(n_model = r$n, term_test(r$fit, paste0(expo, ":", mod)), p_lrt = if (lrt) lrt_p(r, paste0(expo, ":", mod)) else NA_real_)
  list(r = r, cells = cbind(n_model = r$n, cells_t(r, expo, mod, or)), test = tst)
}
# 추세(음주 수준 순서형 0·1·2) × 수정변수: 군별 한 단계당 효과와 기준군 대비 차이
trend_t <- function(design, y, mod, covs, fam, or, where) {
  r <- fit5(design, mk_form(y, paste("drink_ord *", mod), covs), fam, where); fit <- r$fit
  dat <- r$data; m_lv <- levels(dat[[mod]]); tr <- if (or) exp else identity
  tab <- rbindlist(lapply(m_lv, function(m) {
    nm <- if (m == m_lv[1]) "drink_ord" else c("drink_ord", paste0("drink_ord:", mod, m))
    s  <- lin_t(fit, ones(nm), tr)
    dr <- if (m == m_lv[1]) c(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_) else lin_t(fit, ones(paste0("drink_ord:", mod, m)), tr)
    data.table(modifier = m, n = sum(dat[[mod]] == m), per_level_est = s[["est"]], ci_low = s[["lo"]], ci_high = s[["hi"]], p_trend = s[["p"]],
               diff_vs_ref_est = dr[["est"]], diff_vs_ref_lo = dr[["lo"]], diff_vs_ref_hi = dr[["hi"]], p_diff_vs_ref = dr[["p"]])
  }))
  tab[, `:=`(n_model = r$n, p_trend_x_modifier = term_test(fit, paste0("drink_ord:", mod))$p)]
  list(tab = tab[], r = r)
}
# 상호작용항이 없는 모형의 주효과
main_eff <- function(design, y, mod, covs, fam, or, where) {
  r <- fit5(design, mk_form(y, c("drink3", mod), covs), fam, where); tr <- if (or) exp else identity
  nm <- grep(paste0("^(drink3|", mod, ")"), names(coef(r$fit)), value = TRUE)
  rbindlist(lapply(nm, function(x) { s <- lin_t(r$fit, ones(x), tr); data.table(term = x, n_model = r$n, est = s[["est"]], lo = s[["lo"]], hi = s[["hi"]], p = s[["p"]]) }))
}
# 홍조 이력 3군 모형: 공동 검정, 군별 대비(2 df Wald, 수준별 OR의 비), 고음주의 군별 층화 효과
hist_core <- function(design, y, expo, covs, fam, or, where) {
  r <- fit5(design, mk_form(y, paste(expo, "* flush_hist"), covs), fam, where); f <- r$fit; tr <- if (or) exp else identity
  e_lv <- levels(r$data[[expo]])[-1]; stopifnot(length(e_lv) == 2)
  nmh <- function(e, g) paste0(expo, e, ":flush_hist", g)
  L <- list("소실 vs 비홍조"     = lapply(e_lv, function(e) setNames(1, nmh(e, "홍조 소실"))),
            "지속 vs 비홍조"     = lapply(e_lv, function(e) setNames(1, nmh(e, "지속 홍조"))),
            "소실 vs 지속(탐색)" = lapply(e_lv, function(e) setNames(c(1, -1), c(nmh(e, "홍조 소실"), nmh(e, "지속 홍조")))))
  con <- rbindlist(lapply(names(L), function(cn) {
    w <- contrast_test(f, L[[cn]]); per <- lapply(L[[cn]], function(l) lin_t(f, l, tr))
    data.table(contrast = cn, df = w$df, ddf = w$ddf, F = w$F, p_wald = w$p,
               mid_est = per[[1]][["est"]], mid_lo = per[[1]][["lo"]], mid_hi = per[[1]][["hi"]], mid_p = per[[1]][["p"]],
               high_est = per[[2]][["est"]], high_lo = per[[2]][["lo"]], high_hi = per[[2]][["hi"]], high_p = per[[2]][["p"]],
               high_mde80 = if (or) exp(z80 * per[[2]][["se"]]) else z80 * per[[2]][["se"]])
  }))
  sh <- rbindlist(lapply(levels(r$data$flush_hist), function(g) {
    nm <- c(paste0(expo, e_lv[2]), if (g != "비홍조") nmh(e_lv[2], g)); x <- lin_t(f, ones(nm), tr)
    k <- r$data$flush_hist == g & r$data[[expo]] == e_lv[2]
    data.table(group = g, n = sum(k), n_event = if (r$binary) as.integer(sum(r$data[[r$y]][k])) else NA_integer_, est = x[["est"]], lo = x[["lo"]], hi = x[["hi"]], p = x[["p"]])
  }))
  list(r = r, omni = term_test(f, paste0(expo, ":flush_hist")), con = con, strat_high = sh)
}
# 이력 대비의 Rao–Scott LRT: 지시변수로 같은 모형을 다시 쓰고(이탈도로 동일성 확인) 해당 항을 뺀다
hist_lrt <- function(r, covs) {
  res <- c("공동(4 df)" = NA_real_, "소실 vs 비홍조" = NA_real_, "지속 vs 비홍조" = NA_real_, "소실 vs 지속(탐색)" = NA_real_)
  y <- r$y
  f_a <- mk_form(y, c("drink3", "lost", "pers", "drink3:lost", "drink3:pers"), covs)       # drink3 * flush_hist 와 같은 모형
  f_b <- mk_form(y, c("drink3", "ever_h", "pers", "drink3:ever_h", "drink3:pers"), covs)   # 여기서 drink3:pers = 지속 − 소실
  logw(tryCatch({
    ma <- refit(r, f_a); mb <- refit(r, f_b)
    stopifnot(isTRUE(all.equal(deviance(ma), deviance(r$fit), tolerance = 1e-6)), isTRUE(all.equal(deviance(mb), deviance(r$fit), tolerance = 1e-6)))
    res[["소실 vs 비홍조"]]     <- as.numeric(regTermTest(ma, ~drink3:lost, method = "LRT")$p)
    res[["지속 vs 비홍조"]]     <- as.numeric(regTermTest(ma, ~drink3:pers, method = "LRT")$p)
    res[["소실 vs 지속(탐색)"]] <- as.numeric(regTermTest(mb, ~drink3:pers, method = "LRT")$p)
    res[["공동(4 df)"]]         <- as.numeric(regTermTest(refit(r), ~drink3:flush_hist, method = "LRT")$p)
  }, error = function(e) NULL), "LRT 홍조 이력 대비")
  res
}
res <- list()
add <- function(nm, x) res[[nm]][[length(res[[nm]]) + 1L]] <<- x

# ==== 3. 기술통계 ===========================================================
# 3-1. 음주 수준 분포(성별)
dist <- rbindlist(lapply(sexes, function(s) {
  dat <- dat_of(D[[s]]); est <- svymean(~drink3, D[[s]])
  by_lv <- function(v) sapply(lv3, function(e) unname(coef(svymean(as.formula(paste0("~", v)), dsub(D[[s]], dat$drink3 == e)))[1]))
  data.table(sex = s, drink = lv3, range = rng3, n = as.integer(table(dat$drink3)[lv3]), wt_pct = 100 * unname(coef(est)[paste0("drink3", lv3)]),
             wt_pct_se = 100 * unname(SE(est)[paste0("drink3", lv3)]), n_hua = as.integer(tapply(dat$hua, dat$drink3, sum)[lv3]),
             hua_pct_wt = 100 * by_lv("hua"), uacid_mean_wt = by_lv("uacid"), drinks_wk_mean_wt = by_lv("drinks_wk"))
}))
# 3-2. 칸별(성별 × 홍조 × 음주 수준) 비가중 n·사례 수·가중 유병률·가중 평균 요산·범주 안 실제 잔수
cell_desc <- function(gvar) rbindlist(lapply(sexes, function(s) {
  dat <- dat_of(D[[s]])
  rbindlist(lapply(levels(dat[[gvar]]), function(g) rbindlist(lapply(lv3, function(e) {
    k <- !is.na(dat[[gvar]]) & dat[[gvar]] == g & dat$drink3 == e; ds <- dsub(D[[s]], k)
    mn <- function(v) { m <- svymean(as.formula(paste0("~", v)), ds); c(unname(coef(m)[1]), unname(SE(m)[1])) }
    hp <- mn("hua"); ua <- mn("uacid"); dk <- mn("drinks_wk")
    md <- tryCatch(as.numeric(coef(svyquantile(~drinks_wk, ds, 0.5)))[1], error = function(err) NA_real_)
    data.table(sex = s, group_var = gvar, group = g, drink = e, n = sum(k), n_hua = as.integer(sum(dat$hua[k])), hua_pct_wt = 100 * hp[1], hua_se_wt = 100 * hp[2],
               uacid_mean_wt = ua[1], uacid_se_wt = ua[2], drinks_wk_mean_wt = dk[1], drinks_wk_median_wt = md)
  }))))
}))
cellsD <- rbind(cell_desc("flush_now"), cell_desc("flush_grp"), cell_desc("flush_ever"))
# 3-3. Table 1: 비가중 n + 가중 추정치 (성별 × 음주 수준, 성별 × 현재 홍조, 성별 × 홍조 이력)
t1_vars_d <- c("age", "f_year", "f_edu", "f_incm", "f_smk", "f_pa", "he_bmi", "f_htn", "f_dm", "egfr", "drinks_wk", "binge3", "flush_now", "flush_grp", "uacid", "f_hua", "meno")
t1_drink <- rbindlist(lapply(sexes, function(s) cbind(sex = s, table1_group(D[[s]], "drink3", t1_vars_d))))
t1_now   <- rbindlist(lapply(sexes, function(s) cbind(sex = s, table1_group(D[[s]], "flush_now", t1_vars))))
t1_hist  <- rbindlist(lapply(sexes, function(s) cbind(sex = s, table1_group(dsub(D[[s]], !is.na(dat_of(D[[s]])$flush_hist)), "flush_hist", t1_vars))))

# ==== 4. 주질문: 음주 수준 × 현재 홍조 (남녀 각각, Crude·Model 1–3, 고요산혈증 OR과 요산 β) ====
for (s in sexes) for (mdl in names(models4)) for (oc in ocs) {
  covs <- models4[[mdl]]; wh <- paste("주질문", s, mdl)
  a <- run2(D[[s]], oc$y, "drink3", "flush_now", covs, oc$fam, oc$or, wh)
  add("main_cells", cbind(outcome = oc$lab, sex = s, model = mdl, a$cells))
  add("main_tests", cbind(outcome = oc$lab, sex = s, model = mdl, test = "음주 수준 × 현재 홍조 (공동, 2 df)", a$test))
  add("main_trend", cbind(outcome = oc$lab, sex = s, model = mdl, trend_t(D[[s]], oc$y, "flush_now", covs, oc$fam, oc$or, paste(wh, "추세"))$tab))
  add("main_effects", cbind(outcome = oc$lab, sex = s, model = mdl, main_eff(D[[s]], oc$y, "flush_now", covs, oc$fam, oc$or, paste(wh, "주효과"))))
}

# ==== 5. 부질문 1: 음주 수준 × 홍조 이력 ======================================
# 5-1. 계획서의 3군 모형(비홍조·소실·지속, 신규 제외): 공동 4 df → 우선 대비(소실 vs 비홍조) → 나머지 대비
tr_con <- list("소실 vs 비홍조" = c("drink_ord:flush_hist홍조 소실" = 1), "지속 vs 비홍조" = c("drink_ord:flush_hist지속 홍조" = 1),
               "소실 vs 지속(탐색)" = c("drink_ord:flush_hist홍조 소실" = 1, "drink_ord:flush_hist지속 홍조" = -1))
grp_of <- list("소실 vs 비홍조" = c("비홍조", "홍조 소실"), "지속 vs 비홍조" = c("비홍조", "지속 홍조"), "소실 vs 지속(탐색)" = c("홍조 소실", "지속 홍조"))
for (s in sexes) for (mdl in names(models4)) for (oc in ocs) {
  covs <- models4[[mdl]]; wh <- paste("부질문1", s, mdl); tr <- if (oc$or) exp else identity
  h  <- hist_core(D[[s]], oc$y, "drink3", covs, oc$fam, oc$or, wh)
  cl <- cbind(n_model = h$r$n, cells_t(h$r, "drink3", "flush_hist", oc$or))
  lr <- if (mdl %in% c("Model 2", "Model 3")) hist_lrt(h$r, covs) else c("공동(4 df)" = NA_real_, "소실 vs 비홍조" = NA_real_, "지속 vs 비홍조" = NA_real_, "소실 vs 지속(탐색)" = NA_real_)
  tt <- trend_t(D[[s]], oc$y, "flush_hist", covs, oc$fam, oc$or, paste(wh, "추세"))
  add("hist_cells", cbind(outcome = oc$lab, sex = s, model = mdl, cl))
  add("hist_tests", cbind(outcome = oc$lab, sex = s, model = mdl, test = "음주 수준 × 홍조 이력 (공동, 4 df)", n_model = h$r$n, h$omni, p_lrt = lr[["공동(4 df)"]]))
  add("hist_trend", cbind(outcome = oc$lab, sex = s, model = mdl, tt$tab))
  for (cn in h$con$contrast) {
    tx <- lin_t(tt$r$fit, tr_con[[cn]], tr); g2 <- cl[modifier %in% grp_of[[cn]]]
    add("hist_contrasts", cbind(outcome = oc$lab, sex = s, model = mdl, n_model = h$r$n, h$con[contrast == cn], p_lrt = lr[[cn]],
                                trend_est = tx[["est"]], trend_lo = tx[["lo"]], trend_hi = tx[["hi"]], p_trend_diff = tx[["p"]],
                                min_cell_n = min(g2$n), min_cell_events = if (oc$or) min(g2$n_event) else NA_integer_))
  }
}
# 5-2. 현재 비홍조자만: 음주 수준 × 과거 홍조 (소실 vs 비홍조와 같은 질문, 공변량 효과를 현재 비홍조자 안에서만 추정)
for (s in sexes) for (mdl in c("Model 2", "Model 3")) for (oc in ocs) {
  ds <- dsub(D[[s]], dat_of(D[[s]])$flush_now == "비홍조")
  a <- run2(ds, oc$y, "drink3", "past_flush", models4[[mdl]], oc$fam, oc$or, paste("부질문1 현재 비홍조자", s, mdl))
  add("nonflush_cells", cbind(outcome = oc$lab, sex = s, model = mdl, a$cells))
  add("nonflush_tests", cbind(outcome = oc$lab, sex = s, model = mdl, test = "현재 비홍조자: 음주 수준 × 과거 홍조 (2 df)", a$test))
}
# 5-3. 2문항 분류(과거 또는 현재 홍조 vs 둘 다 없음)를 효과수정변수로: 전체 대상(신규 포함)
for (s in sexes) for (mdl in names(models4)) for (oc in ocs) {
  covs <- models4[[mdl]]; wh <- paste("부질문1 2문항 분류", s, mdl)
  a <- run2(D[[s]], oc$y, "drink3", "flush_ever", covs, oc$fam, oc$or, wh)
  add("ever_cells", cbind(outcome = oc$lab, sex = s, model = mdl, a$cells))
  add("ever_tests", cbind(outcome = oc$lab, sex = s, model = mdl, test = "음주 수준 × 홍조 체질(과거 또는 현재) (2 df)", a$test))
  add("ever_trend", cbind(outcome = oc$lab, sex = s, model = mdl, trend_t(D[[s]], oc$y, "flush_ever", covs, oc$fam, oc$or, paste(wh, "추세"))$tab))
}
# 5-4. 보조: 신규 홍조를 포함한 4군
for (s in sexes) for (mdl in c("Model 2", "Model 3")) {
  a <- run2(D[[s]], "hua", "drink3", "flush_grp", models4[[mdl]], quasibinomial(), TRUE, paste("부질문1 4군", s, mdl))
  add("grp4_cells", cbind(outcome = oc_bin$lab, sex = s, model = mdl, p_wald_6df = a$test$p, p_lrt_6df = a$test$p_lrt, a$cells))
}

# ==== 6. 부질문 2: 성별 차이 =================================================
# 3원 상호작용의 두 구현: (가) 공변량 효과도 성별마다 다르게 = 남녀 층화 모형의 상호작용 계수 차이를 그대로 검정(주 구현)
#                         (나) 공변량 효과는 남녀 공통 = 계획서 문구 그대로(민감도)
sex_core <- function(design, y, core, covs, fam, sexcov, where) {
  form <- if (sexcov) as.formula(paste(y, "~ f_sex * (", paste(c(core, covs), collapse = " + "), ")")) else mk_form(y, paste(core, "* f_sex"), covs)
  fit5(design, form, fam, where)
}
cov_lab <- c(`TRUE` = "성별마다 다름(층화 모형과 대응)", `FALSE` = "남녀 공통(계획서 문구)")
for (mdl in names(models4)) for (oc in ocs) {
  covs <- models4[[mdl]]; tr <- if (oc$or) exp else identity
  for (sc in c(TRUE, FALSE)) {
    r3 <- sex_core(sdes5, oc$y, "drink3 * flush_now", covs, oc$fam, sc, paste("부질문2 3원", mdl))
    nm <- pick(r3$fit, c("f_sex", "drink3", "flush_now")); stopifnot(length(nm) == 2); w <- wald_names(r3$fit, nm)
    add("sex_tests", data.table(outcome = oc$lab, model = mdl, covariates = cov_lab[[as.character(sc)]], test = "음주 수준 × 현재 홍조 × 성별 (3원, 2 df)",
                                n_model = r3$n, df = w$df, ddf = w$ddf, F = w$F, p = w$p,
                                p_lrt = if (sc && mdl %in% c("Model 2", "Model 3")) lrt_p(r3, term_label(r3$fit, c("f_sex", "drink3", "flush_now"))) else NA_real_))
    add("sex_terms", rbindlist(lapply(nm, function(x) { e <- lin_t(r3$fit, ones(x), tr)
      data.table(outcome = oc$lab, model = mdl, covariates = cov_lab[[as.character(sc)]], term = x, n_model = r3$n, est = e[["est"]], lo = e[["lo"]], hi = e[["hi"]], p = e[["p"]],
                 mde80 = if (oc$or) exp(z80 * e[["se"]]) else z80 * e[["se"]]) })))
  }
  for (m in levels(d5$flush_now)) {          # 홍조군 안에서 음주 수준 × 성별
    r2 <- sex_core(dsub(sdes5, d5$flush_now == m), oc$y, "drink3", covs, oc$fam, TRUE, paste("부질문2 홍조군 안", m, mdl))
    nm <- pick(r2$fit, c("f_sex", "drink3")); stopifnot(length(nm) == 2); w <- wald_names(r2$fit, nm)
    add("sex_tests", data.table(outcome = oc$lab, model = mdl, covariates = cov_lab[["TRUE"]], test = paste0("음주 수준 × 성별 (", m, " 안에서, 2 df)"),
                                n_model = r2$n, df = w$df, ddf = w$ddf, F = w$F, p = w$p, p_lrt = NA_real_))
  }
  # 같은 범주라도 남녀의 실제 잔수가 다르므로 추세(1 df)와 연속형(1 df) 3원 상호작용을 함께 본다
  for (xv in list(c("drink_ord", "추세(음주 수준 한 단계당)"), c("drinks7", "연속형(하루 1잔당)"), c("logdrk", "연속형(log2, 잔수 두 배당)"))) {
    r1 <- sex_core(sdes5, oc$y, paste(xv[1], "* flush_now"), covs, oc$fam, TRUE, paste("부질문2 용량 3원", xv[1], mdl))
    nm <- pick(r1$fit, c("f_sex", xv[1], "flush_now")); stopifnot(length(nm) == 1); e <- lin_t(r1$fit, ones(nm), tr)
    add("sex_dose", data.table(outcome = oc$lab, model = mdl, dose_form = xv[2], n_model = r1$n, est = e[["est"]], lo = e[["lo"]], hi = e[["hi"]], p = e[["p"]]))
  }
}
# 여성 폐경 전·후(탐색): 층별 음주 × 현재 홍조, 3원 상호작용(범주형 2 df, 추세 1 df, 주 5잔 이상 여부 1 df)
dwf <- dat_of(D[["여자"]]); Dwm <- dsub(D[["여자"]], !is.na(dwf$meno)); dwm <- dat_of(Dwm)
for (mdl in c("Model 2", "Model 3")) for (oc in ocs) {
  covs <- models4[[mdl]]
  for (m in levels(dwm$meno)) {
    ds <- dsub(Dwm, dwm$meno == m); wh <- paste("폐경", m, mdl)
    a <- run2(ds, oc$y, "drink3", "flush_now", covs, oc$fam, oc$or, wh, lrt = FALSE)
    tt <- trend_t(ds, oc$y, "flush_now", covs, oc$fam, oc$or, paste(wh, "추세"))
    add("meno_cells", cbind(outcome = oc$lab, menopause = m, model = mdl, a$cells))
    add("meno_tests", data.table(outcome = oc$lab, menopause = m, model = mdl, test = "음주 수준 × 현재 홍조 (2 df)", n_model = a$r$n, df = a$test$df, F = a$test$F, p = a$test$p))
    add("meno_tests", data.table(outcome = oc$lab, menopause = m, model = mdl, test = "추세 차이: 홍조 vs 비홍조 (1 df)", n_model = tt$r$n, df = 1L, F = NA_real_, p = tt$tab$p_trend_x_modifier[1]))
  }
  for (cc in list(c("drink3 * flush_now", "drink3", "범주형"), c("drink_ord * flush_now", "drink_ord", "추세"), c("drink2 * flush_now", "drink2", "주 5잔 이상 여부"))) {
    r3 <- fit5(Dwm, as.formula(paste(oc$y, "~ meno * (", paste(c(cc[1], covs), collapse = " + "), ")")), oc$fam, paste("폐경 3원", cc[3], mdl))
    nm <- pick(r3$fit, c("meno", cc[2], "flush_now")); w <- wald_names(r3$fit, nm)
    add("meno_tests", data.table(outcome = oc$lab, menopause = "폐경 변수 있는 여성 전체", model = mdl, test = sprintf("음주 × 현재 홍조 × 폐경 3원: %s (%d df)", cc[3], w$df),
                                 n_model = r3$n, df = w$df, F = w$F, p = w$p))
  }
}

# ==== 7. 민감도 분석 (고요산혈증, Model 2·3) ==================================
sp0 <- list(inc = "inc", expo = "drink3", mod = "flush_now", hist = TRUE, models = c("Model 2", "Model 3"), covf = identity)
sens_specs <- list(
  list(id = "S0",  label = "주 분석(비교 기준)"),
  list(id = "S1",  label = "통풍 의사진단자 포함(모름·무응답은 제외)", inc = "inc_s1"),
  list(id = "S2",  label = "통풍 문항 모름·무응답 포함(진단자는 제외)", inc = "inc_s2"),
  list(id = "S3a", label = "대표값 변경: 월 1회 미만 = 월 0.25회, 주 4회 이상 = 주 4회", expo = "drink3_b"),
  list(id = "S3b", label = "대표값 변경: 월 1회 미만 = 월 0.75회, 주 4회 이상 = 주 7회", expo = "drink3_c"),
  list(id = "S4a", label = "2019년 제외", inc = "inc_no2019"),
  list(id = "S4b", label = "2020년 제외", inc = "inc_no2020"),
  list(id = "S4c", label = "2021년 제외", inc = "inc_no2021"),
  list(id = "S6",  label = "현재 홍조 = '자주·늘'만('가끔'은 제외)", mod = "flush_now_ko", hist = FALSE),
  list(id = "S8",  label = "고혈압·당뇨병을 의사진단으로 보정", inc = "inc_s8", models = "Model 3", covf = function(cv) sub("^f_dm$", "f_dm_dx", sub("^f_htn$", "f_htn_dx", cv))),
  list(id = "R1",  label = "흡연 3범주(비흡연·과거·현재)", covf = function(cv) sub("^f_smk$", "f_smk3", cv)),
  list(id = "R2",  label = "임신·수유 여성 제외", inc = "inc_nopreg"),
  list(id = "R3",  label = "흡연 3범주 + 임신·수유 여성 제외", inc = "inc_nopreg", covf = function(cv) sub("^f_smk$", "f_smk3", cv)),
  list(id = "A1",  label = "연령을 10세 단위 범주로 보정", covf = function(cv) sub("^age$", "age10", cv)))
for (sp in sens_specs) { sp <- modifyList(sp0, sp)
  for (sx in 1:2) for (mdl in sp$models) {
    ds <- dsub(des5, dd5[[sp$inc]] == 1 & dd5$sex == sx); covs <- sp$covf(models4[[mdl]]); wh <- paste("민감도", sp$id, sexes[sx], mdl)
    a <- run2(ds, "hua", sp$expo, sp$mod, covs, quasibinomial(), TRUE, wh, lrt = FALSE); cl <- a$cells; hi <- cl[exposure == lv3[3]]
    add("sens_main", data.table(id = sp$id, analysis = sp$label, sex = sexes[sx], model = mdl, n_model = a$r$n, p_interaction = a$test$p,
                                nonflush_high_n = hi$n[1], nonflush_high_events = hi$n_event[1], nonflush_high_or = hi$strat_est[1], nonflush_high_lo = hi$strat_lo[1], nonflush_high_hi = hi$strat_hi[1],
                                flush_high_n = hi$n[2], flush_high_events = hi$n_event[2], flush_high_or = hi$strat_est[2], flush_high_lo = hi$strat_lo[2], flush_high_hi = hi$strat_hi[2],
                                ratio_high = hi$ratio_est[2], ratio_high_lo = hi$ratio_lo[2], ratio_high_hi = hi$ratio_hi[2], ratio_high_p = hi$ratio_p[2]))
    if (sp$mod == "flush_now") {            # 2문항 분류(과거 또는 현재 홍조)에도 같은 민감도 분석을 적용한다
      ev <- run2(ds, "hua", sp$expo, "flush_ever", covs, quasibinomial(), TRUE, paste(wh, "2문항 분류"), lrt = FALSE); eh <- ev$cells[exposure == lv3[3]]
      add("sens_ever", data.table(id = sp$id, analysis = sp$label, sex = sexes[sx], model = mdl, n_model = ev$r$n, p_interaction = ev$test$p,
                                  never_high_n = eh$n[1], never_high_events = eh$n_event[1], never_high_or = eh$strat_est[1], never_high_lo = eh$strat_lo[1], never_high_hi = eh$strat_hi[1],
                                  ever_high_n = eh$n[2], ever_high_events = eh$n_event[2], ever_high_or = eh$strat_est[2], ever_high_lo = eh$strat_lo[2], ever_high_hi = eh$strat_hi[2],
                                  ratio_high = eh$ratio_est[2], ratio_high_lo = eh$ratio_lo[2], ratio_high_hi = eh$ratio_hi[2], ratio_high_p = eh$ratio_p[2]))
    }
    if (sp$hist) {
      h <- hist_core(ds, "hua", sp$expo, covs, quasibinomial(), TRUE, paste(wh, "이력")); c1 <- h$con[contrast == "소실 vs 비홍조"]; sl <- h$strat_high[group == "홍조 소실"]
      # 칸별 사례 수와 분리(사례 0명인 칸 때문에 계수가 발산) 여부. 분리된 모형의 Wald p와 OR은 의미가 없으므로 결측으로 둔다
      mev <- min(h$r$data[flush_hist %in% c("비홍조", "홍조 소실"), .(ev = sum(hua)), by = c("flush_hist", sp$expo)]$ev)
      bb <- coef(h$r$fit); sep <- max(abs(bb[names(bb) != "(Intercept)"])) > 8; nz <- function(x) if (sep) NA_real_ else x
      add("sens_hist", data.table(id = sp$id, analysis = sp$label, sex = sexes[sx], model = mdl, n_model = h$r$n, min_cell_events = mev, estimable = mev >= EST_MIN_EVENTS, separation = sep,
                                  p_omnibus_4df = nz(h$omni$p), p_lost_vs_never_2df = nz(c1$p_wald),
                                  lost_high_n = sl$n, lost_high_events = sl$n_event, lost_high_or = nz(sl$est), lost_high_lo = nz(sl$lo), lost_high_hi = nz(sl$hi),
                                  ratio_high = nz(c1$high_est), ratio_high_lo = nz(c1$high_lo), ratio_high_hi = nz(c1$high_hi), ratio_high_p = nz(c1$high_p)))
    }
  }
}
# 7-2. 절단점과 용량 형태를 바꿔도 같은가: 현재 홍조(주질문)와 2문항 분류(부질문 1 보조)의 상호작용을 같은 틀로 나란히 본다
#      (절단점을 고르는 용도가 아니라, 결과가 절단점 선택에 얼마나 의존하는지 공개하는 용도)
alt_schemes <- list("3.5·14잔(계획서)" = c(3.5, 14), "5·15잔(주 기준)" = c(5, 15), "5·15·30잔(4군)" = c(5, 15, 30))
for (s in sexes) for (mdl in c("Model 2", "Model 3")) for (md in c("flush_now", "flush_ever")) {
  covs <- models4[[mdl]]; wh <- paste("형태 의존성", s, mdl, md)
  for (sc in names(alt_schemes)) {
    br <- alt_schemes[[sc]]; ds <- D[[s]]; v <- ds$variables
    v$drinkX <- cut(v$drinks_wk, c(-Inf, br, Inf), right = FALSE, labels = paste0("L", seq_len(length(br) + 1))); ds$variables <- v
    r <- fit5(ds, mk_form("hua", paste("drinkX *", md), covs), quasibinomial(), paste(wh, sc)); tt <- term_test(r$fit, paste0("drinkX:", md))
    nm <- grep(paste0("^drinkX.+:", md), names(coef(r$fit)), value = TRUE); top <- lin_t(r$fit, ones(nm[length(nm)]), exp)
    k <- r$data$drinkX == paste0("L", length(br) + 1) & as.integer(r$data[[md]]) == 2
    add("alt_form", data.table(sex = s, model = mdl, modifier = md, form = sc, n_model = r$n, df = tt$df, p = tt$p, estimate_label = sprintf("주 %s잔 이상에서의 OR의 비", max(br)),
                               top_cell_n = sum(k), top_cell_events = as.integer(sum(r$data$hua[k])), est = top[["est"]], lo = top[["lo"]], hi = top[["hi"]], est_p = top[["p"]]))
  }
  for (xv in list(c("drink_ord", "추세(5·15잔 3군, 한 단계당)"), c("drinks7", "연속형(하루 1잔당)"), c("logdrk", "연속형(log2, 잔수 두 배당)"))) {
    r <- fit5(D[[s]], mk_form("hua", paste(xv[1], "*", md), covs), quasibinomial(), paste(wh, xv[1]))
    nm <- pick(r$fit, c(xv[1], md)); stopifnot(length(nm) == 1); e <- lin_t(r$fit, ones(nm), exp)
    add("alt_form", data.table(sex = s, model = mdl, modifier = md, form = xv[2], n_model = r$n, df = 1L, p = e[["p"]], estimate_label = "기울기(OR)의 비",
                               top_cell_n = NA_integer_, top_cell_events = NA_integer_, est = e[["est"]], lo = e[["lo"]], hi = e[["hi"]], est_p = e[["p"]]))
  }
}
# 7-3. 조사연도별 결과와 연도 간 이질성 (연도별 모형에서는 조사연도 보정을 뺀다. 이질성 검정은 연도마다 모든 항의 효과가 다르도록 둔 통합 모형의 3원 항)
for (s in sexes) for (md in c("flush_now", "flush_ever")) for (mdl in c("Model 2", "Model 3")) {
  covs0 <- setdiff(models4[[mdl]], "f_year"); dat <- dat_of(D[[s]])
  for (yy in levels(dat$f_year)) {
    a <- run2(dsub(D[[s]], dat$f_year == yy), "hua", "drink3", md, covs0, quasibinomial(), TRUE, paste("연도별", s, md, yy, mdl), lrt = FALSE); hi <- a$cells[exposure == lv3[3]]
    add("year_strat", data.table(sex = s, modifier = md, model = mdl, year = yy, n_model = a$r$n, p_interaction = a$test$p,
                                 ref_high_n = hi$n[1], ref_high_events = hi$n_event[1], ref_high_or = hi$strat_est[1], ref_high_lo = hi$strat_lo[1], ref_high_hi = hi$strat_hi[1],
                                 flush_high_n = hi$n[2], flush_high_events = hi$n_event[2], flush_high_or = hi$strat_est[2], flush_high_lo = hi$strat_lo[2], flush_high_hi = hi$strat_hi[2],
                                 ratio_high = hi$ratio_est[2], ratio_high_lo = hi$ratio_lo[2], ratio_high_hi = hi$ratio_hi[2], ratio_high_p = hi$ratio_p[2]))
  }
  r <- fit5(D[[s]], as.formula(paste("hua ~ f_year * (", paste(c(paste("drink3 *", md), covs0), collapse = " + "), ")")), quasibinomial(), paste("연도 이질성", s, md, mdl))
  nm <- pick(r$fit, c("f_year", "drink3", md)); stopifnot(length(nm) == 4); w <- wald_names(r$fit, nm)
  add("year_het", data.table(sex = s, modifier = md, model = mdl, test = "음주 수준 × 홍조 × 조사연도 (3원, 4 df)", n_model = r$n, df = w$df, F = w$F, p = w$p))
}
R <- lapply(res, rbindlist, use.names = TRUE, fill = TRUE)
# 부질문 1 대비의 다중비교: 성별·모형·결과 안에서 세 대비에 Holm 보정. 칸별 사례 수 기준으로 추정 가능 여부 표시
R$hist_contrasts[, p_holm := p.adjust(p_wald, method = "holm"), by = .(outcome, sex, model)]
R$hist_contrasts[, estimable := ifelse(outcome == oc_bin$lab, min_cell_events >= EST_MIN_EVENTS, min_cell_n >= EST_MIN_N)]
for (nm in c("main_cells", "hist_cells", "nonflush_cells", "ever_cells", "grp4_cells", "meno_cells"))
  R[[nm]][, sparse := n < EST_MIN_N | (!is.na(n_event) & n_event < EST_MIN_EVENTS)]
# 민감도·연도별 표에도 사례 수 기준 미달 표시(고음주 칸 기준)
R$sens_main[, sparse := pmin(nonflush_high_events, flush_high_events) < EST_MIN_EVENTS]
R$sens_ever[, sparse := pmin(never_high_events, ever_high_events) < EST_MIN_EVENTS]
R$year_strat[, sparse := pmin(ref_high_events, flush_high_events) < EST_MIN_EVENTS]
diag <- rbindlist(diag_log)

# ==== 8. 공식 통계 대조 ======================================================
# (1) 가중치 합계 = 추계인구수(만 1세 이상, 천명): 8기 PDF p.40 / 인쇄 p.32 표 11
wt_chk <- d[, .(n_all = .N, n_weighted = sum(!is.na(wt_itvex)), estimate = sum(wt_itvex, na.rm = TRUE) / 1000), keyby = year]
wt_chk[, official := c(`2019` = 51386, `2020` = 51474, `2021` = 51531)[as.character(year)]]
wt_chk[, `:=`(indicator = "wt_itvex 합계(천명) = 추계인구수(만 1세 이상)", diff = estimate - official, match = abs(estimate - official) < 0.5,
              source = "8기 원시자료 이용지침서 PDF p.40 / 인쇄 p.32 표 11")]
# (2) 연간음주자(만 19세 이상, 최근 1년 음주자) 중 고위험음주율·월간음주율: 질병관리청 주간 건강과 질병(PHWR) 2024;17(21):877-902 표 1
#     정의(같은 논문): 고위험음주 = 1회 평균 음주량 남 7잔·여 5잔 이상이면서 주 2회 이상, 월간음주 = 한 달에 1회 이상. 변수 코드: 8기 PDF p.161-162, 지표 SAS 정의 p.48
phwr <- data.table(indicator = rep(c("고위험음주율", "월간음주율"), each = 9), year = rep(rep(2019:2021, each = 3), 2), sex_lab = rep(c("전체", "남자", "여자"), 6),
                   official = c(15.8, 21.5, 8.6, 17.7, 25.3, 8.1, 17.1, 23.6, 8.9, 76.6, 84.9, 65.8, 74.8, 82.6, 64.7, 74.2, 82.2, 63.5))
d[, `:=`(ad_high = fifelse(bd1_11 %in% 2:6 & !is.na(bd2_1), as.integer(bd1_11 %in% 5:6 & ((sex == 1 & bd2_1 %in% 4:5) | (sex == 2 & bd2_1 %in% 3:5))), NA_integer_),
         ad_month = fifelse(bd1_11 %in% 2:6, as.integer(bd1_11 %in% 3:6), NA_integer_))]
yr_est <- function(yr, sx, v, pop) {       # 단년도 가중치로 부분모집단 추정(설계 지정 → subset)
  dd <- d[year == yr]; dd[, `:=`(w1 = fifelse(!is.na(wt_itvex), wt_itvex, 0), val = get(v))]
  dd[, flag := as.integer(!is.na(wt_itvex) & age >= 19 & (sx == "전체" | f_sex == sx) & eval(pop))]
  dz <- subset(svydesign(ids = ~psu, strata = ~kstrata, weights = ~w1, nest = TRUE, data = as.data.frame(dd)), flag == 1)
  est <- svymean(~val, dz, na.rm = TRUE); ci <- confint(est)
  data.table(n_unweighted = dd[flag == 1 & !is.na(val), .N], estimate = 100 * unname(coef(est)), se = 100 * as.numeric(SE(est)), ci_low = 100 * unname(ci[1]), ci_high = 100 * unname(ci[2]))
}
off_ad <- rbindlist(lapply(seq_len(nrow(phwr)), function(i) cbind(phwr[i], yr_est(phwr$year[i], phwr$sex_lab[i], if (phwr$indicator[i] == "고위험음주율") "ad_high" else "ad_month", quote(bd1_11 %in% 2:6)))))
off_ad[, `:=`(diff = estimate - official, within_ci = official >= ci_low & official <= ci_high, match_rounded = abs(estimate - official) < 0.1 + 1e-9,
              source = "PHWR 2024;17(21):877-902 표 1 (연간음주자 중 분율, 국민건강영양조사)")]
# (3) 참고: 만 19세 이상 전체의 고위험음주율. 공식 발표값은 2005년 추계인구 연령표준화율이라 조율과 직접 비교할 수 없다(표준인구가 지침서에 없음)
ref_std <- data.table(year = rep(2019:2021, each = 3), sex_lab = rep(c("전체", "남자", "여자"), 3), official_age_standardized = c(12.6, 18.6, 6.5, 14.1, 21.6, 6.3, 13.4, 19.7, 6.9))
ref_std <- cbind(ref_std, rbindlist(lapply(seq_len(nrow(ref_std)), function(i) yr_est(ref_std$year[i], ref_std$sex_lab[i], "dr_high", quote(TRUE)))))
setnames(ref_std, c("estimate", "se", "ci_low", "ci_high"), c("crude_estimate", "crude_se", "crude_ci_low", "crude_ci_high"))
ref_std[, source := "국가정신건강정보포털 정신건강통계(출처 표기: 질병관리청 2022년 국민건강통계, 2005년 추계인구 연령표준화)"]

# ==== 9. 검증: 새 도구(t 분포)의 추정치가 기존 도구와 같은가, 5·15 검정값이 code/21과 같은가 =========
val <- list()
addv <- function(item, a, b, tol = 1e-8) { stopifnot(length(a) == length(b), length(a) > 0)
  val[[length(val) + 1L]] <<- data.table(item = item, n_compared = length(a), max_abs_diff = max(abs(a - b)), pass = isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))) }
jt <- joint_table(fit_svy(D[["남자"]], mk_form("hua", "drink3 * flush_now", cov_base), quasibinomial()), "drink3", "flush_now", TRUE)
mc <- R$main_cells[outcome == oc_bin$lab & sex == "남자" & model == "Model 2"]
addv("칸 표 추정치(결합·층화·RERI): cells_t = code/13 joint_table", c(mc$joint_est, mc$strat_est, mc$reri[!is.na(mc$reri)]), c(jt$joint_est, jt$strat_est, jt$reri[!is.na(jt$reri)]))
f21 <- file.path("output", "17_cutpoint_5_15", c("02b_main_tests.csv", "03b_history_tests.csv", "04a_sex_tests.csv"))
if (all(file.exists(f21))) {
  rd <- function(f) { x <- fread(f, encoding = "UTF-8"); x[grepl("^5", scheme)] }
  t21 <- rd(f21[1]); addv("주질문 상호작용 p = code/21 (유효숫자 3자리)", signif(R$main_tests$p, 3), t21$p, 1e-6)
  h21 <- rd(f21[2])[grepl("공동", test)]; addv("부질문 1 공동 검정 p = code/21 (유효숫자 3자리)", signif(R$hist_tests$p, 3), h21$p, 1e-6)
  h21c <- rd(f21[2])[!grepl("공동", test)]; addv("부질문 1 대비 검정 p = code/21 (유효숫자 3자리)", signif(R$hist_contrasts$p_wald, 3), h21c$p, 1e-6)
  s21 <- rd(f21[3])[grepl("3원", test)]; addv("부질문 2 3원 p(공변량 남녀 공통) = code/21 (유효숫자 3자리)", signif(R$sex_tests[grepl("3원", test) & grepl("공통", covariates), p], 3), s21$p, 1e-6)
}
# 성별 공변량 상호작용 모형의 3원 계수 = 여자 층화 모형 계수 − 남자 층화 모형 계수
st <- R$sex_terms[outcome == oc_bin$lab & model == "Model 2" & grepl("층화", covariates)]
mcw <- R$main_cells[outcome == oc_bin$lab & model == "Model 2" & modifier == "홍조" & exposure != lv3[1]]
addv("3원 계수 = 여자 OR의 비 ÷ 남자 OR의 비(층화 모형)", st$est, mcw[sex == "여자", ratio_est] / mcw[sex == "남자", ratio_est], 1e-6)
val <- rbindlist(val)
if (!all(val$pass)) warning("검증에서 일치하지 않는 항목: ", paste(val[pass == FALSE, item], collapse = " / "))

# ==== 10. 저장 ===============================================================
derived <- data.table(
  variable = c("drinks_wk", "drink3", "drink_ord", "flush_now", "flush_grp / flush_hist", "flush_ever", "past_flush", "hua", "uacid", "meno", "보정변수"),
  definition = c("주당 음주잔수 = 음주 빈도 대표값(회/주) × 1회 음주량 대표값(잔). 10잔 이상은 BD2_14 기입값. 환산표: output/13_v3_main_analysis/03c_conversion_table.csv",
                 "저음주 = 주 5잔 미만, 중음주 = 5잔 이상 15잔 미만, 고음주 = 15잔 이상 (남녀 공통, 저음주 기준)",
                 "음주 수준의 순서형 점수 0·1·2 (추세 검정용)",
                 "현재 홍조: BD9_2 = 2·3·4(가끔·자주·늘) → 홍조, 1(아니오) → 비홍조",
                 "BD9_1(과거) × BD9_2(현재) 조합 4군: 비홍조, 홍조 소실, 지속 홍조, 신규 홍조. 이력 3군(flush_hist)은 신규 홍조 제외",
                 "2문항 분류: 과거(BD9_1 = 1) 또는 현재(BD9_2 = 2–4) 홍조 → 홍조 체질, 둘 다 없음 → 홍조 없음",
                 "과거 홍조: BD9_1 = 1(예) / 2(아니오)",
                 "고요산혈증: 혈청 요산 남 7.0 mg/dL 이상, 여 6.0 mg/dL 이상",
                 "혈청 요산(mg/dL). '<1.0' 표기(4명)는 결측",
                 "폐경 상태: LW_ms 5·6 = 폐경 후, 2·3·4 = 폐경 전, 1·7은 결측",
                 "Model 1 = 연령 / Model 2 = + 교육·소득·조사연도·현재흡연·유산소 신체활동 / Model 3 = + BMI·고혈압(HE_HP = 3)·당뇨병(HE_DM_HbA1c = 3)·eGFR(CKD-EPI 2009)"),
  source = c("BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162 / 인쇄 p.153-154. 대표값은 연구보고서 V3 환산표(가정)",
             "절단점은 지침서에 없음. 문헌 근거: output/15_cutpoint_exploration/04_절단점_근거_검토.md (1잔 ≈ 7 g 가정)",
             "(파생)", "BD9_2: 8기 PDF p.163-164 / 인쇄 p.155-156", "BD9_1·BD9_2: 8기 PDF p.163-164 / 인쇄 p.155-156",
             "문항: 8기 PDF p.163-164 / 인쇄 p.155-156. 분류 근거: Yokoyama 2003 Cancer Epidemiol Biomarkers Prev 12:1227-33; Brooks 2009 PLoS Med 6:e50",
             "BD9_1: 8기 PDF p.163-164 / 인쇄 p.155-156", "HE_Uacid: 8기 PDF p.200 / 인쇄 p.192. 기준값은 지침서에 없는 문헌 기준(조작적 정의)",
             "HE_Uacid·HE_Uacid_etc: 8기 PDF p.200 / 인쇄 p.192", "LW_ms: 8기 PDF p.190 / 인쇄 p.182",
             "code/13_v3_main_analysis.R의 var_dict(01_variable_definitions.csv)와 같음. eGFR 식은 지침서에 없는 문헌식"))
outs <- list("01_variable_definitions.csv" = var_dict, "01b_derived_variables.csv" = derived, "02_participant_flow.csv" = flow,
             "03a_distribution.csv" = dist, "03b_cell_counts.csv" = cellsD,
             "04a_table1_by_sex_drinklevel.csv" = t1_drink, "04b_table1_by_sex_flushnow.csv" = t1_now, "04c_table1_by_sex_flushhist.csv" = t1_hist,
             "05a_main_cells.csv" = R$main_cells, "05b_main_tests.csv" = R$main_tests, "05c_main_trend.csv" = R$main_trend, "05d_main_effects.csv" = R$main_effects,
             "06a_hist_cells.csv" = R$hist_cells, "06b_hist_omnibus_tests.csv" = R$hist_tests, "06c_hist_contrasts.csv" = R$hist_contrasts, "06d_hist_trend.csv" = R$hist_trend,
             "06e_nonflusher_past_cells.csv" = R$nonflush_cells, "06f_nonflusher_past_tests.csv" = R$nonflush_tests,
             "06g_ever_never_cells.csv" = R$ever_cells, "06h_ever_never_tests.csv" = R$ever_tests, "06i_ever_never_trend.csv" = R$ever_trend, "06j_four_groups_cells.csv" = R$grp4_cells,
             "07a_sex_tests.csv" = R$sex_tests, "07b_sex_3way_terms.csv" = R$sex_terms, "07c_sex_dose_3way.csv" = R$sex_dose,
             "08a_menopause_cells.csv" = R$meno_cells, "08b_menopause_tests.csv" = R$meno_tests,
             "09a_sensitivity_main.csv" = R$sens_main, "09b_sensitivity_history.csv" = R$sens_hist, "09c_sensitivity_ever_never.csv" = R$sens_ever,
             "09d_cutpoint_form_dependence.csv" = R$alt_form, "09e_year_specific.csv" = R$year_strat, "09f_year_heterogeneity.csv" = R$year_het,
             "10a_official_weight_totals.csv" = wt_chk, "10b_official_annual_drinkers.csv" = off_ad, "10c_reference_age_standardized.csv" = ref_std,
             "11_model_diagnostics.csv" = diag, "12_validation.csv" = val, "_warnings.csv" = warn_log)
for (nm in names(outs)) fwrite(round_dt(outs[[nm]]), file.path(out, nm), bom = TRUE)
writeLines(c("연도: 2019, 2020, 2021 / 가중치: wt_itvex × 1/3(제8기 기수 내 조사기간 비례 통합비율, 8기 PDF p.36 / 인쇄 p.28), 층 kstrata, 집락 psu",
             "n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치(SE)",
             "p_value: 범주형은 Rao-Scott 보정 카이제곱(F), 연속형은 복합표본 선형회귀 Wald 검정 (성별 안에서 군 간 비교)",
             "음주 수준: 저음주 주 5잔 미만, 중음주 5잔 이상 15잔 미만, 고음주 15잔 이상. 고요산혈증: 남 7.0, 여 6.0 mg/dL 이상(조작적 정의). 통풍 의사진단자(모름·무응답 포함) 제외."),
           file.path(out, "04_table1_footnote.txt"))
writeLines(capture.output(sessionInfo()), file.path(out, "_sessionInfo.txt"))
fwrite(data.table(file = files, size = file.size(files), md5 = unname(tools::md5sum(files))), file.path(out, "_inputs.csv"), bom = TRUE)
save_objs <- list(R = R, dist = dist, cellsD = cellsD, wt_chk = wt_chk, off_ad = off_ad, ref_std = ref_std, val = val, diag = diag, warn_log = warn_log, flow = flow,
                  t1_drink = t1_drink, ref13 = ref13, n_sub = nrow(d5), n_men = d5[f_sex == "남자", .N], n_women = d5[f_sex == "여자", .N])
saveRDS(save_objs, file.path(out, "_results.rds"))

cat("\n[검증]\n"); print(val)
cat("\n[공식 통계 대조: 가중치 합계]\n"); print(wt_chk[, .(year, estimate = round(estimate, 2), official, match)])
cat("\n[공식 통계 대조: 연간음주자 중 고위험·월간음주율]\n"); print(off_ad[, .(indicator, year, sex_lab, n_unweighted, estimate = round(estimate, 2), official, diff = round(diff, 2), within_ci, match_rounded)])
cat("\n[모형 진단] 적합", nrow(diag), "개 / 수렴 실패", diag[converged == FALSE, .N], "/ NA 계수", diag[n_na_coef > 0, .N], "/ |계수| > 8:", diag[max_abs_coef > 8, .N], "/ 경고", nrow(warn_log), "건\n")
cat("\n[주질문]\n"); print(R$main_tests[, .(outcome, sex, model, n_model, F = round(F, 2), p = signif(p, 3), p_lrt = signif(p_lrt, 3))])
cat("\n[부질문 1 대비: 고요산혈증, Model 2·3]\n")
print(R$hist_contrasts[outcome == oc_bin$lab & model %in% c("Model 2", "Model 3"), .(sex, model, contrast, p_wald = signif(p_wald, 3), p_lrt = signif(p_lrt, 3), p_holm = signif(p_holm, 3),
                       high = sprintf("%.2f (%.2f-%.2f)", high_est, high_lo, high_hi), p_trend_diff = signif(p_trend_diff, 3), min_cell_events, estimable)])
cat("\n[부질문 2 3원]\n"); print(R$sex_tests[grepl("3원", test), .(outcome, model, covariates, n_model, F = round(F, 2), p = signif(p, 3), p_lrt = signif(p_lrt, 3))])
cat("\n완료:", out, "\n")
