# 21_cutpoint_5_15_main_sub.R
# 목적: 음주 수준 절단점을 주 5잔·15잔(저음주 5잔 미만, 중음주 5잔 이상 15잔 미만, 고음주 15잔 이상)으로 바꿔
#       주질문(음주 수준 × 현재 홍조)과 부질문 1(홍조 이력)·부질문 2(성별, 여성 폐경)를 계획서와 같은 방법으로 다시 분석한다.
#       같은 코드로 계획서 절단점(3.5·14잔)도 함께 돌려 나란히 비교하고, 기존 결과(code/13, code/18)와 수치가 같은지 확인한다.
# 절단점 근거: output/15_cutpoint_exploration/04_절단점_근거_검토.md (국건영 1잔 ≈ 순알코올 7 g → 5·15잔/주 ≈ 5·15 g/일)
# 주의: 계획서(3.5·14잔) 결과를 본 뒤에 바꾼 절단점이므로 사후 분석이다. 보고할 때 계획서 기준 결과와 함께 싣는다.
# 변수 정의·결측 처리·대상 선정(inc 표시)·설계(wt_itvex × 1/3, kstrata, psu)는 code/13_v3_main_analysis.R을 그대로 쓴다.
#   음주 빈도·1회량 BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162 / 인쇄 p.153-154
#   홍조 BD9_1·BD9_2: 8기 PDF p.163-164 / 인쇄 p.155-156, 요산 HE_Uacid: 8기 PDF p.200 / 인쇄 p.192, 폐경 LW_ms: 8기 PDF p.190 / 인쇄 p.182
# 실행: Rscript --vanilla code/21_cutpoint_5_15_main_sub.R   (프로젝트 폴더에서)
# 결과: output/17_cutpoint_5_15/

suppressPackageStartupMessages({ library(data.table); library(survey) })
source("code/13_v3_main_analysis.R")   # 자료·변수·대상 표시·설계 객체·함수(run_int, joint_table, trend_table, contrast_test, summ_models 등)

out5 <- file.path("output", "17_cutpoint_5_15")
dir.create(out5, showWarnings = FALSE, recursive = TRUE)
# 13번 스크립트가 계획서 절단점으로 만든 검정 결과를 대조용으로 보관한다
ref13 <- list(int4 = copy(int4), hist_tests = copy(hist_tests), sexdiff = copy(sexdiff), meno_tests = copy(meno_tests))

schemes <- list("5·15잔" = c(5, 15), "3.5·14잔(계획서)" = c(3.5, 14))
S5 <- names(schemes)[1]; S0 <- names(schemes)[2]
lv3 <- c("저음주", "중음주", "고음주")
rng <- function(br) c(sprintf("주 %s잔 미만", br[1]), sprintf("주 %s잔 이상 %s잔 미만", br[1], br[2]), sprintf("주 %s잔 이상", br[2]))
recut <- function(ds, br) {            # 설계 객체 안의 음주 수준 변수만 새 절단점으로 바꾼다(행·가중치·층·집락은 그대로)
  v <- ds$variables
  v$drink3    <- cut(v$drinks_wk, c(-Inf, br, Inf), right = FALSE, labels = lv3)
  v$drink_ord <- as.numeric(v$drink3) - 1          # 추세 검정용 순서형(0·1·2)
  ds$variables <- v; ds
}
# 상호작용항 계수(OR의 비 또는 β의 차이)와 95% CI. 이름으로 찾으므로 계수 순서와 무관하다
int_terms <- function(fit, pat, or) {
  ct <- summary(fit)$coefficients; nm <- grep(pat, rownames(ct), value = TRUE); tr <- if (or) exp else identity
  data.table(term = nm, est = tr(ct[nm, 1]), ci_low = tr(ct[nm, 1] - z975 * ct[nm, 2]), ci_high = tr(ct[nm, 1] + z975 * ct[nm, 2]), p = ct[nm, ncol(ct)])
}
ocs <- list(oc_bin, oc_con)            # 고요산혈증(OR), 혈청 요산(β)

# ==== 한 절단점에 대한 전체 분석 =============================================
analyze <- function(sc) {
  br <- schemes[[sc]]
  D    <- lapply(sdes_sex, recut, br = br)      # 성별 설계 객체(분석 대상 subset)
  Dall <- recut(sdes, br)                        # 남녀 전체(성별 3원 상호작용용)
  L <- list(main_cells = list(), main_tests = list(), main_terms = list(), main_trend = list(), main_effects = list(),
            hist_cells = list(), hist_tests = list(), hist_terms = list(), hist_trend = list(), grp4 = list(),
            sex_tests = list(), sex_terms = list(), meno_cells = list(), meno_tests = list())
  add <- function(nm, x) L[[nm]][[length(L[[nm]]) + 1]] <<- x

  # (0) 음주 수준별 인원(비가중)·가중 분율·고요산혈증 사례와 가중 유병률·가중 평균 요산
  dist <- rbindlist(lapply(sexes, function(s) {
    dat <- dat_of(D[[s]]); est <- svymean(~drink3, D[[s]])
    by_lv <- function(v) sapply(lv3, function(e) unname(coef(svymean(as.formula(paste0("~", v)), dsub(D[[s]], dat$drink3 == e)))[1]))
    data.table(scheme = sc, sex = s, drink = lv3, range = rng(br), n = as.integer(table(dat$drink3)[lv3]),
               wt_pct = 100 * unname(coef(est)[paste0("drink3", lv3)]), n_hua = as.integer(tapply(dat$hua, dat$drink3, sum)[lv3]),
               hua_pct_wt = 100 * by_lv("hua"), uacid_mean_wt = by_lv("uacid"), drinks_wk_mean_wt = by_lv("drinks_wk"))
  }))

  # (1) 주질문: 음주 수준 × 현재 홍조 (남녀 각각, Crude·Model 1–3, 고요산혈증 OR과 요산 β)
  for (s in sexes) for (mdl in names(models4)) {
    covs <- models4[[mdl]]
    for (oc in ocs) {
      a <- run_int(D[[s]], oc$y, "drink3", "flush_now", covs, oc$fam, oc$or)
      add("main_cells", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, a$cells))
      add("main_tests", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, test = "음주 수준 × 현재 홍조 (공동, 2 df)", a$test))
      add("main_terms", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, n_model = a$r$n, int_terms(a$r$fit, "^drink3.+:flush_now", oc$or)))
      add("main_trend", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, trend_table(D[[s]], oc$y, "flush_now", covs, oc$fam, oc$or)))
    }
    add("main_effects", cbind(scheme = sc, sex = s, model = mdl, summ_models(D[[s]], covs)))   # 상호작용항 없는 모형의 주효과 OR
  }

  # (2) 부질문 1: 음주 수준 × 홍조 이력 3군(비홍조·소실·지속, 신규 제외) — 공동 4 df, 군별 대비 2 df
  nm_h <- function(e, g) paste0("drink3", e, ":flush_hist", g)
  for (s in sexes) for (mdl in names(models4)) for (oc in ocs) {
    covs <- models4[[mdl]]
    a <- run_int(D[[s]], oc$y, "drink3", "flush_hist", covs, oc$fam, oc$or); f <- a$r$fit
    ct <- function(Lc, lab) cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, test = lab, n_model = a$r$n, contrast_test(f, Lc))
    add("hist_cells", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, a$cells))
    add("hist_tests", rbind(
      cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, test = "음주 수준 × 홍조 이력 (공동, 4 df)", a$test),
      ct(lapply(lv3[-1], function(e) setNames(1, nm_h(e, "홍조 소실"))), "소실 vs 비홍조 (2 df)"),
      ct(lapply(lv3[-1], function(e) setNames(1, nm_h(e, "지속 홍조"))), "지속 vs 비홍조 (2 df)"),
      ct(lapply(lv3[-1], function(e) setNames(c(1, -1), c(nm_h(e, "홍조 소실"), nm_h(e, "지속 홍조")))), "소실 vs 지속 (2 df, 탐색)")))
    add("hist_terms", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, n_model = a$r$n, int_terms(f, "^drink3.+:flush_hist", oc$or)))
    add("hist_trend", cbind(scheme = sc, outcome = oc$lab, sex = s, model = mdl, trend_table(D[[s]], oc$y, "flush_hist", covs, oc$fam, oc$or)))
  }
  # 보조: 신규 홍조를 포함한 4군 (고요산혈증, Model 2·3)
  for (s in sexes) for (mdl in c("Model 2", "Model 3")) {
    a <- run_int(D[[s]], "hua", "drink3", "flush_grp", models4[[mdl]], quasibinomial(), TRUE)
    add("grp4", cbind(scheme = sc, outcome = oc_bin$lab, sex = s, model = mdl, p_interaction_6df = a$test$p, a$cells))
  }

  # (3) 부질문 2: 성별 차이 — 3원 상호작용(음주 × 현재 홍조 × 성별)과 홍조군 안에서의 음주 × 성별
  dall <- dat_of(Dall)
  for (mdl in names(models4)) for (oc in ocs) {
    covs <- models4[[mdl]]
    r3 <- fit_svy(Dall, mk_form(oc$y, "drink3 * flush_now * f_sex", covs), oc$fam)
    add("sex_tests", cbind(scheme = sc, outcome = oc$lab, model = mdl, test = "음주 수준 × 현재 홍조 × 성별 (3원, 2 df)", n_model = r3$n, term_test(r3$fit, "drink3:flush_now:f_sex")))
    add("sex_terms", cbind(scheme = sc, outcome = oc$lab, model = mdl, n_model = r3$n, int_terms(r3$fit, "^drink3.+:flush_now.+:f_sex", oc$or)))
    for (m in levels(dall$flush_now)) {
      r2 <- fit_svy(dsub(Dall, dall$flush_now == m), mk_form(oc$y, "drink3 * f_sex", covs), oc$fam)
      add("sex_tests", cbind(scheme = sc, outcome = oc$lab, model = mdl, test = paste0("음주 수준 × 성별 (", m, " 안에서, 2 df)"), n_model = r2$n, term_test(r2$fit, "drink3:f_sex")))
    }
  }

  # (4) 부질문 2(탐색): 여성 폐경 전·후 — 층별 음주 × 현재 홍조, 3원 상호작용(음주 × 홍조 × 폐경)
  Dw <- D[["여자"]]; dw <- dat_of(Dw)
  for (mdl in c("Model 2", "Model 3")) for (oc in ocs) {
    covs <- models4[[mdl]]
    for (m in levels(dw$meno)) {
      a <- run_int(dsub(Dw, !is.na(dw$meno) & dw$meno == m), oc$y, "drink3", "flush_now", covs, oc$fam, oc$or)
      add("meno_cells", cbind(scheme = sc, outcome = oc$lab, menopause = m, model = mdl, a$cells))
      add("meno_tests", cbind(scheme = sc, outcome = oc$lab, menopause = m, model = mdl, test = "음주 수준 × 현재 홍조 (2 df)", a$test))
    }
    r3 <- fit_svy(Dw, mk_form(oc$y, "drink3 * flush_now * meno", covs), oc$fam)
    add("meno_tests", cbind(scheme = sc, outcome = oc$lab, menopause = "폐경 변수 있는 여성 전체", model = mdl,
                            test = "음주 수준 × 현재 홍조 × 폐경 (3원, 2 df)", n_model = r3$n, term_test(r3$fit, "drink3:flush_now:meno")))
  }
  c(list(dist = dist), lapply(L, rbindlist))
}

res_list <- lapply(setNames(names(schemes), names(schemes)), analyze)
res <- lapply(setNames(names(res_list[[1]]), names(res_list[[1]])), function(nm) rbindlist(lapply(res_list, `[[`, nm)))
# 인원이 적은 칸 표시: 30명 미만이거나 고요산혈증 사례 5명 미만
for (nm in c("main_cells", "hist_cells", "grp4", "meno_cells")) res[[nm]][, sparse := n < 30 | (!is.na(n_event) & n_event < 5)]

# ==== 절단점을 바꾸면 누가 다른 군으로 옮겨 가는가 ============================
dsb <- dat_of(sdes)
dsb[, `:=`(old3 = cut(drinks_wk, c(-Inf, schemes[[S0]], Inf), right = FALSE, labels = lv3),
           new3 = cut(drinks_wk, c(-Inf, schemes[[S5]], Inf), right = FALSE, labels = lv3))]
rec <- dsb[, .(n = .N), keyby = .(sex = f_sex, cut_3.5_14 = old3, cut_5_15 = new3)]
freq_lab <- c(`2` = "월 1회 미만", `3` = "월 1회 정도", `4` = "월 2-4회", `5` = "주 2-3회", `6` = "주 4회 이상")       # BD1_11 (8기 PDF p.161)
amt_lab  <- c(`1` = "1-2잔", `2` = "3-4잔", `3` = "5-6잔", `4` = "7-9잔", `5` = "10잔 이상(기입값)")                   # BD2_1 (8기 PDF p.162)
moved <- dsb[old3 != new3, .(n = .N), keyby = .(sex = f_sex, from = old3, to = new3, frequency = unname(freq_lab[as.character(bd1_11)]),
                                                amount = unname(amt_lab[as.character(bd2_1)]), per_occasion = per_occ, drinks_wk = round(drinks_wk, 2))]

# ==== 두 절단점 비교표 =======================================================
key_tests <- rbind(
  res$main_tests[, .(scheme, question = "주질문", outcome, sex, model, test, p)],
  res$hist_tests[, .(scheme, question = "부질문 1(홍조 이력)", outcome, sex, model, test, p)],
  res$sex_tests[,  .(scheme, question = "부질문 2(성별)", outcome, sex = "남녀 전체", model, test, p)],
  res$meno_tests[, .(scheme, question = "부질문 2(여성 폐경, 탐색)", outcome, sex = paste0("여자: ", menopause), model, test, p)])
same_keys <- function(a, b, vars) nrow(a) == nrow(b) && all(vapply(vars, function(v) identical(as.character(a[[v]]), as.character(b[[v]])), logical(1)))
k5 <- key_tests[scheme == S5]; k0 <- key_tests[scheme == S0]
stopifnot(same_keys(k5, k0, c("question", "outcome", "sex", "model", "test")))      # 두 절단점의 행 순서가 같아야 나란히 붙일 수 있다
cmp_p <- cbind(k5[, .(question, outcome, sex, model, test, p_5_15 = p)], p_3.5_14 = k0$p)
e5 <- res$main_cells[scheme == S5 & outcome == oc_bin$lab & exposure != lv3[1]]; e0 <- res$main_cells[scheme == S0 & outcome == oc_bin$lab & exposure != lv3[1]]
stopifnot(same_keys(e5, e0, c("sex", "model", "modifier", "exposure")))
cmp_or <- cbind(e5[, .(sex, model, modifier, exposure, n_5_15 = n, events_5_15 = n_event, or_5_15 = strat_est, lo_5_15 = strat_lo, hi_5_15 = strat_hi)],
                e0[, .(n_3.5_14 = n, events_3.5_14 = n_event, or_3.5_14 = strat_est, lo_3.5_14 = strat_lo, hi_3.5_14 = strat_hi)])

# ==== 검증: 같은 코드로 돌린 계획서 절단점 결과가 13번 결과와, 5·15 결과가 18번 결과와 같은가 ====
val <- list()
addv <- function(item, a, b) {
  stopifnot(length(a) == length(b), length(a) > 0)
  val[[length(val) + 1]] <<- data.table(item = item, n_compared = length(a), max_abs_diff = max(abs(a - b)), pass = isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-6)))
}
m0 <- merge(res$main_tests[scheme == S0 & outcome == oc_bin$lab, .(sex, model, p)], unique(ref13$int4[, .(sex, model, p_ref = p_interaction_joint)]), by = c("sex", "model"))
addv("3.5·14 주질문 상호작용 p = code/13(12b)", m0$p, m0$p_ref)
addv("3.5·14 부질문 1 검정 p = code/13(06b), Model 2·3", res$hist_tests[scheme == S0 & outcome == oc_bin$lab & model %in% c("Model 2", "Model 3"), p],
     ref13$hist_tests[outcome == oc_bin$lab, p])
addv("3.5·14 부질문 2 성별 검정 p = code/13(07), Model 2·3", res$sex_tests[scheme == S0 & model %in% c("Model 2", "Model 3"), p], ref13$sexdiff$p)
addv("3.5·14 폐경 검정 p = code/13(08b), Model 3", res$meno_tests[scheme == S0 & model == "Model 3", p], ref13$meno_tests$p)
f18 <- file.path("output", "15_cutpoint_exploration", "05_cutpoint_5_15_30_tests.csv")
if (file.exists(f18)) {                # 18번 스크립트의 5·15 3군 결과(유효숫자 3자리로 저장됨)
  t18 <- fread(f18, encoding = "UTF-8"); t18 <- t18[grepl("3군", scheme)]
  addv("5·15 주질문 상호작용 p = code/18(유효숫자 3자리)", signif(res$main_tests[scheme == S5 & outcome == oc_bin$lab, p], 3), t18[modifier_var == "현재 홍조", p])
  addv("5·15 부질문 1 검정 p = code/18(유효숫자 3자리), Model 2·3", signif(res$hist_tests[scheme == S5 & outcome == oc_bin$lab & model %in% c("Model 2", "Model 3"), p], 3),
       t18[modifier_var == "홍조 이력", p])
}
val <- rbindlist(val)
if (!all(val$pass)) warning("기존 결과와 다른 항목이 있습니다: ", paste(val[pass == FALSE, item], collapse = " / "))

# ==== 저장 ===================================================================
outs5 <- list("01_distribution.csv" = res$dist, "01b_reclassification.csv" = rec, "01c_moved_response_patterns.csv" = moved,
              "02a_main_cells.csv" = res$main_cells, "02b_main_tests.csv" = res$main_tests, "02c_main_interaction_terms.csv" = res$main_terms,
              "02d_main_trend.csv" = res$main_trend, "02e_main_effects_four_models.csv" = res$main_effects,
              "03a_history_cells.csv" = res$hist_cells, "03b_history_tests.csv" = res$hist_tests, "03c_history_interaction_terms.csv" = res$hist_terms,
              "03d_history_trend.csv" = res$hist_trend, "03e_history_4groups_supp.csv" = res$grp4,
              "04a_sex_tests.csv" = res$sex_tests, "04b_sex_3way_terms.csv" = res$sex_terms,
              "05a_menopause_cells.csv" = res$meno_cells, "05b_menopause_tests.csv" = res$meno_tests,
              "06a_compare_p.csv" = cmp_p, "06b_compare_stratified_or.csv" = cmp_or, "_validation.csv" = val)
for (nm in names(outs5)) fwrite(round_dt(outs5[[nm]]), file.path(out5, nm), bom = TRUE)

# ==== 요약(markdown) — 수치는 모두 위 객체에서 가져온다 =======================
big <- function(x) format(x, big.mark = ",", trim = TRUE)
t5  <- function(nm) res[[nm]][scheme == S5]
mt5 <- t5("main_tests"); ht5 <- t5("hist_tests"); st5 <- t5("sex_tests"); nt5 <- t5("meno_tests")
p_main <- function(s, mdl, o = oc_bin$lab) mt5[sex == s & model == mdl & outcome == o, p]
p_hist <- function(s, mdl, key, o = oc_bin$lab) ht5[sex == s & model == mdl & outcome == o & grepl(key, test, fixed = TRUE), p]
p_rng  <- function(x) sprintf("%s–%s", fmt_p(min(x)), fmt_p(max(x)))
term_lab  <- function(x) gsub(":f_sex", " × ", gsub(":flush_(now|hist)", " × ", sub("^drink3", "", x)))
term_line <- function(x) paste(x[, sprintf("%s %s (p %s)", term_lab(term), ci2(est, ci_low, ci_high), fmt_p(p))], collapse = " / ")
trend_line <- function(x) sprintf("%s; 추세 차이 p = %s", paste(x[, sprintf("%s %s (p %s)", modifier, ci2(per_level_est, ci_low, ci_high), fmt_p(p_trend))], collapse = " / "), fmt_p(x$p_trend_x_modifier[1]))
tests_md <- function(x) md_tab(x[, .(결과 = outcome, 성별 = sex, 모형 = model, 검정 = test, `모형 n` = big(n_model), F = sprintf("%.2f", F), p = fmt_p(p))])
sparse_line <- function(x, who) {
  y <- x[sparse == TRUE & outcome == oc_bin$lab]
  if (!nrow(y)) return("- 30명 미만이거나 사례 5명 미만인 칸은 없다.")
  sprintf("- 인원이 적은 칸(30명 미만 또는 사례 5명 미만, 해석 주의): %s", paste(unique(y[, sprintf("%s %s·%s n = %d(사례 %d)", get(who), modifier, exposure, n, n_event)]), collapse = "; "))
}
# 보고 누락 방지: p < 0.05인 검정과 보조 지표(추세 차이, RERI)를 모두 찾아 요약 첫머리에 적는다
k5sig  <- k5[p < 0.05]
tr_sig <- function(nm, what) { x <- unique(t5(nm)[p_trend_x_modifier < 0.05, .(outcome, sex, model, p = p_trend_x_modifier)]); if (nrow(x)) x[, `:=`(what = what, detail = "")]; x }
re_sig <- function(nm) {
  x <- t5(nm)[!is.na(reri_p) & reri_p < 0.05, .(outcome, sex, model, p = reri_p, detail = sprintf(" %s·%s %s", modifier, exposure, ci2(reri, reri_lo, reri_hi)))]
  if (nrow(x)) x[, what := "RERI"]; x
}
sec5 <- rbindlist(list(tr_sig("main_trend", "현재 홍조별 추세 차이"), tr_sig("hist_trend", "홍조 이력별 추세 차이(2 df)"), re_sig("main_cells"), re_sig("hist_cells")), use.names = TRUE, fill = TRUE)
sig_line <- if (nrow(k5sig)) paste(k5sig[, sprintf("%s · %s · %s · %s (p %s)", outcome, sex, model, test, fmt_p(p))], collapse = "; ") else "없음"
sec_line <- if (nrow(sec5)) paste(sec5[, sprintf("%s%s · %s · %s · %s (p %s)", what, detail, outcome, sex, model, fmt_p(p))], collapse = "; ") else "없음"
d5 <- res$dist[scheme == S5]; d0 <- res$dist[scheme == S0]
mm <- c("# 절단점 주 5잔·15잔으로 다시 본 주질문·부질문 (사후 분석)", "",
  sprintf("- 음주 수준: 저음주 = %s, 중음주 = %s, 고음주 = %s. 국건영 1잔 ≈ 순알코올 7 g이므로 약 5·15 g/일에 해당한다(근거: output/15_cutpoint_exploration/04_절단점_근거_검토.md).", rng(schemes[[S5]])[1], rng(schemes[[S5]])[2], rng(schemes[[S5]])[3]),
  sprintf("- 절단점 외에는 계획서 분석(code/13)과 같다. 대상 %s명(남 %s·여 %s), 주당 잔수 환산표, 현재 홍조(BD9_2 가끔 이상), 홍조 이력 3군(신규 제외), 고요산혈증(남 ≥7.0, 여 ≥6.0 mg/dL), Crude·Model 1–3.",
          big(nrow(d_sub)), big(d_sub[f_sex == "남자", .N]), big(d_sub[f_sex == "여자", .N])),
  "- 자료: 국민건강영양조사 제8기 2019–2021 기본DB. 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu. n은 비가중 표본 수, %·평균·OR·β는 복합표본 가중 추정치. 검정은 설계 기반 Wald F.",
  "- 변수 근거(제8기 원시자료 이용지침서): 음주 빈도·1회량 BD1_11·BD2_1·BD2_14 PDF p.161-162 / 인쇄 p.153-154, 홍조 BD9_1·BD9_2 PDF p.163-164 / 인쇄 p.155-156, 요산 HE_Uacid PDF p.200 / 인쇄 p.192, 폐경 LW_ms PDF p.190 / 인쇄 p.182.",
  "- **사후 분석이다.** 계획서 절단점(3.5·14잔) 결과를 본 뒤에 바꾼 것이므로, 보고할 때 계획서 기준 결과와 함께 싣고 변경 이유를 적는다. 6절에 두 절단점의 결과를 나란히 적었다.", "",
  "## 0. 핵심 결과", "",
  sprintf("- 주질문(음주 수준 × 현재 홍조, 고요산혈증): Model 2 상호작용 p = 남자 %s, 여자 %s. 네 모형 범위는 남자 %s, 여자 %s.",
          fmt_p(p_main("남자", "Model 2")), fmt_p(p_main("여자", "Model 2")), p_rng(mt5[sex == "남자" & outcome == oc_bin$lab, p]), p_rng(mt5[sex == "여자" & outcome == oc_bin$lab, p])),
  sprintf("- 부질문 1(홍조 이력 3군, 고요산혈증): Model 2 공동 p = 남자 %s, 여자 %s. 소실 vs 비홍조 p = 남자 %s(Model 3 %s), 여자 %s(Model 3 %s).",
          fmt_p(p_hist("남자", "Model 2", "공동")), fmt_p(p_hist("여자", "Model 2", "공동")), fmt_p(p_hist("남자", "Model 2", "소실 vs 비홍조")), fmt_p(p_hist("남자", "Model 3", "소실 vs 비홍조")),
          fmt_p(p_hist("여자", "Model 2", "소실 vs 비홍조")), fmt_p(p_hist("여자", "Model 3", "소실 vs 비홍조"))),
  sprintf("- 부질문 2(성별): 음주 수준 × 현재 홍조 × 성별 3원 상호작용 p = %s(고요산혈증, Model 2), %s(요산, Model 2).",
          fmt_p(st5[model == "Model 2" & outcome == oc_bin$lab & grepl("3원", test), p]), fmt_p(st5[model == "Model 2" & outcome == oc_con$lab & grepl("3원", test), p])),
  sprintf("- 부질문 2(여성 폐경, 탐색): 음주 수준 × 현재 홍조 × 폐경 3원 상호작용 p = %s(고요산혈증, Model 3), %s(요산, Model 3).",
          fmt_p(nt5[model == "Model 3" & outcome == oc_bin$lab & grepl("3원", test), p]), fmt_p(nt5[model == "Model 3" & outcome == oc_con$lab & grepl("3원", test), p])),
  sprintf("- 수행한 상호작용 검정 %d개(두 결과 변수, 네 모형 포함) 가운데 p < 0.05: %s.", nrow(k5), sig_line),
  sprintf("- 검정 외 보조 지표(추세 차이, RERI) 가운데 p < 0.05: %s. 여러 지표를 함께 본 결과이므로 탐색적 소견으로 해석한다.", sec_line), "",
  "## 1. 절단점을 바꾸면 달라지는 인원", "",
  md_tab(d5[, .(성별 = sex, `음주 수준` = drink, 범위 = range, n = n, `가중 %` = sprintf("%.1f", wt_pct), `고요산혈증 사례` = n_hua, `가중 유병률 %` = sprintf("%.1f", hua_pct_wt),
                `가중 평균 요산(mg/dL)` = sprintf("%.2f", uacid_mean_wt), `가중 평균 잔수(잔/주)` = sprintf("%.1f", drinks_wk_mean_wt), `3.5·14 기준 n` = d0$n)]),
  "두 절단점 사이의 이동(비가중 n):", "",
  md_tab(rec[, .(성별 = sex, `3.5·14 기준` = cut_3.5_14, `5·15 기준` = cut_5_15, n = n)]),
  "군이 바뀐 사람의 응답 유형:", "",
  md_tab(moved[, .(성별 = sex, 이동 = paste(from, "→", to), `음주 빈도(BD1_11)` = frequency, `1회 음주량(BD2_1)` = amount, `1회량 대표값(잔)` = sprintf("%.1f", per_occasion), `주당 잔수` = sprintf("%.2f", drinks_wk), n = n)]),
  "## 2. 주질문: 음주 수준 × 현재 홍조", "",
  "### 2-1. 상호작용 검정", "", tests_md(mt5),
  "### 2-2. 홍조군별 고요산혈증 OR (Knol & VanderWeele 형식)", "")
for (s in sexes) for (mdl in c("Model 2", "Model 3")) {
  z <- t5("main_cells")[outcome == oc_bin$lab & sex == s & model == mdl]
  mm <- c(mm, sprintf("**%s · %s** (상호작용 p = %s, n = %s)", s, mdl, fmt_p(p_main(s, mdl)), big(z$n_model[1])), "", md_tab(kv_table(z)),
          sprintf("- OR의 비(홍조군 OR ÷ 비홍조군 OR): %s", term_line(t5("main_terms")[outcome == oc_bin$lab & sex == s & model == mdl])),
          sprintf("- 한 단계당 OR(P for trend): %s", trend_line(t5("main_trend")[outcome == oc_bin$lab & sex == s & model == mdl])), "")
}
mm <- c(mm, sparse_line(t5("main_cells")[model == "Model 2"], "sex"), "",
  "### 2-3. 네 모형의 음주 주효과 OR과 홍조군별 OR", "",
  "주효과 OR은 상호작용항 없이 음주 수준과 홍조를 함께 넣은 모형의 값, 홍조군별 OR은 상호작용 모형의 값이다(기준: 저음주).", "",
  md_tab(t5("main_effects")[, .(성별 = sex, 모형 = model, `음주 수준` = exposure, `주효과 OR (95% CI)` = ci2(main_or, main_lo, main_hi),
                                `비홍조군 OR (95% CI)` = ci2(nonflush_or, nonflush_lo, nonflush_hi), `홍조군 OR (95% CI)` = ci2(flush_or, flush_lo, flush_hi),
                                `홍조 자체의 OR (95% CI)` = ci2(flush_main_or, flush_main_lo, flush_main_hi), `상호작용 p` = fmt_p(p_interaction))]),
  "### 2-4. 혈청 요산 연속형 (β, mg/dL)", "")
for (s in sexes) for (mdl in c("Model 2", "Model 3")) {
  z <- t5("main_cells")[outcome == oc_con$lab & sex == s & model == mdl]
  mm <- c(mm, sprintf("**%s · %s** (상호작용 p = %s)", s, mdl, fmt_p(p_main(s, mdl, oc_con$lab))), "", md_tab(kv_table(z, or = FALSE)),
          sprintf("- β의 차이(홍조군 − 비홍조군): %s", term_line(t5("main_terms")[outcome == oc_con$lab & sex == s & model == mdl])), "")
}
mm <- c(mm, "## 3. 부질문 1: 음주 수준 × 홍조 이력 (비홍조·홍조 소실·지속 홍조, 신규 홍조 제외)", "",
        "### 3-1. 검정 (Model 2·3)", "", tests_md(ht5[model %in% c("Model 2", "Model 3")]),
        "### 3-2. 홍조 이력군별 고요산혈증 OR", "")
for (s in sexes) for (mdl in c("Model 2", "Model 3")) {
  z <- t5("hist_cells")[outcome == oc_bin$lab & sex == s & model == mdl]
  mm <- c(mm, sprintf("**%s · %s** (공동 p = %s, n = %s)", s, mdl, fmt_p(p_hist(s, mdl, "공동")), big(z$n_model[1])), "", md_tab(kv_table(z)),
          sprintf("- OR의 비(해당 군 OR ÷ 비홍조군 OR): %s", term_line(t5("hist_terms")[outcome == oc_bin$lab & sex == s & model == mdl])),
          sprintf("- 한 단계당 OR(P for trend): %s", trend_line(t5("hist_trend")[outcome == oc_bin$lab & sex == s & model == mdl])), "")
}
mm <- c(mm, sparse_line(t5("hist_cells")[model == "Model 2"], "sex"), "",
        "### 3-3. 혈청 요산 연속형 (β, mg/dL), Model 2", "")
for (s in sexes) {
  z <- t5("hist_cells")[outcome == oc_con$lab & sex == s & model == "Model 2"]
  mm <- c(mm, sprintf("**%s · Model 2** (공동 p = %s)", s, fmt_p(p_hist(s, "Model 2", "공동", oc_con$lab))), "", md_tab(kv_table(z, or = FALSE)))
}
g5 <- t5("grp4")
mm <- c(mm, "### 3-4. 보조: 신규 홍조를 포함한 4군 (고요산혈증, 저음주 대비 층화 OR)", "",
        md_tab(g5[exposure != lv3[1], .(성별 = sex, 모형 = model, 홍조패턴 = modifier, `음주 수준` = exposure, n = n, 사례 = n_event,
                                        `층화 OR (95% CI)` = ci2(strat_est, strat_lo, strat_hi), `공동 p(6 df)` = fmt_p(p_interaction_6df))]),
        "## 4. 부질문 2: 성별 차이", "",
        md_tab(st5[, .(결과 = outcome, 모형 = model, 검정 = test, `모형 n` = big(n_model), F = sprintf("%.2f", F), p = fmt_p(p))]),
        "3원 상호작용항(여자의 'OR의 비' ÷ 남자의 'OR의 비', 요산은 β 차이의 차이):", "",
        md_tab(t5("sex_terms")[model %in% c("Model 2", "Model 3"), .(결과 = outcome, 모형 = model, 항 = term_lab(term), `추정치 (95% CI)` = ci2(est, ci_low, ci_high), p = fmt_p(p))]),
        "## 5. 부질문 2(탐색): 여성 폐경 전·후", "",
        md_tab(nt5[, .(결과 = outcome, 구분 = menopause, 모형 = model, 검정 = test, `모형 n` = big(n_model), F = sprintf("%.2f", F), p = fmt_p(p))]))
for (m in levels(dat_of(sdes_sex[["여자"]])$meno)) {
  z <- t5("meno_cells")[outcome == oc_bin$lab & menopause == m & model == "Model 3"]
  mm <- c(mm, sprintf("**%s · Model 3 · 고요산혈증 OR** (상호작용 p = %s, n = %s)", m, fmt_p(nt5[outcome == oc_bin$lab & menopause == m & model == "Model 3", p]), big(z$n_model[1])), "", md_tab(kv_table(z)))
}
mm <- c(mm, sparse_line(t5("meno_cells")[model == "Model 3"], "menopause"), "",
        "## 6. 계획서 절단점(3.5·14잔)과 비교", "",
        "### 6-1. 검정 p (Model 2·3)", "",
        md_tab(cmp_p[model %in% c("Model 2", "Model 3"), .(질문 = question, 결과 = outcome, 대상 = sex, 모형 = model, 검정 = test, `5·15잔 p` = fmt_p(p_5_15), `3.5·14잔 p` = fmt_p(p_3.5_14))]),
        "### 6-2. 홍조군별 고요산혈증 OR (저음주 대비, Model 2)", "",
        md_tab(cmp_or[model == "Model 2", .(성별 = sex, 홍조 = modifier, `음주 수준` = exposure, `5·15: n (사례)` = sprintf("%s (%d)", big(n_5_15), events_5_15),
                                            `5·15: OR (95% CI)` = ci2(or_5_15, lo_5_15, hi_5_15), `3.5·14: n (사례)` = sprintf("%s (%d)", big(n_3.5_14), events_3.5_14),
                                            `3.5·14: OR (95% CI)` = ci2(or_3.5_14, lo_3.5_14, hi_3.5_14))]),
        "## 7. 검증", "",
        "같은 코드로 계획서 절단점을 돌린 결과가 기존 분석(code/13)과, 5·15잔 결과가 앞서 만든 code/18의 3군 결과와 같은지 확인했다.", "",
        md_tab(val[, .(항목 = item, `비교한 값 수` = n_compared, `최대 차이` = sprintf("%.1e", max_abs_diff), 판정 = ifelse(pass, "일치", "불일치"))]),
        "## 8. 가정과 확인하지 못한 것", "",
        "- 절단점 5·15잔은 선행연구의 g/일 기준(5·15 g/일)을 국건영 1잔 ≈ 7 g으로 환산한 것이다. 1잔의 알코올량은 지침서에 없는 문헌 가정이다.",
        "- 고요산혈증 기준(남 ≥7.0, 여 ≥6.0 mg/dL)과 eGFR 식(CKD-EPI 2009)은 지침서에 없는 문헌 기준이다.",
        "- 공식 통계(2019 고위험음주율 조율값) 대조는 code/13과 같이 공식값 출처를 찾지 못해 비어 있다.",
        "- 여성 홍조 소실·고음주, 폐경 후 홍조·고음주 칸은 인원이 적어 신뢰구간이 넓다.")
con <- file(file.path(out5, "SUMMARY_5_15_주질문_부질문.md"), open = "w", encoding = "UTF-8"); writeLines(mm, con); close(con)

cat("\n[검증]\n"); print(val)
cat("\n[5·15잔: 주질문 상호작용 p]\n"); print(mt5[, .(outcome, sex, model, n_model, F = round(F, 2), p = signif(p, 3))])
cat("\n[5·15잔: 부질문 1]\n"); print(ht5[model %in% c("Model 2", "Model 3"), .(outcome, sex, model, test, p = signif(p, 3))], nrows = 100)
cat("\n[5·15잔: 부질문 2 성별]\n"); print(st5[, .(outcome, model, test, p = signif(p, 3))], nrows = 100)
cat("\n[5·15잔: 폐경]\n"); print(nt5[, .(outcome, menopause, model, test, n_model, p = signif(p, 3))], nrows = 100)
cat("\n완료:", out5, "\n")
