# ptest.R — 음주량 기준·보정변수·대상을 받아 주질문·부질문의 상호작용 검정 p값을 빠르게 낸다
# 사용(프로젝트 폴더에서):
#   Rscript --vanilla .claude/skills/knhanes_ptest/scripts/ptest.R [옵션]
# 옵션(모두 --이름=값):
#   --label=이름            기록에 남길 이름(없으면 자동)
#   --drink=cut|linear|log2  cut: 절단점 범주(기본) / linear: 하루 잔수(잔/주 ÷ 7) 1잔당 / log2: log2(잔/주 + 1), 두 배당
#   --cuts=a,b               남녀 공통 절단점(주당 잔수). a 미만 / a 이상 b 미만 / b 이상
#   --male=a,b --female=c,d  남녀 각각 다른 절단점(개수는 같아야 함)
#   --covars="Model 2;Model 3"  보정변수 묶음을 ;로 구분. 묶음 안은 +로 잇는다.
#                            프리셋(Crude, Model 1, Model 2, Model 3), 변수 이름, '-변수'(빼기)를 섞을 수 있다.
#                            예) "Model 2;Model 2+he_bmi;age+f_edu+f_smk3;Model 3-egfr10"
#   --outcome=hua|uacid|both 고요산혈증(기본) / 혈청 요산(mg/dL) / 둘 다
#   --pop=all|premeno|nopreg 대상: 전체(기본) / 여자는 폐경 전만(남자는 전체) / 임신·수유 여성 제외
# 결과: 표를 화면에 출력하고 output/21_ptest/ptest_log.csv에 누적 기록한다(시도한 기준을 모두 남기기 위해).
# 근거: 변수 정의·대상 선정·설계는 code/13_v3_main_analysis.R의 0–3절을 그대로 실행해 만든다
#       (음주 BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162, 홍조 BD9_1·BD9_2: p.163-164, 요산 HE_Uacid: p.200,
#        폐경 LW_ms: p.190, 가중치 wt_itvex × 1/3(p.36), 층 kstrata, 집락 psu).

suppressPackageStartupMessages({ library(data.table); library(survey) })
.pt_args <- commandArgs(trailingOnly = TRUE)
.pt_opt <- function(key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), .pt_args, value = TRUE)
  if (length(hit)) sub(paste0("^--", key, "="), "", hit[length(hit)]) else default
}
.pt_t0 <- Sys.time()

# ---- 0. 옵션 해석 (자료를 읽기 전에 잘못된 옵션을 먼저 걸러 낸다) ----
# 폐경 상태(meno_cov)는 2026-11-16 결정으로 Crude를 뺀 모든 보정 묶음에 기본으로 들어간다(여자에게만 작용).
cov_presets <- list("Crude" = character(0), "Model 1" = c("age", "meno_cov"),
                    "Model 2" = c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa", "meno_cov"),
                    "Model 3" = c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa", "he_bmi", "f_htn", "f_dm", "egfr10", "meno_cov"))
preset_alias <- c("crude" = "Crude", "m0" = "Crude", "model 1" = "Model 1", "model1" = "Model 1", "m1" = "Model 1",
                  "model 2" = "Model 2", "model2" = "Model 2", "m2" = "Model 2", "model 3" = "Model 3", "model3" = "Model 3", "m3" = "Model 3")
cov_avail <- c(age = "연령(세)", age10 = "연령 10세 범주", f_edu = "교육수준", f_incm = "소득 4분위", f_year = "조사연도",
               f_smk = "현재흡연(2범주)", f_smk3 = "흡연 3범주", f_pa = "유산소 신체활동", he_bmi = "체질량지수",
               f_htn = "고혈압 유병(HE_HP)", f_dm = "당뇨병 유병(HE_DM_HbA1c)", f_htn_dx = "고혈압 의사진단",
               f_dm_dx = "당뇨병 의사진단", egfr10 = "eGFR(10 단위)", meno_cov = "폐경 상태(여자만: 폐경 전·폐경 후·기타)")
parse_covset <- function(txt) {
  toks <- trimws(regmatches(txt, gregexpr("[+-]?[^+-]+", txt))[[1]]); toks <- sub("^\\+\\s*", "", toks); toks <- sub("^-\\s*", "-", toks); toks <- toks[nzchar(toks)]
  vars <- character(0); meno_removed <- FALSE
  for (tk in toks) {
    neg <- startsWith(tk, "-"); nm <- trimws(sub("^-", "", tk)); key <- tolower(nm)
    if (key %in% c("meno", "폐경")) nm <- "meno_cov"
    add <- if (key %in% names(preset_alias)) cov_presets[[preset_alias[[key]]]] else if (nm %in% names(cov_avail)) nm else
      stop("알 수 없는 보정변수: '", nm, "'. 쓸 수 있는 것: ", paste(c(names(cov_presets), names(cov_avail)), collapse = ", "))
    if (neg && "meno_cov" %in% add) meno_removed <- TRUE
    vars <- if (neg) setdiff(vars, add) else union(vars, add)
  }
  # 보정이 있는 묶음에는 폐경 상태를 항상 넣는다('-meno'로 명시적으로 뺀 경우만 제외). Crude(보정 없음)는 그대로 둔다.
  if (length(vars) && !meno_removed) vars <- union(vars, "meno_cov")
  vars
}
cov_txt  <- trimws(strsplit(.pt_opt("covars", "Model 2;Model 3"), ";", fixed = TRUE)[[1]]); cov_txt <- cov_txt[nzchar(cov_txt)]
cov_sets <- setNames(lapply(cov_txt, parse_covset), cov_txt)
drink    <- tolower(.pt_opt("drink", "cut")); if (!drink %in% c("cut", "linear", "log2", "rcs")) stop("--drink는 cut, linear, log2, rcs 중 하나")
per      <- as.numeric(.pt_opt("per", "7"))              # linear의 단위(g/일). 1잔 ≈ 7 g이라 잔/주 = g/일. 기본 7 = 하루 1잔
if (is.na(per) || per <= 0) stop("--per는 양수(g/일)여야 합니다.")
trend    <- tolower(.pt_opt("trend", "none")) %in% c("median", "yes", "true")   # 범주 중앙값 점수 경향성 검정(범주형일 때)
numv     <- function(x) if (is.null(x)) NULL else sort(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
cut_m    <- numv(.pt_opt("male", .pt_opt("cuts"))); cut_f <- numv(.pt_opt("female", .pt_opt("cuts")))
if (drink == "cut") {
  if (is.null(cut_m) || is.null(cut_f)) stop("범주형이면 --cuts=a,b 또는 --male=..와 --female=..를 주세요.")
  if (anyNA(c(cut_m, cut_f)) || length(cut_m) != length(cut_f)) stop("절단점은 숫자로, 남녀 개수를 같게 주세요.")
}
outcome  <- tolower(.pt_opt("outcome", "hua")); if (!outcome %in% c("hua", "uacid", "both")) stop("--outcome은 hua, uacid, both 중 하나")
pop      <- tolower(.pt_opt("pop", "all"));     if (!pop %in% c("all", "premeno", "nopreg")) stop("--pop은 all, premeno, nopreg 중 하나")
drink_desc <- switch(drink, cut = sprintf("범주: 남 %s / 여 %s 잔/주(= g/일)%s", paste(cut_m, collapse = ","), paste(cut_f, collapse = ","), if (trend) " + 범주 중앙값 경향성" else ""),
                     linear = sprintf("연속: %s g/일당(잔/주 = g/일, 1잔 ≈ 7 g)", format(per)), log2 = "연속: log2(잔/주 + 1), 두 배당",
                     rcs = "연속: g/일 제한 삼차 스플라인(매듭 4개, 성별 5·35·65·95 백분위; 선형항 단위 10 g/일)")
label <- .pt_opt("label", paste(drink_desc, if (pop != "all") paste0("[", pop, "]") else ""))

# ---- 1. 본 분석(code/13)의 자료 준비 부분(0–3절)만 실행: 변수·대상·설계가 본 분석과 같다 ----
.src <- readLines("code/13_v3_main_analysis.R", encoding = "UTF-8")
.stop <- grep("^# ==== 4\\.", .src)[1]; if (is.na(.stop)) stop("code/13의 4절 표시를 찾지 못했습니다.")
fwrite <- function(...) invisible(NULL)                  # 준비 단계가 output/13 파일을 다시 쓰지 않게 막는다
suppressWarnings(eval(parse(text = .src[seq_len(.stop - 1)], encoding = "UTF-8"), envir = globalenv()))
rm(fwrite)
sexes <- c("남자", "여자")

v <- sdes$variables
v$flush_ever <- factor(ifelse(is.na(v$flush_grp), NA, ifelse(v$flush_grp == "비홍조", "홍조 없음", "과거 또는 현재 홍조")), c("홍조 없음", "과거 또는 현재 홍조"))
v$age10 <- cut(v$age, c(-Inf, 30, 40, 50, 60, 70, Inf), right = FALSE, labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70+"))
# 폐경 상태 보정변수(여자만). LW_ms(8기 PDF p.190 / 인쇄 p.182): 2·3·4 폐경 전, 5·6 폐경 후, 7 기타(실제 응답이므로 별도 범주),
# 9 모름·무응답은 code/13에서 이미 결측. 남녀 통합 모형에서는 여자에게만 작용하도록 지시변수(meno_post, meno_other)로 넣는다.
v$meno_cov <- factor(ifelse(v$f_sex != "여자", NA, ifelse(v$lw_ms %in% 2:4, "폐경 전", ifelse(v$lw_ms %in% 5:6, "폐경 후", ifelse(v$lw_ms %in% 7, "기타", NA)))),
                     c("폐경 전", "폐경 후", "기타"))
v$meno_post  <- ifelse(v$f_sex == "남자", 0, ifelse(is.na(v$meno_cov), NA, as.numeric(v$meno_cov == "폐경 후")))
v$meno_other <- ifelse(v$f_sex == "남자", 0, ifelse(is.na(v$meno_cov), NA, as.numeric(v$meno_cov == "기타")))
lv <- NULL
if (drink == "cut") {
  k <- length(cut_m); lv <- if (k == 1) c("저음주", "고음주") else if (k == 2) c("저음주", "중음주", "고음주") else paste0("수준", seq_len(k + 1))
  v$drk <- factor(NA_character_, lv)
  for (s in sexes) { i <- v$f_sex == s; br <- if (s == "남자") cut_m else cut_f
    v$drk[i] <- as.character(cut(v$drinks_wk[i], c(-Inf, br, Inf), right = FALSE, labels = lv)) }
  if (trend) {                                           # 경향성 점수: 성별·범주별 g/일 중앙값(비가중) ÷ 10 → 효과는 10 g/일당
    v$drk_sc <- NA_real_
    for (s in sexes) { i <- v$f_sex == s; md <- tapply(v$drinks_wk[i], v$drk[i], median); v$drk_sc[i] <- md[as.character(v$drk[i])] / 10 }
    trend_tab <- unique(data.table(sex = v$f_sex, level = v$drk, median_g_day = v$drk_sc * 10))[order(sex, level)]
  }
} else if (drink == "rcs") {
  # Harrell 제한 삼차 스플라인: 선형항(drk1 = g/일 ÷ 10) + 비선형항 2개(drk2, drk3). 매듭은 성별 5·35·65·95 백분위(비가중)
  rcs_basis <- function(x, t) { k <- length(t); pp <- function(z) pmax(z, 0)^3
    sapply(seq_len(k - 2), function(j) (pp(x - t[j]) - pp(x - t[k - 1]) * (t[k] - t[j]) / (t[k] - t[k - 1]) + pp(x - t[k]) * (t[k - 1] - t[j]) / (t[k] - t[k - 1])) / (t[k] - t[1])^2) }
  v$drk1 <- v$drk2 <- v$drk3 <- NA_real_; knots_tab <- list()
  for (s in sexes) { i <- v$f_sex == s; x <- v$drinks_wk[i] / 10; t <- unname(quantile(x, c(0.05, 0.35, 0.65, 0.95)))
    if (any(diff(t) <= 0)) stop("스플라인 매듭이 겹칩니다(", s, ").")
    B <- rcs_basis(x, t); v$drk1[i] <- x; v$drk2[i] <- B[, 1]; v$drk3[i] <- B[, 2]
    knots_tab[[s]] <- data.table(sex = s, knots_g_day = paste(round(t * 10, 2), collapse = ", ")) }
  knots_tab <- rbindlist(knots_tab)
} else v$drk <- if (drink == "linear") v$drinks_wk / per else log2(v$drinks_wk + 1)
keep <- switch(pop, all = rep(TRUE, nrow(v)), premeno = v$f_sex == "남자" | (!is.na(v$meno) & v$meno == "폐경 전"), nopreg = v$preg_lact == 0L)
sd <- sdes; sd$variables <- v; sd <- dsub(sd, keep); d <- dat_of(sd)

# ---- 2. 검정 도구 ----
MIN_EV <- 10
base_terms <- if (drink == "cut") paste0("drk", lv[-1]) else if (drink == "rcs") c("drk1", "drk2", "drk3") else "drk"
drk_term   <- if (drink == "rcs") "(drk1 + drk2 + drk3)" else "drk"
FH <- paste(drk_term, "* flush_hist"); FN <- paste(drk_term, "* flush_now"); FE <- paste(drk_term, "* flush_ever")
lincomb <- function(fit, l, tr) {                         # 1 df 검정의 효과 크기(OR의 비 또는 β 차이)와 95% CI(t 분포)
  b <- coef(fit); V <- vcov(fit); w <- setNames(rep(0, length(b)), names(b)); w[names(l)] <- l
  e <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w)); q <- qt(0.975, fit$df.residual)
  sprintf("%.2f (%.2f, %.2f)", tr(e), tr(e - q * se), tr(e + q * se))
}
sep_flag <- function(fit, nm) {                          # 검정하는 계수가 추정되지 않았거나(분리) 비정상적으로 크면 표시
  b <- coef(fit)[nm]; if (!all(nm %in% names(coef(fit))) || any(is.na(b))) return(TRUE)
  # 계수 크기 기준(|b| > 8)은 범주형 칸이 비어 생기는 분리를 잡기 위한 것. 연속형(특히 스플라인 비선형항)은 계수 척도가 달라 쓰지 않는다
  drink == "cut" && max(abs(b)) > 8 }
min_ev <- function(dat, gvar, groups) {
  if (drink != "cut") return(Inf)
  g <- CJ(grp = groups, lev = lv); e <- dat[get(gvar) %in% groups, .(ev = sum(hua)), keyby = .(grp = get(gvar), lev = drk)]
  m <- merge(g, e, by = c("grp", "lev"), all.x = TRUE); m[is.na(ev), ev := 0]; min(m$ev)
}
rows <- list()
test <- function(q, lab, who, covname, r, L, y, note_groups = NULL, gvar = NULL) {
  p <- tryCatch(contrast_test(r$fit, L)$p, error = function(e) NA_real_)
  eff <- if (length(L) == 1) tryCatch(lincomb(r$fit, L[[1]], if (y == "hua") exp else identity), error = function(e) "") else ""
  nt <- c(if (!is.null(note_groups) && y == "hua" && min_ev(r$data, gvar, note_groups) < MIN_EV) "사례 10명 미만 칸 있음",
          if (sep_flag(r$fit, unique(unlist(lapply(L, names))))) "분리(추정 불가)")
  rows[[length(rows) + 1L]] <<- data.table(결과 = if (y == "hua") "고요산혈증" else "혈청 요산", 구분 = q, 검정 = lab, 대상 = who, 보정 = covname,
                                          `모형 n` = r$n, df = length(L), p = p, `효과(1 df)` = eff, 비고 = paste(nt, collapse = ", "))
}
one <- function(nm) lapply(nm, function(x) setNames(1, x))
dfl <- length(base_terms)

ys <- switch(outcome, hua = "hua", uacid = "uacid", both = c("hua", "uacid"))
for (y in ys) { fam <- if (y == "hua") quasibinomial() else gaussian()
  for (cn in names(cov_sets)) { covs <- cov_sets[[cn]]
    for (s in sexes) { D <- dsub(sd, d$f_sex == s)
      # 폐경 상태는 여자 모형에만 넣는다(폐경 전 여자만 볼 때는 값이 하나라 뺀다)
      covs_s <- if (s == "남자" || pop == "premeno") setdiff(covs, "meno_cov") else covs
      # 홍조 이력 3군(비홍조 기준·홍조 소실·지속 홍조) 한 모형에서 주질문과 부질문 1을 함께 본다
      rh <- fit_svy(D, mk_form(y, FH, covs_s), fam)
      nh <- function(g) paste0(base_terms, ":flush_hist", g)
      test("주질문", sprintf("지속 홍조 vs 비홍조 (%d df)", dfl), s, cn, rh, one(nh("지속 홍조")), y, c("비홍조", "지속 홍조"), "flush_hist")
      test("부질문 1", sprintf("홍조 소실 vs 비홍조 (%d df)", dfl), s, cn, rh, one(nh("홍조 소실")), y, c("비홍조", "홍조 소실"), "flush_hist")
      test("부질문 1(탐색)", sprintf("홍조 소실 vs 지속 홍조 (%d df)", dfl), s, cn, rh,
           Map(function(a, b) setNames(c(1, -1), c(a, b)), nh("홍조 소실"), nh("지속 홍조")), y, c("홍조 소실", "지속 홍조"), "flush_hist")
      test("참고", sprintf("홍조 이력 세 군 공동 (%d df)", 2 * dfl), s, cn, rh, one(c(nh("홍조 소실"), nh("지속 홍조"))), y, levels(d$flush_hist), "flush_hist")
      r <- fit_svy(D, mk_form(y, FN, covs_s), fam)
      test("참고", sprintf("현재 홍조 vs 현재 비홍조 (%d df, 계획 발표의 주질문)", dfl), s, cn, r, one(paste0(base_terms, ":flush_now홍조")), y, levels(d$flush_now), "flush_now")
      re <- fit_svy(D, mk_form(y, FE, covs_s), fam)
      test("탐색적 분석", sprintf("홍조 체질(과거 또는 현재) vs 홍조 없음 (%d df)", dfl), s, cn, re, one(paste0(base_terms, ":flush_ever과거 또는 현재 홍조")), y, levels(d$flush_ever), "flush_ever")
      if (drink == "rcs") {                              # 용량-반응이 직선인가: 홍조 이력을 넣은 주효과 모형에서 비선형항 2개 공동 검정
        rl <- fit_svy(D, mk_form(y, c(drk_term, "flush_hist"), covs_s), fam)
        test("용량-반응", "음주–결과 관계의 비선형성 (스플라인 비선형항, 2 df)", s, cn, rl, one(c("drk2", "drk3")), y)
      }
      if (drink == "cut" && trend) {                     # 범주 중앙값 점수로 1 df 경향성 검정
        rt <- fit_svy(D, mk_form(y, "drk_sc * flush_hist", covs_s), fam)
        test("주질문", "지속 홍조 vs 비홍조, 경향성 (1 df, 범주 중앙값 10 g/일당)", s, cn, rt, one("drk_sc:flush_hist지속 홍조"), y)
        test("부질문 1", "홍조 소실 vs 비홍조, 경향성 (1 df, 범주 중앙값 10 g/일당)", s, cn, rt, one("drk_sc:flush_hist홍조 소실"), y)
        test("용량-반응", "비홍조군 음주 경향성 (10 g/일당)", s, cn, rt, list(c(drk_sc = 1)), y)
        test("용량-반응", "지속 홍조군 음주 경향성 (10 g/일당)", s, cn, rt, list(c(drk_sc = 1, "drk_sc:flush_hist지속 홍조" = 1)), y)
        test("용량-반응", "홍조 소실군 음주 경향성 (10 g/일당)", s, cn, rt, list(c(drk_sc = 1, "drk_sc:flush_hist홍조 소실" = 1)), y)
      }
    }
    # 부질문 2: '지속 홍조 vs 비홍조' 차이가 남녀에서 다른가 — 보정변수 효과도 성별마다 다르게 둔 통합 모형의 3원 상호작용
    # 통합 모형: 폐경은 f_sex 상호작용 밖에 여자 전용 지시변수로 넣는다(남자는 0이라 여자에게만 작용)
    covs_p <- setdiff(covs, "meno_cov")
    meno_x <- if ("meno_cov" %in% covs && pop != "premeno") " + meno_post + meno_other" else ""
    r3 <- fit_svy(sd, as.formula(paste0(y, " ~ f_sex * (", paste(c(FH, covs_p), collapse = " + "), ")", meno_x)), fam)
    n3 <- grep("f_sex", grep("^(f_sex.*:)?drk.*:flush_hist지속 홍조", names(coef(r3$fit)), value = TRUE), value = TRUE)
    test("부질문 2", sprintf("지속 홍조 vs 비홍조 차이 × 성별 (3원, %d df)", length(n3)), "남녀 전체", cn, r3, one(n3), y)
    if (drink == "cut" && trend) {
      rt3 <- fit_svy(sd, as.formula(paste0(y, " ~ f_sex * (", paste(c("drk_sc * flush_hist", covs_p), collapse = " + "), ")", meno_x)), fam)
      test("부질문 2", "지속 홍조 vs 비홍조 차이 × 성별, 경향성 (1 df)", "남녀 전체", cn, rt3, one(grep("f_sex.*drk_sc:flush_hist지속 홍조", names(coef(rt3$fit)), value = TRUE)), y)
    }
    r3n <- fit_svy(sd, as.formula(paste0(y, " ~ f_sex * (", paste(c(FN, covs_p), collapse = " + "), ")", meno_x)), fam)
    n3n <- grep("f_sex", grep("drk.*flush_now|flush_now.*drk", names(coef(r3n$fit)), value = TRUE), value = TRUE)
    test("참고", sprintf("현재 홍조 차이 × 성별 (3원, %d df)", length(n3n)), "남녀 전체", cn, r3n, one(n3n), y)
    if (pop != "premeno") {
      Dw <- dsub(sd, d$f_sex == "여자" & !is.na(d$meno))
      rm3 <- fit_svy(Dw, as.formula(paste(y, "~ meno * (", paste(c(FH, setdiff(covs, "meno_cov")), collapse = " + "), ")")), fam)   # 폐경이 효과수정변수이므로 보정변수에서는 뺀다
      nm3 <- grep("meno", grep("drk.*:flush_hist지속 홍조", names(coef(rm3$fit)), value = TRUE), value = TRUE)
      test("부질문 2(탐색)", sprintf("지속 홍조 vs 비홍조 차이 × 폐경 (3원, %d df)", length(nm3)), "여자", cn, rm3, one(nm3), y)
    }
  }
}
res <- rbindlist(rows)

# ---- 3. 출력과 기록 ----
fp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
options(width = 260, datatable.print.nrows = 500, datatable.print.class = FALSE)
cat("\n================ knhanes_ptest ================\n")
cat("기준:", label, "\n음주량:", drink_desc, "\n대상:", switch(pop, all = "전체", premeno = "남자 전체 + 폐경 전 여자", nopreg = "임신·수유 여성 제외"), "\n")
for (cn in names(cov_sets)) cat(sprintf("보정 '%s': %s\n", cn, if (length(cov_sets[[cn]])) paste(cov_sets[[cn]], collapse = " + ") else "(없음)"))
if (any(sapply(cov_sets, function(x) "meno_cov" %in% x))) cat("※ meno_cov(폐경 상태)는 여자 모형에만 들어가고, 남녀 통합 모형에서는 여자 전용 지시변수로 들어간다.",
                                                         if (pop == "premeno") "폐경 전 여자만 보므로 이번에는 빠진다." else "", "\n")
cat("\n[대상 인원]\n")
if (drink == "cut") {
  cells <- d[, .(v = sprintf("%d (%d)", .N, sum(hua))), keyby = .(성별 = f_sex, 현재홍조 = flush_now, drk)]
  print(dcast(cells, 성별 + 현재홍조 ~ drk, value.var = "v"))
  cat("칸 안의 값: 비가중 n (고요산혈증 사례)\n")
  if (trend) { cat("\n[경향성 점수: 범주별 g/일 중앙값(비가중)]\n"); print(trend_tab) }
} else {
  print(d[, .(n = .N, 고요산혈증 = sum(hua), `잔/주 중앙값` = median(drinks_wk), `잔/주 평균` = round(mean(drinks_wk), 1), `잔/주 최댓값` = max(drinks_wk)), keyby = .(성별 = f_sex, 현재홍조 = flush_now)])
  cat("(비가중 요약)\n")
  if (drink == "rcs") { cat("\n[스플라인 매듭(g/일)]\n"); print(knots_tab) }
}
for (y in unique(res$결과)) {
  cat(sprintf("\n[%s: 상호작용 검정 p]\n", y))
  w <- dcast(res[결과 == y], 구분 + 검정 + 대상 ~ 보정, value.var = "p")
  for (cn in names(cov_sets)) w[[cn]] <- fp(w[[cn]])
  nts <- res[결과 == y & nzchar(비고), .(비고 = paste(unique(비고), collapse = "; ")), by = .(구분, 검정, 대상)]
  w <- merge(w, nts, by = c("구분", "검정", "대상"), all.x = TRUE); w[is.na(비고), 비고 := ""]
  qorder <- c("주질문", "부질문 1", "부질문 1(탐색)", "부질문 2", "부질문 2(탐색)", "탐색적 분석", "용량-반응", "참고")
  ord <- unique(res[결과 == y, .(구분, 검정, 대상)])[, o := .I]
  ord[, `:=`(qo = match(구분, qorder), so = match(대상, c("남자", "여자", "남녀 전체")))]
  w <- merge(ord, w, by = c("구분", "검정", "대상"))[order(qo, so, o)][, c("o", "qo", "so") := NULL]
  print(w)
  e1 <- res[결과 == y & df == 1 & nzchar(`효과(1 df)`), .(구분, 검정, 대상, 보정, `효과(1 df)`)]
  if (nrow(e1)) { cat(sprintf("\n[%s: 1 df 검정의 효과 크기 (%s, 95%% CI)]\n", y, if (y == "고요산혈증") "OR의 비" else "β 차이(mg/dL)")); print(e1) }
}
out <- file.path("output", "21_ptest"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
log_f <- file.path(out, "ptest_log.csv")
logd <- cbind(run_time = format(Sys.time(), "%Y-%m-%d %H:%M"), 기준 = label, 음주량 = drink_desc, 대상군 = pop,
              보정변수 = sapply(res$보정, function(cn) paste(cov_sets[[cn]], collapse = "+")), res)
logd[, p := signif(p, 3)]
if (file.exists(log_f)) logd <- rbind(data.table::fread(log_f, encoding = "UTF-8"), logd, fill = TRUE)
data.table::fwrite(logd, log_f, bom = TRUE)
cat(sprintf("\n기록: %s  (소요 %.0f초)\n", log_f, as.numeric(difftime(Sys.time(), .pt_t0, units = "secs"))))
