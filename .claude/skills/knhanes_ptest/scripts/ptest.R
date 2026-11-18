# ptest.R — 음주량 기준·보정변수·대상을 받아 주질문·부질문의 상호작용 검정 p값을 빠르게 낸다
# 사용(프로젝트 폴더에서):
#   Rscript --vanilla .claude/skills/knhanes_ptest/scripts/ptest.R [옵션]
#   Rscript --vanilla .claude/skills/knhanes_ptest/scripts/ptest.R --batch=기준목록.txt [공통 옵션]
# 옵션(모두 --이름=값):
#   --label=이름             기록에 남길 이름(없으면 자동)
#   --drink=cut|linear|log2|rcs
#                            cut: 절단점 범주(기본) / linear: g/일 연속(--per 단위) / log2: log2(잔/주 + 1), 두 배당
#                            rcs: g/일 제한 삼차 스플라인(매듭 4개 = 성별 5·35·65·95 백분위, 선형항 10 g/일당)
#   --cuts=a,b               남녀 공통 절단점(g/일). a 미만 / a 이상 b 미만 / b 이상
#   --male=a,b --female=c,d  남녀 각각 다른 절단점(g/일, 개수는 같아야 함)
#   --gram=7                 국건영 1잔의 알코올 g(기본 7). g/일 = 주당 잔수 × gram ÷ 7. 7이면 g/일 = 잔/주
#   --per=7                  linear의 단위(g/일). 10이면 10 g/일당, 14.2857이면 100 g/주당
#   --trend=median           범주형에 경향성 검정 추가(성별·범주별 g/일 중앙값을 점수로, 10 g/일당)
#   --covars="Model 2;Model 3"  보정변수 묶음을 ;로 구분. 묶음 안은 +로 잇는다.
#                            프리셋(Crude, Model 1, Model 2, Model 3), 변수 이름, '-변수'(빼기)를 섞을 수 있다.
#                            폐경 상태(meno_cov)는 Crude를 뺀 모든 묶음에 자동으로 들어간다('-meno'로만 제외).
#   --outcome=hua|uacid|both 고요산혈증(기본) / 혈청 요산(mg/dL) / 둘 다
#   --pop=all|premeno|nopreg 대상: 전체(기본) / 여자는 폐경 전만(남자는 전체) / 임신·수유 여성 제외
#   --batch=파일             한 줄에 기준 하나(위 옵션을 공백으로 구분, 공백이 든 값은 "..."로 감쌈, #으로 시작하면 주석).
#                            자료는 한 번만 읽으므로 여러 기준을 빠르게 돌린다. 명령줄의 다른 옵션은 모든 줄에 공통으로 적용된다.
#   --help                   이 설명을 출력
# 결과: 표를 화면에 출력하고 output/21_ptest/ptest_log.csv에 누적 기록한다(시도한 기준을 모두 남기기 위해).
# 근거: 변수 정의·대상 선정·설계는 code/13_v3_main_analysis.R의 0–3절을 그대로 실행해 만든다
#       (음주 BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162, 홍조 BD9_1·BD9_2: p.163-164, 요산 HE_Uacid: p.200,
#        폐경 LW_ms: p.190, 가중치 wt_itvex × 1/3(p.36), 층 kstrata, 집락 psu).

suppressPackageStartupMessages({ library(data.table); library(survey) })
.pt_t0 <- Sys.time()
.all_args <- commandArgs(trailingOnly = TRUE)
if (any(.all_args %in% c("-h", "--help"))) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  h <- readLines(f, encoding = "UTF-8"); cat(sub("^# ?", "", h[seq_len(which(!startsWith(h, "#"))[1] - 1)]), sep = "\n"); quit(status = 0)
}
opt <- function(args, key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", key, "="), "", hit[length(hit)]) else default
}

# ---- 0. 옵션 해석 (자료를 읽기 전에 모든 기준의 잘못된 옵션을 먼저 걸러 낸다) ----
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
numv <- function(x, what) {
  if (is.null(x)) return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
  if (anyNA(v) || any(v <= 0)) stop(what, "는 양수를 쉼표로 이어 주세요: ", x)
  if (anyDuplicated(v)) stop(what, "에 같은 값이 두 번 있습니다: ", x)
  sort(v)
}
parse_opts <- function(args) {
  o <- list()
  stray <- args[!startsWith(args, "--")]
  if (length(stray)) stop("옵션이 아닌 값이 섞여 있습니다: ", paste(stray, collapse = " "),
                          ". 공백이 든 값은 \"--label=이름 공백\"처럼 옵션 전체를 따옴표로 감싸세요.")
  cov_txt <- trimws(strsplit(opt(args, "covars", "Model 2;Model 3"), ";", fixed = TRUE)[[1]]); cov_txt <- cov_txt[nzchar(cov_txt)]
  o$cov_sets <- setNames(lapply(cov_txt, parse_covset), cov_txt)
  o$drink <- tolower(opt(args, "drink", "cut")); if (!o$drink %in% c("cut", "linear", "log2", "rcs")) stop("--drink는 cut, linear, log2, rcs 중 하나")
  o$gram  <- suppressWarnings(as.numeric(opt(args, "gram", "7"))); if (is.na(o$gram) || o$gram <= 0) stop("--gram은 양수(1잔의 g)여야 합니다.")
  o$per   <- suppressWarnings(as.numeric(opt(args, "per", "7")));  if (is.na(o$per) || o$per <= 0) stop("--per는 양수(g/일)여야 합니다.")
  o$trend <- tolower(opt(args, "trend", "none")) %in% c("median", "yes", "true")
  o$cut_m <- numv(opt(args, "male", opt(args, "cuts")), "--male/--cuts"); o$cut_f <- numv(opt(args, "female", opt(args, "cuts")), "--female/--cuts")
  if (o$drink == "cut") {
    if (is.null(o$cut_m) || is.null(o$cut_f)) stop("범주형이면 --cuts=a,b 또는 --male=..와 --female=..를 주세요.")
    if (length(o$cut_m) != length(o$cut_f)) stop("남녀 절단점 개수를 같게 주세요.")
  }
  if (o$trend && o$drink != "cut") stop("--trend는 범주형(--drink=cut)에서만 씁니다.")
  o$outcome <- tolower(opt(args, "outcome", "hua")); if (!o$outcome %in% c("hua", "uacid", "both")) stop("--outcome은 hua, uacid, both 중 하나")
  o$pop <- tolower(opt(args, "pop", "all")); if (!o$pop %in% c("all", "premeno", "nopreg")) stop("--pop은 all, premeno, nopreg 중 하나")
  gtxt <- if (o$gram == 7) "" else sprintf(", 1잔 = %s g", format(o$gram))
  o$desc <- switch(o$drink,
    cut = sprintf("범주: 남 %s / 여 %s g/일%s%s", paste(o$cut_m, collapse = ","), paste(o$cut_f, collapse = ","), gtxt, if (o$trend) " + 범주 중앙값 경향성" else ""),
    linear = sprintf("연속: %s g/일당%s", format(o$per), gtxt), log2 = "연속: log2(잔/주 + 1), 두 배당",
    rcs = sprintf("연속: g/일 제한 삼차 스플라인(매듭 4개 = 성별 5·35·65·95 백분위, 선형항 10 g/일당%s)", gtxt))
  o$label <- opt(args, "label", paste(o$desc, if (o$pop != "all") paste0("[", o$pop, "]") else ""))
  o
}
bf <- opt(.all_args, "batch")
base_args <- .all_args[!grepl("^--batch=", .all_args)]
crit_args <- if (is.null(bf)) list(base_args) else {
  if (!file.exists(bf)) stop("기준 목록 파일이 없습니다: ", bf)
  ln <- trimws(readLines(bf, encoding = "UTF-8", warn = FALSE)); ln <- ln[nzchar(ln) & !startsWith(ln, "#")]
  if (!length(ln)) stop("기준 목록 파일에 기준이 없습니다: ", bf)
  lapply(ln, function(l) c(base_args, scan(text = l, what = "", quiet = TRUE)))
}
crits <- lapply(seq_along(crit_args), function(i) tryCatch(parse_opts(crit_args[[i]]),
  error = function(e) stop(if (length(crit_args) > 1) sprintf("기준 %d번 줄: ", i) else "", conditionMessage(e), call. = FALSE)))

# ---- 1. 본 분석(code/13)의 자료 준비 부분(0–3절)만 한 번 실행: 변수·대상·설계가 본 분석과 같다 ----
.src <- readLines("code/13_v3_main_analysis.R", encoding = "UTF-8")
.stop <- grep("^# ==== 4\\.", .src)[1]; if (is.na(.stop)) stop("code/13의 4절 표시를 찾지 못했습니다.")
fwrite <- function(...) invisible(NULL)                  # 준비 단계가 output/13 파일을 다시 쓰지 않게 막는다
suppressWarnings(eval(parse(text = .src[seq_len(.stop - 1)], encoding = "UTF-8"), envir = globalenv()))
rm(fwrite)
sexes <- c("남자", "여자")
v0 <- sdes$variables
v0$flush_ever <- factor(ifelse(is.na(v0$flush_grp), NA, ifelse(v0$flush_grp == "비홍조", "홍조 없음", "과거 또는 현재 홍조")), c("홍조 없음", "과거 또는 현재 홍조"))
v0$age10 <- cut(v0$age, c(-Inf, 30, 40, 50, 60, 70, Inf), right = FALSE, labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70+"))
# 폐경 상태 보정변수(여자만). LW_ms(8기 PDF p.190 / 인쇄 p.182): 2·3·4 폐경 전, 5·6 폐경 후, 1·7 기타(실제 응답이므로 별도 범주. 분석계획서 4.4와 같음),
# 9 모름·무응답은 code/13에서 이미 결측. 남녀 통합 모형에서는 여자에게만 작용하도록 지시변수(meno_post, meno_other)로 넣는다.
v0$meno_cov <- factor(ifelse(v0$f_sex != "여자", NA, ifelse(v0$lw_ms %in% 2:4, "폐경 전", ifelse(v0$lw_ms %in% 5:6, "폐경 후", ifelse(v0$lw_ms %in% c(1, 7), "기타", NA)))),
                      c("폐경 전", "폐경 후", "기타"))
v0$meno_post  <- ifelse(v0$f_sex == "남자", 0, ifelse(is.na(v0$meno_cov), NA, as.numeric(v0$meno_cov == "폐경 후")))
v0$meno_other <- ifelse(v0$f_sex == "남자", 0, ifelse(is.na(v0$meno_cov), NA, as.numeric(v0$meno_cov == "기타")))
fp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
options(width = 260, datatable.print.nrows = 500, datatable.print.class = FALSE)
MIN_EV <- 10
log_f <- file.path("output", "21_ptest", "ptest_log.csv"); dir.create(dirname(log_f), showWarnings = FALSE, recursive = TRUE)
rcs_basis <- function(x, t) { k <- length(t); pp <- function(z) pmax(z, 0)^3
  sapply(seq_len(k - 2), function(j) (pp(x - t[j]) - pp(x - t[k - 1]) * (t[k] - t[j]) / (t[k] - t[k - 1]) + pp(x - t[k]) * (t[k - 1] - t[j]) / (t[k] - t[k - 1])) / (t[k] - t[1])^2) }

# ---- 2. 기준 하나를 실행 ----
run_criterion <- function(o) {
  drink <- o$drink; pop <- o$pop; cov_sets <- o$cov_sets
  v <- v0
  v$gday <- v$drinks_wk * o$gram / 7                      # g/일. 1잔 = 7 g이면 주당 잔수와 같다
  lv <- NULL; trend_tab <- NULL; knots_tab <- NULL
  if (drink == "cut") {
    k <- length(o$cut_m); lv <- if (k == 1) c("저음주", "고음주") else if (k == 2) c("저음주", "중음주", "고음주") else paste0("수준", seq_len(k + 1))
    v$drk <- factor(NA_character_, lv)
    for (s in sexes) { i <- v$f_sex == s; br <- if (s == "남자") o$cut_m else o$cut_f
      v$drk[i] <- as.character(cut(v$gday[i], c(-Inf, br, Inf), right = FALSE, labels = lv)) }
    if (o$trend) {                                         # 경향성 점수: 성별·범주별 g/일 중앙값(비가중) ÷ 10 → 효과는 10 g/일당
      v$drk_sc <- NA_real_
      for (s in sexes) { i <- v$f_sex == s; md <- tapply(v$gday[i], v$drk[i], median); v$drk_sc[i] <- md[as.character(v$drk[i])] / 10 }
      trend_tab <- unique(data.table(성별 = v$f_sex, 수준 = v$drk, `g/일 중앙값` = round(v$drk_sc * 10, 2)))[order(성별, 수준)]
    }
  } else if (drink == "rcs") {
    # Harrell 제한 삼차 스플라인: 선형항(drk1 = g/일 ÷ 10) + 비선형항 2개(drk2, drk3). 매듭은 성별 5·35·65·95 백분위(비가중)
    v$drk1 <- v$drk2 <- v$drk3 <- NA_real_; knots_tab <- list()
    for (s in sexes) { i <- v$f_sex == s; x <- v$gday[i] / 10; t <- unname(quantile(x, c(0.05, 0.35, 0.65, 0.95)))
      if (any(diff(t) <= 0)) { cat("※ 스플라인 매듭이 겹쳐 이 기준을 건너뜁니다(", s, ").\n"); return(invisible(FALSE)) }
      B <- rcs_basis(x, t); v$drk1[i] <- x; v$drk2[i] <- B[, 1]; v$drk3[i] <- B[, 2]
      knots_tab[[s]] <- data.table(성별 = s, `매듭(g/일)` = paste(round(t * 10, 2), collapse = ", ")) }
    knots_tab <- rbindlist(knots_tab)
  } else v$drk <- if (drink == "linear") v$gday / o$per else log2(v$drinks_wk + 1)
  keep <- switch(pop, all = rep(TRUE, nrow(v)), premeno = v$f_sex == "남자" | (!is.na(v$meno) & v$meno == "폐경 전"), nopreg = v$preg_lact == 0L)
  sd <- sdes; sd$variables <- v; sd <- dsub(sd, keep); d <- dat_of(sd)
  if (drink == "cut") {                                    # 빈 범주가 있으면 모형이 성립하지 않으므로 먼저 알린다
    emp <- d[, .(n = .N), keyby = .(f_sex, drk)][CJ(f_sex = factor(sexes, levels(d$f_sex)), drk = factor(lv, lv)), on = .(f_sex, drk)][is.na(n)]
    if (nrow(emp)) { cat(sprintf("\n※ [%s] 비어 있는 범주가 있어 건너뜁니다: %s. 절단점을 확인하세요.\n", o$label,
                                 paste(emp[, sprintf("%s %s", f_sex, drk)], collapse = ", "))); return(invisible(FALSE)) }
  }

  base_terms <- if (drink == "cut") paste0("drk", lv[-1]) else if (drink == "rcs") c("drk1", "drk2", "drk3") else "drk"
  drk_term   <- if (drink == "rcs") "(drk1 + drk2 + drk3)" else "drk"
  FH <- paste(drk_term, "* flush_hist"); FN <- paste(drk_term, "* flush_now"); FE <- paste(drk_term, "* flush_ever")
  dfl <- length(base_terms)
  lincomb <- function(fit, l, tr) {                        # 효과 크기(OR의 비 또는 β 차이)와 95% CI(t 분포)
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
  # top = TRUE: 수준별 대비 묶음(범주형)이면 가장 높은 음주 수준의 효과(OR의 비)를 함께 낸다
  test <- function(q, lab, who, covname, r, L, y, note_groups = NULL, gvar = NULL, top = FALSE) {
    p <- tryCatch(contrast_test(r$fit, L)$p, error = function(e) NA_real_)
    tr <- if (y == "hua") exp else identity
    eff <- if (length(L) == 1) tryCatch(lincomb(r$fit, L[[1]], tr), error = function(e) "") else
      if (top && drink == "cut") tryCatch(paste0("최고 수준: ", lincomb(r$fit, L[[length(L)]], tr)), error = function(e) "") else ""
    nt <- c(if (!is.null(note_groups) && y == "hua" && min_ev(r$data, gvar, note_groups) < MIN_EV) "사례 10명 미만 칸 있음",
            if (sep_flag(r$fit, unique(unlist(lapply(L, names))))) "분리(추정 불가)")
    rows[[length(rows) + 1L]] <<- data.table(결과 = if (y == "hua") "고요산혈증" else "혈청 요산", 구분 = q, 검정 = lab, 대상 = who, 보정 = covname,
                                            `모형 n` = r$n, df = length(L), p = p, 효과 = eff, 비고 = paste(nt, collapse = ", "))
  }
  one <- function(nm) lapply(nm, function(x) setNames(1, x))

  ys <- switch(o$outcome, hua = "hua", uacid = "uacid", both = c("hua", "uacid"))
  for (y in ys) { fam <- if (y == "hua") quasibinomial() else gaussian()
    for (cn in names(cov_sets)) { covs <- cov_sets[[cn]]
      for (s in sexes) { D <- dsub(sd, d$f_sex == s)
        # 폐경 상태는 여자 모형에만 넣는다(폐경 전 여자만 볼 때는 값이 하나라 뺀다)
        covs_s <- if (s == "남자" || pop == "premeno") setdiff(covs, "meno_cov") else covs
        # 홍조 이력 3군(비홍조 기준·홍조 소실·지속 홍조) 한 모형에서 주질문과 부질문 1을 함께 본다
        rh <- fit_svy(D, mk_form(y, FH, covs_s), fam)
        nh <- function(g) paste0(base_terms, ":flush_hist", g)
        test("주질문", sprintf("지속 홍조 vs 비홍조 (%d df)", dfl), s, cn, rh, one(nh("지속 홍조")), y, c("비홍조", "지속 홍조"), "flush_hist", top = TRUE)
        test("부질문 1", sprintf("홍조 소실 vs 비홍조 (%d df)", dfl), s, cn, rh, one(nh("홍조 소실")), y, c("비홍조", "홍조 소실"), "flush_hist", top = TRUE)
        test("부질문 1(탐색)", sprintf("홍조 소실 vs 지속 홍조 (%d df)", dfl), s, cn, rh,
             Map(function(a, b) setNames(c(1, -1), c(a, b)), nh("홍조 소실"), nh("지속 홍조")), y, c("홍조 소실", "지속 홍조"), "flush_hist", top = TRUE)
        test("참고", sprintf("홍조 이력 세 군 공동 (%d df)", 2 * dfl), s, cn, rh, one(c(nh("홍조 소실"), nh("지속 홍조"))), y, levels(d$flush_hist), "flush_hist")
        r <- fit_svy(D, mk_form(y, FN, covs_s), fam)
        test("참고", sprintf("현재 홍조 vs 현재 비홍조 (%d df, 계획 발표의 주질문)", dfl), s, cn, r, one(paste0(base_terms, ":flush_now홍조")), y, levels(d$flush_now), "flush_now", top = TRUE)
        re <- fit_svy(D, mk_form(y, FE, covs_s), fam)
        test("탐색적 분석", sprintf("홍조 체질(과거 또는 현재) vs 홍조 없음 (%d df)", dfl), s, cn, re, one(paste0(base_terms, ":flush_ever과거 또는 현재 홍조")), y, levels(d$flush_ever), "flush_ever", top = TRUE)
        if (drink == "rcs") {                              # 용량-반응이 직선인가: 홍조 이력을 넣은 주효과 모형에서 비선형항 2개 공동 검정
          rl <- fit_svy(D, mk_form(y, c(drk_term, "flush_hist"), covs_s), fam)
          test("용량-반응", "음주–결과 관계의 비선형성 (스플라인 비선형항, 2 df)", s, cn, rl, one(c("drk2", "drk3")), y)
        }
        if (drink == "cut" && o$trend) {                   # 범주 중앙값 점수로 1 df 경향성 검정(사례 수 경고는 같은 범주 기준)
          rt <- fit_svy(D, mk_form(y, "drk_sc * flush_hist", covs_s), fam)
          test("주질문", "지속 홍조 vs 비홍조, 경향성 (1 df, 범주 중앙값 10 g/일당)", s, cn, rt, one("drk_sc:flush_hist지속 홍조"), y, c("비홍조", "지속 홍조"), "flush_hist")
          test("부질문 1", "홍조 소실 vs 비홍조, 경향성 (1 df, 범주 중앙값 10 g/일당)", s, cn, rt, one("drk_sc:flush_hist홍조 소실"), y, c("비홍조", "홍조 소실"), "flush_hist")
          test("용량-반응", "비홍조군 음주 경향성 (10 g/일당)", s, cn, rt, list(c(drk_sc = 1)), y, "비홍조", "flush_hist")
          test("용량-반응", "지속 홍조군 음주 경향성 (10 g/일당)", s, cn, rt, list(c(drk_sc = 1, "drk_sc:flush_hist지속 홍조" = 1)), y, "지속 홍조", "flush_hist")
          test("용량-반응", "홍조 소실군 음주 경향성 (10 g/일당)", s, cn, rt, list(c(drk_sc = 1, "drk_sc:flush_hist홍조 소실" = 1)), y, "홍조 소실", "flush_hist")
        }
      }
      # 부질문 2: '지속 홍조 vs 비홍조' 차이가 남녀에서 다른가 — 보정변수 효과도 성별마다 다르게 둔 통합 모형의 3원 상호작용
      # 통합 모형: 폐경은 f_sex 상호작용 밖에 여자 전용 지시변수로 넣는다(남자는 0이라 여자에게만 작용)
      covs_p <- setdiff(covs, "meno_cov")
      meno_x <- if ("meno_cov" %in% covs && pop != "premeno") " + meno_post + meno_other" else ""
      r3 <- fit_svy(sd, as.formula(paste0(y, " ~ f_sex * (", paste(c(FH, covs_p), collapse = " + "), ")", meno_x)), fam)
      n3 <- grep("f_sex", grep("^(f_sex.*:)?drk.*:flush_hist지속 홍조", names(coef(r3$fit)), value = TRUE), value = TRUE)
      test("부질문 2", sprintf("지속 홍조 vs 비홍조 차이 × 성별 (3원, %d df)", length(n3)), "남녀 전체", cn, r3, one(n3), y, top = TRUE)
      if (drink == "cut" && o$trend) {
        rt3 <- fit_svy(sd, as.formula(paste0(y, " ~ f_sex * (", paste(c("drk_sc * flush_hist", covs_p), collapse = " + "), ")", meno_x)), fam)
        test("부질문 2", "지속 홍조 vs 비홍조 차이 × 성별, 경향성 (1 df)", "남녀 전체", cn, rt3, one(grep("f_sex.*drk_sc:flush_hist지속 홍조", names(coef(rt3$fit)), value = TRUE)), y)
      }
      r3n <- fit_svy(sd, as.formula(paste0(y, " ~ f_sex * (", paste(c(FN, covs_p), collapse = " + "), ")", meno_x)), fam)
      n3n <- grep("f_sex", grep("drk.*flush_now|flush_now.*drk", names(coef(r3n$fit)), value = TRUE), value = TRUE)
      test("참고", sprintf("현재 홍조 차이 × 성별 (3원, %d df)", length(n3n)), "남녀 전체", cn, r3n, one(n3n), y, top = TRUE)
      if (pop != "premeno") {
        Dw <- dsub(sd, d$f_sex == "여자" & !is.na(d$meno))
        rm3 <- fit_svy(Dw, as.formula(paste(y, "~ meno * (", paste(c(FH, setdiff(covs, "meno_cov")), collapse = " + "), ")")), fam)   # 폐경이 효과수정변수이므로 보정변수에서는 뺀다
        nm3 <- grep("meno", grep("drk.*:flush_hist지속 홍조", names(coef(rm3$fit)), value = TRUE), value = TRUE)
        test("부질문 2(탐색)", sprintf("지속 홍조 vs 비홍조 차이 × 폐경 (3원, %d df)", length(nm3)), "여자", cn, rm3, one(nm3), y, top = TRUE)
      }
    }
  }
  res <- rbindlist(rows)

  # ---- 출력 ----
  cat("\n================ knhanes_ptest ================\n")
  cat("기준:", o$label, "\n음주량:", o$desc, "\n대상:", switch(pop, all = "전체", premeno = "남자 전체 + 폐경 전 여자", nopreg = "임신·수유 여성 제외"), "\n")
  for (cn in names(cov_sets)) cat(sprintf("보정 '%s': %s\n", cn, if (length(cov_sets[[cn]])) paste(cov_sets[[cn]], collapse = " + ") else "(없음)"))
  if (any(sapply(cov_sets, function(x) "meno_cov" %in% x))) cat("※ meno_cov(폐경 상태)는 여자 모형에만 들어가고, 남녀 통합 모형에서는 여자 전용 지시변수로 들어간다.",
                                                           if (pop == "premeno") "폐경 전 여자만 보므로 이번에는 빠진다." else "", "\n")
  if (drink == "cut" && !identical(o$cut_m, o$cut_f)) cat("※ 남녀 절단점이 달라, 부질문 2는 '같은 양'이 아니라 '각 성별 기준 안의 상대적 음주 수준'에서의 남녀 비교다.\n")
  cat("\n[대상 인원]\n")
  if (drink == "cut") {
    wt <- rbindlist(lapply(sexes, function(s) { Ds <- dsub(sd, d$f_sex == s); e <- svymean(~drk, Ds); br <- if (s == "남자") o$cut_m else o$cut_f
      b <- format(br, trim = TRUE); rg <- c(sprintf("%s 미만", b[1]), if (length(b) > 1) sprintf("%s 이상 %s 미만", b[-length(b)], b[-1]), sprintf("%s 이상", b[length(b)]))
      data.table(성별 = s, 수준 = lv, `범위(g/일)` = rg, n = as.integer(table(d[f_sex == s, drk])[lv]), `가중 %` = round(100 * unname(coef(e)[paste0("drk", lv)]), 1)) }))
    print(wt)
    cells <- d[!is.na(flush_grp), .(v = sprintf("%d (%d)", .N, sum(hua))), keyby = .(성별 = f_sex, 홍조패턴 = flush_grp, drk)]
    print(dcast(cells, 성별 + 홍조패턴 ~ drk, value.var = "v"))
    cat("칸 안의 값: 비가중 n (고요산혈증 사례). n은 비가중 표본 수, %는 복합표본 가중 추정치. 신규 홍조는 홍조 이력 분석에서 빠진다.\n")
    if (!is.null(trend_tab)) { cat("\n[경향성 점수: 범주별 g/일 중앙값(비가중)]\n"); print(trend_tab) }
  } else {
    print(d[!is.na(flush_grp), .(n = .N, 고요산혈증 = sum(hua), `g/일 중앙값` = round(median(gday), 2), `g/일 평균` = round(mean(gday), 1), `g/일 최댓값` = round(max(gday), 1)),
            keyby = .(성별 = f_sex, 홍조패턴 = flush_grp)])
    cat("(비가중 요약)\n")
    if (!is.null(knots_tab)) { cat("\n[스플라인 매듭]\n"); print(knots_tab) }
  }
  res[, p표시 := ifelse(grepl("분리", 비고), "무효", fp(p))]   # 분리된 검정의 p는 의미가 없으므로 숫자 대신 '무효'로 보인다
  qorder <- c("주질문", "부질문 1", "부질문 1(탐색)", "부질문 2", "부질문 2(탐색)", "탐색적 분석", "용량-반응", "참고")
  for (yy in unique(res$결과)) {
    cat(sprintf("\n[%s: 상호작용 검정 p]\n", yy))
    w <- dcast(res[결과 == yy], 구분 + 검정 + 대상 ~ 보정, value.var = "p표시")
    nts <- res[결과 == yy & nzchar(비고), .(비고 = paste(unique(비고), collapse = "; ")), by = .(구분, 검정, 대상)]
    w <- merge(w, nts, by = c("구분", "검정", "대상"), all.x = TRUE); w[is.na(비고), 비고 := ""]
    ord <- unique(res[결과 == yy, .(구분, 검정, 대상)])[, o := .I]
    ord[, `:=`(qo = match(구분, qorder), so = match(대상, c("남자", "여자", "남녀 전체")))]
    w <- merge(ord, w, by = c("구분", "검정", "대상"))[order(qo, so, o)][, c("o", "qo", "so") := NULL]
    print(w)
    e1 <- res[결과 == yy & nzchar(효과) & !grepl("분리", 비고), .(구분, 검정, 대상, 보정, 효과)]
    if (nrow(e1)) {
      cat(sprintf("\n[%s: 효과 크기 (%s, 95%% CI)]\n", yy, if (yy == "고요산혈증") "OR의 비 = 해당 군 OR ÷ 비교 군 OR" else "β 차이(mg/dL)"))
      cat(if (drink == "cut") "범주형은 가장 높은 음주 수준에서의 값(저음주 대비)이다.\n" else if (drink == "rcs") "" else sprintf("연속형은 %s당 값이다.\n", if (drink == "log2") "잔수 두 배" else paste(format(o$per), "g/일")))
      print(e1)
    }
  }
  logd <- cbind(run_time = format(Sys.time(), "%Y-%m-%d %H:%M"), 기준 = o$label, 음주량 = o$desc, 대상군 = pop, gram = o$gram,
                보정변수 = sapply(res$보정, function(cn) paste(cov_sets[[cn]], collapse = "+")), res[, !"p표시"])
  logd[, p := signif(p, 3)]
  if (file.exists(log_f)) logd <- rbind(data.table::fread(log_f, encoding = "UTF-8"), logd, fill = TRUE)
  data.table::fwrite(logd, log_f, bom = TRUE)
  invisible(TRUE)
}

ok <- vapply(crits, function(o) isTRUE(run_criterion(o)), logical(1))
cat(sprintf("\n기록: %s  (기준 %d개 중 %d개 실행, 소요 %.0f초)\n", log_f, length(crits), sum(ok), as.numeric(difftime(Sys.time(), .pt_t0, units = "secs"))))
if (!all(ok)) quit(status = 1)
