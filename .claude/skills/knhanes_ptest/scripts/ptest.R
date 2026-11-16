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
cov_presets <- list("Crude" = character(0), "Model 1" = "age",
                    "Model 2" = c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa"),
                    "Model 3" = c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa", "he_bmi", "f_htn", "f_dm", "egfr10"))
preset_alias <- c("crude" = "Crude", "m0" = "Crude", "model 1" = "Model 1", "model1" = "Model 1", "m1" = "Model 1",
                  "model 2" = "Model 2", "model2" = "Model 2", "m2" = "Model 2", "model 3" = "Model 3", "model3" = "Model 3", "m3" = "Model 3")
cov_avail <- c(age = "연령(세)", age10 = "연령 10세 범주", f_edu = "교육수준", f_incm = "소득 4분위", f_year = "조사연도",
               f_smk = "현재흡연(2범주)", f_smk3 = "흡연 3범주", f_pa = "유산소 신체활동", he_bmi = "체질량지수",
               f_htn = "고혈압 유병(HE_HP)", f_dm = "당뇨병 유병(HE_DM_HbA1c)", f_htn_dx = "고혈압 의사진단",
               f_dm_dx = "당뇨병 의사진단", egfr10 = "eGFR(10 단위)")
parse_covset <- function(txt) {
  toks <- trimws(regmatches(txt, gregexpr("[+-]?[^+-]+", txt))[[1]]); toks <- sub("^\\+\\s*", "", toks); toks <- sub("^-\\s*", "-", toks); toks <- toks[nzchar(toks)]
  vars <- character(0)
  for (tk in toks) {
    neg <- startsWith(tk, "-"); nm <- trimws(sub("^-", "", tk)); key <- tolower(nm)
    add <- if (key %in% names(preset_alias)) cov_presets[[preset_alias[[key]]]] else if (nm %in% names(cov_avail)) nm else
      stop("알 수 없는 보정변수: '", nm, "'. 쓸 수 있는 것: ", paste(c(names(cov_presets), names(cov_avail)), collapse = ", "))
    vars <- if (neg) setdiff(vars, add) else union(vars, add)
  }
  vars
}
cov_txt  <- trimws(strsplit(.pt_opt("covars", "Model 2;Model 3"), ";", fixed = TRUE)[[1]]); cov_txt <- cov_txt[nzchar(cov_txt)]
cov_sets <- setNames(lapply(cov_txt, parse_covset), cov_txt)
drink    <- tolower(.pt_opt("drink", "cut")); if (!drink %in% c("cut", "linear", "log2")) stop("--drink는 cut, linear, log2 중 하나")
numv     <- function(x) if (is.null(x)) NULL else sort(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
cut_m    <- numv(.pt_opt("male", .pt_opt("cuts"))); cut_f <- numv(.pt_opt("female", .pt_opt("cuts")))
if (drink == "cut") {
  if (is.null(cut_m) || is.null(cut_f)) stop("범주형이면 --cuts=a,b 또는 --male=..와 --female=..를 주세요.")
  if (anyNA(c(cut_m, cut_f)) || length(cut_m) != length(cut_f)) stop("절단점은 숫자로, 남녀 개수를 같게 주세요.")
}
outcome  <- tolower(.pt_opt("outcome", "hua")); if (!outcome %in% c("hua", "uacid", "both")) stop("--outcome은 hua, uacid, both 중 하나")
pop      <- tolower(.pt_opt("pop", "all"));     if (!pop %in% c("all", "premeno", "nopreg")) stop("--pop은 all, premeno, nopreg 중 하나")
drink_desc <- switch(drink, cut = sprintf("범주: 남 %s / 여 %s 잔/주", paste(cut_m, collapse = ","), paste(cut_f, collapse = ",")),
                     linear = "연속: 하루 1잔당(잔/주 ÷ 7)", log2 = "연속: log2(잔/주 + 1), 두 배당")
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
lv <- NULL
if (drink == "cut") {
  k <- length(cut_m); lv <- if (k == 1) c("저음주", "고음주") else if (k == 2) c("저음주", "중음주", "고음주") else paste0("수준", seq_len(k + 1))
  v$drk <- factor(NA_character_, lv)
  for (s in sexes) { i <- v$f_sex == s; br <- if (s == "남자") cut_m else cut_f
    v$drk[i] <- as.character(cut(v$drinks_wk[i], c(-Inf, br, Inf), right = FALSE, labels = lv)) }
} else v$drk <- if (drink == "linear") v$drinks_wk / 7 else log2(v$drinks_wk + 1)
keep <- switch(pop, all = rep(TRUE, nrow(v)), premeno = v$f_sex == "남자" | (!is.na(v$meno) & v$meno == "폐경 전"), nopreg = v$preg_lact == 0L)
sd <- sdes; sd$variables <- v; sd <- dsub(sd, keep); d <- dat_of(sd)

# ---- 2. 검정 도구 ----
MIN_EV <- 10
base_terms <- if (drink == "cut") paste0("drk", lv[-1]) else "drk"
lincomb <- function(fit, l, tr) {                         # 1 df 검정의 효과 크기(OR의 비 또는 β 차이)와 95% CI(t 분포)
  b <- coef(fit); V <- vcov(fit); w <- setNames(rep(0, length(b)), names(b)); w[names(l)] <- l
  e <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w)); q <- qt(0.975, fit$df.residual)
  sprintf("%.2f (%.2f, %.2f)", tr(e), tr(e - q * se), tr(e + q * se))
}
sep_flag <- function(fit, nm) {                          # 검정하는 계수가 추정되지 않았거나(분리) 비정상적으로 크면 표시
  b <- coef(fit)[nm]; any(is.na(b)) || !all(nm %in% names(coef(fit))) || max(abs(b)) > 8 }
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
      r <- fit_svy(D, mk_form(y, "drk * flush_now", covs), fam)
      test("주질문", sprintf("음주 × 현재 홍조 (%d df)", dfl), s, cn, r, one(paste0(base_terms, ":flush_now홍조")), y, levels(d$flush_now), "flush_now")
      rh <- fit_svy(D, mk_form(y, "drk * flush_hist", covs), fam)
      nh <- function(g) paste0(base_terms, ":flush_hist", g)
      test("부질문 1", sprintf("음주 × 홍조 이력, 세 군 공동 (%d df)", 2 * dfl), s, cn, rh, one(c(nh("홍조 소실"), nh("지속 홍조"))), y, levels(d$flush_hist), "flush_hist")
      test("부질문 1", sprintf("소실 vs 비홍조 (%d df, 핵심 대비)", dfl), s, cn, rh, one(nh("홍조 소실")), y, c("비홍조", "홍조 소실"), "flush_hist")
      test("부질문 1", sprintf("지속 vs 비홍조 (%d df)", dfl), s, cn, rh, one(nh("지속 홍조")), y, c("비홍조", "지속 홍조"), "flush_hist")
      test("부질문 1", sprintf("소실 vs 지속 (%d df, 탐색)", dfl), s, cn, rh,
           Map(function(a, b) setNames(c(1, -1), c(a, b)), nh("홍조 소실"), nh("지속 홍조")), y, c("홍조 소실", "지속 홍조"), "flush_hist")
      re <- fit_svy(D, mk_form(y, "drk * flush_ever", covs), fam)
      test("탐색적 분석", sprintf("음주 × 홍조 체질(과거 또는 현재) (%d df)", dfl), s, cn, re, one(paste0(base_terms, ":flush_ever과거 또는 현재 홍조")), y, levels(d$flush_ever), "flush_ever")
    }
    # 부질문 2: 보정변수 효과도 성별마다 다르게 둔 통합 모형의 3원 상호작용(남녀 층화 모형과 대응)
    r3 <- fit_svy(sd, as.formula(paste(y, "~ f_sex * (", paste(c("drk * flush_now", covs), collapse = " + "), ")")), fam)
    n3 <- grep("f_sex", grep("drk.*flush_now|flush_now.*drk", names(coef(r3$fit)), value = TRUE), value = TRUE)
    test("부질문 2", sprintf("음주 × 현재 홍조 × 성별 (3원, %d df)", length(n3)), "남녀 전체", cn, r3, one(n3), y)
    if (pop != "premeno") {
      Dw <- dsub(sd, d$f_sex == "여자" & !is.na(d$meno))
      rm3 <- fit_svy(Dw, as.formula(paste(y, "~ meno * (", paste(c("drk * flush_now", covs), collapse = " + "), ")")), fam)
      nm3 <- grep("meno", grep("drk.*flush_now|flush_now.*drk", names(coef(rm3$fit)), value = TRUE), value = TRUE)
      test("부질문 2(탐색)", sprintf("음주 × 현재 홍조 × 폐경 (3원, %d df)", length(nm3)), "여자", cn, rm3, one(nm3), y)
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
cat("\n[대상 인원]\n")
if (drink == "cut") {
  cells <- d[, .(v = sprintf("%d (%d)", .N, sum(hua))), keyby = .(성별 = f_sex, 현재홍조 = flush_now, drk)]
  print(dcast(cells, 성별 + 현재홍조 ~ drk, value.var = "v"))
  cat("칸 안의 값: 비가중 n (고요산혈증 사례)\n")
} else {
  print(d[, .(n = .N, 고요산혈증 = sum(hua), `잔/주 중앙값` = median(drinks_wk), `잔/주 평균` = round(mean(drinks_wk), 1), `잔/주 최댓값` = max(drinks_wk)), keyby = .(성별 = f_sex, 현재홍조 = flush_now)])
  cat("(비가중 요약)\n")
}
for (y in unique(res$결과)) {
  cat(sprintf("\n[%s: 상호작용 검정 p]\n", y))
  w <- dcast(res[결과 == y], 구분 + 검정 + 대상 ~ 보정, value.var = "p")
  ord <- unique(res[결과 == y, .(구분, 검정, 대상)])[, o := .I]
  w <- merge(ord, w, by = c("구분", "검정", "대상"))[order(o)][, o := NULL]
  for (cn in names(cov_sets)) w[[cn]] <- fp(w[[cn]])
  nts <- res[결과 == y & nzchar(비고), .(비고 = paste(unique(비고), collapse = "; ")), by = .(구분, 검정, 대상)]
  w <- merge(w, nts, by = c("구분", "검정", "대상"), all.x = TRUE, sort = FALSE); w[is.na(비고), 비고 := ""]
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
