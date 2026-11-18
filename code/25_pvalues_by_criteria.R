# 25_pvalues_by_criteria.R
# 목적: 음주 수준 절단점(남녀 각각 다르게 줄 수 있음)을 넣으면 주질문·부질문의 상호작용 검정 p값을 표로 낸다.
#       결과 변수는 고요산혈증(남 7.0, 여 6.0 mg/dL 이상), 모형은 Model 2(주 모형)와 Model 3(민감도).
# 사용: Rscript --vanilla code/25_pvalues_by_criteria.R "이름|남자 절단점|여자 절단점" ["이름|..|.." ...]
#   예) Rscript --vanilla code/25_pvalues_by_criteria.R "5·15잔|5,15|5,15" "성별 삼분위|2.4,10|0.4,2.5"
#   절단점은 주당 잔수. 'a,b'면 a 미만 / a 이상 b 미만 / b 이상의 3군. 남녀 절단점 개수는 같아야 한다.
# 기록: 시도한 모든 기준과 결과를 output/20_pvalues_by_criteria/00_criteria_log.csv에 누적한다(같은 이름이면 덮어씀).
#       여러 기준을 시도한 뒤 p가 가장 작은 기준만 고르면 사후 선택이 되므로, 보고서에는 시도한 기준을 모두 밝힌다.
# 변수·대상·설계: code/13_v3_main_analysis.R과 같다(음주 BD1_11·BD2_1·BD2_14: 8기 PDF p.161-162, 홍조 BD9_1·BD9_2: p.163-164,
#                 요산 HE_Uacid: p.200, 폐경 LW_ms: p.190, 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu).

suppressPackageStartupMessages({ library(data.table); library(survey) })
.crit_args <- commandArgs(trailingOnly = TRUE)
if (!length(.crit_args)) stop("기준을 넣어 주세요. 예: Rscript --vanilla code/25_pvalues_by_criteria.R \"5·15잔|5,15|5,15\"")
.parse_set <- function(s) {
  p <- trimws(strsplit(s, "|", fixed = TRUE)[[1]])
  if (length(p) != 3) stop("형식은 \"이름|남자 절단점|여자 절단점\"입니다: ", s)
  num <- function(x) sort(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
  m <- num(p[2]); f <- num(p[3])
  if (anyNA(m) || anyNA(f) || length(m) != length(f) || !length(m)) stop("절단점을 확인하세요(남녀 개수 같게, 숫자만): ", s)
  list(label = p[1], male = m, female = f)
}
.crit_sets <- lapply(.crit_args, .parse_set)

source("code/13_v3_main_analysis.R")   # 자료·대상(inc)·설계(sdes)·함수(fit_svy, term_test, contrast_test, mk_form, models4)

out <- file.path("output", "20_pvalues_by_criteria")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
M23 <- c("Model 2", "Model 3")
MIN_EV <- 10                                          # 비교하는 칸의 고요산혈증 사례가 이보다 적으면 해석하지 않음(code/22와 같은 기준)
safe_p <- function(expr) tryCatch(expr, error = function(e) NA_real_)
lvl_names <- function(k) if (k == 1) c("저음주", "고음주") else if (k == 2) c("저음주", "중음주", "고음주") else paste0("수준", seq_len(k + 1))
rng_txt <- function(br) { b <- format(br, trim = TRUE)
  c(sprintf("%s 미만", b[1]), if (length(b) > 1) sprintf("%s 이상 %s 미만", b[-length(b)], b[-1]), sprintf("%s 이상", b[length(b)])) }
sep_flag <- function(fit) { b <- coef(fit); any(is.na(b)) || max(abs(b[names(b) != "(Intercept)"]), na.rm = TRUE) > 8 }
min_events <- function(dat, groups, lv) {            # 비교하는 군 × 음주 수준 칸의 최소 사례 수(빈 칸은 0)
  g <- CJ(grp = groups, lev = lv); e <- dat[get("grp_var") %in% groups, .(ev = sum(hua)), keyby = .(grp = grp_var, lev = drinkX)]
  m <- merge(g, e, by = c("grp", "lev"), all.x = TRUE); m[is.na(ev), ev := 0]; min(m$ev)
}

run_set <- function(cs) {
  k <- length(cs$male); lv <- lvl_names(k); nonref <- lv[-1]
  v <- sdes$variables
  v$drinkX <- factor(NA_character_, lv)
  for (s in sexes) { br <- if (s == "남자") cs$male else cs$female; i <- v$f_sex == s
    v$drinkX[i] <- as.character(cut(v$drinks_wk[i], c(-Inf, br, Inf), right = FALSE, labels = lv)) }
  v$flush_ever <- factor(ifelse(is.na(v$flush_grp), NA, ifelse(v$flush_grp == "비홍조", "홍조 없음", "과거 또는 현재 홍조")), c("홍조 없음", "과거 또는 현재 홍조"))
  sd <- sdes; sd$variables <- v; d <- dat_of(sd)
  D <- lapply(setNames(sexes, sexes), function(s) dsub(sd, d$f_sex == s))

  # (1) 군별 인원과 칸별 사례
  cells <- rbindlist(lapply(sexes, function(s) { ds <- D[[s]]; dd <- dat_of(ds); est <- svymean(~drinkX, ds)
    base <- data.table(criterion = cs$label, sex = s, level = lv, range = rng_txt(if (s == "남자") cs$male else cs$female),
                       n = as.integer(table(dd$drinkX)[lv]), wt_pct = round(100 * unname(coef(est)[paste0("drinkX", lv)]), 1))
    fl <- dcast(dd[, .(v = sprintf("%d (%d)", .N, sum(hua))), keyby = .(drinkX, flush_now)], drinkX ~ flush_now, value.var = "v")
    lo <- dd[flush_hist == "홍조 소실", .(lost = sprintf("%d (%d)", .N, sum(hua))), keyby = drinkX]
    m <- merge(merge(base, fl, by.x = "level", by.y = "drinkX", all.x = TRUE), lo, by.x = "level", by.y = "drinkX", all.x = TRUE)
    setnames(m, c("비홍조", "홍조", "lost"), c("비홍조 n (사례)", "홍조 n (사례)", "홍조 소실 n (사례)"), skip_absent = TRUE)
    m[order(match(level, lv))] }))

  # (2) 검정
  rows <- list(); add <- function(...) rows[[length(rows) + 1L]] <<- data.table(...)
  for (mdl in M23) { covs <- models4[[mdl]]
    for (s in sexes) {
      r <- fit_svy(D[[s]], mk_form("hua", "drinkX * flush_now", covs), quasibinomial())
      dd <- copy(r$data); dd[, grp_var := flush_now]
      add(구분 = "주질문", 검정 = sprintf("음주 수준 × 현재 홍조 (%d df)", k), 대상 = s, model = mdl, p = safe_p(term_test(r$fit, "drinkX:flush_now")$p),
          비고 = paste(c(if (min_events(dd, levels(dd$flush_now), lv) < MIN_EV) "사례 10명 미만 칸 있음", if (sep_flag(r$fit)) "분리(추정 불가)"), collapse = ", "))
      rh <- fit_svy(D[[s]], mk_form("hua", "drinkX * flush_hist", covs), quasibinomial()); f <- rh$fit
      dh <- copy(rh$data); dh[, grp_var := flush_hist]
      nm <- function(e, g) paste0("drinkX", e, ":flush_hist", g)
      note <- function(groups) paste(c(if (min_events(dh, groups, lv) < MIN_EV) "사례 10명 미만 칸 있음", if (sep_flag(f)) "분리(추정 불가)"), collapse = ", ")
      add(구분 = "부질문 1", 검정 = sprintf("음주 수준 × 홍조 이력, 세 군 공동 (%d df)", 2 * k), 대상 = s, model = mdl, p = safe_p(term_test(f, "drinkX:flush_hist")$p), 비고 = note(levels(dh$flush_hist)))
      add(구분 = "부질문 1", 검정 = sprintf("소실 vs 비홍조 (%d df, 핵심 대비)", k), 대상 = s, model = mdl,
          p = safe_p(contrast_test(f, lapply(nonref, function(e) setNames(1, nm(e, "홍조 소실"))))$p), 비고 = note(c("비홍조", "홍조 소실")))
      add(구분 = "부질문 1", 검정 = sprintf("지속 vs 비홍조 (%d df)", k), 대상 = s, model = mdl,
          p = safe_p(contrast_test(f, lapply(nonref, function(e) setNames(1, nm(e, "지속 홍조"))))$p), 비고 = note(c("비홍조", "지속 홍조")))
      add(구분 = "부질문 1", 검정 = sprintf("소실 vs 지속 (%d df, 탐색)", k), 대상 = s, model = mdl,
          p = safe_p(contrast_test(f, lapply(nonref, function(e) setNames(c(1, -1), c(nm(e, "홍조 소실"), nm(e, "지속 홍조")))))$p), 비고 = note(c("홍조 소실", "지속 홍조")))
      re <- fit_svy(D[[s]], mk_form("hua", "drinkX * flush_ever", covs), quasibinomial())
      de <- copy(re$data); de[, grp_var := flush_ever]
      add(구분 = "탐색적 분석", 검정 = sprintf("음주 수준 × 홍조 체질(과거 또는 현재) (%d df)", k), 대상 = s, model = mdl, p = safe_p(term_test(re$fit, "drinkX:flush_ever")$p),
          비고 = paste(c(if (min_events(de, levels(de$flush_ever), lv) < MIN_EV) "사례 10명 미만 칸 있음", if (sep_flag(re$fit)) "분리(추정 불가)"), collapse = ", "))
    }
    # 부질문 2: 보정변수 효과도 성별마다 다르게 둔 통합 모형의 3원 상호작용(남녀 층화 모형과 대응)
    r3 <- fit_svy(sd, as.formula(paste("hua ~ f_sex * (", paste(c("drinkX * flush_now", covs), collapse = " + "), ")")), quasibinomial())
    n3 <- grep("f_sex", grep("drinkX.*flush_now|flush_now.*drinkX", names(coef(r3$fit)), value = TRUE), value = TRUE)
    add(구분 = "부질문 2", 검정 = sprintf("음주 수준 × 현재 홍조 × 성별 (3원, %d df)", length(n3)), 대상 = "남녀 전체", model = mdl,
        p = safe_p(contrast_test(r3$fit, lapply(n3, function(x) setNames(1, x)))$p), 비고 = if (sep_flag(r3$fit)) "분리(추정 불가)" else "")
    dw <- dat_of(D[["여자"]]); Dwm <- dsub(D[["여자"]], !is.na(dw$meno))
    rm3 <- fit_svy(Dwm, as.formula(paste("hua ~ meno * (", paste(c("drinkX * flush_now", covs), collapse = " + "), ")")), quasibinomial())
    nm3 <- grep("meno", grep("drinkX.*flush_now|flush_now.*drinkX", names(coef(rm3$fit)), value = TRUE), value = TRUE)
    add(구분 = "부질문 2(탐색)", 검정 = sprintf("음주 수준 × 현재 홍조 × 폐경 (3원, %d df)", length(nm3)), 대상 = "여자", model = mdl,
        p = safe_p(contrast_test(rm3$fit, lapply(nm3, function(x) setNames(1, x)))$p), 비고 = if (sep_flag(rm3$fit)) "분리(추정 불가)" else "")
  }
  tt <- rbindlist(rows)
  wide <- dcast(tt, 구분 + 검정 + 대상 ~ model, value.var = c("p", "비고"))
  ord <- c("주질문", "부질문 1", "부질문 2", "부질문 2(탐색)", "탐색적 분석")
  wide <- wide[order(match(구분, ord), match(대상, c("남자", "여자", "남녀 전체")), match(검정, unique(tt$검정)))]
  wide[, 비고 := ifelse(`비고_Model 2` == "", `비고_Model 3`, `비고_Model 2`)][, c("비고_Model 2", "비고_Model 3") := NULL]
  setnames(wide, c("p_Model 2", "p_Model 3"), c("p_Model2", "p_Model3"))
  res <- cbind(criterion = cs$label, male_cuts = paste(cs$male, collapse = ","), female_cuts = paste(cs$female, collapse = ","), wide)
  list(res = res, cells = cells)
}

all <- lapply(.crit_sets, run_set)
res <- rbindlist(lapply(all, `[[`, "res")); cells <- rbindlist(lapply(all, `[[`, "cells"), fill = TRUE)
res[, run_date := format(Sys.Date())]

# 누적 기록(같은 이름의 기준은 새 결과로 바꾼다)
log_f <- file.path(out, "00_criteria_log.csv"); cell_f <- file.path(out, "01_criteria_cells_log.csv")
upd <- function(f, new) { if (file.exists(f)) { old <- fread(f, encoding = "UTF-8"); old <- old[!criterion %in% unique(new$criterion)]; new <- rbind(old, new, fill = TRUE) }
  fwrite(new, f, bom = TRUE) }
res_out <- copy(res); res_out[, `:=`(p_Model2 = signif(p_Model2, 3), p_Model3 = signif(p_Model3, 3))]
upd(log_f, res_out); upd(cell_f, cells)

fp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
options(width = 250)
for (cs in .crit_sets) {
  cat(sprintf("\n\n==== 기준: %s (남자 %s / 여자 %s 잔/주) ====\n", cs$label, paste(cs$male, collapse = ", "), paste(cs$female, collapse = ", ")))
  print(cells[criterion == cs$label, !"criterion"])
  print(res[criterion == cs$label, .(구분, 검정, 대상, `p (Model 2)` = fp(p_Model2), `p (Model 3)` = fp(p_Model3), 비고)])
}
cat("\n기록:", log_f, "\n")
