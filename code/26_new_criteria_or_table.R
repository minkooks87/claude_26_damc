# 26_new_criteria_or_table.R
# 목적: 2026-11-16 개정 연구 질문(README 3절) 기준의 보정 결과표(OR·95% CI·p)를 만든다.
#   - 주질문: 남녀 각각, 성별 기준 음주 수준과 고요산혈증의 관계가 비홍조군과 지속 홍조군 사이에 다른가
#   - 부질문 1: 같은 관계가 홍조 소실군과 비홍조군 사이에 다른가
#   - 음주 수준 절단점은 아직 조원 논의 중이므로, README 4.2의 후보 기준 두 가지를 모두 계산한다(둘 다 '후보').
#   - 보정: Crude → Model 1(연령) → Model 2(기본) → Model 3(추가). 폐경 상태(여자)는 Crude를 뺀 모든 모형에 넣는다
#     (README 4.2, .claude/skills/knhanes_ptest/scripts/ptest.R와 같은 규칙).
# 자료 준비: code/13_v3_main_analysis.R의 0–3절(변수 정의·대상·복합표본설계)을 그대로 실행한다(ptest.R와 같은 방식).
#   → 대상 10,779명(남 5,411·여 5,368), 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu.
#   변수 근거 쪽수: 음주 BD1_11·BD2_1·BD2_14 8기 PDF p.161-162 / 인쇄 p.153-154, 홍조 BD9_1·BD9_2 p.163-164 / 155-156,
#   요산 HE_Uacid p.200 / 192, 폐경 LW_ms p.190 / 182 (code/13의 var_dict와 같음).
# 실행: 프로젝트 폴더에서  Rscript --vanilla code/26_new_criteria_or_table.R
# 출력: output/26_new_criteria_or_table/
# data 폴더는 읽기만 한다.

suppressPackageStartupMessages({ library(data.table); library(survey) })
t0 <- Sys.time()

# ---- 1. 본 분석(code/13)의 자료 준비 부분(0–3절)만 실행 ----
.src  <- readLines("code/13_v3_main_analysis.R", encoding = "UTF-8")
.stop <- grep("^# ==== 4\\.", .src)[1]; if (is.na(.stop)) stop("code/13의 4절 표시를 찾지 못했습니다.")
fwrite <- function(...) invisible(NULL)                  # 준비 단계가 output/13 파일을 다시 쓰지 않게 막는다
suppressWarnings(eval(parse(text = .src[seq_len(.stop - 1)], encoding = "UTF-8"), envir = globalenv()))
rm(fwrite)

out <- file.path("output", "26_new_criteria_or_table"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
sexes <- c("남자", "여자")
v <- sdes$variables
# 폐경 상태(여자만): LW_ms 2·3·4 폐경 전, 5·6 폐경 후, 7 기타, 9는 code/13에서 이미 결측 (ptest.R와 같음)
v$meno_cov <- factor(ifelse(v$f_sex != "여자", NA, ifelse(v$lw_ms %in% 2:4, "폐경 전", ifelse(v$lw_ms %in% 5:6, "폐경 후",
                     ifelse(v$lw_ms %in% 7, "기타", NA)))), c("폐경 전", "폐경 후", "기타"))
stopifnot(identical(levels(v$flush_hist), c("비홍조", "홍조 소실", "지속 홍조")))

# ---- 2. 후보 기준과 모형 ----
criteria <- list(
  "성별 삼분위(후보)"    = list(male = c(2.4, 10), female = c(0.4, 2.5), lv = c("저음주", "중음주", "고음주")),
  "절주 지침 환산(후보)" = list(male = 16,         female = 8,          lv = c("적정 이내", "적정 초과"))
)
models <- list("Crude"   = character(0),
               "Model 1" = c("age", "meno_cov"),
               "Model 2" = c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa", "meno_cov"),
               "Model 3" = c("age", "f_edu", "f_incm", "f_year", "f_smk", "f_pa", "he_bmi", "f_htn", "f_dm", "egfr10", "meno_cov"))

lin <- function(fit, w) {                                 # 계수 선형결합의 추정치·SE·t 분포 p
  b <- coef(fit); V <- vcov(fit); l <- setNames(rep(0, length(b)), names(b)); l[names(w)] <- w
  e <- sum(l * b); se <- sqrt(as.numeric(t(l) %*% V %*% l)); df <- fit$df.residual; q <- qt(0.975, df)
  list(or = exp(e), lo = exp(e - q * se), hi = exp(e + q * se), p = 2 * pt(-abs(e / se), df))
}

or_rows <- list(); test_rows <- list(); cell_rows <- list()
for (cn in names(criteria)) {
  cr <- criteria[[cn]]; lv <- cr$lv
  v$drk <- factor(NA_character_, lv)
  for (s in sexes) { i <- v$f_sex == s; br <- if (s == "남자") cr$male else cr$female
    v$drk[i] <- as.character(cut(v$drinks_wk[i], c(-Inf, br, Inf), right = FALSE, labels = lv)) }
  sd <- sdes; sd$variables <- v; d <- dat_of(sd)
  # 칸별 비가중 n과 고요산혈증 사례 수
  cell_rows[[cn]] <- cbind(criteria = cn, d[!is.na(flush_hist), .(n = .N, cases = sum(hua)), keyby = .(sex = f_sex, flush = flush_hist, drink = drk)])
  for (mn in names(models)) for (s in sexes) {
    covs <- if (s == "남자") setdiff(models[[mn]], "meno_cov") else models[[mn]]
    r <- fit_svy(dsub(sd, d$f_sex == s & !is.na(d$flush_hist)), mk_form("hua", "drk * flush_hist", covs), quasibinomial())
    for (g in levels(d$flush_hist)) for (L in lv[-1]) {
      w <- setNames(1, paste0("drk", L)); if (g != "비홍조") w <- c(w, setNames(1, paste0("drk", L, ":flush_hist", g)))
      e <- lin(r$fit, w)
      or_rows[[length(or_rows) + 1L]] <- data.table(criteria = cn, sex = s, model = mn, n_model = r$n, flush = g, drink = L,
                                                    or = e$or, lo = e$lo, hi = e$hi, p = e$p)
    }
    for (g in c("지속 홍조", "홍조 소실")) {
      L <- lapply(lv[-1], function(x) setNames(1, paste0("drk", x, ":flush_hist", g)))
      tt <- contrast_test(r$fit, L)
      test_rows[[length(test_rows) + 1L]] <- data.table(criteria = cn, sex = s, model = mn, n_model = r$n,
        question = if (g == "지속 홍조") "주질문: 지속 홍조 vs 비홍조" else "부질문 1: 홍조 소실 vs 비홍조", df = tt$df, p = tt$p)
    }
  }
}
or_tab <- rbindlist(or_rows); test_tab <- rbindlist(test_rows); cell_tab <- rbindlist(cell_rows)
data.table::fwrite(or_tab,   file.path(out, "01_or_by_flush_history.csv"), bom = TRUE)
data.table::fwrite(test_tab, file.path(out, "02_interaction_tests.csv"), bom = TRUE)
data.table::fwrite(cell_tab, file.path(out, "03_cell_counts.csv"), bom = TRUE)
data.table::fwrite(data.table(criteria = names(criteria),
                              male = sapply(criteria, function(x) paste(x$male, collapse = ",")),
                              female = sapply(criteria, function(x) paste(x$female, collapse = ",")),
                              levels = sapply(criteria, function(x) paste(x$lv, collapse = " / "))),
                   file.path(out, "00_criteria.csv"), bom = TRUE)
# 재현성 기록
d0 <- dat_of(sdes)
writeLines(c(sprintf("실행: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
             sprintf("대상(비가중): 전체 %d, 남 %d, 여 %d", nrow(d0), sum(d0$f_sex == "남자"), sum(d0$f_sex == "여자")),
             sprintf("홍조 이력 3군 대상(신규 홍조 제외): %d", sum(!is.na(d0$flush_hist))),
             sprintf("입력 자료: %s", paste(files, collapse = ", ")),
             capture.output(sessionInfo())), file.path(out, "_reproducibility.txt"))
cat(sprintf("완료: %s (%.0f초)\n", out, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
print(test_tab[model == "Model 2"])
