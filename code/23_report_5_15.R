# 23_report_5_15.R
# 목적: code/22_main_5_15_precise.R의 결과 객체(_results.rds)로 결과 보고서(markdown)를 만든다. 수치는 모두 객체에서 가져온다(손입력 없음).
# 실행: Rscript --vanilla code/23_report_5_15.R   (22번 실행 뒤, 프로젝트 폴더에서)
# 결과: output/18_main_5_15/REPORT_5_15_정밀분석.md  → docx는 code/06_flushing_report_docx.py, pdf는 code/16_report_md_to_pdf.py로 변환

suppressPackageStartupMessages(library(data.table))
out <- file.path("output", "18_main_5_15")
o <- readRDS(file.path(out, "_results.rds")); R <- o$R
HUA <- "고요산혈증(OR)"; UA <- "혈청 요산(β, mg/dL)"
lv3 <- c("저음주", "중음주", "고음주"); sexes <- c("남자", "여자"); M23 <- c("Model 2", "Model 3")

# ---- 표기 도구 ----
P   <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
ci  <- function(e, l, h, d = 2) { f <- paste0("%.", d, "f")
  ifelse(is.na(e), "", ifelse(l < 0 | h < 0, sprintf(paste0(f, " (", f, ", ", f, ")"), e, l, h), sprintf(paste0(f, " (", f, "–", f, ")"), e, l, h))) }
big <- function(x) format(x, big.mark = ",", trim = TRUE)
f1  <- function(x) sprintf("%.1f", x); f2 <- function(x) sprintf("%.2f", x)
tab <- function(dt) { x <- as.data.table(lapply(dt, function(v) ifelse(is.na(v), "", as.character(v))))
  c(paste0("| ", paste(names(x), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|"),
    apply(as.matrix(x), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")), "") }
kv <- function(z, or = TRUE, g_lab = "홍조") {      # Knol & VanderWeele 형식의 칸 표
  e1 <- z$exposure == z$exposure[1]; m1 <- z$modifier == z$modifier[1]
  rf <- function(e, l, h, isref) ifelse(isref, "기준", ci(e, l, h))
  x <- data.table(g = z$modifier, 음주 = z$exposure, n = big(z$n)); setnames(x, "g", g_lab)
  if (or) { x[, `사례` := z$n_event]; x[, `가중 유병률 %` := f1(100 * z$wt_mean)] } else x[, `가중 평균(mg/dL)` := f2(z$wt_mean)]
  x[, `결합효과(기준: 첫 군·저음주)` := rf(z$joint_est, z$joint_lo, z$joint_hi, e1 & m1)]
  x[, `같은 군 안에서 저음주 대비` := rf(z$strat_est, z$strat_lo, z$strat_hi, e1)]
  x[, `같은 음주 수준에서 첫 군 대비` := rf(z$mod_est, z$mod_lo, z$mod_hi, m1)]
  x[, (if (or) "OR의 비 (p)" else "β의 차이 (p)") := ifelse(is.na(z$ratio_est), "", sprintf("%s (p %s)", ci(z$ratio_est, z$ratio_lo, z$ratio_hi), P(z$ratio_p)))]
  if (or) x[, RERI := ci(z$reri, z$reri_lo, z$reri_hi)]
  x
}
g1 <- function(x) { stopifnot(nrow(x) == 1); x }          # 한 행만 뽑혔는지 확인
mc  <- function(s, mdl, oc = HUA) R$main_cells[outcome == oc & sex == s & model == mdl]
mt  <- function(s, mdl, oc = HUA) g1(R$main_tests[outcome == oc & sex == s & model == mdl])
hi_main <- function(s, mdl, grp, oc = HUA) g1(mc(s, mdl, oc)[modifier == grp & exposure == "고음주"])
hc  <- function(s, mdl, con, oc = HUA) g1(R$hist_contrasts[outcome == oc & sex == s & model == mdl & contrast == con])
ht  <- function(s, mdl, oc = HUA) g1(R$hist_tests[outcome == oc & sex == s & model == mdl])
ec  <- function(s, mdl, oc = HUA) R$ever_cells[outcome == oc & sex == s & model == mdl]
et  <- function(s, mdl, oc = HUA) g1(R$ever_tests[outcome == oc & sex == s & model == mdl])
hi_ever <- function(s, mdl, grp) g1(ec(s, mdl)[modifier == grp & exposure == "고음주"])
etr <- function(s, mdl) R$ever_trend[outcome == HUA & sex == s & model == mdl]
st3 <- function(mdl, oc = HUA, key = "층화") g1(R$sex_tests[outcome == oc & model == mdl & grepl("3원", test) & grepl(key, covariates)])
sterm <- function(mdl, lev, oc = HUA, key = "층화") g1(R$sex_terms[outcome == oc & model == mdl & grepl(key, covariates) & grepl(lev, term, fixed = TRUE)])
prange <- function(p) sprintf("%s–%s", P(min(p)), P(max(p)))

# ---- p < 0.05 목록(보고 누락 방지): 공동 검정과 보조 지표를 모두 훑는다 ----
S <- function(y, lab, who, item, p) if (nrow(y)) data.table(구분 = lab, 결과 = y$outcome, 대상 = who, 모형 = y$model, 항목 = item, p = p) else NULL   # 해당 행이 없으면 NULL
sig <- rbindlist(list(
  { y <- R$main_tests[p < 0.05]; S(y, "주질문", y$sex, y$test, y$p) },
  { y <- R$main_cells[!is.na(ratio_p) & ratio_p < 0.05]; S(y, "주질문, 보조", y$sex, paste0("OR의 비 또는 β 차이: ", y$exposure), y$ratio_p) },
  { y <- unique(R$main_trend[p_trend_x_modifier < 0.05, .(outcome, sex, model, p_trend_x_modifier)]); S(y, "주질문, 보조", y$sex, "추세 차이(1 df)", y$p_trend_x_modifier) },
  { y <- R$main_cells[!is.na(reri_p) & reri_p < 0.05]; S(y, "주질문, 보조", y$sex, paste0("RERI: ", y$exposure), y$reri_p) },
  { y <- R$hist_tests[p < 0.05]; S(y, "부질문 1(3군)", y$sex, y$test, y$p) },
  { y <- R$hist_contrasts[p_wald < 0.05]; S(y, "부질문 1(3군)", y$sex, paste(y$contrast, "(2 df)"), y$p_wald) },
  { y <- R$hist_contrasts[p_trend_diff < 0.05]; S(y, "부질문 1(3군), 보조", y$sex, paste(y$contrast, "추세 차이(1 df)"), y$p_trend_diff) },
  { y <- R$hist_cells[!is.na(reri_p) & reri_p < 0.05]; S(y, "부질문 1(3군), 보조", y$sex, paste0("RERI: ", y$modifier, "·", y$exposure), y$reri_p) },
  { y <- R$nonflush_tests[p < 0.05]; S(y, "부질문 1 보조(현재 비홍조자)", y$sex, y$test, y$p) },
  { y <- R$ever_tests[p < 0.05]; S(y, "탐색적 분석(2문항 분류)", y$sex, y$test, y$p) },
  { y <- R$ever_cells[!is.na(ratio_p) & ratio_p < 0.05]; S(y, "탐색적 분석(2문항 분류)", y$sex, paste0("OR의 비 또는 β 차이: ", y$exposure), y$ratio_p) },
  { y <- unique(R$ever_trend[p_trend_x_modifier < 0.05, .(outcome, sex, model, p_trend_x_modifier)]); S(y, "탐색적 분석(2문항 분류)", y$sex, "추세 차이(1 df)", y$p_trend_x_modifier) },
  { y <- R$ever_cells[!is.na(reri_p) & reri_p < 0.05]; S(y, "탐색적 분석(2문항 분류)", y$sex, paste0("RERI: ", y$exposure), y$reri_p) },
  { y <- unique(R$grp4_cells[p_wald_6df < 0.05, .(outcome, sex, model, p_wald_6df)]); S(y, "부질문 1 보조(4군)", y$sex, "공동(6 df)", y$p_wald_6df) },
  { y <- R$sex_tests[p < 0.05]; S(y, "부질문 2", "남녀 전체", paste0(y$test, " [보정변수 효과: ", y$covariates, "]"), y$p) },
  { y <- R$meno_tests[p < 0.05]; S(y, "부질문 2(폐경, 탐색)", paste0("여자: ", y$menopause), y$test, y$p) }), use.names = TRUE)
if (!nrow(sig)) sig <- data.table(구분 = character(0), 결과 = character(0), 대상 = character(0), 모형 = character(0), 항목 = character(0), p = numeric(0))
pm_hi <- g1(R$meno_cells[outcome == HUA & menopause == "폐경 후" & model == "Model 2" & modifier == "홍조" & exposure == "고음주"])   # 폐경 후 홍조·고음주 칸
wl_hi <- g1(R$hist_cells[outcome == HUA & sex == "여자" & model == "Model 2" & modifier == "홍조 소실" & exposure == "고음주"])      # 여자 홍조 소실·고음주 칸
n_tests <- c(주질문 = nrow(R$main_tests), `부질문 1(3군)` = nrow(R$hist_tests) + nrow(R$hist_contrasts), `부질문 1 보조` = nrow(R$nonflush_tests) + nrow(unique(R$grp4_cells[, .(sex, model)])),
             `부질문 2` = nrow(R$sex_tests), 폐경 = nrow(R$meno_tests), `탐색적 분석(2문항 분류)` = nrow(R$ever_tests))
mt3 <- function(key, mdl = "Model 2") g1(R$meno_tests[outcome == HUA & model == mdl & grepl(key, test, fixed = TRUE) & grepl("전체", menopause)])   # 폐경 3원 검정 한 행

# 보고서는 구역별 버퍼에 쌓은 뒤 마지막에 순서대로 이어 붙인다(탐색적 분석을 부질문 1 뒤의 독립된 절로 싣기 위해)
buf <- list(); SEC <- "a"; A <- function(...) buf[[SEC]] <<- c(buf[[SEC]], ...)
# ============================== 0. 요약 ==============================
m2m <- hi_main("남자", "Model 2", "홍조"); m2w <- hi_main("여자", "Model 2", "홍조")
A("# 음주 수준 주 5잔·15잔 기준 정밀 분석: 주질문과 부질문", "",
  "음주 관련 안면홍조 이력에 따른 음주량–고요산혈증 연관성의 차이(성별 효과수정), 국민건강영양조사 제8기 2019–2021", "",
  "- 생성: code/22_main_5_15_precise.R(분석) → code/23_report_5_15.R(이 문서). 수치는 모두 결과 객체에서 가져왔다. 원자료 표는 같은 폴더의 CSV.",
  "- 음주 수준: 저음주 = 주 5잔 미만, 중음주 = 주 5잔 이상 15잔 미만, 고음주 = 주 15잔 이상(남녀 공통, 저음주가 기준). 국건영 1잔을 순알코올 약 7 g으로 보면 약 5·15 g/일이다.",
  "- **사후 분석이다.** 계획서 절단점(3.5·14잔)의 결과를 본 뒤 절단점을 바꿨다. 9절에 계획서 기준과 다른 절단점의 결과를 나란히 실었다.",
  "- 과거 또는 현재 홍조로 묶은 2문항 분류는 계획서에 없던 분석이므로 6절에 **탐색적 분석**으로 따로 실었다.", "",
  "## 0. 결론 요약", "",
  sprintf("1. **주질문(음주 수준 × 현재 홍조): 효과수정의 근거가 없다.** Model 2의 상호작용 검정 p는 남자 %s, 여자 %s이고, 네 모형 전체에서 남자 %s, 여자 %s이다. 고음주에서 홍조군 OR ÷ 비홍조군 OR은 남자 %s, 여자 %s이다.",
          P(mt("남자", "Model 2")$p), P(mt("여자", "Model 2")$p), prange(R$main_tests[outcome == HUA & sex == "남자", p]), prange(R$main_tests[outcome == HUA & sex == "여자", p]),
          ci(m2m$ratio_est, m2m$ratio_lo, m2m$ratio_hi), ci(m2w$ratio_est, m2w$ratio_lo, m2w$ratio_hi)),
  sprintf("2. **'차이가 없다'가 아니라 '확인되지 않았다'이다.** 이 자료가 검정력 80%%로 검출할 수 있는 OR의 비는 남자 %.1f배, 여자 %.1f배 이상이다. 그보다 작은 차이는 있어도 드러나지 않는다.", m2m$ratio_mde80, m2w$ratio_mde80),
  sprintf("3. **부질문 1(계획서의 3군 분석): 남자에서 방향은 가설과 같지만 유의하지 않다.** 소실 vs 비홍조 대비의 p는 Model 2 %s(Holm 보정 %s), Model 3 %s이다. 고음주에서 소실군 OR ÷ 비홍조군 OR은 %s(Model 2)이다. 여자는 소실군의 고요산혈증 사례가 너무 적어 검정을 해석할 수 없다.",
          P(hc("남자", "Model 2", "소실 vs 비홍조")$p_wald), P(hc("남자", "Model 2", "소실 vs 비홍조")$p_holm), P(hc("남자", "Model 3", "소실 vs 비홍조")$p_wald),
          ci(hc("남자", "Model 2", "소실 vs 비홍조")$high_est, hc("남자", "Model 2", "소실 vs 비홍조")$high_lo, hc("남자", "Model 2", "소실 vs 비홍조")$high_hi)),
  sprintf("4. **탐색적 분석(과거 또는 현재 홍조를 '홍조 체질'로 묶는 2문항 분류, 6절): 남자에서 가장 뚜렷한 신호가 나왔으나 확증이 아니다.** 고음주의 OR은 홍조 체질 %s, 홍조 없음 %s이고 OR의 비는 %s(p %s)이다. 공동 검정 p는 Model 2 %s, Model 3 %s, 추세 차이 p는 Model 2 %s이다. 요산을 연속형으로 보면 차이가 없다(p %s).",
          ci(hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$strat_est, hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$strat_lo, hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$strat_hi),
          ci(hi_ever("남자", "Model 2", "홍조 없음")$strat_est, hi_ever("남자", "Model 2", "홍조 없음")$strat_lo, hi_ever("남자", "Model 2", "홍조 없음")$strat_hi),
          ci(hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$ratio_est, hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$ratio_lo, hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$ratio_hi),
          P(hi_ever("남자", "Model 2", "과거 또는 현재 홍조")$ratio_p), P(et("남자", "Model 2")$p), P(et("남자", "Model 3")$p), P(etr("남자", "Model 2")$p_trend_x_modifier[1]), P(et("남자", "Model 2", UA)$p)),
  { sv <- R$sens_ever[sex == "남자" & model == "Model 2" & id != "S0"]; w <- sv[which.min(ratio_high)]
    cf <- R$alt_form[sex == "남자" & model == "Model 2" & modifier == "flush_ever" & grepl("잔", form) & !grepl("추세|연속", form)]
    yv <- R$year_strat[sex == "남자" & model == "Model 2" & modifier == "flush_ever"]
    sprintf("5. **위 4번 소견은 절단점에는 견고하지만 조사연도에는 견고하지 않다.** 절단점을 바꿔도 맨 위 범주의 OR의 비는 %.2f–%.2f로 비슷하다. 민감도 분석 %d개에서 OR의 비는 %.2f–%.2f였고 가장 작은 경우는 '%s'(%s)이다. 조사연도별 OR의 비는 %s로 %s년에 치우쳐 있다(연도 간 이질성 검정 p %s). 여자는 방향이 같지만(%s) 유의하지 않다.",
            min(cf$est), max(cf$est), nrow(sv), min(sv$ratio_high), max(sv$ratio_high), w$analysis, ci(w$ratio_high, w$ratio_high_lo, w$ratio_high_hi),
            paste(yv[, sprintf("%s년 %.2f", year, ratio_high)], collapse = ", "), yv[which.max(ratio_high), year], P(g1(R$year_het[sex == "남자" & model == "Model 2" & modifier == "flush_ever"])$p),
            ci(hi_ever("여자", "Model 2", "과거 또는 현재 홍조")$ratio_est, hi_ever("여자", "Model 2", "과거 또는 현재 홍조")$ratio_lo, hi_ever("여자", "Model 2", "과거 또는 현재 홍조")$ratio_hi)) },
  sprintf("6. **부질문 2(성별): 홍조에 따른 차이가 남녀에서 다르다는 근거가 없고, 판단할 만큼 정밀하지도 않다.** 3원 상호작용 p는 Model 2 %s, Model 3 %s이다. 고음주에서 여자의 OR의 비 ÷ 남자의 OR의 비는 %s이다.",
          P(st3("Model 2")$p), P(st3("Model 3")$p), ci(sterm("Model 2", "고음주")$est, sterm("Model 2", "고음주")$lo, sterm("Model 2", "고음주")$hi)),
  sprintf("7. **여성 폐경 전·후(탐색): 근거가 없다.** 폐경 후 홍조·고음주 여성이 %d명(사례 %d명)이라 추정이 불안정하다.", pm_hi$n, pm_hi$n_event),
  sprintf("8. **품질 확인:** 연도별 가중치 합계가 지침서의 추계인구수와 일치했다. 모형 %d개가 모두 수렴했고 경고는 %d건이다. 사례가 0명인 칸 때문에 분리가 생긴 모형 %d개(여자의 민감도 분석)는 결측 처리했다. 새 도구의 추정치는 기존 분석과 일치했다(검증 %d개 항목 모두 일치). 국민건강통계의 조율 공식값은 찾지 못해 지표 수준의 공식 통계 대조는 끝내지 못했다.",
          nrow(o$diag), nrow(o$warn_log), o$diag[max_abs_coef > 8, .N], nrow(o$val)), "")
# ---- 질문별 검정 요약표: 각 질문이 무엇을 묻는지와 그 검정의 p (고요산혈증, Model 2·3) ----
qrow <- function(grp, q, test, who, p2, p3, note = "") data.table(구분 = grp, `무엇을 묻는가` = q, 검정 = test, 대상 = who, `p (Model 2)` = P(p2), `p (Model 3)` = P(p3), 비고 = note)
q_main <- "음주 수준과 고요산혈증의 연관성이 현재 홍조군과 비홍조군 사이에 다른가"
q_h    <- "연관성이 홍조 이력 세 군(비홍조·홍조 소실·지속 홍조) 사이에 다른가"
q_lost <- "현재 홍조가 없는 사람 가운데 소실군이 비홍조군보다 연관성이 강한가 (핵심 대비)"
q_sex  <- "홍조에 따른 연관성 차이가 남녀 사이에서 다른가"
q_meno <- "여성에서 그 차이가 폐경 전과 후에 다른가"
q_ever <- "과거 또는 현재 홍조가 있는 사람과 한 번도 없던 사람 사이에 연관성이 다른가"
low_ev <- "사례 10명 미만인 칸이 있어 해석하지 않음"
con_note <- function(s, con) if (hc(s, "Model 2", con)$estimable) sprintf("Holm 보정 p %s", P(hc(s, "Model 2", con)$p_holm)) else low_ev
omn_note <- function(s) if (all(R$hist_contrasts[outcome == HUA & sex == s & model == "Model 2", estimable])) "계획서 순서상 첫 검정" else low_ev
con_row <- function(q, test, s, con) qrow("부질문 1", q, test, s, hc(s, "Model 2", con)$p_wald, hc(s, "Model 3", con)$p_wald, con_note(s, con))
qt <- rbindlist(c(
  lapply(sexes, function(s) qrow("주질문(참고)", q_main, "음주 수준 × 현재 홍조 (2 df)", s, mt(s, "Model 2")$p, mt(s, "Model 3")$p, "계획서의 주 분석")),
  lapply(sexes, function(s) qrow("부질문 1", q_h, "음주 수준 × 홍조 이력, 세 군 공동 (4 df)", s, ht(s, "Model 2")$p, ht(s, "Model 3")$p, omn_note(s))),
  lapply(sexes, function(s) con_row(q_lost, "소실 vs 비홍조 (2 df)", s, "소실 vs 비홍조")),
  list(con_row("지속군이 비홍조군과 다른가", "지속 vs 비홍조 (2 df)", "남자", "지속 vs 비홍조"),
       con_row("소실군이 지속군과 다른가 (탐색)", "소실 vs 지속 (2 df)", "남자", "소실 vs 지속(탐색)"),
       qrow("부질문 2", q_sex, "음주 수준 × 현재 홍조 × 성별 (3원, 2 df)", "남녀 전체", st3("Model 2")$p, st3("Model 3")$p, "보정변수 효과를 성별마다 다르게 둔 모형"),
       qrow("부질문 2(탐색)", q_meno, "음주 수준 × 현재 홍조 × 폐경 (3원, 2 df)", "여자", mt3("범주형", "Model 2")$p, mt3("범주형", "Model 3")$p, "폐경 후 홍조군의 칸이 작아 탐색으로만 해석")),
  lapply(sexes, function(s) qrow("탐색적 분석(6절)", q_ever, "음주 수준 × 홍조 체질 (2 df)", s, et(s, "Model 2")$p, et(s, "Model 3")$p, "계획서에 없던 사후 분석")),
  lapply(sexes, function(s) qrow("탐색적 분석(6절)", q_ever, "추세 차이 (1 df)", s, etr(s, "Model 2")$p_trend_x_modifier[1], etr(s, "Model 3")$p_trend_x_modifier[1], "계획서에 없던 사후 분석"))))
fwrite(qt, file.path(out, "00_question_p_summary.csv"), bom = TRUE)
A("### 0-1. 질문별 검정 요약 (고요산혈증)", "", tab(qt),
  "- p는 상호작용항 전체의 설계 기반 Wald F 검정 값이다. 주 모형은 Model 2이고 Model 3은 임상 지표까지 보정한 민감도 분석이다.",
  "- 주질문과 부질문은 계획서에 정해 둔 분석이고, 탐색적 분석은 결과를 본 뒤 추가한 분석이다. 여자의 부질문 1 나머지 대비도 사례 수 기준 미달이라 표에 싣지 않았다(5-3절).", "",
  "### 0-2. p < 0.05인 항목 전체", "")
A(sprintf("전체 검정 %d개(공동 검정 기준: %s) 가운데 p < 0.05인 항목과 보조 지표는 다음과 같다. 여러 검정을 함께 본 결과이므로 하나하나를 확증으로 읽지 않는다.", sum(n_tests),
          paste(sprintf("%s %d", names(n_tests), n_tests), collapse = ", ")), "",
  if (nrow(sig)) tab(sig[, .(구분, 결과, 대상, 모형, 항목, p = P(p))]) else c("(없음)", ""))

# ============================== 1. 자료와 방법 ==============================
A("## 1. 자료와 방법", "", "### 1-1. 자료·대상·설계", "",
  tab(data.table(항목 = c("자료", "대상", "가중치·층·집락", "분석 대상 표시", "결과", "노출", "효과수정변수", "보정변수", "분석"),
    내용 = c("국민건강영양조사 제8기 기본DB hn19_all, hn20_all, hn21_all (data 폴더, 읽기 전용)",
             sprintf("만 19세 이상 최근 1년 음주자, 통풍 의사진단자(모름·무응답 포함) 제외, 보정변수 완전 응답자. 최종 %s명(남 %s, 여 %s)", big(o$n_sub), big(o$n_men), big(o$n_women)),
             "wt_itvex × 1/3(제8기 기수 내 통합: 8기 PDF p.36 / 인쇄 p.28), kstrata, psu",
             "행을 지우지 않고 inc(1/0)로 표시한 뒤 svydesign → subset. 성별·홍조군·폐경 상태도 설계 객체에서 subset",
             "고요산혈증(남 7.0, 여 6.0 mg/dL 이상), 보조로 혈청 요산(mg/dL) 연속형",
             "주당 음주잔수 3군(5·15잔), 보조로 추세(0·1·2)와 연속형(하루 잔수)",
             "현재 홍조(주질문), 홍조 이력 3군(부질문 1), 성별·폐경(부질문 2). 탐색적 분석에서는 과거 또는 현재 홍조로 묶은 2문항 분류(6절)",
             "Crude = 없음 / Model 1 = 연령 / Model 2 = + 교육·소득·조사연도·현재흡연·유산소 신체활동 / Model 3 = + BMI·고혈압·당뇨병·eGFR. 주 모형은 Model 2",
             "복합표본 로지스틱 회귀(quasibinomial)와 선형회귀. R survey 패키지"))),
  "### 1-2. 변수 정의와 근거", "", tab(fread(file.path(out, "01b_derived_variables.csv"), encoding = "UTF-8")[, .(변수 = variable, 정의 = definition, 근거 = source)]),
  "코드값과 결측 처리 전체는 01_variable_definitions.csv에 있다(9·99·999는 결측, 8·88·888은 변수마다 지침서의 의미에 따라 처리).", "",
  "### 1-3. 검정과 추정", "",
  "- **주 검정**: 상호작용항 전체의 설계 기반 Wald F 검정(양측, 유의수준 0.05). 칸이 작을 때 Wald 검정이 불안정할 수 있어 Rao–Scott 작업 우도비 검정(LRT)을 함께 적었다.",
  "- **신뢰구간과 p**: 계수의 선형결합에 대해 설계 기반 잔차 자유도의 t 분포를 썼다. 기존 분석(code/13, 21)은 정규 근사라 소수 둘째 자리에서 0.01 정도 다를 수 있다.",
  "- **효과 크기**: 같은 홍조군 안의 OR(저음주 대비), OR의 비(홍조군 OR ÷ 기준군 OR = 곱셈 척도의 상호작용), RERI(덧셈 척도, 델타법), 음주 수준 한 단계당 OR과 그 차이(추세).",
  sprintf("- **검출 가능한 최소 효과**: OR의 비의 표준오차로 계산했다. exp(%.2f × SE)이며 양측 5%%, 검정력 80%% 기준이다.", qnorm(0.975) + qnorm(0.8)),
  "- **칸별 사례 수 기준**: 비교에 들어가는 칸 중 고요산혈증 사례가 10명 미만인 칸이 있으면 그 검정은 해석하지 않고 기술통계로만 제시했다. 로지스틱 회귀에서 변수당 사례가 10건보다 적으면 계수와 Wald 검정이 불안정하다는 기준(Peduzzi 1996)을 칸 단위에 준용한 것이다.",
  "- **다중비교**: 주질문은 성별마다 검정 하나다. 부질문 1의 세 대비에는 Holm 보정 p를 함께 적었다. 보조 지표와 탐색 분석은 유의성 판정에 쓰지 않는다.", "")

# ============================== 2. 흐름 ==============================
A("## 2. 대상자 선정 흐름", "", tab(o$flow[, .(단계 = step, `남은 n` = big(n), 남자 = big(n_men), 여자 = big(n_women), 제외 = ifelse(is.na(excluded), "", big(excluded)))]),
  "절단점은 대상 선정에 영향을 주지 않으므로 계획서 분석과 같은 사람들이다.", "")

# ============================== 3. 특성 ==============================
dd <- o$dist
A("## 3. 대상자 특성", "", "### 3-1. 음주 수준 분포", "",
  tab(dd[, .(성별 = sex, `음주 수준` = drink, 범위 = range, n = big(n), `가중 % (SE)` = sprintf("%.1f (%.1f)", wt_pct, wt_pct_se), `고요산혈증 사례` = n_hua,
             `가중 유병률 %` = f1(hua_pct_wt), `가중 평균 요산(mg/dL)` = f2(uacid_mean_wt), `가중 평균 잔수(잔/주)` = f1(drinks_wk_mean_wt))]),
  "### 3-2. Table 1: 성별 × 음주 수준", "", "n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치(SE). p는 범주형 Rao–Scott 카이제곱, 연속형 설계 기반 Wald 검정.", "")
for (s in sexes) { x <- o$t1_drink[sex == s & !(variable == "meno" & s == "남자")]
  A(sprintf("**%s**", s), "", tab(x[, .(변수 = label, 범주 = level, `저음주 n` = big(`저음주: n`), `저음주` = `저음주: 추정치`, `중음주 n` = big(`중음주: n`), `중음주` = `중음주: 추정치`,
                                         `고음주 n` = big(`고음주: n`), `고음주` = `고음주: 추정치`, p = P(p_value))])) }
cd <- o$cellsD
A("현재 홍조별·홍조 이력별 Table 1은 04b, 04c CSV에 있다.", "", "### 3-3. 칸별 인원과 범주 안 실제 잔수", "",
  tab(cd[group_var == "flush_now", .(성별 = sex, `현재 홍조` = group, 음주 = drink, n = big(n), 사례 = n_hua, `가중 유병률 % (SE)` = sprintf("%.1f (%.1f)", hua_pct_wt, hua_se_wt),
                                      `가중 평균 요산` = f2(uacid_mean_wt), `가중 평균 잔수` = f1(drinks_wk_mean_wt), `가중 중앙값 잔수` = f1(drinks_wk_median_wt))]),
  tab(cd[group_var == "flush_grp", .(성별 = sex, `홍조 패턴` = group, 음주 = drink, n = big(n), 사례 = n_hua, `가중 유병률 % (SE)` = sprintf("%.1f (%.1f)", hua_pct_wt, hua_se_wt),
                                      `가중 평균 요산` = f2(uacid_mean_wt), `가중 평균 잔수` = f1(drinks_wk_mean_wt))]))

# ============================== 4. 주질문 ==============================
tr_p <- function(tb) unique(tb[, .(outcome, sex, model, p_trend_x_modifier)])
A("## 4. 주질문: 음주 수준 × 현재 홍조", "", "### 4-1. 상호작용 검정", "")
for (oc in c(HUA, UA)) { x <- merge(R$main_tests[outcome == oc], tr_p(R$main_trend), by = c("outcome", "sex", "model"), sort = FALSE)
  A(sprintf("**%s**", oc), "", tab(x[, .(성별 = sex, 모형 = model, `모형 n` = big(n_model), `Wald F` = f2(F), `p (Wald, 2 df)` = P(p), `p (Rao–Scott LRT)` = P(p_lrt), `추세 차이 p (1 df, 보조)` = P(p_trend_x_modifier))])) }
A("### 4-2. 홍조군별 고요산혈증 OR", "")
for (s in sexes) for (mdl in M23) { z <- mc(s, mdl); t1 <- R$main_trend[outcome == HUA & sex == s & model == mdl]; h <- z[modifier == "홍조" & exposure == "고음주"]
  A(sprintf("**%s · %s** (상호작용 p = %s, n = %s)", s, mdl, P(mt(s, mdl)$p), big(z$n_model[1])), "", tab(kv(z)),
    sprintf("- 한 단계당 OR: %s. 추세 차이 p = %s.", paste(t1[, sprintf("%s %s", modifier, ci(per_level_est, ci_low, ci_high))], collapse = ", "), P(t1$p_trend_x_modifier[1])),
    sprintf("- 고음주에서 OR의 비 %s. 검출 가능한 최소 OR의 비는 %.2f배다.", ci(h$ratio_est, h$ratio_lo, h$ratio_hi), h$ratio_mde80), "") }
me <- R$main_effects[outcome == HUA & model %in% M23]
A("### 4-3. 상호작용항이 없는 모형의 주효과 OR", "",
  tab(me[, .(성별 = sex, 모형 = model, 항 = sub("^drink3", "음주: ", sub("^flush_now", "현재 홍조: ", term)), `OR (95% CI)` = ci(est, lo, hi), p = P(p))]),
  "### 4-4. 혈청 요산 연속형 (β, mg/dL), Model 2", "")
for (s in sexes) { z <- mc(s, "Model 2", UA); A(sprintf("**%s** (상호작용 p = %s)", s, P(mt(s, "Model 2", UA)$p)), "", tab(kv(z, or = FALSE))) }
A("### 4-5. 해석", "",
  sprintf("- 남녀 모두, 홍조군과 비홍조군 모두에서 음주 수준이 높을수록 고요산혈증이 많다. 고음주의 OR은 남자 비홍조 %s, 홍조 %s, 여자 비홍조 %s, 홍조 %s이다(Model 2).",
          ci(hi_main("남자", "Model 2", "비홍조")$strat_est, hi_main("남자", "Model 2", "비홍조")$strat_lo, hi_main("남자", "Model 2", "비홍조")$strat_hi), ci(m2m$strat_est, m2m$strat_lo, m2m$strat_hi),
          ci(hi_main("여자", "Model 2", "비홍조")$strat_est, hi_main("여자", "Model 2", "비홍조")$strat_lo, hi_main("여자", "Model 2", "비홍조")$strat_hi), ci(m2w$strat_est, m2w$strat_lo, m2w$strat_hi)),
  "- 홍조군의 OR이 수치상 더 크지만 OR의 비의 신뢰구간이 1을 포함한다. Wald 검정과 우도비 검정, 추세 검정, 덧셈 척도(RERI) 모두 같은 결론이다.",
  sprintf("- 저음주에서 홍조군은 비홍조군보다 고요산혈증이 적다(남자 OR %s). 홍조 자체가 요산을 높이는 요인은 아니다.",
          { z <- g1(mc("남자", "Model 2")[modifier == "홍조" & exposure == "저음주"]); ci(z$mod_est, z$mod_lo, z$mod_hi) }), "")

# ============================== 5. 부질문 1 ==============================
A("## 5. 부질문 1: 홍조 이력에 따른 차이", "",
  "### 5-1. 부질문 1을 어떻게 분석해야 하는가", "",
  "계획서의 질문은 \"현재 홍조가 없는 사람 가운데, 과거에 홍조가 있었던 사람(소실군)은 원래 홍조가 없던 사람(비홍조군)보다 음주–고요산혈증 연관성이 강한가\"이다. 이 질문에 정확히 답하도록 다음과 같이 정했다.", "",
  "1. **답으로 삼는 검정은 '소실 vs 비홍조' 대비 하나다.** 중·고음주 상호작용항 두 개를 함께 검정한다(2 df, 남녀 각각, Model 2). 계획서가 '우선 비교'로 정한 대비다. 세 군 전체의 4 df 공동 검정은 계획서 순서대로 먼저 제시하지만, 지속군의 차이가 섞여 가설에 대한 직접적인 답이 아니다.",
  "2. **p와 함께 효과 크기를 낸다.** 고음주에서 소실군 OR ÷ 비홍조군 OR과 95% 신뢰구간이다. 계획서는 '차이가 작고 신뢰구간이 좁은 경우'와 '신뢰구간이 넓어 판단하기 어려운 경우'를 구분하겠다고 했는데, 이 구분은 p가 아니라 신뢰구간으로만 할 수 있다.",
  "3. **칸별 사례 수 기준을 미리 둔다.** 비교하는 두 군의 여섯 칸 가운데 고요산혈증 사례가 10명 미만인 칸이 있으면 검정을 해석하지 않는다. 계획서의 '적으면 기술통계와 불확실성 중심으로 제시'를 숫자로 정한 것이다.",
  "4. **나머지 대비는 탐색이다.** 지속 vs 비홍조, 소실 vs 지속에는 Holm 보정 p를 함께 적는다.",
  "5. **같은 질문을 다른 방식으로 확인한다.** (가) 현재 비홍조자만으로 음주 × 과거 홍조를 본다. (나) 신규 홍조를 포함한 4군을 본다. 과거 또는 현재 홍조로 묶는 2문항 분류는 계획서에 없던 분석이므로 6절에 탐색적 분석으로 따로 싣는다.",
  "6. **주 검정은 계획서의 범주형 공동 검정으로 유지한다.** 가설 문구가 '음주 수준이 높아질 때 늘어나는 정도'이므로 추세 차이(1 df)가 가설에 더 가깝고 검정력도 높지만, 결과를 본 뒤 주 검정을 바꾸면 안 되므로 보조로만 제시한다.", "",
  "### 5-2. 칸별 인원과 추정 가능 여부", "")
h2 <- R$hist_cells[outcome == HUA & model == "Model 2"]
A(tab(dcast(h2[, .(성별 = sex, `홍조 이력` = factor(modifier, unique(modifier)), exposure = factor(exposure, lv3), v = sprintf("%s (%d)", big(n), n_event))], 성별 + `홍조 이력` ~ exposure, value.var = "v")),
  "칸 안의 값은 비가중 n(고요산혈증 사례)이다.", "",
  tab(R$hist_contrasts[outcome == HUA & model == "Model 2", .(성별 = sex, 대비 = contrast, `가장 작은 칸의 n` = min_cell_n, `가장 작은 칸의 사례` = min_cell_events, 판정 = ifelse(estimable, "추정 가능", "기술통계만(사례 10명 미만)"))]),
  "### 5-3. 계획서 분석: 3군 모형의 검정", "")
for (s in sexes) { x <- R$hist_contrasts[outcome == HUA & sex == s & model %in% M23]; hh <- R$hist_tests[outcome == HUA & sex == s & model %in% M23]
  A(sprintf("**%s**%s", s, if (!all(x$estimable)) " — 사례 수 기준 미달. 아래 수치는 참고용이며 해석하지 않는다." else ""), "",
    tab(hh[, .(모형 = model, 검정 = "세 군 공동(4 df)", `모형 n` = big(n_model), `p (Wald)` = P(p), `p (LRT)` = P(p_lrt))]),
    tab(x[, .(모형 = model, 대비 = contrast, `p (Wald, 2 df)` = P(p_wald), `p (LRT)` = P(p_lrt), `p (Holm)` = P(p_holm), `중음주 OR의 비` = ci(mid_est, mid_lo, mid_hi), `고음주 OR의 비` = ci(high_est, high_lo, high_hi),
              `검출 가능한 최소 비` = f2(high_mde80), `추세의 비(한 단계당)` = ci(trend_est, trend_lo, trend_hi), `추세 차이 p` = P(p_trend_diff))])) }
A("### 5-4. 홍조 이력군별 고요산혈증 OR", "")
for (mdl in M23) A(sprintf("**남자 · %s**", mdl), "", tab(kv(R$hist_cells[outcome == HUA & sex == "남자" & model == mdl], g_lab = "홍조 이력")))
A("**여자 · Model 2** (기술 목적)", "", tab(kv(R$hist_cells[outcome == HUA & sex == "여자" & model == "Model 2"], g_lab = "홍조 이력")),
  "### 5-5. 보조 (가): 현재 비홍조자만 (음주 수준 × 과거 홍조)", "",
  tab(R$nonflush_tests[, .(결과 = outcome, 성별 = sex, 모형 = model, `모형 n` = big(n_model), `p (Wald, 2 df)` = P(p), `p (LRT)` = P(p_lrt))]),
  "3군 모형의 소실 vs 비홍조 대비와 같은 결론이다. 보정변수 효과를 현재 비홍조자 안에서만 추정해도 달라지지 않는다.", "",
  "")
# ============================== 6. 탐색적 분석: 2문항 분류 (보고서에서는 부질문 1의 나머지 절 뒤에 놓인다) ==============================
SEC <- "s6"
ev_n  <- function(s, g) g1(etr(s, "Model 2")[modifier == g])$n
grp_n <- function(s, g) cd[group_var == "flush_grp" & sex == s & group == g, sum(n)]
A("## 6. 탐색적 분석: 과거 또는 현재 홍조로 묶은 2문항 분류", "",
  "### 6-1. 이 분석의 지위와 근거", "",
  "- **지위.** 계획서에 없던 분석이다. 주질문과 부질문 1의 결과를 본 뒤에 추가했다. 주질문과 부질문의 결론을 바꾸지 않으며, 다음 연구에서 확인할 가설을 제시하는 용도로만 쓴다.",
  sprintf("- **정의.** 과거 홍조(BD9_1 = 예) 또는 현재 홍조(BD9_2 = 가끔·자주·늘)가 있으면 '홍조 체질', 둘 다 없으면 '홍조 없음'이다. 남자는 홍조 없음 %s명과 홍조 체질 %s명, 여자는 홍조 없음 %s명과 홍조 체질 %s명이다.",
          big(ev_n("남자", "홍조 없음")), big(ev_n("남자", "과거 또는 현재 홍조")), big(ev_n("여자", "홍조 없음")), big(ev_n("여자", "과거 또는 현재 홍조"))),
  sprintf("- **주질문의 분류와 다른 점.** 주질문은 현재 홍조만 보므로 소실군(남 %s명, 여 %s명)이 비홍조군에 들어간다. 2문항 분류에서는 소실군이 홍조 체질군으로 옮겨 간다. 신규 홍조군(남 %s명, 여 %s명)은 두 분류 모두에서 홍조 쪽이다. 부질문 1의 이력 분석과 달리 아무도 제외하지 않는다.",
          big(grp_n("남자", "홍조 소실")), big(grp_n("여자", "홍조 소실")), big(grp_n("남자", "신규 홍조")), big(grp_n("여자", "신규 홍조"))),
  "- **근거.** 국건영의 과거 홍조(BD9_1)와 현재 홍조(BD9_2) 문항은 Yokoyama 홍조 설문의 두 문항과 구조가 같다. 이 설문은 두 문항 중 하나라도 '예'이면 ALDH2 결핍으로 분류하며, 일본 남성 대조군에서 민감도 90.1%, 특이도 88.0%였다(Yokoyama 2003). 과거 문항을 넣은 이유는 일부가 홍조에 내성이 생기기 때문이다(Brooks 2009). 부질문 1의 가설(소실군도 체질은 홍조군)이 맞다면, 소실군을 비홍조군에 넣는 주질문의 분류보다 이 분류에서 효과수정이 더 선명해야 한다.",
  "- **검정.** 주질문과 같은 모형에서 효과수정변수만 바꿨다. 음주 수준 × 홍조 체질의 공동 Wald 검정(2 df)과 추세 차이(1 df)를 함께 적는다.", "",
  "### 6-2. 결과", "")
for (oc in c(HUA, UA)) { x <- merge(R$ever_tests[outcome == oc], tr_p(R$ever_trend), by = c("outcome", "sex", "model"), sort = FALSE)
  A(sprintf("**%s**", oc), "", tab(x[, .(성별 = sex, 모형 = model, `모형 n` = big(n_model), `Wald F` = f2(F), `p (Wald, 2 df)` = P(p), `p (LRT)` = P(p_lrt), `추세 차이 p (1 df)` = P(p_trend_x_modifier))])) }
for (s in sexes) for (mdl in M23) { z <- ec(s, mdl); t1 <- etr(s, mdl)
  A(sprintf("**%s · %s** (공동 p = %s)", s, mdl, P(et(s, mdl)$p)), "", tab(kv(z, g_lab = "홍조 체질")),
    sprintf("- 한 단계당 OR: %s. 추세 차이 p = %s.", paste(t1[, sprintf("%s %s", modifier, ci(per_level_est, ci_low, ci_high))], collapse = ", "), P(t1$p_trend_x_modifier[1])), "") }
se <- R$sens_ever
A("### 6-3. 견고성 확인", "", "**민감도 분석 (남자, 고요산혈증)**", "",
  tab(se[sex == "남자", .(ID = id, 분석 = analysis, 모형 = model, `모형 n` = big(n_model), `공동 p` = P(p_interaction), `홍조 없음·고음주 OR` = ci(never_high_or, never_high_lo, never_high_hi),
                          `홍조 체질·고음주 OR` = ci(ever_high_or, ever_high_lo, ever_high_hi), `n (사례)` = sprintf("%s (%d)", big(ever_high_n), ever_high_events), `OR의 비` = ci(ratio_high, ratio_high_lo, ratio_high_hi), `비의 p` = P(ratio_high_p))]))
ys <- R$year_strat; yh <- R$year_het; mod_lab <- function(x) ifelse(x == "flush_now", "현재 홍조", "2문항 분류")
A("**조사연도별 결과 (남자, Model 2, 고요산혈증)**", "",
  tab(ys[sex == "남자" & model == "Model 2", .(효과수정변수 = mod_lab(modifier), 연도 = year, `모형 n` = big(n_model), `상호작용 p` = P(p_interaction), `기준군·고음주 OR` = ci(ref_high_or, ref_high_lo, ref_high_hi),
                                               `홍조군·고음주 OR` = ci(flush_high_or, flush_high_lo, flush_high_hi), `홍조군·고음주 n (사례)` = sprintf("%s (%d)", big(flush_high_n), flush_high_events),
                                               `OR의 비` = ci(ratio_high, ratio_high_lo, ratio_high_hi))]),
  "연도 간 이질성 검정(음주 수준 × 홍조 × 조사연도, 4 df):", "",
  tab(yh[, .(성별 = sex, 효과수정변수 = mod_lab(modifier), 모형 = model, `모형 n` = big(n_model), F = f2(F), p = P(p))]))
af <- R$alt_form
A("**절단점과 용량 형태를 바꿨을 때 (고요산혈증)**", "",
  "아래 표는 절단점을 고르기 위한 것이 아니라, 결과가 절단점 선택에 얼마나 의존하는지 공개하기 위한 것이다. 현재 홍조(주질문)와 2문항 분류를 같은 틀로 나란히 적었다.", "",
  tab(af[, .(성별 = sex, 모형 = model, 효과수정변수 = ifelse(modifier == "flush_now", "현재 홍조", "2문항 분류"), `절단점·형태` = form, df = df, `상호작용 p` = P(p),
             추정치 = estimate_label, `값 (95% CI)` = ci(est, lo, hi), `맨 위 칸 n (사례)` = ifelse(is.na(top_cell_n), "", sprintf("%s (%d)", big(top_cell_n), top_cell_events)))]),
  "")
# ---- 부질문 1의 나머지 절 (보고서에서는 5-5 뒤, 6절 앞에 놓인다) ----
SEC <- "s5b"
A("### 5-6. 보조 (나): 신규 홍조를 포함한 4군", "")
g4 <- R$grp4_cells
A(tab(g4[exposure != "저음주", .(성별 = sex, 모형 = model, `홍조 패턴` = modifier, 음주 = exposure, n = big(n), 사례 = n_event, `저음주 대비 OR` = ci(strat_est, strat_lo, strat_hi),
                                 `공동 p (6 df, Wald)` = P(p_wald_6df), `공동 p (LRT)` = P(p_lrt_6df))]),
  "### 5-7. 혈청 요산 연속형", "",
  tab(R$hist_contrasts[outcome == UA & model %in% M23, .(성별 = sex, 모형 = model, 대비 = contrast, `p (Wald, 2 df)` = P(p_wald), `고음주 β의 차이 (mg/dL)` = ci(high_est, high_lo, high_hi),
                                                          `추세 차이 p` = P(p_trend_diff), 판정 = ifelse(estimable, "추정 가능", "기술통계만"))]))

c_lost <- hc("남자", "Model 2", "소실 vs 비홍조"); e_m2 <- hi_ever("남자", "Model 2", "과거 또는 현재 홍조"); n_m2 <- hi_ever("남자", "Model 2", "홍조 없음"); e_w2 <- hi_ever("여자", "Model 2", "과거 또는 현재 홍조")
y_ev <- R$year_strat[sex == "남자" & model == "Model 2" & modifier == "flush_ever"]; s4b <- g1(R$sens_ever[sex == "남자" & model == "Model 2" & id == "S4b"])
yr_txt <- paste(y_ev[, sprintf("%s년 %.2f", year, ratio_high)], collapse = ", ")
A("### 5-8. 해석", "",
  sprintf("- **계획서 분석의 답.** 남자에서 소실군의 OR이 비홍조군보다 큰 방향이다(고음주 OR의 비 %s). 그러나 신뢰구간이 넓고 p는 %s(Holm %s)라서 확인되지 않았다. 이 대비에서 검출 가능한 최소 OR의 비가 %.1f배이므로, 실제로 두 배 차이가 있어도 이 자료에서는 유의하게 나오기 어렵다.",
          ci(c_lost$high_est, c_lost$high_lo, c_lost$high_hi), P(c_lost$p_wald), P(c_lost$p_holm), c_lost$high_mde80),
  "- **여자는 판단할 수 없다.** 소실군과 지속군의 중·고음주 칸에 고요산혈증 사례가 10명 미만이다. 수치를 싣더라도 기술 목적이라고 밝힌다.",
  "- **다른 방식으로 봐도 같다.** 현재 비홍조자만 본 모형과 신규 홍조를 포함한 4군 모형에서도 결론이 같다. 요산 수치(연속형)에서는 군 간 차이가 보이지 않는다.",
  "- 과거 또는 현재 홍조로 묶은 2문항 분류는 계획서에 없던 분석이므로 6절에 탐색적 분석으로 따로 실었다.", "")
SEC <- "s6"
A("### 6-4. 해석과 보고 방법", "",
  sprintf("- **2문항 분류는 같은 질문을 더 많은 사람으로 본 분석이다.** 남자의 고음주 OR의 비는 %s이고 검출 가능한 최소 비는 %.2f배로 3군 대비보다 정밀하다. 범주형 공동 검정은 Model 2에서 p %s로 유의하지 않고, 추세·연속형(1 df) 검정과 Model 3에서는 0.05보다 작다.",
          ci(e_m2$ratio_est, e_m2$ratio_lo, e_m2$ratio_hi), e_m2$ratio_mde80, P(et("남자", "Model 2")$p)),
  sprintf("- **그러나 확증으로 쓸 수 없는 이유가 네 가지다.** 첫째, 결과를 본 뒤 추가한 분석이다. 둘째, 조사연도별 OR의 비가 %s로 한 해에 치우쳐 있고 2020년을 빼면 %s로 줄어든다. 셋째, 요산 수치(연속형)에서는 차이가 보이지 않는다(p %s). 넷째, 여자에서는 유의하지 않다.",
          paste(y_ev[, sprintf("%s년 %.2f", year, ratio_high)], collapse = ", "), ci(s4b$ratio_high, s4b$ratio_high_lo, s4b$ratio_high_hi), P(et("남자", "Model 2", UA)$p)),
  sprintf("- **주질문과의 관계.** 현재 홍조로 나눈 주질문에서 OR의 비가 %s로 작았던 것은, 연관성이 강한 소실군이 비홍조군에 섞여 있기 때문일 수 있다. 다만 이것은 해석이지 검정된 결론이 아니다.",
          ci(m2m$ratio_est, m2m$ratio_lo, m2m$ratio_hi)),
  "- **보고 원칙.** 계획서의 분석을 주 결과로 제시한다. 이 분석은 '결과를 본 뒤 추가한 탐색적 분석'이라고 밝혀 따로 싣는다. 초록과 결론에는 주질문의 결과를 먼저 쓴다. 유의하다, 유의하지 않다는 표현보다 OR의 비와 신뢰구간으로 서술한다.", "",
  "**연구보고서에 옮겨 쓸 수 있는 문장**", "",
  "- 방법: \"계획서에 없던 탐색적 분석으로, 과거 또는 현재 홍조가 있다고 답한 사람을 홍조 체질군, 두 문항 모두 아니라고 답한 사람을 비홍조군으로 나눈 2문항 분류(Yokoyama 2003; Brooks 2009)를 효과수정변수로 써서 주분석과 같은 모형을 적합하였다. 이 분석은 주분석의 결과를 확인한 뒤 추가하였다.\"",
  sprintf("- 결과: \"탐색적 분석에서 남자의 고음주 OR은 홍조 체질군 %s, 비홍조군 %s였고 OR의 비는 %s였다. 음주 수준 × 홍조 체질 상호작용의 공동 검정은 p = %s, 추세 차이 검정은 p = %s이었다. 조사연도별 OR의 비는 %s였다. 여자의 고음주 OR의 비는 %s, 공동 검정은 p = %s이었다.\"",
          ci(e_m2$strat_est, e_m2$strat_lo, e_m2$strat_hi), ci(n_m2$strat_est, n_m2$strat_lo, n_m2$strat_hi), ci(e_m2$ratio_est, e_m2$ratio_lo, e_m2$ratio_hi),
          P(et("남자", "Model 2")$p), P(etr("남자", "Model 2")$p_trend_x_modifier[1]), yr_txt, ci(e_w2$ratio_est, e_w2$ratio_lo, e_w2$ratio_hi), P(et("여자", "Model 2")$p)),
  sprintf("- 한계: \"이 결과는 사후에 추가한 분석에서 나왔고 조사연도에 따라 일관되지 않았으며(2020년을 제외하면 OR의 비 %s), 혈청 요산을 연속형으로 본 분석에서는 나타나지 않았다(p = %s). 따라서 가설을 제시하는 수준의 근거로만 해석한다.\"",
          ci(s4b$ratio_high, s4b$ratio_high_lo, s4b$ratio_high_hi), P(et("남자", "Model 2", UA)$p)), "")
SEC <- "z"

# ============================== 7. 부질문 2 ==============================
A("## 7. 부질문 2: 성별에 따른 차이", "",
  "### 7-1. 부질문 2를 어떻게 분석해야 하는가", "",
  "질문은 \"홍조에 따른 연관성 차이(주질문의 상호작용)가 남녀에서 다른가\"이고, 통계적으로는 음주 수준 × 현재 홍조 × 성별의 3원 상호작용이다.", "",
  "1. **3원 검정을 남녀 층화 모형과 정확히 대응시킨다.** 주질문은 남녀를 나눠 분석하므로 보정변수의 효과가 성별마다 다르게 추정된다. 3원 검정도 통합 모형에서 보정변수 효과를 성별마다 다르게 두어야 같은 것을 비교하게 된다. 이렇게 하면 3원 계수가 '여자의 OR의 비 ÷ 남자의 OR의 비'와 정확히 같아지고(확인함), 같은 조사구에 사는 남녀의 상관도 설계 기반 분산에 반영된다. 계획서 문구 그대로(보정변수 효과 남녀 공통)인 모형은 민감도 분석으로 함께 적었다.",
  "2. **'남자는 유의, 여자는 비유의'로 비교하지 않는다.** 성별마다 유의성이 다르다는 것은 차이의 근거가 아니다. 차이 자체를 검정한 3원 검정과 그 신뢰구간으로 판단한다.",
  "3. **같은 범주라도 남녀의 실제 잔수가 다르다는 점을 확인한다.** 계획서는 '실제 음주량 분포가 겹치는 범위를 고려해 해석'한다고 했다. 범주 안의 실제 잔수를 표로 제시하고, 범주 구성에 덜 민감한 추세(1 df)와 연속형(하루 1잔당, 1 df) 3원 상호작용을 함께 본다.",
  "4. **결과 척도를 두 가지로 본다.** 고요산혈증은 기준값이 남녀에서 다르므로(7.0, 6.0 mg/dL) 같은 척도인 요산 수치(mg/dL)도 함께 본다.",
  "5. **폐경 전·후는 탐색이다.** 칸이 작아 2 df 검정이 한두 명에 좌우되므로 추세(1 df)와 주 5잔 이상 여부(1 df)로 요약한 3원 검정을 함께 적는다.", "",
  "### 7-2. 3원 상호작용 검정", "",
  tab(R$sex_tests[grepl("3원", test), .(결과 = outcome, 모형 = model, `보정변수 효과` = covariates, `모형 n` = big(n_model), `Wald F` = f2(F), `p (Wald, 2 df)` = P(p), `p (LRT)` = P(p_lrt))]),
  "3원 계수(고요산혈증: 여자의 OR의 비 ÷ 남자의 OR의 비, 요산: β 차이의 차이). 보정변수 효과를 성별마다 다르게 둔 모형.", "",
  tab(R$sex_terms[model %in% M23 & grepl("층화", covariates), .(결과 = outcome, 모형 = model, 수준 = ifelse(grepl("중음주", term), "중음주", "고음주"), `추정치 (95% CI)` = ci(est, lo, hi), p = P(p),
                                                                 `검출 가능한 최소 효과` = f2(mde80))]),
  "### 7-3. 범주 안 실제 잔수와 용량 형태별 3원 상호작용", "",
  tab(dcast(cd[group_var == "flush_now", .(성별 = sex, `현재 홍조` = factor(group, unique(group)), drink = factor(drink, lv3), v = sprintf("%.1f / %.1f", drinks_wk_mean_wt, drinks_wk_median_wt))], 성별 + `현재 홍조` ~ drink, value.var = "v")),
  "칸 안의 값은 주당 잔수의 가중 평균 / 가중 중앙값이다.", "",
  tab(R$sex_dose[model %in% M23, .(결과 = outcome, 모형 = model, `용량 형태` = dose_form, `3원 계수 (95% CI)` = ci(est, lo, hi, 3), p = P(p))]),
  "### 7-4. 홍조군 안에서 음주 수준 × 성별", "",
  tab(R$sex_tests[!grepl("3원", test), .(결과 = outcome, 모형 = model, 검정 = test, `모형 n` = big(n_model), `Wald F` = f2(F), p = P(p))]),
  "### 7-5. 여성 폐경 전·후 (탐색)", "",
  tab(R$meno_tests[, .(결과 = outcome, 구분 = menopause, 모형 = model, 검정 = test, `모형 n` = big(n_model), p = P(p))]))
for (m in c("폐경 전", "폐경 후")) A(sprintf("**%s · Model 2 · 고요산혈증**", m), "", tab(kv(R$meno_cells[outcome == HUA & menopause == m & model == "Model 2"])))
t3 <- sterm("Model 2", "고음주")
A("### 7-6. 해석", "",
  sprintf("- **3원 상호작용의 근거가 없다.** p는 Model 2 %s, Model 3 %s이고 우도비 검정, 계획서 문구 그대로의 모형, 추세·연속형 형태에서도 같다.", P(st3("Model 2")$p), P(st3("Model 3")$p)),
  sprintf("- **'남녀가 같다'는 뜻은 아니다.** 고음주에서 여자의 OR의 비 ÷ 남자의 OR의 비는 %s이다. 검출 가능한 최소 효과가 %.1f배여서, 남녀 차이가 세 배 가까이 되어도 이 자료로는 알 수 없다.",
          ci(t3$est, t3$lo, t3$hi), t3$mde80),
  sprintf("- **방향은 남녀가 같다.** 고음주에서 홍조군 OR ÷ 비홍조군 OR은 남자 %s, 여자 %s로 둘 다 1보다 크고 둘 다 신뢰구간이 1을 포함한다.",
          ci(m2m$ratio_est, m2m$ratio_lo, m2m$ratio_hi), ci(m2w$ratio_est, m2w$ratio_lo, m2w$ratio_hi)),
  "- **보정 전에만 보이는 차이는 성별과 연령 구성의 차이다.** 비홍조군 안의 음주 × 성별 검정은 Crude에서만 p < 0.05이고 연령을 넣으면 사라진다.",
  sprintf("- **폐경 전·후는 판단할 수 없다.** 3원 검정 p는 범주형 %s, 추세 %s, 주 5잔 이상 여부 %s이다(Model 2). 폐경 후 홍조군의 중·고음주 칸은 사례가 %d명, %d명뿐이다.",
          P(mt3("범주형")$p), P(mt3("추세")$p), P(mt3("주 5잔")$p),
          g1(R$meno_cells[outcome == HUA & menopause == "폐경 후" & model == "Model 2" & modifier == "홍조" & exposure == "중음주"])$n_event, pm_hi$n_event), "")

# ============================== 8. 민감도 ==============================
sm <- R$sens_main; sh <- R$sens_hist
A("## 8. 민감도 분석 (고요산혈증)", "", "### 8-1. 주질문", "",
  tab(sm[, .(ID = id, 분석 = analysis, 성별 = sex, 모형 = model, `모형 n` = big(n_model), `상호작용 p` = P(p_interaction), `비홍조·고음주 OR` = ci(nonflush_high_or, nonflush_high_lo, nonflush_high_hi),
             `홍조·고음주 OR` = ci(flush_high_or, flush_high_lo, flush_high_hi), `홍조·고음주 n (사례)` = sprintf("%s (%d)", big(flush_high_n), flush_high_events), `OR의 비` = ci(ratio_high, ratio_high_lo, ratio_high_hi))]),
  sprintf("주질문의 상호작용 p는 모든 민감도 분석에서 %s이다.", prange(sm$p_interaction)), "",
  "2문항 분류의 민감도 분석은 6-3절에 있다.", "",
  "### 8-2. 부질문 1 (남자, 소실 vs 비홍조)", "",
  tab(sh[sex == "남자", .(ID = id, 분석 = analysis, 모형 = model, `모형 n` = big(n_model), `공동 p (4 df)` = P(p_omnibus_4df), `소실 vs 비홍조 p (2 df)` = P(p_lost_vs_never_2df),
                          `소실·고음주 n (사례)` = sprintf("%s (%d)", big(lost_high_n), lost_high_events), `소실·고음주 OR` = ci(lost_high_or, lost_high_lo, lost_high_hi), `OR의 비` = ci(ratio_high, ratio_high_lo, ratio_high_hi))]))

# ============================== 9. 계획서 절단점과 비교 ==============================
p13 <- unique(o$ref13$int4[, .(sex, model, p_3.5_14 = p_interaction_joint)])
x8 <- merge(R$main_tests[outcome == HUA, .(sex, model, p_5_15 = p)], p13, by = c("sex", "model"), sort = FALSE)
A("## 9. 계획서 절단점(3.5·14잔)과 비교", "", "주질문의 상호작용 p(고요산혈증):", "",
  tab(x8[, .(성별 = sex, 모형 = model, `5·15잔 p` = P(p_5_15), `3.5·14잔 p` = P(p_3.5_14))]),
  "부질문과 칸별 OR의 비교는 output/17_cutpoint_5_15/SUMMARY_5_15_주질문_부질문.md의 6절, 절단점·형태별 비교는 이 문서의 6-3절 표에 있다.", "")

# ============================== 10. 품질 확인 ==============================
oa <- o$off_ad
A("## 10. 품질 확인", "", "### 10-1. 공식 통계 대조", "",
  "**(1) 가중치 합계 = 추계인구수** (8기 원시자료 이용지침서 PDF p.40 / 인쇄 p.32 표 11, 천명)", "",
  tab(o$wt_chk[, .(연도 = year, `전체 행` = big(n_all), `가중치 있는 행` = big(n_weighted), `wt_itvex 합계(천명)` = formatC(estimate, format = "f", digits = 2, big.mark = ","), `지침서 값(천명)` = big(official), 판정 = ifelse(match, "일치", "불일치"))]),
  "**(2) 연간음주자 중 고위험음주율·월간음주율** (질병관리청 주간 건강과 질병 2024;17(21):877-902 표 1)", "",
  tab(oa[, .(지표 = indicator, 연도 = year, 성별 = sex_lab, `비가중 n` = big(n_unweighted), `추정치 % (95% CI)` = sprintf("%.1f (%.1f–%.1f)", estimate, ci_low, ci_high), `게재값 %` = f1(official), 차이 = sprintf("%+.1f", diff),
             `95% CI 안` = ifelse(within_ci, "예", "아니오"))]),
  sprintf("- 게재값 %d개가 모두 추정치의 95%% 신뢰구간 안에 있다(차이 %+.1f ~ %+.1f %%p). 다만 소수 첫째 자리까지 같지는 않다. 남자는 추정치가 조금 높고 여자는 조금 낮은 일정한 방향이어서 게재값이 연령표준화율일 가능성이 있으나, 논문에 표준화 여부가 적혀 있지 않아 확인하지 못했다.",
          nrow(oa), min(oa$diff), max(oa$diff)),
  "- 만 19세 이상 전체의 고위험음주율은 공식 발표값이 2005년 추계인구 연령표준화율이어서 조율과 직접 비교할 수 없다. 지침서에 표준인구가 없어 표준화율을 재현하지 못했다. 참고로 나란히 적는다.", "",
  tab(o$ref_std[, .(연도 = year, 성별 = sex_lab, `조율 추정치 % (95% CI)` = sprintf("%.1f (%.1f–%.1f)", crude_estimate, crude_ci_low, crude_ci_high), `공식 연령표준화율 %` = f1(official_age_standardized))]),
  "- 국민건강통계의 조율(만 19세 이상) 공식값은 이번에도 출처를 찾지 못했다.", "",
  "### 10-2. 계산 검증", "", tab(o$val[, .(항목 = item, `비교한 값 수` = n_compared, `최대 차이` = sprintf("%.1e", max_abs_diff), 판정 = ifelse(pass, "일치", "불일치"))]),
  "### 10-3. 모형 진단", "",
  sprintf("- 적합한 모형 %d개: 수렴 실패 %d개, 추정되지 않은 계수가 있는 모형 %d개, 계수 절댓값이 8을 넘는 모형(분리) %d개, 경고 %d건.",
          nrow(o$diag), o$diag[converged == FALSE, .N], o$diag[n_na_coef > 0, .N], o$diag[max_abs_coef > 8, .N], nrow(o$warn_log)),
  if (o$diag[max_abs_coef > 8, .N]) sprintf("- 분리가 생긴 모형: %s. 모두 여자 홍조 소실군의 한 칸에 고요산혈증 사례가 0명이어서 생겼다. 이 모형의 OR과 p는 의미가 없어 결측으로 두었고, 본문 결과에는 쓰지 않았다.",
                                            paste(sub(" \\|.*$", "", o$diag[max_abs_coef > 8, where]), collapse = "; ")) else NULL,
  sprintf("- 설계 기반 잔차 자유도는 %d–%d이다.", min(o$diag$df_resid), max(o$diag$df_resid)),
  "- 재현성 확인(새 세션 2회 실행, CSV md5 비교) 결과는 같은 폴더의 _reproducibility.txt에 있다.", "")

# ============================== 11. 가정과 한계 ==============================
A("## 11. 가정, 한계, 확인하지 못한 것", "",
  "- **사후 분석**: 절단점 5·15잔, 2문항 분류, 추세 검정은 계획서 분석의 결과를 본 뒤에 추가했다. 확증이 아니라 탐색으로 읽는다.",
  "- **1잔의 알코올량**: 약 7 g은 지침서에 없는 문헌 가정이다. 주종을 구분하지 않으므로 같은 1잔의 알코올량이 다를 수 있다.",
  "- **고요산혈증 기준과 eGFR 식**: 지침서에 없는 문헌 기준이다.",
  "- **단면연구**: 음주와 요산 상승의 선후를 알 수 없다. 홍조 때문에 술을 끊은 사람은 대상에서 빠져 있다.",
  "- **자기보고**: 음주량과 홍조는 본인 응답이고, 과거 홍조는 회상이다. 2문항 분류의 타당도(민감도 90%, 특이도 88%)는 일본 남성 자료이며 국건영 문항의 응답 범주(가끔·자주·늘)와 다르다.",
  sprintf("- **작은 칸**: 여자 홍조 소실·고음주 %d명(사례 %d명), 폐경 후 홍조·고음주 %d명(사례 %d명) 등은 해석하지 않았다.", wl_hi$n, wl_hi$n_event, pm_hi$n, pm_hi$n_event),
  "- **공식 통계 대조**: 가중치 합계는 일치했다. 지표 수준의 대조는 연령표준화 여부 때문에 완결하지 못했다.", "",
  "## 12. 방법 관련 참고문헌 (PubMed에서 서지 확인)", "",
  "- Yokoyama T, Yokoyama A, Kato H, et al. Alcohol flushing, alcohol and aldehyde dehydrogenase genotypes, and risk for esophageal squamous cell carcinoma in Japanese men. Cancer Epidemiol Biomarkers Prev. 2003;12(11 Pt 1):1227-33. PMID 14652286.",
  "- Brooks PJ, Enoch MA, Goldman D, Li TK, Yokoyama A. The alcohol flushing response: an unrecognized risk factor for esophageal cancer from alcohol consumption. PLoS Med. 2009;6(3):e50. https://doi.org/10.1371/journal.pmed.1000050",
  "- Knol MJ, VanderWeele TJ. Recommendations for presenting analyses of effect modification and interaction. Int J Epidemiol. 2012;41(2):514-20. https://doi.org/10.1093/ije/dyr218",
  "- Peduzzi P, Concato J, Kemper E, Holford TR, Feinstein AR. A simulation study of the number of events per variable in logistic regression analysis. J Clin Epidemiol. 1996;49(12):1373-9. https://doi.org/10.1016/s0895-4356(96)00236-3",
  "- 질병관리청. 성인 음주행동 변화의 연령-기간-코호트 분석. 주간 건강과 질병. 2024;17(21):877-902. https://www.phwr.org/journal/view.html?doi=10.56786%2FPHWR.2024.17.21.1",
  "- 질병관리청. 국민건강영양조사 제8기(2019–2021) 원시자료 이용지침서.")
stopifnot(setequal(names(buf), c("a", "s5b", "s6", "z")))
mm <- c(buf$a, buf$s5b, buf$s6, buf$z)   # 0–5-5절 → 5-6–5-8절 → 6절(탐색적 분석) → 7절 이후
con <- file(file.path(out, "REPORT_5_15_정밀분석.md"), open = "w", encoding = "UTF-8"); writeLines(mm, con); close(con)
cat("완료:", file.path(out, "REPORT_5_15_정밀분석.md"), " (", length(mm), "줄 )\n")
cat("\n[p < 0.05 목록]\n"); print(sig[, .(구분, 결과, 대상, 모형, 항목, p = signif(p, 3))], nrows = 200)
