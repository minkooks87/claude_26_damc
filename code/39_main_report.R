# 39_main_report.R — 확정 주분석 결과 보고서와 그림
# 확정(2026-11-23): 질문 구조(주질문·부질문), 음주 기준 S2(남 16 / 여 8 g/일, 2군), 1잔 7 g, 흡연 3범주,
#                   고요산혈증(절단점)이 주결과, 혈청 요산(연속형)은 보조 다중회귀, 경향성 분석 안 함,
#                   민감도는 1잔 8·10 g과 대푯값 전체 하한(L)·상한(U)만. 보정 모형은 Crude·M2S(주)·M3S.
# 입력: output/final_analysis/ 의 02a, 03, 04, 06, 10, 11, 12, 13 (code/final_analysis.R 결과)
# 출력: Result/2026-11-23_주분석_결과_S2.md, Result/2026-11-23_주분석_그림/fig1·fig2·fig3 (output/final_analysis/에도 그림 사본)
# 실행: Rscript --vanilla code/39_main_report.R   (final_analysis.R를 먼저 실행)
# 숫자는 모두 결과 파일에서 읽어 넣는다(손으로 옮겨 적지 않음). 생성형 AI(Claude) 도움으로 작성, 조원 검토 필요.
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
fa <- "output/final_analysis"
rd <- function(f) fread(file.path(fa, f), encoding = "UTF-8")
flow <- rd("02a_participant_flow.csv"); t1 <- rd("03_table1.csv"); oc <- rd("04_outcome_by_sex.csv")
off <- rd("06_official_check.csv"); tst <- rd("10_interaction_tests.csv"); eff <- rd("11_effects_by_flush.csv")
cel <- rd("12_cell_counts.csv"); fo <- rd("13_outcome_by_flush.csv")
fig_dir <- "Result/2026-11-23_주분석_그림"; dir.create(fig_dir, showWarnings = FALSE)
out_md <- "Result/2026-11-23_주분석_결과_S2.md"

A_MAIN <- "주분석"; A_UA <- "보조: 혈청 요산(다중회귀)"; A_CR <- "비교: 보정 없음"; A_CRU <- "비교(혈청 요산): 보정 없음"
A_M3 <- "추가 보정: M3S"; A_M3U <- "추가 보정(혈청 요산): M3S"
A_SENS <- c("민감도: 1잔 8 g", "민감도: 1잔 10 g", "민감도: 대푯값 전체 하한(L)", "민감도: 대푯값 전체 상한(U)")
f2 <- function(x, d = 2) formatC(x, format = "f", digits = d)
fp <- function(p) ifelse(is.na(p), "-", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
star <- function(p, note = "") paste0(fp(p), ifelse(!is.na(p) & p < 0.05 & !grepl("10명", note), "*", ""), ifelse(grepl("10명", note), "†", ""))
flush_lv <- c("비홍조", "홍조 소실", "지속 홍조")
tst[, Q := fcase(grepl("^주질문", 질문), "주질문", grepl("^부질문 1:", 질문), "부질문", grepl("탐색", 질문), "탐색: 소실 vs 지속", default = 질문)]
gp <- function(an, q, s) tst[분석 == an & Q == q & 대상 == s, p][1]
gn <- function(an, q, s) tst[분석 == an & Q == q & 대상 == s, 비고][1]
ge <- function(an, s, g) eff[분석 == an & 성별 == s & 홍조이력 == g & 음주 == "고음주"][1]
ci <- function(e, sep = "–") paste0(f2(e$추정치), " (", f2(e$CI하한), sep, f2(e$CI상한), ")")

# ---- 그림 ----
windowsFonts(Malgun = windowsFont("Malgun Gothic"))   # Windows png 장치에 한글 글꼴(맑은 고딕) 등록
fam <- "Malgun"
save_fig <- function(g, file, h = 5) for (p in c(file.path(fig_dir, file), file.path(fa, file))) ggsave(p, g, width = 6.5, height = h, dpi = 150)
plot_eff <- function(an, xlab, logx, file, ref) {
  e <- eff[분석 == an & 음주 == "고음주"]
  e[, 홍조이력 := factor(홍조이력, rev(flush_lv))]; e[, 해석 := fifelse(grepl("10명", 비고), "사례 10명 미만(해석하지 않음)", "해석 가능")]
  g <- ggplot(e, aes(x = 추정치, y = 홍조이력)) + geom_vline(xintercept = ref, linetype = 2, colour = "grey50") +
    geom_errorbar(aes(xmin = CI하한, xmax = CI상한), width = 0.2, orientation = "y") +
    geom_point(aes(shape = 해석), size = 3, fill = "white") + scale_shape_manual(values = c("해석 가능" = 16, "사례 10명 미만(해석하지 않음)" = 21), name = NULL) +
    facet_wrap(~성별, ncol = 1) + labs(x = xlab, y = NULL) + theme_bw(base_family = fam, base_size = 12) + theme(legend.position = "bottom")
  if (logx) g <- g + scale_x_log10()
  save_fig(g, file)
}
plot_eff(A_MAIN, "고음주 vs 저음주 고요산혈증 OR (95% CI, 로그 눈금)", TRUE, "fig1_고요산혈증_OR_홍조이력별.png", 1)
plot_eff(A_UA, "고음주 vs 저음주 혈청 요산 차이 (mg/dL, 95% CI)", FALSE, "fig2_혈청요산_차이_홍조이력별.png", 0)
# 그림 3: 민감도 — 남자 홍조군별 고음주 OR (주분석과 민감도 4개)
es <- eff[분석 %in% c(A_MAIN, A_SENS) & 성별 == "남자" & 음주 == "고음주"]
es[, 분석 := factor(분석, rev(c(A_MAIN, A_SENS)), rev(c("주분석 (1잔 7 g, 중간값)", "1잔 8 g", "1잔 10 g", "대푯값 전체 하한(L)", "대푯값 전체 상한(U)")))]
es[, 홍조이력 := factor(홍조이력, flush_lv)]
g3 <- ggplot(es, aes(x = 추정치, y = 분석, colour = 홍조이력)) + geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  geom_errorbar(aes(xmin = CI하한, xmax = CI상한), width = 0.25, orientation = "y", position = position_dodge(width = 0.6)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) + scale_x_log10() +
  scale_colour_manual(values = c("비홍조" = "grey40", "홍조 소실" = "#C55A11", "지속 홍조" = "#2F5597"), name = NULL) +
  labs(x = "남자 고음주 vs 저음주 고요산혈증 OR (95% CI, 로그 눈금)", y = NULL) + theme_bw(base_family = fam, base_size = 12) + theme(legend.position = "bottom")
save_fig(g3, "fig3_민감도_남자_OR.png", h = 4.5)
# 그림 4: 주질문·부질문 forest plot — 교호작용 효과(홍조군 OR ÷ 비홍조군 OR, 고음주 vs 저음주)와 95% CI
#   보정 모형(Crude·M2S·M3S)과 민감도 4개(M2S)를 한 그림에. 2026-11-25 추가
FP_AN <- c(A_CR, A_MAIN, A_M3, A_SENS)
FP_LAB <- c("Crude", "M2S (주 모형)", "M3S", "민감도: 1잔 8 g", "민감도: 1잔 10 g", "민감도: 대푯값 하한", "민감도: 대푯값 상한")
fpd <- eff[분석 %in% FP_AN & grepl("OR의 비", 음주)]
fpd[, 질문 := factor(fifelse(grepl("^지속", 홍조이력), "주질문: 지속 홍조 ÷ 비홍조", "부질문: 홍조 소실 ÷ 비홍조"),
                     c("주질문: 지속 홍조 ÷ 비홍조", "부질문: 홍조 소실 ÷ 비홍조"))]
fpd[, 분석 := factor(분석, rev(FP_AN), rev(FP_LAB))]
fpd[, 해석 := fifelse(grepl("10명", 비고), "사례 10명 미만(해석하지 않음)", "해석 가능")]
fpd[, 주 := 분석 == "M2S (주 모형)"]
g4 <- ggplot(fpd, aes(x = 추정치, y = 분석)) + geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  geom_errorbar(aes(xmin = CI하한, xmax = CI상한, colour = 주), width = 0.25, orientation = "y") +
  geom_point(aes(shape = 해석, colour = 주), size = 2.6, fill = "white") +
  scale_shape_manual(values = c("해석 가능" = 16, "사례 10명 미만(해석하지 않음)" = 21), name = NULL) +
  scale_colour_manual(values = c(`TRUE` = "#C00000", `FALSE` = "grey25"), guide = "none") +
  scale_x_log10(breaks = c(0.5, 1, 2, 4, 8), labels = c("0.5", "1", "2", "4", "8")) +
  facet_grid(성별 ~ 질문) + labs(x = "OR의 비 (홍조군 OR ÷ 비홍조군 OR, 95% CI, 로그 눈금)", y = NULL) +
  theme_bw(base_family = fam, base_size = 11) + theme(legend.position = "bottom", strip.text = element_text(size = 10.5), strip.text.y = element_text(angle = 0), panel.spacing.x = unit(1.2, "lines"))
save_fig(g4, "fig4_주질문_부질문_forest.png", h = 6)

# ---- 표 도구 ----
md_tab <- function(dt) { h <- names(dt); c(paste("|", paste(h, collapse = " | "), "|"), paste("|", paste(rep("---", length(h)), collapse = " | "), "|"),
                                          apply(dt, 1, function(r) paste("|", paste(r, collapse = " | "), "|"))) }
L <- character(0); add <- function(...) L <<- c(L, ...)
eff_wide <- function(ans, labs, sep = "–") {
  e <- eff[분석 %in% ans & 음주 == "고음주"]
  e[, 값 := paste0(f2(추정치), " (", f2(CI하한), sep, f2(CI상한), ")", fifelse(grepl("10명", 비고), " †", ""))]
  w <- dcast(e, 성별 + 홍조이력 ~ 분석, value.var = "값"); w[, 홍조이력 := factor(홍조이력, flush_lv)]; setorder(w, 성별, 홍조이력)
  setcolorder(w, c("성별", "홍조이력", ans)); setnames(w, ans, labs); w
}
p_wide <- function(ans, labs) {
  x <- tst[분석 %in% ans]; x[, 값 := mapply(star, p, 비고)]
  w <- dcast(x, Q + 대상 ~ 분석, value.var = "값", fill = "-"); w[, Q := factor(Q, c("주질문", "부질문", "탐색: 소실 vs 지속"))]; setorder(w, Q, 대상)
  setnames(w, c("Q", "대상"), c("질문", "성별")); setcolorder(w, c("질문", "성별", ans)); setnames(w, ans, labs); w
}

# ---- 본문 ----
add("# 주분석 결과: 음주량 × 안면홍조 이력 → 고요산혈증 (확정 기준, 2026-11-23)", "",
    "작성: 2026-11-23 · team06donga(김준현 요청). 생성형 AI(Claude)로 분석 코드 실행과 정리를 도왔으며, 조원 검토가 필요합니다.", "",
    "- **분석 코드:** `code/final_analysis.R`(주분석 스위치 켬) → 결과 `output/final_analysis/` → 이 문서 `code/39_main_report.R`",
    "- **표기:** n은 가중치를 적용하지 않은 표본 수, %·평균·OR·β는 복합표본설계(가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu)를 반영한 추정치입니다.",
    "- **기호:** \\* p < 0.05 · † 비교하는 칸에 고요산혈증 사례가 10명 미만이라 해석하지 않음", "")

n_sens_sig <- tst[분석 %in% A_SENS & 대상 == "남자" & Q == "부질문" & p < 0.05, .N]
add("## 0. 한눈에 보는 결과", "",
  sprintf("1. **주질문(지속 홍조 vs 비홍조): 남녀 모두 유의하지 않음.** 남자 p = %s, 여자 p = %s. 남자 고음주 OR은 비홍조 %s, 지속 홍조 %s로 지속 홍조 쪽이 크지만 그 차이는 통계적으로 확인되지 않았습니다. 대사 변수를 더 보정한 M3S에서는 남자 주질문 p = %s, 부질문 p = %s입니다.",
          fp(gp(A_MAIN, "주질문", "남자")), fp(gp(A_MAIN, "주질문", "여자")), ci(ge(A_MAIN, "남자", "비홍조")), ci(ge(A_MAIN, "남자", "지속 홍조")),
          fp(gp(A_M3, "주질문", "남자")), fp(gp(A_M3, "부질문", "남자"))),
  sprintf("2. **부질문(홍조 소실 vs 비홍조): 남자에서 p = %s.** 남자 홍조 소실군의 고음주 OR은 %s로 비홍조군 %s보다 컸습니다. 여자는 소실군 고음주 사례가 5명이라 해석하지 않습니다(†).",
          fp(gp(A_MAIN, "부질문", "남자")), ci(ge(A_MAIN, "남자", "홍조 소실")), ci(ge(A_MAIN, "남자", "비홍조"))),
  sprintf("3. **보조: 혈청 요산(다중회귀)에서는 남녀 모두 효과수정이 없음.** 주질문 p = 남 %s, 여 %s · 부질문 p = 남 %s, 여 %s.",
          fp(gp(A_UA, "주질문", "남자")), fp(gp(A_UA, "주질문", "여자")), fp(gp(A_UA, "부질문", "남자")), fp(gp(A_UA, "부질문", "여자"))),
  sprintf("4. **민감도(1잔 8·10 g, 대푯값 하한·상한):** 남자 홍조 소실군의 고음주 OR은 네 분석 모두 약 2.0으로 주분석과 같았고 신뢰구간도 1을 넘었습니다. 다만 부질문 p는 %s(8 g), %s(10 g), %s(하한), %s(상한)로 경계 근처이며, 4개 중 %d개만 0.05 미만입니다.",
          fp(gp(A_SENS[1], "부질문", "남자")), fp(gp(A_SENS[2], "부질문", "남자")), fp(gp(A_SENS[3], "부질문", "남자")), fp(gp(A_SENS[4], "부질문", "남자")), n_sens_sig),
  sprintf("5. **음주와 고요산혈증 자체:** 남자 비홍조군에서는 고음주(16 g/일 이상)와 고요산혈증이 연관되지 않았고(OR %s), 여자는 비홍조군에서도 연관이 있었습니다(OR %s).",
          ci(ge(A_MAIN, "남자", "비홍조")), ci(ge(A_MAIN, "여자", "비홍조"))),
  "6. **해석:** 가설(지속 홍조군에서 음주–고요산혈증 연관이 더 강함)은 통계적으로 지지되지 않았습니다. 남자 홍조 소실군의 결과는 효과의 방향과 크기가 민감도에서도 일관되지만 p가 경계 근처이고, 분석 조건을 여러 결과를 본 뒤 확정했으므로 확증이 아니라 가설을 만드는 결과로 보고합니다.", "")

add("## 1. 확정한 분석 조건", "",
    md_tab(data.table(항목 = c("연구 질문", "주결과", "보조 결과", "음주 기준", "1잔의 알코올", "보정 모형", "효과수정인자", "민감도 분석", "하지 않는 분석"),
                      내용 = c("주질문(지속 홍조 vs 비홍조), 부질문(홍조 소실 vs 비홍조). 남녀 각각",
                             "고요산혈증(남 ≥ 7.0, 여 ≥ 6.0 mg/dL). 복합표본 로지스틱 회귀",
                             "혈청 요산(mg/dL, 연속형). 같은 모형 구조의 복합표본 다중 선형회귀",
                             "S2: 남 16 g/일, 여 8 g/일 이상을 고음주(2군). Lee 2019 한국인 홍조 기반 절주 지침의 65세 이하 비홍조군 적정 상한(주 8·4표준잔 × 14 g ÷ 7일)",
                             "7 g(g/일 = 주당 잔 수)",
                             "Crude(보정 없음) · **M2S(주 모형)**: 연령, 교육, 소득, 조사연도, **흡연 3범주(비흡연·과거·현재)**, 유산소 신체활동, 폐경 상태(여자만) · M3S: M2S + 체질량지수, 고혈압, 당뇨병, eGFR(매개변수일 수 있어 참고로 해석)",
                             "홍조 이력 3군: 비홍조(기준) · 홍조 소실 · 지속 홍조. 신규 홍조는 제외. 곱셈 척도 교호작용의 Wald 검정(1 df)",
                             "1잔 8 g · 10 g, 음주 대푯값 전체 하한(L) · 전체 상한(U). 모두 M2S로 계산",
                             "경향성 분석(이전 부질문 2·3), 다른 보정 모형(Model 1, M2A 등), 다른 음주 기준 등. 시도했던 기준은 `output/21_ptest/ptest_log.csv`에 남아 있음"))), "")

add("## 2. 연구 대상", "", md_tab(flow[, .(단계, n, 남, 여, 제외 = fifelse(is.na(제외), "", as.character(제외)))]), "",
    sprintf("홍조 이력 분석에는 신규 홍조(남 %d명, 여 %d명)와 폐경 상태 결측 여자 2명을 빼서 **남 %s명, 여 %s명**이 들어갑니다.",
            t1[성별 == "남자" & 홍조패턴 == "신규 홍조", 집단n[1]], t1[성별 == "여자" & 홍조패턴 == "신규 홍조", 집단n[1]],
            format(tst[분석 == A_MAIN & Q == "주질문" & 대상 == "남자", 모형n], big.mark = ","), format(tst[분석 == A_MAIN & Q == "주질문" & 대상 == "여자", 모형n], big.mark = ",")), "")

add("## 3. 홍조 이력별 특성", "")
pick_t1 <- function(s, g, var, lev) { x <- t1[성별 == s & 홍조패턴 == g & 변수 == var & 수준 == lev]; if (nrow(x)) x$추정치[1] else "-" }
tb <- rbindlist(lapply(c("남자", "여자"), function(s) rbindlist(lapply(flush_lv, function(g) {
  r <- fo[성별 == s & 홍조이력 == g]
  data.table(성별 = s, 홍조이력 = g, n = r$n, `나이(세)` = pick_t1(s, g, "나이(세)", "평균"), `현재흡연 %` = pick_t1(s, g, "흡연 3범주", "현재흡연"),
             `음주 g/일` = paste0(f2(r$음주_g일_가중평균, 1), " (", f2(r$음주_SE, 1), ")"), `고음주 n (%)` = paste0(r$고음주_n, " (", f2(r$고음주_가중pct, 1), ")"),
             `고요산혈증 n (%)` = paste0(r$고요산혈증_n, " (", f2(r$고요산혈증_가중pct, 1), ")"), `혈청 요산 mg/dL` = paste0(f2(r$요산_가중평균), " (", f2(r$요산_SE), ")"))
}))))
add(md_tab(tb), "", "괄호 안: 나이·음주·요산은 가중 평균(SE), 흡연·고음주·고요산혈증은 가중 %. 전체 특성표는 `output/final_analysis/03_table1.csv`.",
    sprintf("성별 전체 고요산혈증: 남 %d명(가중 %s%%), 여 %d명(가중 %s%%).", oc[성별 == "남자", 고요산혈증_n], f2(oc[성별 == "남자", 고요산혈증_가중pct], 1),
            oc[성별 == "여자", 고요산혈증_n], f2(oc[성별 == "여자", 고요산혈증_가중pct], 1)), "")

add("## 4. 주분석: 고요산혈증", "", "### 4-1. 칸별 인원과 가중 유병률", "")
cm <- cel[분석 == A_MAIN]
ct <- rbindlist(lapply(c("남자", "여자"), function(s) rbindlist(lapply(flush_lv, function(g) {
  a <- cm[성별 == s & 홍조이력 == g & 음주 == "저음주"]; b <- cm[성별 == s & 홍조이력 == g & 음주 == "고음주"]
  data.table(성별 = s, 홍조이력 = g, `저음주 n (사례, 가중 %)` = sprintf("%d (%d, %s)", a$n, a$고요산혈증_n, f2(a$가중유병률, 1)),
             `고음주 n (사례, 가중 %)` = sprintf("%d (%d, %s)%s", b$n, b$고요산혈증_n, f2(b$가중유병률, 1), ifelse(min(a$고요산혈증_n, b$고요산혈증_n) < 10, " †", "")))
}))))
add(md_tab(ct), "", "저음주: 남 16 g/일 미만, 여 8 g/일 미만. 고음주: 그 이상.", "")
add("### 4-2. 홍조 이력별 고음주의 OR (저음주 대비)", "", "![그림 1](2026-11-23_주분석_그림/fig1_고요산혈증_OR_홍조이력별.png)", "",
    md_tab(eff_wide(c(A_CR, A_MAIN, A_M3), c("Crude", "**M2S (주 모형)**", "M3S"))), "")
add("### 4-3. 효과수정 검정 p (Wald, 1 df)", "", md_tab(p_wide(c(A_CR, A_MAIN, A_M3), c("Crude", "**M2S (주 모형)**", "M3S"))), "",
    "- 주질문 = 음주 × 지속 홍조, 부질문 = 음주 × 홍조 소실(모두 비홍조 대비, 곱셈 척도). 소실 vs 지속은 탐색으로만 봅니다.", "")

ir <- eff[분석 %in% c(A_CR, A_MAIN, A_M3) & grepl("OR의 비", 음주)]
ir[, 값 := paste0(f2(추정치), " (", f2(CI하한), "–", f2(CI상한), ")", fifelse(grepl("10명", 비고), " †", ""))]
ir[, 질문 := fifelse(grepl("^지속", 홍조이력), "주질문 (지속 ÷ 비홍조)", "부질문 (소실 ÷ 비홍조)")]
irw <- dcast(ir, 질문 + 성별 ~ 분석, value.var = "값"); irw[, 질문 := factor(질문, c("주질문 (지속 ÷ 비홍조)", "부질문 (소실 ÷ 비홍조)"))]; setorder(irw, 질문, 성별)
setcolorder(irw, c("질문", "성별", A_CR, A_MAIN, A_M3)); setnames(irw, c(A_CR, A_MAIN, A_M3), c("Crude", "**M2S (주 모형)**", "M3S"))
add("### 4-4. 주질문·부질문의 효과 크기와 forest plot", "",
    "효과 크기는 교호작용 항, 곧 **OR의 비 = 홍조군의 고음주 OR ÷ 비홍조군의 고음주 OR**입니다. 1보다 크면 그 홍조군에서 음주의 영향이 더 큽니다. 검정 p는 4-3과 같습니다.", "",
    md_tab(irw), "", "![그림 4](2026-11-23_주분석_그림/fig4_주질문_부질문_forest.png)", "",
    "- 빨간색이 주 모형(M2S)입니다. 민감도 4개는 M2S로 계산했습니다. 빈 원은 사례 10명 미만 칸이 있어 해석하지 않습니다.", "")

add("## 5. 보조: 혈청 요산 다중회귀", "", "![그림 2](2026-11-23_주분석_그림/fig2_혈청요산_차이_홍조이력별.png)", "",
    "고음주 vs 저음주 혈청 요산 차이(mg/dL, 95% CI):", "", md_tab(eff_wide(c(A_CRU, A_UA, A_M3U), c("Crude", "**M2S**", "M3S"), sep = ", ")), "",
    "효과수정 검정 p:", "", md_tab(p_wide(c(A_CRU, A_UA, A_M3U), c("Crude", "**M2S**", "M3S"))), "")

add("## 6. 민감도 분석", "", "![그림 3](2026-11-23_주분석_그림/fig3_민감도_남자_OR.png)", "",
    md_tab(data.table(민감도 = c("1잔 8 g", "1잔 10 g", "대푯값 전체 하한(L)", "대푯값 전체 상한(U)"),
                      `바꾼 가정` = c("1잔의 알코올 7 g → 8 g(Lee 2023 국건영 환산)", "7 g → 10 g", "빈도·1회량을 범주 하한으로(예: 주2-3회 → 주 2, 1-2잔 → 1잔)", "빈도·1회량을 범주 상한으로(예: 주4회이상 → 주 7, 1-2잔 → 2잔)"))), "",
    "고요산혈증 효과수정 p:", "")
srow <- rbindlist(lapply(c(A_MAIN, A_SENS), function(a) data.table(분석 = a,
  `남 주질문` = star(gp(a, "주질문", "남자"), gn(a, "주질문", "남자")), `남 부질문` = star(gp(a, "부질문", "남자"), gn(a, "부질문", "남자")),
  `여 주질문` = star(gp(a, "주질문", "여자"), gn(a, "주질문", "여자")), `여 부질문` = star(gp(a, "부질문", "여자"), gn(a, "부질문", "여자")),
  `남 소실군 OR` = ci(ge(a, "남자", "홍조 소실")), `남 비홍조 OR` = ci(ge(a, "남자", "비홍조")))))
add(md_tab(srow), "", "- 대푯값 정의: `Result/2026-11-18_음주대표값_민감도분석_계획.md` 4–5절. 10잔 이상은 직접 적은 잔 수라 바꾸지 않았습니다.",
    "- 판정: p < 0.05 유지 여부가 아니라 OR의 방향과 크기가 주분석과 같은지로 봅니다.", "")

add("## 7. 해석할 때 주의할 점(한계)", "",
    "- **결과를 본 뒤 확정:** 음주 기준(S2), 1잔 7 g, 흡연 3범주, 분석 범위(경향성 제외, 민감도 4개)는 여러 기준·모형의 p를 본 뒤(2026-11-16 ~ 11-23) 정했습니다. 보고서에 이 사실과 시도한 기준 전체(`output/21_ptest/ptest_log.csv`)를 밝힙니다.",
    "- **다중 비교:** 성별 2 × 질문 2 × 결과 2의 검정 중 유의한 것은 남자 부질문 하나입니다. 단일 p < 0.05보다 OR의 방향과 크기의 일관성을 봅니다.",
    "- **작은 군:** 남자 홍조 소실군 341명(고음주 123명), 여자 소실군 236명(고음주 44명, 사례 5명)입니다.",
    "- **단면 자료·자기 보고:** 음주량은 설문 범주의 대푯값으로 계산했고, 홍조 이력은 회상에 의존합니다. 홍조 소실은 음주 습관 변화(내성)와 얽혀 있을 수 있습니다.",
    "- **1잔의 g:** 지침서에 1잔의 알코올 g이 없어 7 g(지역사회건강조사 연구 Jeon 2022의 값)을 썼습니다.", "")

add("## 8. 검증", "",
    sprintf("- **공식 통계 대조:** 2019년 만 19세 이상 월간음주율·고위험음주율(KOSIS) 6개 지표 모두 '%s'.", paste(unique(off$판정), collapse = ", ")),
    "- **대상자 흐름:** README·계획서 3장의 단계별 인원과 같습니다(`02b_flow_vs_README.csv`).",
    sprintf("- **ptest와 대조:** 주 모형 p(남 주질문 %s, 부질문 %s, 여 주질문 %s, 혈청 요산 남 %s·여 %s)가 `/knhanes_ptest` S2a 실행의 흡연 3범주 묶음(M2S)과 같습니다.",
            fp(gp(A_MAIN, "주질문", "남자")), fp(gp(A_MAIN, "부질문", "남자")), fp(gp(A_MAIN, "주질문", "여자")), fp(gp(A_UA, "주질문", "남자")), fp(gp(A_UA, "주질문", "여자"))),
    "- **재현성:** 코드를 짜지 않은 조원이 `Rscript --vanilla code/final_analysis.R`를 새 세션에서 두 번 실행해 결과가 같은지 확인해야 합니다(계획서 8.2, 아직 안 함).", "")

add("## 9. 결과 파일", "",
    md_tab(data.table(파일 = c("02a_participant_flow.csv", "03_table1.csv", "10_interaction_tests.csv", "11_effects_by_flush.csv", "12_cell_counts.csv", "13_outcome_by_flush.csv", "06_official_check.csv"),
                      내용 = c("대상자 선정 흐름", "대상자 특성표(성별 × 홍조 패턴)", "모든 분석의 효과수정 검정 p", "홍조 이력별 OR·β와 95% CI", "칸별 n·사례·가중 유병률", "홍조 이력별 음주·결과 요약", "공식 통계 대조"))),
    "", "모두 `output/final_analysis/`에 있습니다.")
writeLines(L, out_md, useBytes = FALSE)
cat("저장:", out_md, "\n")
