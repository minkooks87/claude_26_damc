# 14_v3_results_figures.R
# 목적: 13_v3_main_analysis.R 의 결과 CSV로 보고서용 그림(PNG)을 만든다.
#       원시자료(data/)는 읽지 않고, output/13_v3_main_analysis/ 의 CSV만 읽는다.
# 실행: Rscript --vanilla code/14_v3_results_figures.R   (프로젝트 폴더에서)
# 그림: fig/fig1_participant_flow.png   대상자 선정 흐름
#       fig/fig2_main_forest.png        주분석: 현재 홍조군별 음주 수준(저음주 대비) OR, 확장 모형
#       fig/fig3_history_forest.png     홍조 이력 3군별 음주 수준 OR, 확장 모형
#       fig/fig4_menopause_forest.png   여성 폐경 전·후 × 현재 홍조별 음주 수준 OR
#       fig/fig5a_sensitivity_men.png, fig5b_sensitivity_women.png  민감도 분석(가장 높은 노출 수준의 OR)
#       fig/fig6_prevalence.png         고요산혈증 가중 유병률(95% CI): 성별 × 현재 홍조 × 음주 수준

suppressPackageStartupMessages(library(data.table))

in_dir  <- file.path("output", "13_v3_main_analysis")
fig_dir <- file.path(in_dir, "fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")
big <- function(x) format(x, big.mark = ",", trim = TRUE)

# 한글 글꼴: Windows 에서는 맑은 고딕을 등록해서 쓴다
fam <- "sans"
if (.Platform$OS.type == "windows") { windowsFonts(kr = windowsFont("Malgun Gothic")); fam <- "kr" }

# 색: dataviz 참조 팔레트의 범주형 슬롯 1·2·3(밝은 모드). 비홍조 = 파랑, 홍조·지속 홍조 = 주황, 홍조 소실 = 아쿠아.
#     검증 스크립트(node)는 이 PC에 없어, 팔레트 문서에 '인접 쌍 통과'로 적힌 처음 세 슬롯을 순서대로 쓴다.
#     아쿠아는 밝은 바탕에서 대비가 낮아, 모든 점 옆에 수치 라벨을 직접 적는다(relief rule).
pal <- c("#2a78d6", "#eb6834", "#1baf7a")
col_grid <- "#e1e0d9"; col_axis <- "#c3c2b7"; col_txt <- "#0b0b0b"; col_sub <- "#52514e"; col_mut <- "#898781"

open_png <- function(name, w, h) {
  png(file.path(fig_dir, name), width = w, height = h, units = "in", res = 200, family = fam, bg = "white")
  par(family = fam)
}

# ---- 포레스트 플롯: dat = data.table(row, series, est, lo, hi, label) ----
forest_plot <- function(dat, ser, cols, xlim, ticks, xlab, right_lab, log = TRUE, mar_right = 14.5, cex_lab = 0.76, title = NULL, sub = NULL) {
  rows <- unique(dat$row); nr <- length(rows); ns <- length(ser)
  # 제목·부제목은 바깥 여백(oma)에, 범례는 안쪽 위 여백에 둔다 → 서로 겹치지 않는다
  par(oma = c(0, 0, 2.8, 0), mar = c(3.8, 11.5, 2.4, mar_right), xaxs = "i", yaxs = "i")
  plot(NA, xlim = xlim, ylim = c(0.5, nr + 0.5), log = if (log) "x" else "", axes = FALSE, xlab = "", ylab = "")
  abline(v = ticks, col = col_grid, lwd = 1)
  abline(v = if (log) 1 else 0, col = col_axis, lwd = 1.3)
  axis(1, at = ticks, labels = format(ticks, drop0trailing = TRUE), col = NA, col.ticks = NA, col.axis = col_mut, cex.axis = 0.8, line = -0.5)
  mtext(xlab, side = 1, line = 1.9, cex = 0.8, col = col_mut)
  axis(2, at = rev(seq_len(nr)), labels = rows, las = 1, tick = FALSE, col.axis = col_txt, cex.axis = 0.88, line = -0.5)
  mtext(right_lab, side = 4, at = nr + 0.5, line = 0.6, las = 1, adj = 0, cex = 0.74, col = col_mut)
  off <- if (ns == 3) c(0.27, 0, -0.27) else if (ns == 2) c(0.18, -0.18) else 0
  for (i in seq_len(nr)) for (j in seq_len(ns)) {
    r <- dat[row == rows[i] & series == ser[j]]
    if (nrow(r) != 1 || is.na(r$est)) next
    y  <- nr - i + 1 + off[j]
    lo <- max(r$lo, xlim[1]); hi <- min(r$hi, xlim[2])
    segments(lo, y, hi, y, col = cols[j], lwd = 2.2, lend = 1)
    if (r$lo < xlim[1]) arrows(if (log) lo * 1.3 else lo + diff(xlim) * 0.03, y, lo, y, length = 0.06, col = cols[j], lwd = 2.2)
    if (r$hi > xlim[2]) arrows(if (log) hi / 1.3 else hi - diff(xlim) * 0.03, y, hi, y, length = 0.06, col = cols[j], lwd = 2.2)
    points(r$est, y, pch = 21, bg = cols[j], col = "white", lwd = 1.6, cex = 1.4)   # 흰 테두리 = 겹침 방지용 표면 링
    mtext(r$label, side = 4, at = y, line = 0.6, las = 1, adj = 0, cex = cex_lab, col = col_sub)
  }
  # 범례: 그림 영역 위쪽 약 0.9줄 높이에 (inset 은 그림 영역 높이에 대한 비율이므로 줄 높이를 환산한다)
  yoff <- 0.9 * par("cin")[2] * par("lheight") / par("pin")[2]
  legend("topleft", inset = c(0, -yoff), horiz = TRUE, legend = ser, col = cols, lwd = 2.2, pch = 16, pt.cex = 1.1,
         bty = "n", cex = 0.85, xpd = NA, text.col = col_sub, y.intersp = 0.8)
  if (!is.null(title)) mtext(title, side = 3, line = 1.4, adj = 0, cex = 0.95, col = col_txt, font = 2, outer = TRUE)
  if (!is.null(sub)) mtext(sub, side = 3, line = 0.3, adj = 0, cex = 0.78, col = col_sub, outer = TRUE)
}
lab_or <- function(est, lo, hi, n, ev = NULL) {
  ci <- ifelse(lo < 0 | hi < 0, sprintf("%.2f (%.2f, %.2f)", est, lo, hi), sprintf("%.2f (%.2f–%.2f)", est, lo, hi))
  if (is.null(ev)) sprintf("%s, n %s", ci, big(n)) else sprintf("%s, n %s (사례 %s)", ci, big(n), big(ev))
}

# ==== fig1. 대상자 선정 흐름 ===============================================
flow <- rd("02_participant_flow.csv")
short <- c("기본DB 전체 (2019–2021)", "가중치 있음 (검진·설문 참여)", "만 19세 이상", "최근 1년 음주자",
           "음주량 계산 가능 · 과거/현재 홍조 응답 · 요산 측정", "통풍 의사진단 없음 (모름·무응답 제외)", "보정변수 모두 있음 (최종 분석 대상)")
stopifnot(nrow(flow) == length(short))
open_png("fig1_participant_flow.png", 7.6, 7.2)
par(mar = c(0.4, 0.4, 1.6, 0.4))
plot(NA, xlim = c(0, 10), ylim = c(0.3, nrow(flow) + 0.6), axes = FALSE, xlab = "", ylab = "")
ys <- rev(seq_len(nrow(flow)))
for (i in seq_len(nrow(flow))) {
  y <- ys[i]; last <- i == nrow(flow)
  rect(0.5, y - 0.37, 6.3, y + 0.37, col = if (last) "#e3eefb" else "#f1efe8", border = if (last) pal[1] else col_axis, lwd = if (last) 1.6 else 1)
  text(0.7, y + 0.1, short[i], adj = c(0, 0.5), cex = 0.8, col = col_txt, font = if (last) 2 else 1)
  text(0.7, y - 0.17, sprintf("n = %s  (남 %s · 여 %s)", big(flow$n[i]), big(flow$n_men[i]), big(flow$n_women[i])), adj = c(0, 0.5), cex = 0.76, col = col_sub)
  if (i > 1) {
    arrows(3.4, y + 1 - 0.37, 3.4, y + 0.39, length = 0.07, col = col_mut, lwd = 1.2)
    text(6.6, y + 0.5, sprintf("제외 %s명", big(flow$excluded[i])), adj = c(0, 0.5), cex = 0.78, col = col_sub)
  }
}
mtext("그림 1. 대상자 선정 흐름 (n은 비가중 표본 수)", side = 3, line = 0.2, adj = 0, cex = 0.95, col = col_txt, font = 2)
invisible(dev.off())

# ==== fig2. 주분석: 현재 홍조군별 음주 수준 OR (확장 모형) ===================
a5 <- rd("05a_main_joint_effects.csv"); t5 <- rd("05b_main_interaction_tests.csv")
hua <- "고요산혈증(OR)"; M2 <- "M2 확장"
m <- a5[outcome == hua & model == M2 & exposure != "저음주"]
m[, `:=`(row = paste(sex, exposure, sep = " · "), series = modifier, est = strat_est, lo = strat_lo, hi = strat_hi)]
m[, label := lab_or(est, lo, hi, n, n_event)]
m[, row := factor(row, c("남자 · 중음주", "남자 · 고음주", "여자 · 중음주", "여자 · 고음주"))]; setorder(m, row)
m[, row := as.character(row)]
p_int <- t5[outcome == hua & model == M2, .(sex, p)]
open_png("fig2_main_forest.png", 10, 4.6)
forest_plot(m, ser = c("비홍조", "홍조"), cols = pal[1:2], xlim = c(0.4, 8), ticks = c(0.5, 1, 2, 4, 8),
            xlab = "저음주(<3.5잔/주) 대비 고요산혈증 오즈비 (로그 눈금). 세로선 1 = 차이 없음", right_lab = "OR (95% CI), n (사례)",
            title = "그림 2. 현재 홍조군별 음주 수준과 고요산혈증: 확장 모형",
            sub = sprintf("음주 수준 × 현재 홍조 상호작용 p: 남자 %.2f, 여자 %.2f  (연령·교육·소득·연도·흡연·신체활동·BMI·고혈압·당뇨병·eGFR 보정)",
                          p_int[sex == "남자", p], p_int[sex == "여자", p]))
invisible(dev.off())

# ==== fig3. 홍조 이력 3군별 음주 수준 OR (확장 모형) ==========================
a6 <- rd("06a_history_joint_effects.csv"); t6 <- rd("06b_history_tests.csv")
h <- a6[outcome == hua & model == M2 & exposure != "저음주"]
h[, `:=`(row = paste(sex, exposure, sep = " · "), series = modifier, est = strat_est, lo = strat_lo, hi = strat_hi)]
h[, label := lab_or(est, lo, hi, n, n_event)]
h[, row := factor(row, c("남자 · 중음주", "남자 · 고음주", "여자 · 중음주", "여자 · 고음주"))]; setorder(h, row); h[, row := as.character(row)]
pj <- t6[outcome == hua & model == M2 & grepl("공동", test), .(sex, p)]
open_png("fig3_history_forest.png", 10, 5.4)
forest_plot(h, ser = c("비홍조", "홍조 소실", "지속 홍조"), cols = pal[c(1, 3, 2)], xlim = c(0.2, 64), ticks = c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64),
            xlab = "저음주 대비 고요산혈증 오즈비 (로그 눈금). 신뢰구간이 눈금 밖이면 화살표", right_lab = "OR (95% CI), n (사례)",
            title = "그림 3. 홍조 이력 3군별 음주 수준과 고요산혈증: 확장 모형 (신규 홍조 제외)",
            sub = sprintf("음주 수준 × 홍조 이력 공동 상호작용 p: 남자 %.2f, 여자 %.2f. 여자 소실군 고음주는 17명(사례 2명)이라 해석하지 않는다",
                          pj[sex == "남자", p], pj[sex == "여자", p]))
invisible(dev.off())

# ==== fig4. 여성 폐경 전·후 × 현재 홍조 =====================================
a8 <- rd("08a_women_menopause_cells.csv"); t8 <- rd("08b_women_menopause_tests.csv")
w <- a8[outcome == hua & exposure != "저음주"]
w[, `:=`(row = paste(menopause, exposure, sep = " · "), series = modifier, est = strat_est, lo = strat_lo, hi = strat_hi)]
w[, label := lab_or(est, lo, hi, n, n_event)]
w[, row := factor(row, c("폐경 전 · 중음주", "폐경 전 · 고음주", "폐경 후 · 중음주", "폐경 후 · 고음주"))]; setorder(w, row); w[, row := as.character(row)]
pw <- t8[outcome == hua & !grepl("3원", test), .(menopause, p)]
p3 <- t8[outcome == hua & grepl("3원", test), p]
open_png("fig4_menopause_forest.png", 10, 4.6)
forest_plot(w, ser = c("비홍조", "홍조"), cols = pal[1:2], xlim = c(0.2, 64), ticks = c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64),
            xlab = "저음주 대비 고요산혈증 오즈비 (로그 눈금, 여성, 확장 모형). 탐색적 분석", right_lab = "OR (95% CI), n (사례)",
            title = "그림 4. 여성 폐경 전·후, 현재 홍조군별 음주 수준과 고요산혈증 (탐색)",
            sub = sprintf("상호작용 p: 폐경 전 %.2f, 폐경 후 %.3f; 음주 × 홍조 × 폐경 3원 상호작용 p %.2f. 폐경 후 홍조·고음주는 23명(사례 6명)",
                          pw[menopause == "폐경 전", p], pw[menopause == "폐경 후", p], p3))
invisible(dev.off())

# ==== fig5. 민감도 분석: 가장 높은 노출 수준의 OR (남·여 따로) ==================
s9 <- rd("09_sensitivity.csv")
top_lv <- c(S5 = "주1회 이상", S7 = "T3", S10 = "월1회 초과")
s9[, top := ifelse(id %in% names(top_lv), top_lv[id], "고음주")]
short_lab <- c(S0 = "S0 주분석(확장 모형)", S1 = "S1 통풍 진단자 포함", S2 = "S2 통풍 무응답 포함", S3a = "S3a 대표값 작게",
               S3b = "S3b 대표값 크게", S4 = "S4 2020년 제외", S5 = "S5 노출=폭음 빈도(주1회 이상)", S6 = "S6 홍조='자주·늘'만",
               S7 = "S7 노출=성별 삼분위(T3)", S8 = "S8 고혈압·당뇨 의사진단 보정", S10 = "S10 노출=빈도 2군(월1회 초과)")
ss <- s9[exposure == top]
ss[, `:=`(row = short_lab[id], series = modifier, est = strat_est, lo = strat_lo, hi = strat_hi)]
ss[, label := lab_or(est, lo, hi, n, n_event)]
ss[, row := factor(row, short_lab)]; setorder(ss, sex, row); ss[, row := as.character(row)]
for (sx in c("남자", "여자")) {
  open_png(if (sx == "남자") "fig5a_sensitivity_men.png" else "fig5b_sensitivity_women.png", 10, 6.4)
  forest_plot(ss[sex == sx], ser = c("비홍조", "홍조"), cols = pal[1:2], xlim = c(0.25, 16), ticks = c(0.25, 0.5, 1, 2, 4, 8, 16),
              xlab = "기준 수준(저음주 / 폭음 없음 / T1 / 월1회 이하) 대비 고요산혈증 오즈비 (로그 눈금)", right_lab = "OR (95% CI), n (사례)",
              title = sprintf("그림 5%s. 민감도 분석 (%s): 가장 높은 노출 수준의 OR, 현재 홍조군별", if (sx == "남자") "a" else "b", sx),
              sub = sprintf("상호작용 p 범위: %.2f–%.2f (어느 분석에서도 0.05 미만 아님)", min(ss[sex == sx, p_interaction]), max(ss[sex == sx, p_interaction])))
  invisible(dev.off())
}

# ==== fig6. 고요산혈증 가중 유병률: 성별 × 현재 홍조 × 음주 수준 ==============
c3 <- rd("03_cell_counts.csv")[group_var == "flush_now"]
c3[, `:=`(row = paste(sex, drink, sep = " · "), series = group, est = hua_pct_wt, lo = hua_pct_wt - 1.96 * hua_se_wt, hi = hua_pct_wt + 1.96 * hua_se_wt)]
c3[, label := sprintf("%.1f%% (%.1f–%.1f), n %s (사례 %s)", est, lo, hi, big(n), big(n_hua))]
c3[, row := factor(row, c("남자 · 저음주", "남자 · 중음주", "남자 · 고음주", "여자 · 저음주", "여자 · 중음주", "여자 · 고음주"))]; setorder(c3, row); c3[, row := as.character(row)]
open_png("fig6_prevalence.png", 10, 5.2)
forest_plot(c3, ser = c("비홍조", "홍조"), cols = pal[1:2], xlim = c(0, 35), ticks = seq(0, 35, 5), log = FALSE,
            xlab = "고요산혈증 가중 유병률 (%) 와 95% 신뢰구간 (보정 전)", right_lab = "% (95% CI), n (사례)",
            title = "그림 6. 고요산혈증 가중 유병률: 성별 × 현재 홍조 × 음주 수준",
            sub = "남 ≥7.0, 여 ≥6.0 mg/dL. 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu")
invisible(dev.off())

cat("완료:", fig_dir, "\n")
