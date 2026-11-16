# 07_meal_bedtime_figures.R
# 목적: 02_meal_bedtime_cardiometabolic.R 의 결과(CSV)로 보고서용 그림(PNG)을 만든다.
#       원시자료(data/)는 읽지 않고 output/02_meal_bedtime/ 의 CSV만 읽는다.
# 실행: Rscript --vanilla code/07_meal_bedtime_figures.R   (프로젝트 폴더에서)
# 그림: fig/fig1_participant_flow.png      대상자 선정 흐름
#       fig/fig2_exposure_distribution.png 마지막 식사–취침 간격 분포(가중 %)
#       fig/fig3_forest_main.png           본 분석: 결과변수별 간격 범주의 효과(모형 1·2)
#       fig/fig4_forest_sensitivity.png    민감도 분석: 1시간 미만 vs 3시간 이상

suppressPackageStartupMessages(library(data.table))

in_dir  <- file.path("output", "02_meal_bedtime")
fig_dir <- file.path(in_dir, "fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")

fam <- "sans"
if (.Platform$OS.type == "windows") { windowsFonts(kr = windowsFont("Malgun Gothic")); fam <- "kr" }

# 색: 본 분석(모형 2)은 파랑, 비교용(모형 1)은 회색(강조 형식). 축·격자는 옅은 회색.
col_main <- "#2a78d6"; col_ctx <- "#898781"
col_grid <- "#e1e0d9"; col_axis <- "#c3c2b7"; col_txt <- "#0b0b0b"; col_sub <- "#52514e"; col_mut <- "#898781"

open_png <- function(name, w, h) {
  png(file.path(fig_dir, name), width = w, height = h, units = "in", res = 200, family = fam)
  par(family = fam)
}
lv_int <- c("<1시간", "1-<2시간", "2-<3시간", "3시간 이상")

# ---- 그림 1. 대상자 선정 흐름 ----
flow <- rd("02_participant_flow.csv")
open_png("fig1_participant_flow.png", 7.5, 8.5)
par(mar = c(0.3, 0.3, 0.3, 0.3))
n <- nrow(flow)
plot(NA, xlim = c(0, 10), ylim = c(0.4, n + 0.6), axes = FALSE, xlab = "", ylab = "")
for (i in seq_len(n)) {
  y <- n - i + 1
  rect(0.4, y - 0.36, 7.0, y + 0.36, col = "#f1efe8", border = col_axis)
  text(3.7, y + 0.10, flow$step[i], cex = 0.78, col = col_txt)
  text(3.7, y - 0.16, sprintf("n = %s", format(flow$n[i], big.mark = ",")), cex = 0.85, col = col_txt)
  if (i > 1) {
    arrows(3.7, y + 1 - 0.36, 3.7, y + 0.36, length = 0.07, col = col_sub)
    text(7.3, y + 0.5, sprintf("제외 %s명", format(flow$excluded[i], big.mark = ",")), cex = 0.78, col = col_sub, adj = 0)
  }
}
dev.off()

# ---- 그림 2. 노출 분포 ----
ex <- rd("03_exposure_distribution.csv")
ex[, level := factor(level, lv_int)]; setorder(ex, level)
ex[, pct := as.numeric(sub(" .*", "", weighted_pct))]
open_png("fig2_exposure_distribution.png", 7, 4.2)
par(mar = c(4, 4.5, 1.5, 1))
bp <- barplot(ex$pct, names.arg = as.character(ex$level), col = col_main, border = NA, ylim = c(0, 90),
              axes = FALSE, cex.names = 0.9, width = 0.6, space = 0.6)
abline(h = seq(20, 80, 20), col = col_grid)
barplot(ex$pct, col = col_main, border = NA, add = TRUE, axes = FALSE, axisnames = FALSE, width = 0.6, space = 0.6)
axis(2, at = seq(0, 80, 20), las = 1, col = NA, col.ticks = NA, col.axis = col_mut, cex.axis = 0.85)
mtext("가중 % (SE)", side = 2, line = 2.8, cex = 0.85, col = col_mut)
mtext("마지막 식사–취침 간격", side = 1, line = 2.6, cex = 0.85, col = col_mut)
text(bp, ex$pct + 4, sprintf("%s\nn = %s", ex$weighted_pct, format(ex$n, big.mark = ",")), cex = 0.75, col = col_sub)
dev.off()

# ---- 포레스트 패널: dat = data.table(row, series, est, lo, hi) ----
forest_panel <- function(dat, title, xlab, log = FALSE, cols, series_order = NULL) {
  rows <- unique(dat$row); nr <- length(rows)
  ser  <- if (is.null(series_order)) unique(dat$series) else intersect(series_order, unique(dat$series)); ns <- length(ser)
  ref  <- if (log) 1 else 0
  if (log) {
    xlim <- c(min(c(dat$lo, ref)) / 1.15, max(c(dat$hi, ref)) * 1.15)
    cand <- c(0.25, 0.5, 1, 2, 4, 8); ticks <- cand[cand >= xlim[1] & cand <= xlim[2]]
  } else {
    xl <- range(c(dat$lo, dat$hi, ref)); pad <- 0.08 * diff(xl); xlim <- xl + c(-pad, pad)
    ticks <- pretty(xlim, 5); ticks <- ticks[ticks >= xlim[1] & ticks <= xlim[2]]
  }
  # 왼쪽 여백은 가장 긴 행 이름의 폭(한글 1, 영문 0.55, 공백 0.35 em)에 맞춘다
  em_len <- function(s) { ch <- strsplit(s, "")[[1]]; sum(ifelse(grepl("[가-힣]", ch), 1, ifelse(ch == " ", 0.35, 0.55))) }
  left <- 2 + 0.82 * max(vapply(rows, em_len, numeric(1)))
  par(mar = c(3.6, max(8, left), 2.6, 9.5), xaxs = "i", yaxs = "i")
  plot(NA, xlim = xlim, ylim = c(0.5, nr + 0.5), log = if (log) "x" else "", axes = FALSE, xlab = "", ylab = "")
  title(main = title, cex.main = 0.95, font.main = 1, col.main = col_txt, line = 1.0)
  abline(v = ticks, col = col_grid); abline(v = ref, col = col_axis, lwd = 1.3)
  axis(1, at = ticks, labels = format(ticks, drop0trailing = TRUE), col = NA, col.ticks = NA, col.axis = col_mut, cex.axis = 0.78, line = -0.5)
  mtext(xlab, side = 1, line = 1.7, cex = 0.72, col = col_mut)
  axis(2, at = rev(seq_len(nr)), labels = rows, las = 1, tick = FALSE, col.axis = col_txt, cex.axis = 0.82, line = -0.5)
  mtext("추정치 (95% CI)", side = 4, at = nr + 0.5, line = 0.5, las = 1, adj = 0, cex = 0.68, col = col_mut)
  off <- if (ns == 2) c(0.18, -0.18) else 0
  for (i in seq_len(nr)) for (j in seq_len(ns)) {
    r <- dat[row == rows[i] & series == ser[j]]
    if (nrow(r) != 1) next
    y <- nr - i + 1 + off[j]
    segments(r$lo, y, r$hi, y, col = cols[j], lwd = 2.2, lend = 1)
    points(r$est, y, pch = 21, bg = cols[j], col = "white", lwd = 1.6, cex = 1.35)
    mtext(sprintf("%.2f (%.2f, %.2f)", r$est, r$lo, r$hi), side = 4, at = y, line = 0.5, las = 1, adj = 0, cex = 0.7, col = col_sub)
  }
}

outs <- list(c("공복혈당(mg/dL)", "공복혈당: 차이 (mg/dL)", "FALSE"),
             c("중성지방",        "중성지방: % 차이",        "FALSE"),
             c("허리둘레(cm)",    "허리둘레: 차이 (cm)",    "FALSE"),
             c("대사증후군",      "대사증후군: 오즈비 (로그 눈금)", "TRUE"))

# ---- 그림 3. 본 분석 (모형 1·2, 기준 = 3시간 이상) ----
m <- rd("06_main_results.csv")
m[, model_s := ifelse(grepl("^모형1", model), "모형 1", "모형 2")]
mm <- m[level != "3시간 이상(기준)"]
mm[, row := factor(level, lv_int[1:3])]; setorder(mm, outcome, row, model_s); mm[, row := as.character(row)]
open_png("fig3_forest_main.png", 11, 6.4)
par(mfrow = c(2, 2), oma = c(0, 0, 2.4, 0))
for (o in outs) {
  d <- mm[outcome == o[1], .(row, series = model_s, est, lo = ci_low, hi = ci_high)]
  forest_panel(d, title = o[2], xlab = if (as.logical(o[3])) "오즈비 (기준: 3시간 이상)" else "기준(3시간 이상) 대비 차이",
               log = as.logical(o[3]), cols = c(col_ctx, col_main), series_order = c("모형 1", "모형 2"))
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE); plot.new()
legend("top", horiz = TRUE, legend = c("모형 1 (나이·성별·연도)", "모형 2 (+소득·교육·흡연·음주·신체활동·수면시간·에너지)"),
       col = c(col_ctx, col_main), lwd = 2.2, pch = 16, pt.cex = 1.1, bty = "n", cex = 0.85, text.col = col_sub)
dev.off()

# ---- 그림 4. 민감도 분석 (1시간 미만 vs 3시간 이상, 모형 2) ----
s <- rd("07_sensitivity.csv")
s[, key := sub("^(S[1-5]).*", "\\1", analysis)]
lab_s <- c(S1 = "S1 가중치 wt_itvex", S2 = "S2 자정 이후 섭취자 제외", S3 = "S3 주간 외 근무자 제외",
           S4 = "S4 혈압 전환식 적용", S5 = "S5 평일 회상자만")
s[, row := unname(lab_s[key])]
s[outcome == "대사증후군(혈압 전환식)", outcome := "대사증후군"]
main2 <- mm[model_s == "모형 2" & level == "<1시간", .(outcome, row = "본 분석 (모형 2)", est, lo = ci_low, hi = ci_high)]
sd <- rbind(main2, s[, .(outcome, row, est, lo = ci_low, hi = ci_high)])
row_order <- c("본 분석 (모형 2)", unname(lab_s))
open_png("fig4_forest_sensitivity.png", 11, 6.8)
par(mfrow = c(2, 2), oma = c(0, 0, 1.8, 0))
for (o in outs) {
  d <- sd[outcome == o[1]]
  d[, row := factor(row, row_order)]; setorder(d, row); d[, row := as.character(row)]; d[, series := "본 분석"]
  forest_panel(d, title = o[2], xlab = if (as.logical(o[3])) "오즈비 (기준: 3시간 이상)" else "기준(3시간 이상) 대비 차이",
               log = as.logical(o[3]), cols = col_main)
}
mtext("민감도 분석: 마지막 식사–취침 간격 1시간 미만 vs 3시간 이상 (모형 2 보정)", outer = TRUE, cex = 0.9, col = col_sub, line = 0.3)
dev.off()

cat("그림 저장:", fig_dir, "\n"); print(list.files(fig_dir))
