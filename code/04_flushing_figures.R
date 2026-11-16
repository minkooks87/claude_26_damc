# 04_flushing_figures.R
# 목적: 03_flushing_hyperuricemia.R 의 결과(CSV)로 보고서용 그림(PNG)을 만든다.
#       원시자료(data/)는 읽지 않고, output/03_flushing_hyperuricemia/ 의 CSV만 읽는다.
# 실행: Rscript --vanilla code/04_flushing_figures.R   (프로젝트 폴더에서)
# 그림: fig/fig1_participant_flow.png  대상자 선정 흐름
#       fig/fig2_forest_flush_pattern.png  홍조 패턴 vs 비홍조 (음주 수준 보정)
#       fig/fig3_forest_highrisk_by_flush.png  홍조군별 고위험음주 vs 월1회 미만
#       fig/fig4_forest_women_menopause.png  여성: 폐경 × 현재 홍조
#       fig/fig5_prevalence.png  고요산혈증 유병률(가중 %): 성별 × 홍조 × 음주 수준

suppressPackageStartupMessages(library(data.table))

in_dir  <- file.path("output", "03_flushing_hyperuricemia")
fig_dir <- file.path(in_dir, "fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")

# 한글 글꼴: Windows 에서는 맑은 고딕을 등록해서 쓴다
fam <- "sans"
if (.Platform$OS.type == "windows") { windowsFonts(kr = windowsFont("Malgun Gothic")); fam <- "kr" }

# 색: 남자 파랑, 여자 주황 (dataviz 참조 팔레트 슬롯 1·2, 밝은 모드 값). 축·격자는 회색.
col_m <- "#2a78d6"; col_f <- "#eb6834"
col_grid <- "#e1e0d9"; col_axis <- "#c3c2b7"; col_txt <- "#0b0b0b"; col_sub <- "#52514e"; col_mut <- "#898781"

open_png <- function(name, w, h) {
  png(file.path(fig_dir, name), width = w, height = h, units = "in", res = 200, family = fam)
  par(family = fam)
}

# ---- 포레스트 플롯: dat = data.table(row, series, est, lo, hi, n) ----
forest_plot <- function(dat, xlim, ticks, cols, n_lab = "n") {
  rows <- unique(dat$row); nr <- length(rows)
  ser  <- intersect(c("남자", "여자"), unique(dat$series)); ns <- length(ser)
  par(mar = c(3.8, 10.5, 2.4, 13.5), xaxs = "i", yaxs = "i")
  plot(NA, xlim = xlim, ylim = c(0.5, nr + 0.5), log = "x", axes = FALSE, xlab = "", ylab = "")
  abline(v = ticks, col = col_grid, lwd = 1)
  abline(v = 1, col = col_axis, lwd = 1.3)
  axis(1, at = ticks, labels = format(ticks, drop0trailing = TRUE), col = NA, col.ticks = NA,
       col.axis = col_mut, cex.axis = 0.8, line = -0.5)
  mtext("오즈비 (로그 눈금). 세로선 1 = 차이 없음", side = 1, line = 1.9, cex = 0.8, col = col_mut)
  axis(2, at = rev(seq_len(nr)), labels = rows, las = 1, tick = FALSE, col.axis = col_txt, cex.axis = 0.9, line = -0.5)
  mtext(paste0("OR (95% CI), ", n_lab), side = 4, at = nr + 0.5, line = 0.6, las = 1, adj = 0, cex = 0.75, col = col_mut)
  off <- if (ns == 2) c(0.18, -0.18) else 0
  for (i in seq_len(nr)) for (j in seq_len(ns)) {
    r <- dat[row == rows[i] & series == ser[j]]
    if (nrow(r) != 1) next
    y  <- nr - i + 1 + off[j]
    lo <- max(r$lo, xlim[1]); hi <- min(r$hi, xlim[2])
    segments(lo, y, hi, y, col = cols[j], lwd = 2.2, lend = 1)
    if (r$lo < xlim[1]) arrows(lo * 1.3, y, lo, y, length = 0.06, col = cols[j], lwd = 2.2)   # 눈금 밖으로 나가면 화살표
    if (r$hi > xlim[2]) arrows(hi / 1.3, y, hi, y, length = 0.06, col = cols[j], lwd = 2.2)
    points(r$est, y, pch = 21, bg = cols[j], col = "white", lwd = 1.6, cex = 1.5)
    mtext(sprintf("%.2f (%.2f–%.2f), %d", r$est, r$lo, r$hi, r$n), side = 4, at = y, line = 0.6,
          las = 1, adj = 0, cex = 0.78, col = col_sub)
  }
  if (ns == 2) legend("topleft", inset = c(0, -0.16), horiz = TRUE, legend = ser, col = cols, lwd = 2.2,
                      pch = 16, pt.cex = 1.1, bty = "n", cex = 0.85, xpd = NA, text.col = col_sub)
}

hua <- "고요산혈증(OR)"
flush_lv <- c("비홍조", "지속 홍조", "홍조 소실", "신규 홍조")
drink_lv <- c("월1회 미만", "월1회 이상(고위험 아님)", "고위험음주")

# ---- 그림 1. 대상자 선정 흐름 ----
flow <- rd("02_participant_flow.csv")
open_png("fig1_participant_flow.png", 7.5, 8.5)
par(mar = c(0.3, 0.3, 0.3, 0.3))
n <- nrow(flow)
plot(NA, xlim = c(0, 10), ylim = c(0.4, n + 0.6), axes = FALSE, xlab = "", ylab = "")
for (i in seq_len(n)) {
  y <- n - i + 1
  rect(0.4, y - 0.36, 7.0, y + 0.36, col = "#f1efe8", border = col_axis)
  text(3.7, y + 0.10, flow$step[i], cex = 0.8, col = col_txt)
  text(3.7, y - 0.16, sprintf("n = %s", format(flow$n[i], big.mark = ",")), cex = 0.85, col = col_txt)
  if (i > 1) {
    arrows(3.7, y + 1 - 0.36, 3.7, y + 0.36, length = 0.07, col = col_sub)
    text(7.3, y + 0.5, sprintf("제외 %s명", format(flow$excluded[i], big.mark = ",")), cex = 0.78, col = col_sub, adj = 0)
  }
}
dev.off()

# ---- 그림 2. 홍조 패턴 vs 비홍조 ----
b <- rd("06b_flush_pattern_by_sex.csv")[outcome == hua]
datA <- b[, .(row = term, series = sex, est, lo = ci_low, hi = ci_high, n = n_cat)]
datA[, row := factor(row, c("지속 홍조", "홍조 소실", "신규 홍조"))]
setorder(datA, row, series); datA[, row := as.character(row)]
open_png("fig2_forest_flush_pattern.png", 9, 3.3)
forest_plot(datA, xlim = c(0.25, 2), ticks = c(0.25, 0.5, 1, 2), cols = c(col_m, col_f), n_lab = "해당 군 n")
dev.off()

# ---- 그림 3. 홍조군별 고위험음주 vs 월1회 미만 ----
a <- rd("06a_drink_by_sex_flush.csv")[outcome == hua & term == "고위험음주"]
datB <- a[, .(row = flush, series = sex, est, lo = ci_low, hi = ci_high, n = n_cat)]
datB[, row := factor(row, flush_lv)]
setorder(datB, row, series); datB[, row := as.character(row)]
open_png("fig3_forest_highrisk_by_flush.png", 9, 3.8)
forest_plot(datB, xlim = c(0.3, 40), ticks = c(0.5, 1, 2, 4, 8, 16, 32), cols = c(col_m, col_f), n_lab = "고위험음주자 n")
dev.off()

# ---- 그림 4. 여성: 폐경 × 현재 홍조 ----
w <- rd("06d_women_menopause.csv")[outcome == hua & term == "고위험음주"]
datC <- w[, .(row = paste(menopause, flush_now, sep = " · "), series = "여자", est, lo = ci_low, hi = ci_high, n = n_cat)]
open_png("fig4_forest_women_menopause.png", 9, 3.6)
forest_plot(datC, xlim = c(0.5, 40), ticks = c(0.5, 1, 2, 4, 8, 16, 32), cols = col_f, n_lab = "고위험음주자 n")
dev.off()

# ---- 그림 5. 고요산혈증 유병률 (가중 %) ----
pv <- rd("05b_prevalence_by_sex_flush_drink.csv")
open_png("fig5_prevalence.png", 11, 4.8)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.2, 0.8))
for (s in c("남자", "여자")) {
  m  <- matrix(NA_real_, 3, 4, dimnames = list(drink_lv, flush_lv))
  nm <- matrix(NA_integer_, 3, 4, dimnames = list(drink_lv, flush_lv))
  for (i in 1:3) for (j in 1:4) {
    v <- pv[sex == s & drink == drink_lv[i] & flush == flush_lv[j]]
    if (nrow(v)) { m[i, j] <- v$pct; nm[i, j] <- v$n }
  }
  base  <- if (s == "남자") col_m else col_f
  cols3 <- c(adjustcolor(base, alpha.f = 0.4), adjustcolor(base, alpha.f = 0.7), base)   # 음주 수준은 순서형 → 같은 색의 농도
  bp <- barplot(m, beside = TRUE, col = cols3, border = NA, ylim = c(0, 50), axes = FALSE,
                names.arg = flush_lv, cex.names = 0.85, space = c(0.1, 1), main = s, cex.main = 1, font.main = 1)
  abline(h = seq(10, 40, 10), col = col_grid)
  # 격자선 위에 막대를 다시 그린다. 이름표는 다시 그리지 않는다(겹침 방지).
  barplot(m, beside = TRUE, col = cols3, border = NA, add = TRUE, axes = FALSE, axisnames = FALSE, space = c(0.1, 1))
  axis(2, at = seq(0, 40, 10), las = 1, col = NA, col.ticks = NA, col.axis = col_mut, cex.axis = 0.8)
  mtext("고요산혈증 유병률 (가중 %)", side = 2, line = 2.6, cex = 0.8, col = col_mut)
  text(bp, m + 1.3, sprintf("%.1f%s", m, ifelse(nm < 30, "*", "")), cex = 0.62, col = col_sub)
  # 범례는 막대가 낮은 왼쪽 위(비홍조군 위)에 둔다
  if (s == "남자") legend("topleft", legend = drink_lv, fill = cols3, border = NA, bty = "n", cex = 0.75, title = "음주 수준", inset = c(0, 0.01))
  if (s == "여자") legend("topleft", legend = "* 비가중 n < 30", bty = "n", cex = 0.75, text.col = col_sub)
}
dev.off()

cat("그림 저장:", fig_dir, "\n"); print(list.files(fig_dir))
