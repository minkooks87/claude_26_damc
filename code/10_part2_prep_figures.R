# 10_part2_prep_figures.R
# 목적: 연구보고서 V3 2부 발표 준비 문서용 그림(PNG)을 만든다.
#       개념도는 직접 그리고, 수치 그림은 09_part2_descriptives.R 결과(CSV)만 읽는다(원시자료 읽지 않음).
#       노출·홍조별 요산 결과는 쓰지 않는다.
# 실행: Rscript --vanilla code/10_part2_prep_figures.R   (프로젝트 폴더에서)

suppressPackageStartupMessages(library(data.table))

in_dir  <- file.path("output", "09_part2_prep")
fig_dir <- file.path(in_dir, "fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")

fam <- "sans"
if (.Platform$OS.type == "windows") { windowsFonts(kr = windowsFont("Malgun Gothic")); fam <- "kr" }

# 색: 남 파랑·여 주황(04_flushing_figures.R 와 같음), 음주 수준은 연한 → 진한 순서
col_m <- "#2a78d6"; col_f <- "#eb6834"
col_txt <- "#0b0b0b"; col_sub <- "#52514e"; col_mut <- "#898781"; col_grid <- "#e1e0d9"; col_line <- "#c3c2b7"
col_low <- "#dbe8f7"; col_mid <- "#8fb6e6"; col_high <- "#2f5f9e"
col_flush <- "#d9534f"; col_noflush <- "#5b6770"
col_box <- "#f5f4ef"; col_key <- "#fff4d6"; col_warn <- "#b3261e"

open_png <- function(name, w, h) {
  png(file.path(fig_dir, name), width = w, height = h, units = "in", res = 200, family = fam)
  par(family = fam)
}
canvas <- function(xmax = 100, ymax = 100) {
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, xmax), c(0, ymax), xaxs = "i", yaxs = "i")
}
box <- function(x, y, w, h, lab, fill = col_box, border = col_line, cex = 0.9, font = 1, col = col_txt, lwd = 1.2, lty = 1) {
  rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2, col = fill, border = border, lwd = lwd, lty = lty)
  text(x, y, lab, cex = cex, font = font, col = col)
}
arr <- function(x0, y0, x1, y1, col = col_sub, lwd = 1.6, lty = 1, len = 0.09) arrows(x0, y0, x1, y1, length = len, col = col, lwd = lwd, lty = lty)

# ---- 그림 1. 연구 구조: 노출 → 결과, 홍조·성별은 효과수정 ----------------------
open_png("f01_study_structure.png", 9, 4.6); canvas()
box(17, 62, 26, 20, "노출\n음주량 (주당 잔수)\n저 <3.5 · 중 3.5–14 · 고 ≥14", fill = col_low, cex = 0.85)
box(83, 62, 26, 20, "결과\n고요산혈증\n남 ≥7.0 · 여 ≥6.0 mg/dL", fill = "#fbe3d9", cex = 0.85)
arr(30.5, 62, 69.5, 62, col = col_txt, lwd = 2.6)
text(50, 57.5, "연관성 (교차비, OR)", cex = 0.8, col = col_sub)
text(50, 52.5, "핵심 질문: 이 화살표의 기울기가 집단마다 다른가?", cex = 0.76, col = "#8a6200", font = 3)
box(33, 90, 34, 13, "효과수정 ① 안면홍조\n현재 2군 (주분석) · 이력 3군 (부질문 1)", fill = col_key, border = "#d9a400", cex = 0.78)
box(69, 90, 30, 13, "효과수정 ② 성별\n남·여 따로 + 3원 상호작용 (부질문 2)", fill = col_key, border = "#d9a400", cex = 0.74)
arr(38, 83.3, 47, 63.5, col = "#b07d00", lwd = 2); arr(64, 83.3, 54, 63.5, col = "#b07d00", lwd = 2)
box(50, 22, 70, 18, "보정변수 (공변량)\n기본: 연령·교육·소득·조사연도·흡연·유산소 신체활동\n확장: + BMI · 고혈압 · 당뇨병 · eGFR (CKD-EPI 2009)", cex = 0.8)
arr(30, 31, 20, 51.5, lty = 2, col = col_mut); arr(70, 31, 80, 51.5, lty = 2, col = col_mut)
text(50, 5, "대상: 국민건강영양조사 2019–2021 만 19세 이상 현재 음주자 (통풍 의사진단자 제외) · 단면연구 · 복합표본 로지스틱 회귀", cex = 0.72, col = col_sub)
dev.off()

# ---- 그림 2. 보고서 구성과 2부 범위 -----------------------------------------
open_png("f02_report_parts.png", 9, 2.6); canvas()
parts <- list(
  list("1부", "제목 · 서론\n이론적 배경\n선행연구 1–3", col_box),
  list("2부 (담당)", "선행연구 4 (Jee 2019) · 종합\n연구 질문·가설\n연구 대상 · 설계\n공변량 · 음주량 정의", col_key),
  list("3부", "홍조·고요산혈증 정의\n데이터 분석 방법\n(복합표본·상호작용·민감도)", col_box),
  list("4부", "예상 결과 · 기대효과\n한계 · 일정\n참고문헌", col_box))
for (i in seq_along(parts)) {
  x <- 12.5 + (i - 1) * 25
  box(x, 50, 22, 62, parts[[i]][[2]], fill = parts[[i]][[3]], border = if (i == 2) "#d9a400" else col_line, cex = 0.72, lwd = if (i == 2) 2.4 else 1.2)
  text(x, 90, parts[[i]][[1]], cex = 0.9, font = 2, col = if (i == 2) "#8a6200" else col_sub)
}
text(50, 7, "2부 범위는 2부 문서 첫 줄 기준 · 1·3·4부의 경계는 V3 목차 순서로 추정", cex = 0.68, col = col_mut)
dev.off()

# ---- 그림 3. 알코올 대사, 홍조, 요산: 두 방향의 가설 --------------------------
open_png("f03_alcohol_urate_pathway.png", 9.5, 5.6); canvas()
box(9, 78, 14, 11, "에탄올", cex = 0.9, font = 2)
box(37, 78, 18, 11, "아세트알데히드\n(독성 중간산물)", fill = "#fde2e1", cex = 0.82)
box(64, 78, 14, 11, "아세트산", cex = 0.9)
box(89, 78, 17, 11, "아세틸-CoA\n(에너지·지방)", cex = 0.8)
arr(16, 78, 27.8, 78, lwd = 2); text(22, 83, "ADH (정상)", cex = 0.78, font = 2)
arr(46.2, 78, 56.8, 78, lwd = 2); text(51.5, 83, "ALDH2", cex = 0.78, font = 2)
text(51.5, 72.5, "홍조인 사람은\n여기가 느림", cex = 0.68, col = col_flush, font = 2)
arr(71.2, 78, 80.3, 78, lwd = 2); text(75.8, 83, "ATP 사용", cex = 0.7, font = 2, col = col_high)
text(37, 92, "얼굴 붉어짐 = 아세트알데히드가 쌓인다는 신호", cex = 0.75, col = col_flush)
# 요산 생성 경로
box(89, 50, 17, 12, "ATP → AMP\n→ 퓨린 분해", fill = col_low, cex = 0.78)
arr(89, 72.4, 89, 56.2, col = col_high)
box(89, 22, 17, 12, "요산 ↑\n(생성 증가)", fill = "#fbe3d9", cex = 0.85, font = 2)
arr(89, 43.8, 89, 28.2, col = col_high)
# NADH → 젖산 경로
box(37, 50, 22, 12, "NADH ↑ → 젖산 ↑\n(①·② 단계 모두)", fill = col_box, cex = 0.78)
arr(22, 72.4, 32, 56.2, col = col_mut, lty = 2); arr(42, 72.4, 40, 56.2, col = col_mut, lty = 2)
box(37, 22, 22, 12, "신장 요산 배설 ↓\n(재흡수 경쟁)", fill = "#fbe3d9", cex = 0.8)
arr(37, 43.8, 37, 28.2, col = col_sub)
# 가설 상자
box(63, 41, 26, 25, "가설 A (V3)\n아세트알데히드 축적 → 대사 부담 ↑\n→ 같은 양에서 홍조군 요산 ↑\n\n가설 B (반대 방향)\n아세트산이 늦게·적게 생성\n→ ATP 소모 ↓ → 요산 생성 ↓", fill = col_key, border = "#d9a400", cex = 0.66)
text(50, 5, "맥주의 퓨린(구아노신)과 탈수는 홍조와 관계없이 작용 · 근거: Faller & Fox 1982 (ATP 분해 → 요산 생성), 홍조 기전은 추정 수준", cex = 0.66, col = col_sub)
dev.off()

# ---- 그림 4. 멘델 무작위 (Jee 2019) 논리와 결과 ------------------------------
open_png("f04_jee2019_mr.png", 9, 3.8); canvas()
box(14, 70, 24, 18, "유전 변이\nALDH2 rs671 + 10개 SNP 점수\n(태어날 때 무작위로 정해짐)", fill = col_low, cex = 0.72)
box(50, 70, 18, 18, "음주량", cex = 0.95, font = 2)
box(86, 70, 22, 18, "고요산혈증", fill = "#fbe3d9", cex = 0.95, font = 2)
arr(26.2, 70, 40.8, 70, lwd = 2.2); arr(59.2, 70, 74.8, 70, lwd = 2.2)
text(33.5, 89, "① 강하게 관련 ○", cex = 0.78, col = "#1e7d32", font = 2)
text(67, 91, "② 관찰 분석: 양의 연관 ○\n남 P<0.001 · 여 P=0.014", cex = 0.72, col = "#1e7d32", font = 2)
text(50, 51, "③ 유전 변이 → 고요산혈증 직접 연관: 없음 ✗", cex = 0.8, col = col_warn, font = 2)
box(50, 25, 86, 24, "④ MR 인과 추정: 남성에서만 시사 → 다중검정 보정 후 유의하지 않음\n저자 결론: \"한국 남성에서 인과적 근거\" (결과보다 강한 표현)\n해석: 홍조 체질은 '덜 마시게 하는 효과'와 '같은 양에 더 취약할 가능성'이 상쇄될 수 있음 → 음주량을 고정한 비교가 필요", fill = col_key, border = "#d9a400", cex = 0.72)
text(50, 4, "KCPS-II 바이오뱅크 156,701명 · 2017년 음주 GWAS(11,678명)의 27개 SNP · 초록 기준(전문은 구독 필요)", cex = 0.66, col = col_mut)
dev.off()

# ---- 그림 5. 선행연구가 다룬 것과 빈 곳 --------------------------------------
open_png("f05_literature_gap.png", 9.5, 5.2)
par(mar = c(1, 11.5, 6.5, 1))
studies <- c("김어진 2015", "김어진 2016 (학위)", "Kang & Oh 2023", "Jeon 2022", "Jee 2019", "Yokoyama 2016", "Zhang 2018", "본 연구")
cols <- c("전국 대표\n표본", "여성\n포함", "요산이\n결과", "음주'량'\n사용", "과거·현재\n홍조 구분", "홍조×음주\n상호작용", "혈액\n지표")
m <- rbind(
  c("✗", "✗", "○", "○", "?", "✗", "○"),
  c("✗", "✗", "○", "○", "?", "△", "○"),
  c("○", "✗", "✗", "✗", "✗", "○", "○"),
  c("○", "○", "✗", "○", "○", "△", "✗"),
  c("△", "○", "○", "○", "✗", "✗", "○"),
  c("✗", "✗", "○", "?", "✗", "✗", "○"),
  c("✗", "○", "○", "○", "✗", "✗", "○"),
  c("○", "○", "○", "○", "○", "○", "○"))
plot(NA, xlim = c(0.5, ncol(m) + 0.5), ylim = c(0.5, nrow(m) + 0.5), axes = FALSE, xlab = "", ylab = "")
fillc <- c("○" = "#d7ecd9", "△" = "#fff1c7", "✗" = "#f6dcdc", "?" = "#ececec")
for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m))) {
  y <- nrow(m) - i + 1
  rect(j - 0.47, y - 0.42, j + 0.47, y + 0.42, col = fillc[m[i, j]], border = if (i == nrow(m)) "#d9a400" else "white", lwd = if (i == nrow(m)) 2 else 1)
  text(j, y, m[i, j], cex = 1.1, font = 2, col = col_txt)
}
axis(2, at = rev(seq_len(nrow(m))), labels = studies, las = 1, tick = FALSE, cex.axis = 0.85, font = 1)
text(seq_len(ncol(m)), nrow(m) + 1.15, cols, xpd = NA, cex = 0.75, col = col_sub)
mtext("○ 다룸   △ 부분적   ✗ 다루지 않음   ? 원문 확인 필요 (발표자료·계획서·초록 기준)", side = 3, line = 4.8, cex = 0.72, col = col_mut, adj = 0)
dev.off()

# ---- 그림 6. 결과 시나리오 (개념도, 실제 결과 아님) ---------------------------
open_png("f06_scenarios.png", 9.5, 3.3)
par(mfrow = c(1, 3), mar = c(3.2, 3.2, 3.2, 1), mgp = c(1.6, 0.4, 0))
x <- 1:3
scen <- list(
  list("A  홍조군에서 더 가파름\n(V3 가설 · 김어진 2016 방향)", c(1, 1.3, 1.6), c(1, 1.8, 2.9)),
  list("B  비홍조군에서 더 가파름\n(Yokoyama 2016 방향)", c(1, 1.8, 2.9), c(1, 1.3, 1.6)),
  list("C  차이 없음\n(효과수정 없음)", c(1, 1.5, 2.1), c(1, 1.5, 2.1)))
for (s in scen) {
  plot(x, s[[2]], type = "n", ylim = c(0.8, 3.1), xlim = c(0.8, 3.2), axes = FALSE, xlab = "음주 수준", ylab = "고요산혈증 OR")
  abline(h = 1, col = col_line)
  lines(x, s[[2]], col = col_noflush, lwd = 2.6, type = "b", pch = 16)
  lines(x + if (identical(s[[2]], s[[3]])) 0.04 else 0, s[[3]], col = col_flush, lwd = 2.6, type = "b", pch = 17, lty = if (identical(s[[2]], s[[3]])) 2 else 1)
  axis(1, at = x, labels = c("저", "중", "고"), tick = FALSE, cex.axis = 0.9)
  title(s[[1]], cex.main = 0.85, font.main = 1, line = 0.6)
  graphics::box(bty = "l", col = col_line)
}
legend("topleft", c("비홍조", "홍조"), col = c(col_noflush, col_flush), lwd = 2.6, pch = c(16, 17), bty = "n", cex = 0.85)
dev.off()

# ---- 그림 7. 홍조 이력 2×2 (예비 집계 인원) -----------------------------------
pat <- rd("04_flush_pattern_n.csv")
g <- function(p, s) format(pat[홍조패턴 == p][[s]], big.mark = ",")
open_png("f07_flush_pattern_2x2.png", 8, 4.6); canvas()
text(58, 94, "현재 홍조 (BD9_2)", cex = 0.95, font = 2)
text(40, 85, "없음 (아니오)", cex = 0.85, col = col_sub); text(76, 85, "있음 (가끔·자주·늘)", cex = 0.85, col = col_sub)
text(8, 62, "과거 홍조\n(BD9_1)", cex = 0.9, font = 2)
text(18, 62 + 15, "없음", cex = 0.85, col = col_sub); text(18, 62 - 19, "있음", cex = 0.85, col = col_sub)
box(40, 66, 34, 30, paste0("비홍조\n남 ", g("비홍조", "남"), " · 여 ", g("비홍조", "여"), "\n부질문 1의 기준군"), fill = "#e9edf0", cex = 0.85)
box(76, 66, 34, 30, paste0("신규 홍조\n남 ", g("신규 홍조", "남"), " · 여 ", g("신규 홍조", "여"), "\n이력 분석에서 제외 (회상 오류 가능)"), fill = "#ececec", border = col_mut, lty = 2, cex = 0.8)
box(40, 32, 34, 30, paste0("홍조 소실\n남 ", g("홍조 소실", "남"), " · 여 ", g("홍조 소실", "여"), "\n부질문 1의 핵심 비교 대상"), fill = col_key, border = "#d9a400", cex = 0.85, lwd = 2)
box(76, 32, 34, 30, paste0("지속 홍조\n남 ", g("지속 홍조", "남"), " · 여 ", g("지속 홍조", "여")), fill = "#fde2e1", cex = 0.85)
rect(22.5, 15.5, 57.5, 82, border = col_noflush, lwd = 2.2, lty = 3); rect(58.5, 15.5, 93.5, 82, border = col_flush, lwd = 2.2, lty = 3)
text(40, 10, "주분석 '현재 비홍조' = 비홍조 + 홍조 소실", cex = 0.75, col = col_noflush)
text(76, 10, "주분석 '현재 홍조' = 지속 + 신규", cex = 0.75, col = col_flush)
text(50, 3, "인원: 최종 분석 대상 10,779명 기준 비가중 n (발표 v5 예비 집계와 같음)", cex = 0.68, col = col_mut)
dev.off()

# ---- 그림 8. 대상자 선정 흐름 ------------------------------------------------
fl <- rd("01_flow_v3.csv")
nn <- function(k) format(fl$n[k], big.mark = ",")
open_png("f08_participant_flow.png", 8.5, 6.6); canvas()
ys <- c(92, 75, 58, 41, 24, 7)
labs <- c(paste0("2019–2021 기본DB 전체\nn = ", nn(1)),
          paste0("만 19세 이상\nn = ", nn(2)),
          paste0("최근 1년 음주자 (BD1_11 = 2–6)\nn = ", nn(3)),
          paste0("음주량·과거/현재 홍조·요산 확보\nn = ", nn(4)),
          paste0("통풍 의사진단 없음\nn = ", nn(5)),
          paste0("최종 분석 대상 (보정변수 모두 있음)\nn = ", nn(7), "  (남 ", format(fl$n_men[7], big.mark = ","), " · 여 ", format(fl$n_women[7], big.mark = ","), ")"))
for (k in seq_along(ys)) box(32, ys[k], 46, 11, labs[k], fill = if (k == 6) col_key else col_box, border = if (k == 6) "#d9a400" else col_line, cex = 0.8, lwd = if (k == 6) 2 else 1.2)
for (k in 1:5) arr(32, ys[k] - 5.5, 32, ys[k + 1] + 5.5)
ex <- c(paste0("만 19세 미만 ", format(fl$excluded[2], big.mark = ",")),
        paste0("최근 1년 비음주 ", format(fl$excluded[3], big.mark = ",")),
        paste0("결측 ", fl$excluded[4], "\n(1회량·홍조·요산; 요산 <1.0 표기 4명 포함)"),
        "통풍 의사진단 235\n'모름'·무응답 550",
        paste0("보정변수 결측 ", fl$excluded[7], "\n(주로 당뇨병 유병 변수의 공복 조건)"))
for (k in 1:5) {
  ym <- (ys[k] + ys[k + 1]) / 2
  segments(32, ym, 60, ym, col = col_mut, lty = 2)
  box(78, ym, 34, 9, ex[k], fill = "white", border = col_mut, cex = 0.7, col = col_sub)
}
dev.off()

# ---- 그림 9. 음주량 환산 격자 (빈도 × 1회량 → 주당 잔수와 음주 수준) ------------
grid <- rd("05_grid_freq_amount.csv")
fq <- c(0.5 * 12 / 52, 12 / 52, 3 * 12 / 52, 2.5, 5.5)
am <- c(1.5, 3.5, 5.5, 8, 10)
fq_lab <- c("월 1회 미만\n(월 0.5회)", "월 1회 정도\n(월 1회)", "월 2–4회\n(월 3회)", "주 2–3회\n(주 2.5회)", "주 4회 이상\n(주 5.5회)")
am_lab <- c("1–2잔\n(1.5)", "3–4잔\n(3.5)", "5–6잔\n(5.5)", "7–9잔\n(8)", "10잔 이상\n(기입값)")
open_png("f09_conversion_grid.png", 9.5, 6)
par(mar = c(4.5, 8.5, 4.2, 1))
plot(NA, xlim = c(0.5, 5.5), ylim = c(0.5, 5.5), axes = FALSE, xlab = "", ylab = "")
for (i in 1:5) for (j in 1:5) {
  v <- fq[i] * am[j]
  lev <- if (v < 3.5) "저" else if (v < 14) "중" else "고"
  fc <- c("저" = col_low, "중" = col_mid, "고" = col_high)[lev]
  y <- 6 - i
  rect(j - 0.48, y - 0.46, j + 0.48, y + 0.46, col = fc, border = "white", lwd = 2)
  nm <- grid[sex == 1 & bd1_11 == i + 1 & bd2_1 == j, n]; nf <- grid[sex == 2 & bd1_11 == i + 1 & bd2_1 == j, n]
  nm <- if (length(nm)) nm else 0; nf <- if (length(nf)) nf else 0
  vt <- if (j == 5) sprintf("≥ %.1f잔", v) else sprintf("%.2f잔", v)
  tc <- if (lev == "고") "white" else col_txt
  text(j, y + 0.15, vt, cex = 0.86, font = 2, col = tc)
  text(j, y - 0.2, sprintf("남 %s · 여 %s", format(nm, big.mark = ","), format(nf, big.mark = ",")), cex = 0.62, col = tc)
}
axis(2, at = 5:1, labels = fq_lab, las = 1, tick = FALSE, cex.axis = 0.72, line = -0.6)
axis(3, at = 1:5, labels = am_lab, tick = FALSE, cex.axis = 0.72, line = -0.4, padj = 0.3)
mtext("1회 음주량 (BD2_1 · 대표값)", side = 3, line = 2.6, cex = 0.8, font = 2)
mtext("음주 빈도\n(BD1_11 · 대표값)", side = 2, line = 6.2, cex = 0.8, font = 2)
legend("bottom", inset = c(0, -0.13), horiz = TRUE, xpd = NA, bty = "n", cex = 0.78,
       legend = c("저음주 <3.5잔/주", "중음주 3.5–14잔", "고음주 ≥14잔"), fill = c(col_low, col_mid, col_high), border = NA)
dev.off()

# ---- 그림 10. 기준값 비교 (주당 알코올 g) -------------------------------------
open_png("f10_cutoff_comparison.png", 9.5, 5.6)
par(mar = c(3.6, 17, 2.4, 1.5))
items <- list(
  list("V3 저/중 경계: 3.5잔", 24.5, "V3"), list("V3 중/고 경계: 14잔", 98, "V3"),
  list("국건영 고위험음주 최소량 (여 5잔×주2회)", 70, "KN"), list("국건영 고위험음주 최소량 (남 7잔×주2회)", 98, "KN"),
  list("한국 홍조 지침: 홍조 여성 ≤2잔", 28, "KR"), list("한국 홍조 지침: 홍조 남성·비홍조 여성 ≤4잔", 56, "KR"),
  list("한국 홍조 지침: 비홍조 남성 ≤8잔", 112, "KR"),
  list("미국 NIAAA: 여성 ≤7잔", 98, "US"), list("미국 NIAAA: 남성 ≤14잔", 196, "US"),
  list("예비: 여성 홍조 평균 3.0잔", 21, "OB"), list("예비: 여성 비홍조 평균 4.8잔", 33.6, "OB"),
  list("예비: 남성 홍조 평균 8.5잔", 59.5, "OB"), list("예비: 남성 비홍조 평균 15.5잔", 108.5, "OB"))
cc <- c(V3 = "#8a6200", KN = col_high, KR = col_flush, US = col_noflush, OB = col_m)
ny <- length(items)
plot(NA, xlim = c(0, 210), ylim = c(0.5, ny + 0.5), axes = FALSE, xlab = "", ylab = "")
rect(0, 0.5, 24.5, ny + 0.5, col = "#eef4fb", border = NA); rect(24.5, 0.5, 98, ny + 0.5, col = "#dbe8f7", border = NA); rect(98, 0.5, 210, ny + 0.5, col = "#c8daf2", border = NA)
text(c(12, 61, 154), ny + 0.75, c("V3 저음주", "V3 중음주", "V3 고음주"), cex = 0.72, col = col_sub, xpd = NA)
abline(v = seq(0, 200, 25), col = "white", lwd = 1)
for (k in seq_len(ny)) {
  y <- ny - k + 1; it <- items[[k]]
  segments(0, y, it[[2]], y, col = adjustcolor(cc[it[[3]]], 0.35), lwd = 2)
  points(it[[2]], y, pch = 21, bg = cc[it[[3]]], col = "white", cex = 1.5, lwd = 1.3)
  text(it[[2]] + 3, y, sprintf("%.0f g", it[[2]]), adj = 0, cex = 0.7, col = col_sub)
}
axis(2, at = ny:1, labels = sapply(items, `[[`, 1), las = 1, tick = FALSE, cex.axis = 0.7)
axis(1, at = seq(0, 200, 25), tick = FALSE, cex.axis = 0.72, col.axis = col_mut, line = -0.6)
mtext("주당 순수 알코올 (g) · 국건영 1잔 ≈ 7 g (소주 1병 = 7잔, 약 17% 가정) · 지침·NIAAA 1잔 = 14 g", side = 1, line = 2, cex = 0.72, col = col_mut)
dev.off()

# ---- 그림 11. 칸별 인원 (요산 결과 없음) --------------------------------------
cn <- rd("02_cells_current_flush.csv"); cp <- rd("03_cells_flush_pattern.csv")
open_png("f11_cell_sizes.png", 9.5, 5.4)
layout(matrix(1:2, 1), widths = c(1, 1.6))
draw_cells <- function(tab, grp, title) {
  tab <- copy(tab)
  lab <- paste0(tab$성별, "·", tab[[grp]])
  mat <- t(as.matrix(tab[, .(저음주, 중음주, 고음주)]))
  par(mar = c(6.5, 3.6, 3, 0.5), mgp = c(2, 0.5, 0))
  bp <- barplot(mat, beside = TRUE, col = c(col_low, col_mid, col_high), border = NA, names.arg = rep("", ncol(mat)),
                ylim = c(0, max(mat) * 1.22), las = 1, cex.axis = 0.7, ylab = "비가중 n", cex.lab = 0.8)
  text(colMeans(bp), -max(mat) * 0.04, lab, srt = 40, adj = 1, xpd = NA, cex = 0.72)
  for (k in seq_along(mat)) text(bp[k], mat[k] + max(mat) * 0.02, mat[k], cex = 0.6, srt = 90, adj = 0,
                                 col = if (mat[k] < 30) col_warn else if (mat[k] < 100) "#a35b00" else col_sub, font = if (mat[k] < 100) 2 else 1)
  title(title, cex.main = 0.85, font.main = 2, line = 1)
}
draw_cells(cn, "현재홍조", "주분석: 현재 홍조 2군")
draw_cells(cp, "홍조패턴", "부질문 1: 홍조 이력 (신규 홍조는 분석 제외)")
legend("topright", c("저음주", "중음주", "고음주", "n<100 주황 · n<30 빨강"), fill = c(col_low, col_mid, col_high, NA), border = NA, bty = "n", cex = 0.72)
dev.off()

# ---- 그림 12. 음주량 분포 (가중 %, 노출만) -------------------------------------
dist <- rd("07_drinks_distribution.csv")
lv <- c("[0,1)", "[1,2)", "[2,3.5)", "[3.5,7)", "[7,10.5)", "[10.5,14)", "[14,21)", "[21,28)", "[28,42)", "[42,Inf)")
blab <- c("<1", "1–2", "2–3.5", "3.5–7", "7–10.5", "10.5–14", "14–21", "21–28", "28–42", "42+")
open_png("f12_drinks_distribution.png", 9.5, 4.4)
par(mfrow = c(1, 2), mar = c(4.2, 3.6, 2.6, 0.5), mgp = c(2, 0.5, 0))
for (sx in 1:2) {
  sub <- dist[sex == sx]
  mat <- rbind(sub[flush_now == "현재 비홍조"][match(lv, dbin), pct], sub[flush_now == "현재 홍조"][match(lv, dbin), pct])
  mat[is.na(mat)] <- 0
  bp <- barplot(mat, beside = TRUE, col = c(col_noflush, col_flush), border = NA, ylim = c(0, max(mat) * 1.2), las = 1,
                cex.axis = 0.7, ylab = "가중 %", cex.lab = 0.8, names.arg = rep("", length(lv)))
  text(colMeans(bp), -max(mat) * 0.04, blab, srt = 40, adj = 1, xpd = NA, cex = 0.66)
  abline(v = c(mean(bp[, 3:4]), mean(bp[, 6:7])), lty = 2, col = col_mut)
  title(c("남성", "여성")[sx], cex.main = 0.9, line = 0.8)
  mtext("주당 잔수 (점선 = 3.5잔·14잔 경계)", side = 1, line = 3, cex = 0.66, col = col_mut)
}
legend("topright", c("현재 비홍조", "현재 홍조"), fill = c(col_noflush, col_flush), border = NA, bty = "n", cex = 0.75)
dev.off()

# ---- 그림 13. 보정변수: 교란변수와 매개 가능 변수 ------------------------------
open_png("f13_confounder_mediator.png", 9, 4.6); canvas()
box(14, 50, 20, 14, "음주량", fill = col_low, cex = 0.95, font = 2)
box(86, 50, 20, 14, "고요산혈증", fill = "#fbe3d9", cex = 0.95, font = 2)
arr(24.2, 50, 75.8, 50, lwd = 2.6, col = col_txt)
box(50, 86, 52, 13, "교란변수 (기본 모형)\n연령 · 교육 · 소득 · 조사연도 · 흡연 · 신체활동", cex = 0.78)
arr(34, 79.5, 18, 57.2, col = col_mut); arr(66, 79.5, 82, 57.2, col = col_mut)
box(50, 17, 56, 16, "매개 가능 변수 (확장 모형에서 추가)\nBMI · 고혈압 · 당뇨병 · eGFR\n음주 → 이 변수들 → 요산 경로가 있을 수 있음", fill = col_key, border = "#d9a400", cex = 0.76)
arr(20, 42.8, 30, 25.2, col = "#b07d00", lty = 2); arr(70, 25.2, 80, 42.8, col = "#b07d00", lty = 2)
text(50, 56, "보고하는 것: 두 모형의 OR을 나란히 → 추정치가 얼마나 바뀌는지", cex = 0.74, col = col_sub)
dev.off()

cat("그림 저장:", paste(list.files(fig_dir), collapse = ", "), "\n")
