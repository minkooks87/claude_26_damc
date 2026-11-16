# 12_v3_summary_figures.R
# 목적: 연구보고서 V3 정리본에 쓸 추가 개념도(PNG)를 만든다. 수치는 쓰지 않는다.
#       (나머지 그림은 10_part2_prep_figures.R 의 output/09_part2_prep/fig/ 를 함께 쓴다)
# 실행: Rscript --vanilla code/12_v3_summary_figures.R   (프로젝트 폴더에서)

fig_dir <- file.path("output", "10_v3_summary", "fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

fam <- "sans"
if (.Platform$OS.type == "windows") { windowsFonts(kr = windowsFont("Malgun Gothic")); fam <- "kr" }

# 색은 10_part2_prep_figures.R 와 같게 맞춘다
col_txt <- "#0b0b0b"; col_sub <- "#52514e"; col_mut <- "#898781"; col_line <- "#c3c2b7"
col_low <- "#dbe8f7"; col_box <- "#f5f4ef"; col_key <- "#fff4d6"; col_out <- "#fbe3d9"; col_gold <- "#d9a400"

open_png <- function(name, w, h) {
  png(file.path(fig_dir, name), width = w, height = h, units = "in", res = 200, family = fam)
  par(family = fam)
}
canvas <- function() { par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, 100), c(0, 100), xaxs = "i", yaxs = "i") }
box <- function(x, y, w, h, lab, fill = col_box, border = col_line, cex = 0.8, font = 1, col = col_txt, lwd = 1.2, lty = 1) {
  rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2, col = fill, border = border, lwd = lwd, lty = lty)
  text(x, y, lab, cex = cex, font = font, col = col)
}
arr <- function(x0, y0, x1, y1, col = col_sub, lwd = 1.6, lty = 1) arrows(x0, y0, x1, y1, length = 0.08, col = col, lwd = lwd, lty = lty)

# ---- 그림 A. 연구 질문 지도 -------------------------------------------------
open_png("fA_question_map.png", 9.5, 5.4); canvas()
box(50, 90, 70, 12, "연구 목적: 한국 성인 현재 음주자에서 음주량–고요산혈증 연관성이\n과거·현재 음주 관련 안면홍조 이력에 따라, 그리고 성별에 따라 달라지는가",
    fill = col_low, cex = 0.82, font = 2)
qs <- list(
  list(17, "주질문 (주분석)", "현재 홍조 여부\n현재 홍조(가끔·자주·늘) vs 아니오\n\n남녀 각각에서\n음주 수준 × 현재 홍조\n\n가설: 홍조군에서\n연관성이 더 강함", col_key, col_gold),
  list(50, "부질문 1 (부가 분석)", "홍조 이력 3군\n비홍조 · 홍조 소실 · 지속 홍조\n\n중심: 소실 vs 비홍조\n탐색: 소실 vs 지속\n\n가설: 소실군이 비홍조군보다\n연관성이 더 강함", col_box, col_line),
  list(83, "부질문 2 (부가 분석)", "성별 차이\n음주 수준 × 현재 홍조 × 성별\n\n공통 결과: 고요산혈증\n여성: 요산 수치(탐색)\n\n가설: 성별에 따라\n다른 양상", col_box, col_line))
for (q in qs) {
  text(q[[1]], 74, q[[2]], cex = 0.85, font = 2, col = "#8a6200")
  box(q[[1]], 42, 30, 56, q[[3]], fill = q[[4]], border = q[[5]], cex = 0.74, lwd = if (q[[5]] == col_gold) 2 else 1.2)
  arr(q[[1]] * 0.4 + 30, 83.6, q[[1]], 77)
}
text(50, 6, "노출: 음주 수준(저 <3.5 · 중 3.5–14 · 고 ≥14잔/주, 남녀 공통) · 결과: 고요산혈증(남 ≥7.0, 여 ≥6.0 mg/dL)", cex = 0.72, col = col_sub)
dev.off()

# ---- 그림 B. 데이터 분석 흐름 ----------------------------------------------
open_png("fB_analysis_flow.png", 7.6, 8.4); canvas()
steps <- list(
  list("1) 복합표본설계 지정", "wt_itvex × 1/3 (3개년 통합) · 층 kstrata · 집락 psu · R survey 패키지\n설계 지정 후 연구 대상·성별 하위집단을 subset으로 지정", col_low),
  list("2) 대상자 특성 비교", "성별 × 현재 홍조, 홍조 이력 3군별 · 범주형: 비가중 n + 가중 % (Rao–Scott 카이제곱)\n연속형: 가중 평균(SE) (복합표본 선형회귀 Wald 검정)", col_box),
  list("3) 주분석: 현재 홍조의 효과수정", "남녀 각각 복합표본 로지스틱 회귀 · 저음주 대비 중·고음주 보정 OR(95% CI)\n기본 모형 → 확장 모형(+BMI·고혈압·당뇨병·eGFR) · 음주 수준 × 현재 홍조 2개 항 공동 Wald 검정", col_key),
  list("4) 부질문 1: 홍조 이력", "비홍조군 기준 · 음주 수준 × 이력 3군 상호작용 공동 검정\n소실 vs 비홍조 우선 비교 → 소실 vs 지속 탐색 · 칸별 n·사례 수 먼저 확인", col_box),
  list("5) 부질문 2: 성별", "전체 대상 모형: 음주 × 현재 홍조 × 성별 3원 상호작용(모든 주효과·2원 상호작용 포함)\n여성: 혈청 요산 연속형 복합표본 선형회귀(β, 95% CI)", col_box),
  list("6) 결측·민감도 분석", "완전사례 분석 · 결측 규모와 제외자 특성 비교(선택편향 검토)\n개방형 범주 대표값 변경 · 통풍 진단자·통풍 문항 무응답자 포함 분석", col_box),
  list("7) 판단과 제시", "양측검정, 유의수준 0.05 · p값 + OR/β + 95% CI\n비가중 n과 가중 추정치 함께 · 탐색적 분석은 다중비교를 고려해 해석", col_box))
ys <- seq(92, 8, length.out = length(steps))
for (k in seq_along(steps)) {
  box(14, ys[k], 26, 11, gsub(": ", ":\n", steps[[k]][[1]]), fill = steps[[k]][[3]], border = if (k == 3) col_gold else col_line, cex = 0.8, font = 2, lwd = if (k == 3) 2 else 1.2)
  box(63.5, ys[k], 71, 11, gsub(" · ", "\n", steps[[k]][[2]]), fill = "white", border = col_line, cex = 0.6, col = col_txt)
  if (k < length(steps)) arr(14, ys[k] - 5.5, 14, ys[k + 1] + 5.5)
}
dev.off()

# ---- 그림 C. 연구 일정 ------------------------------------------------------
open_png("fC_timeline.png", 9.5, 3.8); canvas()
wk <- list(
  list("0주차", "10/28–11/1", "계획서 확정\n발표 준비"),
  list("1주차", "11/2–11/8", "11/2 계획 발표\n피드백 반영\n선행연구 재검색\n김어진 2015 원문 확인"),
  list("2주차", "11/9–11/15", "분석 검증\n국민건강통계 대조\n변수 정의 점검\n2회 실행 재현성"),
  list("3주차", "11/16–11/22", "결과 확정·시각화\n특성표·연관성표\n성별 비교표\n흐름도·OR 그림"),
  list("4주차", "11/23–11/29", "결과 해석\n보고서 초안\n(서론·방법·\n결과·고찰)"),
  list("5주차", "11/30–12/6", "보고서 완성\n수치·참고문헌 점검\n최종 발표 준비"))
segments(4, 70, 96, 70, col = col_line, lwd = 3)
for (k in seq_along(wk)) {
  x <- 8.5 + (k - 1) * 16.6
  points(x, 70, pch = 21, bg = if (k == 2) col_gold else "#2f5f9e", col = "white", cex = 2.6, lwd = 1.5)
  text(x, 86, wk[[k]][[1]], cex = 0.9, font = 2)
  text(x, 79, wk[[k]][[2]], cex = 0.72, col = col_sub)
  box(x, 36, 15.4, 44, wk[[k]][[3]], fill = if (k == 2) col_key else col_box, border = if (k == 2) col_gold else col_line, cex = 0.68)
}
text(50, 5, "공식 모임: 매주 월·수 (발표 v5 일정 슬라이드) · 12/6 보고서 완성", cex = 0.72, col = col_mut)
dev.off()

cat("그림 저장:", paste(list.files(fig_dir), collapse = ", "), "\n")
