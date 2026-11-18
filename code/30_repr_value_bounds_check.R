# 30_repr_value_bounds_check.R
# 목적: 음주 빈도·1회량 대표값을 범주 하한값·상한값으로 바꾸면 분석 대상(10,779명)의 g/일 분포와
#       주요 기준의 음주 수준 분류가 얼마나 달라지는지 미리 본다(민감도 분석 계획용, 검정은 하지 않음).
# 근거: 문항·응답 범주는 8기 지침서 PDF p.161–162(인쇄 p.153–154). 지침서에는 범주를 횟수·잔으로 바꾸는 값이 없다.
# 입력: code/13_v3_main_analysis.R 0–3절(변수·대상·설계). data/는 읽기만 한다.
# 출력: output/22_repr_sensitivity/ (응답 분포, 시나리오별 g/일 요약, 기준별 재분류표)

suppressPackageStartupMessages({ library(data.table); library(survey) })
.src <- readLines("code/13_v3_main_analysis.R", encoding = "UTF-8")
.stop <- grep("^# ==== 4\\.", .src)[1]
fwrite_orig <- data.table::fwrite
fwrite <- function(...) invisible(NULL)                  # 준비 단계가 output/13 파일을 다시 쓰지 않게 막는다
suppressWarnings(eval(parse(text = .src[seq_len(.stop - 1)], encoding = "UTF-8"), envir = globalenv()))
rm(fwrite)
out <- file.path("output", "22_repr_sensitivity"); dir.create(out, showWarnings = FALSE, recursive = TRUE)

v <- d[inc == 1L]
stopifnot(nrow(v) == 10779)
v[, 성별 := fifelse(sex == 1, "남", "여")]
v[, w := wt_itvex / 3]

# ---- 1. 시나리오: 빈도(회/주)와 1회량(잔) ----
wk <- 12 / 52                                             # 회/월 → 회/주
fq <- list(
  주분석 = c(`2` = 0.5 * wk, `3` = 1 * wk, `4` = 3 * wk, `5` = 2.5, `6` = 5.5),
  하한   = c(`2` = (1 / 12) * wk, `3` = 1 * wk, `4` = 2 * wk, `5` = 2, `6` = 4),   # 월1회미만의 하한 = 1년에 1번(최근 1년 음주자)
  상한   = c(`2` = 1 * wk, `3` = 1 * wk, `4` = 4 * wk, `5` = 3, `6` = 7),
  S3a    = c(`2` = 0.25 * wk, `3` = 1 * wk, `4` = 3 * wk, `5` = 2.5, `6` = 4),     # code/13 기존 민감도(열린 범주만 작게)
  S3b    = c(`2` = 0.75 * wk, `3` = 1 * wk, `4` = 3 * wk, `5` = 2.5, `6` = 7))     # code/13 기존 민감도(열린 범주만 크게)
am <- list(주분석 = c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8),
           하한 = c(`1` = 1, `2` = 3, `3` = 5, `4` = 7),
           상한 = c(`1` = 2, `2` = 4, `3` = 6, `4` = 9))
scen <- data.table(
  시나리오 = c("주분석", "L 전체 하한", "U 전체 상한", "Lf 빈도만 하한", "Uf 빈도만 상한", "La 1회량만 하한", "Ua 1회량만 상한", "S3a 열린 빈도 작게", "S3b 열린 빈도 크게"),
  f = c("주분석", "하한", "상한", "하한", "상한", "주분석", "주분석", "S3a", "S3b"),
  a = c("주분석", "하한", "상한", "주분석", "주분석", "하한", "상한", "주분석", "주분석"))
gday <- function(f, a) {
  per <- fifelse(v$bd2_1 == 5, as.numeric(v$bd2_14), unname(am[[a]][as.character(v$bd2_1)]))
  unname(fq[[f]][as.character(v$bd1_11)]) * per         # 주당 잔수 = g/일(1잔 7 g)
}
for (i in seq_len(nrow(scen))) v[, (scen$시나리오[i]) := gday(scen$f[i], scen$a[i])]
stopifnot(isTRUE(all.equal(v$주분석, v$drinks_wk)))      # 주분석 값이 code/13과 같은지 확인

# ---- 2. 응답 분포(비가중 n, 가중 %) ----
fl <- c(`2` = "월1회미만", `3` = "월1회정도", `4` = "월2-4회", `5` = "주2-3회", `6` = "주4회이상")
al <- c(`1` = "1-2잔", `2` = "3-4잔", `3` = "5-6잔", `4` = "7-9잔", `5` = "10잔 이상(직접 기입)")
dist <- rbind(
  v[, .(n = .N, w = sum(w)), by = .(성별, 응답 = fl[as.character(bd1_11)], k = bd1_11)][, 문항 := "빈도 BD1_11"],
  v[, .(n = .N, w = sum(w)), by = .(성별, 응답 = al[as.character(bd2_1)], k = bd2_1)][, 문항 := "1회량 BD2_1"])
dist[, `가중 %` := round(100 * w / sum(w), 1), by = .(성별, 문항)]
setorder(dist, 문항, k, 성별)
dist <- dcast(dist, 문항 + k + 응답 ~ 성별, value.var = c("n", "가중 %"))
setorder(dist, -문항, k); dist[, k := NULL]
fwrite_orig(dist, file.path(out, "01_응답분포.csv"), bom = TRUE)
b14 <- v[bd2_1 == 5, .(n = .N, 최솟값 = min(bd2_14), 중앙값 = median(bd2_14), 최댓값 = max(bd2_14)), by = 성별]

# ---- 3. 시나리오별 g/일 요약(가중 중앙값·평균) ----
ds <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~w, data = v, nest = TRUE)
wq <- function(x, w, p) { o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= p)[1]] }
summ <- rbindlist(lapply(scen$시나리오, function(s) v[, .(시나리오 = s, `가중 중앙값` = round(wq(get(s), w, 0.5), 2),
  `가중 평균` = round(weighted.mean(get(s), w), 2), `주분석 대비 평균 배율` = round(weighted.mean(get(s), w) / weighted.mean(주분석, w), 2)), by = 성별]))
setorder(summ, 성별)
fwrite_orig(summ, file.path(out, "02_시나리오별_gday요약.csv"), bom = TRUE)

# ---- 4. 기준별 재분류: 주분석 군과 하한·상한 군이 다른 사람 ----
crit <- list(
  "제안 1: 15·30 3군"       = list(남 = c(15, 30), 여 = c(15, 30)),
  "제안 3: 30 이상 2군"     = list(남 = 30, 여 = 30),
  "S2 절주지침 남16/여8"    = list(남 = 16, 여 = 8),
  "S3 남5·15/여2.5·7.5"     = list(남 = c(5, 15), 여 = c(2.5, 7.5)),
  "S4 계획 발표 3.5·14"     = list(남 = c(3.5, 14), 여 = c(3.5, 14)),
  "S1 성별 삼분위(고정)"    = list(남 = c(2.4, 10), 여 = c(0.4, 2.5)),
  "제안 5: 5·15·30 4군"     = list(남 = c(5, 15, 30), 여 = c(5, 15, 30)))
grp <- function(x, br) as.integer(cut(x, c(-Inf, br, Inf), right = FALSE))
recl <- rbindlist(lapply(names(crit), function(cn) rbindlist(lapply(c("남", "여"), function(sx) {
  u <- v[성별 == sx]; br <- crit[[cn]][[sx]]; g0 <- grp(u$주분석, br)
  rbindlist(lapply(c("L 전체 하한", "U 전체 상한", "S3a 열린 빈도 작게", "S3b 열린 빈도 크게"), function(s) {
    g1 <- grp(u[[s]], br); ch <- g1 != g0
    data.table(기준 = cn, 성별 = sx, 시나리오 = s, `바뀐 n` = sum(ch), `바뀐 가중 %` = round(100 * sum(u$w[ch]) / sum(u$w), 1),
               `최고군 n(주분석→시나리오)` = sprintf("%d→%d", sum(g0 == max(g0)), sum(g1 == length(br) + 1)),
               `최고군 지속홍조 n` = sprintf("%d→%d", sum(g0 == length(br) + 1 & u$flush_grp == "지속 홍조"), sum(g1 == length(br) + 1 & u$flush_grp == "지속 홍조")))
  }))
}))))
fwrite_orig(recl, file.path(out, "03_기준별_재분류.csv"), bom = TRUE)

# ---- 5. 칸별 g/일(빈도 × 1회량) 주분석·하한·상한 ----
cell <- CJ(f = 2:6, a = 1:4)
cell[, 칸 := paste(fl[as.character(f)], "×", al[as.character(a)])]
cell[, `주분석` := round(fq$주분석[as.character(f)] * am$주분석[as.character(a)], 2)]
cell[, `하한` := round(fq$하한[as.character(f)] * am$하한[as.character(a)], 2)]
cell[, `상한` := round(fq$상한[as.character(f)] * am$상한[as.character(a)], 2)]
cell <- merge(cell, v[bd2_1 %in% 1:4, .(n = .N), by = .(f = bd1_11, a = bd2_1)], by = c("f", "a"), all.x = TRUE)
cell[is.na(n), n := 0L]
fwrite_orig(cell[, .(칸, n, 주분석, 하한, 상한)], file.path(out, "04_칸별_gday.csv"), bom = TRUE)

options(width = 220)
cat("\n== 응답 분포 ==\n"); print(dist)
cat("\n== 10잔 이상 직접 기입값(BD2_14) ==\n"); print(b14)
cat("\n== 시나리오별 g/일 ==\n"); print(summ)
cat("\n== 기준별 재분류 ==\n"); print(recl)
cat("\n== 칸별 g/일 ==\n"); print(cell[, .(칸, n, 주분석, 하한, 상한)])
cat("\n홍조군 수준:", paste(levels(v$flush_grp), collapse = " / "), "\n")
