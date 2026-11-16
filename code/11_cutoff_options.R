# 11_cutoff_options.R
# 목적: 음주 수준 기준값 후보(남녀 공통)별로 칸 인원과 가중 분포를 비교한다.
#       노출(음주)·효과수정변수(홍조)만 쓰고, 요산 결과는 계산하지 않는다.
# 입력: 09_part2_descriptives.R 를 source 해서 같은 대상(inc == 1)과 주당 잔수(drinks_wk)를 쓴다.
# 실행: Rscript --vanilla code/11_cutoff_options.R   (프로젝트 폴더에서)
# 가정: 국건영 1잔 ≈ 7 g (소주 1병 = 7잔, 지침서 PDF p.162 / 인쇄 p.154; 도수 약 17%) → g/일 = 잔/주 × 7 / 7 = 잔/주

invisible(capture.output(source("code/09_part2_descriptives.R", encoding = "UTF-8")))

opts <- list(
  "V3: 3.5·14잔/주"                       = c(3.5, 14),
  "g/일 5·15 (= 5·15잔/주)"               = c(5, 15),
  "g/일 5·15·30 (= 5·15·30잔/주)"         = c(5, 15, 30),
  "한국 홍조 지침 환산: 8·16잔/주"        = c(8, 16)
)
res <- rbindlist(lapply(names(opts), function(nm) {
  br <- c(-Inf, opts[[nm]], Inf)
  lab <- paste0("L", seq_len(length(br) - 1))
  x <- d[inc == 1, .(sex, flush_now, wt, lev = cut(drinks_wk, br, right = FALSE, labels = lab))]
  x[, .(n = .N, w = sum(wt)), by = .(sex, flush_now, lev)][, `:=`(기준 = nm, pct = round(100 * w / sum(w), 1)), by = .(sex, flush_now)][]
}))
res[, 성별 := fifelse(sex == 1, "남", "여")]
tab <- dcast(res, 기준 + 성별 + flush_now ~ lev, value.var = "n", fill = 0)
pct <- dcast(res, 기준 + 성별 + flush_now ~ lev, value.var = "pct", fill = 0)
fwrite(tab, file.path(out_dir, "08_cutoff_options_n.csv"), bom = TRUE)
fwrite(pct, file.path(out_dir, "08_cutoff_options_pct.csv"), bom = TRUE)
print(tab); print(pct)

# 추천안(5·15잔/주)의 성별 가중 % (발표 슬라이드 갱신용, 노출만)
d[, drink_g := cut(drinks_wk, c(-Inf, 5, 15, Inf), right = FALSE, labels = c("저", "중", "고"))]
s2 <- subset(svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt, nest = TRUE, data = d), inc == 1)
print(svyby(~drink_g, ~sex, s2, svymean))

# 기준을 3.5→5, 14→15로 옮길 때 범주가 바뀌는 사람(빈도 × 1회량 칸)
mv <- d[inc == 1 & ((drinks_wk >= 3.5 & drinks_wk < 5) | (drinks_wk >= 14 & drinks_wk < 15)),
        .N, by = .(sex, bd1_11, bd2_1, wk = round(drinks_wk, 2))][order(wk)]
print(mv)
