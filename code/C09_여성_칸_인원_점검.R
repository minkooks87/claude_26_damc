# C09_여성_칸_인원_점검.R
# 목적: 음주 기준 후보별로 '여자 × 홍조 이력 3군 × 음주 수준' 칸의 비가중 n을 세고,
#       예상 사례 수(n × 7.7%)가 10명 미만인 칸을 표시한다.
# 입력: output/C08_cutpoint_boundaries/04_cells_by_candidate.csv (code/C08_절단점_경계_점검.R의 결과)
#   - 대상자: 2019–2021 현재 음주자 10,779명(여 5,368). 팀 저장소 minkooks87/claude_26_damc의 code/13과 같은 정의.
#   - 홍조 이력 3군: 비홍조(BD9_1=2 & BD9_2=1), 홍조 소실(BD9_1=1 & BD9_2=1), 지속 홍조(BD9_1=1 & BD9_2=2-4).
#     신규 홍조(BD9_1=2 & BD9_2=2-4)는 팀 README대로 이력 3군이 아니므로 뺀다.
#     (BD9_1·BD9_2: 제8기 지침서 PDF p.163-164 / 인쇄 p.155-156, 팀 코드의 인용)
# 7.7%: 여성 고요산혈증 가중 유병률 7.686%(427/5,368명), 팀 저장소 output/final_analysis/04_outcome_by_sex.csv와 README.
#   칸별 결과(요산)는 보지 않는다. 전체 유병률을 모든 칸에 똑같이 곱한 '예상' 사례 수다.
# 실행: Rscript --vanilla code/C09_여성_칸_인원_점검.R   (프로젝트 폴더에서)
# 작성: 생성형 AI(Anthropic Claude)가 코드 작성·실행. 조원 검토 필요.

suppressPackageStartupMessages(library(data.table))

in_file <- file.path("output", "C08_cutpoint_boundaries", "04_cells_by_candidate.csv")
out_dir <- file.path("output", "C09_women_cell_check")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

prev   <- 0.077        # 여성 고요산혈증 유병률(가중 7.686%를 반올림)
prev_u <- 427 / 5368   # 비가중 비율 7.95% (민감도: 이 값을 쓰면 기준 n이 조금 낮아짐)
min_ev <- 10           # 예상 사례 수 기준
grp3   <- c("비홍조", "홍조 소실", "지속 홍조")

x <- fread(in_file, encoding = "UTF-8")
x <- x[성별 == "여자" & 홍조변수 == "flush_grp" & 홍조 %in% grp3]
stopifnot(nrow(x) == 11 * 3)

# 여성 절단점과 수준 이름 (C08의 cands와 같은 값)
cut_w <- c(P = "3.5, 14", J = "5, 15", J4 = "5, 15, 30", J10 = "3.5, 10.5", Jw = "2.5, 7.5",
           B = "7.5", B10 = "5.25", C = "8", T = "0.4, 2.5", A = "14 (초과)", D = "20 (초과)")
lvl_names <- function(k) switch(as.character(k), `2` = c("저", "고"), `3` = c("저", "중", "고"),
                                `4` = c("저", "중", "고", "최고"))

long <- melt(x, id.vars = c("id", "이름", "홍조"), measure.vars = patterns(N = "^N_L", 가중pct = "^가중pct_L"),
             variable.name = "lvl")
long[, lvl := as.integer(lvl)]
nlev <- long[, .(k = max(lvl[N > 0])), by = id]           # 후보별 수준 수(빈 L3·L4 제외)
long <- merge(long, nlev, by = "id")[lvl <= k]
long[, 수준 := mapply(function(k, l) lvl_names(k)[l], k, lvl)]
long[, `:=`(여성절단점_잔 = cut_w[id],
            예상사례 = round(N * prev, 1),
            예상사례_비가중비율 = round(N * prev_u, 1))]
long[, 판정 := fifelse(N * prev < min_ev, "부족(<10)", "충분")]
long[, 홍조 := factor(홍조, grp3)]
setorder(long, id, 홍조, lvl)

# 1) 긴 표: 칸마다 한 줄
fwrite(long[, .(id, 이름, 여성절단점_잔, 홍조, 수준, n = N, 가중pct_홍조군안 = 가중pct,
                예상사례, 판정, 예상사례_비가중비율)],
       file.path(out_dir, "01_women_cells_long.csv"), bom = TRUE)

# 2) 넓은 표: 후보 × 홍조 3군, 칸에 "n (예상)" 과 부족 표시(★)
long[, 칸 := sprintf("%s%d (%.1f)", fifelse(판정 == "충분", "", "★"), N, N * prev)]
wide <- dcast(long, id + 이름 + 여성절단점_잔 + 홍조 ~ 수준, value.var = "칸")
setcolorder(wide, intersect(c("id", "이름", "여성절단점_잔", "홍조", "저", "중", "고", "최고"), names(wide)))
fwrite(wide, file.path(out_dir, "02_women_cells_wide.csv"), bom = TRUE)

# 3) 후보별 요약: 부족 칸 수와 가장 작은 칸
summ <- long[, .(칸수 = .N, 부족칸수 = sum(판정 != "충분"),
                 부족칸 = paste(sprintf("%s·%s(n=%d, 예상 %.1f)", 홍조, 수준, N, N * prev)[판정 != "충분"], collapse = " / "),
                 최소n = min(N), 최소칸 = paste0(홍조, "·", 수준)[which.min(N)]), by = .(id, 이름, 여성절단점_잔)]
setorder(summ, 부족칸수, -최소n)
fwrite(summ, file.path(out_dir, "03_summary_by_candidate.csv"), bom = TRUE)

writeLines(c(paste("실행:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), R.version.string,
             paste("입력:", in_file),
             sprintf("기준: n × %.3f < %d → 부족. 즉 n ≤ %d이면 부족(n ≥ %d이면 충분)",
                     prev, min_ev, ceiling(min_ev / prev) - 1, ceiling(min_ev / prev)),
             sprintf("비가중 비율 %.4f를 쓰면 n ≤ %d이면 부족", prev_u, ceiling(min_ev / prev_u) - 1),
             "요산·고요산혈증은 칸별로 교차하지 않았다(전체 유병률만 사용)."),
           file.path(out_dir, "_run_info.txt"))
print(summ[, .(id, 여성절단점_잔, 부족칸수, 최소n, 최소칸)])
cat("완료:", out_dir, "\n")
