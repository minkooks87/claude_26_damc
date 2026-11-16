# check_reproducibility.R
# 목적: 분석 스크립트를 새 R 세션(--vanilla)에서 두 번 실행해 결과 CSV가 똑같은지 확인한다.
# 사용법 (프로젝트 폴더에서):
#   Rscript --vanilla check_reproducibility.R code/NN_topic.R output/NN_topic
# 비교 대상: out_dir 안의 *.csv 중 이름이 "_"로 시작하지 않는 파일

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("사용법: Rscript --vanilla check_reproducibility.R <분석스크립트> <결과폴더>")
script  <- args[1]
out_dir <- args[2]
rscript <- file.path(R.home("bin"), "Rscript")

run_once <- function(tag) {
  cat("실행", tag, ":", script, "\n")
  status <- system2(rscript, c("--vanilla", shQuote(script)), stdout = FALSE, stderr = FALSE)
  if (status != 0) stop(tag, " 실행 실패 (종료 코드 ", status, "). 스크립트를 직접 실행해 오류를 확인하세요.")
  f <- list.files(out_dir, pattern = "^[^_].*\\.csv$", full.names = TRUE)
  setNames(unname(tools::md5sum(f)), basename(f))
}

h1 <- run_once("1차")
h2 <- run_once("2차")

all_files <- union(names(h1), names(h2))
res <- data.frame(file = all_files,
                  run1 = unname(h1[all_files]), run2 = unname(h2[all_files]))
res$same <- !is.na(res$run1) & res$run1 == res$run2
print(res, row.names = FALSE)

writeLines(c(paste("script:", script), paste("time:", format(Sys.time())),
             paste("result:", if (all(res$same)) "PASS" else "FAIL"),
             capture.output(print(res, row.names = FALSE))),
           file.path(out_dir, "_reproducibility.txt"))

if (all(res$same)) cat("\n재현성 확인: PASS (", nrow(res), "개 파일 동일)\n") else
  stop("재현성 확인: FAIL - 결과가 실행마다 다릅니다. 난수 시드, 정렬 순서, 외부 상태를 확인하세요.")
