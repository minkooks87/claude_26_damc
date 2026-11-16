# 제7기 이용지침서에서 관련 변수의 정의와 쪽수를 찾는 보조 스크립트
library(pdftools)
pdf <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/docs/국건영 이용지침서/국민건강영양조사+제7기(2016-2018)+원시자료+이용지침서.pdf"
txt <- pdf_text(pdf)
cat("총 쪽수:", length(txt), "\n")
keys <- c("HE_BMI", "HE_obe", "wt_itvex", "kstrata", "psu", "HE_ht", "HE_wt", "비만 유병률", "age ", "sex ")
for (k in keys) {
  hit <- which(grepl(k, txt, fixed = TRUE))
  cat("\n=====", k, "=> PDF 쪽:", paste(hit, collapse = ","), "\n")
}
show <- function(p, pat, w = 300) {
  t <- txt[p]; pos <- regexpr(pat, t, fixed = TRUE)
  if (pos > 0) cat("\n--- PDF p", p, "---\n", substr(t, max(1, pos - w), pos + w * 2), "\n")
}
args <- commandArgs(TRUE)
if (length(args) >= 2) for (p in as.integer(strsplit(args[1], ",")[[1]])) show(p, args[2], 600)
