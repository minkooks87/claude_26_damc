# search_guide.R
# 목적: docs 폴더의 원시자료 이용지침서(PDF)에서 키워드(정규식)를 찾아
#       문서명, PDF 쪽, 인쇄 쪽(쪽 하단 숫자)과 주변 줄을 출력한다.
# 사용법 (프로젝트 폴더에서):
#   Rscript search_guide.R "<정규식>" ["<정규식2>" ...] [--doc=<문서명 일부>] [--context=3] [--max=5]
#   예) Rscript search_guide.R "^\s*N\(1\)\s+sex\b" "wt_itvex" --doc=제7기
# 주의: PDF 표가 텍스트로 풀리면서 줄이 섞일 수 있으므로, 찾은 쪽을 --page로 통째로 확인한다.
#   Rscript search_guide.R --doc=제9기 --page=101

suppressMessages(library(pdftools))

args  <- commandArgs(trailingOnly = TRUE)
opt   <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
doc_filter <- opt("doc", "")
ctx        <- as.integer(opt("context", "3"))
max_hits   <- as.integer(opt("max", "5"))
page_only  <- opt("page", "")
patterns   <- args[!grepl("^--", args)]

guide_dir <- file.path("docs", "국건영 이용지침서")
pdfs <- list.files(guide_dir, pattern = "\\.pdf$", full.names = TRUE)
if (nzchar(doc_filter)) pdfs <- pdfs[grepl(doc_filter, basename(pdfs), fixed = TRUE)]
if (!length(pdfs)) stop("지침서 PDF를 찾지 못했습니다: ", guide_dir, " / --doc=", doc_filter)

# 쪽 하단의 숫자만 있는 줄을 인쇄 쪽으로 본다 (없으면 NA)
printed_page <- function(txt) {
  lines <- trimws(strsplit(txt, "\n")[[1]])
  nums  <- lines[grepl("^[0-9]{1,4}$", lines)]
  if (length(nums)) tail(nums, 1) else NA_character_
}

for (pdf in pdfs) {
  txt <- pdf_text(pdf)
  cat("\n==========", basename(pdf), "(총", length(txt), "쪽) ==========\n")

  if (nzchar(page_only)) {
    p <- as.integer(page_only)
    cat(sprintf("[PDF p.%d / 인쇄 p.%s]\n", p, printed_page(txt[p])))
    cat(txt[p], "\n")
    next
  }

  for (pat in patterns) {
    cat("\n###", pat, "\n")
    n_found <- 0
    for (i in seq_along(txt)) {
      lines <- strsplit(txt[i], "\n")[[1]]
      hit <- grep(pat, lines, ignore.case = TRUE, perl = TRUE)
      for (h in hit) {
        if (n_found >= max_hits) break
        n_found <- n_found + 1
        rng <- max(1, h - ctx):min(length(lines), h + ctx)
        cat(sprintf("\n[PDF p.%d / 인쇄 p.%s]\n", i, printed_page(txt[i])))
        cat(paste0("  ", trimws(lines[rng], "right")), sep = "\n")
      }
    }
    if (n_found == 0) cat("  (찾지 못함)\n")
  }
}
