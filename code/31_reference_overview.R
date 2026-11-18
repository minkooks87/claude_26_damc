# 31_reference_overview.R
# 목적: reference/*/summaries/의 논문 요약 파일을 모두 읽어, 논문마다 한 줄(학술지·대상·핵심 결과)로 정리한 한눈 표를 만든다.
# 입력: reference/Drinking|Flushing|Covariates/summaries/*.md (조원과 Claude가 원문·초록을 읽고 쓴 요약)
# 출력: Result/2026-11-18_논문별_요약_한눈표.md
# 실행: Rscript --vanilla code/31_reference_overview.R
# 주의: 표의 글은 요약 파일의 첫 문장을 그대로 옮긴 것이다. 원문을 새로 해석하지 않는다.

suppressPackageStartupMessages(library(data.table))
new_files <- if (length(commandArgs(TRUE))) readLines(commandArgs(TRUE)[1], encoding = "UTF-8") else character(0)  # 이번에 새로 쓴 요약 파일 이름

topics <- c(Drinking = "음주", Flushing = "홍조", Covariates = "보정변수")
fs <- unlist(lapply(names(topics), function(t) list.files(file.path("reference", t, "summaries"), pattern = "[.]md$", full.names = TRUE)))

# 한 절의 본문(다음 ## 전까지)
section <- function(x, head) {
  i <- grep(paste0("^## ", head), x)[1]; if (is.na(i)) return("")
  j <- grep("^## ", x); j <- j[j > i][1]; if (is.na(j)) j <- length(x) + 1
  paste(trimws(x[(i + 1):(j - 1)][nzchar(trimws(x[(i + 1):(j - 1)]))]), collapse = " ")
}
# 첫 문장(“다.”로 끝나는 곳까지). 너무 길면 두 번째 문장은 붙이지 않는다.
first_sent <- function(s, max_n = 2, max_chr = 260) {
  s <- gsub("[*`]", "", s); s <- gsub("[[:space:]]+", " ", s)
  if (!nzchar(s)) return("")
  pos <- gregexpr("다[.)]?[.]? ", paste0(s, " "))[[1]]
  if (pos[1] < 0) return(s)
  ends <- pos + attr(pos, "match.length") - 1
  out <- substr(s, 1, ends[1])
  if (length(ends) >= 2 && max_n >= 2 && ends[2] <= max_chr) out <- substr(s, 1, ends[2])
  trimws(out)
}
section_lines <- function(x, head) {
  i <- grep(paste0("^## ", head), x)[1]; if (is.na(i)) return(character(0))
  j <- grep("^## ", x); j <- j[j > i][1]; if (is.na(j)) j <- length(x) + 1
  l <- trimws(x[(i + 1):(j - 1)]); l[nzchar(l)]
}
# 주요 결과: 글머리표로 쓴 요약은 핵심(HR·OR·위험 등) 글머리표 2개, 아니면 첫 한두 문장
key_result <- function(x) {
  l <- section_lines(x, "주요 결과"); b <- sub("^[-*] ", "", l[grepl("^[-*] ", l)])
  if (!length(b)) return(first_sent(paste(l, collapse = " "), 2))
  k <- b[grepl("HR|OR|RR|위험|연관|높|낮|증가|감소|관련", b) & !grepl("^(군별 |분위별 )?n[: ]", b)]
  if (!length(k)) k <- b
  gsub("[*`]", "", paste(head(k, 2), collapse = " / "))
}
meta <- function(x, key) { l <- grep(paste0("^- ", key, ":"), x, value = TRUE)[1]; if (is.na(l)) "" else trimws(sub(paste0("^- ", key, ":"), "", l)) }

rows <- rbindlist(lapply(fs, function(f) {
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  nm <- sub("[.]md$", "", basename(f))
  parts <- strsplit(nm, ", ")[[1]]
  topic_dir <- basename(dirname(dirname(f)))
  bib <- sub(" DOI .*$", "", meta(x, "서지"))
  # 학술지: 서지에서 마지막 "학술지명. 연도;권..." 부분만
  jr <- sub("^.*[.] ([^.]+[.]? (19|20)[0-9]{2}[^.]*)[.]?$", "\\1", bib)
  if (identical(jr, bib)) {                       # 권·쪽이 없는 서지: 학술지명과 연도만
    jr <- sub("^.*[.] ([^.]+[.]? (19|20)[0-9]{2})[.].*$", "\\1", bib); if (identical(jr, bib)) jr <- "" }
  ft <- sub("[.]md$", "", basename(f))
  has_ft <- any(file.exists(file.path("reference", topic_dir, "full text", paste0(ft, c(".pdf", ".md")))))
  src <- meta(x, "원문 파일")
  if (!has_ft && grepl("음주량선정기준", src)) has_ft <- TRUE
  data.table(분야 = topics[[topic_dir]], 연도 = parts[1], 저자 = parts[2], 주제 = paste(parts[-(1:2)], collapse = ", "),
             학술지 = jr, 요약근거 = meta(x, "요약 근거"), 원문 = ifelse(has_ft, "있음", "없음"),
             대상 = first_sent(section(x, "대상과 자료"), 1), 결과 = key_result(x),
             새로추가 = basename(f) %in% new_files,
             링크 = paste0("../", paste(vapply(strsplit(f, "/")[[1]], URLencode, "", reserved = TRUE), collapse = "/")))  # 괄호·대괄호까지 인코딩
}))
rows[, 분야 := factor(분야, c("음주", "홍조", "보정변수"))]
setorder(rows, 분야, 연도, 저자)
rows[, 번호 := .I]
cl <- function(s) gsub("[|]", "/", s)

md <- c("# 논문별 요약 한눈 표 (2026-11-18)", "",
  "작성: 2026-11-18 · team06donga. 생성형 AI(Claude)로 정리했으며, 조원 검토가 필요합니다.", "",
  sprintf("- **대상:** `reference/`의 논문 요약 파일 %d편(음주 %d · 홍조 %d · 보정변수 %d).", nrow(rows), sum(rows$분야 == "음주"), sum(rows$분야 == "홍조"), sum(rows$분야 == "보정변수")),
  "- **표 내용:** 각 요약 파일에서 \"대상과 자료\"의 첫 문장, \"주요 결과\"의 첫 한두 문장을 그대로 옮겼습니다. 숫자의 근거는 링크한 요약 파일과 원문에서 확인하세요.",
  "- **요약 근거:** 요약을 원문 전문으로 썼는지, 초록만 보고 썼는지입니다. 원문이 저장소에 없으면 원문 = 없음입니다.",
  sprintf("- **이번에 새로 쓴 요약 %d편**은 번호 옆에 ★로 표시했습니다. 그중 보정변수 6편(Park 2012, Liu 2014, Kim 2018 두 편, Bae 2023, Shin & Chang 2025)은 원문 PDF가 저장소에 없습니다. 그래서 `full text/`에 있던 초록 기반 요약을 바탕으로 썼습니다. 나머지 5편은 원문 전문을 읽고 썼습니다. 그 밖의 요약은 조원들이 앞서 쓴 요약이며 고치지 않았습니다.", sum(rows$새로추가)),
  "- **만든 코드:** `code/31_reference_overview.R`", "")
for (t in levels(rows$분야)) {
  r <- rows[분야 == t]
  md <- c(md, sprintf("## %s (%d편)", t, nrow(r)), "",
          "| 번호 | 논문 | 주제 | 학술지 | 대상과 자료 | 주요 결과 | 요약 근거 · 원문 |", "|---|---|---|---|---|---|---|",
          r[, sprintf("| %s%s | [%s, %s](%s) | %s | %s | %s | %s | %s · %s |", 번호, ifelse(새로추가, " ★", ""), 연도, 저자, 링크, cl(주제), cl(학술지),
                      cl(대상), cl(결과), cl(요약근거), 원문)], "")
}
con <- file("Result/2026-11-18_논문별_요약_한눈표.md", open = "w", encoding = "UTF-8"); writeLines(md, con); close(con)
cat("논문 수:", nrow(rows), "/ 새로 추가:", sum(rows$새로추가), "/ 학술지 빈칸:", sum(!nzchar(rows$학술지)), "/ 결과 빈칸:", sum(!nzchar(rows$결과)), "\n")
print(rows[!nzchar(학술지) | !nzchar(결과) | !nzchar(대상), .(번호, 연도, 저자)])
