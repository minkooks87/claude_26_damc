# 26_ptest_summary_G.R
# 목적: /knhanes_ptest로 돌린 GitHub 음주 기준 전체(라벨 G01–G16)의 질문별 p를 한 표로 모은다.
#       결과는 고요산혈증과 혈청 요산(연속형) 둘 다, 보정 묶음은 GitHub 설정안(Result/2026-11-16_보정변수_설정안_검증.md 5-3절)의 6개.
# 입력: output/21_ptest/ptest_log.csv (ptest가 누적 기록). 같은 기준을 여러 번 돌렸으면 마지막 실행만 쓴다.
# 출력: output/21_ptest/G_요약_질문별p.csv, output/21_ptest/G_요약_질문별p.md
# 실행: Rscript --vanilla code/26_ptest_summary_G.R

suppressPackageStartupMessages(library(data.table))
out <- file.path("output", "21_ptest")
lg <- fread(file.path(out, "ptest_log.csv"), encoding = "UTF-8")
lg <- lg[grepl("^G[0-9]{2} ", 기준)]
lg <- lg[, .SD[run_time == max(run_time)], by = 기준]
lg[, id := substr(기준, 1, 3)]
setmap <- c("Crude" = "Crude", "Model 2" = "M2", "Model 2-f_smk+f_smk3" = "M2S", "Model 2-f_smk+f_smk3-age+age10" = "M2A",
            "Model 3" = "M3", "Model 3-f_smk+f_smk3" = "M3S")
lg[, 묶음 := setmap[보정]]
stopifnot(!anyNA(lg$묶음))
fp <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
lg[, 표시 := ifelse(grepl("분리", 비고), "무효", paste0(fp(p), ifelse(grepl("사례 10명", 비고), "†", "")))]
lg[, 경향 := grepl("경향성", 검정)]
# 핵심 검정 이름 붙이기
lg[, 항목 := fcase(
  구분 == "주질문" & !경향, paste0("주 ", substr(대상, 1, 1)),
  구분 == "주질문" & 경향, paste0("주(경향) ", substr(대상, 1, 1)),
  구분 == "부질문 1" & !경향, paste0("부1 ", substr(대상, 1, 1)),
  구분 == "부질문 1" & 경향, paste0("부1(경향) ", substr(대상, 1, 1)),
  구분 == "부질문 2" & !경향, "부2 성별",
  구분 == "부질문 2" & 경향, "부2(경향) 성별",
  구분 == "부질문 2(탐색)", "부2 폐경",
  default = NA_character_)]
core <- lg[!is.na(항목)]
fwrite(core[, .(기준, 결과, 묶음, 구분, 검정, 대상, `모형 n`, df, p, 효과, 비고)], file.path(out, "G_요약_질문별p.csv"), bom = TRUE)

cols <- c("주 남", "주 여", "부1 남", "부1 여", "부2 성별", "부2 폐경", "주(경향) 남", "부1(경향) 남", "주(경향) 여", "부1(경향) 여")
wide <- function(oc, set) {
  x <- dcast(core[결과 == oc & 묶음 == set], 기준 ~ 항목, value.var = "표시")
  for (c in setdiff(cols, names(x))) x[[c]] <- ""
  x[order(기준), c("기준", cols), with = FALSE]
}
md <- c("# GitHub 음주 기준 전체의 질문별 p (/knhanes_ptest, 2026-11-16)", "",
        "- 기준: `Result/2026-11-16_보정변수_설정안_검증.md`의 S1–S5, `reference/Drinking/음주량선정기준`의 문헌 기준, 지난 제안 기준(15·30, 스플라인, 30 g 2군, WHO, 5·15·30). 1잔 = 7 g, g/일 = 주당 잔수.",
        "- 보정 묶음(설정안 5-3절): Crude, M2 = Model 2(주 모형), M2S = 흡연 3범주, M2A = M2S + 연령 10세 범주, M3 = Model 3, M3S = M3의 흡연 3범주. 폐경 상태는 Crude를 뺀 모든 묶음의 여자 모형에 들어간다.",
        "- 주 = 지속 홍조 vs 비홍조, 부1 = 홍조 소실 vs 비홍조, 부2 성별 = 지속 vs 비홍조 차이의 남녀 비교(3원), 부2 폐경 = 같은 차이의 폐경 전후 비교(여자). (경향) = 범주 중앙값 점수 1 df 경향성 검정.",
        "- † = 비교하는 칸에 고요산혈증 사례 10명 미만(해석하지 않음, 고요산혈증만 해당). 무효 = 칸이 비어 계산이 성립하지 않음.",
        "- 여러 기준·묶음을 함께 본 결과이므로 하나의 p < 0.05를 확증으로 읽지 않는다. 주분석 기준과 주 모형은 미리 정한 것을 쓴다(설정안 5-1절).", "")
for (oc in c("고요산혈증", "혈청 요산")) for (set in c("M2", "M2S", "M3")) {
  w <- wide(oc, set)
  md <- c(md, sprintf("## %s · %s", oc, set), "", paste0("| ", paste(names(w), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(w)), collapse = "|"), "|"),
          apply(as.matrix(w), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")), "")
}
# p < 0.05 개수(무효 제외, †는 따로 셈)
cnt <- core[!grepl("분리", 비고) & !grepl("경향", 항목), .(검정수 = .N, `p<0.05` = sum(p < 0.05, na.rm = TRUE), `그중 †` = sum(p < 0.05 & grepl("사례 10명", 비고), na.rm = TRUE)), keyby = .(결과, 묶음)]
md <- c(md, "## p < 0.05 개수 (경향성 제외, 무효 제외)", "", "| 결과 | 묶음 | 검정 수 | p<0.05 | 그중 † |", "|---|---|---|---|---|",
        cnt[, sprintf("| %s | %s | %d | %d | %d |", 결과, 묶음, 검정수, `p<0.05`, `그중 †`)], "")
con <- file(file.path(out, "G_요약_질문별p.md"), open = "w", encoding = "UTF-8"); writeLines(md, con); close(con)

options(width = 260, datatable.print.nrows = 300, datatable.print.class = FALSE)
for (oc in c("고요산혈증", "혈청 요산")) for (set in c("M2", "M2S", "M3")) { cat(sprintf("\n==== %s · %s ====\n", oc, set)); print(wide(oc, set)) }
cat("\n[묶음별 p<0.05]\n"); print(cnt)
cat("\n[연속형·스플라인의 효과 크기, M2]\n")
print(lg[묶음 == "M2" & id %in% c("G05", "G11", "G13") & 구분 %in% c("주질문", "부질문 1", "부질문 2", "용량-반응"), .(id, 결과, 구분, 검정, 대상, p = fp(p), 효과)])
cat("\n[경향성: 홍조군별 음주 경향성(10 g/일당), M2, 고요산혈증, G12(15·30)·G16(5·15·30)]\n")
print(lg[묶음 == "M2" & 결과 == "고요산혈증" & id %in% c("G12", "G16") & 구분 == "용량-반응", .(id, 검정, 대상, p = fp(p), 효과, 비고)])
cat("\n저장:", file.path(out, "G_요약_질문별p.md"), "\n")
