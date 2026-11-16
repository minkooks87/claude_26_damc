# C07_meno_covariate_check.R
# 목적: 폐경 상태를 여성 보정변수로 항상 넣을 때(README 4.2, 2026-11-16) 여성 분석 인원이 줄어드는지 확인 (결과 변수는 보지 않음)
#       - LW_ms 코드(code/13 70행의 지침서 정리): 1 초경 전, 2 월경 중, 3 임신 중, 4 수유 중, 5 자연폐경, 6 인공폐경, 7 기타, 9 모름·무응답
#       - ptest.R 규칙: 2·3·4 폐경 전, 5·6 폐경 후, 7 기타, 그 밖(1, 9, NA)은 결측 → 그 여성은 보정 모형에서 빠짐
# 자료: knhanes_2019_2021.sav (읽기만 함). 대상자 정의는 B02/C02와 같음(최종 10,779명)
# 작성: 생성형 AI(Anthropic Claude)가 코드 작성·실행. 조원 검토 필요.

suppressPackageStartupMessages({ library(foreign); library(data.table) })
sav <- "C:/Users/travi/OneDrive/문서/카카오톡 받은 파일/knhanes_2019_2021.sav"
D <- as.data.table(suppressWarnings(read.spss(sav, to.data.frame = TRUE, use.value.labels = FALSE, reencode = "UTF-8")))
setnames(D, tolower(names(D)))
freq_wk <- c(`2` = 0.5 * 12 / 52, `3` = 1 * 12 / 52, `4` = 3 * 12 / 52, `5` = 2.5, `6` = 5.5)
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
D[, f := freq_wk[as.character(bd1_11)]]
D[, a := fifelse(bd2_1 %in% 1:4, amt[as.character(bd2_1)], fifelse(bd2_1 == 5 & bd2_14 >= 10 & bd2_14 < 999, bd2_14, NA_real_))]
D[, dw := f * a]
D[, in_s := !is.na(wt_itvex) & age >= 19 & bd1 == 2 & bd1_11 %in% 2:6 & !is.na(dw) &
            bd9_1 %in% 1:2 & bd9_2 %in% 1:4 & !is.na(he_uacid) & dm8_dg %in% 0 &
            edu %in% 1:4 & incm %in% 1:4 & bs3_1 %in% c(1, 2, 3, 8) & pa_aerobic %in% c(0, 1) &
            !is.na(he_bmi) & he_hp %in% 1:3 & he_dm_hba1c %in% 1:3 & !is.na(he_crea)]
W <- D[in_s == TRUE & sex == 2]
cat("최종 대상 여성:", nrow(W), "명\n\n[LW_ms 값 분포 (비가중 n)]\n")
print(W[, .N, by = .(lw_ms = fifelse(is.na(lw_ms), "NA", as.character(lw_ms)))][order(lw_ms)])
W[, meno_cov := fcase(lw_ms %in% 2:4, "폐경 전", lw_ms %in% 5:6, "폐경 후", lw_ms %in% 7, "기타", default = NA_character_)]
cat("\n[ptest 규칙의 폐경 상태]\n"); print(W[, .N, by = meno_cov])
cat("\n폐경 상태 결측으로 보정 모형에서 빠지는 여성:", W[is.na(meno_cov), .N], "명\n")
cat("\n[폐경 상태별 나이 (비가중 요약) — 나이와 겹치는 정도 확인]\n")
print(W[, .(n = .N, 나이_최소 = min(age), 나이_중앙값 = as.double(median(age)), 나이_최대 = max(age)), by = meno_cov])
