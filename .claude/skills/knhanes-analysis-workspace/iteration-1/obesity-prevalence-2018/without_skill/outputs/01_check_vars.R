# 01_check_vars.R
# 목적: 2018 기본DB(hn18_all)에서 비만 관련 변수의 코드값 분포와 결측 구조를 확인한다(가중치 미적용 n).
suppressPackageStartupMessages({ library(haven); library(data.table) })

f <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/data/국건영 조사결과/hn18_all.sas7bdat"
nms <- names(read_sas(f, n_max = 0))
cat("변수 수:", length(nms), "\n")
cat("임신 관련 후보 변수:", paste(grep("^LW_|preg", nms, value = TRUE, ignore.case = TRUE), collapse = " "), "\n")
want <- nms[tolower(nms) %in% c("year", "sex", "age", "he_bmi", "he_obe", "he_ht", "he_wt",
                                 "wt_itvex", "kstrata", "psu", "he_prg")]
d <- as.data.table(zap_labels(read_sas(f, col_select = all_of(want))))
setnames(d, tolower(names(d)))
print(names(d))
cat("\n전체 n:", nrow(d), " / year:\n"); print(table(d$year, useNA = "ifany"))
a <- d[age >= 19]
cat("\n19세 이상 n:", nrow(a), "\n")
cat("\nHE_obe 분포(19세 이상):\n"); print(table(a$he_obe, useNA = "ifany"))
cat("\nHE_BMI 요약(19세 이상):\n"); print(summary(a$he_bmi))
cat("\nwt_itvex 결측(19세 이상):", sum(is.na(a$wt_itvex)), "\n")
cat("\nHE_obe 결측인데 HE_BMI 있는 사람(성별):\n"); print(a[is.na(he_obe) & !is.na(he_bmi), .N, by = sex])
cat("\nHE_obe와 HE_BMI>=25 교차:\n"); print(table(obe = a$he_obe, bmi25 = a$he_bmi >= 25, useNA = "ifany"))
cat("\nHE_prg 분포(19세 이상, 성별):\n"); print(table(sex = a$sex, prg = a$he_prg, useNA = "ifany"))
cat("\nHE_prg × HE_obe 결측 여부(19세 이상 여자):\n")
print(table(prg = a[sex == 2]$he_prg, obe_na = is.na(a[sex == 2]$he_obe), useNA = "ifany"))
cat("\nHE_prg × HE_BMI 결측 여부(19세 이상 여자):\n")
print(table(prg = a[sex == 2]$he_prg, bmi_na = is.na(a[sex == 2]$he_bmi), useNA = "ifany"))
cat("\nHE_BMI 비결측인데 wt_itvex 결측:", a[!is.na(he_bmi) & is.na(wt_itvex), .N], "\n")
