# 02_obesity_prevalence_2018.R
# 목적: 2018년 국민건강영양조사 기본DB(hn18_all)로 만 19세 이상 성인의 성별 비만 유병률(BMI 25 kg/m2 이상)을
#       복합표본설계(가중치 wt_itvex, 층 kstrata, 집락 psu)를 반영해 추정한다.
#
# 변수 근거(국민건강영양조사 제7기(2016-2018) 원시자료 이용지침서; 쪽수는 인쇄 쪽 / PDF 쪽):
#   - sex(1 남자, 2 여자), age(만나이, 80=80세이상), psu: 85쪽 / PDF 93쪽
#   - wt_itvex(건강설문-검진조사 가중치), kstrata(분산추정 층): 86쪽 / PDF 94쪽
#   - HE_BMI, HE_obe: 189쪽 / PDF 197쪽
#       2017-2018 HE_obe: 1 저체중, 2 정상, 3 비만전단계, 4 1단계비만(25~<30), 5 2단계비만(30~<35), 6 3단계비만(35+),
#       정의는 "(비임신자 중)" 체질량지수 기준
#   - 2017년 성인 비만 유병률 산출 예제: age>=19 & HE_obe in (1~6) 이면 OBE = (HE_obe in (4,5,6)),
#     wt_itvex / kstrata / psu 사용: 34쪽 / PDF 42쪽 (표 14)
#   - HE_prg(임신여부: 0 아니오, 1 예, 8 비해당(남자)): 184쪽 / PDF 192쪽
#
# 주 분석: HE_obe 기준(지침서 정의 = 임신부 제외). 민감도 분석: HE_BMI >= 25(임신부 포함).
# 원시자료는 읽기만 한다.

suppressPackageStartupMessages({ library(haven); library(data.table); library(survey) })

out_dir <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/.claude/skills/knhanes-analysis-workspace/iteration-1/obesity-prevalence-2018/without_skill/outputs"
f <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/data/국건영 조사결과/hn18_all.sas7bdat"

nms  <- names(read_sas(f, n_max = 0))
want <- nms[tolower(nms) %in% c("year", "sex", "age", "he_bmi", "he_obe", "he_prg", "wt_itvex", "kstrata", "psu")]
d <- as.data.table(zap_labels(read_sas(f, col_select = all_of(want))))
setnames(d, tolower(names(d)))
stopifnot(all(d$year == 2018))

# --- 분석 변수 생성 ---
# HE_obe, HE_BMI는 연속/범주 생성변수로 9/99 같은 무응답 코드가 없고 결측은 NA로 들어 있음(01_check_vars.R로 확인).
d[, adult := age >= 19]
d[, obe  := fifelse(he_obe %in% 1:6, as.numeric(he_obe %in% 4:6), NA_real_)]   # 주 분석
d[, obe2 := fifelse(!is.na(he_bmi), as.numeric(he_bmi >= 25), NA_real_)]       # 민감도 분석
d[, sex_f := factor(sex, levels = 1:2, labels = c("남자", "여자"))]

# --- 복합표본설계 ---
# 가중치가 없는 사람(검진·설문 미참여)은 설계 객체에서 제외. 이들은 HE_BMI도 모두 결측임(01_check_vars.R).
# 하위집단(19세 이상, 해당 변수 비결측)은 subset()으로 도메인 분석해 분산을 올바르게 계산한다.
cat("가중치 결측으로 설계에서 제외된 n:", d[is.na(wt_itvex), .N], "\n")
dd <- d[!is.na(wt_itvex)]
options(survey.lonely.psu = "adjust")   # 단일 PSU 층 대비(안전장치)
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex, data = dd, nest = TRUE)
cat("층 수:", length(unique(dd$kstrata)), " / PSU 수:", length(unique(paste(dd$kstrata, dd$psu))),
    " / 자유도:", degf(des), "\n")

est_one <- function(var, label) {
  sub <- subset(des, adult & !is.na(get(var)))
  fml <- as.formula(paste0("~", var))
  df  <- degf(des)
  one <- function(ds, grp) {
    m  <- svymean(fml, ds)
    ci <- confint(m, df = df)
    dat <- ds$variables
    data.table(정의 = label, 성별 = grp,
               분모_n = nrow(dat), 비만_n = sum(dat[[var]] == 1),
               유병률_pct = 100 * coef(m)[[1]], SE_pct = 100 * SE(m)[[1]],
               CI95_하한 = 100 * ci[1, 1], CI95_상한 = 100 * ci[1, 2],
               추정_비만인구 = coef(svytotal(fml, ds))[[1]])
  }
  rbind(one(sub, "전체"),
        one(subset(sub, sex == 1), "남자"),
        one(subset(sub, sex == 2), "여자"))
}

res_main <- est_one("obe",  "주분석: HE_obe 4~6 (비임신자, BMI>=25)")
res_sens <- est_one("obe2", "민감도: HE_BMI>=25 (임신부 포함)")

# 성별 차이 검정(Rao-Scott 보정 카이제곱)
sub_main <- subset(des, adult & !is.na(obe))
chi <- svychisq(~obe + sex_f, sub_main, statistic = "F")
print(chi)

# 비만 단계별 분포(참고표, 주분석 대상자)
lvl <- c("1 저체중(<18.5)", "2 정상(18.5~<23)", "3 비만전단계(23~<25)",
         "4 1단계비만(25~<30)", "5 2단계비만(30~<35)", "6 3단계비만(>=35)")
sub_main <- update(sub_main, obe_cat = factor(he_obe, levels = 1:6, labels = lvl))
stage_fun <- function(ds, grp) {
  m <- svymean(~obe_cat, ds); ci <- confint(m, df = degf(des))
  data.table(성별 = grp, 단계 = lvl, n = as.integer(table(factor(ds$variables$he_obe, levels = 1:6))),
             가중_pct = 100 * coef(m), SE_pct = 100 * SE(m), CI95_하한 = 100 * ci[, 1], CI95_상한 = 100 * ci[, 2])
}
stage <- rbind(stage_fun(sub_main, "전체"), stage_fun(subset(sub_main, sex == 1), "남자"),
               stage_fun(subset(sub_main, sex == 2), "여자"))

# --- 표본 흐름 ---
flow <- data.table(
  단계 = c("2018 기본DB 전체", "만 19세 이상", "  그중 wt_itvex 결측(미참여)", "  그중 HE_BMI 결측(가중치 있음)",
           "  그중 임신부(HE_prg=1, HE_BMI 있음)", "주분석 대상(HE_obe 비결측)"),
  n = c(nrow(d), d[adult == TRUE, .N], d[adult == TRUE & is.na(wt_itvex), .N],
        d[adult == TRUE & !is.na(wt_itvex) & is.na(he_bmi), .N],
        d[adult == TRUE & he_prg %in% 1 & !is.na(he_bmi), .N], d[adult == TRUE & !is.na(obe), .N]))

# --- 저장 ---
rnd <- function(x) { x <- copy(x); for (v in names(x)) if (is.double(x[[v]])) set(x, j = v, value = round(x[[v]], 2)); x }
res <- rbind(res_main, res_sens)
res_out <- rnd(res); res_out[, 추정_비만인구 := round(res$추정_비만인구)]
fwrite(res_out, file.path(out_dir, "obesity_prevalence_2018_by_sex.csv"), bom = TRUE)
fwrite(rnd(stage), file.path(out_dir, "obesity_stage_distribution_2018_by_sex.csv"), bom = TRUE)
fwrite(flow, file.path(out_dir, "sample_flow_2018.csv"), bom = TRUE)

options(width = 200)
cat("\n=== 표본 흐름 ===\n"); print(flow)
cat("\n=== 성별 비만 유병률 ===\n"); print(res_out)
cat("\n=== 비만 단계 분포 ===\n"); print(rnd(stage))
cat("\n성별 차이 Rao-Scott F p값:", format.pval(chi$p.value, digits = 3), "\n")
cat("\n", R.version.string, "/ survey", as.character(packageVersion("survey")), "\n")
