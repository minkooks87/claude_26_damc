# C08_절단점_경계_점검.R
# 목적: 음주 수준 절단점 후보를 정하기 전에, 노출(주당 음주잔수)의 '실제로 나올 수 있는 값'을 확인한다.
#   (1) 응답 범주 조합으로 나올 수 있는 값 목록과 실제 관측값 목록
#   (2) 후보 절단점마다 '이상(≥)'과 '초과(>)'에 따라 분류가 달라지는 사람이 있는지
#   (3) 서로 다른 후보 절단점이 사실상 같은 분류를 만드는지(겹치는 후보)
#   (4) 폭음 문항(BD2_31·BD2_32)의 응답 분포와 1회 음주량(BD2_1)과의 관계 → 사용 여부 판단 자료
# 원칙: 고요산혈증·요산과의 교차는 하지 않는다(요산은 대상자 선정 조건에만 씀). 결과를 보고 기준을 고르지 않기 위함.
# 대상자·변수 정의: 팀 저장소 code/13_v3_main_analysis.R(minkooks87/claude_26_damc, 커밋 3f85edd)과 같다.
#   음주 빈도·1회량·폭음 BD1_11·BD2_1·BD2_14·BD2_31·BD2_32: 제8기 지침서 PDF p.161-162 / 인쇄 p.153-154 (팀 코드의 인용; 이 PC에서는 PDF 원문 대조 못 함)
#   홍조 BD9_1·BD9_2: 제8기 지침서 PDF p.163-164 / 인쇄 p.155-156 (팀 코드의 인용)
#   통합가중치: 제8기 지침서 PDF p.36 / 인쇄 p.28 (각 연도 1/3)
# 자료: data/ 폴더는 읽기만 한다.
# 실행: Rscript --vanilla code/C08_절단점_경계_점검.R   (프로젝트 폴더에서)
# 작성: 생성형 AI(Anthropic Claude)가 코드 작성·실행. 조원 검토 필요.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})
options(survey.lonely.psu = "adjust")

out_dir <- file.path("output", "C08_cutpoint_boundaries")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# 자료 위치: 팀 저장소 구조(data/국건영 조사결과/)를 먼저 찾고, 없으면 작성자 PC 구조를 찾는다.
find_file <- function(yy) {
  cand <- c(file.path("data", "국건영 조사결과", sprintf("hn%02d_all.sas7bdat", yy)),
            file.path("data", sprintf("hn%02d_all.sas7bdat", yy)),
            file.path("data", sprintf("HN%02d_ALL(SAS)", yy), sprintf("hn%02d_all.sas7bdat", yy)))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) stop("20", yy, "년 자료를 찾지 못함: ", paste(cand, collapse = " / "))
  hit[1]
}
files <- vapply(19:21, find_file, character(1))

# ==== 1. 자료 읽기 (필요한 변수만) ===========================================
want <- c("id", "year", "sex", "age", "psu", "kstrata", "wt_itvex", "incm", "edu",
          "bd1", "bd1_11", "bd2_1", "bd2_14", "bd2_31", "bd2_32", "bd9_1", "bd9_2",
          "dm8_dg", "he_uacid", "he_crea", "he_bmi", "he_hp", "he_dm_hba1c", "bs1_1", "bs3_1", "pa_aerobic")
read_year <- function(f) {
  nms <- names(read_sas(f, n_max = 0))
  x <- as.data.table(zap_labels(read_sas(f, col_select = all_of(nms[tolower(nms) %in% want]))))
  setnames(x, tolower(names(x)))
  x
}
d <- rbindlist(lapply(files, read_year), use.names = TRUE, fill = TRUE)
n_raw <- nrow(d)

# ==== 2. 결측 처리 (팀 코드 13의 var_dict 규칙과 같음) =======================
na_codes <- list(bd1 = c(8, 9), bd1_11 = 9, bd2_1 = 9, bd2_14 = c(888, 999), bd2_31 = 9, bd2_32 = 9,
                 bd9_1 = c(8, 9), bd9_2 = c(8, 9), dm8_dg = c(8, 9))
for (v in names(na_codes)) d[get(v) %in% na_codes[[v]], (v) := NA]
# bd2_31·bd2_32의 8은 팀 코드처럼 범주로 남겨 두고, 분포표에서 따로 센다.

# ==== 3. 노출: 주당 음주잔수 ==================================================
# 빈도 대표값(회/주): 월1회미만 0.5회/월, 월1회 1회/월, 월2-4회 3회/월(월→주 ×12/52), 주2-3회 2.5, 주4회이상 5.5
# 1회량 대표값(잔): 1-2잔 1.5, 3-4잔 3.5, 5-6잔 5.5, 7-9잔 8, 10잔 이상은 BD2_14 기입값
# 소수점 오차를 피하려고 '주당 잔수 × 52'(= 연간 잔수)를 정확한 값(k52)으로 따로 계산한다.
freq52 <- c(`2` = 6, `3` = 12, `4` = 36, `5` = 130, `6` = 286)     # 회/주 × 52
freq_lab <- c(`2` = "월1회미만", `3` = "월1회정도", `4` = "월2-4회", `5` = "주2-3회", `6` = "주4회이상")
amt <- c(`1` = 1.5, `2` = 3.5, `3` = 5.5, `4` = 8)
amt_lab <- c(`1` = "1-2잔", `2` = "3-4잔", `3` = "5-6잔", `4` = "7-9잔", `5` = "10잔이상")
d[, per_occ := fifelse(bd2_1 == 5, as.numeric(bd2_14), unname(amt[as.character(bd2_1)]))]
d[, k52 := unname(freq52[as.character(bd1_11)]) * per_occ]            # 연간 잔수(정확값)
d[, drinks_wk := k52 / 52]
d[, pattern := fifelse(is.na(bd2_1), NA_character_,
                       paste0(freq_lab[as.character(bd1_11)], " × ",
                              fifelse(bd2_1 == 5, paste0(per_occ, "잔(기입)"), amt_lab[as.character(bd2_1)])))]

# 홍조
d[, flush_now := factor(fcase(bd9_2 == 1, "비홍조", bd9_2 %in% 2:4, "현재 홍조"), c("비홍조", "현재 홍조"))]
d[, flush_grp := factor(fcase(bd9_1 == 2 & bd9_2 == 1, "비홍조", bd9_1 == 1 & bd9_2 == 1, "홍조 소실",
                              bd9_1 == 1 & bd9_2 %in% 2:4, "지속 홍조", bd9_1 == 2 & bd9_2 %in% 2:4, "신규 홍조"),
                        c("비홍조", "홍조 소실", "지속 홍조", "신규 홍조"))]

# 대상자 선정용 공변량 완비 여부 (팀 코드 13과 같은 정의)
d[, smk := NA_integer_]
d[(bs1_1 %in% c(1, 2) & bs3_1 %in% c(1, 2, 3)) | bs1_1 == 3, smk := as.integer(bs1_1 == 2 & bs3_1 %in% c(1, 2))]
d[, htn := fcase(he_hp == 3, 1L, he_hp %in% 1:2, 0L)]
d[, dm := fcase(he_dm_hba1c == 3, 1L, he_dm_hba1c %in% 1:2, 0L)]
d[, cov_ok := !is.na(age) & edu %in% 1:4 & incm %in% 1:4 & !is.na(smk) & pa_aerobic %in% 0:1 &
              !is.na(he_bmi) & !is.na(htn) & !is.na(dm) & !is.na(he_crea)]

# ==== 4. 분석 대상 (행 삭제 없이 inc 표시) ===================================
d[, inc := as.integer(!is.na(wt_itvex) & age >= 19 & bd1_11 %in% 2:6 & !is.na(drinks_wk) &
                      !is.na(flush_grp) & !is.na(he_uacid) & dm8_dg %in% 0 & cov_ok)]
d[, sexf := factor(sex, 1:2, c("남자", "여자"))]
d[, wt := wt_itvex / 3]
n_inc <- d[inc == 1, .N]; n_m <- d[inc == 1 & sex == 1, .N]; n_w <- d[inc == 1 & sex == 2, .N]
cat("분석 대상:", n_inc, "(남", n_m, "· 여", n_w, ")  — 팀 기준 10,779(남 5,411 · 여 5,368)\n")
stopifnot(n_inc == 10779, n_m == 5411, n_w == 5368)
A <- d[inc == 1]

# ==== 5. (1) 나올 수 있는 값 목록 ============================================
# 5-1. 범주 조합(빈도 5 × 1회량 4 = 20개)으로 나오는 값. 10잔 이상 기입값은 5-2에서 관측값으로 본다.
grid <- CJ(bd1_11 = 2:6, bd2_1 = 1:4)
grid[, `:=`(빈도 = freq_lab[as.character(bd1_11)], 회당 = amt_lab[as.character(bd2_1)],
            k52 = freq52[as.character(bd1_11)] * amt[as.character(bd2_1)])]
grid[, `:=`(주당잔 = round(k52 / 52, 4), 주당g_7g = round(k52 / 52 * 7, 2), 주당g_10g = round(k52 / 52 * 10, 2),
            하루g_7g = round(k52 / 52, 3))]
cnt <- A[bd2_1 %in% 1:4, .(n = .N), by = .(sexf, bd1_11, bd2_1)]
grid <- merge(grid, dcast(cnt, bd1_11 + bd2_1 ~ sexf, value.var = "n", fill = 0), by = c("bd1_11", "bd2_1"), all.x = TRUE)
setnames(grid, c("남자", "여자"), c("n_남", "n_여"), skip_absent = TRUE)
setorder(grid, k52)
fwrite(grid[, .(빈도, 회당, 주당잔, 주당g_7g, 주당g_10g, 하루g_7g, n_남, n_여)], file.path(out_dir, "01_category_grid_values.csv"), bom = TRUE)

# 5-2. 10잔 이상 기입값 조합
wi <- A[bd2_1 == 5, .(n = .N), by = .(sexf, 빈도 = freq_lab[as.character(bd1_11)], bd1_11, 기입잔 = per_occ, k52)]
wi[, `:=`(주당잔 = round(k52 / 52, 4), 주당g_7g = round(k52 / 52 * 7, 2))]
setorder(wi, sexf, k52)
fwrite(wi[, .(성별 = sexf, 빈도, 기입잔, 주당잔, 주당g_7g, n)], file.path(out_dir, "01b_writein_values.csv"), bom = TRUE)

# 5-3. 실제 관측값 목록(성별): 값마다 비가중 n, 가중 %, 누적 가중 %, 그 값을 만드는 응답 조합
obs <- A[, .(n = .N, w = sum(wt), 응답조합 = paste(unique(pattern), collapse = " / ")), by = .(sexf, k52)]
setorder(obs, sexf, k52)
obs[, `:=`(가중pct = 100 * w / sum(w), 누적가중pct = 100 * cumsum(w) / sum(w),
           아래값 = shift(k52) / 52, 위값 = shift(k52, -1) / 52), by = sexf]
obs[, `:=`(주당잔 = round(k52 / 52, 4), 주당g_7g = round(k52 / 52 * 7, 2))]
fwrite(obs[, .(성별 = sexf, 주당잔, 주당g_7g, n, 가중pct = round(가중pct, 2), 누적가중pct = round(누적가중pct, 2), 응답조합)],
       file.path(out_dir, "02_observed_values.csv"), bom = TRUE)
cat("관측된 서로 다른 값 수: 남", obs[sexf == "남자", .N], "· 여", obs[sexf == "여자", .N], "\n")

# ==== 6. (2)(3) 후보 절단점 점검 =============================================
# 후보: 단위는 주당 '국건영 잔'. g 기준은 1잔 = 7 g(주분석 가정) 또는 10 g(민감도)로 잔으로 바꿨다.
# rule: 팀 코드가 쓰는 규칙(ge = 이상, gt = 초과)
cands <- rbindlist(list(
  list(id = "P",   이름 = "계획 발표 기준(3.5·14잔)",            남 = "3.5,14",   여 = "3.5,14",   rule = "ge"),
  list(id = "J",   이름 = "Jeon 2022 5·15 g/일(7 g)",            남 = "5,15",     여 = "5,15",     rule = "ge"),
  list(id = "J4",  이름 = "Jeon 2022 4구간 5·15·30 g/일(7 g)",   남 = "5,15,30",  여 = "5,15,30",  rule = "ge"),
  list(id = "J10", 이름 = "5·15 g/일을 1잔 10 g로(3.5·10.5잔)",  남 = "3.5,10.5", 여 = "3.5,10.5", rule = "ge"),
  list(id = "Jw",  이름 = "5·15 g/일, 여성 절반(2.5·7.5)",       남 = "5,15",     여 = "2.5,7.5",  rule = "ge"),
  list(id = "B",   이름 = "교수님 제안 남 105·여 52.5 g/주(7 g)", 남 = "15",       여 = "7.5",      rule = "ge"),
  list(id = "B10", 이름 = "교수님 제안을 1잔 10 g로",             남 = "10.5",     여 = "5.25",     rule = "ge"),
  list(id = "C",   이름 = "홍조 절주지침 환산(남 16·여 8잔)",     남 = "16",       여 = "8",        rule = "ge"),
  list(id = "T",   이름 = "성별 삼분위 반올림(README)",           남 = "2.4,10",   여 = "0.4,2.5",  rule = "ge"),
  list(id = "A",   이름 = "NIAAA 고위험(남 >28·여 >14잔)",        남 = "28",       여 = "14",       rule = "gt"),
  list(id = "D",   이름 = "NAFLD 유의 음주(남 >30·여 >20잔)",     남 = "30",       여 = "20",       rule = "gt")
))
long <- rbindlist(lapply(seq_len(nrow(cands)), function(i) {
  r <- cands[i]
  rbind(data.table(id = r$id, 이름 = r$이름, rule = r$rule, sexf = "남자", cut = as.numeric(strsplit(r$남, ",")[[1]])),
        data.table(id = r$id, 이름 = r$이름, rule = r$rule, sexf = "여자", cut = as.numeric(strsplit(r$여, ",")[[1]])))
}))

# 절단점 하나마다: 같은 값(동점) 인원, 바로 아래·위 관측값, 같은 분류를 주는 구간
tol <- 1e-6
chk <- rbindlist(lapply(seq_len(nrow(long)), function(i) {
  L <- long[i]; o <- obs[sexf == L$sexf]; c52 <- L$cut * 52
  tie <- o[abs(k52 - c52) < tol]
  below <- o[k52 < c52 - tol]; above <- o[k52 > c52 + tol]
  lo <- if (nrow(below)) max(below$k52) else -Inf; hi <- if (nrow(above)) min(above$k52) else Inf
  # '이상' 규칙: (lo, 동점값 또는 hi] 안의 어떤 값을 써도 분류가 같다. 동점이 있으면 동점값이 구간의 끝.
  ge_hi <- if (nrow(tie)) tie$k52 else hi
  data.table(id = L$id, 이름 = L$이름, 성별 = L$sexf, 절단점_잔 = L$cut, 규칙 = fifelse(L$rule == "ge", "이상", "초과"),
             동점_n = if (nrow(tie)) tie$n else 0L, 동점_가중pct = if (nrow(tie)) round(tie$가중pct, 2) else 0,
             동점_응답조합 = if (nrow(tie)) tie$응답조합 else "",
             바로아래값 = round(lo / 52, 4), 바로아래_n = if (nrow(below)) below[.N, n] else NA_integer_,
             바로위값 = round(hi / 52, 4), 바로위_n = if (nrow(above)) above[1, n] else NA_integer_,
             같은분류_구간_이상규칙 = sprintf("(%s, %s]", round(lo / 52, 4), round(ge_hi / 52, 4)),
             key_lo = lo, key_hi = ge_hi)
}))
# 겹치는 후보: '이상' 규칙 기준으로 같은 구간에 놓인 절단점들(초과 규칙 후보는 동점이 없으면 같은 구간)
chk[, 같은구간_후보 := paste(unique(paste0(id, "(", 절단점_잔, ")")), collapse = ", "), by = .(성별, key_lo)]
fwrite(chk[, !c("key_lo", "key_hi")], file.path(out_dir, "03_boundary_check.csv"), bom = TRUE)

# 6-2. 후보별 칸 인원: 성별 × 현재 홍조 / 홍조 이력 × 음주 수준 (비가중 n, 성별·홍조 안 가중 %)
level_of <- function(x52, cuts, rule) {
  br <- c(-Inf, cuts * 52, Inf)
  as.integer(cut(x52, br, right = (rule == "gt"), labels = FALSE))   # ge: [a,b), gt: (a,b]
}
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt, nest = TRUE, data = as.data.frame(d[!is.na(wt)]))
cells <- rbindlist(lapply(seq_len(nrow(cands)), function(i) {
  r <- cands[i]
  rbindlist(lapply(c("남자", "여자"), function(s) {
    cuts <- as.numeric(strsplit(if (s == "남자") r$남 else r$여, ",")[[1]])
    A[sexf == s, lvl := level_of(k52, cuts, r$rule)]
    des$variables$lvl <- NA_integer_
    des$variables$lvl[des$variables$inc == 1 & des$variables$sexf == s] <- level_of(des$variables$k52[des$variables$inc == 1 & des$variables$sexf == s], cuts, r$rule)
    rbindlist(lapply(c("flush_now", "flush_grp"), function(g) {
      n <- A[sexf == s, .N, by = c(g, "lvl")]; setnames(n, g, "홍조")
      sub <- subset(des, inc == 1 & sexf == s & !is.na(lvl))
      w <- svyby(~factor(lvl), as.formula(paste0("~", g)), sub, svymean, na.rm = TRUE)
      wl <- melt(as.data.table(w)[, c(g, grep("^factor\\(lvl\\)", names(w), value = TRUE)), with = FALSE], id.vars = g,
                 variable.name = "lvl", value.name = "가중pct")
      wl[, lvl := as.integer(sub("factor\\(lvl\\)", "", lvl))]; setnames(wl, g, "홍조")
      out <- merge(n, wl, by = c("홍조", "lvl"), all = TRUE)
      cbind(id = r$id, 이름 = r$이름, 성별 = s, 홍조변수 = g, out)
    }))
  }))
}))
cells[is.na(N), N := 0L]
cells[, 가중pct := round(100 * 가중pct, 1)]
cells[, 수준 := paste0("L", lvl)]
fwrite(dcast(cells, id + 이름 + 성별 + 홍조변수 + 홍조 ~ 수준, value.var = c("N", "가중pct"), fill = 0),
       file.path(out_dir, "04_cells_by_candidate.csv"), bom = TRUE)

# 6-3. 대표값 민감도(팀 코드의 S3a·S3b)에서 생기는 동점과 경계를 넘는 사람
#   S3a: 월1회미만 = 월0.25회, 주4회이상 = 주4회 / S3b: 월1회미만 = 월0.75회, 주4회이상 = 주7회
alt <- list(S3a = replace(freq52, c("2", "6"), c(3, 208)), S3b = replace(freq52, c("2", "6"), c(9, 364)))
sens <- rbindlist(lapply(names(alt), function(a) {
  A[, k52_alt := unname(alt[[a]][as.character(bd1_11)]) * per_occ]
  rbindlist(lapply(seq_len(nrow(long)), function(i) {
    L <- long[i]; c52 <- L$cut * 52; x <- A[sexf == L$sexf]
    tie <- x[abs(k52_alt - c52) < tol]
    up_main <- if (L$rule == "ge") x$k52 >= c52 - tol else x$k52 > c52 + tol
    up_alt  <- if (L$rule == "ge") x$k52_alt >= c52 - tol else x$k52_alt > c52 + tol
    data.table(대표값 = a, id = L$id, 성별 = L$sexf, 절단점_잔 = L$cut, 규칙 = fifelse(L$rule == "ge", "이상", "초과"),
               동점_n = nrow(tie), 동점_응답조합 = paste(unique(tie$pattern), collapse = " / "),
               주분석보다_위로 = sum(!up_main & up_alt), 주분석보다_아래로 = sum(up_main & !up_alt))
  }))
}))
fwrite(unique(sens, by = c("대표값", "성별", "절단점_잔", "규칙")), file.path(out_dir, "03b_boundary_check_alt_values.csv"), bom = TRUE)

# ==== 7. (4) 폭음 문항 ========================================================
# 팀 코드 정의: BD2_31 = 한 번에 7잔 이상 마신 빈도(남자 폭음), BD2_32 = 여자 5잔 이상 빈도. 1 전혀 없음 ~ 5 거의 매일, 8 비해당, 9 모름
# 7-1. 원 코드 분포(성별) — 여성이 BD2_31에도 답했는지 확인
raw31 <- A[, .N, by = .(성별 = sexf, 코드 = fifelse(is.na(bd2_31), "NA(9)", as.character(bd2_31)))][, 문항 := "BD2_31"]
raw32 <- A[, .N, by = .(성별 = sexf, 코드 = fifelse(is.na(bd2_32), "NA(9)", as.character(bd2_32)))][, 문항 := "BD2_32"]
fwrite(setorder(rbind(raw31, raw32), 문항, 성별, 코드), file.path(out_dir, "05a_binge_raw_codes.csv"), bom = TRUE)

# 7-2. 성별 폭음 빈도(팀 정의: 남 BD2_31, 여 BD2_32) × 1회 음주량(BD2_1): 비가중 n
binge_lab <- c(`1` = "전혀없음", `2` = "월1회미만", `3` = "월1회정도", `4` = "주1회정도", `5` = "거의매일", `8` = "비해당(8)")
A[, binge := fifelse(sex == 1, bd2_31, bd2_32)]
A[, binge_f := factor(fifelse(is.na(binge), "결측(9)", binge_lab[as.character(binge)]), c(binge_lab, "결측(9)"))]
A[, amt_f := factor(amt_lab[as.character(bd2_1)], amt_lab)]
xt <- dcast(A[, .N, by = .(성별 = sexf, 회당 = amt_f, binge_f)], 성별 + 회당 ~ binge_f, value.var = "N", fill = 0)
fwrite(xt, file.path(out_dir, "05b_binge_by_amount.csv"), bom = TRUE)

# 7-3. 폭음 빈도 × 주당잔수 수준(Jeon 5·15, 남녀 공통) 및 주 1회 이상 폭음의 가중 %
A[, lv515 := factor(level_of(k52, c(5, 15), "ge"), 1:3, c("<5잔", "5-<15잔", "≥15잔"))]
xt2 <- dcast(A[, .N, by = .(성별 = sexf, 주당잔수준 = lv515, binge_f)], 성별 + 주당잔수준 ~ binge_f, value.var = "N", fill = 0)
fwrite(xt2, file.path(out_dir, "05c_binge_by_level515.csv"), bom = TRUE)

# 7-4. 모순·보완 지표(비가중 n과 성별 안 가중 %)
#   (a) 평소 1회량이 폭음 기준보다 적다고 답했는데 폭음이 주 1회 이상: 남 BD2_1 1-3(6잔 이하), 여 BD2_1 1-2(4잔 이하)
#   (b) 평소 1회량이 폭음 기준 이상인데 폭음 '전혀 없음': 남 BD2_1 4-5(7잔 이상), 여 BD2_1 3-5(5잔 이상)
#   (c) 주 5잔 미만(저음주)인데 폭음이 월 1회 이상: 평균 잔수가 놓치는 폭음
A[, `:=`(
  a_모순_적게마심_폭음주1이상 = (sex == 1 & bd2_1 %in% 1:3 | sex == 2 & bd2_1 %in% 1:2) & binge %in% 4:5,
  b_모순_많이마심_폭음없음    = (sex == 1 & bd2_1 %in% 4:5 | sex == 2 & bd2_1 %in% 3:5) & binge %in% 1,
  c_저음주인데_폭음월1이상    = k52 < 5 * 52 & binge %in% 3:5,
  d_고위험음주_국건영정의     = bd1_11 %in% 5:6 & fifelse(sex == 1, bd2_1 %in% 4:5, bd2_1 %in% 3:5)
)]
ind <- rbindlist(lapply(c("a_모순_적게마심_폭음주1이상", "b_모순_많이마심_폭음없음", "c_저음주인데_폭음월1이상", "d_고위험음주_국건영정의"), function(v)
  A[, .(지표 = v, n = sum(get(v), na.rm = TRUE), 분모 = sum(!is.na(get(v))),
        가중pct = round(100 * sum(wt * get(v), na.rm = TRUE) / sum(wt[!is.na(get(v))]), 2)), by = .(성별 = sexf)]))
fwrite(ind, file.path(out_dir, "05d_binge_consistency.csv"), bom = TRUE)

# ==== 8. 기록 =================================================================
writeLines(c(paste("실행:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), R.version.string,
             paste("haven", packageVersion("haven"), "/ data.table", packageVersion("data.table"), "/ survey", packageVersion("survey")),
             paste("대상:", n_inc, "남", n_m, "여", n_w),
             "요산·고요산혈증은 대상 선정(측정 여부)에만 사용했고 어떤 표에도 교차하지 않았다."),
           file.path(out_dir, "_run_info.txt"))
cat("완료:", out_dir, "\n")
