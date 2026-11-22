# 35_subq23_all_criteria.py
# 목적: 연구 질문의 부질문 2·3(주질문·부질문 1의 1 자유도 경향성 검정, 분석계획서 1장 2026-11-22 개정)의 p를
#       GitHub 음주 기준 G01–G16 전부 × 보정 묶음 6개 × 결과 2개로 정리한다.
# 입력: output/21_ptest/ptest_log.csv 중 라벨 T01–T16의 마지막 실행(2026-11-22, /knhanes_ptest)
#       - 범주형: '경향성'이 붙은 주질문·부질문 1 줄(범주별 g/일 비가중 중앙값 점수, 10 g/일당)
#       - 연속형 직선(G05, G11): 처음부터 1 자유도이므로 주질문·부질문 1 줄 자체
#       - 스플라인(G13): 기울기 하나로 줄일 수 없어 해당 없음
# 출력: output/21_ptest/부질문2_3_전체기준.md, .csv
# 실행: python code/35_subq23_all_criteria.py

import csv
import sys
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")
LOG = Path("output/21_ptest/ptest_log.csv")
OUT = Path("output/21_ptest")
rows = list(csv.DictReader(LOG.open(encoding="utf-8-sig")))
T = [r for r in rows if r["기준"][:1] == "T" and r["기준"][1:3].isdigit()]
last = {}
for r in T:
    last[r["기준"]] = max(last.get(r["기준"], ""), r["run_time"])
T = [r for r in T if r["run_time"] == last[r["기준"]]]
crits = sorted(last)
assert len(crits) == 16, crits

BUND = [("Crude", "Crude"), ("Model 2", "M2"), ("Model 2-f_smk+f_smk3", "M2S"), ("Model 2-f_smk+f_smk3-age+age10", "M2A"),
        ("Model 3", "M3"), ("Model 3-f_smk+f_smk3", "M3S")]
OUTC = [("고요산혈증", "고요산혈증(예/아니오)"), ("혈청 요산", "혈청 요산(mg/dL, 연속형)")]
QS = [("부질문 2", "주질문", "지속 홍조 vs 비홍조"), ("부질문 3", "부질문 1", "홍조 소실 vs 비홍조")]
SEX = [("남자", "남"), ("여자", "여")]


def kind(crit):
    d = next(r["음주량"] for r in T if r["기준"] == crit)
    return "spline" if "스플라인" in d else "linear" if d.startswith("연속") else "cut"


def pick(crit, oc, b, q_src, contrast, sex):
    k = kind(crit)
    if k == "spline":
        return "NA"
    for r in T:
        if (r["기준"] == crit and r["결과"] == oc and r["보정"] == b and r["구분"] == q_src and r["대상"] == sex
                and r["검정"].startswith(contrast) and (("경향성" in r["검정"]) == (k == "cut"))):
            return r
    return None


def fmt(r):
    if r == "NA":
        return "해당 없음"
    if r is None:
        return ""
    if "분리" in r["비고"]:
        return "무효"
    p = float(r["p"])
    s = "<0.001" if p < 0.001 else f"{p:.3f}"
    dag = "10명" in r["비고"]
    return s + ("†" if dag else "") + ("*" if p < 0.05 and not dag else "")


def interp(r):
    return isinstance(r, dict) and "10명" not in r["비고"] and "분리" not in r["비고"]


# 지난 G 실행(같은 기준·묶음)의 경향성 p와 같은지 대조
g_map = {}
for r in rows:
    if r["기준"][:1] == "G" and r["기준"][1:3].isdigit():
        g_map[(r["기준"][:3], r["결과"], r["보정"], r["구분"], r["대상"], r["검정"])] = r["p"]
same = diff = 0
for r in T:
    key = ("G" + r["기준"][1:3], r["결과"], r["보정"], r["구분"], r["대상"], r["검정"])
    if key in g_map:
        if abs(float(g_map[key] or "nan") - float(r["p"] or "nan")) < 1e-9 if g_map[key] and r["p"] else g_map[key] == r["p"]:
            same += 1
        else:
            diff += 1

csvrows, md = [], []
md += ["# 부질문 2·3의 p: GitHub 음주 기준 16개 전체", "",
       "- 생성: `code/35_subq23_all_criteria.py`. 입력은 `output/21_ptest/ptest_log.csv`의 T01–T16 실행(2026-11-22, `/knhanes_ptest`, 보정 묶음 6개 × 결과 2개).",
       "- 자료: 국민건강영양조사 2019–2021, 분석 대상 10,779명(남 5,411 · 여 5,368). 복합표본설계(wt_itvex × 1/3, kstrata, psu).",
       "- **부질문 2** = 주질문의 경향성(음주 점수 × 지속 홍조, 비홍조 대비, 1 자유도). **부질문 3** = 부질문 1의 경향성(음주 점수 × 홍조 소실, 비홍조 대비, 1 자유도). `분석계획서.md` 1장·6.2.",
       "- 음주 점수: 성별마다 분석 대상자 안의 각 음주 범주 g/일 비가중 중앙값 ÷ 10(10 g/일당).",
       "- 기준 형태별로 읽는 법:",
       "  - 2군 기준(G02 S2, G14 30 g/일)은 경향성 검정이 범주별 검정과 같은 값이다.",
       "  - 연속형 직선(G05, G11)은 처음부터 1 자유도 직선 검정이라 주질문·부질문 1의 p를 그대로 적었다. G05와 G11은 단위만 다른 같은 모형이다.",
       "  - 스플라인(G13)은 기울기 하나로 줄일 수 없어 '해당 없음'이다.",
       "- 표시: `*` p < 0.05 · `†` 비교 칸에 고요산혈증 사례 10명 미만(해석하지 않음) · `무효` 분리로 계산 불가.",
       "- 보정 묶음: Crude 보정 없음 · **M2(주 모형)** 연령, 교육, 소득, 조사연도, 흡연 2범주, 신체활동, 폐경(여) · M2S 흡연 3범주 · M2A M2S + 연령 10세 범주 · M3 M2 + BMI, 고혈압, 당뇨병, eGFR · M3S M3 + 흡연 3범주.",
       f"- 확인: 이번 실행의 p를 같은 기준·묶음의 지난 G 실행과 대조했더니 {same}개가 같고 {diff}개가 달랐다.", ""]

for oc, ocn in OUTC:
    for qn, qsrc, con in QS:
        head = ["기준"] + [f"{bs} {ss}" for _, bs in BUND for _, ss in SEX]
        md += [f"## {qn} · {ocn}", "", "| " + " | ".join(head) + " |", "|" + "---|" * len(head)]
        for c in crits:
            cells = []
            for b, bs in BUND:
                for sx, ss in SEX:
                    r = pick(c, oc, b, qsrc, con, sx)
                    cells.append(fmt(r))
                    csvrows.append({"결과": ocn, "질문": qn, "기준": c, "보정": bs, "성별": ss, "p": fmt(r),
                                    "효과(10 g/일당 기울기의 비 또는 β 차이)": r["효과"] if isinstance(r, dict) else "",
                                    "모형 n": r["모형 n"] if isinstance(r, dict) else "", "기준 형태": kind(c)})
            md.append(f"| {c} | " + " | ".join(cells) + " |")
        md.append("")

# 요약
md += ["## 요약: p < 0.05 개수 (해석 가능한 검정 중)", "",
       "같은 모형인 G11은 G05와 겹치므로 세지 않았다. 스플라인(G13)과 † · 무효는 분모에서 뺐다.", "",
       "| 결과 | 질문 | 성별 | " + " | ".join(bs for _, bs in BUND) + " |", "|---|---|---|" + "---|" * len(BUND)]
for oc, ocn in OUTC:
    for qn, qsrc, con in QS:
        for sx, ss in SEX:
            cells = []
            for b, bs in BUND:
                rs = [pick(c, oc, b, qsrc, con, sx) for c in crits if not c.startswith("T11")]
                ok = [r for r in rs if interp(r)]
                cells.append(f"{sum(float(r['p']) < 0.05 for r in ok)} / {len(ok)}")
            md.append(f"| {ocn.split('(')[0]} | {qn} | {ss} | " + " | ".join(cells) + " |")
md.append("")

# M2 효과 크기
md += ["## 주 모형 M2의 효과 크기 (고요산혈증: 10 g/일당 기울기의 비 = 해당 홍조군 OR ÷ 비홍조군 OR, 95% CI)", "",
       "| 기준 | 부질문 2 남 | 부질문 2 여 | 부질문 3 남 | 부질문 3 여 |", "|---|---|---|---|---|"]
for c in crits:
    cells = []
    for qn, qsrc, con in QS:
        for sx, ss in SEX:
            r = pick(c, "고요산혈증", "Model 2", qsrc, con, sx)
            if not isinstance(r, dict):
                cells.append("해당 없음" if r == "NA" else "")
                continue
            e = r["효과"].replace("최고 수준: ", "")
            cells.append(f"{e} [p {fmt(r)}]")
    md.append(f"| {c} | " + " | ".join(cells) + " |")
md += ["", "- 연속형 G05는 10 g/일당, G11은 100 g/주당 기울기의 비다.",
       "- 기준 16개 × 묶음 6개 × 결과 2개 × 성별 2개를 함께 본 결과다. p < 0.05 몇 개는 우연으로도 나올 수 있으며, 결과를 보고 기준을 고르면 사후 선택이 된다.", ""]

with (OUT / "부질문2_3_전체기준.csv").open("w", encoding="utf-8-sig", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(csvrows[0].keys()))
    w.writeheader()
    w.writerows(csvrows)
(OUT / "부질문2_3_전체기준.md").write_text("\n".join(md), encoding="utf-8")
print("기준", len(crits), "| 칸", len(csvrows), "| G 대조: 같음", same, "다름", diff)
