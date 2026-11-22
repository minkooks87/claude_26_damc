# 34_trend_vs_categorical.py
# 목적: 주질문(지속 홍조 vs 비홍조)과 부질문 1(홍조 소실 vs 비홍조)을
#       ① 범주별 공동 검정(음주 수준 수 − 1 자유도)과 ② 1 자유도 경향성 검정(범주별 g/일 중앙값 점수)으로 했을 때
#       p값이 어떻게 다른지 기준마다 나란히 보여 준다. 부질문 2는 넣지 않는다(2026-11-22 요청).
# 입력: output/21_ptest/ptest_log.csv 중 라벨이 "T"로 시작하는 실행(/knhanes_ptest --trend=median --outcome=both "--covars=Model 2;Model 3")
# 출력: output/21_ptest/경향성비교_주질문_부질문1.md, .csv
# 실행: python code/34_trend_vs_categorical.py

import csv
import statistics
import sys
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")
LOG = Path("output/21_ptest/ptest_log.csv")
OUT = Path("output/21_ptest")
rows = list(csv.DictReader(LOG.open(encoding="utf-8-sig")))
t = [r for r in rows if r["기준"][:1] == "T" and r["기준"][1:3].isdigit()]
latest = {}
for r in t:                                             # 같은 라벨을 여러 번 돌렸으면 마지막 실행만 쓴다
    latest[r["기준"]] = max(latest.get(r["기준"], ""), r["run_time"])
t = [r for r in t if r["run_time"] == latest[r["기준"]]]
crits = sorted(latest)

QS = [("주질문", "주"), ("부질문 1", "부1")]
SEX = [("남자", "남"), ("여자", "여")]
BUND = [("Model 2", "M2"), ("Model 3", "M3")]
OUTC = [("고요산혈증", "고요산혈증(예/아니오)"), ("혈청 요산", "혈청 요산(mg/dL, 연속형)")]


def pick(crit, outc, bund, q, sex, trend):
    for r in t:
        if (r["기준"] == crit and r["결과"] == outc and r["보정"] == bund and r["구분"] == q and r["대상"] == sex
                and ("경향성" in r["검정"]) == trend and "vs 비홍조" in r["검정"]):
            return r
    return None


def num(r):
    try:
        return float(r["p"])
    except (TypeError, ValueError):
        return None


def fmt(r):
    if r is None:
        return ""
    if "분리" in r["비고"]:
        return "무효"
    p = num(r)
    s = "<0.001" if p < 0.001 else f"{p:.3f}"
    return s + ("†" if "10명" in r["비고"] else "") + ("*" if p < 0.05 and "10명" not in r["비고"] else "")


def ok(r):                                              # 해석할 수 있는 검정(† 없음, 분리 없음)
    return r is not None and num(r) is not None and "10명" not in r["비고"] and "분리" not in r["비고"]


csv_rows, md = [], []
md += ["# 주질문·부질문 1: 범주별 공동 검정 vs 1 자유도 경향성 검정", "",
       "- 생성: `code/34_trend_vs_categorical.py`. 입력은 `output/21_ptest/ptest_log.csv`의 T01–T16 실행(2026-11-22, `/knhanes_ptest --trend=median --outcome=both`).",
       "- 자료: 국민건강영양조사 2019–2021, 분석 대상 10,779명(남 5,411 · 여 5,368). 복합표본설계(wt_itvex × 1/3, kstrata, psu).",
       "- **범주** = 음주 수준 × 홍조 이력 교호작용 항의 공동 Wald 검정(음주 수준 수 − 1 자유도). **경향** = 성별·범주별 g/일 비가중 중앙값을 점수(10 g/일당)로 넣은 1 자유도 교호작용 검정.",
       "- 주 = 지속 홍조 vs 비홍조, 부1 = 홍조 소실 vs 비홍조. 부질문 2는 넣지 않았다.",
       "- 대상 기준: 3군 이상 범주형 G 기준 11개(절단점 g/일, 1잔 7 g). 2군 기준(S2, 30 g/일 2군)은 두 검정이 같고, 연속형(S5, Wood)은 처음부터 1 자유도라 뺐다.",
       "- 표시: `*` p < 0.05 · `†` 비교 칸에 고요산혈증 사례 10명 미만(해석하지 않음) · `무효` 분리로 계산 불가.", ""]
summary = []
for oc_key, oc_name in OUTC:
    for b_key, b_name in BUND:
        head = ["기준"] + [f"{qs} {ss} {kind}" for _, qs in QS for _, ss in SEX for kind in ("범주", "경향")]
        md += [f"## {oc_name} · {b_name}", "", "| " + " | ".join(head) + " |", "|" + "---|" * len(head)]
        for c in crits:
            cells = []
            for q, qs in QS:
                for sx, ss in SEX:
                    rc, rt = pick(c, oc_key, b_key, q, sx, False), pick(c, oc_key, b_key, q, sx, True)
                    cells += [fmt(rc), fmt(rt)]
                    csv_rows.append({"결과": oc_name, "보정": b_name, "기준": c, "질문": qs, "성별": ss,
                                     "범주 p": fmt(rc), "경향 p": fmt(rt), "경향 효과(10 g/일당 OR 비 또는 β 차이)": rt["효과"] if rt else "",
                                     "범주 효과(최고 수준)": rc["효과"] if rc else "", "범주 df": rc["df"] if rc else ""})
                    if ok(rc) and ok(rt):
                        summary.append((oc_name, b_name, qs, ss, num(rc), num(rt)))
            md.append(f"| {c} | " + " | ".join(cells) + " |")
        md.append("")

# 요약: 해석 가능한 쌍만
md += ["## 요약: 두 검정의 p 비교 († 없이 두 값이 모두 있는 쌍만)", "",
       "| 결과 · 보정 | 질문 | 성별 | 쌍 수 | 범주 p < 0.05 | 경향 p < 0.05 | 경향 p가 더 작은 쌍 | p 비(경향 ÷ 범주) 중앙값 |", "|---|---|---|---|---|---|---|---|"]
for oc_key, oc_name in OUTC:
    for b_key, b_name in BUND:
        for _, qs in QS:
            for _, ss in SEX:
                s = [x for x in summary if x[0] == oc_name and x[1] == b_name and x[2] == qs and x[3] == ss]
                if not s:
                    md.append(f"| {oc_name} · {b_name} | {qs} | {ss} | 0 | — | — | — | — |")
                    continue
                ratio = statistics.median(x[5] / x[4] for x in s)
                md.append(f"| {oc_name} · {b_name} | {qs} | {ss} | {len(s)} | {sum(x[4] < 0.05 for x in s)} | {sum(x[5] < 0.05 for x in s)} | "
                          f"{sum(x[5] < x[4] for x in s)} | {ratio:.2f} |")
md += ["", "- p 비가 1보다 작으면 경향성 검정의 p가 범주별 검정보다 작다는 뜻이다.",
       "- 기준 11개 × 결과 2개 × 보정 2개를 함께 본 결과다. p < 0.05 하나를 확증으로 읽지 않는다.", ""]

with (OUT / "경향성비교_주질문_부질문1.csv").open("w", encoding="utf-8-sig", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(csv_rows[0].keys()))
    w.writeheader()
    w.writerows(csv_rows)
(OUT / "경향성비교_주질문_부질문1.md").write_text("\n".join(md), encoding="utf-8")
print("기준:", len(crits), "| 비교 칸:", len(csv_rows), "| 해석 가능한 쌍:", len(summary))
print("\n".join(md[md.index("## 요약: 두 검정의 p 비교 († 없이 두 값이 모두 있는 쌍만)"):]))
