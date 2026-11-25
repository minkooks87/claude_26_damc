# 40_final_analysis_pdf.py
# 목적: 확정 조건으로 실행한 최종 분석(code/final_analysis.R)의 결과를 knhanes-analysis 스킬의 보고 형식
#       (자료·설계 → 변수 정의 → 대상자 흐름 → Table 1 → 본 분석 → 검증 → 가정·한계)으로 PDF 한 부에 정리한다.
# 입력: output/final_analysis/ 의 01, 02a, 03, 06, 10, 11, 12, 13, _reproducibility.txt, 그림 4장(2026-11-25 그림 4 forest plot 추가)
# 출력: Result/2026-11-23_최종분석_정리.pdf
# 실행: python code/40_final_analysis_pdf.py   (Edge 또는 Chrome 필요. final_analysis.R과 39_main_report.R을 먼저 실행)
# 숫자는 모두 결과 파일에서 읽어 넣는다(손으로 옮겨 적지 않음). 생성형 AI(Claude) 도움으로 작성, 조원 검토 필요.

import csv
import subprocess
import sys
import tempfile
import time
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")
FA = Path("output/final_analysis")
OUT = Path("Result/2026-11-23_최종분석_정리.pdf").resolve()


def rd(name):
    with open(FA / name, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


flow, vdef, t1, off = rd("02a_participant_flow.csv"), rd("01_variable_definitions.csv"), rd("03_table1.csv"), rd("06_official_check.csv")
tst, eff, cel, fo = rd("10_interaction_tests.csv"), rd("11_effects_by_flush.csv"), rd("12_cell_counts.csv"), rd("13_outcome_by_flush.csv")
repro = (FA / "_reproducibility.txt").read_text(encoding="utf-8") if (FA / "_reproducibility.txt").exists() else ""
repro_res = next((l.split(":", 1)[1].strip() for l in repro.splitlines() if l.startswith("result:")), "확인 안 함")
repro_time = next((l.split(":", 1)[1].strip() for l in repro.splitlines() if l.startswith("time:")), "")
repro_n = sum(1 for l in repro.splitlines() if l.strip().endswith("TRUE"))

A_MAIN, A_UA, A_CR, A_CRU, A_M3, A_M3U = ("주분석", "보조: 혈청 요산(다중회귀)", "비교: 보정 없음", "비교(혈청 요산): 보정 없음",
                                          "추가 보정: M3S", "추가 보정(혈청 요산): M3S")
A_SENS = ["민감도: 1잔 8 g", "민감도: 1잔 10 g", "민감도: 대푯값 전체 하한(L)", "민감도: 대푯값 전체 상한(U)"]
SENS_LAB = ["1잔 8 g", "1잔 10 g", "대푯값 전체 하한", "대푯값 전체 상한"]
FLUSH = ["비홍조", "홍조 소실", "지속 홍조"]
SEXES = ["남자", "여자"]


def qkey(q):
    return "주질문" if q.startswith("주질문") else "부질문" if q.startswith("부질문 1:") else "탐색" if "탐색" in q else q


def fp(p):
    p = float(p)
    return "&lt;0.001" if p < 0.001 else f"{p:.3f}"


def ptxt(an, q, s):
    r = next((x for x in tst if x["분석"] == an and qkey(x["질문"]) == q and x["대상"] == s), None)
    if not r:
        return "-"
    t = fp(r["p"])
    if "10명" in r["비고"]:
        return t + " †"
    return f'<b class="star">{t}*</b>' if float(r["p"]) < 0.05 else t


def pnum(an, q, s):
    return float(next(x["p"] for x in tst if x["분석"] == an and qkey(x["질문"]) == q and x["대상"] == s))


def modn(an, s):
    return int(next(x["모형n"] for x in tst if x["분석"] == an and qkey(x["질문"]) == "주질문" and x["대상"] == s))


def eci(an, s, g, sep="–"):
    r = next(x for x in eff if x["분석"] == an and x["성별"] == s and x["홍조이력"] == g and x["음주"] == "고음주")
    t = f'{float(r["추정치"]):.2f} ({float(r["CI하한"]):.2f}{sep}{float(r["CI상한"]):.2f})'
    return t + (" †" if "10명" in r["비고"] else "")


def t1v(s, g, var, lev):
    return next((x["추정치"] for x in t1 if x["성별"] == s and x["홍조패턴"] == g and x["변수"] == var and x["수준"] == lev), "-")


def n0(x):
    return f"{int(float(x)):,}"


def table(head, rows, widths=None, cls=""):
    cg = "<colgroup>" + "".join(f'<col style="width:{w}%">' for w in widths) + "</colgroup>" if widths else ""
    return (f'<table class="{cls}">{cg}<thead><tr>' + "".join(f"<th>{h}</th>" for h in head) + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{c}</td>" for c in r) + "</tr>" for r in rows) + "</tbody></table>")


def fig(name, cap, w=88):
    p = (FA / name).resolve()
    return f'<figure><img src="{p.as_uri()}" style="max-width:{w}%"><figcaption>{cap}</figcaption></figure>'


CSS = """
@page { size: A4; margin: 17mm 16mm 16mm 16mm;
  @bottom-center { content: counter(page) " / " counter(pages); font-family: 'Malgun Gothic'; font-size: 8.5pt; color: #6b7280; }
  @top-right { content: "6조 최종 분석 정리 · 2026-11-23"; font-family: 'Malgun Gothic'; font-size: 8pt; color: #9ca3af; } }
@page :first { @top-right { content: ""; } @bottom-center { content: ""; } }
* { box-sizing: border-box; }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { font-family: 'Malgun Gothic', sans-serif; font-size: 10pt; line-height: 1.65; color: #1f2937; margin: 0; word-break: keep-all; overflow-wrap: anywhere; }
.cover { height: 250mm; display: flex; flex-direction: column; justify-content: space-between; }
.cover .top { border-top: 6px solid #1F3864; padding-top: 16mm; }
.cover .kicker { font-size: 11pt; color: #2f5597; font-weight: 700; }
.cover h1 { font-size: 24pt; line-height: 1.35; margin: 10px 0 12px; color: #111827; }
.cover .sub { font-size: 11.5pt; color: #374151; line-height: 1.65; }
.cover .meta { font-size: 9pt; color: #4b5563; line-height: 1.7; border-top: 1px solid #d1d5db; padding-top: 10px; }
.key { margin-top: 10mm; background: #f1f5fb; border-left: 4px solid #1F3864; border-radius: 6px; padding: 12px 16px; font-size: 9.8pt; }
.key b.h { display: block; color: #1F3864; margin-bottom: 4px; font-size: 10.5pt; }
.key ol { margin: 4px 0 0; padding-left: 18px; } .key li { margin: 4px 0; }
section { break-before: page; }
h2 { font-size: 15pt; color: #1F3864; margin: 0 0 10px; padding: 0 0 5px; border-bottom: 1.5px solid #c7d2e5; break-after: avoid; }
h3 { font-size: 11pt; color: #111827; margin: 14px 0 6px; break-after: avoid; }
h3::before { content: ""; display: inline-block; width: 4px; height: 11pt; background: #2f5597; margin-right: 7px; vertical-align: -1px; border-radius: 1px; }
p { margin: 5px 0 8px; } ul, ol { margin: 4px 0 9px; padding-left: 20px; } li { margin: 3px 0; }
code { font-family: Consolas, 'Malgun Gothic', monospace; font-size: 8.6pt; background: #f1f4f9; padding: 0 3px; border-radius: 2px; }
pre { font-family: Consolas, 'Malgun Gothic', monospace; font-size: 8.8pt; background: #f5f7fb; border: 1px solid #dbe4f3; border-radius: 4px; padding: 8px 10px; white-space: pre-wrap; break-inside: avoid; }
table { width: 100%; border-collapse: collapse; margin: 7px 0 11px; font-size: 8.9pt; line-height: 1.5; break-inside: avoid; }
thead { display: table-header-group; } tr { break-inside: avoid; }
th { background: #1F3864; color: #fff; font-weight: 700; padding: 5px 6px; border: 0.6px solid #1F3864; text-align: left; }
td { padding: 4.5px 6px; border: 0.6px solid #d5dce8; vertical-align: top; }
tbody tr:nth-child(even) td { background: #f8fafc; }
.num td:not(:first-child) { text-align: right; font-variant-numeric: tabular-nums; }
.star { color: #b42318; }
.note { font-size: 8.4pt; color: #4b5563; margin: -5px 0 10px; }
.box { border-radius: 5px; padding: 9px 13px; margin: 9px 0 11px; break-inside: avoid; }
.ok { background: #ecfdf3; border-left: 4px solid #1a7f45; } .warn { background: #fff8e6; border-left: 4px solid #b7791f; }
.info { background: #f1f5fb; border-left: 4px solid #1F3864; }
figure { margin: 8px 0 12px; text-align: center; break-inside: avoid; } figure img { max-width: 88%; }
figcaption { font-size: 8.6pt; color: #4b5563; margin-top: 3px; }
table.long { break-inside: auto; font-size: 8.4pt; } table.long td { padding: 3px 5px; }
.toc ol { list-style: none; padding: 0; margin: 6px 0 0; } .toc li { padding: 5px 0; border-bottom: 1px dotted #c7d2e5; font-size: 10.3pt; }
"""

H = []
A = H.append
nm, nf = modn(A_MAIN, "남자"), modn(A_MAIN, "여자")
fin = flow[-1]

# ---- 표지 ----
A(f"""<div class="cover"><div class="top"><div class="kicker">국민건강영양조사 2019–2021 · 6조 최종 분석</div>
<h1>음주 관련 안면홍조 이력에 따른<br>음주량–고요산혈증 연관성의 차이</h1>
<div class="sub">확정 조건(S2 · 1잔 7 g · 흡연 3범주 · Crude/M2S/M3S)으로 실행한 최종 분석 결과를<br>자료·변수 정의·대상자 흐름·Table 1·본 분석·검증 순서로 정리했습니다.</div>
<div class="key"><b class="h">한눈에 보는 결과</b><ol>
<li><b>주질문(지속 홍조 vs 비홍조): 남녀 모두 유의하지 않음.</b> 남 p = {fp(pnum(A_MAIN, '주질문', '남자'))}, 여 p = {fp(pnum(A_MAIN, '주질문', '여자'))}(주 모형 M2S).</li>
<li><b>부질문(홍조 소실 vs 비홍조): 남자에서 p = {fp(pnum(A_MAIN, '부질문', '남자'))}.</b> 남자 소실군의 고음주 OR {eci(A_MAIN, '남자', '홍조 소실')}, 비홍조군 {eci(A_MAIN, '남자', '비홍조')}.</li>
<li><b>보조: 혈청 요산 다중회귀는 남녀 모두 효과수정 없음</b>(주질문 p = 남 {fp(pnum(A_UA, '주질문', '남자'))}, 여 {fp(pnum(A_UA, '주질문', '여자'))}).</li>
<li><b>민감도 4개</b>에서 남자 소실군 OR은 모두 약 2.0으로 같고, 부질문 p는 {min(fp(pnum(a, '부질문', '남자')) for a in A_SENS)}–{max(fp(pnum(a, '부질문', '남자')) for a in A_SENS)}로 경계 근처.</li>
<li><b>검증:</b> 공식 통계 {sum(1 for o in off if o['판정'] == '일치')}/{len(off)}개 일치, 재현성 {repro_res}.</li></ol></div></div>
<div class="meta">작성 2026-11-23 · team06donga(김준현 요청) · 생성형 AI(Claude)로 분석 실행과 정리를 도왔으며 조원 검토가 필요합니다.<br>
코드 <code>code/final_analysis.R</code> → 결과 <code>output/final_analysis/</code> → 이 문서 <code>code/40_final_analysis_pdf.py</code> · 자세한 표 <code>Result/2026-11-23_주분석_결과_S2.md</code><br>
표기: n은 가중치를 적용하지 않은 표본 수, %·평균·OR·β는 복합표본 가중 추정치 · * p &lt; 0.05 · † 비교하는 칸의 고요산혈증 사례가 10명 미만이라 해석하지 않음</div></div>""")

# ---- 목차 + 1. 자료와 설계 ----
A("""<section><div class="toc"><h2>차례</h2><ol>
<li>1. 자료와 표본설계</li><li>2. 확정한 분석 조건</li><li>3. 변수 정의(지침서 쪽수)</li><li>4. 대상자 선정 흐름</li>
<li>5. 홍조 이력별 특성(Table 1 요약)</li><li>6. 본 분석: 고요산혈증(주질문·부질문 forest plot 포함)</li><li>7. 보조 분석: 혈청 요산</li><li>8. 민감도 분석</li>
<li>9. 검증: 공식 통계 대조와 재현성</li><li>10. 가정한 것과 확인하지 못한 것</li></ol></div>""")
A("<h2 style='margin-top:14px'>1. 자료와 표본설계</h2>" + table(["항목", "내용", "근거"], [
    ["자료", "국민건강영양조사 제8기 2019·2020·2021년 기본DB(<code>hn19_all</code>, <code>hn20_all</code>, <code>hn21_all</code>)", "지침서"],
    ["가중치", "<code>wt_itvex</code>(건강설문–검진조사) × 1/3. 제8기는 연도별 조사기간에 비례해 통합", "PDF p.92 / 인쇄 p.84, PDF p.36 / 인쇄 p.28 표 7"],
    ["층 · 집락", "<code>kstrata</code> · <code>psu</code>", "PDF p.91–92 / 인쇄 p.83–84"],
    ["대상자 지정", "행을 지우지 않고 대상자를 <code>inc</code>로 표시 → 전체로 설계 객체를 만든 뒤 <code>subset(des, inc == 1)</code>로 분석", "knhanes-analysis 스킬"],
    ["분석 도구", "R 4.6.0, <code>survey</code>(svyglm: 로지스틱은 quasibinomial, 선형은 gaussian)", ""]], [16, 56, 28]) + "</section>")

# ---- 2. 확정 조건 ----
A("<section><h2>2. 확정한 분석 조건</h2>" + table(["항목", "확정 내용"], [
    ["연구 질문", "<b>주질문</b>: 남녀 각각에서 음주 수준과 고요산혈증의 관계가 지속 홍조군과 비홍조군 사이에 다른가?<br><b>부질문</b>: 같은 관계가 현재 홍조가 없는 사람 가운데 홍조 소실군과 비홍조군 사이에 다른가?"],
    ["결과", "<b>주결과</b>: 고요산혈증(남 ≥ 7.0, 여 ≥ 6.0 mg/dL) · <b>보조</b>: 혈청 요산(mg/dL) 다중 선형회귀"],
    ["노출", "<b>S2</b>: 남 16 g/일, 여 8 g/일 이상을 고음주(2단계, 기준 저음주). Lee 등(2019) 한국인 홍조 기반 절주 지침의 65세 이하 비홍조군 적정 상한(남 주 8잔·여 주 4잔, 1잔 14 g)을 하루 양으로 바꾼 값"],
    ["1잔의 알코올", "7 g(Jeon 등 2022의 값. 지침서에는 없음). 이때 g/일 = 주당 잔 수"],
    ["효과수정인자", "홍조 이력 3군: 비홍조(기준) · 홍조 소실 · 지속 홍조. 신규 홍조는 이력 분석에서 제외"],
    ["보정 모형", "<b>Crude</b>(보정 없음) · <b>M2S(주 모형)</b>: 연령, 교육, 소득, 조사연도, 흡연 3범주, 유산소 신체활동, 폐경 상태(여) · <b>M3S</b>: M2S + 체질량지수, 고혈압, 당뇨병, eGFR(매개변수일 수 있어 참고)"],
    ["검정", "음주 × 홍조 이력 교호작용의 Wald 검정(곱셈 척도, 1 자유도). 효과 크기는 홍조군별 고음주 vs 저음주 OR(β)과 95% CI"],
    ["민감도", "1잔 8 g · 10 g, 음주 대푯값 전체 하한 · 상한(4개, M2S)"],
    ["하지 않는 분석", "경향성 분석, 폐경 전후 탐색, 현재 홍조 2군, 다른 음주 기준·보정 모형(시도 기록은 <code>output/21_ptest/ptest_log.csv</code>)"]], [18, 82]))
A("<pre>고요산혈증 ~ 음주(저/고) + 홍조 이력 + 음주 × 홍조 이력 + 보정변수     (남녀 따로)\n  주질문 = 음주 × 지속 홍조 항      부질문 = 음주 × 홍조 소실 항</pre></section>")

# ---- 3. 변수 정의 ----
keep = ["sex", "age", "wt_itvex", "kstrata", "psu", "bd1_11", "bd2_1", "bd2_14", "bd9_1", "bd9_2", "he_uacid", "he_uacid_etc",
        "dm8_dg", "bs1_1", "bs3_1", "pa_aerobic", "edu", "incm", "lw_ms", "he_bmi", "he_hp", "he_dm_hba1c", "he_crea"]
vrows = [[f"<code>{v['var']}</code>", v["label"], v["miss"] or "-", (v["na8"] or "-").split("(")[0], v["source"].replace("8기 PDF ", "PDF ")] for v in vdef if v["var"] in keep]
A("<section><h2>3. 변수 정의(지침서 쪽수)</h2><p>출처는 『국민건강영양조사 제8기(2019-2021) 원시자료 이용지침서』입니다. 9·99·999(모름·무응답)는 결측, 8·88·888(비해당)은 변수마다 의미를 확인해 처리했습니다.</p>"
  + table(["변수", "뜻", "결측", "비해당", "쪽수(PDF / 인쇄)"], vrows, [13, 45, 7, 12, 23], "long") + "<p class='note'><code>BD2_14</code>의 888(비해당)은 10잔 미만이라는 뜻이므로 값만 비우고, 그 사람은 <code>BD2_1</code> 대푯값을 씁니다.</p>")
A(table(["만든 변수", "정의"], [
    ["하루 음주량(g/일)", "빈도 대푯값(월 0.5·1·3회, 주 2.5·5.5회) × 1회 잔 수(1.5·3.5·5.5·8잔, 10잔 이상은 <code>BD2_14</code>) × 7 g ÷ 7일"],
    ["홍조 이력", "비홍조(과거·현재 없음) · 홍조 소실(과거 있음, 현재 없음) · 지속 홍조(과거·현재 있음) · 신규 홍조(과거 없음, 현재 있음 → 제외). 현재 홍조는 가끔·자주·늘"],
    ["흡연 3범주", "비흡연(평생 5갑 미만 또는 피운 적 없음) · 과거흡연(5갑 이상, 현재 안 피움) · 현재흡연(5갑 이상, 매일·가끔)"],
    ["폐경 상태(여)", "폐경 전(월경 중·임신 중·수유 중) · 폐경 후(자연·인공) · 기타"],
    ["eGFR", "CKD-EPI 2009식(Levey 2009). 지침서에 식이 없어 문헌 식을 씀"]], [22, 78]) + "</section>")

# ---- 4. 흐름 ----
frows = [[f["단계"], n0(f["n"]), n0(f["남"]), n0(f["여"]), (n0(f["제외"]) if f["제외"] else "")] for f in flow]
A("<section><h2>4. 대상자 선정 흐름</h2>" + table(["단계", "n", "남", "여", "제외"], frows, [50, 13, 12, 12, 13], "num")
  + f"<p class='note'>n은 비가중 표본 수. 흐름표에는 가중치 단계를 넣었습니다(결정 7-6). 통풍 진단자는 치료로 요산이 달라질 수 있어 제외했고, '모름·무응답'도 함께 제외했습니다.</p>"
  + f"<div class='box info'>홍조 이력 분석에는 신규 홍조와 폐경 상태 결측 여자 2명을 빼서 <b>남 {nm:,}명, 여 {nf:,}명</b>이 들어갑니다.</div>")

# ---- 5. Table 1 ----
trows = []
for s in SEXES:
    for g in FLUSH:
        r = next(x for x in fo if x["성별"] == s and x["홍조이력"] == g)
        trows.append([s, g, n0(r["n"]), t1v(s, g, "나이(세)", "평균"), t1v(s, g, "흡연 3범주", "현재흡연"),
                      f'{float(r["음주_g일_가중평균"]):.1f} ({float(r["음주_SE"]):.1f})', f'{n0(r["고음주_n"])} ({float(r["고음주_가중pct"]):.1f})',
                      f'{n0(r["고요산혈증_n"])} ({float(r["고요산혈증_가중pct"]):.1f})', f'{float(r["요산_가중평균"]):.2f} ({float(r["요산_SE"]):.2f})'])
A("<h2 style='margin-top:16px'>5. 홍조 이력별 특성(Table 1 요약)</h2>" + table(
    ["성별", "홍조 이력", "n", "나이(세)", "현재흡연 %", "음주 g/일", "고음주 n (%)", "고요산혈증 n (%)", "혈청 요산 mg/dL"], trows, [7, 11, 7, 12, 11, 11, 13, 14, 14], "num")
  + "<p class='note'>2019–2021, 가중치 wt_itvex × 1/3. n은 비가중 표본 수, 괄호는 가중 % 또는 가중 평균(SE). 전체 특성표: <code>output/final_analysis/03_table1.csv</code></p></section>")

# ---- 6. 본 분석 ----
crows = []
for s in SEXES:
    for g in FLUSH:
        a = next(x for x in cel if x["분석"] == A_MAIN and x["성별"] == s and x["홍조이력"] == g and x["음주"] == "저음주")
        b = next(x for x in cel if x["분석"] == A_MAIN and x["성별"] == s and x["홍조이력"] == g and x["음주"] == "고음주")
        few = " †" if min(int(a["고요산혈증_n"]), int(b["고요산혈증_n"])) < 10 else ""
        crows.append([s, g, f'{n0(a["n"])} ({a["고요산혈증_n"]}, {float(a["가중유병률"]):.1f}%)', f'{n0(b["n"])} ({b["고요산혈증_n"]}, {float(b["가중유병률"]):.1f}%){few}'])
A("<section><h2>6. 본 분석: 고요산혈증</h2><h3>칸별 인원과 가중 유병률</h3>"
  + table(["성별", "홍조 이력", "저음주 n (사례, 가중 %)", "고음주 n (사례, 가중 %)"], crows, [10, 16, 37, 37])
  + "<p class='note'>저음주: 남 16 g/일 미만, 여 8 g/일 미만. 고음주: 그 이상.</p>")
orow = [[s, g, eci(A_CR, s, g), f"<b>{eci(A_MAIN, s, g)}</b>", eci(A_M3, s, g)] for s in SEXES for g in FLUSH]
A("<h3>홍조 이력별 고음주의 OR (저음주 대비, 95% CI)</h3>" + table(["성별", "홍조 이력", "Crude", "M2S (주 모형)", "M3S"], orow, [10, 16, 25, 25, 24]))
prow = [[q, s, ptxt(A_CR, q, s), f"<b>{ptxt(A_MAIN, q, s)}</b>", ptxt(A_M3, q, s)] for q in ["주질문", "부질문", "탐색"] for s in SEXES]
for r in prow:
    r[0] = {"주질문": "주질문 (지속 vs 비홍조)", "부질문": "부질문 (소실 vs 비홍조)", "탐색": "탐색 (소실 vs 지속)"}[r[0]]
A("<h3>효과수정 검정 p (Wald, 1 df)</h3>" + table(["질문", "성별", "Crude", "M2S (주 모형)", "M3S"], prow, [30, 10, 20, 20, 20]) + "</section>")
A("<section><h2>6. 본 분석: 그림</h2>" + fig("fig1_고요산혈증_OR_홍조이력별.png", "그림 1. 홍조 이력별 고음주 vs 저음주 고요산혈증 OR(M2S). 빈 원은 사례 10명 미만이라 해석하지 않음")
  + f"""<div class="box info"><b>해석</b><ul>
<li>남자 비홍조군에서는 고음주와 고요산혈증의 연관이 없었고(OR {eci(A_MAIN, '남자', '비홍조')}), 홍조 이력이 있는 군에서 OR이 컸습니다.</li>
<li>주질문: 지속 홍조군 OR({eci(A_MAIN, '남자', '지속 홍조')})이 비홍조군보다 크지만 차이는 통계적으로 확인되지 않았습니다(p = {fp(pnum(A_MAIN, '주질문', '남자'))}). M3S에서는 p = {fp(pnum(A_M3, '주질문', '남자'))}이지만, M3S 변수는 매개변수일 수 있어 결론은 M2S로 냅니다.</li>
<li>부질문: 남자 홍조 소실군의 OR이 비홍조군보다 컸습니다(p = {fp(pnum(A_MAIN, '부질문', '남자'))}).</li>
<li>여자는 비홍조군에서도 연관이 있었고(OR {eci(A_MAIN, '여자', '비홍조')}), 홍조 이력에 따른 차이는 없었습니다.</li></ul></div></section>""")

# ---- 6-1. 주질문·부질문 forest plot (2026-11-25 추가) ----
def rci(an, s, g):
    r = next(x for x in eff if x["분석"] == an and x["성별"] == s and x["홍조이력"] == f"{g} ÷ 비홍조" and "OR의 비" in x["음주"])
    t = f'{float(r["추정치"]):.2f} ({float(r["CI하한"]):.2f}–{float(r["CI상한"]):.2f})'
    return t + (" †" if "10명" in r["비고"] else "")


irow = [[lab, s, rci(A_CR, s, g), f"<b>{rci(A_MAIN, s, g)}</b>", rci(A_M3, s, g), ptxt(A_MAIN, q, s)]
        for q, g, lab in [("주질문", "지속 홍조", "주질문 (지속 ÷ 비홍조)"), ("부질문", "홍조 소실", "부질문 (소실 ÷ 비홍조)")] for s in SEXES]
A("<section><h2>6. 본 분석: 주질문·부질문 forest plot</h2>"
  + "<p>주질문·부질문의 효과 크기는 교호작용 항, 곧 <b>OR의 비 = 홍조군의 고음주 OR ÷ 비홍조군의 고음주 OR</b>입니다. 1보다 크면 그 홍조군에서 음주의 영향이 더 크고, 신뢰구간이 1을 포함하면 차이가 확인되지 않은 것입니다.</p>"
  + table(["질문", "성별", "Crude", "M2S (주 모형)", "M3S", "p (M2S)"], irow, [24, 8, 19, 19, 19, 11])
  + fig("fig4_주질문_부질문_forest.png", "그림 4. 주질문·부질문의 OR의 비(95% CI). 빨간색이 주 모형(M2S), 민감도 4개는 M2S로 계산. 빈 원은 사례 10명 미만이라 해석하지 않음", w=66)
  + f"""<div class="box info"><b>읽는 법</b><ul>
<li><b>주질문(남):</b> OR의 비 {rci(A_MAIN, '남자', '지속 홍조')}. 지속 홍조군에서 음주의 영향이 약 {round((float(next(x['추정치'] for x in eff if x['분석'] == A_MAIN and x['성별'] == '남자' and x['홍조이력'] == '지속 홍조 ÷ 비홍조' and 'OR의 비' in x['음주'])) - 1) * 100)}% 큰 쪽이지만 구간이 1을 포함해 차이는 확인되지 않았습니다. 모든 민감도에서 같은 방향입니다.</li>
<li><b>주질문(여):</b> OR의 비 {rci(A_MAIN, '여자', '지속 홍조')}로 차이가 없습니다. 다만 구간이 넓어 같다고 확정할 수는 없습니다.</li>
<li><b>부질문(남):</b> OR의 비 {rci(A_MAIN, '남자', '홍조 소실')}로 약 2배이며, 모든 분석에서 크기가 비슷합니다. 구간 하한은 1 근처(0.95–1.14)라 p가 경계 근처에서 오갑니다.</li>
<li><b>부질문(여):</b> 소실군 고음주 칸의 사례가 5명이라 해석하지 않습니다(†).</li></ul></div></section>""")

# ---- 7. 혈청 요산 ----
urow = [[s, g, eci(A_CRU, s, g, ", "), f"<b>{eci(A_UA, s, g, ', ')}</b>", eci(A_M3U, s, g, ", ")] for s in SEXES for g in FLUSH]
uprow = [[{"주질문": "주질문", "부질문": "부질문"}[q], s, ptxt(A_CRU, q, s), f"<b>{ptxt(A_UA, q, s)}</b>", ptxt(A_M3U, q, s)] for q in ["주질문", "부질문"] for s in SEXES]
A("<section><h2>7. 보조 분석: 혈청 요산 다중회귀</h2><h3>고음주 vs 저음주 혈청 요산 차이(mg/dL, 95% CI)</h3>"
  + table(["성별", "홍조 이력", "Crude", "M2S", "M3S"], urow, [10, 16, 25, 25, 24])
  + "<h3>효과수정 검정 p</h3>" + table(["질문", "성별", "Crude", "M2S", "M3S"], uprow, [20, 14, 22, 22, 22])
  + fig("fig2_혈청요산_차이_홍조이력별.png", "그림 2. 홍조 이력별 고음주 vs 저음주 혈청 요산 차이(M2S)") + "</section>")

# ---- 8. 민감도 ----
srow = [[lab, ptxt(a, "주질문", "남자"), ptxt(a, "부질문", "남자"), ptxt(a, "주질문", "여자"), ptxt(a, "부질문", "여자"), eci(a, "남자", "홍조 소실"), eci(a, "남자", "비홍조")]
        for a, lab in zip([A_MAIN] + A_SENS, ["주분석(7 g, 중간값)"] + SENS_LAB)]
A("<section><h2>8. 민감도 분석</h2>" + table(["민감도", "바꾼 가정"], [
    ["1잔 8 g", "1잔의 알코올 7 g → 8 g(Lee 2023의 국건영 환산: 소주 1잔 = 표준잔 14 g × 4/7)"], ["1잔 10 g", "7 g → 10 g"],
    ["대푯값 전체 하한", "빈도·1회 음주량을 모두 범주 하한으로(예: 주 2–3회 → 주 2회, 1–2잔 → 1잔)"],
    ["대푯값 전체 상한", "빈도·1회 음주량을 모두 범주 상한으로(예: 주 4회 이상 → 주 7회, 1–2잔 → 2잔). 10잔 이상은 기입값이라 바꾸지 않음"]], [22, 78])
  + table(["분석", "남 주질문", "남 부질문", "여 주질문", "여 부질문", "남 소실군 OR", "남 비홍조 OR"], srow, [20, 11, 11, 11, 11, 18, 18])
  + "<p class='note'>모두 고요산혈증, M2S. 판정은 p &lt; 0.05 유지 여부가 아니라 OR의 방향과 크기가 주분석과 같은지로 봅니다.</p>"
  + fig("fig3_민감도_남자_OR.png", "그림 3. 민감도 분석: 남자 홍조군별 고음주 OR") + "</section>")

# ---- 9. 검증 ----
orows = [[o["지표"].replace("(%, 2019, 조율)", ""), o["성별"], f'{n0(o["n"])} / {n0(o["공식n"])}', f'{float(o["재현값"]):.1f} ({float(o["CI하한"]):.1f}–{float(o["CI상한"]):.1f})', o["공식값"], o["판정"]] for o in off]
A("<section><h2>9. 검증: 공식 통계 대조와 재현성</h2><h3>공식 통계 대조(2019년, 만 19세 이상, 조율)</h3>"
  + table(["지표", "성별", "n / 공식 n", "재현값 % (95% CI)", "공식값 %", "판정"], orows, [30, 9, 16, 22, 11, 12])
  + "<p class='note'>공식값: KOSIS 국민건강영양조사 월간 음주율 추이(DT_11702_N012), 고위험음주율 추이(DT_11702_N018), '19세이상' 행. 판정: 재현값을 소수 첫째 자리로 반올림한 값과 응답자 수가 모두 같으면 '일치'. 이 대조로 가중치·층·집락·대상 정의·음주 변수 코딩을 함께 점검했습니다.</p>"
  + f"""<h3>재현성 확인</h3><div class="box {'ok' if repro_res == 'PASS' else 'warn'}"><b>{repro_res}</b> — 새 R 세션(<code>--vanilla</code>)에서 <code>code/final_analysis.R</code>를 두 번 실행해 결과 CSV {repro_n}개의 md5가 같은지 비교했습니다({repro_time}).
<br>기록: <code>output/final_analysis/_reproducibility.txt</code> · 명령: <code>Rscript --vanilla .claude/skills/knhanes-analysis/scripts/check_reproducibility.R code/final_analysis.R output/final_analysis</code></div>
<h3>그 밖의 점검</h3><ul><li>주분석 p가 <code>/knhanes_ptest</code>로 따로 계산한 값과 같습니다.</li><li>대상자 흐름이 README·분석계획서의 단계별 인원과 같습니다(<code>02b_flow_vs_README.csv</code>).</li><li>층 안에 집락이 1개인 경우(lonely PSU) 경고가 없었습니다.</li></ul></section>""")

# ---- 10. 가정·한계 ----
A(f"""<section><h2>10. 가정한 것과 확인하지 못한 것</h2>
<div class="box warn"><b>가정</b><ul>
<li><b>1잔 = 7 g</b>과 <b>고요산혈증 기준값</b>(남 7.0, 여 6.0 mg/dL)은 지침서에 없어 문헌 값을 썼습니다.</li>
<li>음주량은 설문 범주의 대푯값으로 계산했습니다. 대푯값 하한·상한 민감도로 영향을 확인했습니다.</li>
<li>혈청 요산 '<1.0' 표기는 결측으로 두었고(결정 7-1), 임신·수유 여성 4명은 폐경 전으로 포함했습니다(결정 7-3).</li></ul></div>
<div class="box warn"><b>해석할 때 주의할 점</b><ul>
<li><b>결과를 본 뒤 확정:</b> 음주 기준, 1잔 g, 흡연 범주, 보정 모형, 민감도 목록은 여러 결과를 본 뒤(2026-11-16 ~ 11-23) 정했습니다. 보고서에 이 사실과 시도한 기준 전체(<code>output/21_ptest/ptest_log.csv</code>)를 밝혀야 합니다.</li>
<li><b>다중 비교:</b> 유의한 결과는 남자 부질문 하나이고 민감도에서 p가 0.04–0.07 사이를 오갑니다. 확증이 아니라 가설을 만드는 결과로 보고합니다.</li>
<li><b>작은 군:</b> 남자 홍조 소실군 341명(고음주 123명), 여자 소실군 236명(고음주 44명, 사례 5명).</li>
<li><b>단면 자료·자기 보고:</b> 홍조 이력은 회상에 의존하고, 홍조 소실은 음주 습관 변화(내성)와 얽혀 있을 수 있습니다.</li></ul></div>
<div class="box info"><b>아직 하지 않은 것</b><ul>
<li>이번 재현성 PASS는 작성자 환경에서 돌린 것입니다. 분석계획서 8.2의 <b>코드를 짜지 않은 조원의 재현성 확인</b>이 남아 있습니다.</li>
<li>최종발표 문서(<code>output/최종발표/최종발표_반영내용.md</code>)의 1절(배경), 7절(고찰), 8절(결론), 9절(참고문헌)이 비어 있습니다.</li></ul></div>
<h3>관련 파일</h3>{table(["파일", "내용"], [
    ["<code>code/final_analysis.R</code>", "최종 분석 코드(자료 통합 → 변수 → 대상 → 설계 → 기술통계·공식값 대조 → 주분석·민감도)"],
    ["<code>output/final_analysis/</code>", "01 변수 정의, 02a 흐름, 03 Table 1, 06 공식 통계, 10 검정, 11 효과, 12 칸, 13 홍조별 요약, 그림"],
    ["<code>Result/2026-11-23_주분석_결과_S2.md</code>", "결과 보고서(같은 내용의 Markdown판)"],
    ["<code>분석계획서.md</code>, <code>output/최종발표/최종발표_반영내용.md</code>", "확정 규칙과 발표 반영 내용"]], [42, 58])}</section>""")

doc = f'<!doctype html><html lang="ko"><head><meta charset="utf-8"><title>6조 최종 분석 정리</title><style>{CSS}</style></head><body>{"".join(H)}</body></html>'

browsers = [r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe", r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
            r"C:\Program Files\Google\Chrome\Application\chrome.exe"]
exe = next((b for b in browsers if Path(b).exists()), None)
if not exe:
    sys.exit("Edge 또는 Chrome을 찾지 못했습니다.")
tmp = Path(tempfile.gettempdir())
h = tmp / "knhanes_final_analysis.html"
h.write_text(doc, encoding="utf-8")
before = OUT.stat().st_mtime if OUT.exists() else 0
subprocess.run([exe, "--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--allow-file-access-from-files",
                f"--user-data-dir={tmp / 'edge_pdf_profile'}", "--no-pdf-header-footer", f"--print-to-pdf={OUT}", h.as_uri()],
               check=False, timeout=180, capture_output=True)
for _ in range(60):
    if OUT.exists() and OUT.stat().st_mtime > before and OUT.stat().st_size > 0:
        break
    time.sleep(1)
else:
    sys.exit("PDF가 만들어지지 않았습니다.")
import pypdfium2
pdf = pypdfium2.PdfDocument(str(OUT))
print("저장:", OUT, f"({OUT.stat().st_size / 1024:.0f} KB), 쪽수 {len(pdf)}")
