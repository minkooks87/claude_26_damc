# 33_repo_overview_pdf.py
# 목적: GitHub 저장소 전체 내용을 처음 보는 사람도 읽을 수 있게 정리한 PDF를 만든다.
# 입력: Result/2026-11-18_ptest_전체표_조건과p.csv (p값 표는 이 파일에서 그대로 읽는다)
#       나머지 수치의 출처는 본문의 "출처" 표시와 같다(분석계획서.md, README.md, Result 문서).
# 출력: Result/2026-11-18_저장소_전체_정리.pdf
# 실행: python code/33_repo_overview_pdf.py   (프로젝트 폴더에서. Edge 또는 Chrome 필요)

import csv
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")
SRC = Path("Result/2026-11-18_ptest_전체표_조건과p.csv")
OUT = Path("Result/2026-11-18_저장소_전체_정리.pdf").resolve()

with SRC.open(encoding="utf-8-sig", newline="") as f:
    rows = list(csv.DictReader(f))
HUA, UA = "고요산혈증(예/아니오)", "혈청 요산(mg/dL, 연속형)"
g_m2 = [r for r in rows if r["단계"] == "G" and r["보정묶음"] == "M2"]
assert len(g_m2) == 32
PCOLS = ["주 남", "주 여", "부1 남", "부1 여", "부2 성별", "부2 폐경", "주 경향 남"]


def pcell(raw):
    raw = (raw or "").strip()
    if not raw:
        return '<td class="p"></td>'
    if raw == "무효":
        return '<td class="p dag">무효</td>'
    show = raw.replace("*", "")
    if "†" in raw:
        return f'<td class="p dag">{show}</td>'
    v = float(show)
    cls = "sig" if v < 0.05 else "near" if v < 0.10 else ""
    return f'<td class="p {cls}">{show}</td>'


def crit_label(s):
    s = re.sub(r"^G[0-9]{2} ", "", s)
    s = s.replace("[폴더] ", "").replace("[설정안 ", "").replace("[제안 ", "제안 ").replace("] ", " ")
    return s


def cut_label(r):
    m, f = r["남 절단점(g/일)"], r["여 절단점(g/일)"]
    if not m:
        return "스플라인" if "스플라인" in r["음주량형태"] else r["연속형 단위"]
    return m if m == f else f"남 {m}<br>여 {f}"


def p_table(outcome):
    data = sorted([r for r in g_m2 if r["결과변수"] == outcome], key=lambda r: r["기준"])
    head = "".join(f"<th>{c}</th>" for c in PCOLS)
    body = []
    for r in data:
        cls = ' class="cand"' if "설정안" in r["기준"] else ""
        body.append(f"<tr{cls}><td class=\"c\">{r['기준'][:3]}</td><td>{crit_label(r['기준'])}</td><td class=\"cut\">{cut_label(r)}</td>"
                    + "".join(pcell(r[c]) for c in PCOLS) + "</tr>")
    return f'<table class="ptab"><thead><tr><th>번호</th><th>음주 기준</th><th>절단점(g/일)</th>{head}</tr></thead><tbody>{"".join(body)}</tbody></table>'


def table(head, body, cls="", widths=None):
    cg = "<colgroup>" + "".join(f'<col style="width:{w}%">' for w in widths) + "</colgroup>" if widths else ""
    h = "".join(f"<th>{c}</th>" for c in head)
    b = "".join("<tr>" + "".join(f"<td>{c}</td>" for c in r) + "</tr>" for r in body)
    return f'<table class="{cls}">{cg}<thead><tr>{h}</tr></thead><tbody>{b}</tbody></table>'


CSS = """
@page { size: A4; margin: 19mm 17mm 18mm 17mm;
  @bottom-center { content: counter(page) " / " counter(pages); font-family: 'Malgun Gothic'; font-size: 8.5pt; color: #6b7280; }
  @top-right { content: "6조 저장소 전체 정리 · 2026-11-18"; font-family: 'Malgun Gothic'; font-size: 8pt; color: #9ca3af; } }
@page :first { @top-right { content: ""; } }
* { box-sizing: border-box; }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { font-family: 'Malgun Gothic', sans-serif; font-size: 10pt; line-height: 1.62; color: #1f2937; margin: 0; word-break: keep-all; overflow-wrap: anywhere; }
.cover { border-bottom: 2.5px solid #1F3864; padding-bottom: 12px; margin-bottom: 16px; }
.kicker { font-size: 9.5pt; color: #1F3864; font-weight: 700; }
h1.title { font-size: 21pt; line-height: 1.32; margin: 5px 0 7px; color: #111827; }
.meta { font-size: 8.8pt; color: #4b5563; line-height: 1.55; }
h2 { font-size: 14pt; color: #1F3864; margin: 24px 0 8px; padding-bottom: 4px; border-bottom: 1.2px solid #c7d2e5; break-after: avoid; }
h3 { font-size: 10.8pt; color: #111827; margin: 14px 0 5px; break-after: avoid; }
p { margin: 5px 0 8px; }
ul, ol { margin: 4px 0 9px; padding-left: 19px; }
li { margin: 2.5px 0; }
b { color: #111827; }
code { font-family: Consolas, 'Malgun Gothic', monospace; font-size: 8.7pt; background: #f1f4f9; padding: 0 3px; border-radius: 2px; }
table { width: 100%; border-collapse: collapse; margin: 7px 0 11px; font-size: 8.9pt; line-height: 1.45; break-inside: avoid; }
thead { display: table-header-group; }
tr { break-inside: avoid; }
th { background: #1F3864; color: #fff; font-weight: 700; padding: 5px 6px; border: 0.6px solid #1F3864; text-align: left; vertical-align: middle; }
td { padding: 4.5px 6px; border: 0.6px solid #cbd5e1; vertical-align: top; }
.key { background: #f1f5fb; border-left: 3.5px solid #1F3864; padding: 9px 13px; margin: 10px 0 12px; break-inside: avoid; }
.key p:first-child { margin-top: 0; } .key ul { margin-bottom: 0; }
.warn { background: #fff8e6; border-left: 3.5px solid #b7791f; padding: 8px 12px; margin: 9px 0 11px; font-size: 9.4pt; break-inside: avoid; }
.src { font-size: 8.1pt; color: #6b7280; margin: -5px 0 10px; }
.ptab { font-size: 8pt; margin-bottom: 5px; }
.ptab th { text-align: center; padding: 4px 2px; font-size: 7.7pt; }
.ptab td { padding: 3px 4px; vertical-align: middle; }
.ptab td.c { text-align: center; color: #4b5563; }
.ptab td.cut { font-size: 7.4pt; line-height: 1.25; text-align: center; }
.ptab td.p { text-align: center; font-variant-numeric: tabular-nums; width: 7.1%; padding: 3px 1px; }
.ptab tr.cand td:nth-child(-n+3) { background: #eaf0fa; font-weight: 700; }
.sig { background: #c6efce; color: #00590f; font-weight: 700; }
.near { background: #fff2cc; }
.dag { background: #ededed; color: #7a7a7a; font-style: italic; }
.legend { font-size: 8.1pt; color: #4b5563; margin: 2px 0 12px; }
.legend span { padding: 0 5px; margin-right: 3px; }
.grid td { text-align: center; vertical-align: middle; padding: 7px 6px; }
.grid td:first-child { background: #eef2f8; font-weight: 700; }
.grid .main { background: #dce6f5; font-weight: 700; }
.grid .out { background: #f3f4f6; color: #6b7280; }
.num td:nth-child(n+3) { text-align: right; font-variant-numeric: tabular-nums; }
.flow td:first-child { text-align: center; width: 7%; }
.flow td:nth-child(3), .flow td:nth-child(4) { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
.flow tr:last-child td { background: #dce6f5; font-weight: 700; }
.pb { break-before: page; }
"""

html = []
A = html.append

A(f"""<div class="cover">
<div class="kicker">과학적 사고와 의학연구 6조 · GitHub 저장소 전체 정리</div>
<h1 class="title">음주 관련 안면홍조 이력에 따른<br>음주량–고요산혈증 연관성의 차이</h1>
<div class="meta">국민건강영양조사 2019–2021 · 2026년 11월 18일 기준(커밋 915ee92) · 저장소 minkooks87/claude_26_damc<br>
조원: 김민상 · 조용찬 · 이예은 · 박채완 · 김정윤 · 김준현 · 지도교수: 손민국<br>
저장소의 문서·코드·결과를 생성형 AI(Claude)로 읽고 정리했습니다. 조원 검토가 필요합니다. 분석 규칙의 기준 문서는 <code>분석계획서.md</code>입니다.</div>
</div>

<div class="key">
<p><b>한눈에 보기</b></p>
<ul>
<li><b>연구 질문:</b> 술을 마시면 얼굴이 빨개지는 사람은, 음주량이 늘 때 고요산혈증 위험이 더 크게 늘어날까?</li>
<li><b>지금 단계:</b> 대상자 정의, 변수, 최종 분석 코드, 공식 통계 대조까지 준비했습니다. <b>주분석에 쓸 음주 기준이 정해지지 않아 최종 주분석은 아직 실행하지 않았습니다.</b></li>
<li><b>탐색 결과:</b> 주 모형(M2)에서 주질문의 범주별 검정은 음주 기준 16개 모두 p ≥ 0.05였습니다. 여러 기준에서 반복해 p가 0.05 미만으로 나온 것은 고요산혈증의 남자 부질문 1(홍조 소실 vs 비홍조)뿐입니다.</li>
<li><b>다음 결정:</b> ① 주분석 음주 기준 ② 주 검정 형태(A안 또는 B안) ③ 1잔의 알코올 g과 절단점 단위 ④ 흡연 범주</li>
</ul>
</div>""")

A("""<h2>1. 무엇을 왜 연구하나</h2>
<p>술을 마시면 몸에서 아세트알데하이드가 생깁니다. 이를 분해하는 효소(ALDH2)가 약한 사람은 소량만 마셔도 얼굴이 빨개집니다. 이것이 <b>안면홍조</b>이고, 동아시아인의 36–45%가 경험합니다(Jeon 2022). 알코올은 혈청 요산을 높이는데, 홍조가 있는 사람에게서 그 영향이 더 클 수 있습니다. 국내 선행연구 몇 편은 홍조가 있는 사람에서 같은 양의 음주가 대사증후군, 고혈압, 당뇨병과 더 강하게 관련됨을 보였지만, 요산에 대한 근거는 적습니다.</p>
<p>홍조는 시간이 지나며 사라지기도 합니다. 그래서 이 연구는 지금의 홍조만 보지 않고, <b>처음 술을 마시던 때와 지금의 홍조를 함께 본 "홍조 이력"</b>으로 사람들을 나눕니다.</p>
<h3>연구 질문 (2026-11-16 개정)</h3>""")
A(table(["구분", "질문"], [
    ["<b>주질문</b>", "남녀 각각에서, 성별 기준으로 나눈 음주 수준과 고요산혈증의 관계는 <b>지속 홍조군과 비홍조군</b> 사이에 다른가?"],
    ["<b>부질문 1</b>", "같은 관계가 현재 홍조가 없는 사람 가운데 <b>홍조 소실군과 비홍조군</b> 사이에 다른가?"],
    ["<b>부질문 2</b>", "지속 홍조군과 비홍조군의 차이가 <b>남녀 사이</b>에서도 다른가? (각 성별 기준 안에서 많이 마시는 편일 때의 비교)"],
], widths=[14, 86]))

A("""<h2>2. 자료와 연구 대상</h2>
<p>국민건강영양조사 제8기(2019·2020·2021) 기본DB를 씁니다. 복합표본설계를 반영해 가중치 <code>wt_itvex</code> × 1/3, 층 <code>kstrata</code>, 집락 <code>psu</code>로 추정합니다.</p>""")
A(table(["단계", "기준", "남은 인원", "제외"], [
    ["0", "2019–2021 기본DB 전체", "22,559", ""],
    ["1", "만 19세 이상", "18,691", ""],
    ["2", "최근 1년 음주자", "12,231", "6,460"],
    ["3", "음주량 계산 가능, 과거·현재 홍조 응답, 혈청 요산 측정", "12,002", "229"],
    ["4", "통풍 의사진단 없음(진단자와 모름·무응답 제외)", "11,217", "785"],
    ["5", "보정변수 모두 응답 → <b>최종 대상(남 5,411 · 여 5,368)</b>", "10,779", "438"],
], cls="flow", widths=[8, 66, 14, 12]))
A('<p class="src">n은 가중치를 적용하지 않은 표본 수. 출처: 분석계획서 3장</p>')
A("""<p><b>결과 변수</b>는 고요산혈증(남 7.0, 여 6.0 mg/dL 이상)입니다. 남 1,201명(가중 23.6%), 여 427명(가중 7.7%)이 해당합니다. 혈청 요산 수치(mg/dL)도 연속형 결과로 함께 분석합니다.</p>
<h3>홍조 이력 분류 (남 / 여, 명)</h3>
<table class="grid"><thead><tr><th></th><th style="text-align:center">지금 홍조 없음</th><th style="text-align:center">지금 홍조 있음(가끔·자주·늘)</th></tr></thead><tbody>
<tr><td>처음에 홍조 없음</td><td class="main">비홍조(기준군)<br>3,181 / 3,273</td><td class="out">신규 홍조(이력 분석에서 제외)<br>301 / 400</td></tr>
<tr><td>처음에 홍조 있음</td><td class="main">홍조 소실<br>341 / 236</td><td class="main">지속 홍조<br>1,588 / 1,459</td></tr>
</tbody></table>""")

A("<h2>3. 변수와 분석 방법</h2><h3>음주량 계산</h3>")
A("<p>주당 잔 수 = 주당 음주 횟수 × 1회 잔 수입니다. 1잔을 7 g으로 보면 주당 잔 수가 그대로 g/일이 됩니다.</p>")
A(table(["문항", "응답 → 대푯값", "근거"], [
    ["음주 빈도(<code>BD1_11</code>)", "월 1회 미만 0.5회/월 · 월 1회 1회/월 · 월 2–4회 3회/월 · 주 2–3회 2.5회/주 · 주 4회 이상 5.5회/주", "아래 세 칸은 Kwon 2023 등 국내 문헌과 같음. 위 두 칸은 범주 중간값을 가정"],
    ["1회 음주량(<code>BD2_1</code>)", "1–2잔 1.5 · 3–4잔 3.5 · 5–6잔 5.5 · 7–9잔 8 · 10잔 이상은 직접 적은 값(<code>BD2_14</code>)", "Lee 2023과 같음"],
], widths=[20, 52, 28]))
A("<h3>주분석 음주 기준 후보 (미정)</h3>")
A(table(["후보", "남자", "여자", "근거"], [
    ["S1 성별 삼분위", "2.4 · 10", "0.4 · 2.5", "이 표본의 가중 분포를 반올림한 값"],
    ["S2 절주 지침", "16", "8", "Lee 2019 비홍조군 적정 상한을 1잔 7 g으로 환산"],
    ["S3 5·15, 여성 절반", "5 · 15", "2.5 · 7.5", "Jeon 2022, NIAAA 남녀 비율"],
    ["S4 계획 발표(참고)", "3.5 · 14", "3.5 · 14", "남녀 공통"],
    ["S5 연속형", "10 g/일당", "10 g/일당", "절단점 없음"],
], widths=[24, 14, 14, 48]))
A("<h3>보정 묶음</h3>")
A(table(["묶음", "들어간 변수", "역할"], [
    ["Crude", "없음", "비교용"],
    ["<b>Model 2 (M2)</b>", "연령, 교육, 소득, 조사연도, 흡연(2범주), 신체활동, 폐경 상태(여)", "<b>주 모형</b>"],
    ["M2S · M2A", "M2에서 흡연 3범주(M2S), 여기에 연령 10세 범주(M2A)", "민감도 후보"],
    ["Model 3 (M3) · M3S", "M2 + BMI, 고혈압, 당뇨병, eGFR (M3S는 흡연 3범주)", "민감도. 추가 변수는 매개변수일 수 있음"],
], widths=[20, 56, 24]))
A("""<h3>분석 모형과 해석 규칙</h3>
<ul>
<li>남녀 각각 복합표본 로지스틱 회귀(<code>svyglm</code>)로 "음주 수준 × 홍조 이력" 교호작용을 검정합니다. 혈청 요산은 복합표본 선형회귀를 씁니다.</li>
<li>부질문 2는 남녀를 합친 모형에서 음주 수준 × 홍조 이력 × 성별의 3원 교호작용을 봅니다.</li>
<li>고요산혈증 사례가 10명 미만인 칸이 있으면 그 결과는 해석하지 않습니다(표의 †).</li>
<li>시도한 기준은 모두 공개합니다. p &lt; 0.05의 개수보다 여러 기준에서 방향과 크기가 일관된지로 판단합니다.</li>
</ul>
<h2>4. 분석이 제대로 되는지 확인한 것</h2>
<p>최종 코드 <code>code/final_analysis.R</code>로 2019년 공식 통계를 다시 계산했습니다. KOSIS 공식값과 <b>6개 지표 모두 소수 첫째 자리와 응답자 수까지 일치</b>했습니다.</p>""")
A(table(["지표(2019, 만 19세 이상, 조율)", "전체", "남자", "여자"], [
    ["월간음주율 %(응답자 수)", "57.7 (6,195)", "72.4 (2,746)", "43.2 (3,449)"],
    ["고위험음주율 %", "12.1", "18.7", "5.6"],
], widths=[46, 18, 18, 18]))
A('<p class="src">출처: Result/2026-11-18_2019음주_공식통계값_KOSIS.md, output/final_analysis/06_official_check.csv</p>')
A("<p>코드를 짜지 않은 조원이 최종 코드를 두 번 실행해 결과가 같은지 확인하는 재현성 점검은 아직 남아 있습니다(<code>재현성 확인 절차.md</code>).</p>")

A('<h2 class="pb">5. 지금까지 나온 결과 (탐색 단계)</h2>')
A("""<div class="warn"><b>확정 결과가 아닙니다.</b> 주분석 기준을 정하기 전에 음주 기준 16개를 모두 돌려 본 결과입니다. 이 표를 보고 p가 작은 기준을 골라 주분석으로 삼으면 안 됩니다.</div>""")
A("<h3>고요산혈증, 주 모형 M2</h3>")
A(p_table(HUA))
LEG = '<div class="legend"><span class="sig">p &lt; 0.05</span><span class="near">0.05–0.10</span><span class="dag">† 사례 10명 미만 또는 무효(해석하지 않음)</span> 파란 줄: 계획서 후보 S1–S5 · G05와 G11은 단위만 다른 같은 모형</div>'
A(LEG)
A('<p class="src">주 = 지속 홍조 vs 비홍조 · 부1 = 홍조 소실 vs 비홍조 · 부2 성별 = 남녀 차이 · 부2 폐경 = 여성의 폐경 전후 차이(탐색) · 주 경향 남 = 남자 주질문의 1 자유도 경향성 검정. 출처: Result/2026-11-18_ptest_전체표_조건과p.csv</p>')
A("""<ul>
<li><b>주질문:</b> 범주별 검정(주 남·주 여)은 16개 기준 모두 p ≥ 0.05입니다. 지속 홍조군과 비홍조군의 관계가 다르다는 뚜렷한 증거는 아직 없습니다.</li>
<li><b>남자 경향성:</b> 주 경향 남은 성별 사분위 기준(G10)에서만 p &lt; 0.05(0.040)입니다.</li>
<li><b>남자 부질문 1:</b> 사례 부족 표시가 없는 p &lt; 0.05는 S2(0.041), 연속형(0.023), 30 g/일 2군(0.032)입니다.</li>
<li><b>여자:</b> 고요산혈증 사례가 적어 부질문 1은 16개 기준 중 13개가 †입니다.</li>
</ul>""")
A('<h3 class="pb">혈청 요산(연속형), 주 모형 M2</h3>')
A(p_table(UA))
A(LEG)
A("""<ul>
<li><b>주질문과 부질문 1:</b> 남녀 모두 16개 기준에서 p ≥ 0.05입니다. 고요산혈증의 남자 부질문 1에서 보인 신호가 혈청 요산에서는 나타나지 않았습니다.</li>
<li><b>부질문 2:</b> Park 기준의 성별 차이(0.032)와 WHO 기준의 폐경 차이(0.031)가 각각 한 기준에서만 p &lt; 0.05입니다.</li>
<li><b>주의:</b> 여러 기준과 질문을 함께 시도했으므로, p &lt; 0.05 몇 개는 우연으로도 나올 수 있습니다.</li>
</ul>""")

A("<h2>6. 결정을 돕기 위해 점검한 것</h2>")
A(table(["주제", "확인한 내용", "제안"], [
    ["<b>여성 칸 인원</b>", "여성 고요산혈증은 7.7%라 한 칸에 약 130명이 있어야 예상 사례가 10명을 넘습니다. 홍조 소실 여성은 236명뿐이라 어떤 기준으로도 충분하지 않습니다. 4단계 기준은 칸 12개 중 7개가 부족합니다.", "여성 부질문 1은 탐색으로 둡니다. 여성 주분석은 저/고 2단계가 안정적입니다."],
    ["<b>폭음 문항</b>", "주당 알코올량은 빈도와 1회량으로 이미 계산되므로, 폭음 문항을 더하면 같은 음주를 두 번 셉니다. 저음주군의 10–13%는 월 1회 이상 폭음합니다.", "노출·보정변수에는 쓰지 않고, 기술 통계와 민감도 분석에 씁니다."],
    ["<b>음주 대푯값</b>", "대푯값을 범주 하한으로 바꾸면 남자의 30 g/일 이상 군이 870명에서 301명으로 줄고, 평균 g/일은 0.71배가 됩니다. 상한으로 바꾸면 1.33배입니다. 빈도 값의 영향이 1회량보다 큽니다.", "하한·상한과 문헌값 시나리오로 민감도 분석을 합니다."],
], widths=[15, 55, 30]))
A('<p class="src">출처: Result/2026-11-18_여성_칸_인원_점검.md, 2026-11-18_폭음문항_사용여부.md, 2026-11-18_음주대표값_민감도분석_계획.md</p>')

A("<h2>7. 주 검정을 어떻게 할 것인가: A안과 B안</h2>")
A(table(["", "A안: 범주별 공동 검정(현행)", "B안: 경향성 검정(새 제안)"], [
    ["<b>묻는 것</b>", "음주 수준마다 두 군의 차이가 다른가(모양 무관)", "음주량이 늘수록 두 군의 차이가 한 방향으로 커지는가"],
    ["<b>검정</b>", "교호작용 항의 공동 Wald 검정(범주 수 − 1 자유도)", "범주 중앙값 점수의 1 자유도 교호작용"],
    ["<b>장점</b>", "어떤 모양의 차이든 잡습니다", "가설과 형태가 같고 용량 비례 차이에 민감합니다"],
    ["<b>주의</b>", "범주가 많으면 검정력이 나뉩니다", "결과를 본 뒤 나온 제안이라, 채택하면 그 사실을 보고서에 밝혀야 합니다"],
    ["<b>문서</b>", "<code>Result/2026-11-18_분석계획서_A안_범주상호작용.md</code>", "<code>Result/2026-11-18_분석계획서_B안_경향성.md</code>"],
], widths=[12, 44, 44]))

A("<h2>8. 남은 일</h2><h3>회의에서 정할 것</h3>")
A(table(["순서", "항목", "지금 상황"], [
    ["1", "<b>주분석 음주 기준</b>", "S1–S5 후보. 결과를 이미 보았으므로 지침 환산, 칸 인원, 문헌처럼 p와 무관한 근거로 정하고 기록합니다."],
    ["2", "<b>주 검정 형태</b>", "A안 또는 B안"],
    ["3", "1잔의 알코올 g과 절단점 단위", "현재 7 g. 8 g도 검토. 주당 잔과 g/일 중 어느 단위를 고정할지 함께 정합니다."],
    ["4", "흡연 범주", "계획서는 2범주, 보정변수 설정안은 3범주를 권합니다."],
    ["5", "민감도 분석 최종 목록", "Model 3, 다른 기준, 1잔 g, 대푯값, 통풍 기준, 폭음 등"],
    ["6", "세부 정의", "혈청 요산 '&lt;1.0' 표기, eGFR 식(2009 또는 2021), 임신·수유 여성(4명)"],
], widths=[8, 30, 62]))
A("""<h3>조원이 처리할 것</h3>
<ul>
<li>계획서가 근거로 든 <code>연구정의_확정.md</code>와 <code>음주량 알코올 g 기준 설정 과정.md</code>가 저장소에 없습니다. 원본을 가진 조원이 올려야 합니다.</li>
<li>문서의 빈칸 이름 "( )"과 회의 계획의 "Hong 2016" 표기를 작성자가 확인합니다.</li>
<li>재현성 점검을 맡을 조원을 정합니다.</li>
</ul>
<h3>앞으로의 일정 (README 7장)</h3>""")
A(table(["기간", "단계", "할 일"], [
    ["11/16 – 11/18", "결과표·그림", "대상자 특성표, 음주–고요산혈증 표, 남녀 비교표, 흐름도, OR 그림"],
    ["11/23 – 11/25", "보고서 초안", "결과 해석 토론, 서론·방법 초안, 결과·고찰 초안, 한계 정리"],
    ["11/30 – 12/2", "최종 완성", "수치·참고문헌 최종 확인, 발표 자료, 리허설"],
    ["12/6", "완성", "보고서 완성"],
], widths=[20, 18, 62]))

A("<h2>9. 저장소 지도: 무엇이 어디에 있나</h2>")
A(table(["위치", "파일 수", "내용", "먼저 볼 것"], [
    ["맨 위 폴더", "6", "기준 문서, 소개, 재현성 절차, 발표 자료", "<code>분석계획서.md</code>, <code>README.md</code>"],
    ["<code>Result/</code>", "21", "조원이 정리한 결과와 결정 자료", "<code>Result/README.md</code>의 파일 목록"],
    ["<code>code/</code>", "39", "분석 코드. 최종은 <code>final_analysis.R</code>", "<code>code/README.md</code>"],
    ["<code>output/</code>", "270", "코드가 만든 표·그림. 대부분 이전 설계의 기록", "<code>output/README.md</code>"],
    ["<code>reference/</code>", "179", "논문 원문과 요약 101편(음주 42 · 홍조 35 · 보정변수 24)", "<code>reference/논문 목록.md</code>"],
    ["<code>docs/</code>", "22", "국건영 이용지침서(4–9기), 영양조사 코드자료집", "제8기 지침서"],
    ["<code>meeting/</code>", "1", "11/16–11/18 작업 계획", ""],
    ["<code>.claude/</code>", "80", "AI 스킬: 국건영 분석 절차, 기준별 p값 도구", "<code>knhanes_ptest/SKILL.md</code>"],
], widths=[17, 8, 45, 30]))
A('<p class="src">파일 수는 커밋 915ee92 기준으로 Git이 관리하는 파일입니다. 원시자료(data/)는 저장소에 올리지 않습니다.</p>')
A("""<h3>처음 보는 사람이 읽을 순서</h3>
<ol>
<li>이 문서</li>
<li><code>README.md</code> 1–4장: 연구 배경과 방법</li>
<li><code>분석계획서.md</code>: 분석 규칙의 기준</li>
<li><code>Result/2026-11-18_ptest_전체표_조건과p.xlsx</code>의 "M2 한눈에" 시트: 기준별 p값</li>
<li><code>Result/2026-11-18_문서점검_결과.md</code>: 고친 것과 남은 것</li>
</ol>""")

A("<h2>10. 핵심 참고문헌</h2><p>저장소의 논문 요약 101편 가운데, 이 연구의 설계에 직접 쓰인 논문입니다.</p>")
A(table(["논문", "이 연구에서의 쓰임"], [
    ["김어진 외, 2015. Korean J Health Promot", "음주 홍조에 따른 혈중 요산과 호모시스테인의 관계. 연구 주제의 직접 선행연구"],
    ["Kang & Oh, 2023. Nutrients", "KNHANES 2019–2021에서 홍조에 따른 음주와 대사이상 지방간. 같은 자료의 분석 사례"],
    ["Jeon 외, 2022. Sci Rep", "홍조와 우울. 1잔 7 g, 5·15 g/일 구분의 근거"],
    ["Lee 외, 2019. Korean J Fam Med", "홍조 기반 한국인 적정 음주 기준. 후보 S2의 근거"],
    ["Lee 외, 2023. Korean J Fam Med", "1회 음주량 대푯값(1.5·3.5·5.5·8잔)의 근거"],
    ["Kwon 외, 2023. Healthcare", "빈도 대푯값(월 0.5·1·3회)이 같은 KNHANES 논문. 문헌값 시나리오의 근거"],
    ["Jee 외, 2019. Int J Rheum Dis", "멘델 무작위 분석으로 본 음주와 고요산혈증의 인과"],
    ["Sakiyama 외, 2016. Sci Rep", "ALDH2 rs671과 통풍 감수성. 홍조의 유전적 바탕"],
    ["Wood 외, 2018. Lancet", "83개 전향 연구 현재 음주자 599,912명. 100 g/주당 연속형 단위의 근거"],
    ["VanderWeele, 2019. Eur J Epidemiol", "교란변수 선정 원칙. 보정 묶음 구성의 근거"],
], widths=[38, 62]))
A('<p class="src">논문별 요약: reference/*/summaries/, 한 줄 정리: Result/2026-11-18_논문별_요약_한눈표.md</p>')

A("<h2>11. 지금까지의 경과</h2>")
A(table(["시기", "한 일"], [
    ["11/2", "연구계획 발표. 처음 설계는 현재 홍조 2군과 남녀 공통 기준(주 3.5·14잔)"],
    ["11/16 이전", "V3 설계: 홍조 이력 3군, 대상자 10,779명과 변수 사전 정리. 절단점 탐색"],
    ["11/16", "<b>연구 질문 개정</b>(주질문 = 지속 홍조 vs 비홍조), 폐경 보정 추가, 남녀별 음주 기준 도입, <code>/knhanes_ptest</code> 도구 제작"],
    ["11/16–18", "음주 기준 16개 × 보정 묶음 6개 × 결과 2개 실행과 전체 표 정리"],
    ["11/18", "<code>분석계획서.md</code>로 규칙 통합, 최종 코드 임시본, 공식 통계 6개 일치, 칸 인원·폭음·대푯값 점검, 결과표 빈 틀"],
    ["11/18", "저장소 문서 점검 29개 항목 중 27개 수정, 주 검정 A·B안 작성, 폴더 안내와 전체 요약 작성"],
], widths=[14, 86]))

A("<h2>12. 용어 풀이</h2>")
A(table(["용어", "뜻"], [
    ["고요산혈증", "혈액 속 요산이 높은 상태로 통풍의 원인입니다. 이 연구의 기준은 남 7.0, 여 6.0 mg/dL 이상입니다."],
    ["효과수정(교호작용)", "한 요인(음주)의 효과가 다른 요인(홍조)에 따라 달라지는 것입니다."],
    ["복합표본설계", "조사구와 가구를 뽑아 조사한 자료이므로, 가중치·층·집락을 반영해야 전국 값을 바르게 추정합니다."],
    ["OR(교차비)", "기준군에 비해 결과의 오즈가 몇 배인지 나타냅니다. 1보다 크면 위험이 큰 쪽입니다."],
    ["p값", "실제로 차이가 없을 때 이만큼 또는 이보다 큰 차이가 우연히 나올 확률입니다. 여러 번 검정하면 우연히 작은 값이 나오기 쉽습니다."],
    ["경향성 검정", "음주 수준이 오를수록 차이가 한 방향으로 커지는지 하나의 기울기로 보는 검정입니다."],
    ["매개변수", "노출과 결과 사이의 중간 경로에 있는 변수입니다. 보정하면 노출의 효과 일부를 지울 수 있습니다."],
], widths=[22, 78]))

doc = f'<!doctype html><html lang="ko"><head><meta charset="utf-8"><title>6조 저장소 전체 정리</title><style>{CSS}</style></head><body>{"".join(html)}</body></html>'

browsers = [r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe", r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
            r"C:\Program Files\Google\Chrome\Application\chrome.exe"]
exe = next((b for b in browsers if Path(b).exists()), None)
if not exe:
    sys.exit("Edge 또는 Chrome을 찾지 못했습니다.")
tmp = Path(tempfile.gettempdir())
h = tmp / "knhanes_repo_overview.html"
h.write_text(doc, encoding="utf-8")
before = OUT.stat().st_mtime if OUT.exists() else 0
subprocess.run([exe, "--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
                f"--user-data-dir={tmp / 'edge_pdf_profile'}", "--no-pdf-header-footer",
                f"--print-to-pdf={OUT}", h.as_uri()], check=False, timeout=180, capture_output=True)
for _ in range(60):                                     # Edge가 끝난 뒤 파일이 늦게 생길 수 있어 잠시 기다린다
    if OUT.exists() and OUT.stat().st_mtime > before and OUT.stat().st_size > 0:
        break
    time.sleep(1)
else:
    sys.exit("PDF가 만들어지지 않았습니다.")
print("저장:", OUT, f"({OUT.stat().st_size / 1024:.0f} KB)")
