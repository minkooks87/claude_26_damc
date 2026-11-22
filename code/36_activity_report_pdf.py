# 36_activity_report_pdf.py
# 목적: GitHub 저장소의 모든 활동(커밋 기록)과 현재 연구 상태를 사람이 읽기 쉬운 PDF 한 권으로 정리한다.
# 입력: Git 기록(git log), Result/2026-11-18_ptest_전체표_조건과p.csv(주질문·부질문 1, 범주별 검정),
#       output/21_ptest/부질문2_3_전체기준.csv(부질문 2·3, 경향성 검정). 나머지 수치의 출처는 본문에 적었다.
# 출력: Result/2026-11-22_6조_활동_전체정리.pdf
# 실행: python code/36_activity_report_pdf.py   (프로젝트 폴더에서. Git과 Edge 또는 Chrome 필요)

import collections
import csv
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import markdown   # 부록(README·분석계획서·점검 결과)을 Markdown에서 HTML로 바꾼다

sys.stdout.reconfigure(encoding="utf-8")
OUT = Path("Result/2026-11-22_6조_활동_전체정리.pdf").resolve()
GIT = r"C:\Program Files\Git\cmd\git.exe"

# ---------------- 자료 읽기 ----------------
def git(*a):
    return subprocess.run([GIT, "-c", "core.quotepath=false", *a], capture_output=True, text=True, encoding="utf-8").stdout


log = [l.split("|", 3) for l in git("log", "--reverse", "--format=%h|%ad|%an|%s", "--date=format:%m/%d").splitlines()]
n_commit = len(log)
n_merge = int(git("rev-list", "--merges", "--count", "HEAD").strip())
head = git("rev-parse", "--short", "HEAD").strip()
by_author = collections.Counter(git("log", "--no-merges", "--format=%an").splitlines())   # 병합 커밋 제외
by_day = collections.Counter(d for _, d, _, _ in log)
files = git("ls-files").splitlines()
n_files = len(files)
local = [l for l in git("status", "--short").splitlines() if l.strip()]

g_rows = list(csv.DictReader(open("Result/2026-11-18_ptest_전체표_조건과p.csv", encoding="utf-8-sig")))
G = {(r["기준"][:3], r["결과변수"]): r for r in g_rows if r["단계"] == "G" and r["보정묶음"] == "M2"}
t_rows = list(csv.DictReader(open("output/21_ptest/부질문2_3_전체기준.csv", encoding="utf-8-sig")))
T = {(r["기준"][1:3], r["결과"], r["질문"], r["성별"]): r for r in t_rows if r["보정"] == "M2"}
HUA, UA = "고요산혈증(예/아니오)", "혈청 요산(mg/dL, 연속형)"
CRIT = [("01", "S1 성별 삼분위", True), ("02", "S2 절주지침 남 16 / 여 8", True), ("03", "S3 남 5·15 / 여 2.5·7.5", True),
        ("04", "S4 계획 발표 3.5·14", True), ("05", "S5 연속형 10 g/일당", True), ("06", "Chung 2024 1·30", False),
        ("07", "Jeon 2022 5·15", False), ("08", "Park 2022 7·14·28", False), ("09", "Yun 2025 10·20·30", False),
        ("10", "Inan-Eroglu 2022 성별 사분위", False), ("11", "Wood 2018 100 g/주당", False), ("12", "제안 1 15·30 3군", False),
        ("13", "제안 2 제한 삼차 스플라인", False), ("14", "제안 3 30 g/일 이상 2군", False), ("15", "WHO 남 25·60 / 여 15·40", False),
        ("16", "제안 5 5·15·30 4군", False)]


def pcell(raw):
    raw = (raw or "").strip()
    if not raw:
        return '<td class="p"></td>'
    if raw in ("무효", "해당 없음"):
        return f'<td class="p na">{"–" if raw == "해당 없음" else "무효"}</td>'
    show = raw.replace("*", "")
    if "†" in raw:
        return f'<td class="p dag">{show}</td>'
    v = float(show.replace("<", ""))
    return f'<td class="p {"sig" if v < 0.05 else "near" if v < 0.10 else ""}">{show}</td>'


def result_table(outcome):
    tkey = "고요산혈증(예/아니오)" if outcome == HUA else "혈청 요산(mg/dL, 연속형)"
    body = []
    for num, name, cand in CRIT:
        g = G[("G" + num, outcome)]
        cells = [pcell(g[c]) for c in ("주 남", "주 여", "부1 남", "부1 여")]
        cells += [pcell(T[(num, tkey, q, s)]["p"]) for q in ("부질문 2", "부질문 3") for s in ("남", "여")]
        body.append(f'<tr{" class=cand" if cand else ""}><td class="c">G{num}</td><td>{name}</td>{"".join(cells)}</tr>')
    return ('<table class="ptab"><thead><tr><th rowspan="2">번호</th><th rowspan="2">음주 기준</th>'
            '<th colspan="4" class="grp">범주별 검정</th><th colspan="4" class="grp">경향성 검정(1 자유도)</th></tr>'
            '<tr><th>주질문<br>남</th><th>주질문<br>여</th><th>부질문 1<br>남</th><th>부질문 1<br>여</th>'
            '<th>부질문 2<br>남</th><th>부질문 2<br>여</th><th>부질문 3<br>남</th><th>부질문 3<br>여</th></tr></thead>'
            f'<tbody>{"".join(body)}</tbody></table>')


def count_sig(outcome):
    """M2에서 해석 가능한(† 없음, 무효·해당 없음 아님) 검정 중 p<0.05 개수. G11은 G05와 같은 모형이라 뺀다."""
    tkey = "고요산혈증(예/아니오)" if outcome == HUA else "혈청 요산(mg/dL, 연속형)"
    res = []
    for lab, col_or_q, src in [("주질문", "주", "G"), ("부질문 1", "부1", "G"), ("부질문 2", "부질문 2", "T"), ("부질문 3", "부질문 3", "T")]:
        for s in ("남", "여"):
            vals = []
            for num, _, _ in CRIT:
                if num == "11":
                    continue
                raw = G[("G" + num, outcome)][f"{col_or_q} {s}"] if src == "G" else T[(num, tkey, col_or_q, s)]["p"]
                raw = (raw or "").strip()
                if raw and "†" not in raw and raw not in ("무효", "해당 없음"):
                    vals.append(float(raw.replace("*", "").replace("<", "")))
            res.append((lab, s, sum(v < 0.05 for v in vals), len(vals)))
    return res


def table(head, body, widths=None, cls=""):
    cg = "<colgroup>" + "".join(f'<col style="width:{w}%">' for w in widths) + "</colgroup>" if widths else ""
    return (f'<table class="{cls}">{cg}<thead><tr>' + "".join(f"<th>{h}</th>" for h in head) + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{c}</td>" for c in r) + "</tr>" for r in body) + "</tbody></table>")


def bar_svg(counter, width=640, unit="개"):
    items = counter.most_common()
    mx = max(v for _, v in items)
    h = 24 * len(items) + 8
    lab_w = 150
    out = [f'<svg viewBox="0 0 {width} {h}" width="100%" role="img" xmlns="http://www.w3.org/2000/svg" style="font-family:Malgun Gothic;font-size:11px">']
    for i, (k, v) in enumerate(items):
        y = 6 + i * 24
        w = (width - lab_w - 60) * v / mx
        out.append(f'<text x="{lab_w - 8}" y="{y + 14}" text-anchor="end" fill="#374151">{k}</text>')
        out.append(f'<rect x="{lab_w}" y="{y + 2}" width="{w:.1f}" height="16" rx="2" fill="#2f5597"/>')
        out.append(f'<text x="{lab_w + w + 6:.1f}" y="{y + 14}" fill="#111827">{v}{unit}</text>')
    out.append("</svg>")
    return "".join(out)


# ---------------- 스타일 ----------------
CSS = """
@page { size: A4; margin: 19mm 17mm 18mm 17mm;
  @bottom-center { content: counter(page) " / " counter(pages); font-family: 'Malgun Gothic'; font-size: 8.5pt; color: #6b7280; }
  @top-right { content: "6조 활동 전체 정리 · 2026-11-22"; font-family: 'Malgun Gothic'; font-size: 8pt; color: #9ca3af; } }
@page :first { @top-right { content: ""; } @bottom-center { content: ""; } }
* { box-sizing: border-box; }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { font-family: 'Malgun Gothic', sans-serif; font-size: 10pt; line-height: 1.65; color: #1f2937; margin: 0; word-break: keep-all; overflow-wrap: anywhere; }
.cover { height: 248mm; display: flex; flex-direction: column; justify-content: space-between; }
.cover .top { border-top: 6px solid #1F3864; padding-top: 18mm; }
.cover .kicker { font-size: 11pt; color: #2f5597; font-weight: 700; letter-spacing: 0.02em; }
.cover h1 { font-size: 27pt; line-height: 1.3; margin: 10px 0 14px; color: #111827; }
.cover .sub { font-size: 12.5pt; color: #374151; line-height: 1.6; }
.cover .facts { display: grid; grid-template-columns: repeat(4, 1fr); gap: 8px; margin-top: 18mm; }
.cover .fact { background: #f1f5fb; border-radius: 6px; padding: 12px 12px 10px; }
.cover .fact b { display: block; font-size: 18pt; color: #1F3864; line-height: 1.2; }
.cover .fact span { font-size: 8.8pt; color: #4b5563; }
.cover .meta { font-size: 9pt; color: #4b5563; line-height: 1.7; border-top: 1px solid #d1d5db; padding-top: 10px; }
.toc { break-before: page; }
.toc > ol { list-style: none; padding: 0; margin: 10px 0 0; counter-reset: s; }
.toc > ol > li { counter-increment: s; padding: 6px 0; border-bottom: 1px dotted #c7d2e5; font-size: 11pt; }
.toc > ol > li::before { content: counter(s) ". "; color: #2f5597; font-weight: 700; }
.toc > ol.toc-app > li { counter-increment: none; }
.toc > ol.toc-app > li::before { content: none; }
.toc em.pg { float: right; font-style: normal; font-weight: 700; color: #2f5597; font-size: 10.5pt; }
.read em.pg2 { font-style: normal; color: #2f5597; font-size: 8.8pt; margin-right: 4px; }
.toc > ol > li span { display: block; font-size: 9pt; color: #6b7280; margin-left: 20px; }
h2 { font-size: 15pt; color: #1F3864; margin: 0 0 10px; padding: 0 0 5px; border-bottom: 1.5px solid #c7d2e5; break-after: avoid; }
section { break-before: page; }
h3 { font-size: 11pt; color: #111827; margin: 16px 0 6px; break-after: avoid; }
h3::before { content: ""; display: inline-block; width: 4px; height: 11pt; background: #2f5597; margin-right: 7px; vertical-align: -1px; border-radius: 1px; }
p { margin: 5px 0 8px; }
ul, ol { margin: 4px 0 9px; padding-left: 19px; }
li { margin: 3px 0; }
b { color: #111827; }
code { font-family: Consolas, 'Malgun Gothic', monospace; font-size: 8.6pt; background: #f1f4f9; padding: 0 3px; border-radius: 2px; }
table { width: 100%; border-collapse: collapse; margin: 7px 0 11px; font-size: 8.9pt; line-height: 1.5; break-inside: avoid; }
thead { display: table-header-group; }
tr { break-inside: avoid; }
th { background: #1F3864; color: #fff; font-weight: 700; padding: 5px 6px; border: 0.6px solid #1F3864; text-align: left; vertical-align: middle; }
td { padding: 4.5px 6px; border: 0.6px solid #d5dce8; vertical-align: top; }
tbody tr:nth-child(even) td { background: #f8fafc; }
.key { background: #f1f5fb; border-left: 4px solid #1F3864; padding: 10px 14px; margin: 10px 0 12px; break-inside: avoid; }
.key p:first-child { margin-top: 0; } .key ul { margin-bottom: 0; }
.warn { background: #fff8e6; border-left: 4px solid #b7791f; padding: 8px 12px; margin: 9px 0 11px; font-size: 9.4pt; break-inside: avoid; }
.new { background: #ecfdf3; border-left: 4px solid #1a7f45; padding: 8px 12px; margin: 9px 0 11px; font-size: 9.4pt; break-inside: avoid; }
.src { font-size: 8.1pt; color: #6b7280; margin: -5px 0 10px; }
.ptab { font-size: 7.9pt; }
.ptab th { text-align: center; padding: 3px 2px; font-size: 7.6pt; line-height: 1.25; }
.ptab th.grp { background: #2f5597; }
.ptab td { padding: 3px 4px; vertical-align: middle; }
.ptab td.c { text-align: center; color: #4b5563; }
.ptab td.p { text-align: center; font-variant-numeric: tabular-nums; width: 7.6%; padding: 3px 1px; }
.ptab tr.cand td:nth-child(-n+2) { background: #e3ebf7 !important; font-weight: 700; }
.sig { background: #c6efce !important; color: #00590f; font-weight: 700; }
.near { background: #fff2cc !important; }
.dag { background: #ececec !important; color: #808080; font-style: italic; }
.na { color: #9ca3af; }
.legend { font-size: 8.1pt; color: #4b5563; margin: 2px 0 12px; }
.legend span { padding: 0 5px; margin-right: 3px; }
.grid td { text-align: center; vertical-align: middle; padding: 7px 6px; }
.grid td:first-child { background: #eef2f8 !important; font-weight: 700; }
.grid .main { background: #dce6f5 !important; font-weight: 700; }
.grid .out { background: #f3f4f6 !important; color: #6b7280; }
.flow td:first-child { text-align: center; }
.flow td:nth-child(3), .flow td:nth-child(4) { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
.flow tr:last-child td { background: #dce6f5 !important; font-weight: 700; }
.qbox { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin: 8px 0 12px; break-inside: avoid; }
.q { border: 1px solid #c7d2e5; border-radius: 6px; padding: 9px 11px; }
.q .tag { display: inline-block; font-size: 8.3pt; font-weight: 700; color: #fff; background: #1F3864; border-radius: 3px; padding: 0 6px; margin-bottom: 4px; }
.q.trend .tag { background: #1a7f45; }
.q p { margin: 2px 0 0; font-size: 9.4pt; }
.q .how { font-size: 8.3pt; color: #6b7280; }
.tl td:first-child { white-space: nowrap; font-weight: 700; color: #1F3864; width: 13%; }
.tl td:nth-child(2) { width: 22%; }
.who { font-size: 8pt; color: #4b5563; }
th .who { color: #c7d2e5; }
section.flow-on { break-before: auto; margin-top: 22px; }
.chart { break-inside: avoid; margin: 6px 0 4px; }
/* 부록: 저장소 문서를 그대로 옮긴 부분 */
.app-head { background: #1F3864; color: #fff; border-radius: 6px; padding: 14px 16px; margin: 0 0 14px; break-after: avoid; }
.app-head .lab { font-size: 9.5pt; letter-spacing: 0.03em; color: #c7d2e5; font-weight: 700; }
.app-head h2 { color: #fff; border: 0; margin: 2px 0 4px; padding: 0; font-size: 16pt; }
.app-head p { margin: 0; font-size: 9pt; color: #dbe4f3; }
.app h3 { font-size: 12.5pt; color: #1F3864; margin: 20px 0 7px; padding-bottom: 3px; border-bottom: 1px solid #dbe4f3; break-after: avoid; }
.app h3::before { display: none; }
.app h4 { font-size: 10.6pt; color: #111827; margin: 14px 0 5px; break-after: avoid; }
.app h4::before { content: ""; display: inline-block; width: 4px; height: 10.5pt; background: #2f5597; margin-right: 7px; vertical-align: -1px; border-radius: 1px; }
.app h5 { font-size: 10pt; margin: 12px 0 4px; break-after: avoid; }
.app table { break-inside: auto; font-size: 8.6pt; }
.app blockquote { margin: 8px 0 10px; padding: 7px 12px; background: #f5f7fb; border-left: 4px solid #9fb3d6; color: #374151; break-inside: avoid; }
.app blockquote p { margin: 2px 0; }
.app pre { background: #f5f7fb; border: 1px solid #dbe4f3; border-radius: 4px; padding: 8px 10px; font-size: 8.4pt; line-height: 1.5; white-space: pre-wrap; break-inside: avoid; }
.app pre code { background: none; padding: 0; }
.app hr { border: 0; border-top: 1px dashed #c7d2e5; margin: 14px 0; }
.app a { color: #1F3864; text-decoration: none; }
.flowfig { display: flex; flex-wrap: wrap; align-items: center; justify-content: center; gap: 6px; margin: 10px 0 12px; break-inside: avoid; font-size: 9pt; }
.flowfig .n { border: 1.5px solid #2f5597; border-radius: 6px; padding: 8px 12px; text-align: center; background: #fff; }
.flowfig .n.hot { border-color: #c0504d; background: #f8e3e0; color: #9b2c2c; font-weight: 700; }
.flowfig .n.final { background: #1F3864; border-color: #1F3864; color: #fff; font-weight: 700; }
.flowfig .e { text-align: center; color: #4b5563; font-size: 8pt; line-height: 1.3; }
.flowfig .e::after { content: "→"; display: block; font-size: 16pt; color: #2f5597; line-height: 1; }
.read { counter-reset: r; list-style: none; padding: 0; }
.read li { counter-increment: r; display: flex; gap: 10px; align-items: baseline; padding: 6px 0; border-bottom: 1px dotted #c7d2e5; }
.read li::before { content: counter(r); flex: 0 0 22px; height: 22px; border-radius: 11px; background: #1F3864; color: #fff; font-weight: 700; text-align: center; line-height: 22px; font-size: 9pt; }
.read li b { min-width: 0; }
"""

# ---------------- 본문 ----------------
H = []
A = H.append
top_author = by_author.most_common()

A(f"""<div class="cover"><div class="top">
<div class="kicker">과학적 사고와 의학연구 6조 · 활동 전체 정리</div>
<h1>음주 관련 안면홍조 이력에 따른<br>음주량–고요산혈증 연관성의 차이</h1>
<div class="sub">국민건강영양조사 2019–2021 연구의<br>계획, GitHub 활동, 분석 결과, 남은 일을 한 권으로 정리했습니다.</div>
<div class="facts">
<div class="fact"><b>10,779명</b><span>최종 분석 대상<br>(남 5,411 · 여 5,368)</span></div>
<div class="fact"><b>{n_commit}회</b><span>GitHub 커밋<br>(병합 {n_merge}회 포함)</span></div>
<div class="fact"><b>16개</b><span>시도한 음주 기준<br>(보정 묶음 6개)</span></div>
<div class="fact"><b>101편</b><span>요약한 논문<br>(음주·홍조·보정변수)</span></div>
</div></div>
<div class="meta">작성일 2026년 11월 22일 · 저장소 minkooks87/claude_26_damc (기준 커밋 {head})<br>
조원: 김민상 · 조용찬 · 이예은 · 박채완 · 김정윤 · 김준현 · 지도교수: 손민국<br>
저장소의 커밋 기록, 문서, 코드, 결과를 생성형 AI(Claude)로 읽고 정리했습니다. 조원 검토가 필요합니다.<br>
분석 규칙의 기준 문서는 <code>분석계획서.md</code>이며, 이 문서와 다르면 계획서를 따릅니다.</div></div>""")

A("""<div class="toc"><h2>목차</h2><ol>
<li>한눈에 보기<em class="pg">{{P:1}}</em><span>지금 어디까지 왔고, 무엇이 남았나</span></li>
<li>연구 질문<em class="pg">{{P:2}}</em><span>11/22 개정: 주질문·부질문 1(범주별)과 부질문 2·3(경향성)</span></li>
<li>자료와 분석 방법<em class="pg">{{P:3}}</em><span>대상자 선정, 홍조 이력, 음주량 계산, 음주 기준 후보, 보정변수</span></li>
<li>활동 기록<em class="pg">{{P:4}}</em><span>날짜별로 본 GitHub 활동과 계정별 기여</span></li>
<li>분야별 산출물<em class="pg">{{P:5}}</em><span>문헌, 보정변수, 음주 기준, 분석 도구, 점검, 최종 코드</span></li>
<li>지금까지의 결과<em class="pg">{{P:6}}</em><span>음주 기준 16개의 질문별 p값(탐색 단계)</span></li>
<li>남은 일과 일정<em class="pg">{{P:7}}</em><span>회의에서 정할 것, 조원이 처리할 것</span></li>
<li>저장소 지도와 용어<em class="pg">{{P:8}}</em><span>어디에 무엇이 있나, 처음 보는 사람이 읽을 순서</span></li>
</ol>
<ol class="toc-app" style="list-style:none;padding:0;margin:6px 0 0">
<li style="padding:6px 0;border-bottom:1px dotted #c7d2e5;font-size:11pt"><b style="color:#2f5597">부록 A.</b> README 1–4장<em class="pg">{{P:A}}</em><span style="display:block;font-size:9pt;color:#6b7280;margin-left:20px">연구 배경, 선행연구, 연구 질문과 가설, 연구 방법</span></li>
<li style="padding:6px 0;border-bottom:1px dotted #c7d2e5;font-size:11pt"><b style="color:#2f5597">부록 B.</b> 분석계획서 전문<em class="pg">{{P:B}}</em><span style="display:block;font-size:9pt;color:#6b7280;margin-left:20px">분석 규칙의 기준 문서(변수 정의, 지침서 쪽수, 모형, 민감도, 검증, 변경 기록)</span></li>
<li style="padding:6px 0;border-bottom:1px dotted #c7d2e5;font-size:11pt"><b style="color:#2f5597">부록 C.</b> 저장소 문서 점검 결과<em class="pg">{{P:C}}</em><span style="display:block;font-size:9pt;color:#6b7280;margin-left:20px">점검 29개 항목과 처리 상태</span></li>
</ol>
<div class="key" style="margin-top:6mm"><p><b>이 문서를 읽는 법</b></p><ul>
<li>처음 보는 사람이 읽어야 할 문서를 <b>모두 이 한 권에 넣었습니다</b>(부록 A–C). 읽는 순서는 8장에 있습니다.</li>
<li>바쁘면 1장과 2장만 읽어도 연구의 현재 상태를 알 수 있습니다.</li>
<li>6장의 p값은 <b>주분석 기준을 정하기 전의 탐색 결과</b>입니다. 확정 결과로 인용하지 마세요.</li>
<li>초록 칸은 p &lt; 0.05, 노란 칸은 0.05–0.10, 회색 칸(†)은 사례가 적어 해석하지 않는 값입니다.</li>
<li>초록 상자는 2026-11-22에 새로 한 작업입니다.</li>
</ul></div></div>""")

# 1. 한눈에 보기
A(f"""<section><h2>1. 한눈에 보기</h2>
<div class="key"><p><b>연구 질문을 한 문장으로</b></p>
<p>술을 마시면 얼굴이 빨개지는 사람(안면홍조)은, 음주량이 늘 때 고요산혈증 위험이 더 크게 늘어날까?</p></div>
<h3>지금 상태</h3>
<ul>
<li><b>준비 끝:</b> 대상자 정의(10,779명), 변수 사전, 분석계획서, 최종 분석 코드, 공식 통계 대조(6개 지표 모두 일치).</li>
<li><b>탐색 끝:</b> 음주 기준 16개 × 보정 묶음 6개 × 결과 2개(고요산혈증, 혈청 요산)의 질문별 p값을 모두 계산했습니다.</li>
<li><b>아직 안 함:</b> 주분석에 쓸 음주 기준이 정해지지 않아 <b>최종 주분석은 실행하지 않았습니다.</b></li>
</ul>
<h3>지금까지 보인 결과 (주 모형 M2, 탐색 단계)</h3>
<ul>
<li><b>주질문(지속 홍조 vs 비홍조, 범주별):</b> 16개 기준 모두 p ≥ 0.05. 뚜렷한 차이의 증거는 아직 없습니다.</li>
<li><b>부질문 2(주질문의 경향성):</b> 남자에서 기울기의 비가 모든 기준에서 1보다 컸지만(1.06–1.16), p &lt; 0.05는 한 기준뿐이었습니다.</li>
<li><b>부질문 3(소실 vs 비홍조의 경향성):</b> 남자에서 해석 가능한 10개 기준 중 6개가 p &lt; 0.05였습니다. 기울기의 비는 1.20–1.34입니다.</li>
<li><b>여자와 혈청 요산:</b> 사례가 적거나 신호가 없습니다.</li>
</ul>
<h3>다음에 정할 것</h3>
<ol><li><b>주분석 음주 기준</b>(후보 S1–S5)</li><li><b>11/22 질문 개정</b>을 조원·교수님과 공유하고 확정</li><li>1잔의 알코올 g과 절단점 단위</li><li>흡연 범주(2범주 또는 3범주)</li><li>민감도 분석 최종 목록</li></ol>
<div class="new"><b>11/22 새 작업:</b> 연구 질문을 개정했습니다. 성별 차이를 묻던 부질문 2를 삭제하고, 주질문·부질문 1의 경향성 검정을 부질문 2·3으로 추가했습니다. 그리고 부질문 2·3의 p를 16개 기준 모두로 구했습니다. 이 작업은 같은 날 GitHub에 올렸습니다(분석계획서·README·최종 코드·결과 파일).</div>
</section>""")

# 2. 연구 질문
A("""<section><h2>2. 연구 질문</h2>
<p>술을 마시면 생기는 아세트알데하이드를 잘 분해하지 못하는 사람은 소량만 마셔도 얼굴이 빨개집니다. 동아시아인의 36–45%가 경험합니다(Jeon 2022). 알코올은 요산을 높이므로, 홍조가 있는 사람에서 그 영향이 더 클 수 있습니다. 홍조는 시간이 지나며 사라지기도 해서, 이 연구는 <b>처음 술을 마시던 때와 지금의 홍조를 함께 본 "홍조 이력"</b>으로 사람들을 나눕니다.</p>
<h3>네 가지 질문 (2026-11-22 개정)</h3>
<div class="qbox">
<div class="q"><span class="tag">주질문</span><p>남녀 각각에서, 음주 수준과 고요산혈증의 관계는 <b>지속 홍조군과 비홍조군</b> 사이에 다른가?</p><p class="how">범주별 공동 검정 · 음주 수준마다 차이를 함께 봄</p></div>
<div class="q"><span class="tag">부질문 1</span><p>같은 관계가 현재 홍조가 없는 사람 가운데 <b>홍조 소실군과 비홍조군</b> 사이에 다른가?</p><p class="how">범주별 공동 검정</p></div>
<div class="q trend"><span class="tag">부질문 2</span><p>음주 수준이 높아질수록 위험이 커지는 <b>기울기가 지속 홍조군에서</b> 비홍조군보다 더 큰가?</p><p class="how">주질문의 경향성 · 1 자유도 검정 · 10 g/일당</p></div>
<div class="q trend"><span class="tag">부질문 3</span><p><b>홍조 소실군의 기울기</b>가 비홍조군과 다른가?</p><p class="how">부질문 1의 경향성 · 1 자유도 검정 · 10 g/일당</p></div>
</div>
<h3>질문이 바뀌어 온 과정</h3>""")
A(table(["시점", "주질문", "부질문"], [
    ["11/2 계획 발표", "현재 홍조군 vs 현재 비홍조군", "홍조 이력, 남녀 차이"],
    ["11/16 개정", "지속 홍조군 vs 비홍조군(남녀별 음주 기준)", "부질문 1 소실 vs 비홍조 · 부질문 2 남녀 차이"],
    ["11/18 검토", "같음. 검정 방식을 A안(범주별)과 B안(경향성) 중 고르자는 제안", "같음"],
    ["<b>11/22 개정</b>", "같음(범주별 공동 검정)", "<b>부질문 2 남녀 차이 삭제</b>, 부질문 2·3 = 주질문·부질문 1의 경향성"],
], widths=[17, 40, 43]))
A("""<div class="warn"><b>밝혀 둘 점:</b> 부질문 2·3은 여러 음주 기준의 결과를 본 뒤에 추가했습니다. 그래서 보고서의 방법과 한계에 그 사실을 적고, 범주별 검정과 경향성 검정의 p를 나란히 싣기로 했습니다(분석계획서 7장).</div>
</section>""")

# 3. 자료와 방법
A("""<section><h2>3. 자료와 분석 방법</h2>
<p>국민건강영양조사 제8기(2019·2020·2021) 기본DB를 씁니다. 복합표본설계를 반영해 가중치 <code>wt_itvex</code> × 1/3, 층 <code>kstrata</code>, 집락 <code>psu</code>로 추정합니다.</p>
<h3>대상자 선정</h3>""")
A(table(["단계", "기준", "남은 인원", "제외"], [
    ["0", "2019–2021 기본DB 전체", "22,559", ""], ["1", "만 19세 이상", "18,691", ""], ["2", "최근 1년 음주자", "12,231", "6,460"],
    ["3", "음주량 계산 가능, 과거·현재 홍조 응답, 혈청 요산 측정", "12,002", "229"],
    ["4", "통풍 의사진단 없음(진단자와 모름·무응답 제외)", "11,217", "785"],
    ["5", "보정변수 모두 응답 → <b>최종 대상(남 5,411 · 여 5,368)</b>", "10,779", "438"]], widths=[8, 66, 14, 12], cls="flow"))
A("""<p class="src">n은 가중치를 적용하지 않은 표본 수. 고요산혈증(남 7.0, 여 6.0 mg/dL 이상)은 남 1,201명(가중 23.6%), 여 427명(가중 7.7%). 출처: 분석계획서 3장, 4.3</p>
<h3>홍조 이력 (남 / 여, 명)</h3>
<table class="grid"><thead><tr><th></th><th style="text-align:center">지금 홍조 없음</th><th style="text-align:center">지금 홍조 있음(가끔·자주·늘)</th></tr></thead><tbody>
<tr><td>처음에 홍조 없음</td><td class="main">비홍조(기준군)<br>3,181 / 3,273</td><td class="out">신규 홍조(이력 분석에서 제외)<br>301 / 400</td></tr>
<tr><td>처음에 홍조 있음</td><td class="main">홍조 소실<br>341 / 236</td><td class="main">지속 홍조<br>1,588 / 1,459</td></tr></tbody></table>
<h3>음주량 계산</h3>
<p>주당 잔 수 = 주당 음주 횟수 × 1회 잔 수. 1잔을 7 g으로 보면 이 값이 그대로 g/일입니다.</p>""")
A(table(["문항", "응답 → 대푯값", "근거"], [
    ["음주 빈도", "월 1회 미만 0.5회/월 · 월 1회 · 월 2–4회 3회/월 · 주 2–3회 2.5회/주 · 주 4회 이상 5.5회/주", "아래 세 칸은 국내 문헌(Kwon 2023)과 같음. 위 두 칸은 범주 중간값 → 하한·상한 민감도 계획"],
    ["1회 음주량", "1–2잔 1.5 · 3–4잔 3.5 · 5–6잔 5.5 · 7–9잔 8 · 10잔 이상은 직접 적은 값", "Lee 2023과 같음"]], widths=[14, 50, 36]))
A("<h3>주분석 음주 기준 후보 (아직 정하지 않음)</h3>")
A(table(["후보", "남자 (g/일)", "여자 (g/일)", "근거"], [
    ["S1 성별 삼분위", "2.4 · 10", "0.4 · 2.5", "이 표본의 가중 분포(반올림값)"], ["S2 절주 지침", "16", "8", "Lee 2019 비홍조군 적정 상한을 1잔 7 g으로 환산"],
    ["S3 5·15, 여성 절반", "5 · 15", "2.5 · 7.5", "Jeon 2022, NIAAA 남녀 비율"], ["S4 계획 발표(참고)", "3.5 · 14", "3.5 · 14", "남녀 공통"],
    ["S5 연속형", "10 g/일당", "10 g/일당", "절단점 없음"]], widths=[22, 14, 14, 50]))
A("<h3>보정 묶음과 모형</h3>")
A(table(["묶음", "들어간 변수", "역할"], [
    ["Crude", "없음", "비교용"], ["<b>M2</b>", "연령, 교육, 소득, 조사연도, 흡연(2범주), 신체활동, 폐경 상태(여)", "<b>주 모형</b>"],
    ["M2S · M2A", "M2에서 흡연 3범주(M2S), 여기에 연령 10세 범주(M2A)", "민감도 후보"],
    ["M3 · M3S", "M2 + BMI, 고혈압, 당뇨병, eGFR (M3S는 흡연 3범주)", "민감도(매개변수 가능성)"]], widths=[14, 62, 24]))
A("""<ul>
<li>남녀 각각 복합표본 로지스틱 회귀(혈청 요산은 선형회귀)로 "음주 × 홍조 이력" 교호작용을 검정합니다.</li>
<li>부질문 2·3의 음주 점수는 성별마다 각 음주 범주의 g/일 중앙값 ÷ 10이며, 효과는 10 g/일당 기울기의 비로 나타냅니다.</li>
<li>고요산혈증 사례가 10명 미만인 칸이 있으면 그 결과는 해석하지 않습니다(†).</li>
</ul></section>""")

# 4. 활동 기록
A(f"""<section class="flow-on"><h2>4. 활동 기록</h2>
<p>공유 저장소는 2026년 11월 16일에 만들어졌습니다. 그 전에 한 분석(11/2 계획 발표 전후의 코드와 결과)은 첫 업로드에 함께 들어 있습니다. 지금까지 커밋은 <b>{n_commit}회</b>입니다(11/16에 {by_day.get('11/16', 0)}회, 11/18에 {by_day.get('11/18', 0)}회, 11/22에 {by_day.get('11/22', 0)}회). 그중 {n_merge}회는 Pull Request 병합입니다.</p>
""")
A("<h3>계정별 커밋 수 (병합 제외)</h3>")
A(f'<div class="chart">{bar_svg(by_author, unit="회")}</div>')
A('<p class="src">GitHub 계정 이름 그대로 적었습니다. 계정 이름이 실명과 다른 경우가 있습니다. "Claude"는 README 초안을 AI가 직접 커밋한 기록입니다.</p>')
A("<h3>날짜별 주요 활동</h3>")
A(table(["날짜", "분야", "한 일 <span class='who'>(GitHub 계정)</span>"], [
    ["11/2", "계획 발표", "연구계획 발표. 처음 설계는 현재 홍조 2군과 남녀 공통 기준(주 3.5·14잔)"],
    ["11/16 오전", "저장소 시작", "저장소 생성, 분석 코드·결과물·참고 문서 첫 업로드 <span class='who'>(minkooks87)</span>, README 연구계획 소개"],
    ["11/16 오전", "참고문헌", "논문 80편 원문·요약을 주제별 폴더로 정리 <span class='who'>(kikimin0214-kms)</span>, 음주량 선정 기준 폴더(분위수·연속형·고정구간) 정리 <span class='who'>(arahan90wany-gif)</span>, 보정변수 근거 문헌 <span class='who'>(jungyoon07284)</span>, VanderWeele 2019 보정변수 선정 원칙 <span class='who'>(yongchan211)</span>"],
    ["11/16 오후", "계획·규칙", "11/16–11/18 작업 계획과 역할 분담 <span class='who'>(yongchan211)</span>, Result 폴더 신설 <span class='who'>(yongchan211)</span>"],
    ["11/16 오후", "연구 질문", "<b>연구 질문 개정</b>: 주질문 = 지속 홍조 vs 비홍조, 남녀별 음주 기준. 폐경 상태를 보정변수에 항상 포함 <span class='who'>(team06donga)</span>"],
    ["11/16 오후", "분석 도구", "기준별 p값 도구 <code>/knhanes_ptest</code> 제작과 개선(스플라인, 경향성 검정, 일괄 실행) <span class='who'>(team06donga)</span>"],
    ["11/16 오후", "보정변수", "보정 결과표 PDF <span class='who'>(jungyoon07284)</span>, 보정변수 설정안과 검증 <span class='who'>(yongchan211)</span>"],
    ["11/16 저녁", "탐색 결과", "음주 기준 16개 × 보정 묶음 6개의 질문별 p 요약 <span class='who'>(team06donga)</span>"],
    ["11/18 오전", "보정변수 근거", "교육(4편), 소득, 조사연도(3편), 폐경(Hak & Choi 2008) 근거 문헌 추가 <span class='who'>(jungyoon07284, kikimin0214-kms)</span>"],
    ["11/18 오전", "기준 정하기", "폭음 문항 사용 여부, 절단점 경계 점검, 여성 칸 인원 점검 <span class='who'>(yeeun)</span>. 음주 대푯값 민감도 계획 <span class='who'>(team06donga)</span>"],
    ["11/18 오전", "결과표", "보고서용 결과표 빈 틀(표 0·1·2·3, 보조표) PDF·엑셀 <span class='who'>(jungyoon07284)</span>"],
    ["11/18 오전", "계획서·최종 코드", "<b>분석계획서</b>로 규칙 통합, 최종 분석 코드 임시본, 공식 통계 대조 6개 모두 일치, 재현성 확인 절차 <span class='who'>(team06donga)</span>"],
    ["11/18 오전", "정리", "ptest 실행 목록·전체 표·엑셀본, 논문 요약 11편과 100편 한눈 표 <span class='who'>(team06donga)</span>"],
    ["11/18 낮", "점검", "저장소 문서 점검 29개 항목 중 27개 수정, 폴더 안내, 전체 요약, 정리 PDF <span class='who'>(team06donga)</span>"],
    ["<b>11/22</b>", "<b>질문 개정</b>", "<b>부질문 2(남녀 차이) 삭제, 부질문 2·3 = 경향성 검정 추가.</b> 경향성 비교와 부질문 2·3의 16개 기준 p값 <span class='who'>(team06donga)</span>"],
], widths=[13, 17, 70], cls="tl"))
A("</section>")

# 5. 분야별 산출물
A("<section><h2>5. 분야별 산출물</h2>")
A(table(["분야", "만든 것", "어디에"], [
    ["<b>문헌</b>", "논문 101편 요약(음주 42 · 홍조 35 · 보정변수 24), 원문 PDF, 논문 목록, 한 줄 요약표", "<code>reference/</code>, <code>Result/…논문별_요약_한눈표.md</code>"],
    ["<b>보정변수</b>", "변수 10개의 근거 문헌, 보정 묶음 설정안(M2·M2S·M2A·M3), 폐경 결측 점검, 보정 결과표(이전 V3 기준)", "<code>Result/2026-11-16_보정변수_설정안_검증.md</code>"],
    ["<b>음주 기준</b>", "후보 16개의 질문별 p, 실행 목록, 248줄 전체 표와 엑셀본, 대푯값 하한·상한 민감도 계획", "<code>Result/…ptest_전체표_조건과p.xlsx</code>"],
    ["<b>결정 점검</b>", "여성 칸 인원(홍조 소실 여성은 236명뿐), 폭음 문항은 노출·보정에 쓰지 않고 민감도에만, 절단점 경계값", "<code>Result/2026-11-18_여성_칸_인원_점검.md</code> 등"],
    ["<b>분석 도구</b>", "<code>/knhanes_ptest</code>: 절단점·보정 묶음을 바꿔 가며 질문별 p를 몇 초 만에 계산", "<code>.claude/skills/knhanes_ptest/</code>"],
    ["<b>계획과 코드</b>", "분석계획서(기준 문서), 최종 분석 코드(주분석은 기준 확정 전까지 꺼 둠), 재현성 확인 절차", "<code>분석계획서.md</code>, <code>code/final_analysis.R</code>"],
    ["<b>검증</b>", "2019 월간음주율 57.7%·고위험음주율 12.1% 등 공식 통계 6개를 응답자 수까지 재현", "<code>output/final_analysis/06_official_check.csv</code>"],
    ["<b>보고서 준비</b>", "결과표 빈 틀(표 0·1·2·3), 문서 점검 결과, 폴더 안내, 전체 요약", "<code>Result/</code>, 각 폴더 <code>README.md</code>"],
], widths=[14, 54, 32]))
A("""<h3>결정을 돕기 위해 확인한 것</h3>""")
A(table(["주제", "확인한 내용", "제안"], [
    ["여성 칸 인원", "여성 고요산혈증은 7.7%라 한 칸에 약 130명이 있어야 예상 사례가 10명을 넘습니다. 홍조 소실 여성은 236명뿐입니다.", "여성 부질문 1·3은 탐색으로 둡니다. 여성은 2단계 기준이 안정적입니다."],
    ["폭음 문항", "주당 알코올량은 이미 빈도와 1회량으로 계산되므로, 폭음 문항을 더하면 같은 음주를 두 번 셉니다.", "노출·보정에는 쓰지 않고 기술 통계와 민감도 분석에 씁니다."],
    ["음주 대푯값", "대푯값을 하한으로 바꾸면 남자 30 g/일 이상 군이 870명에서 301명으로 줄어듭니다. 빈도 값의 영향이 큽니다.", "하한·상한과 문헌값 시나리오로 민감도 분석을 합니다."],
], widths=[15, 55, 30]))
A('<p class="src">출처: Result/ 각 문서(작성: yeeun, team06donga)</p></section>')

# 6. 결과
hs, us = count_sig(HUA), count_sig(UA)
A("""<section><h2>6. 지금까지의 결과 (탐색 단계)</h2>
<div class="warn"><b>확정 결과가 아닙니다.</b> 주분석 기준을 정하기 전에 16개 기준을 모두 돌려 본 결과입니다. 이 표를 보고 p가 작은 기준을 골라 주분석으로 삼으면 안 됩니다.</div>
<h3>고요산혈증, 주 모형 M2</h3>""")
A(result_table(HUA))
A('<div class="legend"><span class="sig">p &lt; 0.05</span><span class="near">0.05–0.10</span><span class="dag">† 사례 10명 미만(해석하지 않음)</span> – 해당 없음(스플라인) · 파란 줄: 계획서 후보 S1–S5 · G05와 G11은 같은 모형</div>')
A('<p class="src">2군 기준(G02, G14)은 경향성 검정이 범주별 검정과 같고, 연속형(G05, G11)은 처음부터 1 자유도라 두 쪽 값이 같습니다. 출처: Result/2026-11-18_ptest_전체표_조건과p.csv, output/21_ptest/부질문2_3_전체기준.csv</p>')
A("<h3>혈청 요산(연속형), 주 모형 M2</h3>")
A(result_table(UA))
A("<h3>p &lt; 0.05 개수 (M2, 해석 가능한 기준 중)</h3>")
body = []
for (lab, s, k, n), (_, _, k2, n2) in zip(hs, us):
    body.append([lab, s, f"{k} / {n}", f"{k2} / {n2}"])
A(table(["질문", "성별", "고요산혈증", "혈청 요산"], body, widths=[30, 14, 28, 28]))
A("""<h3>어떻게 읽나</h3><ul>
<li><b>범주별로 보면(주질문·부질문 1)</b> 지속 홍조군과 비홍조군의 차이는 뚜렷하지 않습니다.</li>
<li><b>경향성으로 보면(부질문 2·3)</b> 남자에서 "음주량이 늘수록 홍조 이력이 있는 사람의 위험이 조금 더 가파르게 오른다"는 한 방향의 신호가 있습니다. 소실군(부질문 3)에서 더 일관되고, 지속 홍조군(부질문 2)은 약합니다.</li>
<li><b>보정을 늘리면(M3)</b> 경향성 p가 더 작아집니다(부질문 2 남자 13개 중 7개, 부질문 3 남자 10개 중 10개가 p &lt; 0.05).</li>
<li><b>여자와 혈청 요산</b>에서는 같은 신호가 보이지 않았습니다. 홍조 소실군은 남자도 341명이라 칸이 작습니다.</li>
<li><b>주의:</b> 기준 16개 × 보정 묶음 6개 × 결과 2개를 함께 시도했으므로, p &lt; 0.05 일부는 우연일 수 있습니다.</li>
</ul></section>""")

# 7. 남은 일
A("<section><h2>7. 남은 일과 일정</h2><h3>회의에서 정할 것</h3>")
A(table(["순서", "항목", "지금 상황"], [
    ["1", "<b>주분석 음주 기준</b>", "S1–S5 후보. 결과를 이미 보았으므로 지침 환산, 칸 인원, 문헌처럼 p와 무관한 근거로 정하고 기록합니다."],
    ["2", "<b>11/22 질문 개정 공유</b>", "부질문 2 삭제와 부질문 2·3 추가를 조원과 교수님께 공유하고 확정합니다."],
    ["3", "1잔의 알코올 g과 절단점 단위", "현재 7 g. 8 g도 검토. 주당 잔과 g/일 중 어느 단위를 고정할지 함께 정합니다."],
    ["4", "흡연 범주", "계획서는 2범주, 보정변수 설정안은 3범주를 권합니다."],
    ["5", "민감도 분석 최종 목록", "Model 3, 다른 기준, 1잔 g, 대푯값, 통풍 기준, 폭음 등"],
    ["6", "세부 정의", "혈청 요산 '&lt;1.0' 표기, eGFR 식(2009 또는 2021), 임신·수유 여성(4명), 폐경 전후 탐색 분석을 남길지"],
], widths=[8, 30, 62]))
A("""<h3>조원이 처리할 것</h3><ul>
<li>계획서가 근거로 든 <code>연구정의_확정.md</code>, <code>음주량 알코올 g 기준 설정 과정.md</code>를 원본을 가진 조원이 올립니다.</li>
<li>결과표 빈 틀에 남녀 비교표가 있다면 질문 개정에 맞게 고칩니다.</li>
<li>재현성 점검(코드를 짜지 않은 조원이 최종 코드를 두 번 실행)을 맡을 사람을 정합니다.</li>
</ul><h3>앞으로의 일정 (README 7장)</h3>""")
A(table(["기간", "단계", "할 일"], [
    ["11/23 – 11/25", "보고서 초안", "주분석 기준 확정과 최종 분석 실행, 결과 해석 토론, 서론·방법·결과·고찰 초안, 한계 정리"],
    ["11/30 – 12/2", "최종 완성", "수치·참고문헌 최종 확인, 발표 자료, 리허설"], ["12/6", "완성", "보고서 완성"]], widths=[20, 18, 62]))
A("</section>")

# 8. 지도와 용어
A("<section><h2>8. 저장소 지도와 용어</h2>")
cnt = collections.Counter((f.split("/")[0] if "/" in f else "(맨 위)") for f in files)
A(table(["위치", "파일 수", "내용", "먼저 볼 것"], [
    ["맨 위 폴더", cnt["(맨 위)"], "기준 문서, 소개, 재현성 절차, 발표 자료", "<code>분석계획서.md</code>, <code>README.md</code>"],
    ["<code>Result/</code>", cnt["Result"], "조원이 정리한 결과와 결정 자료", "<code>Result/README.md</code>의 파일 목록"],
    ["<code>code/</code>", cnt["code"], "분석 코드. 최종은 <code>final_analysis.R</code>", "<code>code/README.md</code>"],
    ["<code>output/</code>", cnt["output"], "코드가 만든 표·그림. 대부분 이전 설계의 기록", "<code>output/README.md</code>"],
    ["<code>reference/</code>", cnt["reference"], "논문 원문과 요약 101편", "<code>reference/논문 목록.md</code>"],
    ["<code>docs/</code>", cnt["docs"], "국건영 원시자료 이용지침서, 영양조사 코드자료집", "제8기 지침서"],
    ["<code>meeting/</code>", cnt["meeting"], "11/16–11/18 작업 계획", ""],
    ["<code>.claude/</code>", cnt[".claude"], "AI 스킬: 국건영 분석 절차, 기준별 p값 도구", "<code>knhanes_ptest/SKILL.md</code>"],
], widths=[17, 8, 45, 30]))
A(f'<p class="src">파일 수는 GitHub에 올라간 기준(커밋 {head}, 총 {n_files}개)입니다. 원시자료(data/)는 올리지 않습니다.</p>')
A("""<h3>처음 보는 사람이 읽을 순서 (모두 이 문서 안에 있습니다)</h3><ol class="read">
<li><span><b>1–2장</b> <em class="pg2">{{P:1}}쪽</em> 한눈에 보기와 연구 질문: 지금 어디까지 왔고 무엇을 묻는가</span></li>
<li><span><b>부록 A</b> <em class="pg2">{{P:A}}쪽</em> README 1–4장: 연구 배경, 선행연구, 가설, 연구 방법</span></li>
<li><span><b>부록 B</b> <em class="pg2">{{P:B}}쪽</em> 분석계획서 전문: 변수 정의와 지침서 쪽수, 모형, 민감도, 검증까지 분석 규칙 전체</span></li>
<li><span><b>6장</b> <em class="pg2">{{P:6}}쪽</em> 지금까지의 결과: 음주 기준 16개의 질문별 p값(주 모형 M2)</span></li>
<li><span><b>부록 C</b> <em class="pg2">{{P:C}}쪽</em> 문서 점검 결과: 저장소에서 고친 것과 남은 것</span></li>
<li><span><b>7장</b> <em class="pg2">{{P:7}}쪽</em> 남은 일과 일정</span></li></ol>
<p class="src">원본 파일: <code>README.md</code>, <code>분석계획서.md</code>, <code>Result/2026-11-18_문서점검_결과.md</code>. 보정 묶음 6개 전체의 p값은 <code>Result/2026-11-18_ptest_전체표_조건과p.xlsx</code>와 <code>output/21_ptest/부질문2_3_전체기준.md</code>에 있습니다.</p>
<h3>용어 풀이</h3>""")
A(table(["용어", "뜻"], [
    ["고요산혈증", "혈액 속 요산이 높은 상태로 통풍의 원인. 이 연구는 남 7.0, 여 6.0 mg/dL 이상"],
    ["효과수정(교호작용)", "한 요인(음주)의 효과가 다른 요인(홍조)에 따라 달라지는 것"],
    ["범주별 공동 검정", "음주 수준 범주마다의 차이를 한꺼번에 보는 검정. 어떤 모양의 차이든 잡는다"],
    ["경향성 검정", "음주량이 늘수록 차이가 한 방향으로 커지는지 기울기 하나로 보는 1 자유도 검정"],
    ["기울기의 비", "홍조군의 10 g/일당 오즈비 ÷ 비홍조군의 10 g/일당 오즈비. 1보다 크면 홍조군에서 더 가파름"],
    ["복합표본설계", "조사구와 가구를 뽑아 조사한 자료이므로 가중치·층·집락을 반영해야 전국 값을 바르게 추정한다"],
    ["p값", "실제로 차이가 없을 때 이만큼 또는 이보다 큰 차이가 우연히 나올 확률. 여러 번 검정하면 작은 값이 우연히 나오기 쉽다"],
    ["†", "비교하는 칸에 고요산혈증 사례가 10명 미만이라 해석하지 않는다는 표시"],
], widths=[22, 78]))
A("</section>")

# ---------------- 부록: 저장소 문서를 그대로 옮긴다 ----------------
def mermaid_flow(block):
    """GitHub 전용 mermaid 'flowchart LR'(노드 → 노드 사슬)을 같은 내용의 HTML 흐름 상자로 바꾼다."""
    labels, order, edges, style = {}, [], [], {}
    node = r'(\w+)(?:\[\s*"?(.*?)"?\s*\])?'
    for line in block.splitlines():
        line = line.strip()
        m = re.match(node + r'\s*--\s*"(.*?)"\s*-->\s*' + node + r'\s*$', line)
        if m:
            a, la, e, b, lb = m.groups()
            for k, lab in ((a, la), (b, lb)):
                if lab:
                    labels[k] = lab
                if k not in order:
                    order.append(k)
            edges.append(e)
            continue
        s = re.match(r"style\s+(\w+)\s+(.*)", line)
        if s:
            style[s.group(1)] = "hot" if "c0504d" in s.group(2) else "final"
    out = ['<div class="flowfig">']
    for i, k in enumerate(order):
        out.append(f'<div class="n {style.get(k, "")}">{labels.get(k, k)}</div>')
        if i < len(edges):
            out.append(f'<div class="e">{edges[i]}</div>')
    out.append("</div>")
    return "".join(out).replace("<br/>", "<br>")


def md_html(text):
    """Markdown → HTML. GitHub 전용 mermaid 도식은 같은 내용의 흐름 상자로 바꾸고, 제목 단계를 부록에 맞게 한 단계씩 내린다."""
    figs = []

    def keep(m):
        figs.append(mermaid_flow(m.group(0)))
        return f"MERMAIDFIG{len(figs) - 1}"
    text = re.sub(r"```mermaid.*?```", keep, text, flags=re.S)
    text = "\n".join(l for l in text.splitlines() if not l.lstrip().startswith("!["))   # 배지 그림 줄
    # 저장소 문서는 하위 목록을 2칸으로 들여쓴다. python-markdown은 4칸을 요구하므로 목록 줄의 들여쓰기를 두 배로 늘린다
    text = re.sub(r"(?m)^( +)(?=[-*] |\d+\. )", lambda m: m.group(1) * 2, text)
    h = markdown.markdown(text, extensions=["tables", "fenced_code", "sane_lists", "nl2br"])
    for i, f in enumerate(figs):
        h = h.replace(f"<p>MERMAIDFIG{i}</p>", f)
    for a, b in (("h4", "h5"), ("h3", "h4"), ("h2", "h3"), ("h1", "h3")):
        h = re.sub(rf"<(/?){a}>", rf"<\1{b}>", h)
    h = re.sub(r"<table>", '<table class="mdt">', h)
    return h


def appendix(label, title, note, html):
    A(f'<section class="app"><div class="app-head"><div class="lab">{label}</div><h2>{title}</h2><p>{note}</p></div>{html}</section>')


readme = Path("README.md").read_text(encoding="utf-8")
r1 = readme.index("## 1. 연구 배경"); r5 = readme.index("## 5. 예비 분석")
appendix("부록 A", "README 1–4장: 연구 배경과 방법", "원본: README.md 1–4장(조 공용 소개 문서). 5장 이후(예비 분석, 예상 결과, 일정, 작업 방법, 참고문헌)는 저장소에서 볼 수 있습니다.",
         md_html(readme[r1:r5].rstrip().rstrip("-").rstrip()))
plan = Path("분석계획서.md").read_text(encoding="utf-8")
plan = plan.split("\n", 1)[1]                                   # 맨 위 제목 줄은 부록 제목으로 대신한다
appendix("부록 B", "분석계획서 전문", "원본: 분석계획서.md(분석 규칙의 기준 문서). 상태 표시: ✅ 확정 · ⏳ 미확정 · 🔍 확인 필요.", md_html(plan))
rev = Path("Result/2026-11-18_문서점검_결과.md").read_text(encoding="utf-8").split("\n", 1)[1]
appendix("부록 C", "저장소 문서 점검 결과", "원본: Result/2026-11-18_문서점검_결과.md. 2026-11-18 점검, 같은 날 처리 상태 갱신.", md_html(rev))

doc = f'<!doctype html><html lang="ko"><head><meta charset="utf-8"><title>6조 활동 전체 정리</title><style>{CSS}</style></head><body>{"".join(H)}</body></html>'
browsers = [r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe", r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
            r"C:\Program Files\Google\Chrome\Application\chrome.exe"]
exe = next((b for b in browsers if Path(b).exists()), None)
if not exe:
    sys.exit("Edge 또는 Chrome을 찾지 못했습니다.")
tmp = Path(tempfile.gettempdir())


def render(html_doc):
    h = tmp / "knhanes_activity_report.html"
    h.write_text(html_doc, encoding="utf-8")
    before = OUT.stat().st_mtime if OUT.exists() else 0
    subprocess.run([exe, "--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
                    f"--user-data-dir={tmp / 'edge_pdf_profile'}", "--no-pdf-header-footer", f"--print-to-pdf={OUT}", h.as_uri()],
                   check=False, timeout=180, capture_output=True)
    for _ in range(60):
        if OUT.exists() and OUT.stat().st_mtime > before and OUT.stat().st_size > 0:
            return
        time.sleep(1)
    sys.exit("PDF가 만들어지지 않았습니다.")


# 목차·읽는 순서의 쪽 번호: 한 번 만든 PDF에서 각 장의 시작 쪽을 찾아 넣고 다시 만든다
MARK = {"1": "1. 한눈에 보기", "2": "2. 연구 질문", "3": "3. 자료와 분석 방법", "4": "4. 활동 기록", "5": "5. 분야별 산출물",
        "6": "6. 지금까지의 결과", "7": "7. 남은 일과 일정", "8": "8. 저장소 지도와 용어",
        "A": "부록 A\r\nREADME", "B": "부록 B\r\n분석계획서", "C": "부록 C\r\n저장소"}   # 부록 머리 상자의 글자
render(re.sub(r"\{\{P:\w\}\}", "00", doc))
import pypdfium2
pdf = pypdfium2.PdfDocument(str(OUT))
texts = [pdf[i].get_textpage().get_text_range() for i in range(len(pdf))]
pdf.close()
pg = {}
for k, m in MARK.items():
    for i, tx in enumerate(texts[2:], start=3):                  # 1–2쪽(표지·목차)은 건너뛴다
        if m in tx:
            pg[k] = i
            break
render(re.sub(r"\{\{P:(\w)\}\}", lambda m: str(pg.get(m.group(1), "")), doc))
print("쪽 번호:", pg)
print("저장:", OUT, f"({OUT.stat().st_size / 1024:.0f} KB) | 커밋 {n_commit}, 병합 {n_merge}, 로컬 변경 {len(local)}")
print("M2 고요산혈증:", hs)
print("M2 혈청 요산:", us)
