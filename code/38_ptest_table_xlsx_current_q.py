# 38_ptest_table_xlsx_current_q.py
# 목적: /knhanes_ptest 전체 표를 '지금의 연구 질문'으로 다시 묶은 엑셀을 만든다.
#       지금 질문(조원 논의안): 주질문(지속 홍조 vs 비홍조, 범주별) · 부질문(홍조 소실 vs 비홍조, 범주별)
#       + 보조 분석: 경향성 ①(주질문의 1 df 경향성) · 경향성 ②(부질문의 1 df 경향성).
#       삭제된 이전 부질문 2(남녀 차이 3원 교호작용)와 그 탐색(폐경 전후)은 '참고'로 옮긴다.
# 입력: Result/2026-11-18_ptest_전체표_조건과p.csv  (주질문·부질문 범주별 검정 등, code/29가 만듦)
#       output/21_ptest/부질문2_3_전체기준.csv        (경향성 ①·② 16개 기준 전체, code/35가 만듦.
#                                                     2군은 범주별과 같은 값, 연속형 직선은 1 df 검정 그대로, 스플라인은 해당 없음)
# 출력: Result/2026-11-23_ptest_전체표_현재질문.xlsx (output/21_ptest/에도 같은 파일)
# 실행: python code/38_ptest_table_xlsx_current_q.py
# 원칙: p는 숫자로 넣고 색·기호는 서식으로만 붙인다. 계산식은 없다. 값은 두 CSV에서 그대로 옮긴다.

import csv
import re
import shutil
import sys
from pathlib import Path

from openpyxl import Workbook
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import get_column_letter

sys.stdout.reconfigure(encoding="utf-8")
SRC = Path("Result/2026-11-18_ptest_전체표_조건과p.csv")
TRD = Path("output/21_ptest/부질문2_3_전체기준.csv")
OUT = Path("Result/2026-11-23_ptest_전체표_현재질문.xlsx")
OUT2 = Path("output/21_ptest/ptest_전체표_현재질문.xlsx")

rows = list(csv.DictReader(SRC.open(encoding="utf-8-sig", newline="")))
trend = {}
for r in csv.DictReader(TRD.open(encoding="utf-8-sig", newline="")):
    g = "G" + re.match(r"T(\d{2})", r["기준"]).group(1)
    q = "경향성① " if r["질문"] == "부질문 2" else "경향성② "
    trend[(g, r["결과"], r["보정"], q + r["성별"])] = r["p"]

# 지금 질문 이름으로 열 이름 바꾸기
RENAME = {"주 남": "주질문 남", "주 여": "주질문 여", "부1 남": "부질문 남", "부1 여": "부질문 여",
          "부2 성별": "참고: 성별 차이(삭제된 이전 부질문 2)", "부2 폐경": "참고: 폐경 전후 차이(탐색)",
          "주 경향 남": "경향성① 남", "주 경향 여": "경향성① 여", "부1 경향 남": "경향성② 남", "부1 경향 여": "경향성② 여",
          "부2 경향 성별": "참고: 성별 차이 경향성(삭제)", "소실vs지속 남": "탐색: 소실 vs 지속 남", "소실vs지속 여": "탐색: 소실 vs 지속 여",
          "탐색(홍조 체질) 남": "탐색: 홍조 체질 남", "탐색(홍조 체질) 여": "탐색: 홍조 체질 여",
          "비선형성 남": "탐색: 비선형성 남", "비선형성 여": "탐색: 비선형성 여",
          "참고 현재홍조 남": "참고: 현재 홍조 2군 남", "참고 현재홍조 여": "참고: 현재 홍조 2군 여"}
for r in rows:
    for a, b in RENAME.items():
        r[b] = r.pop(a, "")
    if r["단계"] == "G":                                   # 경향성 ①·②는 16개 기준 전체가 있는 경향성 표에서 가져온다
        for k in ("경향성① 남", "경향성① 여", "경향성② 남", "경향성② 여"):
            v = trend.get((r["기준"][:3], r["결과변수"], r["보정묶음"], k))
            if v is not None:
                r[k] = v

MAIN = ["주질문 남", "주질문 여", "부질문 남", "부질문 여"]
TREND = ["경향성① 남", "경향성① 여", "경향성② 남", "경향성② 여"]
EXTRA = ["탐색: 소실 vs 지속 남", "탐색: 소실 vs 지속 여", "탐색: 홍조 체질 남", "탐색: 홍조 체질 여",
         "탐색: 비선형성 남", "탐색: 비선형성 여", "참고: 현재 홍조 2군 남", "참고: 현재 홍조 2군 여",
         "참고: 폐경 전후 차이(탐색)", "참고: 성별 차이(삭제된 이전 부질문 2)", "참고: 성별 차이 경향성(삭제)"]
P_COLS = set(MAIN + TREND + EXTRA)
HUA, UA = "고요산혈증(예/아니오)", "혈청 요산(mg/dL, 연속형)"
BUNDLE_ORDER = ["Crude", "M2", "M2S", "M2A", "M3", "M3S"]

# ---------------- 서식 ----------------
FONT = "맑은 고딕"
f_base = Font(name=FONT, size=10); f_bold = Font(name=FONT, size=10, bold=True)
f_head = Font(name=FONT, size=10, bold=True, color="FFFFFF"); f_title = Font(name=FONT, size=14, bold=True)
f_sig = Font(name=FONT, size=10, bold=True, color="006100"); f_dag = Font(name=FONT, size=10, italic=True, color="808080")
f_note = Font(name=FONT, size=9, color="595959")
fill_head = PatternFill("solid", fgColor="305496"); fill_trend = PatternFill("solid", fgColor="2E7D4F")
fill_ref = PatternFill("solid", fgColor="7F7F7F"); fill_sub = PatternFill("solid", fgColor="D9E1F2")
fill_sig = PatternFill("solid", fgColor="C6EFCE"); fill_near = PatternFill("solid", fgColor="FFF2CC")
fill_dag = PatternFill("solid", fgColor="EDEDED"); fill_band = PatternFill("solid", fgColor="F7F9FC")
fill_m2 = PatternFill("solid", fgColor="FCE4D6"); fill_cand = PatternFill("solid", fgColor="E3EBF7")
thin = Side(style="thin", color="BFBFBF"); border = Border(left=thin, right=thin, top=thin, bottom=thin)
wrap = Alignment(wrap_text=True, vertical="top"); center = Alignment(horizontal="center", vertical="center", wrap_text=True)


def crit_name(s):
    return re.sub(r"^[GC][0-9]{2} ", "", s)


def put_p(cell, raw):
    raw = (raw or "").strip()
    cell.alignment = center; cell.border = border; cell.font = f_base
    if not raw:
        return
    if raw in ("무효", "해당 없음"):
        cell.value = raw; cell.font = f_dag; cell.fill = fill_dag; return
    dag = "†" in raw
    num = raw.replace("*", "").replace("†", "")
    if num.startswith("<"):
        cell.value = raw.replace("*", ""); p = 0.0
    else:
        p = float(num); cell.value = p; cell.number_format = '0.000"†"' if dag else "0.000"
    if dag:
        cell.font = f_dag; cell.fill = fill_dag
    elif p < 0.05:
        cell.font = f_sig; cell.fill = fill_sig
    elif p < 0.10:
        cell.fill = fill_near


def head_cell(ws, r, c, v, fill=fill_head):
    x = ws.cell(row=r, column=c, value=v); x.font = f_head; x.fill = fill; x.alignment = center; x.border = border
    return x


def text(ws, r, c, v, font=f_base, fill=None, align=wrap):
    x = ws.cell(row=r, column=c, value=v); x.font = font; x.alignment = align; x.border = border
    if fill:
        x.fill = fill
    return x


def col_fill(name):
    return fill_trend if name in TREND else fill_ref if name.startswith(("참고", "탐색")) else fill_head


wb = Workbook()

# ================= 1. 읽는 법 =================
ws = wb.active; ws.title = "읽는 법"
ws.column_dimensions["A"].width = 30; ws.column_dimensions["B"].width = 100
ws["A1"] = "/knhanes_ptest 전체 표 — 지금의 연구 질문 기준 (2026-11-23)"; ws["A1"].font = f_title
info = [
    "자료: 국민건강영양조사 2019–2021, 최종 분석 대상 10,779명(남 5,411 · 여 5,368). 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu. p는 설계 기반 Wald F 검정.",
    "지금의 연구 질문(조원 논의안): 주질문과 부질문은 범주별 공동 검정, 경향성 ①·②는 보조 분석. 이전 부질문 2(남녀 차이)는 삭제되어 '참고' 열로 옮겼다.",
    "출처: Result/2026-11-18_ptest_전체표_조건과p.csv(주질문·부질문 등) + output/21_ptest/부질문2_3_전체기준.csv(경향성 ①·②) → code/38_ptest_table_xlsx_current_q.py. 생성형 AI(Claude)로 정리했으며 조원 검토가 필요하다.",
    "절단점 단위는 g/일(1잔 7 g이면 주당 잔 수와 같음, 주당 g으로는 ×7). 'a · b'는 a 미만 / a 이상 b 미만 / b 이상.",
    "탐색 단계 결과다. 기준 16개 × 결과 2개 × 보정 묶음 6개를 함께 보았으므로, p < 0.05 하나를 확증으로 읽지 말고 미리 정한 주분석 기준과 주 모형을 먼저 해석한다.",
    "'남HUA·여요산' 시트 2장: 남자는 고요산혈증, 여자는 혈청 요산(연속형)으로 본 표다. 여자는 고요산혈증 사례가 427명뿐이라 거의 모든 칸에 †가 붙기 때문이다. 11/23 추가 실행(R·L·W)도 함께 실었다(출처 output/21_ptest/ptest_log.csv).",
]
for i, s in enumerate(info, 3):
    ws.cell(row=i, column=1, value=s).font = f_base
    ws.merge_cells(start_row=i, start_column=1, end_row=i, end_column=2)
    ws.cell(row=i, column=1).alignment = Alignment(wrap_text=True, vertical="top"); ws.row_dimensions[i].height = 32
r = 9
ws.cell(row=r, column=1, value="지금의 질문과 열 이름").font = f_bold; r += 1
head_cell(ws, r, 1, "열"); head_cell(ws, r, 2, "무엇에 대한 p인가")
for a, b in [
    ("주질문 남 / 여", "남자(여자)에서 음주 수준과 결과의 관계가 지속 홍조군과 비홍조군 사이에 다른가. 음주 × 지속 홍조 교호작용의 범주별 공동 검정(음주 수준 수 − 1 자유도)"),
    ("부질문 남 / 여", "같은 관계가 현재 홍조가 없는 사람 가운데 홍조 소실군과 비홍조군 사이에 다른가. 음주 × 홍조 소실 교호작용의 범주별 공동 검정"),
    ("경향성① 남 / 여 (보조)", "주질문의 경향성: 음주가 늘수록 위험이 커지는 기울기가 지속 홍조군에서 비홍조군보다 큰가. 범주별 g/일 중앙값 점수의 1 자유도 검정. 2군 기준은 범주별과 같은 값, 연속형 직선(G05·G11)은 주질문의 1 자유도 값 그대로, 스플라인(G13)은 해당 없음"),
    ("경향성② 남 / 여 (보조)", "부질문의 경향성: 홍조 소실군의 기울기가 비홍조군과 다른가. 규칙은 경향성 ①과 같음"),
    ("탐색: 소실 vs 지속", "홍조 소실군과 지속 홍조군 사이의 차이"),
    ("탐색: 홍조 체질", "과거 또는 현재 홍조가 있는 사람 vs 한 번도 없는 사람"),
    ("탐색: 비선형성", "스플라인일 때 음주와 결과의 관계가 직선에서 벗어나는가"),
    ("참고: 현재 홍조 2군", "11/2 계획 발표 때의 주질문(현재 홍조 vs 현재 비홍조)"),
    ("참고: 폐경 전후 차이", "여성에서 지속 홍조와 비홍조의 차이가 폐경 전후에 다른가(이전 부질문 2의 탐색)"),
    ("참고: 성별 차이(삭제)", "지속 홍조와 비홍조의 차이가 남녀 사이에 다른가. 2026-11-22에 삭제된 이전 부질문 2"),
]:
    r += 1; text(ws, r, 1, a, f_bold); text(ws, r, 2, b)
r += 2; ws.cell(row=r, column=1, value="색과 기호").font = f_bold; r += 1
head_cell(ws, r, 1, "표시"); head_cell(ws, r, 2, "뜻")
for v, fl, fo, nf, desc in [(0.023, fill_sig, f_sig, "0.000", "p < 0.05 (초록)"), (0.074, fill_near, f_base, "0.000", "0.05 ≤ p < 0.10 (노랑)"),
                            (0.019, fill_dag, f_dag, '0.000"†"', "† 비교하는 칸에 고요산혈증 사례 10명 미만 — p가 작아도 해석하지 않음"),
                            ("무효", fill_dag, f_dag, "General", "칸이 비어 계산이 성립하지 않음(분리)"),
                            ("해당 없음", fill_dag, f_dag, "General", "그 기준에서는 정의되지 않는 검정(예: 스플라인의 경향성)")]:
    r += 1; c = text(ws, r, 1, v, fo, fl, center); c.number_format = nf; text(ws, r, 2, desc)
r += 2; ws.cell(row=r, column=1, value="열 머리 색").font = f_bold; r += 1
for lab, fl, desc in [("파랑", fill_head, "주질문·부질문(범주별 검정)"), ("초록", fill_trend, "경향성 ①·②(보조 분석)"), ("회색", fill_ref, "탐색·참고")]:
    r += 1; x = text(ws, r, 1, lab, f_head, fl, center); text(ws, r, 2, desc)
r += 2; ws.cell(row=r, column=1, value="보정 묶음").font = f_bold; r += 1
head_cell(ws, r, 1, "묶음"); head_cell(ws, r, 2, "변수")
seen = {}
for x in rows:
    if x["단계"] == "G":
        seen.setdefault(x["보정묶음"], x["보정변수"])
for b in BUNDLE_ORDER:
    r += 1; text(ws, r, 1, b + (" (주 모형)" if b == "M2" else ""), f_bold, fill_m2 if b == "M2" else None); text(ws, r, 2, seen.get(b, ""))
r += 1; ws.cell(row=r, column=1, value="회의 자료(11/22)의 추천안은 흡연 3범주(M2S에 해당)를 주 모형으로, Model 1(연령+폐경)을 비교 모형으로 둔다. Model 1 결과는 이 표에 없다(ptest 실행 기록 R1–R5 참고).").font = f_note
ws.merge_cells(start_row=r, start_column=1, end_row=r, end_column=2)

# ================= 2. M2 한눈에 =================
ws = wb.create_sheet("M2 한눈에")
g_m2 = sorted([x for x in rows if x["단계"] == "G" and x["보정묶음"] == "M2"], key=lambda z: z["번호"])
crits = []
for x in g_m2:
    if x["기준"] not in crits:
        crits.append(x["기준"])
by = {(x["기준"], x["결과변수"]): x for x in g_m2}
ws["A1"] = "최종 분석 G01–G16 · 주 모형 M2 · 지금의 질문(주질문·부질문 + 경향성 보조)"; ws["A1"].font = f_title
fixed = ["번호", "기준", "음주량 형태", "남 절단점(g/일)", "여 절단점(g/일)"]
r0 = 3
for j, c in enumerate(fixed, 1):
    ws.merge_cells(start_row=r0, start_column=j, end_row=r0 + 2, end_column=j); head_cell(ws, r0, j, c)
col = len(fixed) + 1
for oc_title, ocfill in (("결과: 고요산혈증(예/아니오)", "305496"), ("결과: 혈청 요산(mg/dL, 연속형)", "548235")):
    ws.merge_cells(start_row=r0, start_column=col, end_row=r0, end_column=col + 7)
    head_cell(ws, r0, col, oc_title, PatternFill("solid", fgColor=ocfill))
    ws.merge_cells(start_row=r0 + 1, start_column=col, end_row=r0 + 1, end_column=col + 3); head_cell(ws, r0 + 1, col, "범주별 검정", fill_head)
    ws.merge_cells(start_row=r0 + 1, start_column=col + 4, end_row=r0 + 1, end_column=col + 7); head_cell(ws, r0 + 1, col + 4, "경향성(보조)", fill_trend)
    for k, q in enumerate(MAIN + TREND):
        h = ws.cell(row=r0 + 2, column=col + k, value=q); h.font = f_bold; h.fill = fill_sub; h.alignment = center; h.border = border
    col += 8
ws.row_dimensions[r0 + 2].height = 30
r = r0 + 2
for i, cr in enumerate(crits):
    r += 1
    base = by.get((cr, HUA))
    band = fill_cand if "설정안" in cr else (fill_band if i % 2 else None)
    text(ws, r, 1, cr[:3], f_base, band, center); text(ws, r, 2, crit_name(cr), f_bold, band)
    text(ws, r, 3, base["음주량형태"], f_base, band, center)
    text(ws, r, 4, base["남 절단점(g/일)"] or base["연속형 단위"], f_base, band, center)
    text(ws, r, 5, base["여 절단점(g/일)"] or base["연속형 단위"], f_base, band, center)
    c0 = len(fixed) + 1
    for oc in (HUA, UA):
        src = by.get((cr, oc), {})
        for k, q in enumerate(MAIN + TREND):
            put_p(ws.cell(row=r, column=c0 + k), src.get(q, ""))
        c0 += 8
for j, w in enumerate([7, 36, 15, 19, 19] + [9.2] * 16, 1):
    ws.column_dimensions[get_column_letter(j)].width = w
ws.freeze_panes = ws.cell(row=r0 + 3, column=3)
r += 2
for note in ["파란 바탕 기준(S1–S5)은 분석계획서의 주분석 후보. 회의 자료의 추천은 S3(G03)이며, 주분석 기준은 아직 정하지 않았다.",
             "색: 초록 p < 0.05 · 노랑 0.05–0.10 · 회색 † 사례 10명 미만 또는 무효·해당 없음. G05와 G11은 단위만 다른 같은 모형."]:
    ws.cell(row=r, column=1, value=note).font = f_note; r += 1

# ================= 3·4. 결과별 상세 =================
DETAIL_FIXED = ["번호", "기준", "음주량 형태", "남 절단점(g/일)", "여 절단점(g/일)", "연속형 단위", "보정 묶음"]


def detail_sheet(name, outcome):
    ws = wb.create_sheet(name)
    data = sorted([x for x in rows if x["단계"] == "G" and x["결과변수"] == outcome], key=lambda z: (z["번호"], BUNDLE_ORDER.index(z["보정묶음"])))
    pcols = [c for c in MAIN + TREND + EXTRA if any(x.get(c) for x in data)]
    cols = DETAIL_FIXED + pcols
    for j, c in enumerate(cols, 1):
        head_cell(ws, 1, j, c, col_fill(c) if c in P_COLS else fill_head)
    ws.row_dimensions[1].height = 46
    for j, w in enumerate([7, 34, 15, 18, 18, 16, 10] + [10] * len(pcols), 1):
        ws.column_dimensions[get_column_letter(j)].width = w
    prev, band_on = None, False
    for i, x in enumerate(data, 2):
        if x["번호"] != prev:
            band_on, prev = not band_on, x["번호"]
        band = fill_band if band_on else None
        vals = [x["번호"], crit_name(x["기준"]), x["음주량형태"], x["남 절단점(g/일)"], x["여 절단점(g/일)"], x["연속형 단위"], x["보정묶음"]]
        for j, v in enumerate(vals, 1):
            text(ws, i, j, v, f_bold if j in (2, 7) else f_base, fill_m2 if (j == 7 and v == "M2") else band, center if j != 2 else wrap)
        for j, c in enumerate(pcols, len(DETAIL_FIXED) + 1):
            put_p(ws.cell(row=i, column=j), x.get(c, ""))
    ws.freeze_panes = "H2"
    ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(data) + 1}"
    ws.cell(row=len(data) + 3, column=1, value=f"결과 변수: {outcome}. 열 머리 색: 파랑 주질문·부질문, 초록 경향성(보조), 회색 탐색·참고. 같은 기준의 6줄은 같은 바탕색, 주 모형 M2는 주황.").font = f_note


detail_sheet("고요산혈증", HUA)
detail_sheet("혈청 요산(연속형)", UA)

# ================= 5. 1차 분석 C =================
ws = wb.create_sheet("1차 분석 C")
data = sorted([x for x in rows if x["단계"] == "C"], key=lambda z: (BUNDLE_ORDER.index(z["보정묶음"]) if z["보정묶음"] in BUNDLE_ORDER else 9, z["번호"]))
pcols = [c for c in MAIN + TREND + EXTRA if any(x.get(c) for x in data)]
cols = DETAIL_FIXED + pcols
for j, c in enumerate(cols, 1):
    head_cell(ws, 1, j, c, col_fill(c) if c in P_COLS else fill_head)
ws.row_dimensions[1].height = 46
for j, w in enumerate([7, 38, 15, 18, 18, 16, 10] + [10] * len(pcols), 1):
    ws.column_dimensions[get_column_letter(j)].width = w
for i, x in enumerate(data, 2):
    band = fill_band if x["보정묶음"] == "M3" else None
    vals = [x["번호"], crit_name(x["기준"]), x["음주량형태"], x["남 절단점(g/일)"], x["여 절단점(g/일)"], x["연속형 단위"], x["보정묶음"]]
    for j, v in enumerate(vals, 1):
        text(ws, i, j, v, f_bold if j in (2, 7) else f_base, band, center if j != 2 else wrap)
    for j, c in enumerate(pcols, len(DETAIL_FIXED) + 1):
        put_p(ws.cell(row=i, column=j), x.get(c, ""))
ws.freeze_panes = "H2"
ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(data) + 1}"
ws.cell(row=len(data) + 3, column=1, value="1차 분석(고요산혈증). 경향성 ①·②는 이 실행에서 경향성 검정을 함께 한 기준에만 있다.").font = f_note

# ================= 6. 검증·시험 =================
ws = wb.create_sheet("검증·시험")
data = [x for x in rows if x["단계"] == "검증"]
vcols = ["번호", "기준", "질문구조", "음주량형태", "남 절단점(g/일)", "여 절단점(g/일)", "연속형 단위", "1잔(g)", "대상", "결과변수", "보정변수"]
pcols = [c for c in MAIN + TREND + EXTRA if any(x.get(c) for x in data)]
cols = vcols + pcols
for j, c in enumerate(cols, 1):
    head_cell(ws, 1, j, c, col_fill(c) if c in P_COLS else fill_head)
ws.row_dimensions[1].height = 46
for j, w in enumerate([6, 30, 16, 14, 14, 14, 14, 7, 16, 18, 40] + [10] * len(pcols), 1):
    ws.column_dimensions[get_column_letter(j)].width = w
for i, x in enumerate(data, 2):
    for j, c in enumerate(vcols, 1):
        v = x[c].split("(")[0] if c == "질문구조" else x[c]
        text(ws, i, j, v, f_base, fill_dag if (c == "질문구조" and x["질문구조"].startswith("개정 전")) else None, wrap)
    for j, c in enumerate(pcols, len(vcols) + 1):
        put_p(ws.cell(row=i, column=j), x.get(c, ""))
ws.freeze_panes = "C2"
ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(data) + 1}"
ws.cell(row=len(data) + 3, column=1, value="도구 검증·시험 실행(해석에 쓰지 않음). 질문구조가 '개정 전'인 줄의 주질문 열은 현재 홍조 vs 현재 비홍조 비교의 p다.").font = f_note

# ================= 6-1. 남 고요산혈증 · 여 혈청 요산(연속형) =================
# 여자는 고요산혈증 사례가 적어(427명) 거의 모든 칸에 †가 붙으므로, 여자는 혈청 요산(mg/dL, 선형회귀)으로 본다.
# 값은 ptest 실행 기록(output/21_ptest/ptest_log.csv)에서 직접 가져온다.
#   G01–G16: 2026-11-16 실행(주질문·부질문·효과), 경향성 p는 위와 같이 경향성 표(code/35)에서.
#   11/23 추가 실행: R1–R5(추천안·민감도), L1·L2(Lee 2019 3군), W1–W3(여성 요산 연속형 음주량).
LOG = Path("output/21_ptest/ptest_log.csv")
BMAP = {"Crude": "Crude", "Model 1": "M1", "Model 2": "M2", "Model 2-f_smk+f_smk3": "M2S",
        "Model 2-f_smk+f_smk3-age+age10": "M2A", "Model 3": "M3", "Model 3-f_smk+f_smk3": "M3S"}
B_ORDER2 = ["Crude", "M1", "M2", "M2S", "M2A", "M3", "M3S"]
OUT_FULL = {"고요산혈증": HUA, "혈청 요산": UA}
log_rows = list(csv.DictReader(LOG.open(encoding="utf-8-sig", newline="")))


def log_p(r):
    """기록의 p를 표 문자열로: 분리 → 무효, 사례 10명 미만 → †, 0.001 미만 → <0.001"""
    if "분리" in r["비고"]:
        return "무효"
    try:
        p = float(r["p"])
    except ValueError:
        return "무효"
    s = "<0.001" if p < 0.001 else f"{p:.3f}"
    return s + ("†" if "10명 미만" in r["비고"] else "")


def log_eff(r):
    return (r.get("효과") or r.get("효과(1 df)") or "").strip()


runs = {}                                                  # 기준 → 가장 늦은 실행 시각
for r in log_rows:
    k = r["기준"]
    keep = (k[:1] == "G" and r["run_time"].startswith("2026-11-16")) or (k[:1] in "RWL" and r["run_time"] >= "2026-11-23")
    if keep and r["run_time"] > runs.get(k, ""):
        runs[k] = r["run_time"]
idx = {}
for r in log_rows:
    if runs.get(r["기준"]) == r["run_time"] and r["보정"] in BMAP:
        idx.setdefault((r["기준"], r["결과"], BMAP[r["보정"]], r["대상"]), []).append(r)


def pick(rs, 구분, trend=None, contains=None):
    for r in rs:
        if r["구분"] != 구분:
            continue
        t = r["검정"]
        if contains and contains not in t:
            continue
        if trend is True and "경향성" not in t:
            continue
        if trend is False and "경향성" in t:
            continue
        return r
    return None


def cell_vals(label, outcome, b, sex):
    """한 기준·결과·보정·성별의 [주, 부, 경향①, 경향②] p 문자열과 효과(주, 경향①, 비홍조군 기울기)"""
    rs = idx.get((label, outcome, b, sex), [])
    if not rs:
        return None
    main, sub = pick(rs, "주질문", False), pick(rs, "부질문 1", False)
    t1, t2 = pick(rs, "주질문", True), pick(rs, "부질문 1", True)
    form = "spline" if (main and "3 df" in main["검정"] and pick(rs, "용량-반응", contains="비선형성")) else (
        "linear" if (main and "(1 df)" in main["검정"]) else "cut")
    ps = [log_p(main) if main else "", log_p(sub) if sub else ""]
    if label[:1] == "G":                                   # 경향성: 16개 기준 전체가 있는 경향성 표 값
        g = label[:3]
        ps += [trend.get((g, OUT_FULL[outcome], b, f"경향성① {sex[0]}"), ""), trend.get((g, OUT_FULL[outcome], b, f"경향성② {sex[0]}"), "")]
    elif form == "spline":
        ps += ["해당 없음", "해당 없음"]
    elif form == "linear":                                 # 연속형 직선은 1 df 검정이 곧 경향성 검정
        ps += ps[:2]
    else:
        ps += [log_p(t1) if t1 else "", log_p(t2) if t2 else ""]
    nf = pick(rs, "용량-반응", contains="비홍조군")
    effs = [log_eff(main) if main else "", log_eff(t1) if t1 else (log_eff(main) if (main and form == "linear") else ""),
            (f"{log_eff(nf)} · p {log_p(nf)}" if nf else "")]
    return ps, effs, form


g_labels = sorted([k for k in runs if k[:1] == "G"])
x_order = ["R1", "R2", "R3", "R4", "R5", "L1", "L2", "W1", "W2", "W3"]
x_labels = sorted([k for k in runs if k[:1] in "RWL"], key=lambda k: x_order.index(k[:2]) if k[:2] in x_order else 99)
g_info = {x["기준"][:3]: x for x in rows if x["단계"] == "G" and x["결과변수"] == HUA and x["보정묶음"] == "M2"}
drink_txt = {}
for r in log_rows:
    if runs.get(r["기준"]) == r["run_time"]:
        drink_txt.setdefault(r["기준"], (r["음주량"], r.get("gram") or "7"))


def label_desc(lab):
    if lab[:1] == "G":
        x = g_info[lab[:3]]
        if x["남 절단점(g/일)"]:
            return f"남 {x['남 절단점(g/일)']} / 여 {x['여 절단점(g/일)']} g/일", "7"
        return x["연속형 단위"], "7"
    d, gram = drink_txt.get(lab, ("", "7"))
    return re.sub(r" \+ 범주 중앙값 경향성|, 1잔 = [0-9]+ g", "", d), gram


SUBHEAD = ["주질문", "부질문", "경향성①", "경향성②"]
EFF_HEAD = ["여 주질문 β 차이(mg/dL)", "여 경향성① β 차이(10 g/일당)", "여 비홍조군 음주 기울기(β, 10 g/일당)"]


def mixed_sheet(title, bundles, note):
    ws = wb.create_sheet(title)
    ws["A1"] = "남자 = 고요산혈증(예/아니오) · 여자 = 혈청 요산(mg/dL, 연속형)"; ws["A1"].font = f_title
    ws["A2"] = note; ws["A2"].font = f_note
    fixed = ["번호", "기준", "음주량 기준", "1잔(g)", "보정 묶음"]
    r0 = 4
    for j, c in enumerate(fixed, 1):
        ws.merge_cells(start_row=r0, start_column=j, end_row=r0 + 1, end_column=j); head_cell(ws, r0, j, c)
    c0 = len(fixed) + 1
    for t, fl in (("남자: 고요산혈증 p", PatternFill("solid", fgColor="305496")), ("여자: 혈청 요산(연속형) p", PatternFill("solid", fgColor="C55A11"))):
        ws.merge_cells(start_row=r0, start_column=c0, end_row=r0, end_column=c0 + 3); head_cell(ws, r0, c0, t, fl)
        for k, s in enumerate(SUBHEAD):
            head_cell(ws, r0 + 1, c0 + k, s, fill_trend if s.startswith("경향") else fill_head)
        c0 += 4
    ws.merge_cells(start_row=r0, start_column=c0, end_row=r0, end_column=c0 + 2); head_cell(ws, r0, c0, "여자: 효과 크기(95% CI)", fill_ref)
    for k, s in enumerate(EFF_HEAD):
        head_cell(ws, r0 + 1, c0 + k, s, fill_ref)
    ws.row_dimensions[r0 + 1].height = 44
    r = r0 + 1
    for block, labs in (("최종 분석 G01–G16 (2026-11-16)", g_labels), ("11/23 추가 실행: 추천안·민감도(R), Lee 2019 3군(L), 여성 연속형 음주량(W)", x_labels)):
        r += 1
        ws.merge_cells(start_row=r, start_column=1, end_row=r, end_column=len(fixed) + 11)
        x = ws.cell(row=r, column=1, value=block); x.font = f_bold; x.fill = fill_sub
        for i, lab in enumerate(labs):
            for b in bundles:
                men = cell_vals(lab, "고요산혈증", b, "남자")
                wom = cell_vals(lab, "혈청 요산", b, "여자")
                if not men and not wom:
                    continue
                r += 1
                desc, gram = label_desc(lab)
                band = fill_cand if lab.startswith(("G01", "G02", "G03", "G04", "G05", "R1", "L1")) else (fill_band if i % 2 else None)
                name = re.sub(r"^[GRLW][0-9]{1,2} ", "", lab)
                for j, v in enumerate([lab.split(" ")[0], name, desc, gram, b], 1):
                    text(ws, r, j, v, f_bold if j == 2 else f_base, fill_m2 if (j == 5 and b == "M2S") else band, wrap if j in (2, 3) else center)
                for k in range(4):
                    put_p(ws.cell(row=r, column=len(fixed) + 1 + k), men[0][k] if men else "")
                    put_p(ws.cell(row=r, column=len(fixed) + 5 + k), wom[0][k] if wom else "")
                for k in range(3):
                    text(ws, r, len(fixed) + 9 + k, wom[1][k] if wom else "", f_base, band, center)
    for j, w in enumerate([7, 34, 26, 7, 9] + [9.5] * 8 + [24, 22, 28], 1):
        ws.column_dimensions[get_column_letter(j)].width = w
    ws.freeze_panes = f"C{r0 + 2}"
    r += 2
    for nt in ["남자 칸 색: 초록 p < 0.05 · 노랑 0.05–0.10 · 회색 † 사례 10명 미만 또는 무효. 여자 혈청 요산은 연속형 결과라 †가 붙지 않는다.",
               "여자 효과 크기: 범주형은 가장 높은 음주 수준에서 '해당 홍조군의 요산 차이 − 비홍조군의 요산 차이'(저음주 대비, mg/dL). 연속형은 음주 단위당 기울기의 차이(G11은 100 g/주당, W2는 두 배당).",
               "β가 음수면 지속 홍조군에서 음주에 따른 요산 상승이 비홍조군보다 작다는 뜻이다. '여 비홍조군 음주 기울기'는 비홍조군 안에서 음주 10 g/일당 요산 변화로, 범주형 경향성 기준에만 있다.",
               "M1 = 연령 + 폐경 상태(Model 1). 다른 묶음의 변수는 '읽는 법' 시트. 파란 바탕 = 계획서 후보(S1–S5)와 추천안(R1)·Lee 3군(L1)."]:
        ws.cell(row=r, column=1, value=nt).font = f_note; r += 1
    return ws


ws_m2s = mixed_sheet("남HUA·여요산 (M2S)", ["M2S"], "보정 묶음 M2S(Model 2에서 흡연을 3범주로 바꾼 것, 회의 자료의 추천 주 모형). 같은 표의 모든 보정 묶음은 다음 시트에 있다.")
ws_all = mixed_sheet("남HUA·여요산 (전체 보정)", B_ORDER2, "모든 보정 묶음. G 실행은 Crude·M2·M2S·M2A·M3·M3S, 11/23 실행은 Crude(L만)·M1·M2·M2S·M3S이다.")
wb.move_sheet(ws_m2s, offset=-(len(wb.sheetnames) - 3))
wb.move_sheet(ws_all, offset=-(len(wb.sheetnames) - 3))   # 순서: 읽는 법 → M2S → 전체 보정 → M2 한눈에 …

# ================= 7. 전체(원자료) =================
ws = wb.create_sheet("전체(원자료)")
base_cols = ["번호", "단계", "기준", "음주량형태", "남 절단점(g/일)", "여 절단점(g/일)", "연속형 단위", "경향성검정", "1잔(g)", "대상", "결과변수", "보정묶음", "보정변수", "질문구조"]
cols = base_cols + MAIN + TREND + EXTRA
for j, c in enumerate(cols, 1):
    head_cell(ws, 1, j, c, col_fill(c) if c in P_COLS else fill_head)
ws.row_dimensions[1].height = 46
for j, w in enumerate([6, 7, 34, 14, 16, 16, 14, 8, 7, 14, 20, 12, 40, 20] + [10] * (len(cols) - 14), 1):
    ws.column_dimensions[get_column_letter(j)].width = w
for i, x in enumerate(rows, 2):
    for j, c in enumerate(cols, 1):
        if c in P_COLS:
            put_p(ws.cell(row=i, column=j), x.get(c, ""))
        else:
            text(ws, i, j, x.get(c, ""), f_base, None, Alignment(vertical="top"))
ws.freeze_panes = "D2"
ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(rows) + 1}"

for s in wb.worksheets:
    s.sheet_view.zoomScale = 90
wb.save(OUT2)                                              # output/ 사본을 먼저 저장하고 Result/로 복사
try:
    shutil.copyfile(OUT2, OUT)
    print("저장:", OUT, "와", OUT2)
except PermissionError:                                    # Result/ 파일이 엑셀에서 열려 있으면 덮어쓸 수 없다
    print("주의: Result/ 파일이 열려 있어 덮어쓰지 못함. 엑셀을 닫고 다시 실행하세요. 저장된 곳:", OUT2)
print("시트:", ", ".join(wb.sheetnames), "| 원자료 줄:", len(rows), "| 경향성 값 출처 키:", len(trend))
