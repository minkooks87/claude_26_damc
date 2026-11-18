# 32_ptest_table_xlsx.py
# 목적: /knhanes_ptest 전체 표(CSV)를 읽기 편한 엑셀 파일로 바꾼다.
#       시트: 읽는 법 / M2 한눈에 / 고요산혈증 / 혈청 요산(연속형) / 1차 분석 C / 검증·시험 / 전체(원자료)
# 입력: output/21_ptest/ptest_전체표_조건과p.csv (code/29_ptest_full_table.R이 만든 표)
# 출력: output/21_ptest/ptest_전체표_조건과p.xlsx
# 실행: python code/32_ptest_table_xlsx.py
# 값: p는 숫자로 넣고(소수 셋째 자리 표시), 색과 기호는 서식으로만 붙인다. 계산식은 없다.

import csv
import re
from pathlib import Path

from openpyxl import Workbook
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import get_column_letter

SRC = Path("output/21_ptest/ptest_전체표_조건과p.csv")
OUT = Path("output/21_ptest/ptest_전체표_조건과p.xlsx")

with SRC.open(encoding="utf-8-sig", newline="") as f:
    rows = list(csv.DictReader(f))

# ---- 서식 ----
FONT = "맑은 고딕"
f_base = Font(name=FONT, size=10)
f_bold = Font(name=FONT, size=10, bold=True)
f_head = Font(name=FONT, size=10, bold=True, color="FFFFFF")
f_title = Font(name=FONT, size=14, bold=True)
f_sig = Font(name=FONT, size=10, bold=True, color="006100")
f_dag = Font(name=FONT, size=10, italic=True, color="808080")
f_note = Font(name=FONT, size=9, color="595959")
fill_head = PatternFill("solid", fgColor="305496")
fill_sub = PatternFill("solid", fgColor="D9E1F2")
fill_sig = PatternFill("solid", fgColor="C6EFCE")
fill_near = PatternFill("solid", fgColor="FFF2CC")
fill_dag = PatternFill("solid", fgColor="EDEDED")
fill_band = PatternFill("solid", fgColor="F7F9FC")
fill_m2 = PatternFill("solid", fgColor="FCE4D6")
thin = Side(style="thin", color="BFBFBF")
border = Border(left=thin, right=thin, top=thin, bottom=thin)
wrap = Alignment(wrap_text=True, vertical="top")
center = Alignment(horizontal="center", vertical="center", wrap_text=True)

BUNDLE_ORDER = ["Crude", "M2", "M2S", "M2A", "M3", "M3S"]
CORE = ["주 남", "주 여", "부1 남", "부1 여", "부2 성별", "부2 폐경"]
EXTRA = ["주 경향 남", "주 경향 여", "부1 경향 남", "부1 경향 여", "부2 경향 성별",
         "소실vs지속 남", "소실vs지속 여", "탐색(홍조 체질) 남", "탐색(홍조 체질) 여",
         "비선형성 남", "비선형성 여", "참고 현재홍조 남", "참고 현재홍조 여"]
P_COLS = set(CORE + EXTRA)
HUA = "고요산혈증(예/아니오)"
UA = "혈청 요산(mg/dL, 연속형)"


def crit_name(s):
    """'G01 [설정안 S1] 성별 삼분위' → '[설정안 S1] 성별 삼분위'"""
    return re.sub(r"^[GC][0-9]{2} ", "", s)


def put_p(cell, raw):
    """p 문자열('0.019†*', '무효', '')을 숫자 + 서식으로 넣는다."""
    raw = (raw or "").strip()
    cell.alignment = center
    cell.border = border
    if not raw:
        cell.font = f_base
        return
    if raw == "무효":
        cell.value = "무효"
        cell.font = f_dag
        cell.fill = fill_dag
        return
    dag = "†" in raw
    num = raw.replace("*", "").replace("†", "")
    if num.startswith("<"):
        cell.value = raw.replace("*", "")
        p = 0.0
    else:
        p = float(num)
        cell.value = p
        cell.number_format = '0.000"†"' if dag else "0.000"
    if dag:
        cell.font = f_dag
        cell.fill = fill_dag
    elif p < 0.05:
        cell.font = f_sig
        cell.fill = fill_sig
    elif p < 0.10:
        cell.font = f_base
        cell.fill = fill_near
    else:
        cell.font = f_base


def header(ws, r, cols, widths=None):
    for j, c in enumerate(cols, 1):
        x = ws.cell(row=r, column=j, value=c)
        x.font = f_head
        x.fill = fill_head
        x.alignment = center
        x.border = border
    ws.row_dimensions[r].height = 34
    if widths:
        for j, w in enumerate(widths, 1):
            ws.column_dimensions[get_column_letter(j)].width = w


def text(ws, r, c, v, font=f_base, fill=None, align=wrap):
    x = ws.cell(row=r, column=c, value=v)
    x.font = font
    x.alignment = align
    x.border = border
    if fill:
        x.fill = fill
    return x


def bundle_key(r):
    b = r["보정묶음"]
    return BUNDLE_ORDER.index(b) if b in BUNDLE_ORDER else 99


wb = Workbook()

# ================= 1. 읽는 법 =================
ws = wb.active
ws.title = "읽는 법"
ws.column_dimensions["A"].width = 26
ws.column_dimensions["B"].width = 100
ws["A1"] = "/knhanes_ptest 전체 표: 조건과 질문별 p값"
ws["A1"].font = f_title
info = [
    "자료: 국민건강영양조사 2019–2021, 최종 분석 대상 10,779명(남 5,411 · 여 5,368). 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu. p는 설계 기반 Wald F 검정.",
    "출처: output/21_ptest/ptest_log.csv → code/29_ptest_full_table.R → ptest_전체표_조건과p.csv → code/32_ptest_table_xlsx.py. 생성형 AI(Claude)로 정리했으며 조원 검토가 필요합니다.",
    "절단점 단위는 g/일입니다(1잔 ≈ 7 g이라 g/일 = 주당 잔수). 'a · b'는 a 미만 / a 이상 b 미만 / b 이상. 남녀 값이 다르면 성별 기준입니다.",
    "기준 16개 × 결과 2개 × 보정 묶음 6개를 함께 본 결과입니다. p < 0.05 하나를 확증으로 읽지 말고, 미리 정한 주분석 기준과 주 모형(M2)을 먼저 해석하세요.",
]
for i, s in enumerate(info, 3):
    ws.cell(row=i, column=1, value=s).font = f_base
    ws.merge_cells(start_row=i, start_column=1, end_row=i, end_column=2)
    ws.cell(row=i, column=1).alignment = Alignment(wrap_text=True, vertical="top")
    ws.row_dimensions[i].height = 30

r = 8
ws.cell(row=r, column=1, value="색과 기호").font = f_bold
r += 1
header(ws, r, ["표시", "뜻"])
legend_marks = [
    (0.023, fill_sig, f_sig, "0.000", "p < 0.05 (초록)"),
    (0.074, fill_near, f_base, "0.000", "0.05 ≤ p < 0.10 (노랑, 경계)"),
    (0.019, fill_dag, f_dag, '0.000"†"', "† 비교하는 칸에 고요산혈증 사례가 10명 미만. p가 작아도 해석하지 않음 (회색)"),
    ("무효", fill_dag, f_dag, "General", "칸이 비어 계산이 성립하지 않음"),
    (None, None, f_base, "General", "빈칸: 그 실행에서 해당 검정이 없음 (예: 연속형에는 경향성 검정이 없음)"),
]
for v, fl, fo, nf, desc in legend_marks:
    r += 1
    c = text(ws, r, 1, v, fo, fl, center)
    c.number_format = nf
    text(ws, r, 2, desc)

r += 2
ws.cell(row=r, column=1, value="각 p값이 무엇인가").font = f_bold
r += 1
header(ws, r, ["열", "무엇에 대한 p인가"])
legend = [
    ("주 남 / 주 여", "주질문. 남자(여자)에서 음주량과 결과의 관계가 지속 홍조군과 비홍조군 사이에 다른가 (음주 × 지속 홍조 상호작용)"),
    ("부1 남 / 부1 여", "부질문 1. 같은 관계가 홍조 소실군과 비홍조군 사이에 다른가 (음주 × 홍조 소실 상호작용)"),
    ("부2 성별", "부질문 2. 지속 홍조군과 비홍조군의 차이가 남녀 사이에 다른가 (음주 × 홍조 × 성별 3원 상호작용)"),
    ("부2 폐경", "부질문 2(탐색). 같은 차이가 여성의 폐경 전과 후에 다른가 (음주 × 홍조 × 폐경 3원 상호작용)"),
    ("주 경향 · 부1 경향", "주질문·부질문 1을 범주별 g/일 중앙값 점수로 본 1 df 경향성 검정 (범주형이고 경향성 검정을 한 실행만)"),
    ("부2 경향 성별", "부질문 2의 1 df 경향성 검정"),
    ("소실vs지속", "홍조 소실군과 지속 홍조군 사이의 차이 (탐색)"),
    ("탐색(홍조 체질)", "과거 또는 현재 홍조가 있는 사람 vs 한 번도 없는 사람 (계획서에 없던 탐색 분석)"),
    ("비선형성", "스플라인일 때, 음주량과 결과의 관계가 직선에서 벗어나는가"),
    ("참고 현재홍조", "계획 발표 때의 주질문: 현재 홍조군 vs 현재 비홍조군"),
]
for a, b in legend:
    r += 1
    text(ws, r, 1, a, f_bold)
    text(ws, r, 2, b)

r += 2
ws.cell(row=r, column=1, value="보정 묶음별 변수").font = f_bold
r += 1
header(ws, r, ["보정 묶음", "들어간 변수"])
seen = {}
for x in rows:
    if x["단계"] == "G" and x["보정묶음"] in BUNDLE_ORDER:
        seen.setdefault(x["보정묶음"], x["보정변수"])
for b in BUNDLE_ORDER:
    r += 1
    text(ws, r, 1, b + (" (주 모형)" if b == "M2" else ""), f_bold, fill_m2 if b == "M2" else None)
    text(ws, r, 2, seen.get(b, ""))
r += 1
ws.cell(row=r, column=1, value="폐경(여)은 여자 모형에만 들어갑니다. M3·M3S는 매개 가능 변수(BMI, 고혈압, 당뇨병, eGFR)까지 넣은 민감도 분석입니다.").font = f_note
ws.merge_cells(start_row=r, start_column=1, end_row=r, end_column=2)

r += 2
ws.cell(row=r, column=1, value="시트 안내").font = f_bold
r += 1
header(ws, r, ["시트", "내용"])
for a, b in [
    ("M2 한눈에", "최종 분석 G01–G16을 주 모형(M2)으로만 보고, 고요산혈증과 혈청 요산을 나란히 놓은 요약"),
    ("고요산혈증", "G01–G16 × 보정 묶음 6개, 결과 = 고요산혈증. 모든 p 열"),
    ("혈청 요산(연속형)", "G01–G16 × 보정 묶음 6개, 결과 = 혈청 요산(mg/dL). 모든 p 열"),
    ("1차 분석 C", "1차 분석 C01–C12 (고요산혈증, M2·M3)"),
    ("검증·시험", "스킬을 만들며 계산을 확인한 실행. 해석에 쓰지 않음"),
    ("전체(원자료)", "CSV 248줄 그대로. 필터로 원하는 조건만 골라 볼 수 있음"),
]:
    r += 1
    text(ws, r, 1, a, f_bold)
    text(ws, r, 2, b)

# ================= 2. M2 한눈에 =================
ws = wb.create_sheet("M2 한눈에")
g_m2 = [x for x in rows if x["단계"] == "G" and x["보정묶음"] == "M2"]
crits = []
for x in sorted(g_m2, key=lambda z: z["번호"]):
    if x["기준"] not in crits:
        crits.append(x["기준"])
ws["A1"] = "최종 분석 G01–G16 · 주 모형 M2 (연령, 교육, 소득, 조사연도, 흡연 2범주, 신체활동, 폐경(여))"
ws["A1"].font = f_title
fixed = ["번호", "기준", "음주량 형태", "남 절단점(g/일)", "여 절단점(g/일)"]
r0 = 3
for j, c in enumerate(fixed, 1):
    ws.merge_cells(start_row=r0, start_column=j, end_row=r0 + 1, end_column=j)
for j, (title, fill) in enumerate([("결과: 고요산혈증(예/아니오)", "305496"), ("결과: 혈청 요산(mg/dL, 연속형)", "548235")]):
    c0 = len(fixed) + 1 + j * len(CORE)
    ws.merge_cells(start_row=r0, start_column=c0, end_row=r0, end_column=c0 + len(CORE) - 1)
    x = ws.cell(row=r0, column=c0, value=title)
    x.font = f_head
    x.fill = PatternFill("solid", fgColor=fill)
    x.alignment = center
    for k, q in enumerate(CORE):
        h = ws.cell(row=r0 + 1, column=c0 + k, value=q)
        h.font = f_bold
        h.fill = fill_sub
        h.alignment = center
        h.border = border
for j, c in enumerate(fixed, 1):
    x = ws.cell(row=r0, column=j, value=c)
    x.font = f_head
    x.fill = fill_head
    x.alignment = center
    x.border = border
ws.row_dimensions[r0 + 1].height = 22
by = {(x["기준"], x["결과변수"]): x for x in g_m2}
r = r0 + 1
for i, cr in enumerate(crits):
    r += 1
    base = by.get((cr, HUA)) or by.get((cr, UA))
    band = fill_band if i % 2 else None
    text(ws, r, 1, cr[:3], f_base, band, center)
    text(ws, r, 2, crit_name(cr), f_bold, band)
    text(ws, r, 3, base["음주량형태"], f_base, band, center)
    text(ws, r, 4, base["남 절단점(g/일)"] or base["연속형 단위"], f_base, band, center)
    text(ws, r, 5, base["여 절단점(g/일)"] or base["연속형 단위"], f_base, band, center)
    for j, oc in enumerate([HUA, UA]):
        src = by.get((cr, oc), {})
        for k, q in enumerate(CORE):
            put_p(ws.cell(row=r, column=len(fixed) + 1 + j * len(CORE) + k), src.get(q, ""))
for j, w in enumerate([7, 38, 15, 20, 20] + [9.5] * 12, 1):
    ws.column_dimensions[get_column_letter(j)].width = w
ws.freeze_panes = ws.cell(row=r0 + 2, column=3)
r += 2
ws.cell(row=r, column=1, value="색: 초록 p < 0.05 · 노랑 0.05–0.10 · 회색 † 사례 10명 미만(해석하지 않음) 또는 무효. 연속형 기준은 절단점 칸에 단위를 적었습니다.").font = f_note


# ================= 3·4. 결과별 상세 =================
DETAIL_FIXED = ["번호", "기준", "음주량 형태", "남 절단점(g/일)", "여 절단점(g/일)", "연속형 단위", "보정 묶음"]


def detail_sheet(name, outcome):
    ws = wb.create_sheet(name)
    data = [x for x in rows if x["단계"] == "G" and x["결과변수"] == outcome]
    data.sort(key=lambda z: (z["번호"], bundle_key(z)))
    cols = DETAIL_FIXED + CORE + EXTRA
    # 값이 하나도 없는 p 열은 뺀다(예: 요산에는 경향성 검정 일부가 없음)
    cols = [c for c in cols if c not in P_COLS or any(x.get(c) for x in data)]
    header(ws, 1, cols, [7, 36, 15, 18, 18, 16, 10] + [9.5] * (len(cols) - 7))
    prev, band_on = None, False
    for i, x in enumerate(data, 2):
        if x["번호"] != prev:
            band_on, prev = not band_on, x["번호"]
        band = fill_band if band_on else None
        vals = [x["번호"], crit_name(x["기준"]), x["음주량형태"], x["남 절단점(g/일)"], x["여 절단점(g/일)"], x["연속형 단위"], x["보정묶음"]]
        for j, v in enumerate(vals, 1):
            fl = fill_m2 if (j == 7 and v == "M2") else band
            text(ws, i, j, v, f_bold if j in (2, 7) else f_base, fl, center if j != 2 else wrap)
        for j, c in enumerate(cols[7:], 8):
            put_p(ws.cell(row=i, column=j), x.get(c, ""))
    ws.freeze_panes = "H2"
    ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(data) + 1}"
    n = len(data) + 3
    ws.cell(row=n, column=1, value=f"결과 변수: {outcome}. 같은 기준의 보정 묶음 6줄을 같은 바탕색으로 묶었습니다. 주 모형 M2는 주황 바탕. 색 뜻은 '읽는 법' 시트.").font = f_note


detail_sheet("고요산혈증", HUA)
detail_sheet("혈청 요산(연속형)", UA)

# ================= 5. 1차 분석 C =================
ws = wb.create_sheet("1차 분석 C")
data = sorted([x for x in rows if x["단계"] == "C"], key=lambda z: (bundle_key(z), z["번호"]))
cols = DETAIL_FIXED + [c for c in CORE + EXTRA if any(x.get(c) for x in data)]
header(ws, 1, cols, [7, 40, 15, 18, 18, 16, 10] + [9.5] * (len(cols) - 7))
for i, x in enumerate(data, 2):
    band = fill_band if x["보정묶음"] == "M3" else None
    vals = [x["번호"], crit_name(x["기준"]), x["음주량형태"], x["남 절단점(g/일)"], x["여 절단점(g/일)"], x["연속형 단위"], x["보정묶음"]]
    for j, v in enumerate(vals, 1):
        text(ws, i, j, v, f_bold if j in (2, 7) else f_base, band, center if j != 2 else wrap)
    for j, c in enumerate(cols[7:], 8):
        put_p(ws.cell(row=i, column=j), x.get(c, ""))
ws.freeze_panes = "H2"
ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(data) + 1}"
ws.cell(row=len(data) + 3, column=1, value="결과 변수: 고요산혈증(예/아니오). 위 12줄 M2, 아래 12줄 M3(회색 바탕).").font = f_note

# ================= 6. 검증·시험 =================
ws = wb.create_sheet("검증·시험")
data = [x for x in rows if x["단계"] == "검증"]
vcols = ["번호", "기준", "질문구조", "음주량형태", "남 절단점(g/일)", "여 절단점(g/일)", "연속형 단위", "1잔(g)", "대상", "결과변수", "보정변수"]
cols = vcols + [c for c in CORE + EXTRA if any(x.get(c) for x in data)]
header(ws, 1, cols, [6, 30, 16, 14, 14, 14, 14, 7, 16, 18, 40] + [9.5] * (len(cols) - len(vcols)))
for i, x in enumerate(data, 2):
    for j, c in enumerate(vcols, 1):
        v = x[c]
        if c == "질문구조":
            v = v.split("(")[0]
        text(ws, i, j, v, f_base, fill_dag if x["질문구조"].startswith("개정 전") and c == "질문구조" else None, wrap)
    for j, c in enumerate(cols[len(vcols):], len(vcols) + 1):
        put_p(ws.cell(row=i, column=j), x.get(c, ""))
ws.freeze_panes = "C2"
ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(data) + 1}"
ws.cell(row=len(data) + 3, column=1, value="질문구조가 '개정 전'인 줄의 주 남·주 여는 지속 홍조가 아니라 현재 홍조군 vs 현재 비홍조군 비교의 p입니다. 해석에 쓰지 않습니다.").font = f_note

# ================= 7. 전체(원자료) =================
ws = wb.create_sheet("전체(원자료)")
cols = list(rows[0].keys())
header(ws, 1, cols, [6, 7, 34, 14, 16, 16, 14, 8, 7, 14, 20, 12, 40, 20] + [9.5] * (len(cols) - 14))
for i, x in enumerate(rows, 2):
    for j, c in enumerate(cols, 1):
        if c in P_COLS:
            put_p(ws.cell(row=i, column=j), x[c])
        else:
            text(ws, i, j, x[c], f_base, None, Alignment(vertical="top"))
ws.freeze_panes = "D2"
ws.auto_filter.ref = f"A1:{get_column_letter(len(cols))}{len(rows) + 1}"

for s in wb.worksheets:
    s.sheet_view.zoomScale = 90
wb.save(OUT)
print("저장:", OUT, "| 시트:", ", ".join(wb.sheetnames), "| 원자료 줄:", len(rows))
