# 06_flushing_report_docx.py
# 목적: 05_flushing_report.R 이 만든 Markdown 보고서(REPORT_안면홍조_고요산혈증.md)를
#       Word 문서(.docx)로 변환한다. 숫자는 Markdown(=결과 CSV)에서 그대로 옮긴다.
#       처리: 제목(#, ##, ###), 글머리표(-), 번호 목록(1.), 표(| ... |), 그림(![..](..)),
#             굵게(**), 기울임(*), 코드(`) 표기.
# 실행: python code/06_flushing_report_docx.py   (프로젝트 폴더에서)
# 필요: python-docx (설치 확인됨)

import re
import sys
from pathlib import Path

from docx import Document
from docx.enum.section import WD_ORIENT, WD_SECTION
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Mm, Pt, RGBColor

sys.stdout.reconfigure(encoding="utf-8")

# 인자 없이 실행하면 안면홍조 보고서를 변환한다. 다른 보고서는 경로를 인자로 준다:
#   python code/06_flushing_report_docx.py <입력.md> [<출력.docx>]
_args = sys.argv[1:]
MD_PATH = Path(_args[0]) if _args else Path("output/03_flushing_hyperuricemia/REPORT_안면홍조_고요산혈증.md")
OUT_PATH = Path(_args[1]) if len(_args) > 1 else MD_PATH.with_suffix(".docx")
IN_DIR = MD_PATH.parent   # 그림 경로(fig/...)는 보고서 파일이 있는 폴더 기준
FONT = "맑은 고딕"
HEADER_FILL = "F1EFE8"   # 표 머리글 음영(밝은 회색)


# ---- 글꼴 보조 함수: 한글(동아시아) 글꼴까지 지정해야 Word 에서 맑은 고딕으로 보인다 ----
def set_east_asian_font(rpr_parent, name=FONT):
    rpr = rpr_parent.get_or_add_rPr()
    rfonts = rpr.find(qn("w:rFonts"))
    if rfonts is None:
        rfonts = OxmlElement("w:rFonts")
        rpr.insert(0, rfonts)
    for attr in ("w:ascii", "w:hAnsi", "w:eastAsia", "w:cs"):
        rfonts.set(qn(attr), name)


def style_font(doc, style_name, size=None, bold=None, color=None):
    try:
        st = doc.styles[style_name]
    except KeyError:
        return
    st.font.name = FONT
    if size is not None:
        st.font.size = Pt(size)
    if bold is not None:
        st.font.bold = bold
    if color is not None:
        st.font.color.rgb = RGBColor.from_string(color)
    set_east_asian_font(st.element)


# ---- 인라인 서식: **굵게**, *기울임*, `코드` ----
INLINE = re.compile(r"(\*\*.+?\*\*|`[^`]+`|\*[^*\s][^*]*?\*)")


def add_inline(par, text, size=None):
    for tok in INLINE.split(text):
        if not tok:
            continue
        if tok.startswith("**") and tok.endswith("**") and len(tok) > 4:
            run = par.add_run(tok[2:-2]); run.bold = True
        elif tok.startswith("`") and tok.endswith("`"):
            run = par.add_run(tok[1:-1]); run.font.name = "Consolas"
        elif tok.startswith("*") and tok.endswith("*") and len(tok) > 2:
            run = par.add_run(tok[1:-1]); run.italic = True
        else:
            run = par.add_run(tok)
        set_east_asian_font(run._r, FONT if run.font.name != "Consolas" else "Consolas")
        if size is not None:
            run.font.size = Pt(size)
    return par


# ---- 표 ----
def split_cells(line):
    s = line.strip()
    if s.startswith("|"):
        s = s[1:]
    if s.endswith("|"):
        s = s[:-1]
    return [c.replace("\\|", "|").strip() for c in re.split(r"(?<!\\)\|", s)]


def shade_cell(cell, fill):
    tc_pr = cell._tc.get_or_add_tcPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear"); shd.set(qn("w:color"), "auto"); shd.set(qn("w:fill"), fill)
    tc_pr.append(shd)


def parse_table(lines):
    rows = [split_cells(l) for l in lines]
    return [r for r in rows if not all(re.fullmatch(r":?-{3,}:?", c) for c in r)]   # 구분선(---) 제거


CJK = re.compile(r"[ᄀ-ᇿ　-〿가-힯一-鿿＀-￯]")


def em_len(s):
    # 글자 폭 추정(em): 한글·한자는 1, 영문·숫자는 0.55, 공백은 0.3
    return sum(1.0 if CJK.match(ch) else (0.3 if ch == " " else 0.55) for ch in s)


LATIN_RUN = re.compile(r"[^\sᄀ-ᇿ　-〿가-힯一-鿿＀-￯]+")


def col_widths_mm(rows, ncol, total_mm, size_pt, cap_em=26, floor_cap_em=16, pad_mm=4.6):
    # 필요 폭 = 가장 긴 셀의 폭(cap_em 으로 제한) + 셀 안쪽 여백.
    #   Word 의 기본 셀 안쪽 여백은 좌우 각 1.9 mm(합 3.8 mm)이므로 여백은 4.6 mm 로 잡는다(0.8 mm 는 안전 여유).
    # 최소 폭 = 영문·숫자·기호가 공백 없이 이어진 가장 긴 구간의 폭(변수명·숫자가 중간에 끊기지 않게).
    #   한글은 Word 가 글자 사이에서 줄을 바꿀 수 있으므로 최소 폭 계산에서 제외한다.
    pt2mm = 0.3528
    need, floor = [], []
    for j in range(ncol):
        cells = [(r[j] if j < len(r) else "") for r in rows]
        longest = max(em_len(c) for c in cells)
        latin = max((em_len(t) for c in cells for t in LATIN_RUN.findall(c)), default=1.0)
        # 짧은 셀(7 em 이하: "남자", "지속 홍조", "현재 홍조 없음", "0.006" 등)은 한 줄에 다 들어가게 한다
        short = max((em_len(c) for c in cells if em_len(c) <= 7), default=0.0)
        # 띄어쓰기 없는 덩어리도 8 em 까지는 끊지 않는다(세미콜론으로 이어진 긴 코드 목록은 8 em 에서 자른다)
        chunk = max((em_len(t) for c in cells for t in c.split()), default=0.0)
        token = max(min(latin, floor_cap_em), short, min(chunk, 8.0))
        need.append(min(longest, cap_em) * size_pt * pt2mm + pad_mm)
        floor.append(token * size_pt * pt2mm + pad_mm)
    w = list(need)
    if sum(w) > total_mm:
        # 넘치면 최소 폭을 넘는 여유분만 비례해서 줄인다
        for _ in range(20):
            excess = sum(w) - total_mm
            if excess <= 0.01:
                break
            flex = [max(w[j] - floor[j], 0.0) for j in range(ncol)]
            if sum(flex) <= 0:
                break
            w = [max(w[j] - excess * flex[j] / sum(flex), floor[j]) for j in range(ncol)]
        if sum(w) > total_mm:   # 최소 폭 합이 쪽 폭보다 크면 어쩔 수 없이 비례 축소
            w = [x * total_mm / sum(w) for x in w]
    else:
        w = [x * total_mm / sum(w) for x in w]   # 남는 폭은 비례 배분해 표가 쪽 폭을 채우게
    return w


def add_table(doc, lines, total_mm=170):
    rows = parse_table(lines)
    if not rows:
        return
    ncol = max(len(r) for r in rows)
    size = 7 if ncol > 7 else (8 if ncol > 5 else 9)
    table = doc.add_table(rows=len(rows), cols=ncol)
    table.style = "Table Grid"
    table.alignment = WD_TABLE_ALIGNMENT.CENTER
    table.autofit = False   # 고정 레이아웃: 아래에서 정한 열 너비를 Word 가 그대로 쓴다
    widths = col_widths_mm(rows, ncol, total_mm, size)
    for j, wmm in enumerate(widths):
        table.columns[j].width = Mm(wmm)
        for row in table.rows:
            row.cells[j].width = Mm(wmm)
    for i, r in enumerate(rows):
        row = table.rows[i]
        tr_pr = row._tr.get_or_add_trPr()
        split = OxmlElement("w:cantSplit"); split.set(qn("w:val"), "true"); tr_pr.append(split)   # 한 행이 두 쪽에 걸쳐 잘리지 않게
        if i == 0:   # 머리글 행: 음영 + 굵게 + 쪽 넘김 시 반복
            hdr = OxmlElement("w:tblHeader"); hdr.set(qn("w:val"), "true"); tr_pr.append(hdr)
        for j in range(ncol):
            cell = row.cells[j]
            text = r[j] if j < len(r) else ""
            par = cell.paragraphs[0]
            par.paragraph_format.space_after = Pt(0)
            par.paragraph_format.space_before = Pt(0)
            if i == 0:
                par.paragraph_format.keep_with_next = True   # 머리글 행만 쪽 끝에 홀로 남지 않게
                run = par.add_run(text); run.bold = True; run.font.size = Pt(size)
                set_east_asian_font(run._r)
                shade_cell(cell, HEADER_FILL)
            else:
                add_inline(par, text, size=size)
    doc.add_paragraph()   # 표 뒤 여백


# ---- 번호 목록: 목록이 새로 시작할 때마다 1부터 다시 세도록 번호 인스턴스를 새로 만든다 ----
def new_numbering_instance(doc, style_name="List Number"):
    try:
        numbering = doc.part.numbering_part.element
        p_pr = doc.styles[style_name].element.pPr
        num_pr = p_pr.find(qn("w:numPr")) if p_pr is not None else None
        base_id = num_pr.find(qn("w:numId")).get(qn("w:val"))
        abs_id = None
        for num in numbering.findall(qn("w:num")):
            if num.get(qn("w:numId")) == base_id:
                abs_id = num.find(qn("w:abstractNumId")).get(qn("w:val"))
                break
        if abs_id is None:
            return None
        new_id = str(max(int(n.get(qn("w:numId"))) for n in numbering.findall(qn("w:num"))) + 1)
        num = OxmlElement("w:num"); num.set(qn("w:numId"), new_id)
        abs_el = OxmlElement("w:abstractNumId"); abs_el.set(qn("w:val"), abs_id); num.append(abs_el)
        ovr = OxmlElement("w:lvlOverride"); ovr.set(qn("w:ilvl"), "0")
        so = OxmlElement("w:startOverride"); so.set(qn("w:val"), "1"); ovr.append(so); num.append(ovr)
        numbering.append(num)
        return new_id
    except Exception:
        return None


def set_paragraph_num(par, num_id):
    p_pr = par._p.get_or_add_pPr()
    num_pr = OxmlElement("w:numPr")
    ilvl = OxmlElement("w:ilvl"); ilvl.set(qn("w:val"), "0")
    nid = OxmlElement("w:numId"); nid.set(qn("w:val"), num_id)
    num_pr.append(ilvl); num_pr.append(nid)
    p_style = p_pr.find(qn("w:pStyle"))
    p_pr.insert(list(p_pr).index(p_style) + 1 if p_style is not None else 0, num_pr)   # numPr 는 pStyle 바로 뒤


# ---- 그림 ----
IMG = re.compile(r"^!\[(.*?)\]\((.*?)\)\s*$")


def add_image(doc, alt, rel_path):
    path = IN_DIR / rel_path
    if not path.exists():
        par = doc.add_paragraph(); add_inline(par, f"(그림 파일 없음: {rel_path})"); return
    doc.add_picture(str(path), width=Mm(165))
    doc.paragraphs[-1].alignment = WD_ALIGN_PARAGRAPH.CENTER
    cap = doc.add_paragraph()
    cap.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = cap.add_run(alt); run.italic = True; run.font.size = Pt(9); set_east_asian_font(run._r)


# ---- 문서 만들기 ----
def build():
    md = MD_PATH.read_text(encoding="utf-8-sig").splitlines()
    doc = Document()

    # 용지 A4, 여백 20 mm
    for sec in doc.sections:
        sec.page_width, sec.page_height = Mm(210), Mm(297)
        sec.left_margin = sec.right_margin = sec.top_margin = sec.bottom_margin = Mm(20)

    # 문서 기본 글꼴(docDefaults)까지 맑은 고딕으로
    rpr_default = doc.styles.element.find(qn("w:docDefaults")).find(qn("w:rPrDefault"))
    if rpr_default is not None:
        rpr = rpr_default.find(qn("w:rPr"))
        rfonts = rpr.find(qn("w:rFonts"))
        if rfonts is None:
            rfonts = OxmlElement("w:rFonts"); rpr.insert(0, rfonts)
        for attr in ("w:ascii", "w:hAnsi", "w:eastAsia", "w:cs"):
            rfonts.set(qn(attr), FONT)
        for attr in ("w:asciiTheme", "w:hAnsiTheme", "w:eastAsiaTheme", "w:cstheme"):
            if rfonts.get(qn(attr)) is not None:
                del rfonts.attrib[qn(attr)]

    style_font(doc, "Normal", size=10)
    doc.styles["Normal"].paragraph_format.space_after = Pt(6)
    style_font(doc, "Title", size=18, bold=True, color="000000")
    style_font(doc, "Heading 1", size=14, bold=True, color="1F3864")
    style_font(doc, "Heading 2", size=12, bold=True, color="1F3864")
    style_font(doc, "Heading 3", size=11, bold=True, color="333333")
    for s in ("List Bullet", "List Bullet 2", "List Number"):
        style_font(doc, s, size=10)
    try:
        style_font(doc, "Table Grid", size=9)
    except Exception:
        pass

    # 사전 점검: 넓은 표(열 9개 이상, 또는 열 6개 이상이면서 40 em 이상의 긴 셀이 있음)가 들어 있는
    # 장(## 제목부터 다음 ## 제목 전까지)은 가로 쪽으로 만든다
    wide_heads = set()
    cur_head, blk = None, []
    for k, line in enumerate(md + ["## __end__"]):
        if line.startswith("## "):
            cur_head = k
        elif line.startswith("|"):
            blk.append(line)
            if k + 1 >= len(md) or not md[k + 1].startswith("|"):
                rows = parse_table(blk); blk = []
                if rows and cur_head is not None:
                    ncol = max(len(r) for r in rows)
                    longest = max(em_len(c) for r in rows for c in r)
                    if ncol >= 9 or (ncol >= 6 and longest >= 40):
                        wide_heads.add(cur_head)

    def new_section(landscape):
        sec = doc.add_section(WD_SECTION.NEW_PAGE)
        if landscape:
            sec.orientation = WD_ORIENT.LANDSCAPE
            sec.page_width, sec.page_height = Mm(297), Mm(210)
        else:
            sec.orientation = WD_ORIENT.PORTRAIT
            sec.page_width, sec.page_height = Mm(210), Mm(297)
        sec.left_margin = sec.right_margin = sec.top_margin = sec.bottom_margin = Mm(20)
        return sec

    landscape_now = False
    num_id = None            # 현재 진행 중인 번호 목록의 번호 인스턴스
    i, n = 0, len(md)
    n_tables = n_images = 0
    while i < n:
        line = md[i]
        if not line.strip():
            i += 1; continue
        if not re.match(r"^\d+\.\s", line):
            num_id = None    # 번호 목록이 끊기면 다음 목록은 1부터 다시 센다
        if line.startswith("## "):
            want = i in wide_heads
            if want != landscape_now:
                new_section(want); landscape_now = want
        if line.startswith("|"):
            block = []
            while i < n and md[i].startswith("|"):
                block.append(md[i]); i += 1
            add_table(doc, block, total_mm=257 if landscape_now else 170); n_tables += 1
            continue
        m = IMG.match(line)
        if m:
            add_image(doc, m.group(1), m.group(2)); n_images += 1; i += 1; continue
        if line.startswith("# "):
            h = doc.add_heading(level=0); add_inline(h, line[2:].strip())
            doc.core_properties.title = re.sub(r"[*`]", "", line[2:].strip())
        elif line.startswith("## "):
            h = doc.add_heading(level=1); add_inline(h, line[3:].strip())
        elif line.startswith("### "):
            h = doc.add_heading(level=2); add_inline(h, line[4:].strip())
        elif line.startswith("> "):
            # 인용 블록: 왼쪽 들여쓰기 + 왼쪽 세로줄 + 기울임
            par = doc.add_paragraph()
            par.paragraph_format.left_indent = Mm(8)
            par.paragraph_format.space_before = Pt(6); par.paragraph_format.space_after = Pt(6)
            p_pr = par._p.get_or_add_pPr()
            bdr = OxmlElement("w:pBdr"); left = OxmlElement("w:left")
            left.set(qn("w:val"), "single"); left.set(qn("w:sz"), "18"); left.set(qn("w:space"), "8"); left.set(qn("w:color"), "1F3864")
            bdr.append(left); p_pr.insert(1 if p_pr.find(qn("w:pStyle")) is not None else 0, bdr)
            add_inline(par, line[2:].strip())
            for run in par.runs:
                run.italic = True
        elif line.startswith("  - "):
            add_inline(doc.add_paragraph(style="List Bullet 2"), line[4:].strip())
        elif line.startswith("- "):
            add_inline(doc.add_paragraph(style="List Bullet"), line[2:].strip())
        elif re.match(r"^\d+\.\s", line):
            par = doc.add_paragraph(style="List Number")
            if num_id is None:
                num_id = new_numbering_instance(doc)
            if num_id is not None:
                set_paragraph_num(par, num_id)
            add_inline(par, re.sub(r"^\d+\.\s", "", line).strip())
        else:
            add_inline(doc.add_paragraph(), line.strip())
        i += 1

    # 문서가 표로 끝나면 Word 는 표 뒤에 문단 하나를 두는데, 표가 쪽 끝에서 끝나면 그 문단이 빈 쪽을 만든다.
    # 끝의 빈 문단은 하나만 남기고, 그것도 숨김·1pt·간격 0으로 두어 자리를 차지하지 않게 한다.
    body = doc.element.body
    while len(doc.paragraphs) >= 2 and not doc.paragraphs[-1].text.strip() and not doc.paragraphs[-2].text.strip():
        body.remove(doc.paragraphs[-1]._p)
    last = doc.paragraphs[-1]
    if not last.text.strip():
        last.paragraph_format.space_before = Pt(0); last.paragraph_format.space_after = Pt(0)
        last.paragraph_format.line_spacing = Pt(1)
        p_pr = last._p.get_or_add_pPr()
        r_pr = p_pr.find(qn("w:rPr"))                      # 문단 기호의 글꼴 속성(CT_PPr 에는 생성 메서드가 없어 직접 만든다)
        if r_pr is None:
            r_pr = OxmlElement("w:rPr"); p_pr.append(r_pr)
        r_pr.append(OxmlElement("w:vanish"))                                          # 숨김 문단 표시
        sz = OxmlElement("w:sz"); sz.set(qn("w:val"), "2"); r_pr.append(sz)          # 1pt

    doc.core_properties.author = "KNHANES 분석팀"
    doc.save(OUT_PATH)
    print(f"저장: {OUT_PATH}  (표 {n_tables}개, 그림 {n_images}개, 문단 {len(doc.paragraphs)}개)")


if __name__ == "__main__":
    build()
