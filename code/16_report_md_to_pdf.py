# 16_report_md_to_pdf.py
# 목적: Markdown 보고서를 HTML로 바꾼 뒤 Edge(또는 Chrome) 헤드리스로 PDF를 만든다.
#       Word COM 변환이 오래 걸리거나 멈출 때 쓰는 대체 경로. 외부 python 모듈 없이 동작한다.
#       처리: 제목(#, ##, ###), 글머리표(-), 번호 목록(1.), 표(| ... |), 그림(![..](..)), 굵게(**), 기울임(*), 코드(`).
# 실행: python code/16_report_md_to_pdf.py <입력.md> [<출력.pdf>]   (프로젝트 폴더에서)

import html
import os
import re
import subprocess
import sys
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")

args = sys.argv[1:]
if not args:
    sys.exit("사용법: python code/16_report_md_to_pdf.py <입력.md> [<출력.pdf>]")
MD_PATH = Path(args[0]).resolve()
PDF_PATH = Path(args[1]).resolve() if len(args) > 1 else MD_PATH.with_suffix(".pdf")
HTML_PATH = MD_PATH.with_suffix(".html")

INLINE = re.compile(r"(\*\*.+?\*\*|`[^`]+`|\*[^*\s][^*]*?\*)")


def inline(text):
    out = []
    for tok in INLINE.split(text):
        if not tok:
            continue
        if tok.startswith("**") and tok.endswith("**") and len(tok) > 4:
            out.append("<b>" + html.escape(tok[2:-2]) + "</b>")
        elif tok.startswith("`") and tok.endswith("`"):
            out.append("<code>" + html.escape(tok[1:-1]) + "</code>")
        elif tok.startswith("*") and tok.endswith("*") and len(tok) > 2:
            out.append("<i>" + html.escape(tok[1:-1]) + "</i>")
        else:
            out.append(html.escape(tok))
    return "".join(out)


def split_row(line):
    cells = line.strip().strip("|").split("|")
    return [c.replace("\\|", "|").strip() for c in cells]


lines = MD_PATH.read_text(encoding="utf-8").splitlines()
body = []
i = 0
list_open = None  # "ul" | "ol"


def close_list():
    global list_open
    if list_open:
        body.append(f"</{list_open}>")
        list_open = None


while i < len(lines):
    ln = lines[i]
    s = ln.strip()
    if not s:
        close_list(); i += 1; continue
    m = re.match(r"^(#{1,6})\s+(.*)$", s)
    if m:
        close_list(); lvl = len(m.group(1))
        body.append(f"<h{lvl}>{inline(m.group(2))}</h{lvl}>"); i += 1; continue
    m = re.match(r"^!\[(.*?)\]\((.*?)\)$", s)
    if m:
        close_list()
        src = (MD_PATH.parent / m.group(2)).resolve().as_uri()
        body.append(f'<figure><img src="{src}" alt="{html.escape(m.group(1))}"></figure>'); i += 1; continue
    if s.startswith("|"):
        close_list()
        rows = []
        while i < len(lines) and lines[i].strip().startswith("|"):
            rows.append(split_row(lines[i])); i += 1
        if len(rows) >= 2 and all(re.match(r"^:?-{3,}:?$", c) for c in rows[1]):
            header, data = rows[0], rows[2:]
        else:
            header, data = rows[0], rows[1:]
        wide = "wide" if len(header) >= 8 else ""
        t = [f'<table class="{wide}"><thead><tr>' + "".join(f"<th>{inline(c)}</th>" for c in header) + "</tr></thead><tbody>"]
        for r in data:
            r = (r + [""] * len(header))[:len(header)]
            t.append("<tr>" + "".join(f"<td>{inline(c)}</td>" for c in r) + "</tr>")
        t.append("</tbody></table>")
        body.append("".join(t)); continue
    m = re.match(r"^[-*]\s+(.*)$", s)
    if m:
        if list_open != "ul":
            close_list(); body.append("<ul>"); list_open = "ul"
        body.append(f"<li>{inline(m.group(1))}</li>"); i += 1; continue
    m = re.match(r"^\d+\.\s+(.*)$", s)
    if m:
        if list_open != "ol":
            close_list(); body.append("<ol>"); list_open = "ol"
        body.append(f"<li>{inline(m.group(1))}</li>"); i += 1; continue
    close_list()
    # 문단: 빈 줄이 나올 때까지 이어 붙인다
    para = [s]; i += 1
    while i < len(lines) and lines[i].strip() and not re.match(r"^(#{1,6}\s|[-*]\s|\d+\.\s|\||!\[)", lines[i].strip()):
        para.append(lines[i].strip()); i += 1
    body.append(f"<p>{inline(' '.join(para))}</p>")
close_list()

CSS = """
@page { size: A4; margin: 16mm 14mm; }
body { font-family: 'Malgun Gothic', '맑은 고딕', 'Segoe UI', sans-serif; font-size: 10.5pt; line-height: 1.5; color: #0b0b0b; margin: 0; }
h1 { font-size: 17pt; margin: 0 0 6pt; line-height: 1.35; }
h2 { font-size: 14pt; margin: 18pt 0 6pt; border-bottom: 1px solid #c3c2b7; padding-bottom: 3pt; page-break-after: avoid; }
h3 { font-size: 12pt; margin: 14pt 0 4pt; page-break-after: avoid; }
p { margin: 4pt 0; }
ul, ol { margin: 3pt 0 6pt; padding-left: 18pt; }
li { margin: 2pt 0; }
code { font-family: Consolas, monospace; font-size: 9.5pt; background: #f1efe8; padding: 0 2px; }
table { border-collapse: collapse; margin: 6pt 0 10pt; font-size: 8.5pt; width: 100%; page-break-inside: auto; }
table.wide { font-size: 7.6pt; }
th, td { border: 1px solid #c3c2b7; padding: 2.5pt 4pt; vertical-align: top; text-align: left; }
th { background: #f1efe8; font-weight: 600; }
tr { page-break-inside: avoid; }
thead { display: table-header-group; }
figure { margin: 8pt 0 12pt; page-break-inside: avoid; text-align: center; }
figure img { max-width: 100%; height: auto; }
"""
doc = f"<!DOCTYPE html><html lang='ko'><head><meta charset='utf-8'><title>{html.escape(MD_PATH.stem)}</title><style>{CSS}</style></head><body>{''.join(body)}</body></html>"
HTML_PATH.write_text(doc, encoding="utf-8")
print(f"HTML: {HTML_PATH}")

browsers = [r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe", r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
            r"C:\Program Files\Google\Chrome\Application\chrome.exe", r"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe"]
exe = next((b for b in browsers if os.path.exists(b)), None)
if exe is None:
    sys.exit("Edge/Chrome 을 찾지 못해 PDF 를 만들지 못했다. HTML 파일은 만들어졌다.")
cmd = [exe, "--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
       f"--user-data-dir={Path(os.environ.get('TEMP', '.')) / 'edge_pdf_profile'}",
       "--no-pdf-header-footer", f"--print-to-pdf={PDF_PATH}", HTML_PATH.as_uri()]
subprocess.run(cmd, check=False, timeout=180, capture_output=True)
if PDF_PATH.exists():
    print(f"PDF: {PDF_PATH} ({PDF_PATH.stat().st_size:,} bytes)")
else:
    sys.exit("PDF 생성 실패")
