# 41_paper_build.py
# 목적: 확정 조건의 최종 분석 결과로 논문 형식 보고서(완성본)를 Word(.docx)와 PDF로 만든다.
# 바탕: 조원 초안(output/최종발표/논문초안_홍조_음주_고요산혈증.pdf, 2026-11-25, 이예은),
#       초안 틀·고려사항(Result/2026-11-25_논문초안_틀.md, Result/2026-11-25_논문보고서_고려사항.md)과 그 점검 결과.
#   - 확정 범위(민감도 4개) 밖이고 코드가 저장소에 없는 추가 분석(조원 초안 표 8)과 '별도 코드 재계산' 문장은 넣지 않았다.
#   - 표와 본문의 숫자는 모두 output/final_analysis/ 결과 파일에서 읽어 넣는다(손으로 옮겨 적지 않음).
# 입력: output/final_analysis/ 01·02a·03·05·06·10·11·12·13, _reproducibility.txt, 그림(fig1, fig4, fig5),
#       Result/2026-11-18_ptest_전체표_조건과p.csv(부록: 분석 조건을 정하기 전에 검토한 음주 기준)
# 출력: output/최종발표/논문_완성본_홍조_음주_고요산혈증.docx, .pdf
# 실행: python code/41_paper_build.py   (final_analysis.R, 39_main_report.R를 먼저 실행. PDF는 Edge·Chrome 필요)
# 생성형 AI(Claude) 도움으로 작성. 빨간 글씨는 조원 확인이 필요한 곳이다.

import csv
import re
import subprocess
import sys
import tempfile
import time
from html import escape
from pathlib import Path

from docx import Document
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt, RGBColor

sys.stdout.reconfigure(encoding="utf-8")
FA = Path("output/final_analysis")
OUTD = Path("output/최종발표")
BASE = "논문_완성본_홍조_음주_고요산혈증"
OUT_DOCX, OUT_PDF = (OUTD / f"{BASE}.docx").resolve(), (OUTD / f"{BASE}.pdf").resolve()


def rd(p):
    with open(p, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


flow, vdef, t1, inc = rd(FA / "02a_participant_flow.csv"), rd(FA / "01_variable_definitions.csv"), rd(FA / "03_table1.csv"), rd(FA / "05_included_vs_excluded.csv")
off, tst, eff, cel, fo = rd(FA / "06_official_check.csv"), rd(FA / "10_interaction_tests.csv"), rd(FA / "11_effects_by_flush.csv"), rd(FA / "12_cell_counts.csv"), rd(FA / "13_outcome_by_flush.csv")
gcrit = rd(Path("Result/2026-11-18_ptest_전체표_조건과p.csv"))
repro = (FA / "_reproducibility.txt").read_text(encoding="utf-8") if (FA / "_reproducibility.txt").exists() else ""
repro_res = next((l.split(":", 1)[1].strip() for l in repro.splitlines() if l.startswith("result:")), "확인 안 함")

A_MAIN, A_UA, A_CR, A_CRU, A_M3, A_M3U = ("주분석", "보조: 혈청 요산(다중회귀)", "비교: 보정 없음", "비교(혈청 요산): 보정 없음",
                                          "추가 보정: M3S", "추가 보정(혈청 요산): M3S")
A_SENS = ["민감도: 1잔 8 g", "민감도: 1잔 10 g", "민감도: 대푯값 전체 하한(L)", "민감도: 대푯값 전체 상한(U)"]
FL = ["비홍조", "홍조 소실", "지속 홍조"]
SX = ["남자", "여자"]
SXS = {"남자": "남", "여자": "여"}


# ---------------- 값 꺼내기 ----------------
def qk(q):
    return "주질문" if q.startswith("주질문") else "부질문" if q.startswith("부질문 1:") else "탐색" if "탐색" in q else q


def trow(an, q, s):
    return next(x for x in tst if x["분석"] == an and qk(x["질문"]) == q and x["대상"] == s)


def fp(p):
    p = float(p)
    return "<0.001" if p < 0.001 else f"{p:.3f}"


def pt(an, q, s, mark=True):
    r = trow(an, q, s)
    t = fp(r["p"])
    if "10명" in r["비고"]:
        return t + " †"
    return t + ("*" if mark and float(r["p"]) < 0.05 else "")


def erow(an, s, g, kind="고음주"):
    if kind == "고음주":
        return next(x for x in eff if x["분석"] == an and x["성별"] == s and x["홍조이력"] == g and x["음주"] == "고음주")
    return next(x for x in eff if x["분석"] == an and x["성별"] == s and x["홍조이력"] == f"{g} ÷ 비홍조" and kind in x["음주"])


def ci(an, s, g, kind="고음주", sep="–", dag=True):
    r = erow(an, s, g, kind)
    f = lambda v: f"{float(v):.2f}".replace("-", "−")
    t = f"{f(r['추정치'])} ({f(r['CI하한'])}{sep}{f(r['CI상한'])})"
    return t + (" †" if dag and "10명" in r["비고"] else "")


def est(an, s, g, kind="고음주"):
    return float(erow(an, s, g, kind)["추정치"])


def n0(x):
    return f"{int(float(x)):,}"


def t1v(s, g, var, lev):
    r = next((x for x in t1 if x["성별"] == s and x["홍조패턴"] == g and x["변수"] == var and x["수준"] == lev), None)
    if not r:
        return "-"
    if lev == "평균":
        return r["추정치"]
    return f'{n0(r["n"])} ({r["추정치"].split(" ")[0]})'


def t1n(s, g):
    return next(x["집단n"] for x in t1 if x["성별"] == s and x["홍조패턴"] == g)


def fo_(s, g):
    return next(x for x in fo if x["성별"] == s and x["홍조이력"] == g)


def cell(s, g, lvl):
    return next(x for x in cel if x["분석"] == A_MAIN and x["성별"] == s and x["홍조이력"] == g and x["음주"] == lvl)


def modn(s):
    return int(trow(A_MAIN, "주질문", s)["모형n"])


fl_ = {f["단계"]: f for f in flow}
fin = flow[-1]
hua = {s: next(x for x in fo if x["성별"] == s) for s in SX}
oc = rd(FA / "04_outcome_by_sex.csv")
ocs = {x["성별"]: x for x in oc}
new_flush = {s: next(x["집단n"] for x in t1 if x["성별"] == s and x["홍조패턴"] == "신규 홍조") for s in SX}
sens_sub_p = [float(trow(a, "부질문", "남자")["p"]) for a in A_SENS]
sens_ratio_lo = [float(erow(a, "남자", "홍조 소실", "OR의 비")["CI하한"]) for a in A_SENS]
excl_n = int(next(x["집단n"] for x in inc if x["집단"] == "제외"))
sens_ratio = [float(erow(a, "남자", "홍조 소실", "OR의 비")["추정치"]) for a in A_SENS]
SR = f"{min(sens_ratio):.2f}–{max(sens_ratio):.2f}"


def incv(grp, var, lev):
    r = next(x for x in inc if x["집단"] == grp and x["변수"] == var and x["수준"] == lev)
    return r["추정치"]


# ---------------- 참고문헌(처음 인용된 순서로 번호) ----------------
REFS = {
    "faller": "Faller J, Fox IH. Ethanol-induced hyperuricemia: evidence for increased urate production by activation of adenine nucleotide turnover. N Engl J Med. 1982;307(26):1598–1602.",
    "choi2004": "Choi HK, Atkinson K, Karlson EW, Willett W, Curhan G. Alcohol intake and risk of incident gout in men: a prospective study. Lancet. 2004;363(9417):1277–1281.",
    "nakamura": "Nakamura K, Sakurai M, Miura K, Morikawa Y, Yoshita K, Ishizaki M, et al. Alcohol intake and the risk of hyperuricaemia: a 6-year prospective study in Japanese men. Nutr Metab Cardiovasc Dis. 2012;22(11):989–996.",
    "ma2025": "Ma W, Ye G, Liu Y, Sun W, Huang X, Hu L, et al. Impact of alcohol consumption on hyperuricemia and gout: a systematic review and meta-analysis. Front Nutr. 2025;12:1588980.",
    "jee2019": "Jee YH, Jung KJ, Park YB, Spiller W, Jee SH. Causal effect of alcohol consumption on hyperuricemia using a Mendelian randomization design. Int J Rheum Dis. 2019;22(10):1912–1919.",
    "brooks": "Brooks PJ, Enoch MA, Goldman D, Li TK, Yokoyama A. The alcohol flushing response: an unrecognized risk factor for esophageal cancer from alcohol consumption. PLoS Med. 2009;6(3):e50.",
    "shin2018": "Shin CM, Kim N, Cho SI, Sung J, Lee HJ. Validation of alcohol flushing questionnaires in determining inactive aldehyde dehydrogenase-2 and its clinical implication in alcohol-related diseases. Alcohol Clin Exp Res. 2018;42(2):387–396.",
    "kang2023": "Kang DE, Oh SN. Association between alcohol consumption and metabolic dysfunction-associated steatotic liver disease based on alcohol flushing response in men: the Korea National Health and Nutrition Examination Survey 2019–2021. Nutrients. 2023;15(18):3901.",
    "kim2015": "김어진, 김종성, 어원철, 최수영, 이선경, 정복선. 음주연관 안면홍조에 따른 혈중 요산과 호모시스테인 농도관계의 변화. Korean J Health Promot. 2015;15(3):91–97.",
    "kim2016": "김어진. 음주량에 따른 고요산혈증의 발생에서 알코올 연관 안면 홍조의 역할[박사학위논문]. 대전: 충남대학교 대학원; 2016.",
    "yokoyama": "Yokoyama A, Yokoyama T, Mizukami T, Matsui T, Kimura M, Matsushita S, et al. Alcohol dehydrogenase-1B (rs1229984) and aldehyde dehydrogenase-2 (rs671) genotypes and alcoholic ketosis are associated with the serum uric acid level in Japanese alcoholic men. Alcohol Alcohol. 2016;51(3):268–274.",
    "jeon2022": "Jeon S, Kang H, Cho I, Cho SI. The alcohol flushing response is associated with the risk of depression. Sci Rep. 2022;12(1):12569.",
    "park2022": "Park YS, Kang SH, Park EC, Jang SY. Association between changes in facial flushing and hypertension across drinking behavior patterns in South Korean adults. J Clin Hypertens. 2022;24(5):611–620.",
    "lee2019": "Lee S, Kim JS, Jung JG, Oh MK, Chung TH, Kim J. Korean alcohol guidelines for moderate drinking based on facial flushing. Korean J Fam Med. 2019;40(4):204–211.",
    "guide": "질병관리청. 국민건강영양조사 제8기(2019–2021) 원시자료 이용지침서. 청주: 질병관리청.",
    "yook2024": "Yook HJ, Lee GN, Lee JH, Han K, Park YM. Epidemiologic relationship between alcohol flushing and smoking in the Korean population: the Korea National Health and Nutrition Examination Survey. Sci Rep. 2024;14(1):15710.",
    "bae2023": "Bae J, Park KY, Son S, Huh Y, Nam GE. Associations between obesity parameters and hyperuricemia by sex, age, and diabetes mellitus: a nationwide study in Korea. Obes Res Clin Pract. 2023;17(5):405–410.",
    "vanderweele": "VanderWeele TJ. Principles of confounder selection. Eur J Epidemiol. 2019;34(3):211–219.",
    "kimchoe": "Kim SK, Choe JY. Association between smoking and serum uric acid in Korean population: data from the seventh Korea National Health and Nutrition Examination Survey 2016. Medicine (Baltimore). 2019;98(7):e14507.",
    "hak": "Hak AE, Choi HK. Menopause, postmenopausal hormone use and serum uric acid levels in US women—the Third National Health and Nutrition Examination Survey. Arthritis Res Ther. 2008;10(5):R116.",
    "levey": "Levey AS, Stevens LA, Schmid CH, Zhang YL, Castro AF 3rd, Feldman HI, et al. A new equation to estimate glomerular filtration rate. Ann Intern Med. 2009;150(9):604–612.",
    "brenner": "Brenner H, Blettner M. Controlling for continuous confounders in epidemiologic research. Epidemiology. 1997;8(4):429–434.",
    "lee2023": "Lee S, Kim J, Kim JS. Current status of Korean alcohol drinking in accordance with the Korean alcohol guidelines for moderate drinking based on facial flushing. Korean J Fam Med. 2023;44(3):129–142.",
    "knol": "Knol MJ, VanderWeele TJ. Recommendations for presenting analyses of effect modification and interaction. Int J Epidemiol. 2012;41(2):514–520.",
}
ORDER = []


def R(*keys):
    for k in keys:
        if k not in REFS:
            raise KeyError(k)
        if k not in ORDER:
            ORDER.append(k)
    nums = sorted(ORDER.index(k) + 1 for k in keys)
    parts, i = [], 0
    while i < len(nums):                       # 연속 번호는 1–3처럼 묶는다
        j = i
        while j + 1 < len(nums) and nums[j + 1] == nums[j] + 1:
            j += 1
        parts.append(f"{nums[i]}–{nums[j]}" if j - i >= 2 else ", ".join(str(x) for x in nums[i:j + 1]))
        i = j + 1
    return "[" + ", ".join(parts) + "]"


def G(pdf_p, print_p):
    return f"{R('guide')[:-1]}, PDF p.{pdf_p} / 인쇄 p.{print_p}]"


# ---------------- 문서 블록 ----------------
B = []                                           # (종류, ...) 목록. HTML과 Word가 같은 블록에서 만들어진다


def H1(t): B.append(("h1", t))
def H2(t): B.append(("h2", t))
def P(t, cls=""): B.append(("p", t, cls))
def UL(items): B.append(("ul", items))
def TAB(cap, head, rows, note="", widths=None, small=False): B.append(("table", cap, head, rows, note, widths, small))
def FIG(path, cap, w=15.5): B.append(("fig", str(Path(path).resolve()), cap, w))
def BRK(): B.append(("br",))


nm, nf = modn("남자"), modn("여자")
r_main_m = ci(A_MAIN, "남자", "지속 홍조", "OR의 비")
r_sub_m = ci(A_MAIN, "남자", "홍조 소실", "OR의 비")
r_main_f = ci(A_MAIN, "여자", "지속 홍조", "OR의 비")

# ===== 표제 =====
B.append(("title", "한국 성인 음주자에서 음주 관련 안면홍조 이력에 따른 음주량과 고요산혈증의 연관성: 국민건강영양조사 2019–2021 단면연구",
          "Alcohol Consumption and Hyperuricemia by Alcohol-Related Facial Flushing History among Korean Adult Drinkers: A Cross-Sectional Study of the Korea National Health and Nutrition Examination Survey 2019–2021",
          "과학적 사고와 의학연구 6조: 김민상, 조용찬, 이예은, 박채완, 김정윤, 김준현 · 지도교수: 손민국",
          "보고서 완성본 · 2026-11-25 · 빨간 글씨는 조원 확인이 필요한 곳"))

# ===== 초록 =====
H1("초록")
P(f"<b>배경:</b> 음주는 고요산혈증의 위험요인이다. 음주 후 안면홍조는 알데히드 탈수소효소 2(ALDH2)의 활성 저하를 반영하며, 홍조가 있는 사람에서 음주의 해로움이 더 크다는 보고가 있다. 그러나 과거와 현재의 홍조 이력에 따라 음주와 고요산혈증의 연관성이 달라지는지는 전국 대표 자료로 확인되지 않았다.")
P(f"<b>방법:</b> 국민건강영양조사 2019–2021년 자료에서 만 19세 이상 최근 1년 음주자 가운데 통풍 의사진단자와 진단 여부 무응답자, 보정변수 결측자를 제외한 {n0(fin['n'])}명(남 {n0(fin['남'])}명, 여 {n0(fin['여'])}명)을 분석하였다. 하루 알코올 섭취량이 남자 16 g, 여자 8 g 이상이면 고음주로 정의하였고, 홍조 이력은 비홍조, 홍조 소실, 지속 홍조로 나누었다. 고요산혈증은 혈청 요산 남자 7.0 mg/dL, 여자 6.0 mg/dL 이상으로 정의하였다. 남녀 각각 복합표본 로지스틱 회귀로 음주 × 홍조 이력 교호작용을 검정하고, 효과수정의 크기를 오즈비(OR)의 비로 제시하였다.")
P(f"<b>결과:</b> 남자 비홍조군에서 고음주의 고요산혈증 OR은 {ci(A_MAIN, '남자', '비홍조')}였고, 지속 홍조군은 {ci(A_MAIN, '남자', '지속 홍조')}, 홍조 소실군은 {ci(A_MAIN, '남자', '홍조 소실')}이었다. 지속 홍조군과 비홍조군의 OR의 비는 {r_main_m}로 차이가 확인되지 않았고(p = {fp(trow(A_MAIN, '주질문', '남자')['p'])}), 홍조 소실군과 비홍조군의 OR의 비는 {r_sub_m}였다(p = {fp(trow(A_MAIN, '부질문', '남자')['p'])}). 여자는 비홍조군에서도 고음주와 고요산혈증이 연관되었고(OR {ci(A_MAIN, '여자', '비홍조')}), 지속 홍조군과의 차이는 없었다(OR의 비 {r_main_f}, p = {fp(trow(A_MAIN, '주질문', '여자')['p'])}). 혈청 요산 분석에서는 남녀 모두 홍조 이력에 따른 차이가 없었다. 민감도 분석에서 남자 홍조 소실군의 OR의 비는 {SR}로 주분석과 비슷하였으나 p는 {min(sens_sub_p):.3f}–{max(sens_sub_p):.3f}로 경계 근처였다.")
P("<b>결론:</b> 지속 홍조가 음주와 고요산혈증의 연관을 강화한다는 근거는 확인되지 않았다. 남자 홍조 소실군에서 연관이 더 컸으나 탐색적 결과로, 유전형 자료와 종단 자료를 이용한 후속 연구로 확인해야 한다.")
P("<b>주제어:</b> 안면홍조, 알데히드 탈수소효소 2, 음주, 고요산혈증, 효과수정, 국민건강영양조사")

# ===== 1. 서론 =====
H1("1. 서론")
P(f"고요산혈증은 통풍의 직접적인 원인이며, 음주는 잘 알려진 위험요인이다. 알코올을 분해하는 과정에서 아데닌 뉴클레오타이드의 소모가 늘어 요산 생성이 증가한다{R('faller')}. 전향 연구와 메타분석에서 음주량이 많을수록 통풍과 고요산혈증의 위험이 커졌고{R('choi2004', 'nakamura', 'ma2025')}, 한국인 바이오뱅크 자료를 이용한 멘델 무작위 분석에서도 남성에서 음주와 고요산혈증의 인과 관계를 시사하는 근거가 제시되었다{R('jee2019')}.")
P(f"알코올은 알코올 탈수소효소에 의해 독성 중간 대사물인 아세트알데히드로 바뀌고, ALDH2에 의해 아세트산으로 분해된다. 동아시아인의 상당수는 ALDH2 활성이 낮아 소량의 음주에도 아세트알데히드가 쌓여 얼굴이 붉어진다(안면홍조){R('brooks')}. 간단한 홍조 설문은 한국인에서 비활성 ALDH2 보유자를 가려내는 데 쓸 수 있을 만큼 타당도가 보고되었다{R('shin2018')}. 홍조가 있는 사람은 같은 양을 마셔도 식도암 위험이 더 크고{R('brooks')}, 일부 대사 질환과의 연관도 홍조에 따라 다를 수 있다는 보고가 있다{R('kang2023')}.")
P(f"고요산혈증에서도 같은 양상이 나타나는지에 대한 근거는 제한적이다. 대학병원 건강검진 남성을 대상으로 한 연구에서 홍조군은 비홍조군보다 덜 마시는데도 혈중 요산이 비슷하였고{R('kim2015')}, 적은 음주에서도 고요산혈증 위험이 높았다{R('kim2016')}. 그러나 이 연구들은 한 병원의 남성만을 대상으로 하였다. 반대로 일본인 알코올의존 남성에서는 ALDH2 활성이 정상인 사람의 혈청 요산이 더 높았다는 보고도 있어{R('yokoyama')}, 효과의 방향조차 분명하지 않다.")
P(f"또한 홍조는 고정된 특성이 아니다. 처음 술을 마시던 시기에는 붉어졌지만 지금은 붉어지지 않는 사람(홍조 소실)이 있으며, 과거와 현재의 홍조를 나누어 본 연구에서 홍조의 변화 양상에 따라 우울, 고혈압과의 연관이 달랐다{R('jeon2022', 'park2022')}. 국민건강영양조사는 과거와 현재의 홍조를 따로 묻고 혈청 요산을 실제로 측정하므로, 홍조 이력에 따른 차이를 전국 대표 표본에서 살펴볼 수 있다.")
P("이에 본 연구는 국민건강영양조사 2019–2021년 자료를 이용하여, 남녀 각각에서 음주 수준과 고요산혈증의 연관성이 홍조 이력에 따라 다른지 확인하고자 하였다. 주질문은 \"음주 수준과 고요산혈증의 관계가 지속 홍조군과 비홍조군 사이에 다른가?\"이고, 부질문은 \"현재 홍조가 없는 사람 가운데 홍조 소실군과 비홍조군 사이에 다른가?\"이다.")

# ===== 2. 방법 =====
H1("2. 연구 방법")
H2("2.1 자료와 연구 설계")
P(f"국민건강영양조사 제8기 2019·2020·2021년 기본 원시자료(hn19_all, hn20_all, hn21_all)를 합쳐 사용한 단면연구이다. 복합표본설계를 반영하기 위해 건강설문–검진조사 가중치(wt_itvex), 분산추정 층(kstrata), 집락(psu)을 사용하였다{G('91–92', '83–84')}. 제8기는 각 연도의 조사기간에 비례해 통합하며, 세 해 모두 조사기간이 1년이므로 각 연도 가중치에 1/3을 곱하였다{G(36, 28)}. 대상자가 아닌 사람의 행을 지우지 않고 전체 자료로 설계를 만든 뒤 대상자만 부분집단으로 지정하여 분산을 추정하였다.")
H2("2.2 연구 대상")
P(f"만 19세 이상 최근 1년 음주자 가운데 음주량을 계산할 수 있고, 과거·현재 홍조 문항에 응답했으며, 혈청 요산을 측정한 사람을 대상으로 하였다. 통풍 의사진단자와 진단 여부를 모르거나 응답하지 않은 사람은 치료로 요산 수치가 달라질 수 있고 요산강하제 복용 정보가 없어 제외하였다. 보정변수에 결측이 있는 사람을 제외한 최종 대상자는 {n0(fin['n'])}명(남 {n0(fin['남'])}명, 여 {n0(fin['여'])}명)이었다(그림 1). 홍조 이력 분석에서는 세 군에 속하지 않는 신규 홍조(남 {n0(new_flush['남자'])}명, 여 {n0(new_flush['여자'])}명)와 폐경 상태가 결측인 여자 2명을 제외하여 남자 {nm:,}명, 여자 {nf:,}명을 분석하였다. 혈청 요산이 \"1.0 mg/dL 미만\"으로 표기된 경우는 측정값이 없는 것으로 보았고, 임신·수유 중인 여성 4명은 포함하여 폐경 전으로 분류하였다.")
FIG("Result/2026-11-23_주분석_그림/fig5_대상자_선정_흐름도.png", "그림 1. 연구 대상자 선정 과정. n은 가중치를 적용하지 않은 표본 수.", 13.5)
H2("2.3 변수")
P(f"<b>음주 수준(노출).</b> 하루 알코올 섭취량(g/일)은 1년간 음주빈도(BD1_11)의 대푯값 × 한 번에 마시는 잔 수(BD2_1)의 대푯값 × 7 g ÷ 7일로 계산하였다{G('161–162', '153–154')}. 빈도의 대푯값은 월 1회 미만 0.5회/월, 월 1회 1회/월, 월 2–4회 3회/월, 주 2–3회 2.5회/주, 주 4회 이상 5.5회/주로, 잔 수의 대푯값은 1–2잔 1.5잔, 3–4잔 3.5잔, 5–6잔 5.5잔, 7–9잔 8잔으로 두었고, 10잔 이상은 직접 적은 잔 수(BD2_14)를 사용하였다. 지침서에는 1잔의 알코올 양이 없어 선행연구의 값인 7 g을 가정하였다{R('jeon2022')}. 하루 섭취량이 남자 16 g 이상, 여자 8 g 이상이면 고음주, 그 미만이면 저음주(기준)로 정의하였다. 이 절단점은 한국인 홍조 기반 절주 지침의 65세 이하 비홍조군 적정 음주 상한(남자 주 8잔, 여자 주 4잔, 1잔 14 g)을 하루 양으로 바꾼 값이다{R('lee2019')}.")
P(f"<b>홍조 이력(효과수정인자).</b> 처음 술을 마시던 1–2년 동안의 홍조(BD9_1: 예, 아니오)와 현재의 홍조(BD9_2: 아니오, 가끔, 자주, 늘)를 조합하였다{G('163–164', '155–156')}. 과거와 현재 모두 없으면 비홍조(기준), 과거에는 있었으나 현재 없으면 홍조 소실, 과거에 있었고 현재 가끔·자주·늘 있으면 지속 홍조로 분류하였다. 과거에는 없었으나 현재 있는 신규 홍조는 세 군 어디에도 속하지 않아 분석에서 제외하였다{R('yook2024')}.")
P(f"<b>결과 변수.</b> 주결과는 고요산혈증으로, 혈청 요산(HE_Uacid)이 남자 7.0 mg/dL 이상, 여자 6.0 mg/dL 이상인 경우로 정의하였다{G(200, 192)}. 지침서에는 고요산혈증 기준이 없어 국민건강영양조사를 이용한 선행연구의 기준을 따랐다{R('bae2023')}. 보조 결과로 혈청 요산(mg/dL, 연속형)을 분석하였다.")
P(f"<b>보정변수.</b> 보정변수는 교란변수 선정 원칙에 따라 음주 또는 고요산혈증과 관련된 변수로 정하였다{R('vanderweele')}. 주 모형(Model 2)은 연령(연속형), 교육수준(4범주), 소득(성·연령별 가구균등화소득 4분위), 조사연도, 흡연(비흡연·과거흡연·현재흡연){R('kimchoe')}, 유산소 신체활동 실천 여부를 보정하였고, 여자는 폐경 상태(폐경 전·폐경 후·기타)를 추가로 보정하였다{R('hak')}. 흡연은 평생 5갑 이상 피웠고 현재 매일 또는 가끔 피우면 현재흡연, 평생 5갑 이상 피웠으나 현재 피우지 않으면 과거흡연, 그 밖은 비흡연으로 분류하였다{G(175, 167)}. 체질량지수, 고혈압, 당뇨병, 추정사구체여과율(eGFR, CKD-EPI 2009식{R('levey')})은 음주와 고요산혈증 사이의 매개변수일 수 있어 추가 보정 모형(Model 3)에만 넣었다. 연속형 변수는 잔여 교란을 줄이기 위해 범주로 나누지 않았다{R('brenner')}. 모름·무응답(9, 99, 999)은 결측으로, 비해당(8, 88, 888)은 변수마다 지침서에서 의미를 확인해 처리하였다(부록 표 1).")
H2("2.4 통계 분석")
P("남녀를 나누어 분석하였다. 홍조 이력별 특성은 가중치를 적용하지 않은 표본 수(n)와 가중 % 또는 가중 평균(표준오차)으로 제시하였다. 주분석은 복합표본 로지스틱 회귀(quasibinomial)로 고요산혈증 ~ 음주 수준 + 홍조 이력 + 음주 수준 × 홍조 이력 + 보정변수의 모형을 적합하고, 각 홍조군 안에서 저음주 대비 고음주의 OR과 95% 신뢰구간(CI)을 구하였다.")
P("효과수정은 곱셈 척도의 교호작용 항에 대한 Wald 검정(1 자유도)으로 평가하였다. 주질문은 음주 × 지속 홍조 항, 부질문은 음주 × 홍조 소실 항이며, 효과 크기는 교호작용 항의 지수값, 곧 OR의 비(홍조군 OR ÷ 비홍조군 OR)와 95% CI로 제시하였다. OR의 비가 1보다 크면 그 홍조군에서 음주와 고요산혈증의 연관이 더 크다는 뜻이다. 홍조 소실군과 지속 홍조군의 비교는 탐색적으로만 보았다.")
P("보정 모형은 보정하지 않은 모형(Crude), 주 모형(Model 2), Model 3의 세 가지를 사용하였고, 결론은 주 모형으로 내렸다. 혈청 요산은 같은 구조의 복합표본 다중 선형회귀로 분석하였다. 유의수준은 양측 0.05로 하였다. 비교하는 칸 가운데 고요산혈증 사례가 10명 미만인 칸이 있으면 그 OR과 검정은 해석하지 않았다(†). 보정변수를 모두 응답한 사람만 분석하였다(완전 응답자 분석). 분석에는 R 4.6.0과 survey 패키지를 사용하였다.")
H2("2.5 민감도 분석")
P("주분석의 가정을 바꾸어 결과의 방향과 크기가 유지되는지 확인하였다. (1) 1잔의 알코올을 8 g{0}, (2) 10 g으로 바꾸었고, 음주 대푯값을 (3) 모두 범주 하한(예: 주 2–3회 → 주 2회, 1–2잔 → 1잔), (4) 모두 범주 상한(예: 주 4회 이상 → 주 7회, 1–2잔 → 2잔)으로 바꾸었다. 10잔 이상은 직접 적은 잔 수라 바꾸지 않았다. 판단 기준은 p < 0.05의 유지 여부가 아니라 OR의 비의 방향과 크기가 주분석과 같은지로 두었다.".replace("{0}", R("lee2023")))
H2("2.6 분석 조건의 결정과 검증")
P("음주 기준, 1잔의 알코올 양, 흡연 범주, 보정 모형, 민감도 분석 목록은 여러 음주 기준과 보정 묶음의 교호작용 결과를 검토한 뒤(2026년 11월 16–23일) 확정하였다. 검토한 기준과 그 결과는 부록 표 3에 모두 제시하였다. 분석의 정확성을 점검하기 위해 같은 코드로 2019년 만 19세 이상 월간음주율과 고위험음주율을 재현하여 국가 공식 통계와 비교하였고(부록 표 4), 새 R 세션에서 분석을 두 번 실행해 결과 파일이 같은지 확인하였다. 주분석의 p 값은 별도의 분석 도구로 다시 계산해 같음을 확인하였다.")
H2("2.7 연구 윤리")
P("본 연구는 질병관리청이 공개한 국민건강영양조사 원시자료를 이용한 이차 자료 분석으로, 개인을 식별할 수 없는 자료를 사용하였다. <span class='red'>[기관생명윤리위원회 심의 면제 여부와 근거 문구: 과제 안내와 질병관리청 원시자료 이용 안내를 확인해 기재]</span>")

# ===== 3. 결과 =====
H1("3. 결과")
H2("3.1 대상자 특성")
m_nf, m_lf, m_pf = fo_("남자", "비홍조"), fo_("남자", "홍조 소실"), fo_("남자", "지속 홍조")
f_nf, f_pf = fo_("여자", "비홍조"), fo_("여자", "지속 홍조")
P(f"홍조 이력 분석 대상은 남자 {nm:,}명(비홍조 {n0(m_nf['n'])}명, 홍조 소실 {n0(m_lf['n'])}명, 지속 홍조 {n0(m_pf['n'])}명), 여자 {nf:,}명(비홍조 {n0(f_nf['n'])}명, 홍조 소실 {n0(fo_('여자', '홍조 소실')['n'])}명, 지속 홍조 {n0(f_pf['n'])}명)이었다(표 1). 홍조 소실군은 남녀 모두 나이가 가장 많았다. 지속 홍조군의 하루 음주량은 비홍조군의 절반 이하였고(남 {float(m_pf['음주_g일_가중평균']):.1f} vs {float(m_nf['음주_g일_가중평균']):.1f} g/일, 여 {float(f_pf['음주_g일_가중평균']):.1f} vs {float(f_nf['음주_g일_가중평균']):.1f} g/일), 고음주 비율도 낮았다. 남자 홍조 소실군은 하루 음주량({float(m_lf['음주_g일_가중평균']):.1f} g/일)과 고음주 비율({float(m_lf['고음주_가중pct']):.1f}%)이 가장 높았다. 고요산혈증은 남자 {n0(ocs['남자']['고요산혈증_n'])}명(가중 {float(ocs['남자']['고요산혈증_가중pct']):.1f}%), 여자 {n0(ocs['여자']['고요산혈증_n'])}명({float(ocs['여자']['고요산혈증_가중pct']):.1f}%)이었다.")
heads = ["특성"] + [f"{SXS[s]} · {g}" for s in SX for g in FL]
rows = [["n"] + [n0(t1n(s, g)) for s in SX for g in FL]]
spec = [("나이(세), 평균", "나이(세)", "평균"), ("교육: 대졸 이상", "교육수준", "대졸이상"), ("소득: 하", "소득 4분위", "하"), ("소득: 상", "소득 4분위", "상"),
        ("흡연: 과거흡연", "흡연 3범주", "과거흡연"), ("흡연: 현재흡연", "흡연 3범주", "현재흡연"), ("유산소 신체활동 실천", "유산소 신체활동 실천", "실천"),
        ("폐경 후(여자)", "폐경 상태(여자)", "폐경 후"), ("체질량지수(kg/m²), 평균", "체질량지수(kg/m2)", "평균"), ("고혈압", "고혈압 유병", "있음"),
        ("당뇨병", "당뇨병 유병", "있음"), ("eGFR, 평균", "eGFR(CKD-EPI 2009)", "평균")]
for lab, var, lev in spec:
    rows.append([lab] + [("-" if (var.startswith("폐경") and s == "남자") else t1v(s, g, var, lev)) for s in SX for g in FL])
rows.append(["하루 음주량(g/일), 평균"] + [f'{float(fo_(s, g)["음주_g일_가중평균"]):.1f} ({float(fo_(s, g)["음주_SE"]):.1f})' for s in SX for g in FL])
rows.append(["고음주"] + [f'{n0(fo_(s, g)["고음주_n"])} ({float(fo_(s, g)["고음주_가중pct"]):.1f})' for s in SX for g in FL])
rows.append(["고요산혈증"] + [f'{n0(fo_(s, g)["고요산혈증_n"])} ({float(fo_(s, g)["고요산혈증_가중pct"]):.1f})' for s in SX for g in FL])
rows.append(["혈청 요산(mg/dL), 평균"] + [f'{float(fo_(s, g)["요산_가중평균"]):.2f} ({float(fo_(s, g)["요산_SE"]):.2f})' for s in SX for g in FL])
TAB("표 1. 성별 · 홍조 이력별 대상자 특성", heads, rows,
    "범주형은 n (가중 %), 연속형은 가중 평균(표준오차). n은 가중치를 적용하지 않은 표본 수. 국민건강영양조사 2019–2021, 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu 반영. 고음주·고요산혈증·혈청 요산·하루 음주량은 모형 분석 대상 기준(여자 비홍조 3,271명, 폐경 상태 결측 2명 제외). 고음주: 남 16 g/일, 여 8 g/일 이상.",
    small=True)

H2("3.2 홍조 이력별 고음주와 고요산혈증")
P(f"남자 비홍조군에서는 저음주와 고음주의 고요산혈증 유병률이 비슷하였으나({float(cell('남자', '비홍조', '저음주')['가중유병률']):.1f}% vs {float(cell('남자', '비홍조', '고음주')['가중유병률']):.1f}%), 홍조 소실군({float(cell('남자', '홍조 소실', '저음주')['가중유병률']):.1f}% vs {float(cell('남자', '홍조 소실', '고음주')['가중유병률']):.1f}%)과 지속 홍조군({float(cell('남자', '지속 홍조', '저음주')['가중유병률']):.1f}% vs {float(cell('남자', '지속 홍조', '고음주')['가중유병률']):.1f}%)에서는 고음주군의 유병률이 높았다(표 2). 주 모형에서 남자 비홍조군의 고음주 OR은 {ci(A_MAIN, '남자', '비홍조')}로 연관이 없었고, 지속 홍조군은 {ci(A_MAIN, '남자', '지속 홍조')}, 홍조 소실군은 {ci(A_MAIN, '남자', '홍조 소실')}이었다(그림 2). 여자는 비홍조군 {ci(A_MAIN, '여자', '비홍조')}, 지속 홍조군 {ci(A_MAIN, '여자', '지속 홍조')}였고, 홍조 소실군은 고음주 칸의 사례가 {cell('여자', '홍조 소실', '고음주')['고요산혈증_n']}명뿐이라 해석하지 않았다.")
rows2 = []
for s in SX:
    for g in FL:
        a, b = cell(s, g, "저음주"), cell(s, g, "고음주")
        few = min(int(a["고요산혈증_n"]), int(b["고요산혈증_n"])) < 10
        rows2.append([SXS[s], g, f'{n0(a["n"])} ({a["고요산혈증_n"]}, {float(a["가중유병률"]):.1f})', f'{n0(b["n"])} ({b["고요산혈증_n"]}, {float(b["가중유병률"]):.1f})'] +
                     (["†", "†", "†"] if few else [ci(A_CR, s, g), ci(A_MAIN, s, g), ci(A_M3, s, g)]))
TAB("표 2. 홍조 이력별 칸 인원, 가중 유병률과 고음주의 고요산혈증 오즈비(95% CI)", ["성별", "홍조 이력", "저음주 n (사례, 가중 %)", "고음주 n (사례, 가중 %)", "Crude", "Model 2(주 모형)", "Model 3"], rows2,
    "고음주: 남 16 g/일, 여 8 g/일 이상. OR은 같은 홍조군의 저음주 대비. Model 2: 연령, 교육수준, 소득, 조사연도, 흡연 3범주, 유산소 신체활동, 폐경 상태(여자). Model 3: Model 2 + 체질량지수, 고혈압, 당뇨병, eGFR. 분석 인원 남 " + f"{nm:,}명, 여 {nf:,}명(세 모형 같음). † 비교하는 칸의 고요산혈증 사례가 10명 미만이라 제시하지 않음.",
    widths=[7, 11, 18, 18, 15, 16, 15])
FIG(FA / "fig1_고요산혈증_OR_홍조이력별.png", "그림 2. 홍조 이력별 고음주 vs 저음주의 고요산혈증 오즈비(주 모형, 95% CI, 로그 눈금). 빈 원은 사례 10명 미만이라 해석하지 않음.", 12.5)

H2("3.3 홍조 이력에 따른 효과수정")
P(f"남자에서 지속 홍조군의 OR은 비홍조군의 1.45배였으나(OR의 비 {r_main_m}) 차이는 통계적으로 확인되지 않았다(주질문 p = {fp(trow(A_MAIN, '주질문', '남자')['p'])}). 홍조 소실군의 OR은 비홍조군의 약 2배였다(OR의 비 {r_sub_m}, 부질문 p = {fp(trow(A_MAIN, '부질문', '남자')['p'])}). 여자는 지속 홍조군과 비홍조군의 OR이 같았다(OR의 비 {r_main_f}, p = {fp(trow(A_MAIN, '주질문', '여자')['p'])}). 대사 변수를 더 보정한 Model 3에서는 남자 주질문 p = {fp(trow(A_M3, '주질문', '남자')['p'])}, 부질문 p = {fp(trow(A_M3, '부질문', '남자')['p'])}였으나, 추가한 변수는 매개변수일 수 있어 참고로만 해석하였다. 남자 홍조 소실군과 지속 홍조군 사이의 차이는 유의하지 않았다(탐색, p = {fp(trow(A_MAIN, '탐색', '남자')['p'])})(표 3, 그림 3).")
rows3 = []
for q, g, lab in [("주질문", "지속 홍조", "지속 홍조 vs 비홍조(주질문)"), ("부질문", "홍조 소실", "홍조 소실 vs 비홍조(부질문)")]:
    for s in SX:
        few = "10명" in trow(A_MAIN, q, s)["비고"]
        rows3.append([lab, SXS[s]] + (["†"] * 3 if few else [ci(A_CR, s, g, "OR의 비"), ci(A_MAIN, s, g, "OR의 비"), ci(A_M3, s, g, "OR의 비")])
                     + [pt(A_CR, q, s), pt(A_MAIN, q, s), pt(A_M3, q, s)])
TAB("표 3. 홍조 이력에 따른 효과수정: 오즈비의 비(홍조군 OR ÷ 비홍조군 OR)와 교호작용 p", ["비교", "성별", "OR의 비: Crude", "OR의 비: Model 2", "OR의 비: Model 3", "p: Crude", "p: Model 2", "p: Model 3"], rows3,
    "곱셈 척도 교호작용 항의 Wald 검정(1 자유도). * p < 0.05. † 비교하는 칸의 사례가 10명 미만이라 해석하지 않음.", widths=[22, 6, 14, 14, 14, 10, 10, 10])
FIG(FA / "fig4_주질문_부질문_forest.png", "그림 3. 주질문·부질문의 오즈비의 비(95% CI). 빨간색이 주 모형, 민감도 4개는 주 모형으로 계산. 빈 원은 사례 10명 미만이라 해석하지 않음.", 14)

H2("3.4 혈청 요산")
P(f"혈청 요산을 연속형으로 본 다중회귀에서는 남녀 모두 홍조 이력에 따른 차이가 없었다(주질문 p: 남 {fp(trow(A_UA, '주질문', '남자')['p'])}, 여 {fp(trow(A_UA, '주질문', '여자')['p'])}; 부질문 p: 남 {fp(trow(A_UA, '부질문', '남자')['p'])}, 여 {fp(trow(A_UA, '부질문', '여자')['p'])}). 여자 비홍조군에서는 고음주일 때 혈청 요산이 {ci(A_UA, '여자', '비홍조', sep=', ')} mg/dL 높았다(표 4).")
rows4 = [[SXS[s]] + [ci(A_UA, s, g, sep=", ") for g in FL] + [f"{ci(A_UA, s, '지속 홍조', 'β 차이', sep=', ')}; p = {fp(trow(A_UA, '주질문', s)['p'])}",
                                                          f"{ci(A_UA, s, '홍조 소실', 'β 차이', sep=', ')}; p = {fp(trow(A_UA, '부질문', s)['p'])}"] for s in SX]
TAB("표 4. 홍조 이력별 고음주(저음주 대비)의 혈청 요산 차이(mg/dL, 95% CI, Model 2)", ["성별", "비홍조", "홍조 소실", "지속 홍조", "차이의 차이: 지속 − 비홍조", "차이의 차이: 소실 − 비홍조"], rows4,
    "복합표본 다중 선형회귀. '차이의 차이'는 교호작용 항(β)으로, 홍조군의 고음주 효과에서 비홍조군의 고음주 효과를 뺀 값.", widths=[7, 17, 17, 17, 21, 21])

H2("3.5 민감도 분석")
P(f"1잔의 알코올 양과 음주 대푯값을 바꾼 네 분석에서 남자 홍조 소실군의 OR의 비는 {SR}로 주분석({r_sub_m})과 비슷하였다(표 5, 그림 3). 다만 부질문 p는 {min(sens_sub_p):.3f}–{max(sens_sub_p):.3f}로 경계 근처였고, OR의 비의 95% CI 하한은 {min(sens_ratio_lo):.2f}–{max(sens_ratio_lo):.2f}로 네 분석 중 두 분석에서 1보다 작았다. 남자 주질문과 여자 주질문의 결론은 모든 분석에서 같았다.")
rows5 = []
for a, lab in zip([A_MAIN] + A_SENS, ["주분석(1잔 7 g, 중간 대푯값)", "1잔 8 g", "1잔 10 g", "대푯값 전체 하한", "대푯값 전체 상한"]):
    rows5.append([lab, ci(a, "남자", "지속 홍조", "OR의 비"), pt(a, "주질문", "남자"), ci(a, "남자", "홍조 소실", "OR의 비"), pt(a, "부질문", "남자"),
                  ci(a, "여자", "지속 홍조", "OR의 비"), pt(a, "주질문", "여자")])
TAB("표 5. 민감도 분석: 오즈비의 비(95% CI)와 교호작용 p(고요산혈증, Model 2)", ["분석", "남 주질문 OR의 비", "p", "남 부질문 OR의 비", "p", "여 주질문 OR의 비", "p"], rows5,
    "여자 부질문은 모든 분석에서 비교하는 칸의 사례가 10명 미만이라 제시하지 않음. * p < 0.05.", widths=[22, 15, 8, 15, 8, 15, 8])

H2("3.6 분석 검증")
o = {(x["지표"][:16], x["성별"]): x for x in off}
P(f"2019년 만 19세 이상 월간음주율과 고위험음주율 6개 지표의 재현값과 응답자 수가 국가 공식 통계와 모두 일치하였다(부록 표 4). 새 R 세션에서 분석을 두 번 실행한 결과 파일이 모두 같았고(재현성 확인 {repro_res}), 주분석 p 값은 별도의 분석 도구로 다시 계산한 값과 같았다.")

# ===== 4. 고찰 =====
H1("4. 고찰")
H2("4.1 주요 결과")
P(f"한국 성인 음주자를 대표하는 표본에서, 지속 홍조군의 음주–고요산혈증 연관이 비홍조군보다 강하다는 가설은 통계적으로 지지되지 않았다. 남자 지속 홍조군의 OR은 비홍조군보다 컸으나({est(A_MAIN, '남자', '지속 홍조'):.2f} vs {est(A_MAIN, '남자', '비홍조'):.2f}) 그 차이의 신뢰구간이 1을 포함하였다. 대신 과거에 홍조가 있었으나 지금은 없는 남자 홍조 소실군에서 고음주와 고요산혈증의 연관이 비홍조군보다 컸고(OR의 비 {r_sub_m}), 이 크기는 1잔의 알코올 양과 음주 대푯값을 바꾸어도 비슷하였다. 여자는 비홍조군에서도 고음주와 고요산혈증이 연관되었고, 홍조 이력에 따른 차이는 없었다.")
H2("4.2 선행연구와의 비교")
P(f"건강검진 남성을 대상으로 한 선행연구는 홍조군이 더 적은 음주량에서도 고요산혈증 위험이 높다고 보고하였다{R('kim2015', 'kim2016')}. 본 연구의 남자 지속 홍조군도 같은 방향이었으나, 전국 표본에서 차이를 확인하지는 못하였다. 같은 국민건강영양조사 2019–2021 남성 자료로 지방간을 본 연구에서도 홍조군의 OR이 비홍조군보다 조금 컸지만 판정법에 따라 차이의 뚜렷함이 달랐다{R('kang2023')}. 홍조에 따른 효과수정이 있더라도 크기가 작아 현재 표본 크기로는 검출이 어려울 수 있다. 특히 지속 홍조군은 술을 적게 마셔 고음주자가 남자 {n0(cell('남자', '지속 홍조', '고음주')['n'])}명(사례 {cell('남자', '지속 홍조', '고음주')['고요산혈증_n']}명)에 그쳤다. 반대로 알코올의존 남성에서는 ALDH2 활성이 정상인 사람의 요산이 더 높았다는 보고가 있으나{R('yokoyama')}, 매우 많이 마시는 환자군이라 일반 음주자인 본 연구와 직접 비교하기 어렵다. 한편 ALDH2 유전자형 자체는 고요산혈증과 직접 연관되지 않았다는 보고{R('jee2019')}는, 본 연구에서 지속 홍조군의 고요산혈증 유병률이 비홍조군보다 높지 않았던 결과(표 1)와 어긋나지 않는다.")
P(f"남자 비홍조군에서 고음주와 고요산혈증의 연관이 없었던 점은 음주량이 많을수록 위험이 커진다는 선행연구{R('choi2004', 'nakamura', 'ma2025')}와 다르다. 본 연구의 고음주 기준(16 g/일)이 선행연구에서 위험이 뚜렷해지는 수준보다 낮은 일반 건강 기준이고, 고음주군 안에 다양한 음주량이 섞여 있었기 때문일 수 있다.")
H2("4.3 홍조 소실군의 해석")
P(f"홍조 소실군에서 연관이 더 컸던 이유는 이 자료로 판단할 수 없다. 다만 남자 홍조 소실군은 나이가 많고, 현재흡연 비율과 하루 음주량이 가장 높았다(표 1). 홍조 소실은 오랜 음주로 홍조 반응이 줄어드는 내성이나 음주 습관의 변화와 얽혀 있을 수 있으며, 이 경우 소실군은 오랫동안 많이 마셔 온 사람이 모인 군일 수 있다. 과거와 현재의 홍조를 나누어 본 선행연구에서도 홍조의 변화 양상에 따라 건강 결과와의 연관이 달랐다{R('jeon2022', 'park2022')}. 그러나 소실군은 {n0(m_lf['n'])}명(고음주 {n0(m_lf['고음주_n'])}명)으로 작고, 소실군과 지속 홍조군의 차이는 유의하지 않았으며, 혈청 요산에서는 차이가 나타나지 않았다. 따라서 이 결과는 확증이 아니라 후속 연구에서 확인할 가설로 보아야 한다.")
H2("4.4 여성의 결과")
P(f"여자는 비홍조군에서도 고음주와 고요산혈증이 연관되었고(OR {ci(A_MAIN, '여자', '비홍조')}), 혈청 요산도 높았다. 두 군 모두 고음주일 때 유병률이 약 두 배로 늘어(비홍조 {float(cell('여자', '비홍조', '저음주')['가중유병률']):.1f}% → {float(cell('여자', '비홍조', '고음주')['가중유병률']):.1f}%, 지속 홍조 {float(cell('여자', '지속 홍조', '저음주')['가중유병률']):.1f}% → {float(cell('여자', '지속 홍조', '고음주')['가중유병률']):.1f}%) OR의 비가 1에 가까웠다. 다만 여자 고요산혈증 사례가 {n0(ocs['여자']['고요산혈증_n'])}명으로 적고 지속 홍조 고음주 칸은 사례가 {cell('여자', '지속 홍조', '고음주')['고요산혈증_n']}명이어서 OR의 비의 신뢰구간이 넓으므로, 차이가 없다고 결론짓기보다는 판단할 근거가 부족하다고 보는 것이 적절하다. 여자의 고음주 기준(8 g/일)은 남자의 절반이어서 같은 '고음주'라도 실제 음주량 수준이 다르다는 점도 고려해야 한다.")
H2("4.5 강점과 한계")
P("본 연구는 전국을 대표하는 표본에서 복합표본설계를 반영하여 과거·현재 홍조 이력과 실제로 측정한 혈청 요산을 연결하였고, 현재 홍조뿐 아니라 홍조 소실군을 따로 분석하였다. 국가 공식 음주 통계를 재현하고 재현성을 확인하여 분석의 정확성을 점검하였으며, 보정변수를 결과의 변화가 아니라 근거 문헌과 인과 구조로 정하였다.")
P("한계는 다음과 같다.")
UL([
    "<b>결과를 본 뒤 확정한 분석 조건:</b> 음주 기준, 1잔의 알코올 양, 흡연 범주, 보정 모형, 민감도 목록은 여러 기준의 결과를 검토한 뒤 정하였다(부록 표 3). 이 과정에서 생기는 낙관적 비뚤림을 배제할 수 없다.",
    "<b>다중 비교:</b> 성별 2 × 질문 2 × 결과 2의 검정 가운데 유의한 것은 남자 부질문 하나였고, 민감도 분석에서 그 p는 0.04–0.07 사이를 오갔다.",
    f"<b>작은 군:</b> 남자 홍조 소실군은 {n0(m_lf['n'])}명, 여자 홍조 소실군은 {n0(fo_('여자', '홍조 소실')['n'])}명(고음주 사례 {cell('여자', '홍조 소실', '고음주')['고요산혈증_n']}명)이었다.",
    "<b>단면 설계와 역인과:</b> 음주와 요산을 같은 시점에 측정하여 인과 관계를 판단할 수 없다. 고요산혈증이나 통풍 위험을 알게 된 사람이 술을 줄였을 가능성도 있다.",
    "<b>노출 측정:</b> 음주량은 설문 범주의 대푯값으로 계산하였고 1잔의 알코올 양은 가정이다(민감도 분석으로 일부 확인). 고음주 기준은 요산이 아닌 일반 건강 지표로 만든 절주 지침에서 왔다.",
    f"<b>홍조 측정:</b> 과거 홍조는 회상에 의존하며, 홍조 설문이 ALDH2 유전자형을 완전히 반영하지는 않는다{R('shin2018')}.",
    f"<b>선택:</b> 현재 음주자만 분석하여 홍조 때문에 술을 끊은 사람이 빠졌다. 통풍 진단·무응답이나 보정변수 결측으로 제외된 {excl_n:,}명은 포함된 사람보다 나이가 많고(평균 {incv('제외', '나이(세)', '평균').split(' ')[0]}세 vs {incv('포함', '나이(세)', '평균').split(' ')[0]}세) 음주량과 혈청 요산이 높았다(부록 표 2).",
    "<b>측정하지 못한 교란:</b> 술의 종류, 이뇨제 등 요산을 올리는 약물, 퓨린·과당 섭취는 보정하지 못하였다.",
    f"<b>효과수정의 척도:</b> 곱셈 척도의 교호작용만 평가하였다. 덧셈 척도의 교호작용(RERI 등)은 보고하지 않았다{R('knol')}.",
])
H2("4.6 의의와 후속 연구")
P("이 연구는 홍조가 있으면 술이 더 해롭다는 통념이 고요산혈증에서는 지속 홍조를 기준으로 확인되지 않았음을 보여 준다. 반면 홍조가 사라진 남성에서 연관이 더 컸다는 결과는, 현재 홍조만이 아니라 과거 홍조 이력을 함께 물을 필요가 있는지에 대한 질문을 남긴다. ALDH2 유전자형 정보와 반복 측정 자료를 이용한 종단 연구, 그리고 덧셈 척도의 교호작용 분석으로 이를 확인할 필요가 있다.")

# ===== 5. 결론 =====
H1("5. 결론")
P("국민건강영양조사 2019–2021년 성인 음주자에서 지속 홍조군의 음주–고요산혈증 연관이 비홍조군보다 강하다는 근거는 확인되지 않았다. 남자에서는 과거에 홍조가 있었으나 지금은 없는 홍조 소실군에서 고음주와 고요산혈증의 연관이 비홍조군보다 컸으나, p 값이 경계 근처이고 군의 크기가 작아 가설을 만드는 결과로 보아야 한다.")

# ===== 기타 =====
H1("기타")
P("<b>자료 이용:</b> 국민건강영양조사 원시자료는 질병관리청 국민건강영양조사 누리집에서 신청 후 이용할 수 있다. 분석 코드는 팀 저장소(code/final_analysis.R)에 있다.")
P("<b>생성형 AI 사용:</b> 분석 코드 작성과 문서 정리에 생성형 AI(Claude)를 활용하였으며, 모든 결과와 문장은 조원이 검토하였다.")
P("<b>이해관계:</b> <span class='red'>[없음 — 조원 확인]</span>")

# ===== 참고문헌 =====
H1("참고문헌")
B.append(("refs",))

# ===== 부록 =====
BRK()
H1("부록")
keep = ["sex", "age", "wt_itvex", "kstrata", "psu", "edu", "incm", "bd1_11", "bd2_1", "bd2_14", "bd9_1", "bd9_2", "he_uacid", "he_uacid_etc",
        "dm8_dg", "bs1_1", "bs3_1", "pa_aerobic", "lw_ms", "he_bmi", "he_hp", "he_dm_hba1c", "he_crea"]
TAB("부록 표 1. 변수 정의와 결측·비해당 처리(국민건강영양조사 제8기 원시자료 이용지침서)", ["변수", "뜻", "결측 코드", "비해당 처리", "지침서 쪽수(PDF / 인쇄)"],
    [[v["var"].upper() if v["var"] not in ("age", "sex", "psu", "edu", "incm", "kstrata", "wt_itvex", "pa_aerobic") else v["var"], v["label"], v["miss"] or "-",
      (v["na8"] or "-").split("(")[0], v["source"].replace("8기 PDF ", "PDF ")] for v in vdef if v["var"] in keep],
    "BD2_14의 888(비해당)은 10잔 미만이라는 뜻이므로 값만 비우고 BD2_1의 대푯값을 사용하였다.", widths=[13, 42, 9, 13, 23], small=True)
inc_rows = []
for lab, var, lev in [("n", None, None), ("나이(세), 평균", "나이(세)", "평균"), ("남자", "성별", "남자"), ("하루 음주량(g/일), 평균", "주당 음주 잔 수", "평균"),
                      ("지속 홍조", "홍조 패턴", "지속 홍조"), ("홍조 소실", "홍조 패턴", "홍조 소실"), ("혈청 요산(mg/dL), 평균", "혈청 요산", "평균")]:
    if var is None:
        inc_rows.append([lab, n0(next(x["집단n"] for x in inc if x["집단"] == "포함")), n0(excl_n)])
    else:
        inc_rows.append([lab] + [(incv(gr, var, lev) if lev == "평균" else f'{n0(next(x["n"] for x in inc if x["집단"] == gr and x["변수"] == var and x["수준"] == lev))} ({incv(gr, var, lev).split(" ")[0]})') for gr in ["포함", "제외"]])
TAB("부록 표 2. 최종 포함 대상과 제외 대상의 비교(음주량·홍조·요산이 있는 12,002명 중)", ["특성", "포함", "제외(통풍 진단·무응답 또는 보정변수 결측)"], inc_rows,
    "범주형은 n (가중 %), 연속형은 가중 평균(표준오차). 1잔 7 g이면 하루 음주량(g/일)은 주당 잔 수와 같다.", widths=[34, 30, 36])
g_rows = []
for x in sorted([x for x in gcrit if x["단계"] == "G" and x["보정묶음"] == "M2S" and x["결과변수"].startswith("고요산")], key=lambda z: z["기준"]):
    name = re.sub(r"^G\d+ ", "", x["기준"])
    cut = (f'남 {x["남 절단점(g/일)"]} / 여 {x["여 절단점(g/일)"]}' if x["남 절단점(g/일)"] else x["연속형 단위"])
    clean = lambda v: (v.replace("*", "").replace("†", "") + " †") if "†" in v else v   # 사례 10명 미만이면 * 표시를 빼고 †만
    g_rows.append([x["기준"][:3], name, cut, clean(x["주 남"]), clean(x["부1 남"]), clean(x["주 여"]), clean(x["부1 여"])])
TAB("부록 표 3. 분석 조건을 확정하기 전에 검토한 음주 기준과 교호작용 p(고요산혈증, 흡연 3범주 보정 모형)", ["번호", "기준", "절단점(g/일) 또는 단위", "남 주질문", "남 부질문", "여 주질문", "여 부질문"], g_rows,
    "2026-11-16 실행 기록(output/21_ptest/ptest_log.csv)의 요약. G02가 최종 확정 기준(S2)이다. 이 표의 S1(G01)은 반올림한 절단점이다. 기준마다 범주 수가 달라 자유도가 다르며(범주형 공동 검정), 연속형(G05, G11)은 1 자유도, 스플라인(G13)은 3 자유도. * p < 0.05. † 비교하는 칸의 사례가 10명 미만. 보정 묶음 전체와 혈청 요산 결과는 Result/2026-11-23_ptest_전체표_현재질문.xlsx.",
    widths=[7, 30, 21, 10, 10, 11, 11], small=True)
TAB("부록 표 4. 공식 통계 대조(2019년, 만 19세 이상, 조율)", ["지표", "성별", "n / 공식 n", "재현값 % (95% CI)", "공식값 %", "판정"],
    [[x["지표"].replace("(%, 2019, 조율)", "").replace("만19세 이상 ", ""), x["성별"], f'{n0(x["n"])} / {n0(x["공식n"])}',
      f'{float(x["재현값"]):.1f} ({float(x["CI하한"]):.1f}–{float(x["CI상한"]):.1f})', x["공식값"], x["판정"]] for x in off],
    "공식값: KOSIS 국민건강영양조사 월간 음주율 추이(DT_11702_N012), 고위험음주율 추이(DT_11702_N018), '19세이상' 행. 판정: 재현값을 소수 첫째 자리로 반올림한 값과 응답자 수가 모두 같으면 '일치'.",
    widths=[24, 10, 18, 22, 12, 14])

# ---------------- 참고문헌 목록 완성 ----------------
ref_list = [f"{i + 1}. {REFS[k]}" for i, k in enumerate(ORDER)]
unused = [k for k in REFS if k not in ORDER]


def inline(t):                                   # HTML용: <b>, <span class='red'>만 허용
    return t


# ================= HTML(PDF) =================
CSS = """
@page { size: A4; margin: 20mm 18mm 18mm 18mm; @bottom-center { content: counter(page); font-family: 'Malgun Gothic'; font-size: 8.5pt; color: #555; } }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { font-family: 'Malgun Gothic', sans-serif; font-size: 10pt; line-height: 1.75; color: #111; word-break: keep-all; overflow-wrap: anywhere; }
.title { text-align: center; margin: 4mm 0 8mm; } .title h1 { font-size: 16pt; line-height: 1.45; margin: 0 0 6px; }
.title .en { font-size: 10pt; color: #333; font-style: italic; margin-bottom: 10px; } .title .au { font-size: 9.6pt; } .title .note { font-size: 8.6pt; color: #b00020; margin-top: 4px; }
h1.s { font-size: 12.5pt; margin: 16px 0 6px; padding-bottom: 3px; border-bottom: 1px solid #999; break-after: avoid; }
h2 { font-size: 10.8pt; margin: 12px 0 4px; break-after: avoid; }
p { margin: 4px 0 7px; text-align: justify; } ul { margin: 2px 0 8px; padding-left: 20px; } li { margin: 3px 0; }
.red { color: #c00000; }
table { width: 100%; border-collapse: collapse; margin: 6px 0 2px; font-size: 8.4pt; line-height: 1.45; break-inside: avoid; }
table.small { font-size: 7.6pt; }
th { border-top: 1.2px solid #000; border-bottom: 0.8px solid #000; padding: 3px 4px; text-align: left; font-weight: 700; background: #f2f2f2; }
td { border-bottom: 0.4px solid #ccc; padding: 2.5px 4px; vertical-align: top; } tr:last-child td { border-bottom: 1.2px solid #000; }
.cap { font-weight: 700; font-size: 9pt; margin: 10px 0 2px; break-after: avoid; } .tnote { font-size: 7.8pt; color: #333; margin: 2px 0 10px; line-height: 1.5; }
figure { margin: 8px 0 10px; text-align: center; break-inside: avoid; } figure img { max-width: 100%; } figcaption { font-size: 8.4pt; color: #222; margin-top: 3px; text-align: left; }
.refs p { font-size: 8.6pt; line-height: 1.5; margin: 2px 0; padding-left: 18px; text-indent: -18px; text-align: left; }
.brk { break-before: page; }
"""
H = []
for b in B:
    k = b[0]
    if k == "title":
        H.append(f'<div class="title"><h1>{escape(b[1])}</h1><div class="en">{escape(b[2])}</div><div class="au">{escape(b[3])}</div><div class="note">{escape(b[4])}</div></div>')
    elif k == "h1":
        H.append(f'<h1 class="s">{escape(b[1])}</h1>')
    elif k == "h2":
        H.append(f"<h2>{escape(b[1])}</h2>")
    elif k == "p":
        H.append(f"<p>{inline(b[1])}</p>")
    elif k == "ul":
        H.append("<ul>" + "".join(f"<li>{inline(x)}</li>" for x in b[1]) + "</ul>")
    elif k == "table":
        cap, head, rows, note, widths, small = b[1:]
        cg = "<colgroup>" + "".join(f'<col style="width:{w}%">' for w in widths) + "</colgroup>" if widths else ""
        H.append(f'<div class="cap">{escape(cap)}</div><table class="{"small" if small else ""}">{cg}<thead><tr>' + "".join(f"<th>{escape(h)}</th>" for h in head)
                 + "</tr></thead><tbody>" + "".join("<tr>" + "".join(f"<td>{escape(str(c))}</td>" for c in r) + "</tr>" for r in rows) + "</tbody></table>"
                 + (f'<div class="tnote">{escape(note)}</div>' if note else ""))
    elif k == "fig":
        H.append(f'<figure><img src="{Path(b[1]).as_uri()}" style="width:{b[3] / 17.4 * 100:.0f}%"><figcaption>{escape(b[2])}</figcaption></figure>')
    elif k == "refs":
        H.append('<div class="refs">' + "".join(f"<p>{escape(x)}</p>" for x in ref_list) + "</div>")
    elif k == "br":
        H.append('<div class="brk"></div>')
html = f'<!doctype html><html lang="ko"><head><meta charset="utf-8"><title>{BASE}</title><style>{CSS}</style></head><body>{"".join(H)}</body></html>'

# ================= Word =================
doc = Document()
st = doc.styles["Normal"]
st.font.name, st.font.size = "Malgun Gothic", Pt(10)
st.element.rPr.rFonts.set(qn("w:eastAsia"), "Malgun Gothic")
st.paragraph_format.line_spacing = 1.4
for sec in doc.sections:
    sec.page_width, sec.page_height = Cm(21), Cm(29.7)
    sec.top_margin = sec.bottom_margin = Cm(2)
    sec.left_margin = sec.right_margin = Cm(1.8)


def set_font(run, size=None, bold=None, italic=None, color=None):
    run.font.name = "Malgun Gothic"
    run._element.rPr.rFonts.set(qn("w:eastAsia"), "Malgun Gothic")
    if size: run.font.size = Pt(size)
    if bold is not None: run.bold = bold
    if italic is not None: run.italic = italic
    if color: run.font.color.rgb = RGBColor.from_string(color)


TOK = re.compile(r"(<b>.*?</b>|<span class='red'>.*?</span>)", re.S)


def add_rich(par, text, size=None):
    for part in TOK.split(text):
        if not part:
            continue
        if part.startswith("<b>"):
            set_font(par.add_run(part[3:-4]), size, bold=True)
        elif part.startswith("<span"):
            set_font(par.add_run(re.sub(r"^<span class='red'>|</span>$", "", part)), size, color="C00000")
        else:
            set_font(par.add_run(part), size)


def shade(cell_, hexcolor):
    tcPr = cell_._element.get_or_add_tcPr()
    sh = OxmlElement("w:shd"); sh.set(qn("w:val"), "clear"); sh.set(qn("w:color"), "auto"); sh.set(qn("w:fill"), hexcolor); tcPr.append(sh)


for b in B:
    k = b[0]
    if k == "title":
        for txt, sz, bold, it, col in [(b[1], 15, True, False, None), (b[2], 9.5, False, True, "333333"), (b[3], 9.5, False, False, None), (b[4], 8.5, False, False, "C00000")]:
            p = doc.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.CENTER; set_font(p.add_run(txt), sz, bold, it, col)
    elif k == "h1":
        p = doc.add_paragraph(); p.paragraph_format.space_before = Pt(12); p.paragraph_format.keep_with_next = True
        set_font(p.add_run(b[1]), 12.5, True)
    elif k == "h2":
        p = doc.add_paragraph(); p.paragraph_format.space_before = Pt(6); p.paragraph_format.keep_with_next = True
        set_font(p.add_run(b[1]), 10.8, True)
    elif k == "p":
        p = doc.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY; add_rich(p, b[1])
    elif k == "ul":
        for x in b[1]:
            p = doc.add_paragraph(style="List Bullet"); add_rich(p, x)
    elif k == "table":
        cap, head, rows, note, widths, small = b[1:]
        p = doc.add_paragraph(); p.paragraph_format.keep_with_next = True; set_font(p.add_run(cap), 9, True)
        t = doc.add_table(rows=1, cols=len(head)); t.style = "Table Grid"; t.alignment = WD_TABLE_ALIGNMENT.CENTER
        fs = 7 if small else 7.8
        for j, h in enumerate(head):
            c = t.rows[0].cells[j]; c.text = ""; set_font(c.paragraphs[0].add_run(h), fs, True); shade(c, "F2F2F2")
        for r in rows:
            cs = t.add_row().cells
            for j, v in enumerate(r):
                cs[j].text = ""; set_font(cs[j].paragraphs[0].add_run(str(v)), fs)
        if widths:
            tot = 17.4
            for row in t.rows:
                for j, w in enumerate(widths):
                    row.cells[j].width = Cm(tot * w / sum(widths))
        if note:
            p = doc.add_paragraph(); set_font(p.add_run(note), 7.5, color="333333")
    elif k == "fig":
        doc.add_picture(b[1], width=Cm(b[3])); doc.paragraphs[-1].alignment = WD_ALIGN_PARAGRAPH.CENTER
        p = doc.add_paragraph(); set_font(p.add_run(b[2]), 8.4)
    elif k == "refs":
        for x in ref_list:
            p = doc.add_paragraph(); p.paragraph_format.left_indent = Cm(0.6); p.paragraph_format.first_line_indent = Cm(-0.6)
            set_font(p.add_run(x), 8.5)
    elif k == "br":
        doc.add_page_break()
doc.save(OUT_DOCX)

# ================= PDF =================
browsers = [r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe", r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
            r"C:\Program Files\Google\Chrome\Application\chrome.exe"]
exe = next((x for x in browsers if Path(x).exists()), None)
if not exe:
    sys.exit("Edge 또는 Chrome을 찾지 못했습니다.")
tmp = Path(tempfile.gettempdir())
hf = tmp / "knhanes_paper.html"
hf.write_text(html, encoding="utf-8")
before = OUT_PDF.stat().st_mtime if OUT_PDF.exists() else 0
subprocess.run([exe, "--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--allow-file-access-from-files",
                f"--user-data-dir={tmp / 'edge_pdf_profile'}", "--no-pdf-header-footer", f"--print-to-pdf={OUT_PDF}", hf.as_uri()],
               check=False, timeout=180, capture_output=True)
for _ in range(60):
    if OUT_PDF.exists() and OUT_PDF.stat().st_mtime > before and OUT_PDF.stat().st_size > 0:
        break
    time.sleep(1)
else:
    sys.exit("PDF가 만들어지지 않았습니다.")
import pypdfium2
pdf = pypdfium2.PdfDocument(str(OUT_PDF))
print("저장:", OUT_DOCX.name, "/", OUT_PDF.name, f"({len(pdf)}쪽)", "| 참고문헌", len(ref_list), "편", "| 인용 안 된 문헌:", unused)
