# 보정변수 단계별 보정 결과표(OR·p값)를 PDF로 만든다.
# 입력: output/13_v3_main_analysis/12a_four_models.csv (code/13_v3_main_analysis.R의 결과, 새로 계산하지 않음)
# 출력: Result/2026-11-16_보정변수_보정결과표(V3_이전기준).pdf (조원 정리 결과 폴더, Result/README.md 규칙)
# 실행: 저장소 최상위 폴더에서  powershell -File code/25_adjusted_or_table_pdf.ps1
# PDF 변환에는 Microsoft Edge의 headless 인쇄 기능을 쓴다.

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$csv  = Join-Path $root 'output\13_v3_main_analysis\12a_four_models.csv'
$out  = Join-Path $root 'Result'
New-Item -ItemType Directory -Force $out | Out-Null
$rows = Import-Csv $csv -Encoding UTF8

# 숫자 형식: OR은 소수 둘째 자리, p는 0.001 미만이면 "<0.001"
function F2($x) { '{0:N2}' -f [double]$x }
function FP($x) { $v = [double]$x; if ($v -lt 0.001) { '&lt;0.001' } else { '{0:N3}' -f $v } }
function ORCI($o, $l, $h) { "$(F2 $o) ($(F2 $l)–$(F2 $h))" }

$modelLabel = @{
  'Crude'   = 'Crude<br><span class="s">보정 없음</span>'
  'Model 1' = 'Model 1<br><span class="s">연령</span>'
  'Model 2' = 'Model 2<br><span class="s">기본 6개</span>'
  'Model 3' = 'Model 3<br><span class="s">전체 10개</span>'
}

# 표 1: 주효과 OR (상호작용항 없는 모형)
$t1 = foreach ($sex in '남자','여자') {
  $first = $true
  foreach ($m in 'Crude','Model 1','Model 2','Model 3') {
    $mid  = $rows | Where-Object { $_.sex -eq $sex -and $_.model -eq $m -and $_.exposure -eq '중음주' }
    $high = $rows | Where-Object { $_.sex -eq $sex -and $_.model -eq $m -and $_.exposure -eq '고음주' }
    $sexCell = if ($first) { "<td rowspan='4' class='sex'>$sex<br><span class='s'>n = $('{0:N0}' -f [int]$mid.n_model)</span></td>" } else { '' }
    $first = $false
    "<tr>$sexCell<td class='l'>$($modelLabel[$m])</td><td>1.00</td><td>$(ORCI $mid.main_or $mid.main_lo $mid.main_hi)</td><td>$(FP $mid.main_p)</td><td>$(ORCI $high.main_or $high.main_lo $high.main_hi)</td><td>$(FP $high.main_p)</td></tr>"
  }
}

# 표 2: 홍조군별 OR과 상호작용 p (상호작용 모형)
$t2 = foreach ($sex in '남자','여자') {
  $first = $true
  foreach ($m in 'Crude','Model 1','Model 2','Model 3') {
    $mid  = $rows | Where-Object { $_.sex -eq $sex -and $_.model -eq $m -and $_.exposure -eq '중음주' }
    $high = $rows | Where-Object { $_.sex -eq $sex -and $_.model -eq $m -and $_.exposure -eq '고음주' }
    $sexCell = if ($first) { "<td rowspan='4' class='sex'>$sex</td>" } else { '' }
    $first = $false
    "<tr>$sexCell<td class='l'>$($modelLabel[$m])</td><td>$(ORCI $mid.nonflush_or $mid.nonflush_lo $mid.nonflush_hi)</td><td>$(ORCI $high.nonflush_or $high.nonflush_lo $high.nonflush_hi)</td><td>$(ORCI $mid.flush_or $mid.flush_lo $mid.flush_hi)</td><td>$(ORCI $high.flush_or $high.flush_lo $high.flush_hi)</td><td>$(FP $mid.p_interaction)</td></tr>"
  }
}

$html = @"
<!doctype html><html lang="ko"><head><meta charset="utf-8"><title>보정변수 보정 결과표</title>
<style>
@page { size: A4 landscape; margin: 12mm; }
body { font-family: 'Malgun Gothic', sans-serif; font-size: 10pt; color: #111; }
h1 { font-size: 16pt; margin: 0 0 4px; } h2 { font-size: 12.5pt; margin: 10px 0 6px; }
.sub { color: #444; margin: 0 0 10px; }
table { border-collapse: collapse; width: 100%; margin-bottom: 6px; }
th, td { border-top: 1px solid #999; border-bottom: 1px solid #999; padding: 3px 7px; text-align: center; vertical-align: middle; }
th { background: #eef2f7; font-weight: 600; } td.l { text-align: left; } td.sex { font-weight: 600; background: #fafafa; }
.s { color: #555; font-size: 8.5pt; font-weight: normal; }
.note { font-size: 8.5pt; color: #333; margin: 2px 0; line-height: 1.45; }
h2.p2 { break-before: page; } .box { border: 1px solid #ccc; background: #fbfbfb; padding: 8px 10px; margin-top: 10px; }
</style></head><body>
<h1>보정변수 단계별 보정 결과표: 음주 수준과 고요산혈증</h1>
<p class="sub">국민건강영양조사 2019–2021, 만 19세 이상 현재 음주자 · 복합표본 로지스틱 회귀 · 기준: 저음주(주 3.5잔 미만)</p>

<h2>표 1. 음주 수준별 고요산혈증 오즈비 (홍조군 구분 없이 본 주효과)</h2>
<table><tr><th>성별</th><th>모형 (보정변수)</th><th>저음주<br><span class="s">&lt;3.5잔/주</span></th><th>중음주 OR (95% CI)<br><span class="s">3.5–14잔/주</span></th><th>p</th><th>고음주 OR (95% CI)<br><span class="s">≥14잔/주</span></th><th>p</th></tr>
$($t1 -join "`n")
</table>
<p class="note">주효과 OR은 상호작용항 없이 음주 수준과 현재 홍조를 함께 넣은 모형의 값이다.</p>

<h2 class="p2">표 2. 현재 홍조군별 오즈비와 상호작용 검정</h2>
<table><tr><th rowspan="2">성별</th><th rowspan="2">모형 (보정변수)</th><th colspan="2">비홍조군 OR (95% CI)</th><th colspan="2">홍조군 OR (95% CI)</th><th rowspan="2">상호작용 p<br><span class="s">음주 수준 × 홍조</span></th></tr>
<tr><th>중음주</th><th>고음주</th><th>중음주</th><th>고음주</th></tr>
$($t2 -join "`n")
</table>
<p class="note">홍조군별 OR은 '음주 수준 × 현재 홍조' 상호작용항을 넣은 모형에서 구했다. 상호작용 p는 상호작용항 2개를 함께 본 설계 기반 Wald 검정이며, 0.05보다 작으면 홍조군에 따라 음주–고요산혈증 연관이 다르다는 뜻이다.</p>

<div class="box">
<p class="note"><b>모형별 보정변수</b> · Model 1: 연령 · Model 2(기본 6개): 연령, 교육수준, 소득, 조사연도, 흡연(현재흡연 여부), 유산소 신체활동 · Model 3(전체 10개): Model 2 + 체질량지수, 고혈압, 당뇨병, eGFR</p>
<p class="note"><b>정의</b> · 고요산혈증: 혈청 요산 남자 ≥7.0, 여자 ≥6.0 mg/dL · 현재 홍조: 음주 후 얼굴 붉어짐 '가끔·자주·늘'</p>
<p class="note"><b>표본과 가중치</b> · n은 가중치를 적용하지 않은 분석 대상 수이고, OR·95% CI·p는 복합표본설계(가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu)를 반영한 추정치이다. 네 모형은 모두 같은 대상자로 적합했다.</p>
<p class="note"><b>출처</b> · 수치는 팀 분석 코드 code/13_v3_main_analysis.R의 결과 파일 output/13_v3_main_analysis/12a_four_models.csv를 그대로 옮긴 것이다(이 표를 만들면서 새로 계산하지 않음). 표 생성: code/25_adjusted_or_table_pdf.ps1</p>
<p class="note"><b>분석 버전</b> · 이 표는 연구계획서 V3 기준 분석(노출: 현재 홍조 2군, 보정변수 10개, 폐경 상태 미포함)의 결과이다. 2026-11-16에 개정된 연구 질문(주질문: 지속 홍조 vs 비홍조)과 여성 폐경 상태 보정은 반영되지 않았다.</p>
<p class="note"><b>AI 사용</b> · 결과 파일의 수치를 표로 옮기고 PDF로 만드는 과정에 생성형 AI(Claude)를 사용했다. 수치는 바꾸지 않았다.</p>
<p class="note"><b>참고</b> · 이 결과는 보정변수 10개 기준이다. 교육수준·소득·조사연도를 뺀 7개 보정 모형은 아직 분석하지 않았다. 참고로 이 세 변수를 하나씩 넣었을 때 고음주 OR의 변화는 3% 안팎이었다(output/13_v3_main_analysis/13_change_in_estimate.csv).</p>
</div>
</body></html>
"@

$tmpHtml = Join-Path $env:TEMP 'adjusted_or_table.html'
[IO.File]::WriteAllText($tmpHtml, $html, (New-Object System.Text.UTF8Encoding($false)))
$pdf = Join-Path $out '2026-11-16_보정변수_보정결과표(V3_이전기준).pdf'
$edge = @("${env:ProgramFiles(x86)}\Microsoft\Edge\Application\msedge.exe", "$env:ProgramFiles\Microsoft\Edge\Application\msedge.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1
& $edge --headless=new --disable-gpu --no-pdf-header-footer "--print-to-pdf=$pdf" "file:///$($tmpHtml -replace '\\','/')" | Out-Null
Start-Sleep -Seconds 2
Get-Item $pdf | Select-Object FullName, Length
