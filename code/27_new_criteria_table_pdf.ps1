# 새 기준(2026-11-16 개정 연구 질문) 보정 결과표를 PDF로 만든다.
# 입력: output/26_new_criteria_or_table/ (code/26_new_criteria_or_table.R의 결과, 여기서는 새로 계산하지 않음)
# 출력: Result/2026-11-16_보정변수_보정결과표(새기준_후보절단점).pdf
# 실행: 저장소 최상위 폴더에서  powershell -File code/27_new_criteria_table_pdf.ps1
# PDF 변환에는 Microsoft Edge의 headless 인쇄 기능을 쓴다.

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$in   = Join-Path $root 'output\26_new_criteria_or_table'
$out  = Join-Path $root 'Result'
$ors   = Import-Csv (Join-Path $in '01_or_by_flush_history.csv') -Encoding UTF8
$tests = Import-Csv (Join-Path $in '02_interaction_tests.csv') -Encoding UTF8
$cells = Import-Csv (Join-Path $in '03_cell_counts.csv') -Encoding UTF8
$crit  = Import-Csv (Join-Path $in '00_criteria.csv') -Encoding UTF8

function F2($x) { '{0:N2}' -f [double]$x }
function FP($x) { $v = [double]$x; if ($v -lt 0.001) { '&lt;0.001' } else { '{0:N3}' -f $v } }
function PCell($x) { $v = [double]$x; $s = FP $x; if ($v -lt 0.05) { "<b>$s</b>" } else { $s } }
$models = 'Crude', 'Model 1', 'Model 2', 'Model 3'
$flushes = '비홍조', '홍조 소실', '지속 홍조'
$sexes = '남자', '여자'
$mhead = "<th>Crude<br><span class='s'>보정 없음</span></th><th>Model 1<br><span class='s'>연령</span></th><th class='main'>Model 2 (주 모형)<br><span class='s'>기본</span></th><th>Model 3<br><span class='s'>기본 + 추가</span></th>"

$sections = foreach ($c in $crit) {
  $cn = $c.criteria; $lv = $c.levels -split ' / '; $ref = $lv[0]
  # 칸 사례 10명 미만 표시용
  $small = @{}; foreach ($r in ($cells | Where-Object { $_.criteria -eq $cn -and [int]$_.cases -lt 10 })) { $small["$($r.sex)|$($r.flush)"] = $true }
  # 표 A: 홍조 이력군별 OR
  $rowsA = foreach ($s in $sexes) {
    $nRows = $flushes.Count * ($lv.Count - 1); $firstSex = $true
    foreach ($g in $flushes) {
      $firstG = $true
      foreach ($L in $lv[1..($lv.Count - 1)]) {
        $sexCell = if ($firstSex) { "<td rowspan='$nRows' class='sex'>$s</td>" } else { '' }; $firstSex = $false
        $mark = if ($small["$s|$g"]) { ' †' } else { '' }
        $gCell = if ($firstG) { "<td rowspan='$($lv.Count - 1)' class='l'>$g$mark</td>" } else { '' }; $firstG = $false
        $cellsHtml = foreach ($m in $models) {
          $r = $ors | Where-Object { $_.criteria -eq $cn -and $_.sex -eq $s -and $_.model -eq $m -and $_.flush -eq $g -and $_.drink -eq $L }
          $cls = if ($m -eq 'Model 2') { " class='main'" } else { '' }
          "<td$cls>$(F2 $r.or) ($(F2 $r.lo)–$(F2 $r.hi))<br><span class='s'>p = $(PCell $r.p)</span></td>"
        }
        "<tr>$sexCell$gCell<td>$L</td>$($cellsHtml -join '')</tr>"
      }
    }
  }
  # 표 B: 상호작용 검정 p
  $rowsB = foreach ($s in $sexes) {
    $firstSex = $true
    foreach ($q in '주질문: 지속 홍조 vs 비홍조', '부질문 1: 홍조 소실 vs 비홍조') {
      $sexCell = if ($firstSex) { "<td rowspan='2' class='sex'>$s</td>" } else { '' }; $firstSex = $false
      $g = if ($q -like '주질문*') { '지속 홍조' } else { '홍조 소실' }
      $mark = if ($small["$s|$g"] -or $small["$s|비홍조"]) { ' †' } else { '' }
      $cellsHtml = foreach ($m in $models) {
        $r = $tests | Where-Object { $_.criteria -eq $cn -and $_.sex -eq $s -and $_.model -eq $m -and $_.question -eq $q }
        $cls = if ($m -eq 'Model 2') { " class='main'" } else { '' }
        "<td$cls>$(PCell $r.p)</td>"
      }
      $df = ($tests | Where-Object { $_.criteria -eq $cn -and $_.question -eq $q } | Select-Object -First 1).df
      "<tr>$sexCell<td class='l'>$q$mark</td><td>$df</td>$($cellsHtml -join '')</tr>"
    }
  }
  # 표 C: 칸별 비가중 n (사례)
  $rowsC = foreach ($s in $sexes) {
    $firstSex = $true
    foreach ($g in $flushes) {
      $sexCell = if ($firstSex) { "<td rowspan='3' class='sex'>$s</td>" } else { '' }; $firstSex = $false
      $vals = foreach ($L in $lv) { $r = $cells | Where-Object { $_.criteria -eq $cn -and $_.sex -eq $s -and $_.flush -eq $g -and $_.drink -eq $L }; $t = "{0:N0} ({1:N0})" -f [int]$r.n, [int]$r.cases; if ([int]$r.cases -lt 10) { "<td class='warn'>$t</td>" } else { "<td>$t</td>" } }
      "<tr>$sexCell<td class='l'>$g</td>$($vals -join '')</tr>"
    }
  }
  $nm = ($ors | Where-Object { $_.criteria -eq $cn -and $_.sex -eq '남자' -and $_.model -eq 'Model 2' } | Select-Object -First 1).n_model
  $nf = ($ors | Where-Object { $_.criteria -eq $cn -and $_.sex -eq '여자' -and $_.model -eq 'Model 2' } | Select-Object -First 1).n_model
  $lvHead = ($lv | ForEach-Object { "<th>$_</th>" }) -join ''
  @"
<section>
<h2>기준: $cn</h2>
<p class="sub">음주 수준(주당 잔수): 남자 $($c.male -replace ',', ' · ') / 여자 $($c.female -replace ',', ' · ') 를 경계로 $($lv -join ' · ') ($($lv.Count)군). 기준범주: $ref. 모형 n(비가중, Model 2): 남 $('{0:N0}' -f [int]$nm) · 여 $('{0:N0}' -f [int]$nf)</p>
<h3>표 A. 홍조 이력군별 고요산혈증 오즈비 ($ref 대비)</h3>
<table><tr><th>성별</th><th>홍조 이력</th><th>음주 수준</th>$mhead</tr>
$($rowsA -join "`n")
</table>
<h3>표 B. 상호작용 검정 (홍조 이력군 사이에 음주–고요산혈증 연관이 다른가)</h3>
<table><tr><th>성별</th><th>질문</th><th>df</th>$mhead</tr>
$($rowsB -join "`n")
</table>
<h3>표 C. 칸별 비가중 n (고요산혈증 사례 수)</h3>
<table class="narrow"><tr><th>성별</th><th>홍조 이력</th>$lvHead</tr>
$($rowsC -join "`n")
</table>
</section>
"@
}

$html = @"
<!doctype html><html lang="ko"><head><meta charset="utf-8"><title>보정 결과표(새 기준)</title>
<style>
@page { size: A4 landscape; margin: 11mm; }
body { font-family: 'Malgun Gothic', sans-serif; font-size: 9.5pt; color: #111; }
h1 { font-size: 15pt; margin: 0 0 3px; } h2 { font-size: 12.5pt; margin: 4px 0 2px; color: #1d3c6e; } h3 { font-size: 10.5pt; margin: 8px 0 4px; }
.sub { color: #444; margin: 0 0 4px; font-size: 9pt; }
table { border-collapse: collapse; width: 100%; margin-bottom: 4px; } table.narrow { width: 70%; }
th, td { border-top: 1px solid #aaa; border-bottom: 1px solid #aaa; padding: 2px 6px; text-align: center; vertical-align: middle; }
th { background: #eef2f7; font-weight: 600; } th.main { background: #dce7f5; } td.main { background: #f3f7fc; }
td.l { text-align: left; } td.sex { font-weight: 600; background: #fafafa; } td.warn { color: #a33; }
.s { color: #555; font-size: 8pt; font-weight: normal; }
.note { font-size: 8.3pt; color: #333; margin: 2px 0; line-height: 1.4; }
.box { border: 1px solid #ccc; background: #fbfbfb; padding: 6px 9px; margin-top: 6px; }
.flag { border: 1px solid #e0b4b4; background: #fff6f6; padding: 6px 9px; margin: 4px 0 8px; font-size: 8.8pt; }
section { break-before: page; } section:first-of-type { break-before: auto; }
</style></head><body>
<h1>보정변수 보정 결과표 (새 기준: 2026-11-16 개정 연구 질문)</h1>
<p class="sub">국민건강영양조사 2019–2021, 만 19세 이상 현재 음주자 · 남녀 각각 복합표본 로지스틱 회귀 · 홍조 이력 3군(비홍조 기준 · 홍조 소실 · 지속 홍조, 신규 홍조 제외)</p>
<div class="flag"><b>⚠ 음주 수준 절단점은 아직 조원 논의 중입니다.</b> 이 문서는 README 4.2의 <b>후보 기준 두 가지</b>를 모두 계산해 나란히 보여 줍니다. 결과를 보고 기준을 고르면 사후 선택이 되므로, 기준은 결과와 상관없는 근거로 정하고 시도한 기준을 모두 보고해야 합니다.</div>
<div class="box">
<p class="note"><b>질문</b> · 주질문: 남녀 각각에서 성별 기준 음주 수준과 고요산혈증의 관계가 <b>비홍조군과 지속 홍조군</b> 사이에 다른가 · 부질문 1: 같은 관계가 <b>홍조 소실군과 비홍조군</b> 사이에 다른가 (README 3절)</p>
<p class="note"><b>모형</b> · Crude: 보정 없음 · Model 1: 연령 + 폐경 상태(여자) · <b>Model 2(주 모형)</b>: 연령, 교육수준, 소득, 조사연도, 현재흡연, 유산소 신체활동 + 폐경 상태(여자) · Model 3(민감도): Model 2 + 체질량지수, 고혈압, 당뇨병, eGFR. 모든 모형에 '음주 수준 × 홍조 이력' 상호작용항을 넣었다.</p>
<p class="note"><b>읽는 법</b> · 표 A의 OR은 각 홍조 이력군 안에서 가장 낮은 음주 수준 대비 고요산혈증의 오즈비이다. 표 B의 p가 0.05보다 작으면 그 두 홍조군 사이에 음주–고요산혈증 연관이 다르다는 뜻이다(상호작용항 공동 Wald 검정). 굵은 p는 0.05 미만. † 표시는 고요산혈증 사례가 10명 미만인 칸이 있어 추정이 불안정하므로 해석하지 않는다.</p>
</div>
$($sections -join "`n")
<div class="box">
<p class="note"><b>정의</b> · 고요산혈증: 혈청 요산 남자 ≥7.0, 여자 ≥6.0 mg/dL · 주당 음주 잔수 = 음주 빈도 대표값 × 1회 음주량 대표값(10잔 이상은 기입값) · 폐경 상태: LW_ms(8기 지침서 PDF p.190)로 폐경 전·폐경 후·기타, 여자 모형에만 넣음(모름·무응답 여성 2명은 그 모형에서 빠짐)</p>
<p class="note"><b>표본과 가중치</b> · n은 가중치를 적용하지 않은 수이고, OR·95% CI·p는 복합표본설계(가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu)를 반영한 추정치이다. 최종 분석 대상 10,779명(남 5,411 · 여 5,368) 중 홍조 이력 3군에 속하는 10,078명을 분석했다.</p>
<p class="note"><b>여러 번 검정한 점</b> · 후보 기준 2개 × 성별 2개 × 질문 2개 × 모형 4개로 검정이 많다. p &lt; 0.05가 몇 개 나와도 우연일 수 있으므로 주 모형(Model 2)과 미리 정한 기준 중심으로 해석한다.</p>
<p class="note"><b>출처</b> · 수치: code/26_new_criteria_or_table.R → output/26_new_criteria_or_table/(01_or_by_flush_history.csv, 02_interaction_tests.csv, 03_cell_counts.csv). 자료 준비는 code/13_v3_main_analysis.R의 0–3절을 그대로 실행했고, 같은 환경에서 기존 결과(12a, 남자 Model 3 고음주 OR 1.5395)를 똑같이 재현함을 확인했다. 표 생성: code/27_new_criteria_table_pdf.ps1</p>
<p class="note"><b>AI 사용</b> · 분석 코드 작성·실행과 표·PDF 작성에 생성형 AI(Claude)를 사용했다. 조원 검토가 필요하다.</p>
</div>
</body></html>
"@

$tmpHtml = Join-Path $env:TEMP 'new_criteria_table.html'
[IO.File]::WriteAllText($tmpHtml, $html, (New-Object System.Text.UTF8Encoding($false)))
$pdf = Join-Path $out '2026-11-16_보정변수_보정결과표(새기준_후보절단점).pdf'
$edge = @("${env:ProgramFiles(x86)}\Microsoft\Edge\Application\msedge.exe", "$env:ProgramFiles\Microsoft\Edge\Application\msedge.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1
& $edge --headless=new --disable-gpu --no-pdf-header-footer "--print-to-pdf=$pdf" "file:///$($tmpHtml -replace '\\','/')" | Out-Null
Start-Sleep -Seconds 2
Get-Item -LiteralPath $pdf | Select-Object FullName, Length
