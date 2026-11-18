# 결과표 빈 틀(숫자 없는 표 양식)을 PDF와 엑셀로 만든다.
# 근거: README 3절(연구 질문)·4.2(변수)·4.3(분석 방법), 2026-11-16 개정 기준
# 출력: Result/2026-11-18_결과표_빈틀.pdf, Result/2026-11-18_결과표_빈틀.xlsx
# 실행: 저장소 최상위 폴더에서  powershell -File code/28_shell_tables.ps1
# PDF는 Microsoft Edge headless 인쇄, 엑셀은 Excel COM으로 만든다. 숫자는 넣지 않는다(분석 코드로 나중에 채움).

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$out  = Join-Path $root 'Result'
$tmp  = Join-Path $env:TEMP 'shell_tables'; New-Item -ItemType Directory -Force $tmp | Out-Null
$enc  = New-Object System.Text.UTF8Encoding($false)
$lv   = '저음주', '중음주', '고음주'      # 절단점 확정 전: 이름만 둔다
$models = 'Model 1', 'Model 2', 'Model 3'
$mdesc = @{ 'Model 1' = '연령 + 폐경(여)'; 'Model 2' = '주 모형: 기본 보정'; 'Model 3' = '민감도: 기본 + 임상' }

# ---- 행 만드는 도구 ----
function Emp([int]$n) { '<td></td>' * $n }
function Row([string]$label, [int]$n, [string]$cls = '') { "<tr class='$cls'><td class='l'>$label</td>$(Emp $n)</tr>" }
function Grp([string]$label, [int]$n) { "<tr class='grp'><td class='l' colspan='$($n + 1)'><b>$label</b></td></tr>" }
function RowW([string]$label) { "<tr><td class='l'>$label</td><td class='na'>—</td><td class='na'>—</td><td class='na'>—</td><td class='na'>—</td>$(Emp 4)</tr>" }  # 여성만 해당

# ---- 표 0. 포함·제외 비교표 ----
$t0 = @(
  "<tr><td class='l'>인원 (비가중 n)</td><td>10,779</td><td>438</td><td class='na'>—</td></tr>"
  Grp '사회인구' 3; Row '&nbsp;&nbsp;남자, n (가중 %)' 3; Row '&nbsp;&nbsp;여자, n (가중 %)' 3; Row '&nbsp;&nbsp;나이(세), 평균 (SE)' 3
  Row '&nbsp;&nbsp;조사연도 2019, n (가중 %)' 3; Row '&nbsp;&nbsp;조사연도 2020' 3; Row '&nbsp;&nbsp;조사연도 2021' 3
  Grp '노출·홍조' 3; Row '&nbsp;&nbsp;주당 음주 잔 수, 평균 (SE)' 3
  foreach ($l in $lv) { Row "&nbsp;&nbsp;$l (절단점 확정 후), n (가중 %)" 3 }
  Row '&nbsp;&nbsp;비홍조' 3; Row '&nbsp;&nbsp;홍조 소실' 3; Row '&nbsp;&nbsp;지속 홍조' 3; Row '&nbsp;&nbsp;신규 홍조' 3
  Grp '결과' 3; Row '&nbsp;&nbsp;혈청 요산(mg/dL), 평균 (SE)' 3; Row '&nbsp;&nbsp;고요산혈증, n (가중 %)' 3
  Grp '제외 사유: 빠진 보정변수 (제외자 438명 중, 중복 가능)' 3
  foreach ($v in '교육수준', '소득', '흡연', '유산소 신체활동', '체질량지수', '고혈압', '당뇨병', 'eGFR(크레아티닌)') { "<tr><td class='l'>&nbsp;&nbsp;$v 결측, n</td><td class='na'>—</td><td></td><td class='na'>—</td></tr>" }
) -join "`n"
$h0 = "<tr><th>특성</th><th>분석 대상<br><span class='s'>(n = 10,779)</span></th><th>보정변수 결측으로 제외<br><span class='s'>(n = 438)</span></th><th>p</th></tr>"

# ---- 표 1. 대상자 특성 ----
$h1 = @"
<tr><th rowspan='2'>특성</th><th colspan='4'>남자 (n = 5,110)</th><th colspan='4'>여자 (n = 4,968)</th></tr>
<tr><th>비홍조<br><span class='s'>n = 3,181</span></th><th>홍조 소실<br><span class='s'>n = 341</span></th><th>지속 홍조<br><span class='s'>n = 1,588</span></th><th>p</th><th>비홍조<br><span class='s'>n = 3,273</span></th><th>홍조 소실<br><span class='s'>n = 236</span></th><th>지속 홍조<br><span class='s'>n = 1,459</span></th><th>p</th></tr>
"@
$t1 = @(
  Grp '사회인구' 8; Row '&nbsp;&nbsp;나이(세), 평균 (SE)' 8
  foreach ($a in '19–29세', '30–39세', '40–49세', '50–59세', '60–69세', '70세 이상') { Row "&nbsp;&nbsp;$a, n (가중 %)" 8 }
  foreach ($a in '초졸 이하', '중졸', '고졸', '대졸 이상') { Row "&nbsp;&nbsp;교육: $a" 8 }
  foreach ($a in '하', '중하', '중상', '상') { Row "&nbsp;&nbsp;소득 4분위: $a" 8 }
  foreach ($a in '2019', '2020', '2021') { Row "&nbsp;&nbsp;조사연도: $a" 8 }
  Grp '건강행태' 8; Row '&nbsp;&nbsp;현재흡연' 8; Row '&nbsp;&nbsp;유산소 신체활동 실천' 8
  RowW '&nbsp;&nbsp;폐경 상태: 폐경 전'; RowW '&nbsp;&nbsp;폐경 상태: 폐경 후'; RowW '&nbsp;&nbsp;폐경 상태: 기타'
  Grp '임상' 8; Row '&nbsp;&nbsp;체질량지수(kg/m²), 평균 (SE)' 8; Row '&nbsp;&nbsp;고혈압 유병' 8; Row '&nbsp;&nbsp;당뇨병 유병' 8; Row '&nbsp;&nbsp;eGFR(mL/min/1.73m²), 평균 (SE)' 8
  Grp '노출' 8; Row '&nbsp;&nbsp;주당 음주 잔 수, 평균 (SE)' 8
  foreach ($l in $lv) { Row "&nbsp;&nbsp;$l (절단점 확정 후)" 8 }
  Grp '결과' 8; Row '&nbsp;&nbsp;혈청 요산(mg/dL), 평균 (SE)' 8; Row '&nbsp;&nbsp;고요산혈증 (남 ≥7.0, 여 ≥6.0 mg/dL)' 8
) -join "`n"

# ---- 표 2·3-1·S1: 두 홍조군의 OR을 나란히 보는 표 ----
function PairTable([string]$a, [string]$b, [bool]$womenFlag) {
  $rows = foreach ($s in '남자', '여자') {
    $mark = if ($womenFlag -and $s -eq '여자') { ' †' } else { '' }
    "<tr class='sexrow'><td class='l' colspan='8'><b>$s$mark</b></td></tr>"
    foreach ($m in $models) {
      $cls = if ($m -eq 'Model 2') { 'main' } else { '' }
      $first = $true
      foreach ($l in $lv) {
        $mc = if ($first) { "<td rowspan='3' class='l'>$m<br><span class='s'>$($mdesc[$m])</span></td>" } else { '' }
        $pc = if ($first) { "<td rowspan='3'></td>" } else { '' }
        $or = if ($l -eq '저음주') { "<td>1.00 (기준)</td>" } else { '<td></td>' }
        $ratio = if ($l -eq '저음주') { "<td class='na'>—</td>" } else { '<td></td>' }
        "<tr class='$cls'>$mc<td>$l</td><td></td>$or<td></td>$or$ratio$pc</tr>"
        $first = $false
      }
    }
  }
  $head = "<tr><th>모형</th><th>음주 수준</th><th>$a<br>n (사례)</th><th>$a<br>OR (95% CI)</th><th>$b<br>n (사례)</th><th>$b<br>OR (95% CI)</th><th>OR의 비<br><span class='s'>$b ÷ $a</span></th><th>상호작용 p<br><span class='s'>음주 수준 × 홍조</span></th></tr>"
  , @($head, ($rows -join "`n"))
}
$p2  = PairTable '비홍조' '지속 홍조' $false
$p31 = PairTable '비홍조' '홍조 소실' $true
$pS1 = PairTable '현재 비홍조' '현재 홍조' $false

# ---- 표 3-2. 부질문 2 ----
$h32 = "<tr><th>모형</th><th>남자: 고음주 OR의 비<br><span class='s'>지속 홍조 ÷ 비홍조</span></th><th>여자: 고음주 OR의 비<br><span class='s'>지속 홍조 ÷ 비홍조</span></th><th>df</th><th>성별 차이 p<br><span class='s'>음주 수준 × 홍조 × 성별</span></th></tr>"
$t32 = ($models | ForEach-Object { $cls = if ($_ -eq 'Model 2') { 'main' } else { '' }; "<tr class='$cls'><td class='l'>$_<br><span class='s'>$($mdesc[$_])</span></td>$(Emp 4)</tr>" }) -join "`n"

# ---- 표 S2. 여성 혈청 요산 선형회귀 ----
$hS2 = "<tr><th>홍조 이력</th><th>음주 수준</th><th>n</th><th>Model 1<br>β (95% CI), mg/dL</th><th class='mainh'>Model 2 (주 모형)<br>β (95% CI), mg/dL</th><th>Model 3<br>β (95% CI), mg/dL</th></tr>"
$tS2 = @(
  foreach ($g in '비홍조', '홍조 소실', '지속 홍조') { $first = $true; foreach ($l in $lv) {
    $gc = if ($first) { "<td rowspan='3' class='l'>$g</td>" } else { '' }; $first = $false
    $b = if ($l -eq '저음주') { '<td>0 (기준)</td><td>0 (기준)</td><td>0 (기준)</td>' } else { Emp 3 }
    "<tr>$gc<td>$l</td><td></td>$b</tr>" } }
  "<tr class='grp'><td class='l' colspan='3'><b>상호작용 p: 지속 홍조 vs 비홍조</b></td>$(Emp 3)</tr>"
  "<tr class='grp'><td class='l' colspan='3'><b>상호작용 p: 홍조 소실 vs 비홍조</b></td>$(Emp 3)</tr>"
) -join "`n"

# ---- 표 S3. 민감도 분석 ----
$hS3 = @"
<tr><th rowspan='2'>분석</th><th colspan='3'>남자</th><th colspan='3'>여자</th></tr>
<tr><th>비홍조 고음주 OR</th><th>지속 홍조 고음주 OR</th><th>상호작용 p</th><th>비홍조 고음주 OR</th><th>지속 홍조 고음주 OR</th><th>상호작용 p</th></tr>
"@
$sens = '주분석 (Model 2, 비교 기준)', '음주 빈도 대푯값을 작게 (월1회미만 = 월 0.25회, 주4회이상 = 주 4회)', '음주 빈도 대푯값을 크게 (월1회미만 = 월 0.75회, 주4회이상 = 주 7회)',
        '통풍 의사진단자 포함', "통풍 문항 '모름·무응답' 포함", '임신·수유 여성 제외', '흡연 3범주(비·과거·현재)로 보정', '고혈압·당뇨병을 의사진단 기준으로 보정(Model 3)'
$tS3 = ($sens | ForEach-Object { $cls = if ($_ -like '주분석*') { 'main' } else { '' }; "<tr class='$cls'><td class='l'>$_</td>$(Emp 6)</tr>" }) -join "`n"

# ---- 표 목록: 제목, 시트 이름, 머리글, 본문, 주석 ----
$noteOR = '저음주 대비 고요산혈증 오즈비(OR). n (사례)는 그 칸의 비가중 인원과 고요산혈증 사례 수. OR의 비는 두 홍조군 OR을 나눈 값으로 상호작용의 크기를 뜻한다. 상호작용 p는 음주 수준 × 홍조 상호작용항의 공동 Wald 검정.'
$tables = @(
  @{ id = '0';   sheet = '0_포함제외';   title = '표 0. 분석 대상과 보정변수 결측 제외자의 특성 비교'; head = $h0; body = $t0;
     note = '대상자 선정 흐름(README 4.1): 통풍 진단·모름 제외 후 11,217명 → 보정변수 결측 438명 제외 → 10,779명. 폐경 상태 결측 여성 2명은 폐경을 넣는 모형에서만 빠지므로 이 표의 제외자에 들어가지 않는다. p: 범주형 Rao-Scott χ², 연속형 설계 기반 Wald 검정.' }
  @{ id = '1';   sheet = '1_대상자특성'; title = '표 1. 대상자 특성 (성별 × 홍조 이력)'; head = $h1; body = $t1;
     note = '홍조 이력 3군(신규 홍조 남 301·여 400명은 제외). 열의 n은 README 4.2의 비가중 인원. 범주형은 n (가중 %), 연속형은 가중 평균 (SE). p: 범주형 Rao-Scott χ², 연속형 설계 기반 Wald 검정. 폐경 상태는 여성만 해당.' }
  @{ id = '2';   sheet = '2_주결과';     title = '표 2. 주결과: 비홍조군과 지속 홍조군의 음주 수준별 고요산혈증 오즈비 (주질문)'; head = $p2[0]; body = $p2[1]; note = $noteOR }
  @{ id = '3-1'; sheet = '3-1_부질문1';  title = '표 3-1. 부질문 1: 비홍조군과 홍조 소실군의 음주 수준별 고요산혈증 오즈비'; head = $p31[0]; body = $p31[1];
     note = "$noteOR † 여성 홍조 소실군은 236명으로 칸별 사례가 10명 미만일 수 있어, 그런 칸은 해석하지 않는다고 표시한다." }
  @{ id = '3-2'; sheet = '3-2_부질문2';  title = '표 3-2. 부질문 2: 지속 홍조와 비홍조의 차이가 남녀 사이에서 다른가'; head = $h32; body = $t32;
     note = '남녀 통합 모형에서 음주 수준 × 홍조 이력 × 성별 3원 상호작용을 검정한다(보정변수 효과도 성별마다 다르게 둠, 폐경은 여성 전용 지시변수). OR의 비는 최고 음주 수준(고음주) 기준.' }
  @{ id = 'S1';  sheet = 'S1_현재홍조';  title = '표 S1. (참고) 현재 홍조 2군: 계획 발표 때의 주분석'; head = $pS1[0]; body = $pS1[1]; note = "$noteOR 현재 홍조: 음주 후 얼굴 붉어짐 '가끔·자주·늘'. 신규 홍조도 현재 홍조에 포함된다." }
  @{ id = 'S2';  sheet = 'S2_여성요산';  title = '표 S2. (탐색) 여성: 음주 수준별 혈청 요산 차이 (선형회귀)'; head = $hS2; body = $tS2;
     note = 'β는 저음주 대비 혈청 요산의 평균 차이(mg/dL). 여성은 고요산혈증 사례가 적어 요산 수치 자체도 함께 본다(README 4.3 분석 ②).' }
  @{ id = 'S3';  sheet = 'S3_민감도';    title = '표 S3. 민감도 분석'; head = $hS3; body = $tS3;
     note = '행 목록은 예시이며 조원 논의로 확정한다(README 4.3: 대푯값·통풍 제외 기준 변경). 각 행은 주분석에서 한 가지만 바꾼 결과이다.' }
)

$css = @"
body { font-family: 'Malgun Gothic', sans-serif; font-size: 9.5pt; color: #111; }
h1 { font-size: 15pt; margin: 0 0 4px; } h2 { font-size: 12pt; margin: 2px 0 6px; color: #1d3c6e; }
table { border-collapse: collapse; width: 100%; } th, td { border: 1px solid #999; padding: 3px 6px; text-align: center; vertical-align: middle; height: 17px; }
th { background: #eef2f7; font-weight: 600; } th.mainh { background: #dce7f5; } td.l { text-align: left; } td.na { color: #999; }
tr.grp td { background: #f6f6f6; } tr.sexrow td { background: #e9eef6; } tr.main td { background: #f3f7fc; }
.s { color: #555; font-size: 8pt; font-weight: normal; } .note { font-size: 8.3pt; color: #333; margin: 4px 0 0; line-height: 1.4; }
.box { border: 1px solid #ccc; background: #fbfbfb; padding: 6px 9px; margin: 6px 0; } section { break-before: page; }
"@
$general = @"
<p class='note'><b>공통</b> · 국민건강영양조사 2019–2021, 만 19세 이상 현재 음주자. n은 가중치를 적용하지 않은 수, %·평균·OR·β·p는 복합표본설계(가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu)를 반영한 추정치. 남녀는 따로 분석한다.</p>
<p class='note'><b>모형</b> · Model 1: 연령 + 폐경 상태(여) · <b>Model 2(주 모형)</b>: 연령, 교육수준, 소득, 조사연도, 현재흡연, 유산소 신체활동 + 폐경 상태(여) · Model 3(민감도): Model 2 + 체질량지수, 고혈압, 당뇨병, eGFR. 모든 OR 모형에 '음주 수준 × 홍조' 상호작용항을 넣는다.</p>
<p class='note'><b>음주 수준</b> · 성별 기준 절단점은 조원 논의 중이다. 확정되면 '저·중·고'의 경계(주당 잔 수)를 표 제목 아래에 적는다. 2군 기준으로 정해지면 '중음주' 행을 지운다.</p>
<p class='note'><b>결과</b> · 고요산혈증: 혈청 요산 남 ≥7.0, 여 ≥6.0 mg/dL.</p>
<p class='note'><b>작성</b> · 숫자는 넣지 않은 빈 틀이다(결과를 보기 전에 표 양식을 정해 두기 위함). 생성형 AI(Claude)로 작성했고 조원 검토가 필요하다. 생성 코드: code/28_shell_tables.ps1</p>
"@

# ---- PDF ----
$sections = foreach ($t in $tables) { "<section><h2>$($t.title)</h2><table>$($t.head)`n$($t.body)</table><p class='note'>$($t.note)</p></section>" }
$html = "<!doctype html><html lang='ko'><head><meta charset='utf-8'><title>결과표 빈 틀</title><style>@page { size: A4 landscape; margin: 11mm; } $css section:first-of-type { break-before: auto; }</style></head><body><h1>결과표 빈 틀 (2026-11-16 개정 연구 질문 기준)</h1><div class='box'>$general</div>$($sections -join "`n")</body></html>"
$hp = Join-Path $tmp 'all.html'; [IO.File]::WriteAllText($hp, $html, $enc)
$pdf = Join-Path $out '2026-11-18_결과표_빈틀.pdf'
$edge = @("${env:ProgramFiles(x86)}\Microsoft\Edge\Application\msedge.exe", "$env:ProgramFiles\Microsoft\Edge\Application\msedge.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1
& $edge --headless=new --disable-gpu --no-pdf-header-footer "--print-to-pdf=$pdf" "file:///$($hp -replace '\\','/')" | Out-Null
Start-Sleep -Seconds 2

# ---- 엑셀: 표마다 시트 하나 ----
$xlsx = Join-Path $out '2026-11-18_결과표_빈틀.xlsx'
if (Test-Path -LiteralPath $xlsx) { Remove-Item -LiteralPath $xlsx -Confirm:$false }
$xl = New-Object -ComObject Excel.Application; $xl.Visible = $false; $xl.DisplayAlerts = $false
try {
  $wb = $xl.Workbooks.Add()
  $first = $wb.Worksheets.Item(1); $first.Name = '_tmp'
  $guide = "<html><head><meta charset='utf-8'></head><body><h1>결과표 빈 틀 (2026-11-16 개정 연구 질문 기준)</h1>$general<p>시트: $(($tables | ForEach-Object { $_.sheet }) -join ', ')</p></body></html>"
  $files = @(@{ sheet = '안내'; html = $guide }) + @($tables | ForEach-Object { @{ sheet = $_.sheet; html = "<html><head><meta charset='utf-8'><style>$css</style></head><body><h2>$($_.title)</h2><table border='1'>$($_.head)`n$($_.body)</table><p>$($_.note)</p></body></html>" } })
  foreach ($f in $files) {
    $p = Join-Path $tmp "$($f.sheet).html"; [IO.File]::WriteAllText($p, $f.html, $enc)
    $src = $xl.Workbooks.Open($p)
    $src.Worksheets.Item(1).Copy([Type]::Missing, $wb.Worksheets.Item($wb.Worksheets.Count))
    $src.Close($false)
    $ws = $wb.Worksheets.Item($wb.Worksheets.Count); $ws.Name = $f.sheet
    $ws.Cells.WrapText = $false; [void]$ws.Columns.AutoFit(); $ws.Columns.Item(1).ColumnWidth = 48
  }
  $first.Delete()
  $wb.Worksheets.Item(1).Activate()
  $wb.SaveAs($xlsx, 51); $wb.Close($false)
} finally { $xl.Quit(); [void][Runtime.InteropServices.Marshal]::ReleaseComObject($xl) }
Get-Item -LiteralPath $pdf, $xlsx | Select-Object Name, Length
