$env:TARGET_CC="clang"
$env:RUST_BACKTRACE="full"
$THIS=Get-Location

cargo build --release

Set-Location $THIS
foreach($i in Get-ChildItem "mustpass" -Directory){
    Set-Location $i.FullName
    ./../../../target/release/zz clean
    ./.././../../target/release/zz run
    if ($LastExitCode -eq 0){
        Write-Output "$i passed"
    } else {
        Write-Output "$i failed"
        Set-Location $THIS
        exit 1
    }
}

Set-Location $THIS
foreach($i in Get-ChildItem "mustfail" -Directory){
    Set-Location $i.FullName
    ./../../../target/release/zz clean
    ./.././../../target/release/zz check
    if ($LastExitCode -eq 0){
        Write-Output "$i passed, but it should not"
        Set-Location $THIS
        exit 1
    } else {
        Write-Output "$i failed to build, as it's supposed to"       
    }
}

Set-Location $THIS
Write-Output "all passed"
