$env:TARGET_CC="clang"
$env:RUST_BACKTRACE="full"
$THIS=Get-Location

cargo build --release
Set-Location "tests"
./ci.ps1

Set-Location $THIS
foreach($i in Get-ChildItem "examples"){
    Set-Location $i.FullName
    ./../../target/release/zz --smt-timeout=200000 clean
    ./../../target/release/zz --smt-timeout=200000 run
    ./../../target/release/zz --smt-timeout=200000 test
}

Set-Location $THIS
foreach($i in Get-ChildItem "modules"){
    Set-Location $i.FullName
    ./../../target/release/zz --smt-timeout=200000 clean
    ./../../target/release/zz --smt-timeout=200000 test
}

Set-Location $THIS
Write-Output "all passed"
