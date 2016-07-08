@echo off
.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)
if not exist paket.lock (
  .paket\paket.exe install
) else (
  .paket\paket.exe restore
)
if errorlevel 1 (
  exit /b %errorlevel%
)
packages\FAKE\tools\FAKE.exe %* --fsiargs build.fsx