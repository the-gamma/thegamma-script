@echo off
.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)
if not exist paket.lock (
  .paket\paket.exe install -v
) else (
  .paket\paket.exe restore -v
)
if errorlevel 1 (
  exit /b %errorlevel%
)
packages\FAKE\tools\FAKE.exe %* --fsiargs build.fsx