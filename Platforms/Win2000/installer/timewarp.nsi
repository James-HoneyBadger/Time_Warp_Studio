; Time Warp IDE Installer Script
; NSIS 2.x Compatible (Windows 2000+)

!define APP_NAME "Time Warp IDE"
!define APP_VERSION "3.0.0"
!define APP_PUBLISHER "Time Warp IDE Project"
!define APP_URL "https://github.com/your-repo/time-warp-ide"
!define APP_EXE "TimeWarpIDE.exe"

Name "${APP_NAME} ${APP_VERSION}"
OutFile "TimeWarpIDE-Setup-${APP_VERSION}.exe"
InstallDir "$PROGRAMFILES\TimeWarp"
RequestExecutionLevel admin

Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

Section "Time Warp IDE"
  SetOutPath "$INSTDIR"
  
  ; Install files
  File "..\TimeWarpIDE.exe"
  File /nonfatal "..\README.md"
  File /nonfatal "..\LICENSE"
  
  ; Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  ; Create Start Menu shortcuts
  CreateDirectory "$SMPROGRAMS\${APP_NAME}"
  CreateShortcut "$SMPROGRAMS\${APP_NAME}\${APP_NAME}.lnk" "$INSTDIR\${APP_EXE}"
  CreateShortcut "$SMPROGRAMS\${APP_NAME}\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  CreateShortcut "$DESKTOP\${APP_NAME}.lnk" "$INSTDIR\${APP_EXE}"
  
  ; Write registry keys for Add/Remove Programs
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "DisplayName" "${APP_NAME}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "DisplayIcon" "$INSTDIR\${APP_EXE}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "Publisher" "${APP_PUBLISHER}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "DisplayVersion" "${APP_VERSION}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}" "NoRepair" 1
  
  ; File associations
  WriteRegStr HKCR ".twb" "" "TimeWarp.BASIC"
  WriteRegStr HKCR "TimeWarp.BASIC" "" "Time Warp BASIC File"
  WriteRegStr HKCR "TimeWarp.BASIC\shell\open\command" "" '"$INSTDIR\${APP_EXE}" "%1"'
  
  WriteRegStr HKCR ".twp" "" "TimeWarp.PILOT"
  WriteRegStr HKCR "TimeWarp.PILOT" "" "Time Warp PILOT File"
  WriteRegStr HKCR "TimeWarp.PILOT\shell\open\command" "" '"$INSTDIR\${APP_EXE}" "%1"'
  
  WriteRegStr HKCR ".twl" "" "TimeWarp.LOGO"
  WriteRegStr HKCR "TimeWarp.LOGO" "" "Time Warp Logo File"
  WriteRegStr HKCR "TimeWarp.LOGO\shell\open\command" "" '"$INSTDIR\${APP_EXE}" "%1"'
SectionEnd

Section "Uninstall"
  ; Remove files
  Delete "$INSTDIR\${APP_EXE}"
  Delete "$INSTDIR\README.md"
  Delete "$INSTDIR\LICENSE"
  Delete "$INSTDIR\Uninstall.exe"
  RMDir "$INSTDIR"
  
  ; Remove Start Menu shortcuts
  Delete "$SMPROGRAMS\${APP_NAME}\${APP_NAME}.lnk"
  Delete "$SMPROGRAMS\${APP_NAME}\Uninstall.lnk"
  RMDir "$SMPROGRAMS\${APP_NAME}"
  Delete "$DESKTOP\${APP_NAME}.lnk"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}"
  DeleteRegKey HKCR ".twb"
  DeleteRegKey HKCR "TimeWarp.BASIC"
  DeleteRegKey HKCR ".twp"
  DeleteRegKey HKCR "TimeWarp.PILOT"
  DeleteRegKey HKCR ".twl"
  DeleteRegKey HKCR "TimeWarp.LOGO"
SectionEnd
