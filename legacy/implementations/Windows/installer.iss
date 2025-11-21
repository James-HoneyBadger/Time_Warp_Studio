[Setup]
AppName=Time Warp IDE
AppVersion=1.0.0
DefaultDirName={pf}\TimeWarpIDE
DefaultGroupName=Time Warp IDE
OutputBaseFilename=TimeWarpSetup
Compression=lzma
SolidCompression=yes

[Files]
Source: "target\release\time_warp_windows.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "TimeWarp.ico"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\Time Warp IDE"; Filename: "{app}\time_warp_windows.exe"; IconFilename: "{app}\TimeWarp.ico"
