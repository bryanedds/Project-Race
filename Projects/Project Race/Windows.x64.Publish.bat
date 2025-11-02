dotnet build -c Release "../../Nu/Nu.Pipe/Nu.Pipe.fsproj"
dotnet publish -c Release -r win-x64 /p:PublishSingleFile=true /p:IncludeNativeLibrariesForSelfExtract=true --self-contained true
cd ".\bin\Release\net9.0\win-x64"
rename "assimp.dll" "assimp.dll.tmp" & REM for some reason, assimp.dll doesn't get folded into the .exe image, so we preserve it.
del *.dll
del *.pdb
del *.so
del *.xml
rename "assimp.dll.tmp" "assimp.dll"
xcopy "..\Assets" ".\Assets" /E /I /Y
pause