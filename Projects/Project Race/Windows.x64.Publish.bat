dotnet publish -c Release -r win-x64 -p PublishSingleFile=true -p IncludeNativeLibrariesForSelfExtract=true -p PublishDir=./bin/Release/net10.0/win-x64 --self-contained true
cd ./bin/Release/net10.0/win-x64
rename "assimp.dll" "assimp.dll.tmp" & REM for some reason, assimp.dll doesn't get folded into the .exe image, so we preserve it.
del *.dll
del *.pdb
del *.so
del *.xml
rename "assimp.dll.tmp" "assimp.dll"
pause