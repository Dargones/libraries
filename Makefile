all : coverage

build :
	dotnet build dafny/Source/Dafny.sln
	wget https://github.com/Z3Prover/z3/releases/download/Z3-4.8.5/z3-4.8.5-x64-osx-10.14.2.zip
	unzip z3-4.8.5-x64-osx-10.14.2.zip
	mv -n z3-4.8.5-x64-osx-10.14.2 dafny/Binaries/z3
	rm z3-4.8.5-x64-osx-10.14.2.zip
	rm -rf z3-4.8.5-x64-osx-10.14.2

generate : build
	./generateTests.sh

compile : build
	dotnet dafny/Binaries/Dafny.dll /compileVerbose:1 /compile:0 /spillTargetCode:3 /noVerify src/AllSource.dfy
	dotnet dafny/Binaries/Dafny.dll /compileVerbose:1 /compile:0 /spillTargetCode:3 /noVerify tests/AllTests.dfy
	dotnet build src/AllSource.csproj

test: compile
	dotnet test -v:q -noLogo tests/Test.csproj

coverage : compile
	dotnet build DuplicateRemover/DuplicateRemover.sln
	dotnet DuplicateRemover/bin/Debug/net6.0/DuplicateRemover.dll tests/AllTests.cs src/AllSource.cs > testCoverage/OnlyTests.cs
	dotnet build TestedMethodLister/TestedMethodLister.sln
	dotnet TestedMethodLister/bin/Debug/net6.0/TestedMethodLister.dll tests/AllTests.cs > TestedMethodLister/TestedMethods.txt
	dotnet build testCoverage/TestCoverage.csproj
	dotnet test /p:CollectCoverage=true /p:CoverletOutputFormat=json testCoverage/TestCoverage.csproj
	python3 coverage.py testCoverage/coverage.json BoundedInts DivInternals Imaps Maps Math ModInternals MulInternals Power2 Power Seq Sets Uint8__16 Uint8__16_mUint8Seq Uint8__16_mUint16Seq Uint8__32 Uint8__32_mUint8Seq Uint8__32_mUint32Seq Uint8__64 Uint8__64_mUint8Seq Uint8__64_mUint64Seq Uint16__32 Uint16__32_mUint16Seq Uint16__32_mUint32Seq Uint32__64 Uint32__64_mUint32Seq Uint32__64_mUint64Seq Wrappers 

#############################

clean :
	rm -rf test/bin/
	rm -rf test/obj/
	rm -rf test/*.dfy
	rm -rf testCoverage/obj/
	rm -rf testCoverage/bin/
	rm -rf testCoverage/coverage.cobertura.xml
	rm -rf src/obj/
	rm -rf src/bin/

