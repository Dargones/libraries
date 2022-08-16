all : coverage

build :
	dotnet build dafny/boogie/Source/Boogie.sln
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
	dotnet dafny/Binaries/Dafny.dll /compileVerbose:1 /compile:0 /spillTargetCode:3 /noVerify test/AllTests.dfy
	dotnet build src/AllSource.csproj

coverage : compile
	dotnet build DuplicateRemover/DuplicateRemover.sln
	dotnet DuplicateRemover/bin/Debug/net6.0/DuplicateRemover.dll test/AllTests.cs src/AllSource.cs > testCoverage/OnlyTests.cs
	dotnet build testCoverage/TestCoverage.csproj
	rm -rf testCoverage/TestResults/*
	export PATH=${PWD}/dafny/Binaries/:$PATH
	dotnet test -f net6.0 --collect:"XPlat Code Coverage" --settings runsettings.xml testCoverage/TestCoverage.csproj
	python3 MoveCoverage.py
	python3 FilterCoverage.py stats/ testCoverage/TestResults/coverage.cobertura.xml
	dotnet new tool-manifest --force
	dotnet tool install dotnet-reportgenerator-globaltool
	dotnet reportgenerator "-reports:testCoverage/TestResults/coverage.cobertura.xml" "-targetdir:testCoverage/TestResults" "-reporttypes:TextSummary;HtmlSummary;JsonSummary"
	python3 FormatTable.py stats testCoverage/TestResults/Summary.json


clean :
	rm -rf test/bin/
	rm -rf test/obj/
	rm -rf test/*.dfy
	rm -rf testCoverage/obj/
	rm -rf testCoverage/bin/
	rm -rf testCoverage/coverage.cobertura.xml
	rm -rf src/obj/
	rm -rf src/bin/

