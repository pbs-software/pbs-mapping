all: clipPolys.exe findPolys.exe convUL.exe
	

clipPolys.exe: clipPolys.cpp
	g++ clipPolys.cpp -o clipPolys.exe

convUL.exe: convUL.cpp
	g++ convUL.cpp -o convUL.exe

findPolys.exe: findPolys.cpp
	g++ findPolys.cpp -o findPolys.exe

clean:
	@echo Cleaning files
	@rm -f *.o
	@rm -f *.bak
	@rm -f findPolys.exe
	@rm -f clipPolys.exe
	@rm -f convUL.exe