FPC = fpc

.PHONY: all clean

all: bin/xml2txt

bin/xml2txt: src/*.pas
	$(FPC) -Mobjfpc -Sh -Fusrc -FUobj -FEbin src/xml2txt.pas

clean:
	rm -f obj/* bin/*
