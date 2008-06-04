
default : mlc.exe

MLTON_FLAGS = @MLton max-heap 200M --

mlc.exe : mlc
	rm -f mlc.exe
	cp mlc mlc.exe

mlc : makefile mlc.cm *.sml front/*.sml el/*.sml parser/*.sml util/*.sml cps/*.sml il/*.sml runtime/*.sml backend/*.sml ../sml-lib/util/*.sml ../sml-lib/algo/*.sml
	mlton $(MLTON_FLAGS) mlc.cm

# should remove some generated files in runtime/...
clean :
	rm -f `find . -name "*~"` *.exe mlc

wc :
	find . -name "*.sml" | grep -v CM | xargs wc -l

linelen :
	linelen `find . -name "*.sml" | grep -v CM`
