
default : mlc.exe

MLTON_FLAGS = @MLton max-heap 400M --

mlc.exe : makefile mlc.cm *.sml front/*.sml el/*.sml parser/*.sml util/*.sml cps/*.sml il/*.sml runtime/*.sml backend/*.sml ../sml-lib/util/*.sml ../sml-lib/algo/*.sml
	mlton $(MLTON_FLAGS) -output $@ mlc.mlb

# should remove some generated files in runtime/...
clean :
	rm -f `find . -name "*~"` *.exe

wc :
	find . -name "*.sml" | grep -v CM | xargs wc -l

linelen :
	linelen `find . -name "*.sml" | grep -v CM`
