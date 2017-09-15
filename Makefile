# .PHONY: all ch2 ch3 ch4 ch5 ch6 ch7 ch8 clean

all: ch2 ch3 ch4 ch5 ch6 ch7 ch8

_build/default/%/kaleidoscope.bc: %/kaleidoscope.ml
	-jbuilder build $(subst .ml,.bc,$<)

ch2: _build/default/ch2/kaleidoscope.bc
ch3: _build/default/ch3/kaleidoscope.bc
ch4: _build/default/ch4/kaleidoscope.bc
ch5: _build/default/ch5/kaleidoscope.bc
ch6: _build/default/ch6/kaleidoscope.bc
ch7: _build/default/ch7/kaleidoscope.bc
ch8: _build/default/ch8/kaleidoscope.bc

clean:
	rm -r _build
