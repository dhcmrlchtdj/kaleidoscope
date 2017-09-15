.PHONY: all ch2 ch3 ch4 ch5 ch6 ch7 ch8 clean

all: ch2 ch3 ch4 ch5 ch6 ch7 ch8

ch2:
	jbuilder build ch2/kaleidoscope.bc

ch3:
	jbuilder build ch3/kaleidoscope.bc

ch4:
	jbuilder build ch4/kaleidoscope.bc

ch5:
	jbuilder build ch5/kaleidoscope.bc

ch6:
	jbuilder build ch6/kaleidoscope.bc

ch7:
	jbuilder build ch7/kaleidoscope.bc

ch8:
	jbuilder build ch8/kaleidoscope.bc

clean:
	rm -r _build
