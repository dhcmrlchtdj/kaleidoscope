chs := ch2 ch3 ch4 ch5 ch6 ch7 ch8

all: $(chs)

$(chs):
	-jbuilder build $@/kaleidoscope.bc

clean:
	rm -r _build

.PHONY: all $(chs) clean
