chs := ch2 ch3 ch4 ch5 ch6 ch7

all: $(chs)

$(chs):
	-jbuilder build --dev $@/kaleidoscope.bc

clean:
	rm -r _build

.PHONY: all $(chs) clean
