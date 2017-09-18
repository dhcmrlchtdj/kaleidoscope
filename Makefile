chs := $(wildcard ch*)

all: $(chs)

$(chs):
	-jbuilder build $@/kaleidoscope.{bc,exe}

clean:
	-jbuilder clean

.PHONY: all $(chs) clean
