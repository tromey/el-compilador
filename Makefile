EMACS := /home/tromey/Emacs/install/bin/emacs

HERE := $(shell pwd)

all:
	$(EMACS) --batch --eval '(push "$(HERE)" load-path)' \
		--eval '(byte-recompile-directory "." 0)'

clean:
	rm *.elc */*.elc
