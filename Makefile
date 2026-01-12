UNAME=$(shell uname)
ifeq ("$(UNAME)","Darwin")
  EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
else
  EMACS=emacs
endif

all: clean install

install:
	${EMACS} --script packages.el
	${EMACS} --batch -l init.el

clean:
	rm -rf elpa tree-sitter
