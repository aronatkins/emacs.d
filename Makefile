all: compile
clean: contrib-clean aron-clean
compile: contrib-compile aron-compile

contrib-compile:
	$(MAKE) -C contrib compile

contrib-clean:
	$(MAKE) -C contrib clean

aron-compile:
	emacs --batch --eval '(progn (setq load-path (append load-path  (list "." "./contrib"))))' -f batch-byte-compile *.el

aron-clean:
	rm -f *~ *.elc

