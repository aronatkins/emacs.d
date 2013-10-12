EL=js2-mode.el p4.el java-mode-indent-annotations.el
# markdown-mode doesn't compile in batch for some reason.
# markdown-mode.el:3750:1:Error: Invalid modifier in string
# markdown-mode.el
ELC=$(EL:.el=.elc)

all: compile

compile: ${ELC}

# The p4.el requires a perforce executable at load time. That sucks.
p4.elc: p4.el
	if [ -x p4 ] ; then \
		emacs --batch -f batch-byte-compile $< ; \
	fi

.el.elc:
	emacs --batch -f batch-byte-compile $<

clean:
	rm -f *~ ${ELC}
