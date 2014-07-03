SRCS = ghc.el ghc-func.el ghc-doc.el ghc-comp.el ghc-check.el ghc-process.el \
       ghc-command.el ghc-info.el ghc-ins-mod.el ghc-indent.el ghc-rewrite.el
EMACS = emacs
DETECT = xemacs

TEMPFILE  = temp.el

all: $(TEMPFILE) ghc.el
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f ghc-compile
	rm -f $(TEMPFILE)

detect: $(TEMPFILE) ghc.el
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f ghc-compile
	rm -f $(DETECT)

$(TEMPFILE):
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE)
	@echo '(defun ghc-compile () (mapcar (lambda (x) (byte-compile-file x)) (list ' >> $(TEMPFILE)
	@echo $(SRCS)| sed -e 's/\(ghc[^ ]*\.el\)/"\1"/g' >> $(TEMPFILE)
	@echo ')))' >> $(TEMPFILE)

clean:
	rm -f *.elc $(TEMPFILE)

VERSION = `grep version ghc.el | sed -e 's/[^0-9\.]//g'`

bump:
	echo "(define-package\n  \"ghc-mod\"\n  $(VERSION)\n  \"Sub mode for Haskell mode\"\n  nil)" > ghc-pkg.el

archive:
	git archive master -o ~/ghc-$(VERSION).tar --prefix=ghc-$(VERSION)/
