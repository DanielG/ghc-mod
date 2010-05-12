SRCS = ghc.el ghc-func.el ghc-doc.el ghc-comp.el ghc-flymake.el ghc-command.el
EMACS = emacs

TEMPFILE  = temp.el

all: $(TEMPFILE) ghc.el
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f ghc-compile
	rm -f $(TEMPFILE)

$(TEMPFILE):
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE)
	@echo '(defun ghc-compile () (mapcar (lambda (x) (byte-compile-file x)) (list ' >> $(TEMPFILE)
	@echo $(SRCS)| sed -e 's/\(ghc[^ ]*\.el\)/"\1"/g' >> $(TEMPFILE)
	@echo ')))' >> $(TEMPFILE)

clean:
	rm -f *.elc $(TEMPFILE)
