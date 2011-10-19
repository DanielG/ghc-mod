SRCS = ghc.el ghc-func.el ghc-doc.el ghc-comp.el ghc-flymake.el \
       ghc-command.el ghc-info.el
EMACS = emacs

VERSION   = `grep -o "[0-9]\.[0-9]\.[0-9]" ghc.el`
PKGDIR    = ghc-$(VERSION)
PKGFILE   = ghc-pkg.el
PKG       = ghc.tar
TEMPFILE  = temp.el

all: $(TEMPFILE) ghc.el
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f ghc-compile
	rm -f $(TEMPFILE)

pkg: $(PKGFILE)
	cp -r ../elisp ../$(PKGDIR)
	cp ../README ../$(PKGDIR)
	tar cvf $(PKG) ../$(PKGDIR)
	rm -rf ../$(PKGDIR) $(PKGFILE)

$(TEMPFILE):
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE)
	@echo '(defun ghc-compile () (mapcar (lambda (x) (byte-compile-file x)) (list ' >> $(TEMPFILE)
	@echo $(SRCS)| sed -e 's/\(ghc[^ ]*\.el\)/"\1"/g' >> $(TEMPFILE)
	@echo ')))' >> $(TEMPFILE)

$(PKGFILE):
	@echo "(define-package \"ghc\" \"$(VERSION)\"" >> $(PKGFILE)
	@echo "                \"Happy Haskell programming on Emacs\"" >> $(PKGFILE)
	@echo "                '((haskell-mode \"2.8.0\")))"  >> $(PKGFILE)

clean:
	rm -f *.elc $(TEMPFILE) $(PKG)
