# -*- mode:shell-script -*-
# Contributor: Rodney Price <rod@thirdoption.info>

pkgname="emacs-ghc-mod"
pkgver="0.5.4"
pkgrel=2
pkgdesc="An Emacs interface to the GHC API for symbol completion and syntax checking."
arch=('x86_64')
makedepends=('ghc')
url="http://www.mew.org/~kazu/proj/ghc-mod/"
license=('custom:BSD3')
depends=('gmp' 'emacs' 'emacs-haskell-mode')
provides=('emacs-ghc-mod')
install=
options=('strip')
groups=("emacs-haskell")
source=("http://hackage.haskell.org/packages/archive/ghc-mod/$pkgver/ghc-mod-$pkgver.tar.gz")
md5sums=('6c7c047d803a98cd5dd166a0aa3c6ced')

_emacs_root="${HOME}/.emacs.d"
# User's configuration file directory
_emacs_conf="${_emacs_root}/my"
# User's emacs lisp directory
_emacs_lisp="${_emacs_root}/elisp"
# User's snippets directory
_emacs_snippets="${_emacs_root}/snippets/text-mode"
# User's scripts for use at package install time or runtime
_emacs_scripts="${_emacs_root}/scripts"

# Site-wide emacs lisp root
_emacs_site_lisp="/usr/share/emacs/site-lisp"
# Subdirectory for this package's files
_emacs_site_sub="ghc-mod"
# Absolute path to package subdirectory
_emacs_site="${_emacs_site_lisp}/${_emacs_site_sub}"

build() {
    cd "${srcdir}"
    runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} || return 1
    runhaskell Setup build || return 1
    runhaskell Setup copy --destdir=${pkgdir} || return 1
    install -D -m644 LICENSE ${pkgdir}/usr/share/licenses/$pkgname/LICENSE || return 1
    rm -f ${pkgdir}/usr/share/doc/${pkgname}/LICENSE

    cd "$srcdir/elisp"
    # Compile and install the emacs lisp files
    make || return 1
    install -d -m755  "$pkgdir${_emacs_site}"
    install -Dm644 -t "$pkgdir${_emacs_site}" *.el
    install -Dm644 -t "$pkgdir${_emacs_site}" *.elc
    # Remove unnecessary files
    rm -f "$pkgdir${_emacs_site}/temp.el"
    rm -rf "$pkgdir/usr/share/ghc-mod-$pkgver/"
    rm -rf "$pkgdir/usr/share/doc/"
}
