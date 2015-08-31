(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "polyglossia"
    "xcolor"
    "fontspec")
   (TeX-add-symbols
    "gm"
    "gms")))

