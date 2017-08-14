
(fset 'goto-lib-dep
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 21 19 94 108 105 98 114 97 114 121 13 19 98 117 105 108 100 45 100 101 112 101 110 100 115 58 13 19 44 32 25 13 18 44 13 6 6] 0 "%d")) arg)))


(fset 'copy-dep-name
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 19 44 32 return 67108896 134217830 134217847 24 24 67108896 67108896] 0 "%d")) arg)))

(fset 'replace-with-lib-dep
   [?\C-a ?\M-x ?c ?o ?p ?y ?- ?d ?e ?p ?- ?n ?a ?m ?e ?\C-m ?\C-x ?r ?  ?r ?\M-x ?g ?o ?t ?o ?- ?l ?i ?b ?- ?d ?e ?p ?\C-m ?\C-  ?\C-e ?\M-w ?\C-x ?r ?j ?r ?\C-y ?\C-k ?\C-x ?\C-x ?\C-  ?\C- ])

(fset 'yank-kill-replace
   [?\C-  ?\C-  ?\C-y ?\C-k ?\C-x ?\C-x ?\M-w])

(global-set-key (kbd "C-c C-r") 'replace-with-lib-dep)
(global-set-key (kbd "C-c C-k") 'yank-kill-replace)
