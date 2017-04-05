;; The 'nil' configuration applies to all modes.
((nil . ((indent-tabs-mode . nil)
         (tab-width . 2)
         (eval . (progn
                   (put 'test-check 'scheme-indent-function 1))))))
