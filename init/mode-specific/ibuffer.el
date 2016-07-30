(setq-default ibuffer-default-sorting-mode 'major-mode
              ibuffer-eliding-string "…")

(setq ibuffer-formats '((mark modified read-only " "
                              (name 30 30 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))
