
;;
;;  aaa
;;
(defun aaa ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (x (current-time))
        (dt)
        (da)
        )
    (princ x gb) (princ "\n" gb)
    (princ
     (format-time-string "%Y-%m-%d %H:%M:%S.%6N" x) gb)
    (princ "\n" gb)
    (princ
     (format-time-string "%FT%T%z"  x) gb)
    (princ "\n\n" gb)
    (setq x (calendar-nth-named-day 1 0 4 1964))
    (princ x gb)
    (princ "\n\n" gb)
    (put 'dt 'shu-date 'datetime)
    (setq dt 1)
    (setq da dt)
    (princ "dt: " gb) (princ (symbol-plist 'dt) gb) (princ "\n" gb)
    (princ "da: " gb) (princ (symbol-plist 'da) gb) (princ "\n" gb)


    ))



;;
;;  bbb
;;
(defun bbb ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (ss (concat "\\`\\(?:" shu-all-whitespace-regexp "+\\)"))
        (zs (concat "\\`" "shu-"))
        (qq)

        )
    (setq qq (apropos-internal zs))
    (princ (format "\n\n\nCount with no restrictions: %d\n" (length qq)) gb)
    (show-names qq gb)

    (setq qq (apropos-internal zs 'symbolp))
    (princ (format "\n\n\nCount with symbolp: %d\n" (length qq)) gb)
    (show-names qq gb)

    (setq qq (apropos-internal zs 'fboundp))
    (princ (format "\n\n\nCount with fboundp: %d\n" (length qq)) gb)
    (show-names qq gb)

    (setq qq (apropos-internal zs 'commandp))
    (princ (format "\n\n\nCount with commandp: %d\n" (length qq)) gb)
    (show-names qq gb)


    ))



;;
;;  ccc
;;
(defun ccc ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (zs (concat "\\`" "shu-"))
        (sl)
        )
    (mapatoms (lambda (x)
                (when (string-match zs (symbol-name x))
                  (push x sl)
                  )
                )
              )
    (princ (format "\n\n\nCount from mapatoms: %d\n" (length sl)) gb)
    (show-names sl gb)

    ))



;;
;;  ddd
;;
(defun ddd ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        )
    (message "%s" (current-time-string))
    ))



;;
;;  show-names
;;
(defun show-names (name-list gb)
  "Doc string."
  (interactive)
  (let (
;;        (nl (sort name-list 'string<))
        (nl name-list)
        (name)
        (pl)
        )
    (while nl
      (setq name (car nl))
      (princ name  gb)
      (princ "\n" gb)
      (setq pl (symbol-plist name))
      (princ "      " gb)
      (princ pl gb)
      (princ "\n" gb)
      (princ "          file: " gb) (princ (symbol-file name) gb) (princ "\n" gb)
      (princ "          function: " gb) (princ (symbol-function name) gb) (princ "\n" gb)
      (princ "          name: " gb) (princ (symbol-name name) gb) (princ "\n" gb)
;;;      (princ "          value: " gb) (princ (symbol-value name) gb) (princ "\n" gb)
      (when (functionp name)
        (princ "          doc: " gb) (princ (documentation name) gb) (princ "\n" gb)
        )



      (setq nl (cdr nl))
      )


    ))
