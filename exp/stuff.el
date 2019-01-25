
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
