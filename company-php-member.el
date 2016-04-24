(setq company-php-member--prefix-regex
      "\\(?:\\$[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\s*->\s*\\(?:[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\s*\\(?:(.*?)\\)?\s*->\s*\\)*\\([a-zA-Z_\x7f-\xff]?[a-zA-Z0-9_\x7f-\xff]*\\)")

(defun company-php-member--prefix ()
  "Get completion prefix"
  (and
   (eq major-mode 'php-mode)
   (looking-back company-php-member--prefix-regex)
   (match-string 1)))

(defun company-php-member--get-stack ()
  (save-excursion)
  (let ((end (point)) result)
    (re-search-backward company-php-member--prefix-regex)

    (re-search-forward company-php-variable-regex end)
    (push (match-string 0) result)

    (while (re-search-forward "\\([a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*\\)\\(?:\s*(.*?)\\)?\s*->" end 'noerror)
      (push (match-string 1) result))

    (reverse result)))
