(require 'ert)
(require 'auto-dictionary)
(eval-when-compile (require 'cl))

(ert-deftest adict--guess-dictionary-name ()
  (should (equal "deutsch"
                 (adict-guess-dictionary-name '("de" "deutsch" "german")
                                              '("francais" "deutsch" "english"))))
  (should (equal nil
                 (adict-guess-dictionary-name '("de" "deutsch" "german")
                                              '("francais" "english"))))
  (should (equal "english"
                 (flet ((ispell-valid-dictionary-list
                         ()
                         '("francais" "deutsch" "english")))
                   (adict-guess-dictionary-name '("en" "english")))))

  (should (equal nil
                 (flet ((ispell-valid-dictionary-list () '("english" "deutsch")))
                   (adict-guess-dictionary-name '("fr" "francais"))))))

(ert-deftest adict--guess-dictionary-cons ()
  (should (equal '("de" . "deutsch")
                 (flet ((ispell-valid-dictionary-list
                         ()
                         '("francais" "deutsch" "english")))
                   (adict--guess-dictionary-cons '("de" "deutsch")))))

  (should (equal '("de" . "de")
                 (flet ((ispell-valid-dictionary-list () '("fr" "de" "en")))
                   (adict--guess-dictionary-cons '("de" "german")))))

  (should (equal '("de" . nil)
                 (flet ((ispell-valid-dictionary-list () '("fr" "en")))
                   (adict--guess-dictionary-cons '("de" "german"))))))

(ert-deftest adict--evaluate-buffer-find-max-index-should-find-max-index ()
  (should (equal 1 (flet ((adict-evaluate-buffer (idle-only) [20 10 0]))
                     (adict--evaluate-buffer-find-max-index nil))))
  (should (equal 2 (flet ((adict-evaluate-buffer (idle-only) [0 10 20]))
                     (adict--evaluate-buffer-find-max-index nil))))
  (should (equal 1 (flet ((adict-evaluate-buffer (idle-only) [0 20 20 5]))
                     (adict--evaluate-buffer-find-max-index nil)))))

(ert-deftest adict--evaluate-buffer-find-max-should-pass-idle-only-arg ()
  (should
   (let ((idle-only-set nil))
     (flet ((adict-evaluate-buffer (idle-only)
                                   (setq idle-only-set idle-only) [0]))
       (adict--evaluate-buffer-find-max-index t))
     idle-only-set))

  (should
   (not
    (let ((idle-only-set t))
      (flet ((adict-evaluate-buffer (idle-only)
                                    (setq idle-only-set idle-only) [0]))
        (adict--evaluate-buffer-find-max-index nil))
      idle-only-set))))

(ert-deftest adict--evaluate-buffer-find-dictionary-should-find-dictionary-old ()
  (should
   (equal "en_US"
          (let ((adict-dictionary-list '(nil "de_DE" "en_US" "fr")))
            (flet ((adict--evaluate-buffer-find-max-index (idle-only) 2))
              (adict--evaluate-buffer-find-dictionary nil))))))

(ert-deftest adict--evaluate-buffer-find-dictionary-should-find-dictionary ()
  (should
   (equal "en_US"
          (let ((adict-language-list '(nil "de" "en"))
                (adict-dictionary-list '(("de" . "de_DE") ("en" . "en_US"))))
            (flet ((adict--evaluate-buffer-find-max-index (idle-only) 2))
              (adict--evaluate-buffer-find-dictionary nil))))))

(ert-deftest adict--evaluate-buffer-find-lang-should-find-lang ()
  (should
   (equal "en"
          (let ((adict-language-list '(nil "de" "en" "fr")))
            (flet ((adict--evaluate-buffer-find-max-index (idle-only) 2))
              (adict--evaluate-buffer-find-lang nil))))))
