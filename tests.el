(require 'ert)
(require 'auto-dictionary)
(eval-when-compile (require 'cl))

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
