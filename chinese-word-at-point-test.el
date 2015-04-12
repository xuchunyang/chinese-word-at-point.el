;; chinese-word-at-point-test.el --- simple-httpd unit tests

;;; Commentary:

;; Run standalone with this,
;; M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'chinese-word-at-point)

(ert-deftest chinese-word-chinese-string-p-test ()
  "Text CJK string predication."
  (should (equal (chinese-word-chinese-string-p "中国人使用中文") t))
  (should (equal (chinese-word-chinese-string-p "中国人使用中文a") nil))
  (should (equal (chinese-word-chinese-string-p "English") nil))
  (should (equal (chinese-word-chinese-string-p "1234") nil)))

(ert-deftest chinese-word-at-point-test ()
  "Text (only) Chinese word at point."
  (with-temp-buffer
    (setq chinese-word-split-command  "echo %s | python -m jieba -q -d ' '")
    (insert "中国人使用中文。")         ; result of segmentation is "中国 人 使用 中文"
    (goto-char (point-min))
    (should (equal (chinese-word-at-point) "中国"))
    (goto-char 3)
    (should (equal (chinese-word-at-point) "人"))
    (goto-char 4)
    (should (equal (chinese-word-at-point) "使用"))
    (goto-char 6)
    (should (equal (chinese-word-at-point) "中文"))
    (goto-char (point-max))
    (should (equal (chinese-word-at-point) nil))))

(ert-deftest chinese-or-other-word-at-point-test ()
  "Text Chinese or other language word at point."
  (with-temp-buffer
    (setq chinese-word-split-command  "echo %s | python -m jieba -q -d ' '")
    (insert "中国人使用中文。")         ; result of segmentation is "中国 人 使用 中文"
    (insert "Hello world.")
    (goto-char (point-min))
    (should (equal (chinese-or-other-word-at-point) "中国"))
    (forward-sentence)
    (should (equal (chinese-or-other-word-at-point) "Hello"))
    (goto-char (point-max))
    (should (equal (chinese-word-at-point) nil))))

;;; chinese-word-at-point-test.el ends here
