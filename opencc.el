;;; opencc.el --- 中文简繁转换 <-> 中文簡繁轉換  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 徐春阳

;; Author: 徐春阳 <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/emacs-opencc
;; Version: 0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: Chinese
;; Created: 公历2017年6月14日，星期三

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; https://github.com/BYVoid/OpenCC

;;; Code:

;;; Options

(defgroup opencc nil
  "中文简繁转换 <-> 中文簡繁轉換."
  :group 'external)

(defcustom opencc-command "opencc"
  "OpenCC 命令行工具."
  :group 'opencc
  :type '(string))

(defcustom opencc-configuration-files '("s2t"
                                        "t2s"
                                        "s2tw"
                                        "tw2s"
                                        "s2hk"
                                        "hk2s"
                                        "s2twp"
                                        "tw2sp")
  "OpenCC 命令行工具的配置文件.

默认值是 OpenCC 预装的配置，说明如下：

| 配置文件 | 说明                                               |                                                                                         |
|----------+----------------------------------------------------+-----------------------------------------------------------------------------------------|
| s2t      | 简体到繁体                                         | Simplified Chinese to Traditional Chinese                                               |
| t2s      | 繁体到简体                                         | Traditional Chinese to Simplified Chinese                                               |
| s2tw     | 简体到台湾正体                                     | Simplified Chinese to Traditional Chinese <Taiwan Standard>                             |
| tw2s     | 台湾正体到简体                                     | Traditional Chinese <Taiwan Standard> to Simplified Chinese                             |
| s2hk     | 简体到香港繁体（香港小学学习字词表标准）           | Simplified Chinese to Traditional Chinese <Hong Kong Standard>                          |
| hk2s     | 香港繁体（香港小学学习字词表标准）到简体           | Traditional Chinese <Hong Kong Standard> to Simplified Chinese                          |
| s2twp    | 简体到繁体（台湾正体标准）并转换为台湾常用词汇     | Simplified Chinese to Traditional Chinese <Taiwan Standard> with Taiwanese idiom        |
| tw2sp    | 繁体（台湾正体标准）到简体并转换为中国大陆常用词汇 | Traditional Chinese <Taiwan Standard> to Simplified Chinese with Mainland Chinese idiom |
"
  :group 'opencc
  :type '(repeat (string :tag "配置文件")))

;;; Internal helpers

(defmacro opencc-aif (test-form then-form &rest else-forms)
  "Anaphoric version of `if'.
Like `if' but set the result of TEST-FORM in a temporary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro opencc-awhen (test &rest body)
  "Anaphoric version of `when'."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))

;;; API

;;;###autoload
(defun opencc-string (config string)
  "按配置文件 CONFIG 转换字符串 STRING.

如果你没有自己的配置文件，请到在 `opencc-configuration-files' 中选择一个."
  (let* ((proc-name (format " *opencc-%s*" config))
         (proc-buffer proc-name)
         (proc (and (get-buffer proc-buffer)
                    (get-buffer-process proc-buffer)))
         result)
    (unless proc
      (setq proc
            (start-process proc-name
                           proc-buffer
                           opencc-command
                           "--config" config))
      (set-process-query-on-exit-flag proc nil)
      ;; XXX How to know the opencc process is ready to go?
      (sleep-for 0.1))
    (with-current-buffer proc-buffer
      (unless (eq (process-status proc) 'run)
        (message "%s" (buffer-string))
        (delete-region (point-min) (point-max))
        (error "Process %s is not running" proc))
      (delete-region (point-min) (point-max))
      (process-send-string proc (concat string "\n"))
      (while (not (and (> (point-max) 1)
                       (eq (char-after (1- (point-max))) ?\n)))
        (accept-process-output proc 0.005))
      (setq result (buffer-substring (point-min) (1- (point-max)))))
    result))

;;;###autoload
(defun opencc-region (config start end)
  "按配置文件 CONFIG 转换 START 和 END 之间的文字.

如果你没有自己的配置文件，请到在 `opencc-configuration-files' 中选择一个."
  (opencc-string config (buffer-substring-no-properties start end)))

;;; User commands

;;;###autoload
(defun opencc-message ()
  "一个交互命令，使用 minibuffer 和 echo area 读取输入和显示结果."
  (interactive)
  (let ((config (completing-read
                 "配置文件: "
                 opencc-configuration-files))
        (string (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (read-string "需转化文字：" nil nil (thing-at-point 'word)))))
    (message "%s" (opencc-string config string))))
(put 'opencc-message 'interactive-only 'opencc-string)

;;;###autoload
(defun opencc-replace-at-point ()
  "一个交互命令，转化并替换光标下的文字."
  (interactive "*")
  (let* ((config (completing-read
                  "配置文件: "
                  opencc-configuration-files))
         start end
         (string (if (use-region-p)
                     (progn (setq start (region-beginning)
                                  end (region-end))
                            (buffer-substring start end))
                   (opencc-awhen (bounds-of-thing-at-point 'word)
                     (setq start (car it)
                           end (cdr it))
                     (buffer-substring start end)))))
    (unless string
      (apply (if (fboundp 'user-error)
                 #'user-error
               #'error)
             '("Nothing at point to replace")))
    (opencc-awhen (opencc-region config start end)
      (delete-region start end)
      (insert it))))
(put 'opencc-replace-at-point 'interactive-only 'opencc-string)

;;;###autoload
(defun opencc-print-buffer (config &optional input-buffer output-buffer)
  "一个交互命令，转化当前 Buffer 中的内容，在 *OpenCC Output* Buffer 中显示结果."
  (interactive
   (let ((config (completing-read
                  "配置文件: "
                  opencc-configuration-files)))
     (list config nil nil)))
  (unless input-buffer
    (setq input-buffer (current-buffer)))
  (unless output-buffer
    (setq output-buffer (get-buffer-create "*OpenCC Output*")))
  (let ((result (with-current-buffer input-buffer
                  (opencc-region config (point-min) (point-max)))))
    (with-current-buffer output-buffer
      (delete-region (point-min) (point-max))
      (insert result)
      (display-buffer (current-buffer)))))

(provide 'opencc)
;;; opencc.el ends here
