;;; opencc.el --- 中文简繁转换  -*- lexical-binding: t; -*-

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

(defgroup opencc nil
  "中文简繁转换."
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

;;;###autoload
(defun opencc-string (config string)
  "按配置文件 CONFIG 转换字符串 STRING."
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
      ;; XXX How to know the opencc process is ready to go?
      (sleep-for 0.1))
    (with-current-buffer proc-buffer
      (delete-region (point-min) (point-max))
      (process-send-string proc (concat string "\n"))
      (while (not (and (> (point-max) 1)
                       (eq (char-after (1- (point-max))) ?\n)))
        (accept-process-output proc 0 5)
        (setq result (buffer-substring (point-min) (1- (point-max))))))
    result))

;;;###autoload
(defun opencc-region (config start end)
  "按配置文件 CONFIG 转换 START 和 END 之间的文字.
返回值是一个字符串."
  (opencc-string config (buffer-substring-no-properties start end)))

(provide 'opencc)
;;; opencc.el ends here
