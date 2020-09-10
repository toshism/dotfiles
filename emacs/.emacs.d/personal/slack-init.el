;;; slack-init.el --- personal slack config          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; personal slack config

;;; Code:


(use-package helm-slack
  :quelpa (helm-slack :repo "yuya373/helm-slack" :fetcher github)
  :after (slack))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "scoutbee"
   :default t
   :token (auth-source-pick-first-password
         :host "scoutbee.slack.com")
   ;;:subscribed-channels '(test-rename rrrrr)
   :full-and-display-names t)
  (defun org-slack-store-link ()
    (when (derived-mode-p 'slack-mode)
      (let ((slack-buffer (buffer-name)))
	(org-store-link-props
	 :type "slack"
	 :link (format "slack:%s" slack-buffer)
	 :description slack-buffer))))
  (org-link-set-parameters "slack" :follow 'switch-to-buffer :store 'org-slack-store-link))

(provide 'slack-init)

;;; slack-init.el ends here
