;;; projectile-init.el --- tosh's projectile setup       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: projects

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

;; My personal setup for projectile

;;; Code:


(use-package projectile
  :bind
  (("C-c p" . projectile-command-map)
   ("C-x C-f" . projectile-find-file))
  :config
  (setq-default
   projectile-project-search-path '("~/dev/projects/" "~/dev/projects/scoutbee/")
   projectile-completion-system 'helm)
  (projectile-discover-projects-in-search-path))


;; this isn't strictly projectile related, but that's the main place i use it
(use-package ripgrep)

(provide 'projectile-init)

;;; projectile-init.el ends here
