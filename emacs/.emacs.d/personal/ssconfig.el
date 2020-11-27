;;; ssconfig.el --- SSConfig - Super Simple Config   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: convenience

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

;; A Super Simple Configuration framework helper manager thing.

;;; Code:


(defvar ssconfig-config-dir "~/.emacs.d/ssconfig/")

(defun ssconfig-org-file-path (name &optional conf-dir)
  (let ((config-dir (if conf-dir
			conf-dir
		      ssconfig-config-dir)))
    (expand-file-name (concat name ".org") config-dir)))

(defun ssconfig-el-file-path (name &optional conf-dir)
  (let ((config-dir (if conf-dir
			conf-dir
		      ssconfig-config-dir)))
    (expand-file-name (concat name ".el") config-dir)))

(defun ssconfig-dir-conf-path (name)
  (expand-file-name name ssconfig-config-dir))

(defun ssconfig-config-file-path (name)
  (let* ((org-file (ssconfig-org-file-path name))
	 (el-file (ssconfig-el-file-path name))
	 (dir-conf-path (ssconfig-dir-conf-path name))
	 (dir-org-file-init (ssconfig-org-file-path "init" dir-conf-path))
	 (dir-org-file-name (ssconfig-org-file-path name dir-conf-path))
	 (dir-el-file-init (ssconfig-el-file-path "init" dir-conf-path))
	 (dir-el-file-name (ssconfig-el-file-path name dir-conf-path)))
    (cond ((file-exists-p org-file) org-file)
	  ((file-exists-p el-file) el-file)
	  ((file-exists-p dir-org-file-init) dir-org-file-init)
	  ((file-exists-p dir-org-file-name) dir-org-file-name)
	  ((file-exists-p dir-el-file-init) (progn
					      (add-to-list 'load-path (file-name-directory dir-el-file-init))
					      dir-el-file-init))
	  ((file-exists-p dir-el-file-name) (progn
					      (add-to-list 'load-path (file-name-directory dir-el-file-name))
					      dir-el-file-name)))))

(defun ssconfig-loader (conf-name)
  (let* ((conf-file (ssconfig-config-file-path conf-name))
	 (org-file (string= (file-name-extension conf-file) "org")))
    (set (intern (concat "ssconfig-" conf-name "-dir")) (file-name-directory conf-file))
    (if org-file
	(org-babel-load-file conf-file)
      (require (intern (file-name-base conf-name))))))

(defun ssconfig-init ()
  (mapc 'ssconfig-loader ssconfig-files))

(provide 'ssconfig)
;;; ssconfig.el ends here
