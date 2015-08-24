;;; demo-mode.el --- Set up emacs for projector demos upon opening -*- lexical-binding: t -*-

;; Copyright (C) 2015 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(define-minor-mode demo-mode
  "Sets a number of display variable to make things look good on a projector."
  :lighter nil
  (load-theme 'tangomod t)
  (set-face-attribute 'default nil :height 200))

(provide 'demo-mode)
;;; demo-mode.el ends here
