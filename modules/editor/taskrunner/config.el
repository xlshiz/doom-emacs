;;; editor/taskrunner/config.el -*- lexical-binding: t; -*-

(after! taskrunner
  (set-popup-rule! taskrunner--buffer-name-regexp :quit t))
