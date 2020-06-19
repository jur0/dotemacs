;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/guip
  (display-graphic-p)
  "Are we using GUI?")

(defconst sys/mac-x-p
  (and sys/guip sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/linux-x-p
  (and sys/guip sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst exec/rg
  (executable-find "rg")
  "Is ripgrep present?")

(defconst exec/aspell
  (executable-find "aspell")
  "Is aspell present?")

(defconst exec/pass
  (executable-find "pass")
  "Is pass present?")

(defconst exec/sqlite3
  (executable-find "sqlite3")
  "Is sqlite3 present?")

(provide 'init-const)
