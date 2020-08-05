;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(message "* --[ Loading Emacs init file ]--")

;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'

;; allow quick include/exclude of setup parts
(defvar section-environment t)
(defvar section-general t)
(defvar section-korean t)
(defvar section-ui t)
(defvar section-hotkey t)
(defvar section-notab t)
(defvar section-automodehook t)
(defvar section-cedet nil)

;; git
(defvar section-gitemacs nil)
(defvar section-magit nil)
(defvar section-gtags nil)

(defvar section-flymake nil)
(defvar section-w3m nil)
(defvar section-anything nil)
(defvar section-ido t)
(defvar section-dsvn t)
(defvar section-recentf t)
(defvar project-webkit nil)

(defvar section-smtp t)
(defvar section-dot t)
(defvar section-php t)

(defvar section-autocomplete nil)
(defvar section-compile t)

;; start package.el with emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
(defun my:ac-c-header-init ()
    (require 'auto-complete-c-headers)
        (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/usr/llvm-gcc-4.2/lib/gcc/i686-apple-darwin11/4.2.1/include")
            )
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

; turn on Semantic
(semantic-mode 1)
(defun my:add-semantic-to-autocomplete() 
    (add-to-list 'ac-sources 'ac-source-semantic)
    )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(global-semantic-idle-scheduler-mode 1)

;; start jedi with emacs
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) 

;; start perl-completion with emacs
(add-hook 'cperl-mode-hook
    (lambda()
    (require 'perl-completion)
    (perl-completion-mode t)))

(add-hook 'cperl-mode-hook
    (lambda ()
    (when (require 'auto-complete nil t)
    (auto-complete-mode t)
    (make-variable-buffer-local 'ac-sources)
    (setq ac-sources '(ac-source-perl-completion)))))

;; start ajc-java-complete with emacs
(add-to-list 'load-path "~/.emacs.d/auto-java-complete")
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)

;; start ecb
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)

(global-set-key (kbd "M-0") 'ecb-goto-window-edit-last)
(global-set-key (kbd "M-1") 'ecb-goto-window-directories)
(global-set-key (kbd "M-2") 'ecb-goto-window-sources)
(global-set-key (kbd "M-3") 'ecb-goto-window-methods)
(global-set-key (kbd "M-4") 'ecb-goto-window-history)

;; start clojure
(setq mac? (eq system-type 'darwin))
(when mac?
    (let ((usr-local "/usr/local/bin"))
    (add-to-list 'exec-path usr-local)
    (setenv "PATH" (concat usr-local path-separator (getenv "PATH")))))

;; ycmd
(require 'ycmd)
(require 'company-ycmd)
(require 'flycheck-ycmd)

(company-ycmd-setup)
(flycheck-ycmd-setup)

(setq company-idle-delay 0.15)

(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(set-variable 'ycmd-server-command `("python3" ,(file-truename "~/.emacs.d/ycmd/ycmd/")))

;;** Environment

(when section-environment (message "Environment...")

      ;; OS type --- are we running Microsoft Windows?
      (defvar running-ms-windows
        (eq system-type 'windows-nt))

      (defvar running-ms-windows
        (string-match "windows" (prin1-to-string system-type)))

      (defvar running-gnu-linux
        (string-match "linux" (prin1-to-string system-type)))

      ;; Emacs type --- are we running XEmacs (or GNU Emacs)?
      (defvar running-xemacs
        (string-match "XEmacs" emacs-version))

      ;; OS type --- are we running GNU Linux?
      (defmacro GNULinux (&rest body)
        (list 'if (string-match "linux" (prin1-to-string system-type))
              (cons 'progn body)))

      (defmacro Windows (&rest body)
        (list 'if (string-match "windows" (prin1-to-string system-type))
              (cons 'progn body)))

      (defmacro XLaunch (&rest body)
        (list 'if (eq window-system 'x)(cons 'progn body)))

      ;; Emacs type --- are we running GNU Emacs?
      (defmacro GNUEmacs (&rest body)
        "Execute any number of forms if running under GNU Emacs."
        (list 'if (string-match "GNU Emacs" (version))
              (cons 'progn body)))

      (defmacro GNUEmacs23 (&rest body)
        (list 'if (string-match "GNU Emacs 23" (version))
              (cons 'progn body)))

      (defmacro GNUEmacs22 (&rest body)
        (list 'if (string-match "GNU Emacs 22" (version))
              (cons 'progn body)))

      (defmacro XEmacs (&rest body)
        "Execute any number of forms if running under XEmacs."
        (list 'if (string-match "XEmacs" (version))
              (cons 'progn body)))

      ;; Emacs version
      (GNUEmacs
       (list emacs-version emacs-major-version emacs-minor-version
             system-type system-name system-configuration
             window-system
             (when (boundp 'aquamacs-version) aquamacs-version)))

      (XEmacs
       ;; don't offer migration of the init file
       (setq load-home-init-file t))

      (when running-gnu-linux
        (modify-all-frames-parameters
         '((height . 32))))

      (message "0 Environment... Done"))


;; **
(when section-general (message "General...")
      ;; mark could be noticable
      (setq-default transient-mark-mode t)

      ;; no backup ( start with ~(tilt) )
      (setq-default make-backup-files nil)

      ;; column limit 80
      (setq fill-column 80)

      ;; text-mode is default
      (setq default-major-mode 'text-mode)
      (add-hook 'text-mode-hook 'turn-on-auto-fill)

      ;; parenthesis matching
      ;; http://www.emacswiki.org/cgi-bin/wiki/parenthesismatching
      (defun goto-match-paren (arg)
        "go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
        (interactive "p")
        (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
              ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
              (t (self-insert-command (or arg 1)))))

      ;; purpose: when you visit a file, point goes to the last place where
      ;; it was when you previously visited the same file.
      ;;
      ;; http://www.emacswiki.org/cgi-bin/wiki/saveplace
      (require 'saveplace)
      (setq-default save-place t)

      ;; ediff
      ;; http://www.emacswiki.org/emacs/EdiffMode
      (setq ediff-split-window-function (lambda (&optional arg)
                                          (if (> (frame-width) 150)
                                              (split-window-horizontally arg)
                                            (split-window-vertically arg))))
      ;; (setq ediff-split-window-function 'split-window-horizontally)

      ;;    If `gdb-many-windows' is non-`nil', then `M-x gdb' displays the
      ;; following frame layout:

      ;;      +--------------------------------+--------------------------------+
      ;;      |   GUD buffer (I/O of GDB)      |   Locals/Registers buffer      |
      ;;      |--------------------------------+--------------------------------+
      ;;      |   Primary Source buffer        |   I/O buffer for debugged pgm  |
      ;;      |--------------------------------+--------------------------------+
      ;;      |   Stack buffer                 |   Breakpoints/Threads buffer   |
      ;;      +--------------------------------+--------------------------------+
      (setq gdb-many-windows t)

      ;; dircmp-mode
      (load "~/.emacs.d/dircmp.el")

      ;; ediff marked file
      (defun dired-ediff-marked-files ()
        "Run ediff on marked ediff files."
        (interactive)
        (set 'marked-files (dired-get-marked-files))
        (when (= (safe-length marked-files) 2)
          (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
        
        (when (= (safe-length marked-files) 3)
          (ediff3 (buffer-file-name (nth 0 marked-files))
                  (buffer-file-name (nth 1 marked-files)) 
                  (buffer-file-name (nth 2 marked-files)))))

      ;; gdict
      (add-to-list 'load-path "~/.emacs.d")
      (require 'gdict)
      (require 'json)
      (global-set-key (kbd "C-c g d") 'gdict)

      ;; getting git root dir
      ;; http://blog.uberweiss.net/2009/11/scoping-emacs-to-a-git-root-directory.html
      (defun my-git-root ()
        (if buffer-file-name
            (let* ((current-directory (file-name-directory buffer-file-name))
                   (git-directory (concat current-directory ".git")))
              (while (and
                      current-directory
                      (not (file-exists-p git-directory)))
                (setq current-directory (file-name-directory (substring current-directory 0 -1)))
                (setq git-directory (concat current-directory ".git")))
              current-directory)))

      ;; blank-mode
      (require 'blank-mode)
      (global-set-key (kbd "C-c b") 'blank-mode)

      ;; Go to the line of the file easily especially in gdb call stack.
      ;; /etc/passwd:10
      (defun find-file-at-point-with-line()
        "if file has an attached line num goto that line, ie boom.rb:12"
        (interactive)
        (setq line-num 0)
        (save-excursion
          (search-forward-regexp "[^ ]:" (point-max) t)
          (if (looking-at "[0-9]+")
              (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
        (find-file-at-point)
        (if (not (equal line-num 0))
            (goto-line line-num)))

      (global-set-key (kbd "C-<return>") 'find-file-at-point-with-line)

      (message "General... done"))

;; **
(when section-korean (message "Korean...")
      ;; hangul configuration
      ;; (set-language-environment "korean")
      (set-language-environment "UTF-8")
      (setq default-input-method "korean-hangul")
      (global-set-key (kbd "S-SPC") 'toggle-input-method)
      (message "Korean... done"))

;; **
(when section-notab (message "no tab...")
      (defun notab ()
        "use 2 spaces instead of tab and also use spaces for indentation"
        (setq default-tab-width 2)
        (setq c-basic-offset 2)               ;; indent use only 2 blanks
        (setq indent-tabs-mode nil)           ;; no tab
        )  
      
      (add-hook 'c-mode-hook 'notab)
      (add-hook 'c-mode-hook '
                (lambda () (c-set-style "bsd")))
      (add-hook 'c++-mode-hook 'notab)
      (add-hook 'c++-mode-hook '
                (lambda () (c-set-style "bsd")))

      (add-hook 'jave-mode-hook 'notab)
      (add-hook 'css-mode-hook 'notab)
      (add-hook 'python-mode-hook 'notab)
      (add-hook 'perl-mode-hook 'notab)
      (add-hook 'cperl-mode-hook 'notab)
      (add-hook 'emacs-lisp-mode-hook 'notab)

      ;; tab width
      (setq default-tab-width 2)
      (setq c-basic-offset 2)                 ;; indent use only 4 spaces
      (setq-default indent-tabs-mode nil)     ;; no tab

      (message "no tab... done"))

;; **
(when section-ui (message "UI customize...")
      ;; no splash
      (setq inhibit-startup-message t)

      ;; hide toolbar & menubar
      (tool-bar-mode -1)
      ;; (menu-bar-mode -1)
      (set-scroll-bar-mode 'right)

      ;; color theme
      (setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
      (require 'color-theme)
      (color-theme-initialize)
      (color-theme-gray30)

      ;; tabbar
      (load-file "~/.emacs.d/tabbar-tweak.el")
      (global-set-key (kbd "M->") 'tabbar-forward-tab)
      (global-set-key (kbd "M-<") 'tabbar-backward-tab)

      ;; line number
      (if (display-graphic-p)
      (progn
      (global-linum-mode t))
      (require 'line-num)
      (global-linum-mode 1)
      (setq linum-format "%4d \u2502"))

      ;; copy & paste
      (setq x-select-enable-clipboard t)

      ;; auto revert
      (global-auto-revert-mode 1)

      ;; set command key for meta key
      (setq mac-command-modifier 'meta)

      ;; font
      (set-face-attribute 'default nil :family "Courier New" :height 100 :weight 'normal)

      ;; main line
      (require 'main-line)
      (setq main-line-separator-style 'zigzag)
      (set-face-attribute 'mode-line nil :box nil)

      (message "UI customize... done"))

;; **
(when section-hotkey (message "hotkey...")
      ;;(global-set-key (kbd "C-c y") 'clipboard-yank)
      (global-set-key (kbd "C-c c") 'compile)
      (global-set-key (kbd "C-c r y") 'comment-region)
      (global-set-key (kbd "C-c r u") 'uncomment-region)
      (global-set-key (kbd "M-g") 'goto-line)

      ;; fast move next, previous buffer
      (global-set-key (kbd "C-c n") 'next-buffer)
      (global-set-key (kbd "C-c p") 'previous-buffer)

      (global-set-key "\C-xj" 'goto-line)        ;; goto-line
      (global-set-key (kbd "C-c m") 'manual-entry)    ;; manpage
      ;; (global-set-key "\c-cs" 'shell-command)    ;; shell-cmd

      (global-set-key (kbd "M-]") 'goto-match-paren)  ;; goto matching parenthesis

      ;; find from current dir
      (global-set-key (kbd "C-c C-g") 'find-name-dired)
      ;; ask dir to find before
      (global-set-key (kbd "C-c C-h") 'find-grep-dired)
      (global-set-key (kbd "C-c g g") 'grep-find)

      ;; execute the shell buffer in utf-8 encoding.
      ;; (defun unicode-shell ()
      ;;   "execute the shell buffer in utf-8 encoding.
      ;; note that you'll need to set the environment variable lang and others
      ;; appropriately."
      ;;   (interactive)
      ;;   (let ((coding-system-for-read 'utf-8)
      ;;         (coding-system-for-write 'utf-8)
      ;;         (coding-system-require-warning t))
      ;;     (call-interactively 'shell)))

      ;; switch h <-> cpp
      ;; (global-set-key (kbd "M-p") 'eassist-switch-h-cpp)
      (global-set-key (kbd "M-p") 'ff-find-other-file)

      ;; http://www.emacswiki.org/emacs/SearchAtPoint
      ;; http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
      ;; I-search with initial contents
      (defvar isearch-initial-string nil)
      (defun isearch-set-initial-string ()
        (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (setq isearch-string isearch-initial-string)
        (isearch-search-and-update))
      (defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
        "Interactive search forward for the symbol at point."
        (interactive "P\np")
        (if regexp-p (isearch-forward regexp-p no-recursive-edit)
          (let* ((end (progn (skip-syntax-forward "w_") (point)))
                 (begin (progn (skip-syntax-backward "w_") (point))))
            (if (eq begin end)
                (isearch-forward regexp-p no-recursive-edit)
              (setq isearch-initial-string (buffer-substring begin end))
              (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
              (isearch-forward regexp-p no-recursive-edit)))))

      (global-set-key (kbd "C-c C-k") 'isearch-forward-at-point)
      (global-set-key (kbd "C-M-o") 'other-window)

      (defun other-window-prev (&optional step)
        "other-window to opposite direction"
        (interactive "P")
        (setq step -1)
        (other-window step))

      (global-set-key (kbd "C-M-o") 'other-window)
      (global-set-key (kbd "C-M-m") 'other-window-prev)
      (global-set-key (kbd "C-x j") 'goto-line)
      (message "hotkey... done"))

(when section-automodehook (message "automodehook...")
      ;; css-mode
      (autoload 'css-mode "css-mode-simple")
      (setq auto-mode-alist
            (cons '("\\.css\\'" . css-mode) auto-mode-alist))

      ; javascirpt-mode
      (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
      (autoload 'javascript-mode "javascript" nil t)
      (autoload 'javascript-mode "javascript-mode")
      (setq auto-mode-alist
           (cons '("\\.js\\'" . javascript-mode) auto-mode-alist))

      ;; js2 mode
      (add-to-list 'load-path "~/.emacs.d/js2-mode")
      (autoload 'js2-mode "js2-mode" nil t)
      (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

      ;; makefile
      (setq auto-mode-alist
            (append
             '(("makefile\\." . makefile-mode)
               ("Makefile\\.*" . makefile-mode)
               ("\\.mak"      . makefile-mode)
               ("\\.pri" . makefile-mode)
               ("\\.pro" . makefile-mode)
               ("\\.prf" . makefile-mode)
               ("\\.min" . makefile-mode)
               ("Android.mk" . makefile-mode))
             auto-mode-alist))

      ;; (setq auto-mode-alist
      ;;    (cons '("\\.min\\'" . makefile-mode) auto-mode-alist)
      ;;    (cons '("\\.mak\\'" . makefile-mode) auto-mode-alist)
      ;;    (cons '("\\.make\\'" . makefile-mode) auto-mode-alist))

      ;; perl mode
      (add-to-list 'auto-mode-alist '("\\.\\([pp][llm]\\|al\\)\\'" . cperl-mode))
      (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
      (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
      (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

      ;; c++-mode
      (setq auto-mode-alist
            (append
             '(("\\.h$"      . c++-mode))
             auto-mode-alist))

      (message "automodehook..."))

(when section-cedet (message "cedet...")
      ;; cedet

      ;; http://www.emacswiki.org/emacs/collectionofemacsdevelopmentenvironmenttools
      (setq byte-compile-warnings nil)

      ;; load cedet.
      ;; see cedet/common/cedet.info for configuration details.
      (load-file "~/.emacs.d/cedet-1.0.1/common/cedet.el")

      ;; enable ede (project management) features
      (global-ede-mode t)

      (semantic-load-enable-excessive-code-helpers)
      (require 'semantic-ia)

      ;; enable ede for a pre-existing c++ project
      ;; (ede-cpp-root-project "name" :file "~/myproject/makefile")


      ;; enabling semantic (code-parsing, smart completion) features
      ;; select one of the following:

      ;; * this enables the database and idle reparse engines
      (semantic-load-enable-minimum-features)

      ;; * this enables some tools useful for coding, such as summary mode
      ;;   imenu support, and the semantic navigator
      (semantic-load-enable-code-helpers)

      ;; * this enables even more coding tools such as intellisense mode
      ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
      (semantic-load-enable-gaudy-code-helpers)

      ;; * this enables the use of exuberent ctags if you have it installed.
      ;;   if you use c++ templates or boost, you should not enable it.
      ;; (semantic-load-enable-all-exuberent-ctags-support)
      ;;   or, use one of these two types of support.
      ;;   add support for new languges only via ctags.
      ;; (semantic-load-enable-primary-exuberent-ctags-support)
      ;;   add support for using ctags as a backup parser.
      ;; (semantic-load-enable-secondary-exuberent-ctags-support)

      (require 'semanticdb)
      (global-semanticdb-minor-mode 1)

      (require 'semanticdb-global)
      (semanticdb-enable-gnu-global-databases 'c-mode)
      (semanticdb-enable-gnu-global-databases 'c++-mode)
      (semanticdb-enable-gnu-global-databases 'java-mode)

      ;; enable srecode (template management) minor-mode.
      ;; (global-srecode-minor-mode 1)

      ;; ecb
      (add-to-list 'load-path "~/.emacs.d/ecb-snap")
      (require 'ecb)
      (require 'ecb-autoloads)
      (setq ecb-tip-of-the-day nil)
      (setq ecb-source-path (quote("/home/hyungwooklee")))

      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(ecb-options-version "2.40"))
      
      ;; ecb window hotkey
      (global-set-key (kbd "M-0") 'ecb-goto-window-edit-last)
      (global-set-key (kbd "M-1") 'ecb-goto-window-directories)
      (global-set-key (kbd "M-2") 'ecb-goto-window-sources)
      (global-set-key (kbd "M-3") 'ecb-goto-window-methods)
      (global-set-key (kbd "M-4") 'ecb-goto-window-history)

      (global-set-key (kbd "C-c C-e") 'ecb-activate)
      (global-set-key (kbd "C-c C-d") 'ecb-deactivate)

      (message "cedet..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-gtags (message "gtags...")
      (require 'gtags)
      (autoload 'gtags-mode "gtags" "" t)
      (global-set-key (kbd "C-o") 'gtags-find-file)
      (global-set-key (kbd "C-]") 'gtags-find-tag-from-here)
      (global-set-key (kbd "C-/") 'gtags-find-rtag)
      (global-set-key (kbd "C-t") 'gtags-pop-stack)

      (message "gtags... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** git-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-gitemacs (message "gitemacs...")
      (add-to-list 'load-path "~/.emacs.d/git-emacs")
      (require 'git-emacs)
      (message "gitemacs... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-magit (message "magit...")

      ;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
      ;; This should disable the backend:
      ;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
      ;; you might need a (require 'vc) before the above line to get the timing right. Or perhaps wrap it like so:
      ;; (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
      ;; to get the timing right.
      (require 'vc)
      (remove-hook 'find-file-hooks 'vc-find-file-hook)

      (add-to-list 'load-path "~/.emacs.d/magit/")
      (require 'magit)
      (global-set-key (kbd "C-c s") 'magit-status)
      (message "magit... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-w3m (message "w3m...")
      (setq load-path (cons (expand-file-name "~/.emacs.d/emacs-w3m") load-path))
      (require 'w3m-load)
      (message "w3m... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-anything (message "anything...")
      (add-to-list 'load-path "~/.emacs.d/anything-config")
      (add-to-list 'load-path "~/.emacs.d/anything-config/extensions")
      ;; (require 'anything-gtags)
      (global-set-key (kbd "C-x a") 'anything)
      (require 'anything-config)
      (message "anything... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-ido (message "ido...")
      (setq ido-enable-flex-matching t)
      (setq ido-everywhere t)
      (ido-mode 1)

      ;; this setting will force Ido to always create a new buffer (in C-x b) if the name does not exist
      (setq ido-create-new-buffer 'always)

      (setq ido-file-extensions-order '(".cpp" ".c" ".h" ".txt"))

      (message "ido... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** svn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-dsvn (message "dsvn...")
      (autoload 'svn-status "dsvn" "Run `svn status'." t)
      (autoload 'svn-update "dsvn" "Run `svn update'." t)

      (require 'vc-svn)
      (message "dsvn... done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-recentf (message "recentf...")
      (require 'recentf)
      ;; get rid of `find-file-read-only' and replace it with something
      ;; more useful.
      (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

      ;; enable recent files mode.
      (recentf-mode t)

      ;;; 50 files ought to be enough.
      (setq recentf-max-saved-items 50)

      (defun ido-recentf-open ()
        "Use `ido-completing-read' to \\[find-file] a recent file"
        (interactive)
        (if (find-file (ido-completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))

      (message "recentf... done"))

;; ** smtp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when section-smtp (message "smtp...")
      (setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
      (setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
      (setq smtpmail-default-smtp-server "lgekrhqmh01.lge.com") ; set before loading library
      (setq smtpmail-local-domain "lge.com")
      (setq smtpmail-sendto-domain "lge.com")
      (setq smtpmail-debug-info t) ; only to debug problems
      (setq smtpmail-auth-credentials  ; or use ~/.authinfo
            '(("lgekrhqmh01.lge.com" 25 "hyungchan2.kim" "xxxxxxx")))
      (setq smtpmail-starttls-credentials
            '(("lgekrhqmh01.lge.com" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))
      (message "smtp... done"))

(when section-dot (message "dot...")
      (load-file "~/.emacs.d/graphviz-dot-mode.el")
      (message "dot... done"))

(when section-php (message "dot...")
      (require 'php-mode)
      (message "php... done"))

(when section-autocomplete (message "autocomplete...")
      (add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
      (setq ac-dictionary-directories "~/.emacs.d/dict")
      (require 'auto-complete-config)
      (ac-config-default)
      (setq ac-auto-start t)

      ;; auto-complete-clang
      (add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
      (require 'auto-complete-clang)
      ;; (global-set-key (kbd "C-\\") 'ac-complete-clang)

      ;; c++
      (add-hook 'c++-mode-hook (lambda ()
                            (add-to-list 'ac-sources 'ac-source-clang)))

      ;; Complete member name by C-c . for C++ mode.
      (add-hook 'c++-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c .") 'ac-complete-clang)))

      (setq ac-clang-flags
            (mapcar (lambda (item)(concat "-I" item))
                    (split-string
                     "
 /usr/include/c++/4.6
 /usr/include/c++/4.6/x86_64-linux-gnu
 /usr/include/c++/4.6/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
"
                     )))

      (defun my-ac-cc-mode-setup ()
        (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
      (add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
      (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

      ;; javascript
      (add-to-list 'load-path "~/.emacs.d/jquery-doc")
      (require 'jquery-doc)
      ;; adds ac-source-jquery to the ac-sources list
      (add-hook 'js2-mode-hook 'jquery-doc-setup)

      (defun my-js2-mode-hook ()
        (jquery-doc-setup)
        (local-set-key (kbd "C-c .") 'ac-complete-jquery))

      (add-hook 'js2-mode-hook 'my-js2-mode-hook)

      (message "autocomplete... done"))

(when section-compile (message "compile...")
      ;; FIXME: requires cedet(ede)
      (global-ede-mode t)

      (defun alexott/gen-cmake-debug-compile-string ()
        "Generates compile string for compiling CMake project in debug mode"
        (let* ((current-dir (file-name-directory
                             (or (buffer-file-name (current-buffer)) default-directory)))
               (prj (ede-current-project current-dir))
               (root-dir (ede-project-root-directory prj))
               (subdir "")
               )
          (when (string-match root-dir current-dir)
            (setf subdir (substring current-dir (match-end 0))))
          (concat "cd " root-dir "Debug/" "; make -j3")
          ))

      (defun compile-string-make ()
        "Generates compile string for compiling CMake project in debug mode"
        (let* ((current-dir (file-name-directory
                             (or (buffer-file-name (current-buffer)) default-directory)))
               (prj (ede-current-project current-dir))
               (root-dir (ede-project-root-directory prj))
               (subdir "")
               )
          (when (string-match root-dir current-dir)
            (setf subdir (substring current-dir (match-end 0))))
          (concat "cd " root-dir "; make -j")
          ))

      (defun alexott/ede-get-local-var (fname var)
        "fetch given variable var from :local-variables of project of file fname"
        (let* ((current-dir (file-name-directory fname))
               (prj (ede-current-project current-dir)))
          (when prj
            (let* ((ov (oref prj local-variables))
                   (lst (assoc var ov)))
              (when lst
                (cdr lst))))))

      ;; setup compile package
      (require 'compile)
      (setq compilation-disable-input nil)
      (setq compilation-scroll-output t)
      (setq mode-compile-always-save-buffer-p t)

      (defun alexott/compile ()
        "Saves all unsaved buffers, and runs 'compile'."
        (interactive)
        (save-some-buffers t)
        (let* ((r (alexott/ede-get-local-var
                   (or (buffer-file-name (current-buffer)) default-directory)
                   'compile-command))
               (cmd (if (functionp r) (funcall r) r)))
          (set (make-local-variable 'compile-command) (or cmd compile-command))
          (compile compile-command)))

      (global-set-key [f9] 'alexott/compile)

      ;; Sample project
      ;; While editing any files under ~/tmp/ede_test/ such as hello.c
      ;; By pressing F9, it will automatically compile using its Makefile
      (when (file-exists-p "~/tmp/ede_test/Makefile")
        (setq cpp-tests-project
              (ede-cpp-root-project "ede_test"
                                    :file "~/tmp/ede_test/Makefile"
                                    :system-include-path '("/usr/include")
                                    :local-variables (list
                                                      (cons 'compile-command 'compile-string-make)
                                                      )
                                    ;; :local-variables (list (cons 'compile-command "make"))
                                    )))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; QtWebKit
      (when (file-exists-p "~/QtWebKit/build.sh")
        (setq cpp-tests-project
              (ede-cpp-root-project "QtWebKit"
                                    :file "~/QtWebKit/build.sh"
                                    :system-include-path '("/usr/include")
                                    :local-variables (list
                                                      (cons 'compile-command 'compile-string-qtwebkit)
                                                      )
                                    )))

      (defun compile-string-qtwebkit ()
        "Generates compile string for compiling CMake project in debug mode"
        (let* ((current-dir (file-name-directory
                             (or (buffer-file-name (current-buffer)) default-directory)))
               (prj (ede-current-project current-dir))
               (root-dir (ede-project-root-directory prj))
               (subdir "")
               )
          (when (string-match root-dir current-dir)
            (setf subdir (substring current-dir (match-end 0))))
          (concat "cd " root-dir "; ./build.sh")
          ))

      (message "compile... done"))

(when section-flymake (message "flymake...")
      (require 'flymake)
      (require 'flymake-cursor)

      (defun flymake-clang-c++-init ()
        ;; ediff control(at the bottom while emacs running) buffer-file-name is nil
        (if (not buffer-file-name)
            (flymake-mode-off)
          (let* ((temp-file (flymake-init-create-temp-buffer-copy
                             'flymake-create-temp-inplace))
                 (local-file (file-relative-name
                              temp-file
                              (file-name-directory buffer-file-name)))
                 (git-root-dir (my-git-root)))

            (if (or (string-match "\.index$" local-file)
                    ;; (not (string-match ".cpp$" local-file))
                    (not git-root-dir))
                (flymake-mode-off)
              (if (file-exists-p (concat
                                  git-root-dir "Tools/Scripts/check-webkit-style"))
                  (list "bash" (list "flymake-check-webkit-style.sh" git-root-dir local-file)))))))

      (defun flymake-clang-c++-load ()
        (interactive)
        ;; (message "#### %s " buffer-file-name)
        (unless (eq buffer-file-name nil)
        ;; (unless (or (eq buffer-file-name nil)
        ;;             (not (string-match "\.cpp$" buffer-file-name)))
          (add-to-list 'flymake-allowed-file-name-masks
                       '("\\.cpp" flymake-clang-c++-init))
          (add-to-list 'flymake-allowed-file-name-masks
                       '("\\.cc" flymake-clang-c++-init))
          (add-to-list 'flymake-allowed-file-name-masks
                       '("\\.h" flymake-clang-c++-init))
          (flymake-mode t)
          (global-set-key (kbd "C-c f n") 'flymake-goto-next-error)
          (global-set-key (kbd "C-c f p") 'flymake-goto-prev-error)
          (global-set-key (kbd "C-c f d") 'flymake-display-err-menu-for-current-line)))

      (add-hook 'c++-mode-hook 'flymake-clang-c++-load)

      (message "flymake... done"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes
   (quote
    (("left8"
      (ecb-directories-buffer-name 0.24025974025974026 . 0.2826086956521739)
      (ecb-sources-buffer-name 0.24025974025974026 . 0.2608695652173913)
      (ecb-methods-buffer-name 0.24025974025974026 . 0.2608695652173913)
      (ecb-history-buffer-name 0.24025974025974026 . 0.1956521739130435)))))
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(ede-project-directories (quote ("/Users/HyungwookLee")))
 '(package-selected-packages
   (quote
    (ggtags scala-mode ac-haskell-process go-autocomplete go-mode magit flycheck-ycmd company-ycmd yasnippet tabbar skewer-mode rust-mode perl-completion org-ac org main-line jedi haskell-mode google-c-style ghc emacs-eclim ecb auto-complete-c-headers auctex anything adoc-mode ac-cider)))
 '(tabbar-separator (quote (0.5)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier 10 Pitch" :foundry "bitstream" :slant normal :weight normal :height 98 :width normal)))))

;; disable warning
(setq warning-minimum-level :error)

;; fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ggtags
(require 'ggtags)
(ggtags-mode 1)
(global-set-key (kbd "C-o") 'ggtags-find-file)
(global-set-key (kbd "C-]") 'ggtags-find-tag-dwim)
(global-set-key (kbd "C-/") 'ggtags-find-reference)
(global-set-key (kbd "C-t") 'ggtags-prev-mark)
