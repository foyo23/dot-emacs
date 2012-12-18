;;禁用启动画面
(setq inhibit-startup-message t)
(setq default-tab-width 2)

;;========================================
;; 键绑定
;;========================================

;;WIN+s进入Shell ;; M-x shell
(global-set-key (kbd "s-s") 'shell)
;;(define-key ctl-x-map "\M-s" 'shell)

;;WIN+space 设置标记
(global-set-key (kbd "s-SPC") 'set-mark-command)

;;WIN-f代替M-f, WIN-b代替M-b
;;    (global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)

;;方案一
;;令Backspace能删除前一字符,C-x C-h等于原来的C-h  http://kb.iu.edu/data/abvn.html
;;而help的快捷改为C-x C-h. mark-while-buffer仍为C-x h
(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?\C-h] 'help-command)
;;方案二:xshell终端中设置Backspace的键值为127(Ctrl+?)
(global-set-key [?\C-?] 'delete-backward-char)

;;========================================
;; 缓冲区
;;========================================

;;设定行距
(setq default-line-spacing 4)

;;页宽
(setq default-fill-column 60)

;;缺省模式 text-mode
(setq default-major-mode 'text-mode)

;;设置删除纪录
(setq kill-ring-max 200)

;;以空行结束
(setq require-final-newline t)


;;语法加亮
(global-font-lock-mode t)

;;高亮显示区域选择
(transient-mark-mode t)

;;页面平滑滚动， scroll-margin 3 靠近屏幕边沿3行时开始滚动，可以很好的看到上下文。
(setq scroll-margin 3
      scroll-conservatively 10000)

;;高亮显示成对括号，但不来回弹跳
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;鼠标指针规避光标
(mouse-avoidance-mode 'animate)

;;粘贴于光标处，而不是鼠标指针处
(setq mouse-yank-at-point t)

;;========================================
;; 回显区
;;========================================

;;闪屏报警
(setq visible-bell t)

;;使用 y or n 提问
(fset 'yes-or-no-p 'y-or-n-p)

;;锁定行高
(setq resize-mini-windows nil)

;;递归minibuffer
(setq enable-recursive-minibuffers t)

;;========================================
;; 状态栏
;;========================================

;;显示时间
(display-time)
;;时间格式
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

;;显示列号
(setq column-number-mode t)

;;标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号
(setq frame-title-format "%f")


;;========================================
;; 编辑器设定
;;========================================

;;不生成临时文件
(setq-default make-backup-files nil)

;;使用utf-8编码
(prefer-coding-system 'utf-8-with-signature)

;;只渲染当前屏幕语法高亮，加快显示速度
(setq lazy-lock-defer-on-scrolling t)
                                        ;(setq font-lock-support-mode 'lazy-lock-mode) ;这句会出错！
(setq font-lock-maximum-decoration t)

;;将错误信息显示在回显区
(condition-case err
    (progn
      (require 'xxx) )
  (error
   (message "Can't load xxx-mode %s" (cdr err))))

;;使用X剪贴板
(setq x-select-enable-clipboard t)
;;设定剪贴板内容格式    适应Firefox
(set-clipboard-coding-system 'ctext)

(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(add-to-list 'load-path "~/.emacs.d")

;;支持在目录或文件中查找 http://blog.csdn.net/pankun/archive/2007/04/25/1584873.aspx
;;file-search.el需放在~目录下
;; usage: M-x search-file
;;(setq load-path (cons "~" load-path))
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'file-search)

;auto-complete http://cx4a.org/software/auto-complete/manual.html
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-1.3.1")
;以下这2种配置方法都有问题
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete-1.3.1/dict")
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

;;支持代码自动补全 http://www.laihj.net/2010/01/code-complete-yasnippet/
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;;cedet development environment
(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
;;ecb source code browser
;(require 'ecb)
(require 'ecb-autoloads)

;;--------------------------------------------------------------------------
;; Shift the selected region right if distance is postive, left if negative
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 2))

(defun shift-left ()
  (interactive)
  (shift-region -2))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:
;; 在haml文件的编辑中, shift-right会插入tab,令其不可用。

;; 这里的快捷键绑定不起作用，原因未知
(global-set-key (kbd "<C-tab>") 'shift-right)
(global-set-key (kbd "<C-S-tab>") 'shift-left)
;;----------------------------------------------------------------------

;;
;;(add-to-list 'load-path "~/.emacs.d/textmate.el")
(require 'textmate)
(textmate-mode)

(add-to-list 'load-path "~/.emacs.d/highlight-regexp.el")
(require 'highlight-regexp)

