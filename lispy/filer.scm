; vim:set filetype=scheme foldmethod=marker:
(provide "filer")

(require-python "builtins")
(require-python "platform")
(require-python "command")
(require-python "logging")
(require-python "lispy")
(require-python "os.path")

(in-package "filer")

(define (tabcurrent)
  ($mcall currentTab model))
(define (pain-current)
  (ref (tabcurrent) 'current))
(define (pain-other)
  (ref (tabcurrent) 'other))

; file select {{{
(define (select-all)
  ($mcall toggle_isselect_all (pain-current)))
(define (select-up)
  ($mcall toggle_isselet_up (pain-current)))
(define (select-down)
  ($mcall toggle_isselet_down (pain-current)))
;}}}

; tab {{{
(define (tabnew)
  ($mcall tabnew model))
(define (tabclose)
  ($mcall removeTab model (ref model 'currentIndex)))
(define (tabnext)
  ($mcall nextTab model))
(define (tabprev)
  ($mcall prevTab model))
(define (tabchange n)
  ($mcall changeTab model n))
(define (tabfirst)
  (tabchange  0))
(define (tablast)
  (tabchange  (- (buitins::len (ref model 'tabs)) 1)))
(define (tabopen left right)
  (tabnew)
  ($mcall chdir (pain-current) left)
  ($mcall chdir (pain-other) right))
(define (tabnew-pwd)
  ; pwd をleftとrightから取得すること
  (tabopen (pwd) (pwd-other)))
(define *tabstorefile* ".tab")
(define (tabstore)
  (let ((port (open-output-file *tabstorefile*)))
	(write (map (lambda (tab)
				  (list ($mcall cwd ($mcall get_view_left tab))
						($mcall cwd ($mcall get_view_right tab))))
				(ref model 'tabs))
		   port)
	(close-output-port port)))
(define (tabrestore)
  (if (os.path::exists *tabstorefile*)
	(begin
	  ($mcall onlyTab model)
	  (let ((port (open-input-file *tabstorefile*)))
		(for-each (lambda (s) (tabopen (car s) (cadr s))) (read port))
		(close-input-port port))
	  (tabfirst)
	  (tabclose))
	#f))
; }}}

; platform {{{
(define (platform? str)
  (equal? (platform::system) str))
(define (windows?) (platform? "Windows"))
(define (mac?) (platform? "Darwin"))
; }}}

(define (reload-config)
  ($mcall load_config view))
(define (reload-command)
  ($mcall reload_commands model))
(define (reload-pain)
  ($mcall reload (pain-current)))
(define (reload-tab)
  (begin
	($mcall reload (pain-current))
	($mcall reload (pain-other))))
(define (reload-stylesheet)
  ($mcall load_stylesheet view))

; cursor {{{
(define (cursor-first)
  ($mcall cursor_first (pain-current)))
(define (cursor-last)
  ($mcall cursor_last (pain-current)))
(define (cursor-up)
  ($mcall cursor_up (pain-current)))
(define (cursor-down)
  ($mcall cursor_down (pain-current)))
;}}}

(define (change-pain)
  ($mcall change_focus (tabcurrent)))
(define (cd-cursor)
  ($mcall chdir (pain-current)))
(define (cd path)
  ($mcall chdir (pain-current) path))
(define (open-assoc)
  ($mcall open_assoc (pain-current)))
(define (cd-parent)
  ($mcall chdir_parent (pain-current)))
(define (popd)
  ($mcall popd (pain-current)))
(define (pwd)
  ($mcall cwd (pain-current)))
(define (pwd-other)
  ($mcall cwd (pain-other)))
(define (cd-other-to-current)
  ($mcall chdir (pain-other) ($mcall cwd (pain-current))))
(define (cd-current-to-other)
  ($mcall chdir (pain-current) ($mcall cwd (pain-other))))

(define (filer-current)
  (ref (pain-current) 'filer))

; view api {{{
(define (set-window-size width height)
  ($mcall set_window_size view width height))

(define (set-window-maximized)
  ($mcall set_window_maximized view))

(define set-default-font
  (lambda args
	  (apply (ref view 'set_defaultfont) args)))

(define (set-window-title title)
  ($mcall set_window_title view title))

(define (set-style style)
  ($mcall set_style view style))
; }}}

(define (normal-mode)
  ($mcall set_mode model "normal"))
(define (command-mode)
  ($mcall set_mode model "command"))
(define (sh-mode)
  ($mcall set_mode model "sh"))
(define (search-mode)
  ($mcall set_mode model "search"))

(define (search-name fstr)
  ($mcall search  (pain-current) fstr))
(define (search-next)
  ($mcall search  (pain-current)))
(define (search-prev)
  ($mcall rsearch  (pain-current)))
(define (search-cancel)
  ($mcall search_cancel (pain-current))
  (normal-mode))

(define (commandline-text)
  ($mcall text (ref (ref view 'cw) 'commandLine)))
(define (set-commandline-text text)
  ($mcall setText (ref (ref view 'cw) 'commandLine) text))

(define (do-command)
  (command::do_command
			 (+ "(" (commandline-text) ")"))
  (normal-mode))
(define (search-enter)
  ($mcall search_enter (pain-current) (commandline-text))
  (normal-mode))

; logging {{{
(define (log-debug str)
	(logging::debug (lispy::to_string str)))
(define (log-info str)
	(logging::info str))
; }}}

(define-macro sh
  (lambda args
	`(sh-call ,(pwd) (quote ,args))))
(define (do-sh)
  (command::do_command
			 (+ "(sh " (commandline-text) ")"))
  (normal-mode))

(define *favorite-path* "~/.pyfiler/favorite")
(define (cd-favorite)
  (cd *favorite-path*))
(define (mkdir-command)
  (sh-mode)
  (set-commandline-text "mkdir "))

; 関連付け開く {{{
(define *my-assoc* nil)
(define (open-my-assoc)
  (let* ((filename (file-name (cursor-file)))
		 (c (assoc
			  (cadr (os.path::splitext filename))
			  *my-assoc*)))
	(if (not (null? c))
	  (sh-popen (pwd) (append (cadr c) (list filename)))
	  nil)))

(set! *my-assoc*
  (cond ((windows?)
		 '((".xls" ("C:/Program Files (x86)/Microsoft Office/OFFICE11/excel.exe" "/t"))
		   (".doc" ("C:/Program Files (x86)/Microsoft Office/OFFICE11/winword.exe" "/f"))))
		(else nil)))
; }}}

; editor {{{
(define *editor*
  (cond
	((windows?) '("C:/Program Files (x86)/vim74-kaoriya-win64/gvim.exe" "-p" "--remote-tab-silent"))
	((mac?) '("open" "-a" "MacVim"))))
(define (open-editor)
  (sh-popen (pwd) (append *editor* (list (file-name (cursor-file))))))
; }}}

; file 関数{{{
(define (file-state st file)
  ($mcall get (ref file 'state) st))
(define (cursor-file)
  ($mcall get_cursor_file (pain-current)))
(define (file-name file)
  (file-state "filename" file))
(define (files)
  (ref (pain-current) 'files))
(define (select-file? file)
  (ref file 'isselect))
(define (select-files)
  (filter select-file? (files)))
(define (select-or-cursor-files)
  (if (null? (select-files))
	(list (cursor-file))
	(select-files)))
;}}}

; クリップボード {{{
(define (bind1st func arg1)
  (lambda args
	(apply func (cons arg1  args))))

(define (string-join delimiter lis)
	  (reduce (lambda (x y) (+ x delimiter y)) (car lis) (cdr lis)))

(define (set-clipboard-filename)
  (set-clipboard
	  (string-join "\n" (map file-name (select-or-cursor-files)))))

(define (set-clipboard-filepath)
  (set-clipboard
	(string-join "\n" (map (bind1st file-state "abspath") (select-or-cursor-files)))))
; }}}
; TODO Safeモードを実装すること
(define (pain-to-pain func files)
	(for-each (lambda (src) (func (file-state "abspath" src) (pwd-other))) files)
	(reload-tab))

(define (copy-files)
  (pain-to-pain (ref (filer-current) 'copy) (select-files)))

(define (move-files)
  (pain-to-pain (ref (filer-current) 'move) (select-files)))

(define (remove-files)
  (for-each (lambda (src) ((ref (filer-current) 'remove) (file-state "abspath" src) )) (select-files))
	(reload-tab))

(define (favorite-add)
  (let ((func (cond ((windows?) (ref (filer-current) 'create_shortcut))
					(else (ref (filer-current) 'create_symlink)))))
	(func (file-state "abspath" (cursor-file)) *favorite-path*)))


(in-package "global")
