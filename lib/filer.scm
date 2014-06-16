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
  (method::currentTab model))
(define (pain-current)
  (attr::current (tabcurrent)))
(define (pain-other)
  (attr::other (tabcurrent)))

; file select {{{
(define (select-all)
  (method::toggle_isselect_all (pain-current)))
(define (select-up)
  (method::toggle_isselet_up (pain-current)))
(define (select-down)
  (method::toggle_isselet_down (pain-current)))
;}}}

; tab {{{
(define (tabnew)
  (method::tabnew model))
(define (tabclose)
  (method::removeTab model (attr::currentIndex model)))
(define (tabnext)
  (method::nextTab model))
(define (tabprev)
  (method::prevTab model))
(define (tabchange n)
  (method::changeTab model n))
(define (tabfirst)
  (tabchange  0))
(define (tablast)
  (tabchange  (- (buitins::len (attr::tabs model)) 1)))
(define (tabopen left right)
  (tabnew)
  (method::chdir (pain-current) left)
  (method::chdir (pain-other) right))
(define (tabnew-pwd)
  ; pwd をleftとrightから取得すること
  (tabopen (pwd) (pwd-other)))
(define *tabstorefile* ".tab")
(define (tabstore)
  (let ((port (open-output-file *tabstorefile*)))
	(write (map (lambda (tab)
				  (list (method::cwd (method::get_view_left tab))
						(method::cwd (method::get_view_right tab))))
				(attr::tabs model))
		   port)
	(close-output-port port)))
(define (tabrestore)
  (if (os.path::exists *tabstorefile*)
	(begin
	  (method::onlyTab model)
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
  (method::load_config view))
(define (reload-command)
  (method::reload_commands model))
(define (reload-pain)
  (method::reload (pain-current)))
(define (reload-tab)
  (begin
	(method::reload (pain-current))
	(method::reload (pain-other))))
(define (reload-stylesheet)
  (method::load_stylesheet view))

; cursor {{{
(define (cursor-first)
  (method::cursor_first (pain-current)))
(define (cursor-last)
  (method::cursor_last (pain-current)))
(define (cursor-up)
  (method::cursor_up (pain-current)))
(define (cursor-down)
  (method::cursor_down (pain-current)))
;}}}

(define (change-pain)
  (method::change_focus (tabcurrent)))
(define (cd-cursor)
  (method::chdir (pain-current)))
(define (cd path)
  (method::chdir (pain-current) path))
(define (open-assoc)
  (method::open_assoc (pain-current)))
(define (cd-parent)
  (method::chdir_parent (pain-current)))
(define (popd)
  (method::popd (pain-current)))
(define (pwd)
  (method::cwd (pain-current)))
(define (pwd-other)
  (method::cwd (pain-other)))
(define (cd-other-to-current)
  (method::chdir (pain-other) (method::cwd (pain-current))))
(define (cd-current-to-other)
  (method::chdir (pain-current) (method::cwd (pain-other))))

(define (filer-current)
  (attr::filer (pain-current)))

; view api {{{
(define (set-window-size width height)
  (method::set_window_size view width height))

(define (set-window-maximized)
  (method::set_window_maximized view))

(define set-default-font
  (lambda args
	  (apply (attr::set_defaultfont view) args)))

(define (set-window-title title)
  (method::set_window_title view title))

(define (set-style style)
  (method::set_style view style))
; }}}

(define (normal-mode)
  (method::set_mode model "normal"))
(define (command-mode)
  (method::set_mode model "command"))
(define (sh-mode)
  (method::set_mode model "sh"))
(define (search-mode)
  (method::set_mode model "search"))

(define (search-name fstr)
  (method::search (pain-current) fstr))
(define (search-next)
  (method::search  (pain-current)))
(define (search-prev)
  (method::rsearch  (pain-current)))
(define (search-cancel)
  (method::search_cancel (pain-current))
  (normal-mode))

(define (commandline-text)
  (method::text (attr::commandLine (attr::cw view))))
(define (set-commandline-text text)
  (method::setText (attr::commandLine(attr::cw view)) text))

(define (do-command)
  (command::do_command
			 (+ "(" (commandline-text) ")"))
  (normal-mode))
(define (search-enter)
  (method::search_enter (pain-current) (commandline-text))
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

; file 関数{{{
(define (file-state st file)
  (method::get (attr::state file) st))
(define (cursor-file)
  (method::get_cursor_file (pain-current)))
(define (file-name file)
  (file-state "filename" file))
(define (files)
  (attr::files (pain-current)))
(define (select-file? file)
  (attr::isselect file))
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
  (method::join delimiter lis))
  ;(reduce (lambda (x y) (+ x delimiter y)) (car lis) (cdr lis)))

;(require-python "tkinter")
;(define (set-clipboard text)
  ;(cond ((windows?) (begin
					 ;(method::clipboard_clear (tkinter::Text))
					 ;(method::clipboard_append (tkinter::Text) text)))
		;(else *undef*)))

;(define (clipboard)
  ;(cond ((windows?) (method::clipboard_get (tkinter::Text)))
		;(else *undef*)))

;(define (set-clipboard-filename)
  ;(set-clipboard
	  ;(string-join "\n" (map file-name (select-or-cursor-files)))))

;(define (set-clipboard-filepath)
  ;(set-clipboard
	;(string-join "\n" (map (bind1st file-state "abspath") (select-or-cursor-files)))))
;; }}}

; TODO Safeモードを実装すること
(define (pain-to-pain func files)
	(for-each (lambda (src) (func (file-state "abspath" src) (pwd-other))) files)
	(reload-tab))

(define (copy-files)
  (pain-to-pain (attr::copy (filer-current)) (select-files)))

(define (move-files)
  (pain-to-pain (attr::move (filer-current)) (select-files)))

(define (remove-files)
  (for-each (lambda (src) ((attr::remove (filer-current)) (file-state "abspath" src) )) (select-files))
	(reload-tab))

(define (favorite-add)
  (let ((func (cond ((windows?) (attr::create_shortcut (filer-current)))
					(else (attr::create_symlink (filer-current))))))
	(func (file-state "abspath" (cursor-file)) *favorite-path*)))


(in-package "global")
