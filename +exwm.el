;;; ~/.doom.d/+exwm.el -*- lexical-binding: t; -*-

;; installed: brightnessctl, slock

(use-package! exwm
  :config
  (setq exwm-workspace-number 7)
  (setq exwm-workspace-switch-create-limit 7)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  ;; From emacs wiki
  (defun toggle-transparency ()
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha)))
      (set-frame-parameter
       nil 'alpha
       (if (eql (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha)) (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive>) form.
                      ((numberp (cadr alpha)) (cadr alpha)))
                100)
           '(85. 85) '(100 . 100)))))
  (global-set-key (kbd "C-c t t") 'toggle-transparency))

;(use-package! exwm-mff
;    :config
;  (add-hook 'exwm-mode-hook 'exwm-mff-mode))

;;try
(use-package exwm-edit
  :config
  (defun ag-exwm/on-exwm-edit-compose ()
    (funcall 'markdown-mode))
  (add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose))

(defun zackteo/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer (format "%s - %s" exwm-class-name exwm-title)))
(add-hook 'exwm-update-title-hook 'zackteo/exwm-rename-buffer-to-title)
(add-hook 'exwm-update-class-hook
          (defun my-exwm-update-class-hook ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name)
                        (string= "Firefox" exwm-class-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (defun my-exwm-update-title-hook ()
            (cond ((or (not exwm-instance-name)
                       (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                       (string= "gimp" exwm-instance-name)
                       (string= "Firefox" exwm-class-name))
                   (exwm-workspace-rename-buffer exwm-title)))))

;; TODO remove
;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        ((interactive))
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
                    (interactive)
                    (start-process "" nil "/usr/bin/slock")))))

;; More for line mode
;(push ?\s-  exwm-input-prefix-keys)
;(push ?\M-  exwm-input-prefix-keys)

(display-time-mode 1)

(defun zackteo/launch (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun zackteo/screen-to-clipboard ()
  (interactive)
  (shell-command
   (concat "bash -c 'FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim -s $FILENAME"
           " && xclip $FILENAME -selection clipboard "
           "-t image/png &> /dev/null && rm $FILENAME'"))
  (message "Added to clipboard."))

(defun zackteo/switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Global keys also
(exwm-input-set-key (kbd "s-<backspace>") #'zackteo/launch)
;(exwm-input-set-key (kbd "s-p") #'password-store-copy)
;(exwm-input-set-key (kbd "C-x t") #'vterm)
;(exwm-input-set-key (kbd "s-t a") #'zackteo/switch-to-agenda)
;(exwm-input-set-key (kbd "s-t m") #'notmuch)
;(exwm-input-set-key (kbd "s-c") #'zackteo/org-inbox-capture)
(exwm-input-set-key (kbd "s-f") #'counsel-find-file)
(exwm-input-set-key (kbd "s-F") #'counsel-locate)
(exwm-input-set-key (kbd "s-<tab>") #'zackteo/switch-to-last-buffer)
(exwm-input-set-key (kbd "<pause>") #'zackteo/screen-to-clipboard)
(exwm-input-set-key (kbd "s-b") #'zackteo/exwm-ibuffer)

;; move focus directionally
(exwm-input-set-key (kbd "<s-up>") 'exwm-windmove-up)
(exwm-input-set-key (kbd "<s-down>") 'exwm-windmove-down)
(exwm-input-set-key (kbd "<s-right>") 'exwm-windmove-right)
(exwm-input-set-key (kbd "<s-left>") 'exwm-windmove-left)

;; move visible buffer directionally
(exwm-input-set-key (kbd "<C-S-up>") #'buf-move-up)
(exwm-input-set-key (kbd "<C-S-down>") #'buf-move-down)
(exwm-input-set-key (kbd "<C-S-left>") #'buf-move-left)
(exwm-input-set-key (kbd "<C-S-right>") #'buf-move-right)

;; window resizing
(exwm-input-set-key (kbd "s-[") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-{") 'shrink-window)
(exwm-input-set-key (kbd "s-]") 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-}") 'enlarge-window)

;; workspace switching
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)
(exwm-input-set-key (kbd "s-0") (lambda() (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-1") (lambda() (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-2") (lambda() (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-3") (lambda() (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-4") (lambda() (interactive) (exwm-workspace-switch 4)))
(exwm-input-set-key (kbd "s-5") (lambda() (interactive) (exwm-workspace-switch 5)))
(exwm-input-set-key (kbd "s-6") (lambda() (interactive) (exwm-workspace-switch 6)))

;; Switching workspaces
(defun exwm-workspace-switch-previous (p)
  "Switch to previous workspace"
  (interactive "p")
  (if (< (- exwm-workspace-current-index p) 0)
      (exwm-workspace-switch (1- (length exwm-workspace--list)))
    (exwm-workspace-switch (- exwm-workspace-current-index p))))

(defun exwm-workspace-switch-next (p)
  "Switch to next workspace"
  (interactive "p")
  (if (> (+ exwm-workspace-current-index p) (1- (length exwm-workspace--list)))
      (exwm-workspace-switch 0)
    (exwm-workspace-switch (+ exwm-workspace-current-index p))))

(require 'windmove)
(defun exwm-windmove-left (&optional arg)
  "Like windmove-left but go to previous workspace if there is
no window on the left."
  (interactive "P")
  (if (or (<= exwm-connected-displays 1) (windmove-find-other-window 'left arg))
      (windmove-do-window-select 'left arg)
    ;; No window to the left
    ;; Switch to previous workspace and select rightmost window
    (exwm-workspace-switch-previous 1)
    (while (windmove-find-other-window 'right arg)
      (windmove-do-window-select 'right arg))))

(defun exwm-windmove-right (&optional arg)
  "Like windmove-right but go to previous workspace if there is
no window on the right."
  (interactive "P")
  (if (or (<= exwm-connected-displays 1) (windmove-find-other-window 'right arg))
      (windmove-do-window-select 'right arg)
    ;; No window to the left
    ;; Switch to next workspace and select leftmost window
    (exwm-workspace-switch-next 1)
    (while (windmove-find-other-window 'left arg)
      (windmove-do-window-select 'left arg))))

(setq exwm-windmove-workspace-1-below-p t)
;; FIXME: Automatically get displayed workspace on top monitor
(setq exwm-windmove-last-workspace-top 1)

(defun exwm-windmove-down (&optional arg)
  "Like windmove-down but go to workspace 1 if there is no window
or active minibuffer below and `exwm-windmove-workspace-1-below-p' is non-NIL."
  (interactive "P")
  (let ((active-minibuffer-below-p
         (and (minibuffer-window-active-p (minibuffer-window))
              (eq (minibuffer-window) (windmove-find-other-window 'down arg)))))
    (if (or (<= exwm-connected-displays 1)
            active-minibuffer-below-p
            (= exwm-workspace-current-index 0)
            (not (eq (minibuffer-window) (windmove-find-other-window 'down arg))))
        (windmove-do-window-select 'down arg)
      ;; No window below
      (when exwm-windmove-workspace-1-below-p
        ;; Switch to workspace 0 and select top window
        (setq exwm-windmove-last-workspace-top exwm-workspace-current-index)
        (exwm-workspace-switch 0)
        (while (windmove-find-other-window 'up arg)
          (windmove-do-window-select 'up arg))))))

(defun exwm-windmove-up (&optional arg)
  "Like windmove-up but go to workspace 1 if there is
no window below and `exwm-windmove-workspace-1-below-p' is non-NIL."
  (interactive "P")
  (if (or (<= exwm-connected-displays 1) (windmove-find-other-window 'up arg))
      (windmove-do-window-select 'up arg)
    ;; No window below
    (when exwm-windmove-workspace-1-below-p
      ;; Switch to workspace 1 and select bottom window
      (exwm-workspace-switch exwm-windmove-last-workspace-top)
      (while (windmove-find-other-window 'down arg)
        (windmove-do-window-select 'down arg)))))


(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "Xfce4-terminal"))
              (exwm-input-set-local-simulation-keys '(([?\C-c ?\C-c] . ?\C-c))))))

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home]) ;; to change??
        ([?\C-e] . [end])
        ([?\C-v] . [next])
        ([?\M-v] . [prior])
        ([?\M-<] . [C-home])
        ([?\M->] . [C-end])
        ;; delete
        ([?\C-d] . [delete])
        ([?\M-d] . [C-delete])
        ;; kill/cut/copy/paste/selection
        ([?\C-k] . [S-end C-x])
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;([?\C-S-y] . [?\C-\S-v]) ; paste without formatting
        ([?\s-a] . [?\C-a]) ; should it be s-h cause C-x h
        ;; undo/redo
        ([?\C-/] . [?\C-z])
        ([?\C-?] . [?\C-y]) ; redo
        ;; search
        ([?\C-s] . [?\C-f])
        ;; selection-shift movements
        ([?\C-\S-b] . [S-left])
        ([?\C-\S-f] . [S-right])
        ([?\C-\S-p] . [S-up])
        ([?\C-\S-n] . [S-down])
        ([?\M-B] . [C-S-left])
        ([?\M-F] . [C-S-right])
        ;; multiplier, doesn't need to be here but reminder
        ([C-u] . [C-u])
        ;; newline
        ([\\S-\\r] . [end return])
        ;;Navigation C-] is going to home ? Alt-Home?
        ([?\M-\[] . [M-left]) ; back 
        ([?\M-\]] . [M-right]); forward


        ;; Open new window?
        ;([?\M-P] . [C-S-p])
        ;([?\M-N] . [C-S-n])

        ;; firefox stuffs?
        ;; Focus Tab Bar
        ([?\s-z] . [?\C-l S-tab S-tab S-tab]) ;; sometime no back history button hmmm
        ([?\C-g] . [escape])

        ;([S-tab] . [C-left C-S-right C-c])
        ;; copy link
        ;([?\C-l] . [?\C-l ?\C-c])
        ;; questionable shifting line up/down
        ;;([?\M-p] . [home S-end ?\C-c backspace delete up home return up ?\C-v])
        ;;([?\M-n] . [home S-end ?\C-c backspace delete end return ?\C-v])

        ([?\C-\s-p] . [C-p])
        ([?\C-\s-n] . [C-n])
        ([?\C-\S-w] . [?\C-w])
        ([?\C-\s-k] . [C-k])
        ([?\C-\'] . [\" home \" return]);; idk
        ([?\C-\"] . [\" end \" return]) ;; quoting
        ([?\C-\,] . [home S-end ?\C-c])
        ([?\C-\s-b] . [?\C-b])
        ([?\C-\s-s] . [?\C-s])
        ([?\s-a] . [?\C-a])
        ([?\C-\S-u] . [C-S-f])

        ;; Toggle Developer Tools
        ([?\M-i] . [C-S-i])
        ([?\M-k] . [C-S-k]) ;; web console - can't toggle
        ;; C-S-M Responsive Design View
        ;; C-S-S Debugger
        ;; M-s Menu
       ))

(exwm-input-set-simulation-keys exwm-input-simulation-keys)


;(defun zackteo/opacity-100 ()
;  (interactive)
;  (shell-command "transset-df 1")
;  (message "Nay :("))
;(defun zackteo/opacity-085 ()
;  (interactive)
;  (shell-command "transset-df 0.85")
;  (message "Yay :D"))
;(exwm-input-set-key (kbd "s--") #'zackteo/opacity-100) ;;???
;(exwm-input-set-key (kbd "s-=") #'zackteo/opacity-085)

;(defun zackteo/launch-fn (command)
;  (start-process-shell-command command nil command))
;(exwm-input-set-key (kbd "s-l u") (interactive) (zackteo/launch-fn "wine /home/zackteo/.wine/drive_c/Program\ Files\ \(x86\)/Universalis/Universalis.exe"))

(define-ibuffer-column exwm-class (:name "Class")
  (if (bound-and-true-p exwm-class-name)
      exwm-class-name
    ""))
(define-ibuffer-column exwm-instance (:name "Instance")
  (if (bound-and-true-p exwm-instance-name)
      exwm-instance-name
    ""))
(define-ibuffer-column exwm-urgent (:name "U")
  (if (bound-and-true-p exwm--hints-urgency)
      "U"
    " "))

(defun zackteo/exwm-ibuffer (&optional other-window)
  (interactive "P")
  (let ((name (buffer-name)))
    (ibuffer other-window
             "*exwm-ibuffer*"
             '((mode . exwm-mode))
             nil nil nil
             '((mark exwm-urgent
                     " "
                     (name 64 64 :left :elide)
                     " "
                     (exwm-class 20 -1 :left)
                     " "
                     (exwm-instance 10 -1 :left))))
    (ignore-errors (ibuffer-jump-to-buffer name))))


;; already implemented
;(define-key exwm-mode-map (kbd "C-x 4 0")
;  (lambda ()
;    (interactive)
;    (kill-buffer)
;    (delete-window)))


(require 'exwm-randr)

(defvar exwm-connected-displays 1
  "Number of connected displays.")

;; Update exwm-randr-workspace-output-plist with 2 or 3 outputs named
;; 'primary' and 'other-1'/'other-2'.
;; With 3 outputs connected the first workspace will be primary,
;; second workspace goes to 'other-2' and all others to 'other-1'.
;; With 2 outputs, first workspace is 'primary' display and rest 'other-1'.
;; And with only one connected output, primary has all workspaces.
(defun dakra-exwm-randr-screen-change ()
  (let* ((connected-cmd "xrandr -q|awk '/ connected/ {print $1}'")
         (connected (process-lines "bash" "-lc" connected-cmd))
         (primary (car connected))  ; Primary display is always first in list
         (other-1 (cadr connected))
         (other-2 (caddr connected)))
    (setq exwm-connected-displays (length connected))
    (setq exwm-randr-workspace-monitor-plist
          ;(append (list 0 other-1)
          ;        (list 1 (or other-2 other-1 primary))
          ;        (mapcan (lambda (i) (list i (or primary other-1 other-2)))
          ;                (number-sequence 2 exwm-workspace-number))))
          (append (list 0 (or other-1 primary))
                  (mapcan (lambda (i) (list i (or other-2 other-1 primary)))
                          (number-sequence 1 (/ exwm-workspace-number 2)))
                  (mapcan (lambda (i) (list i (or primary other-1 other-2)))
                          (number-sequence (+ 1 (/ exwm-workspace-number 2)) exwm-workspace-number))))
    (exwm-randr-refresh)
    (message "Randr: %s monitors refreshed." (string-join connected ", "))))

(add-hook 'exwm-randr-screen-change-hook #'dakra-exwm-randr-screen-change)

; Non automated
;(setq exwm-randr-workspace-output-plist '(1 "HDMI-1" 2 "eDP-1" 3 "HDMI-1" 4 "eDP-1" 5 "HDMI-1" 6 "eDP-1" 0 "HDMI-1"))
;(add-hook 'exwm-randr-screen-change-hook
;          (lambda ()
;            (start-process-shell-command
;             "xrandr" nil "xrandr --output HDMI-1 --left-of eDP-1 --auto")))
(exwm-randr-enable)
;; Modified FailSafe session in /etc/xdg/xfce4/xfconf/xfce-perchannel-xml/xfce-session.xml
;; /usr/share/xsessions/emacs.desktop
(exwm-enable)

;(require 'exwm-systemtray)
;(exwm-systemtray-enable)

(shell-command "picom &")
