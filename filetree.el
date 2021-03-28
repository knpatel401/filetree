;;; filetree.el --- File tree view/manipulatation package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ketan Patel
;;
;; Author: Ketan Patel <knpatel401@gmail.com>
;; URL: https://github.com/knpatel401/filetree
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.0.0
;; Package-Requires: ((dash "2.12.0"))
;;; Commentary:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package displays file list in tree view and allows user to
;; manipulate files using the tree view.
;; -------------------------------------------
;;; Code:
(require 'dash)
(require 'xref)
(require 'helm)
;;(require 'cl-lib)

;; External functions/variables
(declare-function org-narrow-to-subtree "org" ())
(defvar recentf-list)
;;(defvar text-scale-mode-amount 0)

(defgroup filetree nil
  "Tree view of file list and file notes."
  :group 'matching
  :prefix "filetree-")

(defvar filetree-version "1.0")

(defconst filetree-buffer-name "*filetree*")

(defcustom filetree-notes-file (concat user-emacs-directory
                                       "filetree-notes.org")
  "File used for file specific notes."
  :type 'file)
(defcustom filetree-saved-lists-file (concat user-emacs-directory
                                             "filetree-saved-lists.el")
  "File used for saved file lists."
  :type 'file)
(defcustom filetree-default-file-face 'default
  "Default face to use for files.
This is used if the file doesn't match any regex in `filetree-filetype-list'."
  :type 'face)
(defcustom filetree-use-all-the-icons nil
  "Set to t to use file and directory icons.
This can also be toggled using `filetree-toggle-use-all-icons'."
  :type 'boolean)
(defcustom filetree-info-window nil
  "Set to t to show info in side window.
This can also be toggled using `filetree-toggle-info-buffer'."
  :type 'boolean)
(defcustom filetree-excludeList
  '("~$" "#$" ".git\/" ".gitignore$" "\/\.\/$" "\/\.\.\/$" ".DS_Store$")
  "List of regex for files to exclude from file list."
  :type '(repeat regexp))

(defgroup filetree-symb-for nil
  "Symbols used for drawing tree in filetree package."
  :group 'filetree
  :prefix "filetree-symb-for-")

(defcustom filetree-symb-for-root "\u25ba"
  "Symbol for end of mark indicating root dir."
  :type 'character)
(defcustom filetree-symb-for-box "\u25a0"
  "Box symbol used in mark for root dir."
  :type 'character)
(defcustom filetree-symb-for-vertical-pipe "\u2502"
  "Symbol to indicate continuing branch."
  :type 'character)
(defcustom filetree-symb-for-horizontal-pipe "\u2500"
  "Symbol for branch for node on current line."
  :type 'character)
(defcustom filetree-symb-for-left-elbow "\u2514"
  "Symbol for last node on branch."
  :type 'character)
(defcustom filetree-symb-for-right-elbow "\u2518"
  "Symbol for bottom right hand corner."
  :type 'character)
(defcustom filetree-symb-for-branch-and-cont "\u251c"
  "Symbol indicating continuing branch which also includes node on current line."
  :type 'character)
(defcustom filetree-symb-for-file-node "\u25cf"
  "Symbol for file node."
  :type 'character)

(defvar filetree-info-buffer nil)
(defvar filetree-info-buffer-state nil)
(defvar filetree-saved-lists '(("recentf" (lambda ()
                                            recentf-list))))
(if (file-exists-p filetree-saved-lists-file)
    (with-temp-buffer
      (insert-file-contents filetree-saved-lists-file)
      (eval-buffer)))
  
(defvar filetree-startPosition 0)
(defvar filetree-maxDepth 0)
(defvar filetree-overallDepth nil)
(defvar filetree-currentFileList nil)
(defvar filetree-fileListStack nil)
(defvar filetree-fileListStack-save nil)
(defvar filetree-showFlatList nil)
(defvar filetree-combineDirNames t)
(defvar filetree-helm-source
  '((name . "filetree")
    (candidates . filetree-currentFileList)
    (cleanup . (lambda ()
                 (remove-hook 'helm-after-update-hook
                              #'filetree-helm-hook)
                 (setq filetree-fileListStack filetree-fileListStack-save)
                 (filetree-updateBuffer)))
    (buffer . ("*helm-filetree-buffer*"))
    (prompt . ("selection:"))))
  
(defvar filetree-filetype-list nil
  "List of file types with filter shortcuts, regex for filetype, and face.
This is populated using `filetree-add-filetype', for example see
`filetree-configure-default-filetypes'")

(defvar filetree-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "?" '(lambda () (interactive) (message "%s %s" (filetree-getName)
    ;;                                                          (if (button-at (point))
    ;;                                                              (button-get (button-at (point)) 'subtree)
    ;;                                                            nil))))
    (define-key map "j" 'filetree-next-line)
    (define-key map "k" 'filetree-prev-line)
    (define-key map (kbd "<down>") 'filetree-next-line)
    (define-key map (kbd "<up>") 'filetree-prev-line)
    (define-key map (kbd "C-j") 'filetree-next-line)
    (define-key map (kbd "C-k") 'filetree-prev-line)
    (define-key map (kbd "SPC") 'filetree-next-branch)
    (define-key map (kbd "TAB") 'filetree-prev-branch)
    (define-key map "q" 'filetree-close-session)
    (define-key map "0" 'filetree-set-maxDepth)
    (define-key map "1" 'filetree-set-maxDepth1)
    (define-key map "2" 'filetree-set-maxDepth2)
    (define-key map "3" 'filetree-set-maxDepth3)
    (define-key map "4" 'filetree-set-maxDepth4)
    (define-key map "5" 'filetree-set-maxDepth5)
    (define-key map "6" 'filetree-set-maxDepth6)
    (define-key map "7" 'filetree-set-maxDepth7)
    (define-key map "8" 'filetree-set-maxDepth8)
    (define-key map "9" 'filetree-set-maxDepth9)
    (define-key map "r" 'filetree-showRecentfFiles)
    (define-key map "f" 'filetree-filter)
    (define-key map "/" 'filetree-toggle-combineDirNames)
    (define-key map "b" 'filetree-pop-fileListStack)
    (define-key map "g" 'filetree-grep)
    (define-key map "d" 'filetree-run-dired)
    (define-key map "e" 'filetree-expandDir)
    (define-key map "E" 'filetree-expandDirRecursively)
    (define-key map "x" 'filetree-remove-item)
    (define-key map "L" 'filetree-select-file-list)
    (define-key map "S" 'filetree-save-list)
    (define-key map "D" 'filetree-delete-list)
    (define-key map "-" 'filetree-reduceListBy10)
    (define-key map "." 'filetree-toggle-flat-vs-tree)
    (define-key map "i" 'filetree-toggle-info-buffer)
    (define-key map "I" (lambda ()
                          "Toggle filetree-info-buffer and switch to it if active"
                          (interactive)
                          (filetree-toggle-info-buffer t)))
    (define-key map "s" 'filetree-helm-filter)
    (define-key map ";" 'filetree-toggle-use-all-icons)
    map)
  "Keymap for filetree.")

(defun filetree-close-session ()
  "Close filetree session."
  (interactive)
  (kill-buffer (current-buffer)))

(defun filetree-add-filetype (name shortcut regex face)
  "Add a filetype to `filetree-filetype-list' for special handling.
- NAME is the name of the filetype (e.g., \"elisp\")
- SHORTCUT is the shortcut char literal to use for this filetype when filtering
- REGEX is the regular expression to use for detecting the filetype
- FACE is the face to use for this filetype"
  (print shortcut)
  (push (list shortcut name regex face) filetree-filetype-list))

(defun filetree-configure-default-filetypes ()
  "Define default `filetree-filetype-list'.
This defines filetype faces, filetype regex, and filter shortcuts.
This function is given as an example.  The user can generate their own
custom function with calls to `filetree-add-filetype'"
  (interactive)
  (setq filetree-filetype-list nil)
  (filetree-add-filetype "No Filter" 0   ""        ())
  (filetree-add-filetype "Python"    ?p  "\.py$"   '(:foreground "steel blue"))
  (filetree-add-filetype "Org-mode"  ?o  "\.org$"  '(:foreground "DarkOliveGreen4"))
  (filetree-add-filetype "elisp"     ?e  "\\(?:\\.e\\(?:l\\|macs\\)\\)"  '(:foreground "purple"))
  (filetree-add-filetype "C"         ?c  "\\(?:\\.[ch]$\\|\\.cpp\\)"     '(:foreground "navyblue"))
  (filetree-add-filetype "PDF"       ?d  "\.pdf$"  '(:foreground "maroon"))
  (filetree-add-filetype "Matlab"    ?m  "\.m$"    '(:foreground "orange"))
  (filetree-add-filetype "Text"      ?t  "\.txt$"  '(:foreground "gray50")))

(filetree-configure-default-filetypes)

(defun filetree-filter ()
  "Interactive function to filter 'filetree-currentFileList'.
Uses regular expression in 'filetree-filetype-list' or by expression entered by user."
  (interactive)
  (let ((myFileTree-mode-filterList (mapcar #'(lambda (x)
                                                (seq-subseq x 0 3))
                                            filetree-filetype-list))
        (myFileTree-regex nil)
        (filetree-elem nil)
        (filetree-charInput (read-char)))
    (while (/= (length myFileTree-mode-filterList) 0)
      (setq filetree-elem (car myFileTree-mode-filterList))
      (if (eq filetree-charInput (car filetree-elem))
          (setq myFileTree-regex (nth 2 filetree-elem)))
      (setq myFileTree-mode-filterList (cdr myFileTree-mode-filterList)))
    (if (not myFileTree-regex)
        (setq myFileTree-regex (read-string "Type a string:")))
    (setq filetree-currentFileList (delete nil (mapcar #'(lambda (x)
                                                           (if (string-match
                                                                myFileTree-regex
                                                                (file-name-nondirectory x))
                                                               x nil))
                                                       filetree-currentFileList)))
    (filetree-updateBuffer)))

(defun filetree-remove-item (&optional file_or_dir)
  "Remove the file or subdir FILE_OR_DIR from the `filetree-currentFileList'.
If file_or_dir not specified, use file or dir at point."
  (interactive)
  (let ((file_or_dir (or file_or_dir (filetree-getName))))
    (setq filetree-currentFileList (delete
                                    nil
                                    (if (string= "/"
                                                 (substring file_or_dir -1))
                                        ;; removing subdirectory
                                        (mapcar #'(lambda (x)
                                                    (if (string-match
                                                         file_or_dir
                                                         x)
                                                        nil x))
                                                filetree-currentFileList)
                                      ;; removing file
                                      (mapcar #'(lambda (x)
                                                  (if (string=
                                                       file_or_dir
                                                       x)
                                                      nil x))
                                              filetree-currentFileList)))))
  (filetree-updateBuffer))

(defun filetree-getName ()
  "Get name of file/dir on line at current point."
  (interactive)
  (if (button-at (point))
      (button-get (button-at (point)) 'name)
    nil))

(defun filetree-expandDir (&optional dir filter)
  "Add files in DIR to 'filetree-currentFileList'.
If DIR is not specified, use dir at point.
If FILTER is not specified, will 'read-char' from user.
The corresponding regular expression in 'filetree-filetype-list' will
be used to filter which files are included.  If FILTER does not
correspond to an entry in 'filetree-filetype-list' the user is
prompted for a string/regular expression to filter with."
  (interactive)
  (let ((dir (or dir (filetree-getName)))
        (filetree-charInput (or filter (read-char)))
        (myFileTree-mode-filterList (mapcar #'(lambda (x)
                                                (seq-subseq x 0 3))
                                            filetree-filetype-list))
        (filetree-newFiles nil)
        (myFileTree-regex nil)
        (filetree-elem nil))
    (while (/= (length myFileTree-mode-filterList) 0)
      (setq filetree-elem (car myFileTree-mode-filterList))
      (if (eq filetree-charInput (car filetree-elem))
          (setq myFileTree-regex (nth 2 filetree-elem)))
      (setq myFileTree-mode-filterList (cdr myFileTree-mode-filterList)))
    (if (not myFileTree-regex)
        (setq myFileTree-regex (read-string "Type a string:")))
    (setq filetree-newFiles (delete nil (mapcar #'(lambda (x)
                                                    (if (string-match
                                                         myFileTree-regex
                                                         (file-name-nondirectory (car x)))
                                                        (if (stringp (nth 1 x))
                                                            nil
                                                          (if (null (nth 1 x))
                                                              (car x)
                                                            (concat (car x) "/")))
                                                      nil))
                                                (directory-files-and-attributes dir t))))
    (dolist (entry filetree-excludeList)
                   (setq filetree-newFiles (delete nil (mapcar #'(lambda (x)
                                                                   (if (string-match
                                                                        entry
                                                                        x)
                                                                       nil
                                                                     x))
                                                               filetree-newFiles))))
    (setq filetree-currentFileList
          (-distinct (-non-nil
                      (nconc filetree-currentFileList
                             filetree-newFiles)))))
  (filetree-updateBuffer))

(defun filetree-expandDirRecursively (&optional dir filter)
  "Recursively add files in DIR to 'filetree-currentFileList'.
If DIR is not specified, use dir at point.
If FILTER is not specified, will 'read-char' from user.  The corresponding
regular expression in 'filetree-filetype-list' will be used to filter
which files are included.  If FILTER does not correspond to an entry in
'filetree-filetype-list' the user is prompted for a string/regular
expression to filter with."
  (interactive)
  (let ((dir (or dir (filetree-getName)))
        (filetree-charInput (or filter (read-char)))
        (myFileTree-mode-filterList (mapcar #'(lambda (x)
                                                (seq-subseq x 0 3))
                                            filetree-filetype-list))
        (myFileTree-regex nil)
        (filetree-elem nil)
        (filetree-newFiles nil))
    (while (/= (length myFileTree-mode-filterList) 0)
      (setq filetree-elem (car myFileTree-mode-filterList))
      (if (eq filetree-charInput (car filetree-elem))
          (setq myFileTree-regex (nth 2 filetree-elem)))
      (setq myFileTree-mode-filterList (cdr myFileTree-mode-filterList)))
    (if (not myFileTree-regex)
        (setq myFileTree-regex (read-string "Type a string:")))
    (setq filetree-newFiles (directory-files-recursively dir myFileTree-regex))
    (dolist (entry filetree-excludeList)
      (setq filetree-newFiles (delete nil (mapcar #'(lambda (x)
                                                      (if (string-match entry x)
                                                          nil
                                                        x))
                                                  filetree-newFiles))))
    (setq filetree-currentFileList (-distinct (-non-nil
                                               (nconc filetree-currentFileList
                                                      filetree-newFiles)))))
  (filetree-updateBuffer))

(defun filetree-run-dired ()
  "Run dired on directory at point."
  (dired (filetree-getName)))

(defun filetree-reduceListBy10 ()
  "Drop last 10 entries in `filetree-currentFileList'."
  (interactive)
  (if (>= (length filetree-currentFileList) 20)
      (setq filetree-currentFileList (butlast filetree-currentFileList 10))
    (if (>= (length filetree-currentFileList) 10)
        (setq filetree-currentFileList
              (butlast filetree-currentFileList
                       (- (length filetree-currentFileList) 10)))))
  (message "file list length: %d" (length filetree-currentFileList))
  (filetree-updateBuffer))

(defun filetree-cycle-maxDepth ()
  "Increase depth of file tree by 1 level cycle back to 0 when max depth reached."
  (interactive)
  (setq filetree-maxDepth (% (+ filetree-maxDepth 1)
                             filetree-overallDepth))
  (filetree-updateBuffer))
  
(defun filetree-set-maxDepth (&optional maxDepth)
  "Set depth of displayed file tree to MAXDEPTH.
If maxdepth not specified, show full tree."
  (interactive)
  (setq filetree-maxDepth (or maxDepth 0))
  (filetree-updateBuffer))

(defun filetree-set-maxDepth1 ()
  "Set depth of displayed file to 1."
  (interactive)
  (filetree-set-maxDepth 1))

(defun filetree-set-maxDepth2 ()
  "Set depth of displayed file to 2."
  (interactive)
  (filetree-set-maxDepth 2))

(defun filetree-set-maxDepth3 ()
  "Set depth of displayed file to 3."
  (interactive)
  (filetree-set-maxDepth 3))

(defun filetree-set-maxDepth4 ()
  "Set depth of displayed file to 4."
  (interactive)
  (filetree-set-maxDepth 4))

(defun filetree-set-maxDepth5 ()
  "Set depth of displayed file to 5."
  (interactive)
  (filetree-set-maxDepth 5))

(defun filetree-set-maxDepth6 ()
  "Set depth of displayed file to 6."
  (interactive)
  (filetree-set-maxDepth 6))

(defun filetree-set-maxDepth7 ()
  "Set depth of displayed file to 7."
  (interactive)
  (filetree-set-maxDepth 7))

(defun filetree-set-maxDepth8 ()
  "Set depth of displayed file to 8."
  (interactive)
  (filetree-set-maxDepth 8))

(defun filetree-set-maxDepth9 ()
  "Set depth of displayed file to 9."
  (interactive)
  (filetree-set-maxDepth 9))

(defun filetree-next-line ()
  "Go to file/dir on next line."
  (interactive)
  (move-end-of-line 2)
  (re-search-backward " ")
  (filetree-goto-node))

(defun filetree-prev-line ()
  "Go to file/dir on previous line."
  (interactive)
  (forward-line -1)
  (filetree-goto-node))

(defun filetree-goto-node ()
  "Move point to item on current line."
  (interactive)
  (if (< (point) filetree-startPosition)
      (progn
        (goto-char (point-min))
        (recenter-top-bottom "Top")
        (goto-char filetree-startPosition)))
  (move-end-of-line 1)
  (re-search-backward " ")
  (forward-char)
  (if (and (buffer-live-p filetree-info-buffer)
           (window-live-p filetree-info-window))
    (filetree-update-info-buffer (filetree-getName))))

(defun filetree-next-branch ()
  "Go to next item at the same or higher level in the tree.
In other words go to next branch of tree."
  (interactive)
  (filetree-goto-node)
  (let ((filetree-original-line (line-number-at-pos))
        (filetree-looking t)
        (filetree-current-col (current-column))
        (filetree-current-line (line-number-at-pos)))
    (while filetree-looking
      (filetree-next-line)
      (if (<= (current-column)
              filetree-current-col)
          (setq filetree-looking nil)
        (if (eq (line-number-at-pos) filetree-current-line)
            (progn
              (setq filetree-looking nil)
              (forward-line (- filetree-original-line
                               filetree-current-line))
              (filetree-goto-node))
          (setq filetree-current-line (line-number-at-pos)))))))

(defun filetree-prev-branch ()
  "Go to previous item at the same or higher level in the tree.
In other wrods go to prev branch of tree."
  (interactive)
  (filetree-goto-node)
  (let ((filetree-original-line (line-number-at-pos))
        (filetree-looking t)
        (filetree-current-col (current-column))
        (filetree-current-line (line-number-at-pos)))
    (while filetree-looking
      (filetree-prev-line)
      (if (<= (current-column)
              filetree-current-col)
          (setq filetree-looking nil)
        (if (eq (line-number-at-pos) filetree-current-line)
            (progn
              (setq filetree-looking nil)
              (forward-line (- filetree-original-line
                               filetree-current-line))
              (filetree-goto-node))
          (setq filetree-current-line (line-number-at-pos)))))))

(defun filetree-goto-name (name)
  "Helper function to go to item with name NAME."
  (let ((filetree-looking (stringp name))
        (filetree-end-of-buffer nil)
        (filetree-newName nil)
        (filetree-prevPoint -1))
    (goto-char (point-min))
    (while (and filetree-looking
                (not filetree-end-of-buffer))
      (setq filetree-newName (filetree-getName))
      (setq filetree-end-of-buffer
            (>= filetree-prevPoint (point)))
      (setq filetree-prevPoint (point))
      (if (string-equal filetree-newName name)
          (setq filetree-looking nil)
        (filetree-next-line)))
    (if filetree-end-of-buffer
        (goto-char (point-min)))
    (filetree-goto-node)))


(defun filetree-add-entry-to-tree (newEntry currentTree)
  "Add file NEWENTRY to CURRENTTREE."
  (interactive)
  (if newEntry
      (let ((treeHeadEntries (mapcar #'(lambda (x) (list (car x)
                                                         (nth 1 x)))
                                     currentTree))
            (newEntryHead (list (car newEntry) (nth 1 newEntry)))
            (matchingEntry nil))
        (setq matchingEntry (member newEntryHead treeHeadEntries))
        (if (/= (length matchingEntry) 0)
            (let ((entryNum (- (length currentTree)
                               (length matchingEntry))))
              (setcar (nthcdr entryNum currentTree)
                      (list (car newEntry)
                            (nth 1 newEntry)
                            (filetree-add-entry-to-tree (car (nth 2 newEntry))
                                                        (nth 2 (car (nthcdr entryNum currentTree))))
                            (nth 3 newEntry))))
          (setq currentTree (cons newEntry currentTree)))))
  currentTree)

(defun filetree-print-flat (fileList)
  "Print FILELIST in flat format."
  (let ((firstFile (car fileList))
        (remaining (cdr fileList)))
    (let ((filename (file-name-nondirectory firstFile))
          (directoryName (file-name-directory firstFile)))
      (insert-text-button  filename
                           'face (filetree-file-face firstFile)
                           'action (lambda (x) (find-file (button-get x 'name)))
                           'name firstFile)
      (insert (make-string (max 1 (- 30 (length filename))) ?\s))
      (insert-text-button (concat directoryName "\n")
                          'face 'default
                          'action (lambda (x) (find-file (button-get x 'name)))
                          'name firstFile))
    (if remaining
        (filetree-print-flat remaining))))

(defun filetree-toggle-flat-vs-tree ()
  "Toggle flat vs tree view."
  (interactive)
  (if filetree-showFlatList
      (setq filetree-showFlatList nil)
    (setq filetree-showFlatList t))
  (filetree-updateBuffer))

(defun filetree-toggle-combineDirNames ()
  "Toggle combine dir names."
  (interactive)
  (if filetree-combineDirNames
      (setq filetree-combineDirNames nil)
    (setq filetree-combineDirNames t))
  (filetree-updateBuffer))

(defun filetree-toggle-use-all-icons ()
  "Toggle use-all-icons."
  (interactive)
  (setq filetree-use-all-the-icons
        (if (require 'all-the-icons nil 'noerror)
            (not filetree-use-all-the-icons)
          nil))
  (filetree-updateBuffer))
  
(defun filetree-print-tree (dirTree depthList)
  "Print directory tree.
Print DIRTREE up to a depth of DEPTHLIST.
TODO: Break into smaller functions and clean-up."
  (interactive)
  (let ((myDepthList depthList)
        (myDirTree dirTree)
        (myDepthListCopy nil)
        (curDepth nil)
        (thisType nil)
        (thisName nil)
        (thisEntry nil))
    (if (not myDepthList)
        (setq myDepthList (list (- (length myDirTree) 1)))
      (setcdr (last myDepthList)
              (list (- (length myDirTree) 1))))
    (setq curDepth (- (length myDepthList) 1))
    (if (or (= filetree-maxDepth 0)
            (< curDepth filetree-maxDepth))
        (while (/= (length myDirTree) 0)
          (setq thisEntry (car myDirTree))
          (setq thisType (car thisEntry))
          (setq thisName (nth 1 thisEntry))
          (if (equal thisType "dir")
              (let ((myPrefix (apply 'concat (mapcar #'(lambda (x) (if (> x 0)
                                                                       ;; continue
                                                                     (concat " " filetree-symb-for-vertical-pipe "  ")
                                                                     "    "))
                                                     (butlast myDepthList 1))))
                    (dirContents (nth 2 thisEntry))
                    (filetree-dirString nil))
                (insert myPrefix)
                (if (> (length myDepthList) 1)
                    (if (> (car (last myDepthList)) 0)
                        ;; branch and continue
                        (insert " " filetree-symb-for-branch-and-cont
                                filetree-symb-for-horizontal-pipe
                                filetree-symb-for-horizontal-pipe " ")
                      ;; last branch
                      (insert " " filetree-symb-for-left-elbow
                              filetree-symb-for-horizontal-pipe
                              filetree-symb-for-horizontal-pipe " "))
                  ;; Tree root
                  (insert " " filetree-symb-for-box
                          filetree-symb-for-box
                          filetree-symb-for-root " "))
                (setq filetree-dirString thisName)
                (if (= (length dirContents) 1)
                    (setq thisType (car (car dirContents))))
                ;; combine dirname if no branching
                (if filetree-combineDirNames
                    (while (and (= (length dirContents) 1)
                                (equal thisType "dir")
                                (equal (car (car dirContents)) "dir"))
                      (setq thisEntry (car dirContents))
                      (setq thisType (car thisEntry))
                      (setq thisName (nth 1 thisEntry))
                      (if (equal thisType "dir")
                          (progn
                            (setq filetree-dirString (concat filetree-dirString
                                                             "/"  thisName))
                            (setq dirContents (nth 2 thisEntry))))))
                (if filetree-use-all-the-icons
                    (insert (concat (all-the-icons-icon-for-dir filetree-dirString) " ")))
                (insert-text-button filetree-dirString
                                    'face 'bold
                                    'action (lambda (x) (filetree-narrow
                                                         (button-get x 'subtree)))
                                    'name (concat (nth 3 thisEntry) "/")
                                    'subtree thisEntry)
                (insert "/\n")
                (setq myDepthListCopy (copy-tree myDepthList))
                (if (> (length dirContents) 0)
                    (filetree-print-tree dirContents myDepthListCopy)))
            ;; file
            (let ((myLink (nth 2 thisEntry))
                  (fileText (concat thisName))
                  (myPrefix (apply 'concat (mapcar #'(lambda (x) (if (= x 0)
                                                                     "    "
                                                                   ;; continue
                                                                   (concat " " filetree-symb-for-vertical-pipe "  ")))
                                                   (butlast myDepthList 1)))))
              (if (> (car (last myDepthList)) 0)
                  ;; file and continue
                  (setq myPrefix (concat myPrefix " "
                                         filetree-symb-for-branch-and-cont
                                         filetree-symb-for-horizontal-pipe
                                         filetree-symb-for-file-node " "))
                ;; last file
                (setq myPrefix (concat myPrefix " "
                                       filetree-symb-for-left-elbow
                                       filetree-symb-for-horizontal-pipe
                                       filetree-symb-for-file-node " ")))
              (insert myPrefix)
              (let ((button-face (filetree-file-face fileText)))
                (if filetree-use-all-the-icons
                    (insert (concat (all-the-icons-icon-for-file fileText) " ")))
                (insert-text-button fileText
                                    'face button-face
                                    'action (lambda (x) (find-file (button-get x 'name)))
                                    'name myLink))
              (insert "\n")))
      (let ((remainingEntries (nth curDepth myDepthList)))
        (setcar (nthcdr curDepth myDepthList)
                (- remainingEntries 1)))
      (setq myDirTree (cdr myDirTree))))))

(defun filetree-printHeader ()
  "Print header at top of window."
  (insert (concat filetree-symb-for-vertical-pipe " "
                  (propertize "# files: " 'font-lock-face 'bold)
                  (number-to-string (length filetree-currentFileList))
                  (propertize "\tMax depth: " 'font-lock-face 'bold)
                  (if (> filetree-maxDepth 0)
                      (number-to-string filetree-maxDepth)
                    "full")
                  "\t"
                  (if filetree-showFlatList
                      (propertize "Flat view" 'font-lock-face '(:foreground "blue"))
                    (propertize "Tree view" 'font-lock-face '(:foreground "DarkOliveGreen4")))
                  " \n" filetree-symb-for-left-elbow))
  (insert (make-string (+ (point) 1) ?\u2500))
  (insert filetree-symb-for-right-elbow "\n")
  (setq filetree-startPosition (point)))

(defun filetree-createSingleNodeTree (filename)
  "Create a tree for FILENAME."
  (let* ((filenameList (reverse (cdr (split-string
                                      filename "/"))))
         (singleNodeTree
          (if (equal (car filenameList) "")
              nil
            (list "file" (car filenameList) filename))))
    (setq filenameList (cdr filenameList))
    (while (/= (length filenameList) 0)
      (setq singleNodeTree (list "dir"
                                 (car filenameList)
                                 (if (not singleNodeTree)
                                     nil
                                   (list singleNodeTree))
                                 (concat "/" (mapconcat 'identity (reverse filenameList) "/"))))
      (setq filenameList (cdr filenameList)))
    singleNodeTree))
  
(defun filetree-createFileTree (filelist &optional curTree)
  "Create a tree for FILELIST and add it to CURTREE (or create new tree if not given)."
  (interactive)
  (let ((entry nil))
    (while (/= (length filelist) 0)
      (setq entry (car filelist))
      (setq curTree (filetree-add-entry-to-tree (filetree-createSingleNodeTree entry)
                                                curTree))
      (setq filelist (cdr filelist)))
    curTree))

(defun filetree-createFileList (filetree)
  "Create a list of files from FILETREE."
  (if (listp filetree)
      (progn
        (-flatten (mapcar #'(lambda (x) (if (eq (car x) "file")
                                            (nth 2 x)
                                          (filetree-createFileList (nth 2 x))))
                          filetree)))
    filetree))

(defun filetree-update-or-open-info-buffer()
  "Update info buffer based on current buffer.
Open info buffer if not already open."
  (interactive)
  (if (and (buffer-live-p filetree-info-buffer)
           (window-live-p filetree-info-window))
      (filetree-update-info-buffer)
  (filetree-toggle-info-buffer)))

(defun filetree-toggle-info-buffer (&optional switchToInfoFlag)
  "Toggle info buffer in side window.
If SWITCHTOINFOFLAG is true, then switch to the info window afterwards."
  (interactive)
  (let ((file-for-info-buffer (if (string-equal (buffer-name) filetree-buffer-name)
                                  (filetree-getName)
                                nil)))
    (if (and (buffer-live-p filetree-info-buffer)
             (window-live-p filetree-info-window))
        (progn
          (switch-to-buffer filetree-info-buffer)
          (save-buffer)
          (kill-buffer filetree-info-buffer)
          (setq filetree-info-buffer-state nil))
      (progn
        (setq filetree-info-buffer (find-file-noselect filetree-notes-file))
        (setq filetree-info-buffer-state t)
        (setq filetree-info-window
              (display-buffer-in-side-window filetree-info-buffer
                                             '((side . right))))
        (if file-for-info-buffer
              (filetree-update-info-buffer file-for-info-buffer)
          (filetree-update-info-buffer))
        (if switchToInfoFlag
            (select-window filetree-info-window))))))

(defun filetree-update-info-buffer (&optional current-file-name)
  "Update info buffer contents to reflect CURRENT-FILE-NAME.
If CURENT-FILE-NAME not given use 'buffer-file-name'.
If no entry in info buffer for this file, create new info buffer entry."
  ;; TODO: clean up
  (let ((filetree-create-new-entry (if current-file-name nil t)))
    (unless current-file-name (setq current-file-name (buffer-file-name)))
    (unless current-file-name (setq current-file-name "No File Note Entry"))
    (let ((current-window (selected-window)))
      (select-window filetree-info-window)
      (switch-to-buffer filetree-info-buffer)
      (if (get-buffer-window filetree-info-buffer)
          (let ((searchString (concat "* [[" current-file-name "]")))
            (find-file filetree-notes-file)
            (widen)
            (goto-char (point-min))
            (unless (search-forward searchString nil t)
              (if filetree-create-new-entry
                  (progn
                    (message "creating new entry")
                    (goto-char (point-max))
                    (let ((filename (car (last (split-string current-file-name "/") 1))))
                      (insert (concat "\n" "* [[" current-file-name "][" filename "]]\n"))))
                (unless (search-forward "* [[No File Note Entry]" nil t)
                  (progn
                    (message "creating No File Note Entry")
                    (goto-char (point-max))
                    (filetree-insert-noNoteEntry)))))
            (org-narrow-to-subtree)))
      (select-window current-window))))

(defun filetree-insert-noNoteEntry ()
  "Insert an entry in info file indicating not file note entry.
This is used when first starting an info note file."
  (insert (concat "\n* [[No File Note Entry]]\n"
                  (propertize (concat "\u250c"
                                      (make-string 9 ?\u2500)
                                      "\u2510\n\u2502 NO NOTE \u2502\n\u2514"
                                      (make-string 9 ?\u2500)
                                      "\u2518\n")
                              'font-lock-face '(:foreground "red")))))

(defun filetree-updateBuffer ()
  "Update the display buffer (following some change).
This function should be called after any change to 'filetree-currentFileList'."
  (interactive)
  (let ((text-scale-previous (buffer-local-value 'text-scale-mode-amount
                                                 (current-buffer))))
    (save-current-buffer
      (with-current-buffer (get-buffer-create filetree-buffer-name)
        (let ((filetree-currentName (filetree-getName)))
          (setq buffer-read-only nil)
          (erase-buffer)
          (setq filetree-currentFileList (-distinct (-non-nil
                                                     filetree-currentFileList)))
          (setq filetree-fileListStack (cons (copy-sequence filetree-currentFileList)
                                             filetree-fileListStack))
          (filetree-printHeader)
          (if filetree-showFlatList
              (filetree-print-flat filetree-currentFileList)
            (filetree-print-tree (filetree-createFileTree
                                  (reverse filetree-currentFileList)) ()))
          (setq filetree-overallDepth
                (if (null filetree-currentFileList) 0
                  (apply 'max (mapcar #'(lambda (x) (length (split-string x "/")))
                                      filetree-currentFileList))))
          ;; (filetree-update-info-buffer filetree-buffer-name)
          (switch-to-buffer filetree-buffer-name)
          (filetree-goto-name filetree-currentName)
          (setq buffer-read-only t)
          (filetree)
          (text-scale-increase text-scale-previous))))))

(defun filetree-pop-fileListStack ()
  "Pop last state from file list stack."
  (interactive)
  (if (> (length filetree-fileListStack) 1)
      (setq filetree-fileListStack (cdr filetree-fileListStack)))

  (setq filetree-currentFileList (car filetree-fileListStack))
  (if (> (length filetree-fileListStack) 1)
      (setq filetree-fileListStack (cdr filetree-fileListStack)))
  (filetree-updateBuffer))
  

(defun filetree-narrow (subtree)
  "Narrow file tree to SUBTREE."
  (setq filetree-currentFileList (filetree-createFileList (list subtree)))
  (filetree-updateBuffer))

(defun filetree-file-face (filename)
  "Return face to use for FILENAME.
Info determined from 'filetree-filetype-list' and 'filetree-default-file-face'."
  (let ((file-face filetree-default-file-face)
        (my-file-face-list (mapcar #'(lambda (x) (cdr (cdr x)))
                                   filetree-filetype-list))
        (elem nil)
        (filetree-regex nil))
    (while (/= (length my-file-face-list) 0)
      (setq elem (car my-file-face-list))
      (setq filetree-regex (car elem))
      (if (> (length filetree-regex) 0)
          (if (string-match filetree-regex filename)
              (setq file-face (car (cdr elem)))))
      (setq my-file-face-list (cdr my-file-face-list)))
    file-face))

(defun filetree-grep ()
  "Run grep on files in 'currentFileList'.
Takes input from user for grep pattern."
  (interactive)
  (if (version< emacs-version "27.1")
      (message "filetree-grep not supported for emacs versions before 27.1")
    (let* ((myFileTree-regex (read-string "Type search string:"))
           (xrefs nil)
           (fetcher
            (lambda ()
              (setq xrefs (xref-matches-in-files myFileTree-regex
                                                 (-filter 'file-exists-p filetree-currentFileList)))
              (unless xrefs
                (user-error "No matches for: %s" myFileTree-regex))
              xrefs)))
      (xref--show-xrefs fetcher nil))))

(defun filetree-helm-filter ()
  "Use helm-based filtering on filetree."
  (interactive)
  (setq filetree-fileListStack-save (copy-sequence filetree-fileListStack))
  (let ((current-node (filetree-getName)))
    (add-hook 'helm-after-update-hook
              #'filetree-helm-hook)
    (helm :sources '(filetree-helm-source))
    (filetree-goto-name current-node)))

(defun filetree-helm-hook ()
  "Helm hook for filetree."
  (interactive)
  (setq filetree-currentFileList (car (helm--collect-matches
                                       (list (helm-get-current-source)))))
  (filetree-updateBuffer))

(defun filetree-select-file-list ()
  "Select file list from saved file lists."
  (interactive)
  (let ((fileList (helm :sources (helm-build-sync-source "File Lists"
                                   :candidates (mapcar #'(lambda (x)
                                                           (car x))
                                                       filetree-saved-lists)
                                   :action (lambda (candidate)
                                             (let ((file-list
                                                    (car (cdr (assoc candidate
                                                                     filetree-saved-lists)))))
                                               (if (functionp file-list)
                                                   (setq file-list (funcall file-list)))
                                               file-list))
                                                                   
                                   :fuzzy-match t)
                        :buffer "*filetree-helm-buffer*")))
    (if fileList
        (setq filetree-currentFileList fileList))
    (filetree-updateBuffer)))

(defun filetree-update-saved-lists-file ()
  "Save current `filetree-saved-lists' to file."
  (save-current-buffer
    (with-current-buffer (get-buffer-create "*filetree-temp*")
      (erase-buffer)
      (insert "(setq filetree-saved-lists '")
      (insert (format "%S" filetree-saved-lists))
      (insert ")")
      (write-file filetree-saved-lists-file))))
  
(defun filetree-save-list ()
  "Save current file list."
  (interactive)
  (let ((list-name (helm :sources (helm-build-sync-source "File Lists"
                                    :candidates (cons "*New Entry*"
                                                      (mapcar #'(lambda (x)
                                                                  (car x))
                                                              filetree-saved-lists))
                                   :fuzzy-match t)
                         :buffer "*filetree-helm-buffer*")))
    (if (string= list-name "*New Entry*")
        (setq list-name (read-string "Enter new list name:")))
    ;; first delete previous list-name entry (if any)
    (setq filetree-saved-lists (delete nil (mapcar #'(lambda (x)
                                                       (if (string-match
                                                            list-name
                                                            (car x))
                                                           nil x))
                                                   filetree-saved-lists)))
    ;; add new entry
    (setq filetree-saved-lists (cons (cons list-name (list filetree-currentFileList))
                                     filetree-saved-lists))
    (filetree-update-saved-lists-file)))
      
(defun filetree-delete-list()
  "Delete a file list from the `filetree-saved-list' and save to file."
  (interactive)
  (let ((list-name (helm :sources (helm-build-sync-source "File Lists"
                                    :candidates (mapcar #'(lambda (x)
                                                            (car x))
                                                        filetree-saved-lists)
                                   :fuzzy-match t)
                         :buffer "*filetree-helm-buffer*")))
    (setq filetree-saved-lists (delete nil (mapcar #'(lambda (x)
                                                       (if (string-match
                                                            list-name
                                                            (car x))
                                                           nil x))
                                                   filetree-saved-lists)))
    (filetree-update-saved-lists-file)))
    
(defun filetree-showFiles (fileList)
  "Load FILELIST into current file list and show in tree mode."
  (setq filetree-currentFileList fileList)
  (setq filetree-fileListStack (list filetree-currentFileList))
  (filetree-updateBuffer))

(defun filetree-showRecentfFiles ()
  "Load recentf list into current file list and show in tree mode."
  (interactive)
  (if (not recentf-mode)
      (recentf-mode))
  (filetree-showFiles recentf-list))

(defun filetree-showCurDir ()
  "Load files in current directory into current file list and show in tree mode."
  (interactive)
  (setq filetree-currentFileList nil)
  (setq filetree-fileListStack (list filetree-currentFileList))
  (filetree-expandDir (file-name-directory (buffer-file-name)) 0))

(defun filetree-showCurDirRecursively ()
  "Load files in current directory (recursively) into current file list and show in tree mode."
  (interactive)
  (setq filetree-currentFileList nil)
  (setq filetree-fileListStack (list filetree-currentFileList))
  (filetree-expandDirRecursively (file-name-directory (buffer-file-name)) 0))

(defun filetree-showCurBuffers ()
  "Load file buffers in buffer list into current file list and show in tree mode."
  (interactive)
  (let ((myBufferList (buffer-list))
        (myBuffer nil)
        (myFileList ()))
    (while myBufferList
      (setq myBuffer (car myBufferList))
      (setq myBufferList (cdr myBufferList))
      (if (buffer-file-name myBuffer)
          (setq myFileList (cons (buffer-file-name myBuffer)
                                 myFileList))))
    (setq filetree-currentFileList myFileList)
    (filetree-updateBuffer)))

(defun filetree-findFilesWithNotes ()
  "Return list of files with notes."
  (find-file filetree-notes-file)
  (goto-char (point-min))
  (widen)
  (let ((regexp "^\\* \\[\\[\\(.*\\)\\]\\[")
        (filelist nil)
        (myMatch nil))
    (while (re-search-forward regexp nil t)
      (setq myMatch (match-string-no-properties 1))
      (setq filelist (cons myMatch filelist)))
    filelist))
  
(defun filetree-showFilesWithNotes ()
  "Load files with entries in notes file."
  (interactive)
  (filetree-showFiles (filetree-findFilesWithNotes)))

(define-derived-mode filetree nil "Text"
  "A mode to view and perform operations on files via a tree view"
  (make-local-variable 'filetree-list))

(provide 'filetree)
;;; filetree.el ends here
