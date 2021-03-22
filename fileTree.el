;;; fileTree.el --- file tree view/manipulatation package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ketan Patel
;;
;; Author: Ketan Patel <knpatel401@gmail.com>
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
(defvar text-scale-mode-amount 0)

(defgroup fileTree nil
  "Tree view of file list and file notes."
  :group 'matching
  :prefix "fileTree-")

(defvar fileTree-version "1.0")

(defconst fileTree-buffer-name "*FileTree*")

(defcustom fileTree-notes-file "~/.emacs.d/fileTree-notes.org"
  "File used for file specific notes."
  :type 'file)
(defcustom fileTree-saved-lists-file "~/.emacs.d/fileTree-saved-lists.el"
  "File used for saved file lists."
  :type 'file)
(defcustom fileTree-default-file-face 'default
  "Default face to use for files.
This is used if the file doesn't match any regex in `fileTree-filetype-list'."
  :type 'face)
(defcustom fileTree-use-all-the-icons nil
  "Set to t to use file and directory icons.
This can also be toggled using `fileTree-toggle-use-all-icons'."
  :type 'boolean)
(defcustom fileTree-info-window nil
  "Set to t to show info in side window.
This can also be toggled using `fileTree-toggle-info-buffer'."
  :type 'boolean)
(defcustom fileTree-excludeList
  '("~$" "#$" ".git\/" ".gitignore$" "\/\.\/$" "\/\.\.\/$" ".DS_Store$")
  "List of regex for files to exclude from file list."
  :type '(repeat regexp))

(defgroup fileTree-symb-for nil
  "Symbols used for drawing tree in fileTree package."
  :group 'fileTree
  :prefix "fileTree-symb-for-")

(defcustom fileTree-symb-for-root "\u25ba"
  "Symbol for end of mark indicating root dir."
  :type 'character)
(defcustom fileTree-symb-for-box "\u25a0"
  "Box symbol used in mark for root dir."
  :type 'character)
(defcustom fileTree-symb-for-vertical-pipe "\u2502"
  "Symbol to indicate continuing branch."
  :type 'character)
(defcustom fileTree-symb-for-horizontal-pipe "\u2500"
  "Symbol for branch for node on current line."
  :type 'character)
(defcustom fileTree-symb-for-left-elbow "\u2514"
  "Symbol for last node on branch."
  :type 'character)
(defcustom fileTree-symb-for-right-elbow "\u2518"
  "Symbol for bottom right hand corner."
  :type 'character)
(defcustom fileTree-symb-for-branch-and-cont "\u251c"
  "Symbol indicating continuing branch which also includes node on current line."
  :type 'character)
(defcustom fileTree-symb-for-file-node "\u25cf"
  "Symbol for file node."
  :type 'character)

(defvar fileTree-info-buffer nil)
(defvar fileTree-info-buffer-state nil)
(defvar fileTree-saved-lists '(("recentf" (lambda ()
                                            recentf-list))))
(if (file-exists-p fileTree-saved-lists-file)
    (with-temp-buffer
      (insert-file-contents fileTree-saved-lists-file)
      (eval-buffer)))
  
(defvar fileTree-startPosition 0)
(defvar fileTree-maxDepth 0)
(defvar fileTree-overallDepth nil)
(defvar fileTree-currentFileList nil)
(defvar fileTree-fileListStack nil)
(defvar fileTree-fileListStack-save nil)
(defvar fileTree-showFlatList nil)
(defvar fileTree-combineDirNames t)
(defvar fileTree-helm-source
  '((name . "fileTree")
    (candidates . fileTree-currentFileList)
    (cleanup . (lambda ()
                 (remove-hook 'helm-after-update-hook
                              #'fileTree-helm-hook)
                 (setq fileTree-fileListStack fileTree-fileListStack-save)
                 (fileTree-updateBuffer)))
    (buffer . ("*helm-fileTree-buffer*"))
    (prompt . ("selection:"))))
  
(defvar fileTree-filetype-list nil
  "List of file types with filter shortcuts, regex for filetype, and face.
This is populated using `fileTree-add-filetype', for example see
`fileTree-configure-default-filetypes'")

(defvar fileTree-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "?" '(lambda () (interactive) (message "%s %s" (fileTree-getName)
    ;;                                                          (if (button-at (point))
    ;;                                                              (button-get (button-at (point)) 'subtree)
    ;;                                                            nil))))
    (define-key map "j" 'fileTree-next-line)
    (define-key map "k" 'fileTree-prev-line)
    (define-key map (kbd "<down>") 'fileTree-next-line)
    (define-key map (kbd "<up>") 'fileTree-prev-line)
    (define-key map (kbd "C-j") 'fileTree-next-line)
    (define-key map (kbd "C-k") 'fileTree-prev-line)
    (define-key map (kbd "SPC") 'fileTree-next-branch)
    (define-key map (kbd "TAB") 'fileTree-prev-branch)
    (define-key map "q" 'recentf-cancel-dialog)
    (define-key map "0" 'fileTree-set-maxDepth)
    (define-key map "1" 'fileTree-set-maxDepth1)
    (define-key map "2" 'fileTree-set-maxDepth2)
    (define-key map "3" 'fileTree-set-maxDepth3)
    (define-key map "4" 'fileTree-set-maxDepth4)
    (define-key map "5" 'fileTree-set-maxDepth5)
    (define-key map "6" 'fileTree-set-maxDepth6)
    (define-key map "7" 'fileTree-set-maxDepth7)
    (define-key map "8" 'fileTree-set-maxDepth8)
    (define-key map "9" 'fileTree-set-maxDepth9)
    (define-key map "r" 'fileTree-showRecentfFiles)
    (define-key map "f" 'fileTree-filter)
    (define-key map "/" 'fileTree-toggle-combineDirNames)
    (define-key map "b" 'fileTree-pop-fileListStack)
    (define-key map "g" 'fileTree-grep)
    (define-key map "d" 'fileTree-run-dired)
    (define-key map "e" 'fileTree-expandDir)
    (define-key map "E" 'fileTree-expandDirRecursively)
    (define-key map "x" 'fileTree-remove-item)
    (define-key map "L" 'fileTree-select-file-list)
    (define-key map "S" 'fileTree-save-list)
    (define-key map "D" 'fileTree-delete-list)
    (define-key map "-" 'fileTree-reduceListBy10)
    (define-key map "." 'fileTree-toggle-flat-vs-tree)
    (define-key map "i" 'fileTree-toggle-info-buffer)
    (define-key map "I" (lambda ()
                          "Toggle fileTree-info-buffer and switch to it if active"
                          (interactive)
                          (fileTree-toggle-info-buffer t)))
    (define-key map "s" 'fileTree-helm-filter)
    (define-key map ";" 'fileTree-toggle-use-all-icons)
    map)
  "Keymap for fileTree.")

(defun fileTree-add-filetype (name shortcut regex face)
  "Add a filetype to `fileTree-filetype-list' for special handling.
- NAME is the name of the filetype (e.g., \"elisp\")
- SHORTCUT is the shortcut char literal to use for this filetype when filtering
- REGEX is the regular expression to use for detecting the filetype
- FACE is the face to use for this filetype"
  (print shortcut)
  (push (list shortcut name regex face) fileTree-filetype-list))

(defun fileTree-configure-default-filetypes ()
  "Define default `fileTree-filetype-list'.
This defines filetype faces, filetype regex, and filter shortcuts.
This function is given as an example.  The user can generate their own
custom function with calls to `fileTree-add-filetype'"
  (interactive)
  (setq fileTree-filetype-list nil)
  (fileTree-add-filetype "No Filter" 0   ""        ())
  (fileTree-add-filetype "Python"    ?p  "\.py$"   '(:foreground "steel blue"))
  (fileTree-add-filetype "Org-mode"  ?o  "\.org$"  '(:foreground "DarkOliveGreen4"))
  (fileTree-add-filetype "elisp"     ?e  "\\(?:\\.e\\(?:l\\|macs\\)\\)"  '(:foreground "purple"))
  (fileTree-add-filetype "C"         ?c  "\\(?:\\.[ch]$\\|\\.cpp\\)"     '(:foreground "navyblue"))
  (fileTree-add-filetype "PDF"       ?d  "\.pdf$"  '(:foreground "maroon"))
  (fileTree-add-filetype "Matlab"    ?m  "\.m$"    '(:foreground "orange"))
  (fileTree-add-filetype "Text"      ?t  "\.txt$"  '(:foreground "gray50")))

(fileTree-configure-default-filetypes)

(defun fileTree-filter ()
  "Interactive function to filter 'fileTree-currentFileList'.
Uses regular expression in 'fileTree-filetype-list' or by expression entered by user."
  (interactive)
  (let ((myFileTree-mode-filterList (mapcar #'(lambda (x)
                                                (seq-subseq x 0 3))
                                            fileTree-filetype-list))
        (myFileTree-regex nil)
        (fileTree-elem nil)
        (fileTree-charInput (read-char)))
    (while (/= (length myFileTree-mode-filterList) 0)
      (setq fileTree-elem (car myFileTree-mode-filterList))
      (if (eq fileTree-charInput (car fileTree-elem))
          (setq myFileTree-regex (nth 2 fileTree-elem)))
      (setq myFileTree-mode-filterList (cdr myFileTree-mode-filterList)))
    (if (not myFileTree-regex)
        (setq myFileTree-regex (read-string "Type a string:")))
    (setq fileTree-currentFileList (delete nil (mapcar #'(lambda (x)
                                                           (if (string-match
                                                                myFileTree-regex
                                                                (file-name-nondirectory x))
                                                               x nil))
                                                       fileTree-currentFileList)))
    (fileTree-updateBuffer)))

(defun fileTree-remove-item (&optional file_or_dir)
  "Remove the file or subdir FILE_OR_DIR from the `fileTree-currentFileList'.
If file_or_dir not specified, use file or dir at point."
  (interactive)
  (let ((file_or_dir (or file_or_dir (fileTree-getName))))
    (setq fileTree-currentFileList (delete
                                    nil
                                    (if (string= "/"
                                                 (substring file_or_dir -1))
                                        ;; removing subdirectory
                                        (mapcar #'(lambda (x)
                                                    (if (string-match
                                                         file_or_dir
                                                         x)
                                                        nil x))
                                                fileTree-currentFileList)
                                      ;; removing file
                                      (mapcar #'(lambda (x)
                                                  (if (string=
                                                       file_or_dir
                                                       x)
                                                      nil x))
                                              fileTree-currentFileList)))))
  (fileTree-updateBuffer))

(defun fileTree-getName ()
  "Get name of file/dir on line at current point."
  (interactive)
  (if (button-at (point))
      (button-get (button-at (point)) 'name)
    nil))

(defun fileTree-expandDir (&optional dir filter)
  "Add files in DIR to 'fileTree-currentFileList'.
If DIR is not specified, use dir at point.
If FILTER is not specified, will 'read-char' from user.
The corresponding regular expression in 'fileTree-filetype-list' will
be used to filter which files are included.  If FILTER does not
correspond to an entry in 'fileTree-filetype-list' the user is
prompted for a string/regular expression to filter with."
  (interactive)
  (let ((dir (or dir (fileTree-getName)))
        (fileTree-charInput (or filter (read-char)))
        (myFileTree-mode-filterList (mapcar #'(lambda (x)
                                                (seq-subseq x 0 3))
                                            fileTree-filetype-list))
        (fileTree-newFiles nil)
        (myFileTree-regex nil)
        (fileTree-elem nil))
    (while (/= (length myFileTree-mode-filterList) 0)
      (setq fileTree-elem (car myFileTree-mode-filterList))
      (if (eq fileTree-charInput (car fileTree-elem))
          (setq myFileTree-regex (nth 2 fileTree-elem)))
      (setq myFileTree-mode-filterList (cdr myFileTree-mode-filterList)))
    (if (not myFileTree-regex)
        (setq myFileTree-regex (read-string "Type a string:")))
    (setq fileTree-newFiles (delete nil (mapcar #'(lambda (x)
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
    (dolist (entry fileTree-excludeList)
                   (setq fileTree-newFiles (delete nil (mapcar #'(lambda (x)
                                                                   (if (string-match
                                                                        entry
                                                                        x)
                                                                       nil
                                                                     x))
                                                               fileTree-newFiles))))
    (setq fileTree-currentFileList
          (-distinct (-non-nil
                      (nconc fileTree-currentFileList
                             fileTree-newFiles)))))
  (fileTree-updateBuffer))

(defun fileTree-expandDirRecursively (&optional dir filter)
  "Recursively add files in DIR to 'fileTree-currentFileList'.
If DIR is not specified, use dir at point.
If FILTER is not specified, will 'read-char' from user.  The corresponding
regular expression in 'fileTree-filetype-list' will be used to filter
which files are included.  If FILTER does not correspond to an entry in
'fileTree-filetype-list' the user is prompted for a string/regular
expression to filter with."
  (interactive)
  (let ((dir (or dir (fileTree-getName)))
        (fileTree-charInput (or filter (read-char)))
        (myFileTree-mode-filterList (mapcar #'(lambda (x)
                                                (seq-subseq x 0 3))
                                            fileTree-filetype-list))
        (myFileTree-regex nil)
        (fileTree-elem nil)
        (fileTree-newFiles nil))
    (while (/= (length myFileTree-mode-filterList) 0)
      (setq fileTree-elem (car myFileTree-mode-filterList))
      (if (eq fileTree-charInput (car fileTree-elem))
          (setq myFileTree-regex (nth 2 fileTree-elem)))
      (setq myFileTree-mode-filterList (cdr myFileTree-mode-filterList)))
    (if (not myFileTree-regex)
        (setq myFileTree-regex (read-string "Type a string:")))
    (setq fileTree-newFiles (directory-files-recursively dir myFileTree-regex))
    (dolist (entry fileTree-excludeList)
      (setq fileTree-newFiles (delete nil (mapcar #'(lambda (x)
                                                      (if (string-match entry x)
                                                          nil
                                                        x))
                                                  fileTree-newFiles))))
    (setq fileTree-currentFileList (-distinct (-non-nil
                                               (nconc fileTree-currentFileList
                                                      fileTree-newFiles)))))
  (fileTree-updateBuffer))

(defun fileTree-run-dired ()
  "Run dired on directory at point."
  (dired (fileTree-getName)))

(defun fileTree-reduceListBy10 ()
  "Drop last 10 entries in `fileTree-currentFileList'."
  (interactive)
  (if (>= (length fileTree-currentFileList) 20)
      (setq fileTree-currentFileList (butlast fileTree-currentFileList 10))
    (if (>= (length fileTree-currentFileList) 10)
        (setq fileTree-currentFileList
              (butlast fileTree-currentFileList
                       (- (length fileTree-currentFileList) 10)))))
  (message "file list length: %d" (length fileTree-currentFileList))
  (fileTree-updateBuffer))

(defun fileTree-cycle-maxDepth ()
  "Increase depth of file tree by 1 level cycle back to 0 when max depth reached."
  (interactive)
  (setq fileTree-maxDepth (% (+ fileTree-maxDepth 1)
                             fileTree-overallDepth))
  (fileTree-updateBuffer))
  
(defun fileTree-set-maxDepth (&optional maxDepth)
  "Set depth of displayed file tree to MAXDEPTH.
If maxdepth not specified, show full tree."
  (interactive)
  (setq fileTree-maxDepth (or maxDepth 0))
  (fileTree-updateBuffer))

(defun fileTree-set-maxDepth1 ()
  "Set depth of displayed file to 1."
  (interactive)
  (fileTree-set-maxDepth 1))

(defun fileTree-set-maxDepth2 ()
  "Set depth of displayed file to 2."
  (interactive)
  (fileTree-set-maxDepth 2))

(defun fileTree-set-maxDepth3 ()
  "Set depth of displayed file to 3."
  (interactive)
  (fileTree-set-maxDepth 3))

(defun fileTree-set-maxDepth4 ()
  "Set depth of displayed file to 4."
  (interactive)
  (fileTree-set-maxDepth 4))

(defun fileTree-set-maxDepth5 ()
  "Set depth of displayed file to 5."
  (interactive)
  (fileTree-set-maxDepth 5))

(defun fileTree-set-maxDepth6 ()
  "Set depth of displayed file to 6."
  (interactive)
  (fileTree-set-maxDepth 6))

(defun fileTree-set-maxDepth7 ()
  "Set depth of displayed file to 7."
  (interactive)
  (fileTree-set-maxDepth 7))

(defun fileTree-set-maxDepth8 ()
  "Set depth of displayed file to 8."
  (interactive)
  (fileTree-set-maxDepth 8))

(defun fileTree-set-maxDepth9 ()
  "Set depth of displayed file to 9."
  (interactive)
  (fileTree-set-maxDepth 9))

(defun fileTree-next-line ()
  "Go to file/dir on next line."
  (interactive)
  (move-end-of-line 2)
  (re-search-backward " ")
  (fileTree-goto-node))

(defun fileTree-prev-line ()
  "Go to file/dir on previous line."
  (interactive)
  (forward-line -1)
  (fileTree-goto-node))

(defun fileTree-goto-node ()
  "Move point to item on current line."
  (interactive)
  (if (< (point) fileTree-startPosition)
      (progn
        (goto-char (point-min))
        (recenter-top-bottom "Top")
        (goto-char fileTree-startPosition)))
  (move-end-of-line 1)
  (re-search-backward " ")
  (forward-char)
  (if (and (buffer-live-p fileTree-info-buffer)
           (window-live-p fileTree-info-window))
    (fileTree-update-info-buffer (fileTree-getName))))

(defun fileTree-next-branch ()
  "Go to next item at the same or higher level in the tree.
In other words go to next branch of tree."
  (interactive)
  (fileTree-goto-node)
  (let ((fileTree-original-line (line-number-at-pos))
        (fileTree-looking t)
        (fileTree-current-col (current-column))
        (fileTree-current-line (line-number-at-pos)))
    (while fileTree-looking
      (fileTree-next-line)
      (if (<= (current-column)
              fileTree-current-col)
          (setq fileTree-looking nil)
        (if (eq (line-number-at-pos) fileTree-current-line)
            (progn
              (setq fileTree-looking nil)
              (forward-line (- fileTree-original-line
                               fileTree-current-line))
              (fileTree-goto-node))
          (setq fileTree-current-line (line-number-at-pos)))))))

(defun fileTree-prev-branch ()
  "Go to previous item at the same or higher level in the tree.
In other wrods go to prev branch of tree."
  (interactive)
  (fileTree-goto-node)
  (let ((fileTree-original-line (line-number-at-pos))
        (fileTree-looking t)
        (fileTree-current-col (current-column))
        (fileTree-current-line (line-number-at-pos)))
    (while fileTree-looking
      (fileTree-prev-line)
      (if (<= (current-column)
              fileTree-current-col)
          (setq fileTree-looking nil)
        (if (eq (line-number-at-pos) fileTree-current-line)
            (progn
              (setq fileTree-looking nil)
              (forward-line (- fileTree-original-line
                               fileTree-current-line))
              (fileTree-goto-node))
          (setq fileTree-current-line (line-number-at-pos)))))))

(defun fileTree-goto-name (name)
  "Helper function to go to item with name NAME."
  (let ((fileTree-looking (stringp name))
        (fileTree-end-of-buffer nil)
        (fileTree-newName nil)
        (fileTree-prevPoint -1))
    (goto-char (point-min))
    (while (and fileTree-looking
                (not fileTree-end-of-buffer))
      (setq fileTree-newName (fileTree-getName))
      (setq fileTree-end-of-buffer
            (>= fileTree-prevPoint (point)))
      (setq fileTree-prevPoint (point))
      (if (string-equal fileTree-newName name)
          (setq fileTree-looking nil)
        (fileTree-next-line)))
    (if fileTree-end-of-buffer
        (goto-char (point-min)))
    (fileTree-goto-node)))


(defun fileTree-add-entry-to-tree (newEntry currentTree)
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
                            (fileTree-add-entry-to-tree (car (nth 2 newEntry))
                                                        (nth 2 (car (nthcdr entryNum currentTree))))
                            (nth 3 newEntry))))
          (setq currentTree (cons newEntry currentTree)))))
  currentTree)

(defun fileTree-print-flat (fileList)
  "Print FILELIST in flat format."
  (let ((firstFile (car fileList))
        (remaining (cdr fileList)))
    (let ((filename (file-name-nondirectory firstFile))
          (directoryName (file-name-directory firstFile)))
      (insert-text-button  filename
                           'face (fileTree-file-face firstFile)
                           'action (lambda (x) (find-file (button-get x 'name)))
                           'name firstFile)
      (insert (make-string (max 1 (- 30 (length filename))) ?\s))
      (insert-text-button (concat directoryName "\n")
                          'face 'default
                          'action (lambda (x) (find-file (button-get x 'name)))
                          'name firstFile))
    (if remaining
        (fileTree-print-flat remaining))))

(defun fileTree-toggle-flat-vs-tree ()
  "Toggle flat vs tree view."
  (interactive)
  (if fileTree-showFlatList
      (setq fileTree-showFlatList nil)
    (setq fileTree-showFlatList t))
  (fileTree-updateBuffer))

(defun fileTree-toggle-combineDirNames ()
  "Toggle combine dir names."
  (interactive)
  (if fileTree-combineDirNames
      (setq fileTree-combineDirNames nil)
    (setq fileTree-combineDirNames t))
  (fileTree-updateBuffer))

(defun fileTree-toggle-use-all-icons ()
  "Toggle use-all-icons."
  (interactive)
  (setq fileTree-use-all-the-icons
        (if (require 'all-the-icons nil 'noerror)
            (not fileTree-use-all-the-icons)
          nil))
  (fileTree-updateBuffer))
  
(defun fileTree-print-tree (dirTree depthList)
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
    (if (or (= fileTree-maxDepth 0)
            (< curDepth fileTree-maxDepth))
        (while (/= (length myDirTree) 0)
          (setq thisEntry (car myDirTree))
          (setq thisType (car thisEntry))
          (setq thisName (nth 1 thisEntry))
          (if (equal thisType "dir")
              (let ((myPrefix (apply 'concat (mapcar #'(lambda (x) (if (> x 0)
                                                                       ;; continue
                                                                     (concat " " fileTree-symb-for-vertical-pipe "  ")
                                                                     "    "))
                                                     (butlast myDepthList 1))))
                    (dirContents (nth 2 thisEntry))
                    (fileTree-dirString nil))
                (insert myPrefix)
                (if (> (length myDepthList) 1)
                    (if (> (car (last myDepthList)) 0)
                        ;; branch and continue
                        (insert " " fileTree-symb-for-branch-and-cont
                                fileTree-symb-for-horizontal-pipe
                                fileTree-symb-for-horizontal-pipe " ")
                      ;; last branch
                      (insert " " fileTree-symb-for-left-elbow
                              fileTree-symb-for-horizontal-pipe
                              fileTree-symb-for-horizontal-pipe " "))
                  ;; Tree root
                  (insert " " fileTree-symb-for-box
                          fileTree-symb-for-box
                          fileTree-symb-for-root " "))
                (setq fileTree-dirString thisName)
                (if (= (length dirContents) 1)
                    (setq thisType (car (car dirContents))))
                ;; combine dirname if no branching
                (if fileTree-combineDirNames
                    (while (and (= (length dirContents) 1)
                                (equal thisType "dir")
                                (equal (car (car dirContents)) "dir"))
                      (setq thisEntry (car dirContents))
                      (setq thisType (car thisEntry))
                      (setq thisName (nth 1 thisEntry))
                      (if (equal thisType "dir")
                          (progn
                            (setq fileTree-dirString (concat fileTree-dirString
                                                             "/"  thisName))
                            (setq dirContents (nth 2 thisEntry))))))
                (if fileTree-use-all-the-icons
                    (insert (concat (all-the-icons-icon-for-dir fileTree-dirString) " ")))
                (insert-text-button fileTree-dirString
                                    'face 'bold
                                    'action (lambda (x) (fileTree-narrow
                                                         (button-get x 'subtree)))
                                    'name (concat (nth 3 thisEntry) "/")
                                    'subtree thisEntry)
                (insert "/\n")
                (setq myDepthListCopy (copy-tree myDepthList))
                (if (> (length dirContents) 0)
                    (fileTree-print-tree dirContents myDepthListCopy)))
            ;; file
            (let ((myLink (nth 2 thisEntry))
                  (fileText (concat thisName))
                  (myPrefix (apply 'concat (mapcar #'(lambda (x) (if (= x 0)
                                                                     "    "
                                                                   ;; continue
                                                                   (concat " " fileTree-symb-for-vertical-pipe "  ")))
                                                   (butlast myDepthList 1)))))
              (if (> (car (last myDepthList)) 0)
                  ;; file and continue
                  (setq myPrefix (concat myPrefix " "
                                         fileTree-symb-for-branch-and-cont
                                         fileTree-symb-for-horizontal-pipe
                                         fileTree-symb-for-file-node " "))
                ;; last file
                (setq myPrefix (concat myPrefix " "
                                       fileTree-symb-for-left-elbow
                                       fileTree-symb-for-horizontal-pipe
                                       fileTree-symb-for-file-node " ")))
              (insert myPrefix)
              (let ((button-face (fileTree-file-face fileText)))
                (if fileTree-use-all-the-icons
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

(defun fileTree-printHeader ()
  "Print header at top of window."
  (insert (concat fileTree-symb-for-vertical-pipe " "
                  (propertize "# files: " 'font-lock-face 'bold)
                  (number-to-string (length fileTree-currentFileList))
                  (propertize "\tMax depth: " 'font-lock-face 'bold)
                  (if (> fileTree-maxDepth 0)
                      (number-to-string fileTree-maxDepth)
                    "full")
                  "\t"
                  (if fileTree-showFlatList
                      (propertize "Flat view" 'font-lock-face '(:foreground "blue"))
                    (propertize "Tree view" 'font-lock-face '(:foreground "DarkOliveGreen4")))
                  " \n" fileTree-symb-for-left-elbow))
  (insert (make-string (+ (point) 1) ?\u2500))
  (insert fileTree-symb-for-right-elbow "\n")
  (setq fileTree-startPosition (point)))

(defun fileTree-createSingleNodeTree (filename)
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
  
(defun fileTree-createFileTree (filelist &optional curTree)
  "Create a tree for FILELIST and add it to CURTREE (or create new tree if not given)."
  (interactive)
  (let ((entry nil))
    (while (/= (length filelist) 0)
      (setq entry (car filelist))
      (setq curTree (fileTree-add-entry-to-tree (fileTree-createSingleNodeTree entry)
                                                curTree))
      (setq filelist (cdr filelist)))
    curTree))

(defun fileTree-createFileList (fileTree)
  "Create a list of files from FILETREE."
  (if (listp fileTree)
      (progn
        (-flatten (mapcar #'(lambda (x) (if (eq (car x) "file")
                                            (nth 2 x)
                                          (fileTree-createFileList (nth 2 x))))
                          fileTree)))
    fileTree))

(defun fileTree-update-or-open-info-buffer()
  "Update info buffer based on current buffer.
Open info buffer if not already open."
  (interactive)
  (if (and (buffer-live-p fileTree-info-buffer)
           (window-live-p fileTree-info-window))
      (fileTree-update-info-buffer)
  (fileTree-toggle-info-buffer)))

(defun fileTree-toggle-info-buffer (&optional switchToInfoFlag)
  "Toggle info buffer in side window.
If SWITCHTOINFOFLAG is true, then switch to the info window afterwards."
  (interactive)
  (let ((file-for-info-buffer (if (string-equal (buffer-name) fileTree-buffer-name)
                                  (fileTree-getName)
                                nil)))
    (if (and (buffer-live-p fileTree-info-buffer)
             (window-live-p fileTree-info-window))
        (progn
          (switch-to-buffer fileTree-info-buffer)
          (save-buffer)
          (kill-buffer fileTree-info-buffer)
          (setq fileTree-info-buffer-state nil))
      (progn
        (setq fileTree-info-buffer (find-file-noselect fileTree-notes-file))
        (setq fileTree-info-buffer-state t)
        (setq fileTree-info-window
              (display-buffer-in-side-window fileTree-info-buffer
                                             '((side . right))))
        (if file-for-info-buffer
              (fileTree-update-info-buffer file-for-info-buffer)
          (fileTree-update-info-buffer))
        (if switchToInfoFlag
            (select-window fileTree-info-window))))))

(defun fileTree-update-info-buffer (&optional current-file-name)
  "Update info buffer contents to reflect CURRENT-FILE-NAME.
If CURENT-FILE-NAME not given use 'buffer-file-name'.
If no entry in info buffer for this file, create new info buffer entry."
  ;; TODO: clean up
  (let ((fileTree-create-new-entry (if current-file-name nil t)))
    (unless current-file-name (setq current-file-name (buffer-file-name)))
    (unless current-file-name (setq current-file-name "No File Note Entry"))
    (let ((current-window (selected-window)))
      (select-window fileTree-info-window)
      (switch-to-buffer fileTree-info-buffer)
      (if (get-buffer-window fileTree-info-buffer)
          (let ((searchString (concat "* [[" current-file-name "]")))
            (find-file fileTree-notes-file)
            (widen)
            (goto-char (point-min))
            (unless (search-forward searchString nil t)
              (if fileTree-create-new-entry
                  (progn
                    (message "creating new entry")
                    (goto-char (point-max))
                    (let ((filename (car (last (split-string current-file-name "/") 1))))
                      (insert (concat "\n" "* [[" current-file-name "][" filename "]]\n"))))
                (unless (search-forward "* [[No File Note Entry]" nil t)
                  (progn
                    (message "creating No File Note Entry")
                    (goto-char (point-max))
                    (fileTree-insert-noNoteEntry)))))
            (org-narrow-to-subtree)))
      (select-window current-window))))

(defun fileTree-insert-noNoteEntry ()
  "Insert an entry in info file indicating not file note entry.
This is used when first starting an info note file."
  (insert (concat "\n* [[No File Note Entry]]\n"
                  (propertize (concat "\u250c"
                                      (make-string 9 ?\u2500)
                                      "\u2510\n\u2502 NO NOTE \u2502\n\u2514"
                                      (make-string 9 ?\u2500)
                                      "\u2518\n")
                              'font-lock-face '(:foreground "red")))))

(defun fileTree-updateBuffer ()
  "Update the display buffer (following some change).
This function should be called after any change to 'fileTree-currentFileList'."
  (interactive)
  (let ((text-scale-previous (buffer-local-value 'text-scale-mode-amount
                                                 (current-buffer))))
    (save-current-buffer
      (with-current-buffer (get-buffer-create fileTree-buffer-name)
        (let ((fileTree-currentName (fileTree-getName)))
          (setq buffer-read-only nil)
          (erase-buffer)
          (setq fileTree-currentFileList (-distinct (-non-nil
                                                     fileTree-currentFileList)))
          (setq fileTree-fileListStack (cons (copy-sequence fileTree-currentFileList)
                                             fileTree-fileListStack))
          (fileTree-printHeader)
          (if fileTree-showFlatList
              (fileTree-print-flat fileTree-currentFileList)
            (fileTree-print-tree (fileTree-createFileTree
                                  (reverse fileTree-currentFileList)) ()))
          (setq fileTree-overallDepth
                (if (null fileTree-currentFileList) 0
                  (apply 'max (mapcar #'(lambda (x) (length (split-string x "/")))
                                      fileTree-currentFileList))))
          ;; (fileTree-update-info-buffer fileTree-buffer-name)
          (switch-to-buffer fileTree-buffer-name)
          (fileTree-goto-name fileTree-currentName)
          (setq buffer-read-only t)
          (fileTree)
          (text-scale-increase text-scale-previous))))))

(defun fileTree-pop-fileListStack ()
  "Pop last state from file list stack."
  (interactive)
  (if (> (length fileTree-fileListStack) 1)
      (setq fileTree-fileListStack (cdr fileTree-fileListStack)))

  (setq fileTree-currentFileList (car fileTree-fileListStack))
  (if (> (length fileTree-fileListStack) 1)
      (setq fileTree-fileListStack (cdr fileTree-fileListStack)))
  (fileTree-updateBuffer))
  

(defun fileTree-narrow (subtree)
  "Narrow file tree to SUBTREE."
  (setq fileTree-currentFileList (fileTree-createFileList (list subtree)))
  (fileTree-updateBuffer))

(defun fileTree-file-face (filename)
  "Return face to use for FILENAME.
Info determined from 'fileTree-filetype-list' and 'fileTree-default-file-face'."
  (let ((file-face fileTree-default-file-face)
        (my-file-face-list (mapcar #'(lambda (x) (cdr (cdr x)))
                                   fileTree-filetype-list))
        (elem nil)
        (fileTree-regex nil))
    (while (/= (length my-file-face-list) 0)
      (setq elem (car my-file-face-list))
      (setq fileTree-regex (car elem))
      (if (> (length fileTree-regex) 0)
          (if (string-match fileTree-regex filename)
              (setq file-face (car (cdr elem)))))
      (setq my-file-face-list (cdr my-file-face-list)))
    file-face))

(defun fileTree-grep ()
  "Run grep on files in 'currentFileList'.
Takes input from user for grep pattern."
  (interactive)
  (if (version< emacs-version "27")
      (message "fileTree-grep not supported for emacs versions before 27")
    (let* ((myFileTree-regex (read-string "Type search string:"))
           (xrefs nil)
           (fetcher
            (lambda ()
              (setq xrefs (xref-matches-in-files myFileTree-regex
                                                 (-filter 'file-exists-p fileTree-currentFileList)))
              (unless xrefs
                (user-error "No matches for: %s" myFileTree-regex))
              xrefs)))
      (xref--show-xrefs fetcher nil))))

(defun fileTree-helm-filter ()
  "Use helm-based filtering on fileTree."
  (interactive)
  (setq fileTree-fileListStack-save (copy-sequence fileTree-fileListStack))
  (let ((current-node (fileTree-getName)))
    (add-hook 'helm-after-update-hook
              #'fileTree-helm-hook)
    (helm :sources '(fileTree-helm-source))
    (fileTree-goto-name current-node)))

(defun fileTree-helm-hook ()
  "Helm hook for fileTree."
  (interactive)
  (setq fileTree-currentFileList (car (helm--collect-matches
                                       (list (helm-get-current-source)))))
  (fileTree-updateBuffer))

(defun fileTree-select-file-list ()
  "Select file list from saved file lists."
  (interactive)
  (let ((fileList (helm :sources (helm-build-sync-source "File Lists"
                                   :candidates (mapcar #'(lambda (x)
                                                           (car x))
                                                       fileTree-saved-lists)
                                   :action (lambda (candidate)
                                             (let ((file-list
                                                    (car (cdr (assoc candidate
                                                                     fileTree-saved-lists)))))
                                               (if (functionp file-list)
                                                   (setq file-list (funcall file-list)))
                                               file-list))
                                                                   
                                   :fuzzy-match t)
                        :buffer "*fileTree-helm-buffer*")))
    (if fileList
        (setq fileTree-currentFileList fileList))
    (fileTree-updateBuffer)))

(defun fileTree-update-saved-lists-file ()
  "Save current `fileTree-saved-lists' to file."
  (save-current-buffer
    (with-current-buffer (get-buffer-create "*fileTree-temp*")
      (erase-buffer)
      (insert "(setq fileTree-saved-lists '")
      (insert (format "%S" fileTree-saved-lists))
      (insert ")")
      (write-file fileTree-saved-lists-file))))
  
(defun fileTree-save-list ()
  "Save current file list."
  (interactive)
  (let ((list-name (helm :sources (helm-build-sync-source "File Lists"
                                    :candidates (cons "*New Entry*"
                                                      (mapcar #'(lambda (x)
                                                                  (car x))
                                                              fileTree-saved-lists))
                                   :fuzzy-match t)
                         :buffer "*fileTree-helm-buffer*")))
    (if (string= list-name "*New Entry*")
        (setq list-name (read-string "Enter new list name:")))
    ;; first delete previous list-name entry (if any)
    (setq fileTree-saved-lists (delete nil (mapcar #'(lambda (x)
                                                       (if (string-match
                                                            list-name
                                                            (car x))
                                                           nil x))
                                                   fileTree-saved-lists)))
    ;; add new entry
    (setq fileTree-saved-lists (cons (cons list-name (list fileTree-currentFileList))
                                     fileTree-saved-lists))
    (fileTree-update-saved-lists-file)))
      
(defun fileTree-delete-list()
  "Delete a file list from the `fileTree-saved-list' and save to file."
  (interactive)
  (let ((list-name (helm :sources (helm-build-sync-source "File Lists"
                                    :candidates (mapcar #'(lambda (x)
                                                            (car x))
                                                        fileTree-saved-lists)
                                   :fuzzy-match t)
                         :buffer "*fileTree-helm-buffer*")))
    (setq fileTree-saved-lists (delete nil (mapcar #'(lambda (x)
                                                       (if (string-match
                                                            list-name
                                                            (car x))
                                                           nil x))
                                                   fileTree-saved-lists)))
    (fileTree-update-saved-lists-file)))
    
(defun fileTree-showFiles (fileList)
  "Load FILELIST into current file list and show in tree mode."
  (setq fileTree-currentFileList fileList)
  (setq fileTree-fileListStack (list fileTree-currentFileList))
  (fileTree-updateBuffer))

(defun fileTree-showRecentfFiles ()
  "Load recentf list into current file list and show in tree mode."
  (interactive)
  (if (not recentf-mode)
      (recentf-mode))
  (fileTree-showFiles recentf-list))

(defun fileTree-showCurDir ()
  "Load files in current directory into current file list and show in tree mode."
  (interactive)
  (setq fileTree-currentFileList nil)
  (setq fileTree-fileListStack (list fileTree-currentFileList))
  (fileTree-expandDir (file-name-directory (buffer-file-name)) 0))

(defun fileTree-showCurDirRecursively ()
  "Load files in current directory (recursively) into current file list and show in tree mode."
  (interactive)
  (setq fileTree-currentFileList nil)
  (setq fileTree-fileListStack (list fileTree-currentFileList))
  (fileTree-expandDirRecursively (file-name-directory (buffer-file-name)) 0))

(defun fileTree-showCurBuffers ()
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
    (setq fileTree-currentFileList myFileList)
    (fileTree-updateBuffer)))

(defun fileTree-findFilesWithNotes ()
  "Return list of files with notes."
  (find-file fileTree-notes-file)
  (goto-char (point-min))
  (widen)
  (let ((regexp "^\\* \\[\\[\\(.*\\)\\]\\[")
        (filelist nil)
        (myMatch nil))
    (while (re-search-forward regexp nil t)
      (setq myMatch (match-string-no-properties 1))
      (setq filelist (cons myMatch filelist)))
    filelist))
  
(defun fileTree-showFilesWithNotes ()
  "Load files with entries in notes file."
  (interactive)
  (fileTree-showFiles (fileTree-findFilesWithNotes)))

(define-derived-mode fileTree nil "Text"
  "A mode to view and perform operations on files via a tree view"
  (make-local-variable 'fileTree-list))

(provide 'fileTree)
;;; fileTree.el ends here
