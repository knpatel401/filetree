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
(require 'recentf)
(require 'dash)
(require 'xref)
(require 'helm)
(require 'rect)
(require 'org)
(require 'cl-lib)

(defgroup fileTree nil
  "Tree view of file list and file notes"
  :group 'matching
  :prefix "fileTree-")

(defvar fileTree-version "1.0")

(defconst fileTree-buffer-name "*FileTree*")
;;(defconst fileTree-info-buffer-name "*FileTree-Info*")

(defvar fileTree-notes-file "~/.emacs.d/fileTree-notes.org")
(defvar fileTree-info-buffer nil)
(defvar fileTree-info-buffer-state nil)

;; (setq fileTree-abbrevDirs nil)
(defvar fileTree-startPosition 0)
(defvar fileTree-maxDepth 0)
(defvar fileTree-overallDepth nil)
(defvar fileTree-currentFileList nil)
(defvar fileTree-fileListStack nil)
(defvar fileTree-fileListStack-save nil)
(defvar fileTree-showFlatList nil)
(defvar fileTree-combineDirNames t)
(defvar fileTree-info-window nil)
(defvar fileTree-default-file-face 'default)
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
  
;; (defvar fileTree-file-face-list
;;   '(("\.py$"
;;      (:foreground "dark green"))
;;     ("\\(?:\\.[ch]$\\|\\.cpp\\)"
;;      (:foreground "orange"))
;;     ("\.org$"
;;      (:foreground "maroon"))
;;     ("\\(?:\\.js\\(?:\\|on\\)\\)"
;;      (:foreground "orange"))
;;     ("\.pdf$"
;;      (:foreground "navyblue"))
;;     ("\.m$"
;;      (:foreground "black"))
;;     ("\.lua$" (:foreground "black"))
;;     ("\\(?:\\.e\\(?:l$\\|macs\\)\\)"
;;      (:foreground "gray30"))
;;     ("\.txt$"
;;      (:foreground "gray50"))
;;     ("\.cfg$" (:foreground "gray50"))))
(defvar fileTree-file-face-list
  '(("\.py$"
     (:foreground "beige" :background "dark green"))
    ("\\(?:\\.[ch]$\\|\\.cpp\\)"
     (:foreground "white" :background "orange"))
    ("\.org$"
     (:foreground "beige" :background "maroon"))
    ("\\(?:\\.js\\(?:\\|on\\)\\)"
     (:foreground "beige" :background "orange"))
    ("\.pdf$"
     (:foreground "white" :background "navyblue"))
    ("\.m$"
     (:foreground "yellow" :background "black"))
    ("\.lua$" (:foreground "orange" :background "black"))
    ("\\(?:\\.e\\(?:l$\\|macs\\)\\)"
     (:foreground "white" :background "gray30"))
    ("\.txt$"
     (:foreground "beige" :background "gray50"))
    ("\.cfg$" (:foreground "beige" :background "gray50"))))

(defvar fileTree-filterList
  '((0 "No Filter" "")
    (?p "Python" "\.py$")
    (?o "Org-mode" "\.org$")
    (?e "elisp" "\\(?:\\.e\\(?:l\\|macs\\)\\)")
    (?c "C" "\\(?:\\.[ch]$\\|\\.cpp\\)")
    (?l "Lua" "\.lua$")
    (?d "PDF doc" "\.pdf$")
    (?m "Matlab" "\.m$")))

(defvar fileTree-excludeList
  '("~$" "#$" ".git\/" ".gitignore$" "\/\.\/$" "\/\.\.\/$" ".DS_Store$"))

(defvar fileTree-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" '(lambda () (interactive) (message "%s %s" (fileTree-getName)
                                                             (if (button-at (point))
                                                                 (button-get (button-at (point)) 'subtree)
                                                               nil))))
    (define-key map "j" 'fileTree-next-line)
    (define-key map "k" 'fileTree-prev-line)
    (define-key map (kbd "<down>") 'fileTree-next-line)
    (define-key map (kbd "<up>") 'fileTree-prev-line)
    (define-key map (kbd "C-j") 'fileTree-next-line)
    (define-key map (kbd "C-k") 'fileTree-prev-line)
    (define-key map (kbd "SPC") 'fileTree-next-branch)
    (define-key map (kbd "TAB") 'fileTree-prev-branch)
    (define-key map "q" 'recentf-cancel-dialog)
    (define-key map "0" '(lambda () (interactive) (fileTree-set-maxDepth 0)))
    (define-key map "1" '(lambda () (interactive) (fileTree-set-maxDepth 1)))
    (define-key map "2" '(lambda () (interactive) (fileTree-set-maxDepth 2)))
    (define-key map "3" '(lambda () (interactive) (fileTree-set-maxDepth 3)))
    (define-key map "4" '(lambda () (interactive) (fileTree-set-maxDepth 4)))
    (define-key map "5" '(lambda () (interactive) (fileTree-set-maxDepth 5)))
    (define-key map "6" '(lambda () (interactive) (fileTree-set-maxDepth 6)))
    (define-key map "7" '(lambda () (interactive) (fileTree-set-maxDepth 7)))
    (define-key map "8" '(lambda () (interactive) (fileTree-set-maxDepth 8)))
    (define-key map "9" '(lambda () (interactive) (fileTree-set-maxDepth 9)))
    (define-key map "r" 'fileTree-showRecentfFiles)
    (define-key map "f" 'fileTree-filter)
    (define-key map "/" 'fileTree-toggle-combineDirNames)
    (define-key map "b" 'fileTree-pop-fileListStack)
    (define-key map "g" 'fileTree-grep)
    (define-key map "d" '(lambda () (interactive) (dired (fileTree-getName))))
    (define-key map "e" '(lambda () (interactive) (fileTree-expandDir
                                                   (fileTree-getName))))
    (define-key map "E" '(lambda () (interactive) (fileTree-expandDirRecursively
                                                   (fileTree-getName))))
    (define-key map "x" '(lambda () (interactive) (fileTree-remove-item
                                                   (fileTree-getName))))
    (define-key map "-" 'fileTree-reduceListBy10)
    (define-key map "." 'fileTree-toggle-flat-vs-tree)
    (define-key map "i" 'fileTree-toggle-info-buffer)
    (define-key map "I" (lambda ()
                          "Toggle fileTree-info-buffer and switch to it if active"
                          (interactive)
                          (fileTree-toggle-info-buffer t)))
    (define-key map (kbd "C-.") 'fileTree-toggle-flat-vs-tree)
    (define-key map "s" 'fileTree-helm-filter)
    map)
  "Keymap for fileTree.")

(defun fileTree-filter ()
  "Interactive function to filter 'fileTree-currentFileList'.
Uses regular expression in 'fileTree-filterList' or by expression entered by user."
  (interactive)
  (let ((myFileTree-mode-filterList fileTree-filterList)
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

(defun fileTree-remove-item (file_or_dir)
  "Remove the file or subdir ENTRY from the fileTree-currentFileList."
  (interactive)
  (print file_or_dir)
  (setq fileTree-currentFileList (delete nil
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
								                   fileTree-currentFileList))))
  (fileTree-updateBuffer))

(defun fileTree-getName ()
  "Get name of file/dir on line at current point."
  (interactive)
  (if (button-at (point))
	  (button-get (button-at (point)) 'name)
	nil))

(defun fileTree-expandDir (dir &optional filter)
  "Add files in DIR to 'fileTree-currentFileList'.
If FILTER is not specified, will 'read-char' from user.  The corresponding
regular expression in 'fileTree-filterList' will be used to filter which files
are included.  If FILTER does not correspond to an entry in
'fileTree-filterList' the user is prompted for a string/regular expression to
filter with."
  (interactive)
  (let ((fileTree-charInput (or filter (read-char)))
        (myFileTree-mode-filterList fileTree-filterList)
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

(defun fileTree-expandDirRecursively (dir &optional filter)
  "Recursively add files in DIR to 'fileTree-currentFileList'.
If FILTER is not specified, will 'read-char' from user.  The corresponding
regular expression in 'fileTree-filterList' will be used to filter which files
are included.  If FILTER does not correspond to an entry in
'fileTree-filterList' the user is prompted for a string/regular expression to
filter with."
  (interactive)
  (let ((fileTree-charInput (or filter (read-char)))
        (myFileTree-mode-filterList fileTree-filterList)
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


(defun fileTree-reduceListBy10 ()
  "Drop last 10 entries in fileTree-currentFileList."
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
  
(defun fileTree-set-maxDepth (maxDepth)
  "Set depth of displayed file tree to MAXDEPTH."
  (interactive)
  (setq fileTree-maxDepth maxDepth)
  (fileTree-updateBuffer))

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
  "Helper function to move point to item on current line."
  (interactive)
  (if (< (point) fileTree-startPosition)
      (goto-char fileTree-startPosition))
  (move-end-of-line 1)
  (re-search-backward " ")
  (forward-char)
  ;; (if fileTree-info-buffer-state
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
	;; (fileTree-goto-node)
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
		  (progn
			(setq fileTree-current-line (line-number-at-pos))))))))

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
		  (progn
			(setq fileTree-current-line (line-number-at-pos))))))))

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
	  (insert (spaces-string (max 1 (- 30 (length filename)))))
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

(defun fileTree-print-tree (dirTree depthList)
  "Print directory tree.
Print DIRTREE up to a depth of DEPTHLIST."
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
		  	  (let ((myPrefix (string-join (mapcar #'(lambda (x) (if (> x 0)
                                                                     ;; continue
																	 " \u2502  "
                                                                   "    "))
                                                   (butlast myDepthList 1)) ""))

                    (dirContents (nth 2 thisEntry))
                    (fileTree-dirString nil))
                    ;; (thisType (car (nth 2 thisEntry))))
		  	  	(insert myPrefix)
				(if (> (length myDepthList) 1)
					(if (> (car (last myDepthList)) 0)
                        ;; branch and continue
					  	(insert " \u251c\u2500\u2500 ")
                      ;; last branch
					  (insert " \u2514\u2500\u2500 "))
                  ;; Tree root
				  (insert " \u25a0\u25a0\u25ba "))
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
    						;; (insert (concat "/"  thisName))
    						(setq fileTree-dirString (concat fileTree-dirString
    															  "/"  thisName))
    						(setq dirContents (nth 2 thisEntry))))))
		  	  	(insert-text-button fileTree-dirString
									'face 'bold
									'action (lambda (x) (fileTree-narrow
														 (button-get x 'subtree)))
									;; 'name (nth 3 thisEntry)
									'name (concat (nth 3 thisEntry) "/")
									'subtree thisEntry)
			  	(insert "/\n")
		  	  	(setq myDepthListCopy (copy-tree myDepthList))
				(if (> (length dirContents) 0)
					(fileTree-print-tree dirContents myDepthListCopy)))

			;; file
		    (let ((myLink (nth 2 thisEntry))
                  (fileText (concat thisName))
                  (myPrefix (string-join (mapcar #'(lambda (x) (if (= x 0)
                                                                   "    "
                                                                 ;; continue
                                                                 " \u2502  "))
                                                 (butlast myDepthList 1)) "")))
			  (if (> (car (last myDepthList)) 0)
                  ;; file and continue
		  		  (setq myPrefix (concat myPrefix " \u251c" "\u2500\u25cf "))
                ;; last file
		  		(setq myPrefix (concat myPrefix " \u2514" "\u2500\u25cf ")))

			  (insert myPrefix)
			  (let ((button-face (fileTree-file-face fileText)))
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
  (insert (concat "\u2502 "
                  (propertize "# files: " 'font-lock-face 'bold)
                  (number-to-string (length fileTree-currentFileList))
                  (propertize "\tMax depth: " 'font-lock-face 'bold)
                  (if (> fileTree-maxDepth 0)
                      (number-to-string fileTree-maxDepth)
                    "full")
                  "\t"
                  (if fileTree-showFlatList
                      (propertize "Flat view" 'font-lock-face '(:foreground "blue"))
                    (propertize "Tree view" 'font-lock-face '(:foreground "green")))
                  " \n\u2514"))
  (insert (make-string (+ (point) 1) ?\u2500))
  (insert "\u2518\n")
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
							     (concat "/" (string-join (reverse filenameList) "/"))
							     ))
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
    fileTree ))

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
  "Update the display buffer (following some change)."
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
Info determined from 'fileTree-file-face-list' and 'fileTree-default-file-face'."
  (let ((file-face fileTree-default-file-face)
		(my-file-face-list fileTree-file-face-list)
        (elem nil)
        (fileTree-regex nil))
	(while (/= (length my-file-face-list) 0)
	  (setq elem (car my-file-face-list))
	  (setq fileTree-regex (car elem))
	  (if (string-match fileTree-regex filename)
	   	  (setq file-face (car (cdr elem))))
	  (setq my-file-face-list (cdr my-file-face-list))
	  )
	file-face))

(defun fileTree-grep ()
  "Run grep on files in 'currentFileList'.
Takes input from user for grep pattern."
  (interactive)
  (let* ((myFileTree-regex (read-string "Type search string:"))
         (xrefs nil)
         (fetcher
          (lambda ()
            (setq xrefs (xref-matches-in-files myFileTree-regex
                                               (-filter 'file-exists-p fileTree-currentFileList)))
            (unless xrefs
              (user-error "No matches for: %s" myFileTree-regex))
            xrefs)))
    (xref--show-xrefs fetcher nil)))

(defun fileTree-helm-filter ()
  "Use helm-based filtering on fileTree."
  (interactive)
  (setq fileTree-fileListStack-save (copy-sequence fileTree-fileListStack))
  (add-hook 'helm-after-update-hook
            #'fileTree-helm-hook)
  (helm :sources '(fileTree-helm-source)))

(defun fileTree-helm-hook ()
  "Helm hook for fileTree."
  (interactive)
  (setq fileTree-currentFileList (car (helm--collect-matches
                                       (list (helm-get-current-source)))))
  (fileTree-updateBuffer))

(defun fileTree-showFiles (fileList)
  "Load FILELIST into current file list and show in tree mode."
  (setq fileTree-currentFileList fileList)
  (setq fileTree-fileListStack (list fileTree-currentFileList))
  (fileTree-updateBuffer)
  )

(defun fileTree-showRecentfFiles ()
  "Load recentf list into current file list and show in tree mode."
  (interactive)
  (fileTree-showFiles recentf-list))

(defun fileTree-showCurDir ()
  "Load files in current directory into current file list and show in tree mode."
  (interactive)
  (setq fileTree-currentFileList nil)
  (setq fileTree-fileListStack (list fileTree-currentFileList))
  (fileTree-expandDir (file-name-directory (buffer-file-name)) 0)
  )

(defun fileTree-showCurDirRecursively ()
  "Load files in current directory (recursively) into current file list and show in tree mode."
  (interactive)
  (setq fileTree-currentFileList nil)
  (setq fileTree-fileListStack (list fileTree-currentFileList))
  (fileTree-expandDirRecursively (file-name-directory (buffer-file-name)) 0)
  )

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
                                 myFileList)))
      )
    (setq fileTree-currentFileList myFileList)
    (fileTree-updateBuffer)
    ))

(defun fileTree-findFilesWithNotes ()
  "Return list of files with notes."
  (find-file fileTree-notes-file)
  (goto-char (point-min))
  (widen)
  ;; (let ((regexp "\^\\* \\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]")
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
