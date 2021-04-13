# Filetree
Filetree is a package that provides two basic functions:

* **File tree viewer**
The viewer displays a file list as a directory tree in a special buffer.  The file list can be populated from any list of files.  There are functions to populate from a number of common sources: recentf, files in buffer-list, files in the current directory, and files found recursively in the current directory.  Within the viewer, the file list can be filtered and expanded in various ways and operations can be performed on the filtered file list (e.g., grep over files in list, open file, etc.).  Multiple file lists can be saved and retrieved between sessions.

* **File notes**
The file notes enable the user to write and display (org-mode) notes associated with individual files and directories.  The note can be displayed in a side buffer either when cycling through files in the file tree viewer or when the file is open in a buffer.  The notes are kept in a single org-mode file with a heading for each file/directory.

![filetree demo](filetree_demo.gif)

## File tree Viewer

### Starting and Exit Viewer
The following commands start the viewer with the corresponding file list
| Command                           | Comment                                       |
|-----------------------------------|-----------------------------------------------|
| filetree-select-file-list         | select file list from perviously saved lists  |
| filetree-show-recentf-files       | populate files from recentf-list              |
| filetree-show-cur-dir             | populate files from current dir               |
| filetree-show-cur-dir-recursively | populate files from current dir (recursively) | 
| filetree-show-cur-buffers         | populate files from buffer-list               |
| filetree-show-files-with-notes    | populate file list with files with "notes"    |
| filetree-show-files               | populate files from file list in argument     |
| filetree-close-session            | exit viewer (tied to q in keymap)             |


### Navigation
Within the *Filetree* window the following navigation commands can be used
| Command              | key map     | Comment         |
|----------------------|-------------|-----------------|
| filetree-next-line   | down, j     |                 | 
| filetree-prev-line   | up, k       |                 | 
| filetree-next-branch | SPC         |                 | 
| filetree-prev-branch | TAB         |                 |

### Filtering and Expanding
| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| filetree-filter                 | f       | <ADD>                                       |
| filetree-helm-filter            | s       | helm-based search                           |
| filetree-expand-dir             | e       | Add files in directory at point             |
| filetree-expand-dir-recursively | E       | Add files in directory at point recursively |
| filetree-reduce-list-by-10      | -       |                                             |
| filetree-pop-file-list-stack    | b       | undo prev filter/expansion operation        |

Notes:
* The file list is added to a stack after each filtering operations (filetree-filter, filetree-helm-filter, filetree-reduce-list-by-10), and the filtering operation can be undone by popping off the stack (filetree-pop-file-list-stack).
* Selecting (by pressing RETURN or clicking) on a file in the Filetree buffer opens the file.  Selecting on a directory narrows to the chosen subtree.
* The filetree-toggle-combined-dir-names command can be helpful when wanted to use filetree-expand (or filetree-expand-dir-recursively) on a directory one or more levels above a file in the file list.

### View modes
| Command                           | key map | Comment                                     |
|-----------------------------------|---------|---------------------------------------------|
| filetree-set-max-depth            | 0-9     | set max depth of tree to view 0=max         |
| filetree-cycle-max-depth          | <none>  | cycle through max depth                     |
| filetree-toggle-combine-dir-names | /       | toggle combining dir/subdirs in dir name    |
| filetree-toggle-flat-vs-tree      | .       | toggle between tree and flat view           |
| filetree-toggle-use-all-icons     | ;       | toggle use-all-icons icons (if installed)   |


### Operations
| Command       | key map | Comment                                         |
|---------------|---------|-------------------------------------------------|
| filetree-grep | g       | grep over files in current list                 |
| dired         | d       | opens a dired session at the directory at point |

### Save/Retrieve file list
File lists can be saved/retrieved from the file specified by filetree-saved-lists-file.  The filetree-select-file-list function uses a helm interface for selection of the file list.
| Command                   | key map | Comment                                |
|---------------------------|---------|----------------------------------------|
| filetree-select-file-list | L       | load/select previously saved file list |
| filetree-save-list        | S       | save current file list                 |
| filetree-delete-list      | D       | delete file list                       |

## File Notes
This package maintains a notes file in the file specified by filetree-notes-file (default: ~/.emacs.d/filetree-notes.org).  This is an org-mode file that can hold notes associated with any file, and those notes can be seen in a side window as the user navigates through the file tree.

In order to go to the entry for a file (and create an entry if it doesn't exist), use the filetree-toggle-info-buffer command.  For example, you can use the following key binding in your .emacs to run the command:
```
(global-set-key (kbd "C-c <return>") (lambda ()
                                       "Toggle filetree-info-buffer and switch to it if active"
                                       (interactive)
                                       (filetree-toggle-info-buffer t)))
```
The same command will open and close (after saving) the notes buffer in the side window.

Within the filetree buffer, the "i" key will toggle the side window with the notes file.  The notes will dynamically narrow to the relevant part of the file as the user navigates the file tree.  If there is no note entry for the file, then the message "No File Note Entry" will be shown in the side window.
| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| filetree-toggle-info-buffer     | i       | Toggle info buffer in side window           |
|                                 | I       | ^^ and then switch to side window if active |

## Customizations

### Files used by filetree
| Parameter                 | default                            | Comment                        |
|---------------------------|------------------------------------|--------------------------------|
| filetree-notes-file       | ~/.emacs.d/filetree-notes.org      | File used for file notes       |
| filetree-saved-lists-file | ~/.emacs.d/filetree-saved-lists.el | File used for saved file lists |

### Settings related to startup state
| Parameter                  | default  | Comment                                          |
|----------------------------|----------|--------------------------------------------------|
| filetree-info-window       | nil      | Set to t to show notes/info side window at start |
| filetree-use-all-the-icons | nil      | Set to t to show icons for files/dirs            |
Note enabling use-all-the-icons can make some of the operations sluggish if the file list is large.

### Faces, marks, and misc
The variable filetree-exclude-list is a list of regex for files to ignore.

The marks used to draw the file trees can be customized.  Here is the list of symbols that are used:
filetree-symb-for-root filetree-symb-for-box, filetree-symb-for-vertical-pipe, filetree-symb-for-horizontal-pipe, filetree-symb-for-left-elbow, filetree-symb-for-right-elbow, filetree-symb-for-branch-and-cont, filetree-symb-for-file-node)

The faces used for different file types as well as the shortcuts used to filter those file types are specified using 'filetree-add-filetype.  The function that sets the default settings can be used as an example (see below).  The calls to filetree-add-filetype has the following arguments: file type name, shortcut, regex, face.  Note (setq filetree-filetype-list nil) clears any previous filetype entries in filetree-filetype-list.
```
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
```
The default face is specified by filetree-default-file-face.
