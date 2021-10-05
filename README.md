# Filetree
Filetree is a package that provides two basic functions:

* **File tree viewer**
The viewer displays a file list as a directory tree in a special buffer.  The file list can be populated from any list of files.  There are functions to populate from a number of common sources: recentf, files in buffer-list, files in the current directory, and files found recursively in the current directory.  Within the viewer, the file list can be filtered and expanded in various ways and operations can be performed on the filtered file list (e.g., grep over files in list, open file, etc.).  Multiple file lists can be saved and retrieved between sessions.

* **File notes**
The file notes enable the user to write and display (org-mode) notes associated with individual files and directories.  The note can be displayed in a side buffer either when cycling through files in the file tree viewer or when the file is open in a buffer.  The notes are kept either in a single org-mode file with a heading for each file/directory, and/or in local project specfic org-mode files.

![filetree screenshot](screenshots/filetree_screenshot.jpg)

# Demo Video
A video demoing some of the primary functionality is on [Youtube here](https://youtu.be/-KrMaLq8Bms).  The corresponding notes are in this repo [demo_notes.org](demo_notes.org).

Here are links to sections of the video that cover specific topics.

| Link to video section                                                   | Comment                                                                           |
|-------------------------------------------------------------------------|-----------------------------------------------------------------------------------|
| [Recentf wrapper](https://youtu.be/-KrMaLq8Bms?t=195)                   | This was the motivating use case for the package.                                 |
| [Filtering and expanding file list](https://youtu.be/-KrMaLq8Bms?t=395) | This section covers some of the core functionality for file tree.                 |
| [Other file lists](https://youtu.be/-KrMaLq8Bms?t=938)                  | Covers other ways to populate the file list (e.g., current dir, current buffers). |
| [Grep within files](https://youtu.be/-KrMaLq8Bms?t=1111)                | Covers the search use case.                                                       |
| [File Notes](https://youtu.be/-KrMaLq8Bms?t=1181)                       | Covers the file notes functionality.                                              |

# Use Cases and Alternatives 
The filetree package provides a set of interactive tools for finding and discovering files and information about files:
* **Visual contextualization** - directory structure, faces for file types, icons, file info (mode, size, date, etc.), file notes in side window
* **Interactive filtering tools** - filter by type, regex, helm-based interactive filtering, file marking, etc.
* **File discovery** - tools for finding new files from original file list
* **Actions on files** - tools for acting on files in file list (e.g., grep)
* **Notes** - support for file specific org-mode notes

These tools can be used in a number of different ways.  Some possible use cases along with alternatives are discussed below.

## Recent files use case
The filetree package can be used as a wrapper for the built-in recentf command.  The command for using the wrapper is filetree-showRecentfFiles.  Compared with the basic recentf command, the filetree command provides better visual context for the recent files, and a host of tools for filtering the recent file list or finding other files that are located around a recently accessed file.  This enables users to use recent files in a more expansive way.  Users may want to increase the number of files saved from default of 20 to something higher using the following setting:
```
(setq recentf-max-saved-items 500)
```
Alternative wrappers for recentf (e.g., the [helm](https://emacs-helm.github.io/helm/) command helm-recentf), provide some filtering tools for recentf, but not the other features available in the filetree package.

## Current buffers use case
This package can be used as an alternative to ibuffer in some cases, using the command filetree-show-cur-buffers.  The command provides the same benefits over ibuffer as the recentf wrapper does over recentf in the previous section.

While ibuffer represents all buffers, filetree-show-cur-buffers represents files not buffers.  As a result it does not represent buffers that are not associated with files (e.g., *Messages* or *scratch*).  Another difference is that ibuffer provides some buffer grouping tools.  In filetree there are implicit groupings via the directory structure, or via filtering or font highlighting rules, but no explicit grouping.

[bufler](https://github.com/alphapapa/bufler.el) is an alternative that provides some additional automatic buffer grouping support over ibuffer.  The differences between bufler and filetree are the same as the differences between ibuffer and filetree.

## File/Project explorer use case
The filetree package can be used as a file or project explorer, using filetree-show-cur-dir or filetree-show-cur-dir-recursively or by saving/loading a file list for a project.

There are many other alternative tree-based file/project explorers (e.g., [treemacs](https://github.com/Alexander-Miller/treemacs), [neotree](https://github.com/jaypei/emacs-neotree)).  For the use case of having a (persistent) project directory/file tree displayed in a side window, these other packages are preferable over filetree.  For example, treemacs can be configured to automatically update the tree when there are changes in the file system, it has projectile and git integration, etc..  

The project/file explorer use case for filetree is different, and comes from a couple of fundamental differences between filetree and these other file/project explorers:
* First, filetree starts with the implicit assumption that some files are more important to the user than others (e.g., recent files, files currently opened in a buffer, files in the current directory, a previously saved file list)--only those files are initially displayed in the tree, and others are found through discovery (via expansion tools).
* Secondly, there's an emphasis on interactive tools for project/file exploration--there are a lot of interactive tools available in filetree to navigate, filter, and discover files.

In a typical filetree use case, the filetree is pulled up for an action (e.g., finding a file, searching for something in the files in a project, visualizing the directory structure), the interactive tools are used to find the desired information or file and then filetree is closed.  This is different from how a conventional project/file explorer is typically used.

## File notes use case 
The file notes functionatity in filetree can be used as a light weight file-specific (org-mode) note taking tool.  With the integration with the rest of the filetree package, those notes can also be displayed while navigating the filetree.  What makes filetree's note functionality useful is that it's simple and there's very little overhead--a note for any file can be visited (or automatically created) with a single key binding, and the notes are centralized in a single global org file (or project specific org files if desired).

Alternatives: [org-noter](https://github.com/weirdNox/org-noter) is an alternative package for taking notes associated with a file, and it can do some nice things like have notes sync'ed to your position in the document as you scroll.  For annotating a document this would be preferable to filetree.

# File tree Viewer

## Starting and Exit Viewer + Navigation & selection
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


Within the *Filetree* window the following navigation commands can be used
| Command              | key map          | Comment         |
|----------------------|------------------|-----------------|
| filetree-next-line   | down, j          | down one line   |
| filetree-prev-line   | up, k            | up one line     |
| filetree-next-branch | SPC              | down one branch |
| filetree-prev-branch | TAB              | up one branch   |
| --                   | RETURN (on file) | open file       |

## View modes
![filetree demo views](screenshots/filetree_demo_views.gif)

| Command                           | key map | Comment                                    |
|-----------------------------------|---------|--------------------------------------------|
| filetree-set-max-depth            | 0-9     | set max depth of tree to view 0=max        |
| filetree-cycle-max-depth          | <none>  | cycle through max depth                    |
| filetree-toggle-combine-dir-names | /       | toggle combining dir/subdirs in dir name   |
| filetree-toggle-use-all-icons     | ;       | toggle use-all-icons icons (if installed)  |
|                                   | ]       | cycle right through info views on the left |
|                                   | [       | cycle left through info views on the left  |

## Filtering and Expanding
There are a number of ways to filter down the file list or to add files to the file list.  The results after each filtering or expansion operations is put on a stack and can be undone by popping off the stack using the "b" key.

| Command                             | key map | Comment                                                                     |
|-------------------------------------|---------|-----------------------------------------------------------------------------|
| filetree-pop-file-list-stack        | b       | undo prev filter/expansion operation                                        |

### Regex-based filtering/expansion and narrowing to subdir

![filetree demo filtering](screenshots/filetree_demo_filtering.gif)

| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| filetree-filter                 | f       | Regex based filter                          |
| filetree-expand-dir             | e       | Add files in directory at point             |
| filetree-expand-dir-recursively | E       | Add files in directory at point recursively |
| --                              | RET     | Return on subdir, narrows to that subdir    |

Notes:
* The filetree-toggle-combined-dir-names command (see in View mode section below) can be helpful when wanted to use filetree-expand (or filetree-expand-dir-recursively) on a directory one or more levels above a file in the file list.

### Helm-based filtering
![filetree demo views](screenshots/filetree_demo_helm_search.gif)

| Command                             | key map | Comment                                                                     |
|-------------------------------------|---------|-----------------------------------------------------------------------------|
| filetree-helm-filter                | s       | helm-based search                                                           |

### Stack operations
| Command                             | key map | Comment                                                                     |
|-------------------------------------|---------|-----------------------------------------------------------------------------|
| filetree-diff-with-file-list-stack  | -       | remove files in current file-list from list on stack and make new file-list |
| filetree-union-with-file-list-stack | +       | combine files in current file-list and list on stack into new file-list     |

Notes:
* The filetree-diff-with-file-list-stack command can be helpful for doing "complementary" filters, e.g., filtering for all files with test and then issuing the command will have the effect of removing all the files with test.

### Marking files and filtering based on marks
![filetree demo marking files](screenshots/filetree_demo_mark.gif)

| Command                      | key map | Comment                                                             |
|------------------------------|---------|---------------------------------------------------------------------|
| filetree-mark-item           | m       | toggle mark on file or add mark to all files in file-list in subdir |
| filetree-mark-all            | A       | mark all items in current filetree                                  |
| filetree-clear-marks         | c       | clear marks                                                         |
| filetree-select-marked-items | M       | make file-list all marked files                                     |
    
Marks on files are not affected by the filtering operations, so you can use the filtering tools to track down each of the files you're interested in one by one.

## Operations
| Command                      | key map | Comment                                         |
|------------------------------|---------|-------------------------------------------------|
| filetree-grep                | g       | grep over files in current list                 |
| dired                        | d       | opens a dired session at the directory at point |
| filetree-kill-marked-buffers | K       | kill all buffers associated with marked files   |
| filetree-delete-marked-files | <None>  | delete all marked files                         |
| filetree-open-marked-files   | o       | Open buffer for all marked files                |



## Save/Retrieve file list
File lists can be saved/retrieved from the file specified by filetree-saved-lists-file.  The filetree-select-file-list function uses a helm interface for selection of the file list.
| Command                   | key map | Comment                                |
|---------------------------|---------|----------------------------------------|
| filetree-select-file-list | L       | load/select previously saved file list |
| filetree-save-list        | S       | save current file list                 |
| filetree-delete-list      | D       | delete file list                       |

# File Notes
This package maintains a notes file in the file specified by filetree-notes-file (default: ~/.emacs.d/filetree-notes.org).  This is an org-mode file that can hold notes associated with any file, and those notes can be seen in a side window as the user navigates through the file tree (see [File Notes section of demo video](https://youtu.be/-KrMaLq8Bms?t=1181))

Local project specific notes files can also be used--simply create an empty file in the project directory with the name "filetree-notes-local.org" (or whatever name is set by the variable filetree-relative-notes-filename).  Files under this directory will use this file for notes instead of the main file.  The file links in the local file will be relative to the project directory.  The main org-mode file will still be used for files without a local org-mode file.

In order to go to the entry for a file (and create an entry if it doesn't exist), use the filetree-toggle-info-buffer command.  For example, you can use the following key binding in your .emacs to run the command:
```
(global-set-key (kbd "C-c <return>") (lambda ()
                                       "Toggle filetree-info-buffer and switch to it if active"
                                       (interactive)
                                       (filetree-toggle-info-buffer t)))
```
The same command will open and close (after saving) the notes buffer in the side window.

Within the filetree buffer, the "i" key will toggle the side window with the notes file.  The notes will dynamically narrow to the relevant part of the file as the user navigates the file tree and will also switch to/from the local org-mode file if applicable.  If there is no note entry for the file, then the message "No File Note Entry" will be shown in the side window.
| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| filetree-toggle-info-buffer     | i       | Toggle info buffer in side window           |
|                                 | I       | ^^ and then switch to side window if active |

One thing to keep in mind is that the note is referenced by it's absolute file name/path in the case of a global file notes file, and referenced by the relative path and filename in the case of the project specific file notes.  As a result, if a file is moved or renamed, the correspondence between the note and the file will be lost.  This can be fixed by correcting the link in the notes file (or by creating a new entry for the file and cut-and-pasting the info to the new note).  Since the project specfic notes are relative, moving the whole project will not affect the notes.

# Customizations

## Files used by filetree
| Parameter                        | default                            | Comment                                        |
|----------------------------------|------------------------------------|------------------------------------------------|
| filetree-notes-file              | ~/.emacs.d/filetree-notes.org      | File used for file notes                       |
| filetree-relative-notes-filename | "filetree-notes-local.org"         | Filename used for project specific notes files |
| filetree-saved-lists-file        | ~/.emacs.d/filetree-saved-lists.el | File used for saved file lists                 |

## Settings related to startup state
| Parameter                  | default  | Comment                                          |
|----------------------------|----------|--------------------------------------------------|
| filetree-info-window       | nil      | Set to t to show notes/info side window at start |
| filetree-use-all-the-icons | nil      | Set to t to show icons for files/dirs            |

Note enabling use-all-the-icons can make some of the operations sluggish if the file list is large.  Also, you may need to set the scaling for the icons to match the height of the text:
```
(setq all-the-icons-scale-factor 1)
```
## Additional file info configuration
The additional columns of information that can be shown on the left of the filetree are configurable.  Use M-x customize on the variable filetree-info-cycle-list for this configuration.  The screenshot below shows an example configuration.  

The sets of columns that are cycled through using "]" and "[" are called a view set.  Each view set has a set of columns and each column is specified by the column heading, the width of the column, the column justification (i.e., left/right/center), and a function that takes a filename/dirname as input and returns a string of information to display.  By writing your own functions (similar to functions like filetree-get-file-last-modified) and adding an entry with that function to filetree-info-cycle-list, you can show whatever information about a file you'd like on the left.

![filetree-info-cycle-list config](screenshots/filetree-customize-additional-columns.jpg)

## Faces, marks, and misc
The variable filetree-exclude-list is a list of regex for files to ignore.

The marks used to draw the file trees can be customized.  Here is the list of symbols that are used:
filetree-symb-for-root filetree-symb-for-box, filetree-symb-for-vertical-pipe, filetree-symb-for-horizontal-pipe, filetree-symb-for-left-elbow, filetree-symb-for-right-elbow, filetree-symb-for-branch-and-cont, filetree-symb-for-file-node.

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

