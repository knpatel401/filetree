# Filetree
Filetree is a package that provides two basic functions:

* **File tree viewer**
The viewer displays a file list as a directory tree in a special buffer.  The file list can be populated from any list of files.  There are functions to populate from a number of common sources: recentf, files in buffer-list, files in the current directory, files found recursively in the current directory.  Within the viewer, the file list can be filtered and expanded in various ways and operations can be performed on the filtered file list (e.g., grep over files in list).  Multiple file lists can be saved and retrieved between sessions.

* **File notes**
The file notes enables the user to write and display (org-mode) notes associated with individual files and directories.  The note can be displayed in a side buffer either when cycling through files in the file tree viewer or when the file is open in a buffer.  The notes are kept in a single org-mode file with a heading for each file/directory.

## File tree Viewer

### Starting and Exit Viewer
The following commands start the viewer with the corresponding file list
| Command                        | Comment                                       |
|--------------------------------|-----------------------------------------------|
| filetree-select-file-list      | select file list from perviously saved lists  |
| filetree-showRecentfFiles      | populate files from recentf-list              |
| filetree-showCurDir            | populate files from current dir               |
| filetree-showCurDirRecursively | populate files from current dir (recursively) | 
| filetree-showCurBuffers        | populate files from buffer-list               |
| filetree-showFilesWithNotes    | populate file list with files with "notes"    |
| filetree-showFiles             | populate files from file list in argument     |
| filetree-recentf-cancel-dialog | exit viewer (tied to q in keymap              |


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
| filetree-expandDir              | e       | Add files in directory at point             |
| filetree-expandDirRecursively   | E       | Add files in directory at point recursively |
| filetree-reduceListBy10         | -       |                                             |
| filetree-pop-fileListStack      | b       | undo prev filter/expansion operation        |

Notes:
* The file list is added to a stack after each filtering operations (filetree-filter, filetree-helm-filter, filetree-reduceListBy10), and the filtering operation can be undone by popping off the stack (filetree-pop-fileListStack).
* Selecting (by pressing RETURN or clicking) on a file in the Filetree buffer opens the file.  Selecting on a directory narrows to the chosen subtree.
* The filetree-toggle-combinedDirNames command can be helpful when wanted to use filetree-expand (or filetree-expandDirRecursively) on a directory one or more levels above a file in the file list.

### View modes
| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| filetree-set-maxDepth           | 0-9     | set max depth of tree to view 0=max         |
| filetree-cycle-maxDepth         | <none>  | cycle through max depth                     |
| filetree-toggle-combineDirNames | /       | toggle combining dir/subdirs in dir name    |
| filetree-toggle-flat-vs-tree    | .       | toggle between tree and flat view           |
| filetree-toggle-use-all-icons   | ;       | toggle use-all-icons icons (if installed)   |


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

In order to go to the entry for a file (and create an entry if it doesn't exist), use the filetree-toggle-info-buffer command.  For example,  I use the following key binding in my .emacs to run the command:
```
(global-set-key (kbd "M-<return>") (lambda ()
                                     "Toggle filetree-info-buffer and switch to it if active"
                                     (interactive)
                                     (filetree-toggle-info-buffer t)))
```
The same command will open and close (after saving) the notes buffer in the side window.

Within the filetree buffer, the "i" key will toggle the side window with the notes file.  The notes will dynamically narrow to the relevant part of the file as the user navigates the notes window.  If there is no note entry for the file, then the message "No File Note Entry" will be shown in the side window.
| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| filetree-toggle-info-buffer     | i       | Toggle info buffer in side window           |
|                                 | I       | ^^ and then switch to side window if active |

## Customizations





