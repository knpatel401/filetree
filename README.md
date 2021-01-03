# FileTree
FileTree is a package that provides two basic functions:

* File tree viewer
The viewer displays a file list as a directory tree in a special buffer.  The file list can be populated from any list of files.  There are functions to populate from a number of common sources: recentf, files in buffer-list, files in the current directory, files found recursively in the current directory.  Within the viewer, the file list can be filtered and expanded in various ways and operations can be performed on the filtered file list (e.g., grep over files in list), and multiple file lists can be saved and retrieved between sessions.

* File notes
The file notes enables the user to write and display (org-mode) notes associated with individual files and directories.  The note can be displayed in a side buffer either when cycling through files in the file tree viewer or when the file is open in a buffer.  The notes are kept in a single org-mode file with a heading for each file/directory.

## File Tree Viewer

### Starting and Exit Viewer
The following commands start the viewer with the corresponding file list
| Command                        | Comment                                       |
|--------------------------------|-----------------------------------------------|
| fileTree-select-file-list      | select file list from perviously saved lists  |
| fileTree-save-list             | save current file list                        |
| fileTree-delete-list           | delete a previosly saved file list            |
| fileTree-showRecentfFiles      | populate files from recentf-list              |
| fileTree-showCurDir            | populate files from current dir               |
| fileTree-showCurDirRecursively | populate files from current dir (recursively) | 
| fileTree-showCurBuffers        | populate files from buffer-list               |
| fileTree-showFiles             | populate files from file list in argument     |
| fileTree-recentf-cancel-dialog | exit viewer (tied to q in keymap              |


### Navigation
Within the *FileTree* window the following navigation commands can be used
| Command              | key map     | Comment         |
|----------------------|-------------|-----------------|
| fileTree-next-line   | down, j     |                 | 
| fileTree-prev-line   | up, k       |                 | 
| fileTree-next-branch | SPC         |                 | 
| fileTree-prev-branch | TAB         |                 |

### Filtering, Expanding and View modes
| Command                       | key map | Comment                                     |
|-------------------------------|---------|---------------------------------------------|
| fileTree-filter                 | f       | <ADD>                                       |
| fileTree-helm-filter            | s       | helm-based search                           |
| fileTree-expandDir              | e       | Add files in directory at point             |
| fileTree-expandDirRecursively   | E       | Add files in directory at point recursively |
| fileTree-reduceListBy10         | -       |                                             |
| fileTree-pop-fileListStack      | b       | undo prev filter/expansion operation        |
| fileTree-set-maxDepth           | 0-9     | set max depth of tree to view 0=max         |
| fileTree-cycle-maxDepth         | <none>  | cycle through max depth                     |
| fileTree-toggle-combineDirNames | /       | toggle combining dir/subdirs in dir name    |
| fileTree-toggle-flat-vs-tree    | .       | toggle between tree and flat view           |

Notes:
* The file list is added to a stack after each filtering operations (fileTree-filter, fileTree-helm-filter, fileTree-reduceListBy10), and the filtering operation can be undone by popping off the stack (fileTree-pop-fileListStack).
* Selecting (by pressing RETURN or clicking) on a file in the FileTree buffer opens the file.  Selecting on a directory narrows to the chosen subtree.
* The fileTree-toggle-combinedDirNames command can be helpful when wanted to use fileTree-expand (or fileTree-expandDirRecursively) on a directory one or more levels above a file in the file list.


### Operations
| Command       | key map | Comment                                         |
|---------------|---------|-------------------------------------------------|
| fileTree-grep | g       | grep over files in current list                 |
| dired         | d       | opens a dired session at the directory at point |

### Customizations


## File Notes





