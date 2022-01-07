# Filetree

Filetree is a package that provides a set of interactive file management tools.  The core functionality is a file tree viewer which displays a file list as a directory tree in a special buffer.  There are numerous interactive tools available to the user within this special buffer.

In addition to this file tree viewer functionality, there is also a file note taking feature in the package.  The file notes enable the user to write and display (`org-mode`) notes associated with individual files and directories. The note can be displayed in a side buffer either when cycling through files in the file tree viewer or when the file is open in a buffer.

![filetree screenshot](screenshots/filetree_screenshot.jpg)

# Summary of features of the package:

***File sources & integrations***
 * Commands for populating the file list from numerous  sources (e.g., recently opened files, files in open buffers, files in current directory, and files in current directory tree).
 * Integrations with `dired` and `eshell`&mdash; for example, calling `filetree-show-cur-dir` from an `eshell` buffer will pull up the files in the current `eshell` directory, and conversely, there's a command to start an `eshell` buffer from a directory in the filetree.
 * Saving and retrieving multiple file lists between sessions
 
***Visual contextualization***
 * Display files within directory structure, or as a flat list
 * Show tree at different depths
 * Icons and configurable faces to visually differentiate file types
 * Display additional file info (mode, size, date, etc.) in columns to the left.  Support for user-defined columns.
 * File-specific notes dynamically updated in side window
 
***Interactive filtering & file discovery tools***
 * Filter files by file type or regex (or interactively via helm-based search)
 * Stack providing undo functionality for filter operations
 * Sorting
 * User-defined custom filtering
 * Tools for adding files to original file list
 
***Actions on files***
 * Actions on single files/directories, e.g., `magit-status`, `dired`, `eshell` on directories in filetree.
 * Support for file marking
 * Commands acting on marked files, e.g., grep, move/copy/delete files, open files in buffers, shell command on files
 * Support for custom user-defined operations on marked files or on single files/directories
 
***Notes***&mdash;file-specific notes stored in single global file or local project specific file

# Getting Started
Filetree is available on [MELPA](http://melpa.org/), and can be installed with `M-x package-install` after MELPA is added to package-archives.
``` elisp
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
```

Once the filetree package is installed and enabled.  There are a few ways to bring up the filetree buffer with a particular file list:
* `M-x filetree-load-cmd-menu` will bring up a menu from which you can select a source for the filetree (e.g., *r* from this menu will bring up a filetree with recently opened files).
![filetree load menu](screenshots/filetree-load-cmd-menu.jpg)
* Call one of the filetree load commands directly (see table below)

| Command                             | File list source                                                                               |
|-------------------------------------|------------------------------------------------------------------------------------------------|
| `filetree-show-recentf-files`       | recentf-list                                                                                   |
| `filetree-show-cur-dir`             | current dir (from filetree, file buffer, eshell, or dired)                                     |
| `filetree-show-cur-dir-recursively` | current dir recursively (from filetree, file buffer, eshell, or dired)                         |
| `filetree-show-vc-dir-recursively`  | version control root of current dir recursively (from filetree, file buffer, eshell, or dired) |
| `filetree-show-cur-buffers`         | buffer-list                                                                                    |
| `filetree-show-files-with-notes`    | populate file list with files with "notes"                                                     |
| `filetree-select-file-list`         | select file list from previously saved lists                                                   |
* Execute the elisp command ```(filetree-show-files user-file-list)``` where *user-file-list* is a list of filenames with absolute paths.  This will pull up a filetree with the files in *user-file-list*.

# Keybindings & Commands
Filetree now uses [transient](https://github.com/magit/transient) to organize it's key bindings to both improve key binding scalability and organization, as well as to better facilitate feature discovery.  The best way to see what commands are available along with their key bindings is to call `M-x filetree-command-help` (tied to *h* in the keymap) which will bring up the **Help Main Menu** transient.
![filetree help menu](screenshots/filetree-help-main-menu.jpg)

Here's some additional information on the sub-menus:

| Command                       | key | Comment                                                                |
|-------------------------------|-----|------------------------------------------------------------------------|
| `filetree-view-mode-menu`     | v   | Change views (e.g., tree depth, flat vs. tree, icons, additional info) |
| `filetree-load-cmd-menu`      | l   | Load filetree from different sources                                   |
| `filetree-file-ops-menu`      | o   | Operations on single file/dir (e.g., open dired or eshell on subdir)   |
| `filetree-mark-cmd-menu`      | m   | Marking files and ops on marked files (e.g., grep, move, open)         |
| `filetree-filter`             | f   | Filtering and sorting operations                                       |
| `filetree-expand`             | e   | Add/expand filelist operations                                         |
| `filetree-expand-recursively` | E   | Add/expand filelist recursively TODO: combine with filetree-expand     |


Within the *filetree* buffer the following navigation and basic commands can be used:

| Command                  | key              | Comment                       |
|--------------------------|------------------|-------------------------------|
| `filetree-next-line`     | down, j          | down one line                 |
| `filetree-prev-line`     | up, k            | up one line                   |
| `filetree-next-branch`   | SPC              | down one branch               |
| `filetree-prev-branch`   | TAB              | up one branch                 |
| --                       | RETURN (on file) | open file                     |
| --                       | RETURN (on dir)  | narrow to dir                 |
| `filetree-remove-item`   | x                | remove file/dir from filetree |
| `filetree-close-session` | q                | exit filetree viewer          |

# Demo Video
A video demoing some of the primary functionality is on [Youtube here](https://youtu.be/-KrMaLq8Bms).  The corresponding notes are in this repo [demo_notes.org](demo_notes.org).  Note that this demo video is a bit out-of-date now, but does give an overview of some basic usages for the package.

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

These tools can be used in a number of different ways.  Some possible use cases along with alternatives are discussed below.

## Recent files use case
The filetree package can be used as a wrapper for the built-in recentf commands.  The command for using the wrapper is `filetree-show-recentf-files`.  Compared with the basic recentf commands, the filetree command provides better visual context for the recent files, and a host of tools for filtering the recent file list or finding other files that are located around a recently accessed file.  This enables users to use recent files in a more expansive way.  Users may want to increase the number of files saved from default of 20 to something higher using the following setting:
```
(setq recentf-max-saved-items 500)
```
Alternative wrappers for recentf (e.g., the [helm](https://emacs-helm.github.io/helm/) command `helm-recentf`), provide some filtering tools for recentf, but not the other features available in the filetree package.

## Current buffers use case
This package can be used as an alternative to `ibuffer` in some cases, using the command `filetree-show-cur-buffers`.  The command provides the same benefits over ibuffer as the recentf wrapper does over recentf in the previous section.

While `ibuffer` represents all buffers, `filetree-show-cur-buffers` represents files not buffers.  As a result it does not represent buffers that are not associated with files (e.g., _\*Messages\*_ or _\*scratch\*_).  Another difference is that ibuffer provides some buffer grouping tools.  In filetree there are implicit groupings via the directory structure, or via filtering or font highlighting rules, but no explicit grouping.

[bufler](https://github.com/alphapapa/bufler.el) is an alternative that provides some additional automatic buffer grouping support over ibuffer.  The differences between bufler and filetree are the same as the differences between ibuffer and filetree.

## File/Project explorer use case
The filetree package can be used as a file or project explorer, using `filetree-show-cur-dir` or `filetree-show-cur-dir-recursively` or by saving/loading a file list for a project.

There are many other alternative tree-based file/project explorers (e.g., [treemacs](https://github.com/Alexander-Miller/treemacs), [neotree](https://github.com/jaypei/emacs-neotree)).  For the use case of having a (persistent) project directory/file tree displayed in a side window, these other packages are preferable over filetree.  For example, treemacs can be configured to automatically update the tree when there are changes in the file system, it has projectile and git integration.

The project/file explorer use case for filetree is different, and comes from a couple of fundamental differences between filetree and these other file/project explorers:
* Filetree starts with the implicit assumption that some files are more important to the user than others (e.g., recent files, files currently opened in a buffer, files in the current directory, a previously saved file list)--only those files are initially displayed in the tree, and others are found through discovery (via expansion tools).
* There's an emphasis on interactive tools for project/file exploration&mdash;there are a lot of interactive tools available in filetree to navigate, filter, discover, and perform operations on/with files.

In a typical filetree use case, the filetree is pulled up for an action (e.g., finding a file, searching for something in the files in a project, visualizing the directory structure, running a script on the files), the interactive tools are used to find the desired information or file or an operation is performed on the files, and then filetree is closed.  This is different from how a conventional project/file explorer is typically used.

## Dired use case
The filetree package can do some basic file system operations that are akin to operations available in dired.  In particular, the user can filter, expand, and mark files&mdash;and then perform operations like delete files, copy/move files, run script on files, grep across the marked files, etc..  Dired can run a more extensive set of file system operations, and so filetree is not a replacement for dired.  However, an advantage of filetree is that it makes it easier to find, visualize, and mark files across different parts of the file system.  So if the desired operations are basic (i.e., copy/move/delete/grep/run script on) but the files are scattered across the file system, then filetree can be a good alternative.  

Also, you can go back and forth between a dired buffer and the filetree buffer; the key sequence *od* will open a dired buffer at the directory currently at point in the filetree, and calling `filetree-show-cur-dir` or `filetree-show-cur-dir-recursively` from a dired buffer will open the filetree buffer with files in the directory in current dired buffer.

Users can easily add their own operations acting on individual files/subdirs, marked files or the entire file list using the customization variables *filetree-custom-single-operations*, *filetree-custom-marked-file-operations*, and *filetree-custom-filelist-operations*, respectively.

## File notes use case
The file notes functionality in filetree can be used as a light weight file-specific (org-mode) note taking tool.  With the integration with the rest of the filetree package, those notes can also be displayed while navigating the filetree.  What makes filetree's note functionality useful is that it's simple and there's very little overhead&mdash;a note for any file can be visited (or automatically created) with a single key binding, and the notes are centralized in a single global org file (or project specific org files if desired).

Alternatives: [org-noter](https://github.com/weirdNox/org-noter) is an alternative package for taking notes associated with a file, and it can do some nice things like have notes sync'ed to your position in the document as you scroll.  For annotating a document this would be preferable to filetree.

# File tree viewer

## View modes
There are a number of ways to modify the file list representation and information that appears in the filetree buffer.  The gif below shows some examples.
![filetree demo views](screenshots/filetree_demo_views.gif)

Pressing "v" (`filetree-view-mode-menu`) will pull up a menu of commands to configure the view mode.
![filetree view mode menu](screenshots/filetree-view-mode-menu.jpg)

Alternatively, the following commands can also be used directly.

| Command                                 | key    | Comment                                    |
|-----------------------------------------|--------|--------------------------------------------|
| `filetree-set-max-depth`                | 0-9    | set max depth of tree to view 0=max        |
| `filetree-cycle-max-depth`              | <none> | cycle through max depth                    |
| `filetree-toggle-combine-dir-names`     | /      | toggle combining dir/subdirs in dir name   |
| `filetree-toggle-use-all-icons`         | ;      | toggle use-all-icons icons (if installed)  |
| `filetree-toggle-flat-vs-tree`          | .      | toggle between tree and flat view          |
| `filetree-increment-current-info-cycle` | ]      | cycle right through info views on the left |
| `filetree-decrement-current-info-cycle` | [      | cycle left through info views on the left  |

## Operations on single files/directories
Operations on a single file/directory can be invoked by pressing the *o* key (`filetree-file-ops-menu`)&mdash;this will open the **File Operations Menu** operation menu.  The following commands are in the menu.

| Command                     | key | Comment                                         |
|-----------------------------|-----|-------------------------------------------------|
| `dired`                     | od  | opens a dired session at the directory at point |
| `filetree-run-eshell`       | oe  | opens an eshell at the directory at point       |
| `filetree-run-magit-status` | oms | run magit-status on repo for file/dir at point  |

Users can also add their own operations by customizing *filetree-custom-single-operations*.
## Marking files
Filetree has support for marking files and performing operations on the marked files.  Pressing *m* (`filetree-mark-cmd-menu`) will pull up the **Mark Command Menu**.
![filetree mark cmd menu](screenshots/filetree-mark-menu.jpg)

This enables marking of files followed by an operation on the marked files.  When files are marked, a small green arrow is shown to the left of the filetree indicating the file is marked.  The mark command menu persists through the marking commands, and exits either when the user selects an operation (e.g., "Keep only marked") or when the user presses C-g to exit the menu.

Note that marks on files are not affected by the filtering operations, so you can use the filtering tools to track down each of the files you're interested in one by one, building up the marked file list, and then perform an operations on all marked files.

In addition to the operations that are available in the current menu, users can add their own custom operations by customizing the variable *filetree-custom-marked-file-operations* via `M-x customize`.  Each entry of this operation list specifies a shortcut key, a label, and a function to perform the operation.  The function should take a list of filenames with absolute paths as input.

## Filtering and Expanding

There are a number of ways to filter down the file list or to add files to the file list.  The results after each filtering or expansion operations is put on a stack and can be undone by popping off the stack using the *b* key.  The current filetree can also be "subtracted" from the previous filelist on the stack using the *-* key&mdash;this can be useful to do a complementary filtering (e.g., keep all but the files matching a regex).

| Command                               | key | Comment                                                                     |
|---------------------------------------|-----|-----------------------------------------------------------------------------|
| `filetree-diff-with-file-list-stack`  | -   | remove files in current file-list from list on stack and make new file-list |
| `filetree-union-with-file-list-stack` | +   | combine files in current file-list and list on stack into new file-list     |
| `filetree-pop-file-list-stack`        | b   | undo prev filter/expansion operation                                        |

The filtering and expansion operations menus can be pulled up by the following keys/commands.

| Command                       | key map | Comment                                  |
|-------------------------------|---------|------------------------------------------|
| `filetree-filter`             | f       | Filter operations                        |
| `filetree-expand`             | e       | Expansion operations                     |
| `filetree-expand-recursively` | E       | Expansion operations (recursive)         |
| --                            | RET     | Return on subdir, narrows to that subdir |

For example, pressing *f* will pull up the following filter menu.
![filetree filter menu](screenshots/filetree-filter-menu.jpg)
This allows you to filter based on regex (e.g., *p* will keep only python files) or interactively using a helm-based search.  You can also sort the list, e.g., by last modification time.

Notes:
* The result of the sort operations are only really visible in the flat view mode (see `filetree-toggle-flat-vs-tree` command)
* The `filetree-toggle-combined-dir-names` command (see in View mode section) can be helpful when wanted to use `filetree-expand` (or `filetree-expand-dir-recursively`) on a directory one or more levels above a file in the file list.

# File Notes
This package maintains a notes file in the file specified by *filetree-notes-file* (default: *~/.emacs.d/filetree-notes.org*).  This is an org-mode file that can hold notes associated with any file, and those notes can be seen in a side window as the user navigates through the file tree (see [File Notes section of demo video](https://youtu.be/-KrMaLq8Bms?t=1181))

Local project specific notes files can also be used--simply create an empty file in the project directory with the name "*filetree-notes-local.org*" (or whatever name is set by the variable *filetree-relative-notes-filename*).  Files under this directory will use this file for notes instead of the main file.  The file links in the local file will be relative to the project directory.  The main org-mode file will still be used for files without a local org-mode file.

In order to go to the entry for a file (and create an entry if it doesn't exist), use the `filetree-toggle-info-buffer` command.  For example, you can use the following key binding in your .emacs to run the command:
```
(global-set-key (kbd "C-c <return>") (lambda ()
                                       "Toggle filetree-info-buffer and switch to it if active"
                                       (interactive)
                                       (filetree-toggle-info-buffer t)))
```
The same command will open and close (after saving) the notes buffer in the side window.

Within the *filetree* buffer, the *i* key will toggle the side window with the notes file.  The notes will dynamically narrow to the relevant part of the file as the user navigates the file tree and will also switch to/from the local org-mode file if applicable.  If there is no note entry for the file, then the message "*No File Note Entry*" will be shown in the side window.

| Command                         | key map | Comment                                     |
|---------------------------------|---------|---------------------------------------------|
| `filetree-toggle-info-buffer`   | i       | Toggle info buffer in side window           |
|                                 | I       | ^^ and then switch to side window if active |

One thing to keep in mind is that the note is referenced by it's absolute file name/path in the case of a global file notes file, and referenced by the relative path and filename in the case of the project specific file notes.  As a result, if a file is moved or renamed, the correspondence between the note and the file will be lost.  This can be fixed by correcting the link in the notes file (or by creating a new entry for the file and cut-and-pasting the info to the new note).  Since the project specific notes are relative, moving the whole project will not affect the notes.

# Customizations

Customizations to filetree can be made via emacs's customize command (`M-x customize`).  Some useful customizations are discussed in the sections below.

## Files used by filetree
The files used by filetree are grouped under the *filetree-files* customization group.

| Parameter                        | default                                          | Comment                                        |
|----------------------------------|--------------------------------------------------|------------------------------------------------|
| *filetree-notes-file*              | *user-emacs-directory* + "filetree-notes.org"      | File used for file notes                       |
| *filetree-relative-notes-filename* | "filetree-notes-local.org"                       | Filename used for project specific notes files |
| *filetree-saved-lists-file*        | *user-emacs-directory* + "filetree-saved-lists.el" | File used for saved file lists                 |

## Settings related to startup state
The configurations settings used at startup are grouped under the *filetree-startup-prefs* group.

| Parameter                      | default | Comment                                                                       |
|--------------------------------|---------|-------------------------------------------------------------------------------|
| *filetree-info-window*           | nil     | Set to t to show notes/info side window at start                              |
| *filetree-use-all-the-icons*     | nil     | Set to t to show icons for files/dirs                                         |
| *filetree-show-remote-file-info* | nil     | Set to t to show additional file info for remote files as well as local files |

Note enabling use-all-the-icons can make some of the operations sluggish if the file list is large.  Also, you may need to set the scaling for the icons to match the height of the text:
```
(setq all-the-icons-scale-factor 1)
```
The variable *filetree-show-remote-file-info* is set to nil by default because determining file info (e.g., file size, mode, etc.) can be slow for remote file systems.

## Additional file info configuration
The additional columns of information that can be shown on the left of the filetree are configurable.  Use `M-x customize` on the variable *filetree-info-cycle-list* for this configuration.  The screenshot below shows an example configuration.

The sets of columns that are cycled through using *]* and *\[* are called a view set.  Each view set has a set of columns and each column is specified by the column heading, the width of the column, the column justification (i.e., left/right/center), and a function that takes a filename/dirname as input and returns a string of information to display.  By writing your own functions (similar to functions like `filetree-get-file-last-modified`) and adding an entry with that function to *filetree-info-cycle-list*, you can show whatever information about a file you'd like on the left.

There is a filetree function (`filetree-get-git-status`) available to show the git state of files in a column.  It's not included in the default for *filetree-info-cycle-list*, because for more than a few files, it can be very slow.  But users can add it via the customization dialog.

![filetree-info-cycle-list config](screenshots/filetree-customize-additional-columns.jpg)

## Filetype List
The faces used for different file types as well as the shortcuts used to filter those file types are specified by the customization *filetree-filetype-list*.  Some predefined faces are specified under the customization group *filetree-faces*.

## Custom filter operations
Users can define custom filtering operations by adding to the customization variable *filetree-custom-filelist-operations*, which is a list of operations.  Each entry in the list specifies the shortcut (under the *filetree-filter* transient), a label for the filter, and a function that takes a file list as input and outputs the filtered file list.  Added operations will show up in the **FileTree Filter Menu**.

Note that while this is listed under the filtering menu, any operation that takes the original file list as input and outputs a new file list can be added.  In particular, operations that operate on the files in the list as a side-effect are also a possible operations.  As another example, the operation could simply sort the list.

## Custom single file/dir operations
Users can define custom single file operations by adding to the customization variable *filetree-custom-single-operations*, which is a list of operations.  Each entry in the list specifies the shortcut (under the *filetree-file-ops-menu* transient), a label for the command, and a function that takes a single file/dir name as input.  Added operations will show up in the **Filetree File Operation Menu**.

## Custom marked file operations
Users can define custom operations on a list of marked files by adding to the customization variable *filetree-custom-marked-file-operations*, which is a list of operations.  Each entry in the list specifies the shortcut (under the *filetree-mark-cmd-menu* transient), a label for the command, and a function that takes a list of files as input.  Added operations will show up in the **Filetree Mark Command Menu**.

## Misc
The variable *filetree-exclude-list* is a list of regex for files to ignore.  The marks used to draw the file trees can be customized.  These can be found in the customization group *filetree-symb-for*.  The default face is specified by *filetree-default-file-face*.

# Versions
* **v1.1**
    * Transient based hierarchical key bindings & help menus (note some key bindings have changed)
    * Improved flat view (show additional info, marks)
    * Support for user-defined filters, and operations on single files/dirs and marked files
    * Sorting functionality (in flat view)
    * Customization improvements

* **v1.0x** - Initial release

<!--  LocalWords:  Filetree
 -->
