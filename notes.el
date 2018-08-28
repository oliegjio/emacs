;; EMACS KEYBINDINGS:
;; C-x C-f - Open / create a file.
;; C-x C-f C-f - Open / create a file (disables Ido mode).
;; M-! - Execute bash command and show the result.
;; C-x b - Switch buffer.
;; C-x k - Kill buffer.
;; M-x - Execute a command.
;; M-: - Eval command.
;; C-M-x - Execute current line.
;; C-x 0 - Close current window / split.
;; C-x 1 - Close all splits / windows except current.
;; C-x 2 - Split horizontally.
;; C-x 3 - Split vertically.
;; C-x 5 2 - Open new window.
;; C-x C-+ - Increase font size.
;; C-x C-- - Decrease font size.
;; C-x C-0 - Reset font size.

;; EMACS CLIPBOARD:
;; C-y - Paste.
;; C-w - Cut.
;; M-w - Copy.
;; C-Space - Set mark.

;; EMACS EDITING:
;; C-x C-; - Comment or uncomment.
;; C-S-Backspace - Kill the current line.
;; C-k - Kill to the end of the line.
;; M-d - Kill word forward.
;; M-<Backspace> - Kill word backward.
;; C-d - Delete next character.
;; C-u <Tab> - Align region. Press <Left> or <Right> to indent region.

;; EMACS MOVEMENT:
;; C-v - Scroll forward by full window.
;; M-v - Scroll backward by full window.
;; M-e - Move paragraph forward.
;; M-a - Move paragraph backward.
;; M-m - Move to first non-whitespace character on the current line.
;; C-a - Move to the begining of the line.
;; C-e - Move to the end of the line.
;; M-f - Move forward a word.
;; M-b - Move backward a word.
;; M-> - Move to the end of the buffer.
;; M-< - Move to the beginning of the buffer.

;; EMACS COMMANDS:
;; load-file - Load current file as Emacs config.
;; package-refresh-contents - Refreshes packages list.
;; term / ansi-term - Open terminal emulatior (more useable).
;; shell - Open terminal emulator in a new split (more limited).
;; list-colors-display - Show all Emacs colors.
;; tabify - Convert spaces to tabs in the region.
;; untabify - Convert tabs to spaces in the region.
;; balance-windows - Resize splits to event sizes.

;; EMACS EVAL COMMANDS:
;; buffer-file-name - Shows path for the file in current buffer.

;; WINNER-MODE:
;; C-c <Left> - Undo window changes.
;; C-c <Right> - Redo window changes.

;; DIRED:
;; C-x d - Open Dired.
;; g - Refresh Dired.
;; ^ - Move to parent directory in Dired.
;; + - Create a new directory.
;; R - Rename file of directory. Or move marked files.
;; Q - Search and replace in marked files.
;; t - Mark all files in the current directory.
;; d - Flag file for deletion.
;; u - Remove deletion flag.
;; x - Delete files flagged for deletion.

;; CLOJURE:
;; C-c M-j - Open REPL if in Clojure file.
;; C-x C-e - Executes current line of Clojure code (cursor must be in the end of the line).
;; C-c C-k - Recompile current file.

;; INTERO:
;; M-. - Jump to definition.
;; C-c C-t - Show the type of thing at point or the selection.
;; C-c C-i - Show information of identifier at point.
;; C-u C-c C-t - Insert a type signature for the thing at point.
;; C-c C-l - Load this module in the REPL.
;; C-c C-c - Evaluate the selected region in the REPL.
;; C-c C-k - Clear REPL.
;; C-c C-z - Switch to and from the REPL.

;; CUSTOM KEYBINDINGS:
;; C-<Return> - Insert line below.
;; C-S-<Return> - Insert line above.
;; C-<Up> - Move line up.
;; C-<Down> - Move line down.
;; C-c C-d - Duplicate a line.
;; C-z - Recenter.
;; S-C-<Left> - Shrink horizontally.
;; S-C-<Right> - Enlarge horizontally.
;; S-C-<Down> - Shrink vertically.
;; S-C-<Up> - Enlarge horizontally.
;; C-x C-r - Rename current file and buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; FORMATTING:
;;;;
;; *bold*
;; /italic/
;; _underlined_
;; =verbatim=
;; +strikethrough+
;; * Header 1
;; ** Header 2
;; *** Header 3
;; **** Header 4
;; ***** Header 5
;; - [ ] List item
;; * Header [/] - Show count of done / undone checkboxes.
;; * Header [%] - Show percentage of done checkboxes.
;; #+BEGIN_SRC <language>
;; #+END_SRC
;;;;
;;;; KEYS:
;;;;
;; <Tab> on header line - collapse / uncollapse current header.
;; S-<Tab> on header line - collapse / uncollapse current and all subheaders.
;; S-<Left> / S-<Right> OR C-c C-t on header line - Cycle header state (default: `TODO` and `DONE`).
;; C-c C-l - Insert a link.
;; M-<Up> - Move header up.
;; M-<Down> - Move header down.
;; M-<Left> - Decrease header level.
;; M-<Right> - Increase header level.
;; C-c C-w - Move subheader to different location.
;; C-c C-c - Check / uncheck list item.
;; M-S-<Enter> - New list item / new header.
;; C-c C-d - Add deadline to header.
;; C-c C-p - Move the cursor to the nearest header.
;; M-h - Mark the element at point.
;; C-c @ - Mark the subtree at point.
;; C-c C-x C-w - Remove the subtree at point.
;; C-x n b - Narrow buffer to current block.
;; C-x n w - Weiden buffer to remove narrowing.
;;;;
;;;; TABLES:
;;;;
;; <Tab> - Move to the next cell. Move to the next row if no more cells left on the current row.
;; S-<Tab> - Move to the previous cell.
;; M-<Enter> - Next row (stays on the same column). Creates a new row if no more left.
;; `|-` + <Tab> - Create separator.
;; M-<Up> - Move line up.
;; M-<Down> - Move line down.
;; M-<Left> - Move column left.
;; M-<Right> - Move column right.
;; M-S-<Right> - Create column on the left.
;; M-S-<Down> - Create row above.
;;;;
;;;; OPTIONS:
;;;;
;; #+STARTUP: overview - All headers are collapsed of file open.
;; #+SEQ_TODO: TODO1(t) TODO2(r) | DONE1(d) DONE2(f) - States for headers, key code in parentheses.
;; #+TITLE: The Document Title - Title for the document.
;; #+CREATOR: John Doe - Author of the document.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
