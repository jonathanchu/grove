;;; grove-graph.el --- Graphviz/Mermaid-based graph view for grove -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Graph view for grove using Graphviz or Mermaid.  Extracts wikilinks via ripgrep,
;; generates a DOT or Mermaid graph, and renders it as SVG for display in Emacs.

;;; Code:

(require 'grove-core)
(require 'image)
(require 'image-mode)
(require 'json)

;;;; Customization

(defcustom grove-graph-renderer 'dot
  "The underlying engine used to render the graph.
Valid options are `dot' for Graphviz or `mmdr' for mermaid-rs-renderer."
  :type '(choice (const dot)
                 (const mmdr))
  :group 'grove)

(defcustom grove-graph-executable "dot"
  "Path to the Graphviz dot executable."
  :type 'string
  :group 'grove)

(defcustom grove-graph-mmdr-executable "mmdr"
  "Path to the mmdr executable."
  :type 'string
  :group 'grove)

(defcustom grove-graph-layout "neato"
  "Graphviz layout engine.
Common options: \"dot\" (hierarchical), \"neato\" (force-directed),
\"fdp\" (force-directed), \"sfdp\" (scalable force-directed)."
  :type '(choice (const "dot")
                 (const "neato")
                 (const "fdp")
                 (const "sfdp"))
  :group 'grove)

(defcustom grove-graph-mmdr-direction "TD"
  "Graph direction when using the mmdr renderer (ie TD, LR, RL, BT)."
  :type 'string
  :group 'grove)

(defcustom grove-graph-node-color "#89b4fa"
  "Fill color for graph nodes."
  :type 'string
  :group 'grove)

(defcustom grove-graph-edge-color "#585b70"
  "Color for graph edges."
  :type 'string
  :group 'grove)

(defcustom grove-graph-bg-color "#1e1e2e"
  "Background color for the graph."
  :type 'string
  :group 'grove)

(defcustom grove-graph-display 'auto
  "How to display the graph buffer.
`auto' uses a right side window when the frame is wide enough,
otherwise opens a full buffer.  `side' always uses a right side
window.  `buffer' always opens a full buffer."
  :type '(choice (const :tag "Auto (side if wide enough)" auto)
                 (const :tag "Right side window" side)
                 (const :tag "Full buffer" buffer))
  :group 'grove)

(defcustom grove-graph-min-width 160
  "Minimum frame width (in columns) for auto side window display."
  :type 'integer
  :group 'grove)

;;;; Build adjacency list

(defun grove-graph--adjacency-list ()
  "Build an adjacency list from wikilinks in the vault via ripgrep.
Returns an alist of (SOURCE-TITLE . (TARGET-TITLE ...))."
  (grove--ensure-directory)
  (grove--refresh-cache)
  (let ((adjacency (make-hash-table :test #'equal))
        (all-titles (make-hash-table :test #'equal)))
    ;; Collect all note titles
    (maphash (lambda (_path meta)
               (puthash (plist-get meta :title) t all-titles))
             grove--cache)
    ;; Build edges from cached link data
    (maphash (lambda (_path meta)
               (let ((source (plist-get meta :title))
                     (links (plist-get meta :links)))
                 (dolist (target links)
                   (when (gethash target all-titles)
                     (push target (gethash source adjacency))))))
             grove--cache)
    ;; Convert to alist and include isolated nodes
    (let (result)
      (maphash (lambda (title _)
                 (push (cons title (gethash title adjacency)) result))
               all-titles)
      result)))

;;;; Generate Markup

(defun grove-graph--dot-escape (str)
  "Escape STR for use in a DOT label."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun grove-graph--mermaid-escape (str)
  "Escape STR for use in a Mermaid node label."
  (replace-regexp-in-string "\"" "&quot;" str))

(defun grove-graph--generate-dot (adjacency)
  "Generate a DOT graph string from ADJACENCY list."
  (let ((node-id (make-hash-table :test #'equal))
        (counter 0))
    ;; Assign IDs
    (dolist (entry adjacency)
      (unless (gethash (car entry) node-id)
        (puthash (car entry) (format "n%d" counter) node-id)
        (cl-incf counter)))
    (with-temp-buffer
      (insert "graph vault {\n")
      (insert (format "  bgcolor=\"transparent\";\n")) ;; rely on face remap
      (insert "  overlap=false;\n")
      (insert "  splines=true;\n")
      (insert (format "  node [shape=box style=\"filled,rounded\" fillcolor=\"%s\" "
                      grove-graph-node-color))
      (insert "fontcolor=\"#1e1e2e\" fontname=\"sans-serif\" fontsize=11];\n")
      (insert (format "  edge [color=\"%s\"];\n" grove-graph-edge-color))
      ;; Nodes
      (dolist (entry adjacency)
        (let ((id (gethash (car entry) node-id))
              (label (grove-graph--dot-escape (car entry))))
          (insert (format "  %s [label=\"%s\"];\n" id label))))
      ;; Edges
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (entry adjacency)
          (let ((source-id (gethash (car entry) node-id)))
            (dolist (target (cdr entry))
              (let* ((target-id (gethash target node-id))
                     (edge-key (if (string< source-id target-id)
                                   (concat source-id "--" target-id)
                                 (concat target-id "--" source-id))))
                (when (and target-id (not (gethash edge-key seen)))
                  (puthash edge-key t seen)
                  (insert (format "  %s -- %s;\n" source-id target-id))))))))
      (insert "}\n")
      (buffer-string))))

(defun grove-graph--generate-mermaid (adjacency)
  "Generate a Mermaid graph string from ADJACENCY list."
  (let ((node-id (make-hash-table :test #'equal))
        (counter 0))
    (dolist (entry adjacency)
      (unless (gethash (car entry) node-id)
        (puthash (car entry) (format "n%d" counter) node-id)
        (cl-incf counter)))
    (with-temp-buffer
      (insert (format "graph %s;\n" grove-graph-mmdr-direction))
      (dolist (entry adjacency)
        (let ((id (gethash (car entry) node-id))
              (label (grove-graph--mermaid-escape (car entry))))
          (insert (format "  %s[\"%s\"];\n" id label))))
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (entry adjacency)
          (let ((source-id (gethash (car entry) node-id)))
            (dolist (target (cdr entry))
              (let* ((target-id (gethash target node-id))
                     (edge-key (if (string< source-id target-id)
                                   (concat source-id "---" target-id)
                                 (concat target-id "---" source-id))))
                (when (and target-id (not (gethash edge-key seen)))
                  (puthash edge-key t seen)
                  (insert (format "  %s --- %s;\n" source-id target-id))))))))
      (buffer-string))))

;;;; Render

(defun grove-graph--render-svg (dot-string)
  "Render DOT-STRING to SVG using Graphviz.  Returns the SVG string."
  (unless (executable-find grove-graph-executable)
    (user-error "Graphviz not found.  Install it and ensure `%s' is on your PATH"
                grove-graph-executable))
  (with-temp-buffer
    (let ((exit-code
           (call-process-region dot-string nil
                                grove-graph-executable
                                nil t nil
                                (format "-K%s" grove-graph-layout)
                                "-Tsvg")))
      (unless (zerop exit-code)
        (user-error "Graphviz failed (exit %d): %s" exit-code (buffer-string)))
      (buffer-string))))

(defun grove-graph--render-mmdr-svg (mermaid-string)
  "Render MERMAID-STRING to SVG using mmdr.  Returns the SVG string."
  (unless (executable-find grove-graph-mmdr-executable)
    (user-error "mmdr not found.  Install it and ensure `%s' is on your PATH"
                grove-graph-mmdr-executable))
  ;; Generate the JSON styling payload to align with Emacs customs
  (let* ((config-alist
          `((theme . "base")
            (themeVariables . ((background . "transparent")
                               (primaryColor . ,grove-graph-node-color)
                               (lineColor . ,grove-graph-edge-color)
                               (primaryTextColor . "#1e1e2e")
                               (nodeBorder . ,grove-graph-edge-color)))))
         (config-json (json-encode config-alist))
         (config-file (make-temp-file "grove-mmdr-config-" nil ".json"))
         (input-file (make-temp-file "grove-mmdr-in-" nil ".mmd"))
         (output-file (make-temp-file "grove-mmdr-out-" nil ".svg")))
    (unwind-protect
        (progn
          (with-temp-file config-file (insert config-json))
          (with-temp-file input-file (insert mermaid-string))
          (let ((exit-code (call-process grove-graph-mmdr-executable nil nil nil
                                         "-i" input-file
                                         "-o" output-file
                                         "-c" config-file)))
            (unless (zerop exit-code)
              (user-error "mmdr failed (exit %d)" exit-code))
            (with-temp-buffer
              (insert-file-contents output-file)
              (buffer-string))))
      ;; Cleanup temp files
      (ignore-errors
        (delete-file config-file)
        (delete-file input-file)
        (delete-file output-file)))))

;;;; Mode

(defvar-local grove-graph--scale 1.0
  "Current zoom multiplier for the graph.")

(defvar-local grove-graph--raw-svg nil
  "Stores the raw SVG string to allow dynamic resizing without re-rendering.")

(defun grove-graph--adjust-svg-dimensions (svg-string width height)
  "Modifies the raw SVG string to scale cleanly to the window size.
Replaces hardcoded bounds with the window's dimensions and enforces aspect ratio preservation."
  (if (string-match "<svg\\([^>]*?\\)>" svg-string)
      (let* ((attrs (match-string 1 svg-string))
             ;; Strip out existing width and height declarations from the root tag
             (clean-attrs (replace-regexp-in-string "[ \t\n\r]*\\(?:width\\|height\\)=\"[^\"]*\"" "" attrs)))
        ;; Inject our pixel dimensions and aspect ratio command, keeping the original viewBox intact
        (replace-match (format "<svg width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMidYMid meet\"%s>"
                               width height clean-attrs)
                       t t svg-string))
    svg-string))

(defun grove-graph--update-display (&rest _)
  "Redraws the SVG, keeping it perfectly centred at any scale factor.
Safely executes even when triggered from a completely different buffer."
  (let ((graph-buf (get-buffer "*grove-graph*")))
    (when graph-buf
      (with-current-buffer graph-buf
        (let ((win (get-buffer-window graph-buf t)))
          (when (and win grove-graph--raw-svg)
            (let* ((win-width (window-body-width win t))
                   (win-height (window-body-height win t))
                   (scaled-svg (grove-graph--adjust-svg-dimensions 
                                grove-graph--raw-svg win-width win-height))
                   (inhibit-read-only t))
              (erase-buffer)

              (let* ((img (create-image scaled-svg 'svg t :scale grove-graph--scale))
                     (img-width (truncate (* win-width grove-graph--scale)))
                     (img-height (truncate (* win-height grove-graph--scale)))
                     
                     ;; Calculate once / + = scrol / - = pad
                     (dx (truncate (/ (- img-width win-width) 2.0)))
                     (dy (truncate (/ (- img-height win-height) 2.0)))
                     
                     (pad-x (max 0 (- dx)))
                     (pad-y (max 0 (- dy))))

                (when (> pad-y 0)
                  (insert (propertize " " 'display `(space :height (,pad-y))) "\n"))
                (when (> pad-x 0)
                  (insert (propertize " " 'display `(space :width (,pad-x)))))

                (insert-image img)
                (goto-char (point-min))

                ;; positive delta for scrollin
                (set-window-hscroll win (truncate (/ (max 0 dx) (frame-char-width))))
                (set-window-vscroll win (max 0 dy) t)))))))))

(defun grove-graph-zoom-in ()
  "Zoom in on the graph, maintaining centre alignment."
  (interactive)
  (setq grove-graph--scale (* grove-graph--scale 1.2))
  (grove-graph--update-display))

(defun grove-graph-zoom-out ()
  "Zoom out of the graph, maintaining centre alignment."
  (interactive)
  (setq grove-graph--scale (/ grove-graph--scale 1.2))
  (grove-graph--update-display))

(defun grove-graph-zoom-reset ()
  "Reset the graph zoom to perfectly fit the window."
  (interactive)
  (setq grove-graph--scale 1.0)
  (grove-graph--update-display))

(defvar-keymap grove-graph-mode-map
  :parent special-mode-map
  "+" #'grove-graph-zoom-in
  "-" #'grove-graph-zoom-out
  "0" #'grove-graph-zoom-reset)

(define-derived-mode grove-graph-mode special-mode "Grove-Graph"
  "Major mode for viewing the grove graph."
  :interactive nil
  ;; Set the buffer's background face to match the graph's background.
  ;; This seamlessly camouflages any empty padding added when preserving the aspect ratio.
  (setq-local cursor-type nil)
  (face-remap-add-relative 'default :background grove-graph-bg-color)

  (add-hook 'window-size-change-functions #'grove-graph--update-display nil))

;;;; Command

;;;###autoload
(defun grove-graph ()
  "Display a graph of notes and their links in the vault."
  (interactive)
  (grove--ensure-directory)
  (message "Building graph...")
  (let* ((adjacency (grove-graph--adjacency-list))
         (markup (if (eq grove-graph-renderer 'mmdr)
                     (grove-graph--generate-mermaid adjacency)
                   (grove-graph--generate-dot adjacency)))
         (svg (if (eq grove-graph-renderer 'mmdr)
                  (grove-graph--render-mmdr-svg markup)
                (grove-graph--render-svg markup)))
         (buf (get-buffer-create "*grove-graph*")))
    
    (with-current-buffer buf
      ;; Initialise the mode first so buffer-local variables and hooks are set
      (grove-graph-mode) 
      (setq-local grove-graph--raw-svg svg)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    
    ;; Place the buffer in a window
    (grove-graph--display buf)
    
    ;; Now that the buffer has an active window, calculate dimensions and draw
    (with-current-buffer buf
      (grove-graph--update-display))
    
    (message "Graph: %d notes, %d links"
             (length adjacency)
             (cl-reduce #'+ (mapcar (lambda (e) (length (cdr e))) adjacency)))))

(defun grove-graph--use-side-window-p ()
  "Return non-nil if the graph should display in a side window."
  (pcase grove-graph-display
    ('side t)
    ('buffer nil)
    ('auto (>= (frame-width) grove-graph-min-width))))

(defun grove-graph--display (buf)
  "Display graph buffer BUF according to `grove-graph-display'."
  (if (grove-graph--use-side-window-p)
      (display-buffer-in-side-window
       buf
       '((side . right)
         (slot . 0)
         (window-width . 0.4)
         (window-parameters
          . ((no-delete-other-windows . t)))))
    (switch-to-buffer buf)))

(provide 'grove-graph)
;;; grove-graph.el ends here
