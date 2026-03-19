;;; arrow-lang.el --- Org Babel support for the Arrow flowchart language  -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Usage:
;;   1. (load-file "/path/to/arrow-lang.el")
;;   2. In any org file:
;;
;;      #+begin_src arrow
;;      D > E /Interaction1 > F > (G, G > X /Interaction2) > End
;;      Interaction1 > Interaction2
;;      #+end_src
;;
;;   3. C-c C-c renders to *arrow* buffer and scaffolds named src blocks:
;;        - Nodes without a matching #+name: block get a blank Python stub
;;          inserted after the arrow block (within the same org section).
;;; arrow-lang.el --- Org Babel support for the Arrow flowchart language  -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Usage:
;;   1. (load-file "/path/to/arrow-lang.el")
;;   2. In any org file:
;;
;;      #+begin_src arrow
;;      D > E /Interaction1 > F > (G, G > X /Interaction2) > End
;;      Interaction1 > Interaction2
;;      #+end_src
;;
;;   3. C-c C-c renders to *arrow* buffer and scaffolds named src blocks:
;;        - Nodes without a matching #+name: block get a blank Python stub
;;          inserted after the arrow block (within the same org section).
;;        - Blocks already present are never overwritten.
;;        - Blocks in the section whose names are not in the graph are
;;          flagged as potential renames; for each one you are prompted
;;          (y/n/s=skip all) to rename it to the best-matching missing node.
;;
;; Syntax:
;;   A > B                 sequence
;;   (A, B)                parallel fork
;;   A > B : label         labeled transition into B
;;   A > B / Fac           facilitator on transition into B
;;   A > B / Fac : label   either order
;;   flow ; flow           independent tracks
;;   name := flow          module definition (lowercase)
;;   Node Name             plain node (begins uppercase)
;;   A > B >? C            or-join fork
;;   -- comment            ignored
;;   A > B  (on own line, all nodes already known) = secondary arrow
;;          Secondary arrows are functional: the target node receives
;;          input as a dict keyed by source node names.  Fork-join
;;          targets also receive dicts keyed by branch terminal names.
;;   A > B* > C          parallel map: apply B to each element of A's output list
;;
;; Header arguments:
;;   :workers N           max concurrent map workers (default 6)
;;   :lenient yes         continue on error in map/fork — failed elements
;;                        become None instead of aborting the pipeline

;;; ─── Pre-processing ──────────────────────────────────────────────────────────

(defun arrow--strip-comments (src)
  (mapconcat
   (lambda (line)
     (if (string-match "--" line)
         (substring line 0 (match-beginning 0))
       line))
   (split-string src "\n") "\n"))

;;; ─── Tokeniser ───────────────────────────────────────────────────────────────

(defun arrow--tokenise (src)
  (let ((tokens '()) (i 0) (len (length src)))
    (while (< i len)
      (let ((ch (aref src i)))
        (cond
         ((memq ch '(?\s ?\t ?\n ?\r)) (setq i (1+ i)))
         ((and (eq ch ?:) (< (1+ i) len) (eq (aref src (1+ i)) ?=))
          (push '(define) tokens) (setq i (+ i 2)))
         ((and (eq ch ?>) (< (1+ i) len) (eq (aref src (1+ i)) ??))
          (push '(arrowq) tokens) (setq i (+ i 2)))
         ((eq ch ?>)  (push '(arrow)  tokens) (setq i (1+ i)))
         ((eq ch ?,)  (push '(comma)  tokens) (setq i (1+ i)))
         ((eq ch ?\() (push '(lparen) tokens) (setq i (1+ i)))
         ((eq ch ?\)) (push '(rparen) tokens) (setq i (1+ i)))
         ((eq ch ?\;) (push '(semi)   tokens) (setq i (1+ i)))
         ((eq ch ?/)  (push '(slash)  tokens) (setq i (1+ i)))
         ((eq ch ?:)  (push '(colon)  tokens) (setq i (1+ i)))
         ((eq ch ?*)  (push '(star)   tokens) (setq i (1+ i)))
         ((eq ch ?#)  (push '(hash)   tokens) (setq i (1+ i)))
         ((eq ch ?!)  (push '(bang)   tokens) (setq i (1+ i)))
         ((or (and (>= ch ?A) (<= ch ?Z))
              (and (>= ch ?a) (<= ch ?z))
              (eq ch ?_))
          (let ((start i))
            (while (and (< i len)
                        (let ((c (aref src i)))
                          (or (and (>= c ?A) (<= c ?Z))
                              (and (>= c ?a) (<= c ?z))
                              (and (>= c ?0) (<= c ?9))
                              (eq c ?_)
                              ;; Allow dot for Module.Node references
                              (eq c ?.)
                              (and (eq c ?\s) (< (1+ i) len)
                                   (let ((nc (aref src (1+ i))))
                                     (or (and (>= nc ?A) (<= nc ?Z))
                                         (and (>= nc ?a) (<= nc ?z))))))))
              (setq i (1+ i)))
            (push `(word ,(string-trim (substring src start i))) tokens)))
         (t (setq i (1+ i))))))
    (nreverse tokens)))

;;; ─── Parser ──────────────────────────────────────────────────────────────────

(defvar arrow--tokens nil)
(defvar arrow--pos    nil)

(defun arrow--peek  () (nth arrow--pos arrow--tokens))
(defun arrow--peek2 () (nth (1+ arrow--pos) arrow--tokens))
(defun arrow--consume ()
  (let ((tok (nth arrow--pos arrow--tokens)))
    (setq arrow--pos (1+ arrow--pos)) tok))
(defun arrow--expect (type)
  (let ((tok (arrow--consume)))
    (unless (eq (car tok) type)
      (error "Arrow parse error: expected %s, got %s" type tok))
    tok))

(defun arrow--parse-program (tokens)
  "Parse TOKENS into (defs spine-flow secondary-arrows).
   Collects ALL top-level flows, classifies them after parsing."
  (setq arrow--tokens tokens arrow--pos 0)
  (let (defs all-flows)
    (while (arrow--peek)
      (if (and (eq (car (arrow--peek)) 'word)
               (eq (car (arrow--peek2)) 'define))
          (let ((name (cadr (arrow--consume))))
            (arrow--consume)
            (push (cons name (arrow--parse-flow)) defs))
        (push (arrow--parse-track-line) all-flows)))
    (list (nreverse defs) (nreverse all-flows))))

(defun arrow--parse-track-line ()
  (let ((flows (list (arrow--parse-flow))))
    (while (and (arrow--peek) (eq (car (arrow--peek)) 'semi))
      (arrow--consume)
      (push (arrow--parse-flow) flows))
    (if (= (length flows) 1) (car flows)
      `(tracks ,@(nreverse flows)))))

(defun arrow--parse-flow ()
  (let ((steps (list (arrow--parse-step))))
    (while (and (arrow--peek)
                (memq (car (arrow--peek)) '(arrow arrowq)))
      (let* ((tok-type (car (arrow--consume)))
             (next     (arrow--parse-step)))
        (push (if (and (eq tok-type 'arrowq) (eq (car next) 'fork))
                  `(fork-or ,@(cdr next))
                next)
              steps)))
    (if (= (length steps) 1) (car steps)
      `(seq ,@(nreverse steps)))))

(defun arrow--parse-step ()
  (cond
   ((eq (car (arrow--peek)) 'lparen) (arrow--parse-group))
   ((eq (car (arrow--peek)) 'word)   (arrow--parse-annotated))
   (t (error "Arrow parse error: unexpected token %s" (arrow--peek)))))

(defun arrow--parse-annotated ()
  (let* ((name (cadr (arrow--consume))) label fac map-p cache-p force-p)
    (dotimes (_ 2)
      (cond
       ((and (arrow--peek) (eq (car (arrow--peek)) 'colon) (null label))
        (arrow--consume)
        (setq label (cadr (arrow--expect 'word))))
       ((and (arrow--peek) (eq (car (arrow--peek)) 'slash) (null fac))
        (arrow--consume)
        (setq fac (cadr (arrow--expect 'word))))))
    (when (and (arrow--peek) (eq (car (arrow--peek)) 'star))
      (arrow--consume)
      (setq map-p t))
    ;; # = cache, ! = force-rerun (mutually exclusive; last wins)
    (while (and (arrow--peek) (memq (car (arrow--peek)) '(hash bang)))
      (if (eq (car (arrow--consume)) 'hash)
          (setq cache-p t force-p nil)
        (setq force-p t cache-p nil)))
    (let ((flags `(,@(when label `(:label ,label))
                   ,@(when fac   `(:fac   ,fac))
                   ,@(when cache-p '(:cache t))
                   ,@(when force-p '(:force t)))))
      (if map-p
          `(node-map ,name ,@flags)
        `(node ,name ,@flags)))))

(defun arrow--parse-group ()
  (arrow--expect 'lparen)
  (let ((branches (list (arrow--parse-flow))))
    (while (and (arrow--peek) (eq (car (arrow--peek)) 'comma))
      (arrow--consume)
      (push (arrow--parse-flow) branches))
    (arrow--expect 'rparen)
    `(fork ,@(nreverse branches))))

;;; ─── Node helpers ────────────────────────────────────────────────────────────

(defun arrow--node-name  (n) (cadr n))
(defun arrow--node-label (n) (plist-get (cddr n) :label))
(defun arrow--node-fac   (n) (plist-get (cddr n) :fac))

;;; ─── Terminal node name (last node in a branch) ────────────────────────────

(defun arrow--terminal-node-name (ast)
  "Return the name of the last (terminal) node in AST.
For a bare node, that's its name.  For a seq, it's the terminal of
the last step.  For a fork, returns nil (ambiguous)."
  (pcase ast
    (`(node ,name . ,_)     name)
    (`(node-map ,name . ,_) name)
    (`(seq . ,steps)        (arrow--terminal-node-name (car (last steps))))
    (_ nil)))

;;; ─── Collect all node/fac names from an AST ─────────────────────────────────

(defun arrow--collect-names (ast)
  "Return list of all node and facilitator names in AST."
  (pcase ast
    (`(node ,name . ,rest)
     (let ((names (list name))
           (fac (plist-get rest :fac)))
       (when fac (push fac names))
       names))
    (`(node-map ,name . ,rest)
     (let ((names (list name))
           (fac (plist-get rest :fac)))
       (when fac (push fac names))
       names))
    (`(seq . ,steps)   (apply #'append (mapcar #'arrow--collect-names steps)))
    (`(fork . ,bs)     (apply #'append (mapcar #'arrow--collect-names bs)))
    (`(fork-or . ,bs)  (apply #'append (mapcar #'arrow--collect-names bs)))
    (`(tracks . ,fs)   (apply #'append (mapcar #'arrow--collect-names fs)))
    (_ '())))

;;; ─── Module expansion ────────────────────────────────────────────────────────

(defun arrow--apply-flags (ast flags)
  "Recursively apply FLAGS (plist like :cache t :force t) to all leaf nodes in AST.
Flags already present on a leaf node are not overwritten."
  (if (null flags) ast
    (pcase ast
      (`(node ,name . ,rest)
       (let ((merged rest))
         (cl-loop for (k v) on flags by #'cddr
                  unless (plist-get rest k)
                  do (setq merged (plist-put merged k v)))
         `(node ,name ,@merged)))
      (`(node-map ,name . ,rest)
       (let ((merged rest))
         (cl-loop for (k v) on flags by #'cddr
                  unless (plist-get rest k)
                  do (setq merged (plist-put merged k v)))
         `(node-map ,name ,@merged)))
      (`(seq . ,steps)
       `(seq ,@(mapcar (lambda (s) (arrow--apply-flags s flags)) steps)))
      (`(fork . ,bs)
       `(fork ,@(mapcar (lambda (b) (arrow--apply-flags b flags)) bs)))
      (`(fork-or . ,bs)
       `(fork-or ,@(mapcar (lambda (b) (arrow--apply-flags b flags)) bs)))
      (`(tracks . ,fs)
       `(tracks ,@(mapcar (lambda (f) (arrow--apply-flags f flags)) fs)))
      (_ ast))))

(defun arrow--expand (ast defs)
  (pcase ast
    (`(node ,name . ,rest)
     (let ((def (assoc name defs)))
       (if def
           (let* ((expanded (arrow--expand (cdr def) defs))
                  ;; Propagate flags from the reference site to all leaves
                  (flags (cl-loop for (k v) on rest by #'cddr
                                  when (memq k '(:cache :force))
                                  append (list k v))))
             (arrow--apply-flags expanded flags))
         ast)))
    (`(node-map . ,_) ast)
    (`(seq . ,steps)  `(seq    ,@(mapcar (lambda (s) (arrow--expand s defs)) steps)))
    (`(fork . ,bs)    `(fork   ,@(mapcar (lambda (b) (arrow--expand b defs)) bs)))
    (`(fork-or . ,bs) `(fork-or ,@(mapcar (lambda (b) (arrow--expand b defs)) bs)))
    (`(tracks . ,fs)  `(tracks ,@(mapcar (lambda (f) (arrow--expand f defs)) fs)))
    (_ ast)))

;;; ─── Classify secondary arrows ───────────────────────────────────────────────

(defun arrow--secondary-p (flow known-names)
  "Return t if every node/fac name in FLOW is already in KNOWN-NAMES.
Dotted names like Module.Node are considered known if Module is known."
  (let ((names (arrow--collect-names flow)))
    (and names
         (cl-every (lambda (n)
                     (or (member n known-names)
                         ;; Dotted reference: Module.Node — check if Module is known
                         (and (string-match-p "\\." n)
                              (member (car (split-string n "\\.")) known-names))))
                   names))))

(defun arrow--extract-secondary-edges (flow)
  "Return list of (from-name to-name) pairs from a simple secondary flow.
   Handles seq of nodes only — secondary arrows must be simple chains."
  (pcase flow
    (`(node ,name . ,_) (list name))
    (`(seq . ,steps)
     (let ((names (mapcar (lambda (s)
                            (pcase s
                              (`(node ,n . ,_) n)
                              (_ nil)))
                          steps)))
       (if (cl-every #'identity names)
           (let (edges)
             (let ((prev (car names)))
               (dolist (n (cdr names))
                 (push (list prev n) edges)
                 (setq prev n)))
             (nreverse edges))
         '())))
    (_ '())))

;;; ─── Primary renderer ────────────────────────────────────────────────────────

(defun arrow--annotation-lines (node indent)
  (let ((label (arrow--node-label node))
        (fac   (arrow--node-fac   node))
        (pad   (make-string indent ?\s))
        lines)
    (when label (push (concat pad "  │ " label) lines))
    (when fac   (push (concat pad "  │ [" fac "]") lines))
    (nreverse lines)))

(defvar arrow--render-defs nil
  "Module definitions available during rendering.
Bound dynamically by arrow--render-src so arrow--render can expand node-map.")

(defvar arrow--render-module-sec-edges nil
  "Hash table: module-name -> list of (src tgt) secondary edges for rendering.
Bound dynamically by arrow--render-src.")

(defun arrow--render (ast indent)
  "Render AST to list of strings."
  (pcase ast

    (`(node . ,_)
     (let ((suffix (cond ((plist-get (cddr ast) :cache) "#")
                         ((plist-get (cddr ast) :force) "!")
                         (t ""))))
       (list (concat (make-string indent ?\s) "[ " (arrow--node-name ast) suffix " ]"))))

    (`(node-map ,name . ,rest)
     (let* ((suffix (cond ((plist-get rest :cache) "#")
                          ((plist-get rest :force) "!")
                          (t "")))
            (mod-def (and arrow--render-defs
                         (assoc name arrow--render-defs))))
       (if (null mod-def)
           ;; No module definition — render as opaque box
           (list (concat (make-string indent ?\s) "[ " name "*" suffix " ]"))
         ;; Expand and render the module sub-pipeline
         (let* ((expanded (arrow--expand (cdr mod-def) arrow--render-defs))
                (header (concat (make-string indent ?\s) "[ " name "*" suffix " ]"))
                (sub-lines (arrow--render expanded (+ indent 2)))
                ;; Apply module-internal secondary arrows if any
                (mod-sec (and arrow--render-module-sec-edges
                              (gethash name arrow--render-module-sec-edges)))
                (sub-lines (if mod-sec
                               (arrow--apply-secondary-arrows sub-lines mod-sec)
                             sub-lines)))
           (append (list header) sub-lines)))))

    (`(seq . ,steps)
     (let (result (prev nil))
       (dolist (step steps)
         (let* ((rejoining  (and prev (eq (car prev) 'fork)))
                (step-lines (arrow--render step indent)))
           (when result
             (cond
              ((eq (car step) 'fork)
               (setq result (append result
                                    (list (concat (make-string indent ?\s) "  │")))))
              (rejoining
               (setq result (append result
                                    (list (concat (make-string indent ?\s) "  │"))))
               (when (eq (car step) 'node)
                 (setq result (append result (arrow--annotation-lines step indent))))
               (setq result (append result
                                    (list (concat (make-string indent ?\s) "  ▼")))))
              (t
               (setq result (append result
                                    (list (concat (make-string indent ?\s) "  │"))))
               (when (eq (car step) 'node)
                 (setq result (append result (arrow--annotation-lines step indent))))
               (setq result (append result
                                    (list (concat (make-string indent ?\s) "  ▼")))))))
           (setq result (append result step-lines))
           (setq prev step)))
       result))

    (`(fork-or . ,branches) (arrow--render `(fork ,@branches) indent))

    (`(fork . ,branches)
     (let (result (n (length branches)))
       (dotimes (i n)
         (let* ((branch  (nth i branches))
                (is-last (= i (1- n)))
                (prefix  (concat (make-string indent ?\s)
                                 (if is-last "  └──▶ " "  ├──▶ ")))
                (ci      (+ indent 9))
                (blines  (arrow--render branch ci)))
           (when blines
             (setq result (append result
                                  (list (concat prefix (string-trim-left (car blines))))
                                  (cdr blines))))
           (unless is-last
             (setq result (append result
                                  (list (concat (make-string indent ?\s) "  │")))))))
       result))

    (`(tracks . ,flows)
     (let (result)
       (dolist (flow flows)
         (when result
           (setq result (append result (list "" (make-string 40 ?─) ""))))
         (setq result (append result (arrow--render flow indent))))
       result))

    (_ (list (concat (make-string indent ?\s) "[?]")))))

;;; ─── Secondary arrow overlay ─────────────────────────────────────────────────
;;
;; After primary rendering, we have a list of lines.
;; For each secondary edge (from-name → to-name):
;;   1. Find the row containing [ from-name ] or [ from-name ] (fac form)
;;   2. Find the row containing [ to-name ]
;;   3. Assign a margin column (base + slot * 2)
;;   4. On source row: replace end of line with ─────┐ reaching margin col
;;   5. On rows between: place │ at margin col
;;   6. On dest row: place ◀────┘ reaching margin col


;;; --- Secondary arrow overlay ------------------------------------------------
;;
;; Two-pass overlay: render spine first, then draw margin-routed arrows.
;; All column arithmetic uses string-width to handle Unicode correctly.

(defun arrow--find-node-row (name lines)
  "Return index of first line containing [ NAME ], [ NAME* ], [NAME],
or variants with # or ! suffixes."
  (let ((patterns (list (concat "[ " name " ]")
                        (concat "[ " name "* ]")
                        (concat "[" name "]")
                        (concat "[ " name "# ]")
                        (concat "[ " name "! ]")
                        (concat "[ " name "*# ]")
                        (concat "[ " name "*! ]")))
        found)
    (cl-loop for line in lines for i from 0
             when (and (not found)
                       (cl-some (lambda (pat)
                                  (string-match-p (regexp-quote pat) line))
                                patterns))
             do (setq found i))
    found))

(defun arrow--node-box-end (name line)
  "Return char position just after the node box for NAME in LINE.
Matches [ NAME ], [ NAME* ], [NAME], and variants with # or ! suffixes."
  (let ((patterns (list (concat "[ " name " ]")
                        (concat "[ " name "* ]")
                        (concat "[" name "]")
                        (concat "[ " name "# ]")
                        (concat "[ " name "! ]")
                        (concat "[ " name "*# ]")
                        (concat "[ " name "*! ]"))))
    (or (cl-some (lambda (pat)
                   (when (string-match (regexp-quote pat) line)
                     (match-end 0)))
                 patterns)
        (length line))))

(defun arrow--pad-to-width (line width)
  "Pad LINE with spaces until string-width >= WIDTH."
  (let ((w (string-width line)))
    (if (>= w width) line
      (concat line (make-string (- width w) ?\s)))))

(defun arrow--apply-secondary-arrows (lines secondary-edges)
  "Overlay SECONDARY-EDGES onto LINES via right-margin routing."
  (if (null secondary-edges)
      lines
    (let* ((vec      (vconcat lines))
           (max-w    (apply #'max (mapcar #'string-width lines)))
           (base-col (+ max-w 10))
           (slot     0))
      (dolist (edge secondary-edges)
        (let* ((from-name (car  edge))
               (to-name   (cadr edge))
               (cur-lines (append vec nil))
               (from-row  (arrow--find-node-row from-name cur-lines))
               (to-row    (arrow--find-node-row to-name   cur-lines))
               (margin    (+ base-col (* slot 2))))
          (when (and from-row to-row (not (= from-row to-row)))
            (let* ((top-row    (min from-row to-row))
                   (bot-row    (max from-row to-row))
                   ;; The arrowhead (◀) goes on the to-name row always;
                   ;; the plain connector (───┐ or ───┘) on the from-name row.
                   (from-line  (aref vec from-row))
                   (from-end   (arrow--node-box-end from-name from-line))
                   (from-pre   (substring from-line 0 from-end))
                   (from-pw    (string-width from-pre))
                   (from-dash  (max 3 (- margin from-pw 1)))
                   (corner     (+ from-pw from-dash))
                   ;; to (arrowhead) row
                   (to-line    (aref vec to-row))
                   (to-end     (arrow--node-box-end to-name to-line))
                   (to-pre     (substring to-line 0 to-end))
                   (to-pw      (string-width to-pre))
                   (to-dash    (max 3 (- corner to-pw 2)))
                   ;; Corner characters depend on direction
                   (from-corner (if (< from-row to-row) "┐" "┘"))
                   (to-corner   (if (< from-row to-row) "┘" "┐")))
              ;; from-name row: plain connector
              (aset vec from-row
                    (concat from-pre (make-string from-dash ?─) from-corner))
              ;; middle rows
              (cl-loop for r from (1+ top-row) to (1- bot-row)
                       do (aset vec r
                                (concat (arrow--pad-to-width (aref vec r) corner) "│")))
              ;; to-name row: arrowhead
              (aset vec to-row
                    (concat to-pre " ◀" (make-string to-dash ?─) to-corner))
              (setq slot (1+ slot))))))
      (append vec nil))))

;;; ─── Entry point ─────────────────────────────────────────────────────────────

(defun arrow--render-src (src)
  (condition-case err
      (let* ((clean      (arrow--strip-comments src))
             (tokens     (arrow--tokenise clean))
             (parsed     (arrow--parse-program tokens))
             (defs       (car  parsed))
             (all-flows  (cadr parsed)))
        (if (null all-flows)
            "Nothing to render."
          ;; expand all flows
          (let* ((expanded   (mapcar (lambda (f) (arrow--expand f defs)) all-flows))
                 ;; Pre-populate known-names with module definition names
                 ;; so that dotted references (Module.Node) can be recognized
                 ;; as secondary even before the spine flow is processed.
                 (known-names (mapcar #'car defs))
                 (spine-flow  nil)
                 (sec-edges   '()))
            (dolist (flow expanded)
              (if (arrow--secondary-p flow known-names)
                  ;; secondary: extract edges
                  (setq sec-edges
                        (append sec-edges (arrow--extract-secondary-edges flow)))
                ;; spine: record names, set as the flow to render
                (dolist (n (arrow--collect-names flow))
                  (unless (member n known-names)
                    (push n known-names)))
                (setq spine-flow
                      (if spine-flow
                          ;; multiple spine lines: wrap in tracks
                          (if (eq (car spine-flow) 'tracks)
                              `(tracks ,@(cdr spine-flow) ,flow)
                            `(tracks ,spine-flow ,flow))
                        flow))))
            (if (null spine-flow)
                "Nothing to render."
              ;; Classify secondary edges into top-level and module-internal
              (let* ((classified (arrow--classify-secondary-edges sec-edges))
                     (top-edges  (car classified))
                     (mod-edges  (cdr classified))
                     ;; Promote module-internal edges to top-level for
                     ;; modules that were inlined (referenced without *)
                     (promoted   (arrow--promote-inlined-module-edges
                                  mod-edges spine-flow))
                     (top-edges  (append top-edges (car promoted)))
                     (mod-edges  (cdr promoted))
                     ;; Bind rendering context for module expansion
                     (arrow--render-defs defs)
                     (arrow--render-module-sec-edges mod-edges)
                     (primary-lines (arrow--render spine-flow 0))
                     (final-lines   (arrow--apply-secondary-arrows
                                     primary-lines top-edges)))
                (mapconcat #'identity final-lines "\n"))))))
    (error (format "Error: %s" (error-message-string err)))))

(defun arrow--display (result)
  (let ((buf (get-buffer-create "*arrow*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert result)
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf)))

;;; ─── Org Babel ───────────────────────────────────────────────────────────────

(defun arrow--load-preamble (flow-name)
  "Generate a Python preamble that loads all node outputs for FLOW-NAME
into a `nodes` dict and injects :arrow shared symbols into the namespace.
Errors immediately if FLOW-NAME has not been run this session."
  (let ((entry (gethash flow-name arrow--pipeline-registry)))
    (unless entry
      (error "Arrow: pipeline %S has not been run this session" flow-name))
    (let* ((output-tbl    (car entry))
           (shared-path   (cdr entry))
           (lines         (list "import dill as _arrow_pickle, os as _arrow_os\n"
                                "_arrow_nodes = {}\n")))
      ;; Load each node output
      (maphash
       (lambda (name pkl-path)
         (when (and (stringp pkl-path)
                    (string-suffix-p ".pkl" pkl-path))
           (push (format
                  "_arrow_p = %S\n\
if _arrow_os.path.exists(_arrow_p):\n\
    _arrow_nodes[%S] = _arrow_pickle.load(open(_arrow_p, 'rb'))\n"
                  pkl-path name)
                 lines)))
       output-tbl)
      ;; Inject shared module symbols
      (when (and shared-path (stringp shared-path)
                 (file-exists-p shared-path))
        (push (format
               "import importlib.util as _arrow_ilu\n\
_arrow_spec = _arrow_ilu.spec_from_file_location('_arrow_shared', %S)\n\
_arrow_mod  = _arrow_ilu.module_from_spec(_arrow_spec)\n\
_arrow_spec.loader.exec_module(_arrow_mod)\n\
for _arrow_k, _arrow_v in vars(_arrow_mod).items():\n\
    if not _arrow_k.startswith('_'):\n\
        globals()[_arrow_k] = _arrow_v\n\
del _arrow_ilu, _arrow_spec, _arrow_mod\n"
               shared-path)
              lines))
      ;; Expose nodes dict and clean up private names
      (push "nodes = _arrow_nodes\n\
del _arrow_pickle, _arrow_os, _arrow_nodes\n" lines)
      (mapconcat #'identity (nreverse lines) ""))))

(defun arrow--babel-execute-python-advice (orig-fun body params)
  "Around-advice for `org-babel-execute:python'.
If PARAMS contains :arrow load <flow-name>, prepend the node-loading
preamble to BODY before delegating to the original function."
  (let ((arrow-param (cdr (assq :arrow params))))
    (if (and arrow-param
             (string-match "\\`load\\s-+\\(\\S-+\\)" (string-trim arrow-param)))
        (let* ((flow-name (match-string 1 (string-trim arrow-param)))
               (preamble  (arrow--load-preamble flow-name)))
          (funcall orig-fun (concat preamble body) params))
      (funcall orig-fun body params))))

(advice-add 'org-babel-execute:python :around
            #'arrow--babel-execute-python-advice)

(defun org-babel-execute:arrow (body params)
  ;; Capture point now, before arrow--display can change the selected window
  ;; or trigger hooks that move point in this buffer.
  (let ((block-pos (point))
        (src-buf   (current-buffer))
        (arrow-param (cdr (assq :arrow params))))
    (arrow--display (arrow--render-src body))
    (unless (and arrow-param
                 (string-match-p "noblocks" (string-trim arrow-param)))
      (arrow--scaffold-blocks body src-buf block-pos)))
  nil)

(defun org-babel-prep-session:arrow (_session _params) nil)

(add-to-list 'org-babel-load-languages '(arrow . t))

;;; ─── Block scaffolding ───────────────────────────────────────────────────────

(defun arrow--collect-exec-names (src)
  "Parse SRC and return the ordered list of executable node names.
Excludes facilitator names (annotation-only) and lowercase module
definition names (they expand away at runtime).  Module references
\(including node-map like PerSeizure*) are expanded to their inner
node names rather than appearing as a single block name."
  (let* ((clean    (arrow--strip-comments src))
         (tokens   (arrow--tokenise clean))
         (parsed   (arrow--parse-program tokens))
         (defs     (car parsed))
         (flows    (cadr parsed))
         ;; Expand modules in the flows, INCLUDING node-map references
         (expanded (mapcar (lambda (f)
                             (arrow--expand-for-scaffold f defs))
                           flows))
         (known    (mapcar #'car defs))
         (result   '()))
    (dolist (flow expanded)
      (unless (arrow--secondary-p flow known)
        (dolist (n (arrow--collect-node-names flow))
          (unless (member n known)
            (push n known)
            (push n result)))))
    (nreverse result)))

(defun arrow--expand-for-scaffold (ast defs)
  "Like `arrow--expand' but also expands node-map module references.
This ensures that scaffolding creates blocks for the inner nodes of a
module rather than for the module name itself."
  (pcase ast
    (`(node ,name . ,rest)
     (let ((def (assoc name defs)))
       (if def
           (let* ((expanded (arrow--expand-for-scaffold (cdr def) defs))
                  (flags (cl-loop for (k v) on rest by #'cddr
                                  when (memq k '(:cache :force))
                                  append (list k v))))
             (arrow--apply-flags expanded flags))
         ast)))
    (`(node-map ,name . ,_rest)
     (let ((def (assoc name defs)))
       (if def
           ;; Expand the module definition — the inner nodes need blocks
           (arrow--expand-for-scaffold (cdr def) defs)
         ;; Not a module — keep as-is (it's a single block that gets mapped)
         ast)))
    (`(seq . ,steps)  `(seq    ,@(mapcar (lambda (s) (arrow--expand-for-scaffold s defs)) steps)))
    (`(fork . ,bs)    `(fork   ,@(mapcar (lambda (b) (arrow--expand-for-scaffold b defs)) bs)))
    (`(fork-or . ,bs) `(fork-or ,@(mapcar (lambda (b) (arrow--expand-for-scaffold b defs)) bs)))
    (`(tracks . ,fs)  `(tracks ,@(mapcar (lambda (f) (arrow--expand-for-scaffold f defs)) fs)))
    (_ ast)))

(defun arrow--collect-node-names (ast)
  "Return only executable node names from AST — no facilitators."
  (pcase ast
    (`(node ,name . ,_)     (list name))
    (`(node-map ,name . ,_) (list name))
    (`(seq . ,steps)  (apply #'append (mapcar #'arrow--collect-node-names steps)))
    (`(fork . ,bs)    (apply #'append (mapcar #'arrow--collect-node-names bs)))
    (`(fork-or . ,bs) (apply #'append (mapcar #'arrow--collect-node-names bs)))
    (`(tracks . ,fs)  (apply #'append (mapcar #'arrow--collect-node-names fs)))
    (_ '())))

(defun arrow--section-bounds-at (pos buffer)
  "Return (start . end) for the section after the src block containing POS.
START is just after the block's #+end_src line; END is the next heading or
point-max.  Does not rely on point being inside the block at call time."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char pos)
     (let* ((el      (org-element-at-point))
            (blk-end (org-element-property :end el)))
       (if (null blk-end)
           (cons pos (point-max))
         (save-excursion
           (goto-char blk-end)
           (let ((sec-end
                  (save-excursion
                    (if (re-search-forward "^\\*+ " nil t)
                        (line-beginning-position)
                      (point-max)))))
             (cons blk-end sec-end))))))))

(defun arrow--named-blocks-in-region (buffer start end)
  "Return alist of (name . pos) for all #+name: blocks in region START..END."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (let (result)
       (goto-char start)
       (while (re-search-forward
               "^[ \t]*#\\+name:[ \t]*\\(.*?\\)[ \t]*$" end t)
         (push (cons (match-string-no-properties 1)
                     (line-beginning-position))
               result))
       (nreverse result)))))

(defun arrow--block-exists-p (name buffer)
  "Return non-nil if a #+name: NAME block exists anywhere in BUFFER."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward
      (concat "^[ \t]*#\\+name:[ \t]*"
              (regexp-quote name)
              "[ \t]*$")
      nil t))))

(defun arrow--rename-block (old-name new-name buffer)
  "Rename the first #+name: OLD-NAME line in BUFFER to NEW-NAME."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward
            (concat "^\\([ \t]*#\\+name:[ \t]*\\)"
                    (regexp-quote old-name)
                    "\\([ \t]*\\)$")
            nil t)
       (replace-match (concat "\\1" new-name "\\2"))))))

(defun arrow--scaffold-blocks (src source-buffer block-pos)
  "After rendering an Arrow block, scaffold missing named src blocks.
BLOCK-POS is the buffer position inside the arrow block at time of execution.
For each executable node in SRC that has no #+name: block anywhere in BUFFER,
insert a blank Python stub in pipeline order — each missing block is placed
after the nearest preceding existing block in the section."
  (let* ((node-names  (arrow--collect-exec-names src))
         (bounds      (arrow--section-bounds-at block-pos source-buffer))
         (sec-start   (car bounds))
         (sec-end     (cdr bounds))
         ;; all #+name: blocks already present in this section: (name . pos)
         (local-blocks (arrow--named-blocks-in-region
                        source-buffer sec-start sec-end))
         ;; nodes that have no block anywhere in the buffer
         (missing      (cl-remove-if
                        (lambda (n) (arrow--block-exists-p n source-buffer))
                        node-names)))
    ;; Insert stubs for missing nodes in pipeline order.
    ;; For each missing node, find the position of the nearest preceding
    ;; existing block (in pipeline order) and insert after it.  If no
    ;; preceding block exists, insert at sec-start.
    (when missing
      (with-current-buffer source-buffer
        (save-excursion
          ;; Build a lookup of name -> end-of-block position (after #+end_src)
          ;; for blocks that exist in the section.
          (let ((block-ends (make-hash-table :test #'equal)))
            (dolist (lb local-blocks)
              (let ((name (car lb))
                    (pos  (cdr lb)))
                (org-with-wide-buffer
                 (goto-char pos)
                 ;; Move past #+name: line, the src block, to after #+end_src
                 (when (re-search-forward "^[ \t]*#\\+end_src" sec-end t)
                   (forward-line 1)
                   (puthash name (point) block-ends)))))
            ;; Walk node-names in pipeline order, inserting missing ones
            (let ((inserted '())
                  (last-insert-pos nil))
              (dolist (name node-names)
                (if (member name missing)
                    ;; This node needs a stub — find where to put it
                    (let ((insert-pos
                           (or last-insert-pos
                               ;; Find nearest preceding existing block in pipeline order
                               (let ((best nil)
                                     (found nil))
                                 (dolist (prev node-names)
                                   (unless found
                                     (if (equal prev name)
                                         (setq found t)
                                       (let ((pos (gethash prev block-ends)))
                                         (when pos (setq best pos))))))
                                 best)
                               ;; No preceding block — use section start
                               sec-start)))
                      (goto-char insert-pos)
                      (unless (bolp) (insert "\n"))
                      (insert (format "\n#+name: %s\n#+begin_src python :results output\n\n#+end_src\n"
                                      name))
                      ;; Update last-insert-pos to after this new block
                      (setq last-insert-pos (point))
                      ;; Also register it so subsequent missing nodes can
                      ;; find it as a preceding block
                      (puthash name (point) block-ends)
                      (push name inserted))
                  ;; This node exists — update last-insert-pos to its end
                  (let ((pos (gethash name block-ends)))
                    (when pos (setq last-insert-pos pos)))))
              (when inserted
                (message "Arrow: created stub block(s) for: %s"
                         (mapconcat #'identity (nreverse inserted) ", "))))))))))


(defun arrow--common-prefix-length (a b)
  "Return the number of leading characters shared by strings A and B."
  (let ((i 0) (len (min (length a) (length b))))
    (while (and (< i len) (eq (aref a i) (aref b i)))
      (setq i (1+ i)))
    i))

;;; ─── Pipeline execution ──────────────────────────────────────────────────────

(defun arrow--exec-find-block (name buffer)
  "Return (lang body) for the named babel block in BUFFER, or nil."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((target (concat "^[ \t]*#\\+name:[ \t]*" (regexp-quote name) "[ \t]*$")))
       (when (re-search-forward target nil t)
         (forward-line 1)
         (let ((info (org-babel-get-src-block-info 'no-eval)))
           (when info
             (list (nth 0 info)   ; language
                   (nth 1 info)   ; body
                   (nth 2 info))  ; params
             )))))))

(defun arrow--collect-shared-blocks (buffer)
  "Return a list of (lang . body) for all blocks with :arrow shared in BUFFER."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (results)
       (while (re-search-forward "^[ \t]*#\\+name:[ \t]*\\(.+?\\)[ \t]*$" nil t)
         (forward-line 1)
         (let ((info (org-babel-get-src-block-info 'no-eval)))
           (when info
             (let* ((lang   (nth 0 info))
                    (body   (nth 1 info))
                    (params (nth 2 info))
                    (arrow-param (cdr (assq :arrow params))))
               (when (and arrow-param
                          (string= (string-trim arrow-param) "shared"))
                 (push (cons lang body) results))))))
       (nreverse results)))))

(defun arrow--build-shared-module (buffer)
  "Assemble all :arrow shared blocks in BUFFER into a temp .py file.
Returns the file path, or nil if there are no shared blocks.
The file is copied to the inspect directory so it persists for the REPL."
  (let ((shared (arrow--collect-shared-blocks buffer)))
    (when shared
      (let ((path (make-temp-file "arrow-shared-" nil ".py")))
        (with-temp-file path
          (insert "# arrow shared module — assembled from :arrow shared blocks

")
          (dolist (block shared)
            (insert (cdr block))
            (insert "
")))
        ;; Hash the shared module (small file) so cache-key can use it
        (puthash path
                 (secure-hash 'sha256
                              (with-temp-buffer
                                (insert-file-contents-literally path)
                                (buffer-string)))
                 arrow--pkl-hashes)
        ;; Copy to inspect dir so it survives temp file cleanup for the REPL
        (when arrow--viz-inspect-dir
          (let ((stable (expand-file-name "arrow-shared.py"
                                          arrow--viz-inspect-dir)))
            (ignore-errors (copy-file path stable t))
            ;; Use the stable path so everything references the persistent copy
            (puthash stable (gethash path arrow--pkl-hashes) arrow--pkl-hashes)
            (push path arrow--pipeline-tempfiles)  ; temp copy can be cleaned
            stable))))))

(defun arrow--exec-log (buf msg &optional status)
  "Append a log line to the *arrow-exec* buffer."
  (with-current-buffer buf
    (read-only-mode -1)
    (goto-char (point-max))
    (insert (format "%-12s %s\n" (or status "") msg))
    (read-only-mode 1)))

;;; ─── Interpreter map ────────────────────────────────────────────────────────
(defvar arrow--map-max-workers 6
  "Maximum number of concurrent subprocesses spawned by a parallel map fork.
Can be overridden per-pipeline by the :workers header argument on the arrow block.")

(defvar arrow--lenient nil
  "When non-nil, map forks and parallel forks continue on error.
Failed elements produce None in the output list/dict instead of
aborting the pipeline.  Set per-pipeline by the :lenient header
argument on the arrow block (any truthy value, e.g. :lenient yes).")

(defvar arrow--interpreters
  '(("python"     . "python3")
    ("python3"    . "python3")
    ("sh"         . "bash")
    ("bash"       . "bash")
    ("ruby"       . "ruby")
    ("perl"       . "perl")
    ("R"          . "Rscript")
    ("julia"      . "julia")
    ("javascript" . "node")
    ("js"         . "node"))
  "Alist mapping babel language names to interpreter executables.")

(defun arrow--interpreter (lang)
  "Return interpreter executable for LANG, or signal an error."
  (let ((interp (cdr (assoc lang arrow--interpreters))))
    (unless interp
      (error "Arrow: no interpreter for language '%s'. Add it to arrow--interpreters" lang))
    interp))

;;; ─── Python harness template ─────────────────────────────────────────────────
;;
;; For Python blocks, Arrow wraps the user body in a harness that:
;;   1. Loads `input' from a pickle file (or sets it to None if no prior node)
;;   2. Executes the block body in a fresh namespace
;;   3. Pickles whatever `output' was assigned to a new temp file
;;   4. Prints the output pickle path to stdout (for Arrow's chaining)
;;
;; The user block just reads `input' and assigns `output'. No serialization.

(defun arrow--python-script (input-path body &optional shared-path)
  "Return a complete Python harness script for BODY with INPUT-PATH (string or nil).
If SHARED-PATH is non-nil, inject a from-import of that shared module before exec.
The user BODY is written to a separate temp file and read back at runtime,
avoiding all quoting/escaping issues with triple-quotes and backslashes."
  (let* ((in-expr   (if input-path (format "%S" input-path) "None"))
         ;; Write the raw body to a temp file — no escaping needed
         (body-file (make-temp-file "arrow-body-" nil ".py"))
         (_         (with-temp-file body-file (insert body)))
         (shared-import
          (if shared-path
              (concat
               "import importlib.util as _ilu\n"
               "_spec = _ilu.spec_from_file_location('_arrow_shared', "
               (format "%S" shared-path) ")\n"
               "_mod = _ilu.module_from_spec(_spec)\n"
               "_spec.loader.exec_module(_mod)\n"
               "# collect callables and modules from shared module; exclude primitive data\n"
               "_shared_callables = {k: v for k, v in vars(_mod).items()\n"
               "                     if not k.startswith('_')\n"
               "                     and not isinstance(v, (int, float, str, bool,\n"
               "                                            list, tuple, dict, set))}\n"
               "del _ilu, _spec, _mod\n"
               "\n")
            "")))
    ;; Register body-file for cleanup
    (push body-file arrow--pipeline-tempfiles)
    (concat
     shared-import
     "import dill as pickle\n"
     "\n"
     "# load input\n"
     "_in_path = " in-expr "\n"
     "if _in_path is None:\n"
     "    input = None\n"
     "else:\n"
     "    with open(_in_path, 'rb') as _f:\n"
     "        input = pickle.load(_f)\n"
     "\n"
     "# user block\n"
     "import sys as _sys\n"
     "_real_stdout = _sys.stdout\n"
     "_sys.stdout = _sys.stderr\n"
     "_ns = {'input': input}\n"
     ;; propagate only shared callables into exec namespace — avoids data variable collisions
     (if shared-path
         "_ns.update(_shared_callables)\n"
       "")
     (format "with open(%S) as _bf:\n    _body = _bf.read()\n" body-file)
     "exec(_body, _ns)\n"
     "_sys.stdout = _real_stdout\n"
     "\n"
     "# serialize output\n"
     "import tempfile\n"
     "output = _ns.get('output', None)\n"
     "_out = tempfile.mktemp(suffix='.pkl', prefix='arrow-data-')\n"
     "with open(_out, 'wb') as _f:\n"
     "    pickle.dump(output, _f)\n"
     "import hashlib as _hl\n"
     "with open(_out, 'rb') as _f:\n"
     "    _hash = _hl.sha256(_f.read()).hexdigest()\n"
     "print(_out)\n"
     "print(_hash)\n")))

(defun arrow--python-script-pipeline (body &optional shared-path inspect-dir)
  "Return a Python harness for BODY that loads the pipeline `nodes` dict.
Used for blocks tagged :arrow pipeline — disconnected from the execution
graph but with full access to all node outputs, exactly as in the REPL.
INSPECT-DIR defaults to `arrow--viz-inspect-dir'."
  (let* ((dir       (or inspect-dir arrow--viz-inspect-dir ""))
         (body-file (make-temp-file "arrow-body-" nil ".py"))
         (_         (with-temp-file body-file (insert body)))
         (shared-import
          (if shared-path
              (concat
               "import importlib.util as _ilu\n"
               "_spec = _ilu.spec_from_file_location('_arrow_shared', "
               (format "%S" shared-path) ")\n"
               "_mod = _ilu.module_from_spec(_spec)\n"
               "_spec.loader.exec_module(_mod)\n"
               "_shared_ns = {k: v for k, v in vars(_mod).items()\n"
               "              if not k.startswith('_')}\n"
               "del _ilu, _spec, _mod\n\n")
            "")))
    (push body-file arrow--pipeline-tempfiles)
    (concat
     shared-import
     "import dill as _pickle, os as _os, glob as _glob\n\n"
     "# load all node outputs into `nodes` dict (same as REPL)\n"
     "_inspect_dir = " (format "%S" dir) "\n"
     "_nodes = {}\n"
     "if _inspect_dir and _os.path.isdir(_inspect_dir):\n"
     "    for _f in sorted(_glob.glob(_os.path.join(_inspect_dir, '*.pkl'))):\n"
     "        _base = _os.path.basename(_f)\n"
     "        if _base.endswith('.input.pkl'):\n"
     "            continue\n"
     "        _name = _os.path.splitext(_base)[0]\n"
     "        try:\n"
     "            with open(_f, 'rb') as _fh:\n"
     "                _nodes[_name] = _pickle.load(_fh)\n"
     "        except Exception:\n"
     "            pass\n\n"
     "# user block\n"
     "import sys as _sys\n"
     "_real_stdout = _sys.stdout\n"
     "_sys.stdout = _sys.stderr\n"
     "_ns = {'nodes': _nodes}\n"
     (if shared-path "_ns.update(_shared_ns)\n" "")
     (format "with open(%S) as _bf:\n    _body = _bf.read()\n" body-file)
     "exec(_body, _ns)\n"
     "_sys.stdout = _real_stdout\n\n"
     "# serialize output (optional — pipeline blocks may set output or not)\n"
     "import tempfile\n"
     "output = _ns.get('output', None)\n"
     "_out = tempfile.mktemp(suffix='.pkl', prefix='arrow-data-')\n"
     "with open(_out, 'wb') as _f:\n"
     "    _pickle.dump(output, _f)\n"
     "import hashlib as _hl\n"
     "with open(_out, 'rb') as _f:\n"
     "    _hash = _hl.sha256(_f.read()).hexdigest()\n"
     "print(_out)\n"
     "print(_hash)\n")))

;;; ─── Non-Python preamble (stdout passing, unchanged) ─────────────────────────

(defun arrow--input-preamble (lang input-str)
  "Return a preamble for non-Python langs injecting INPUT-STR as shell var `input'."
  (let ((escaped (shell-quote-argument (or input-str ""))))
    (pcase lang
      ((or "sh" "bash")   (format "input=%s
" escaped))
      ("ruby"             (format "input = %S
" (or input-str "")))
      ("perl"             (format "my $input = %S;
" (or input-str "")))
      ("R"                (format "input <- %S
" (or input-str "")))
      ("julia"            (format "input = "%s"
"
                                  (replace-regexp-in-string """ "\\"" (or input-str ""))))
      ((or "javascript" "js")
                          (format "const input = %S;
" (or input-str "")))
      (_ (error "Arrow: no input preamble for language '%s'" lang)))))

;;; ─── Caching ────────────────────────────────────────────────────────────────
;;
;; A# in the graph = cache this node (body+input hash key, .arrow-cache/ dir).
;; A! in the graph = force rerun, bypass cache even if a hit exists.
;; Cache key = SHA256(block-body || shared-module-contents || input-pickle-bytes).
;; On a hit  → skip subprocess, copy cached pkl to temp, viz state :cached (cyan).
;; On a miss → run normally, write result pkl to cache on success.

(defvar arrow--cache-dir nil
  "Cache directory for the current pipeline run. Set at pipeline start.")

(defun arrow--cache-init (source-buffer)
  "Set arrow--cache-dir based on SOURCE-BUFFER's file location."
  (let* ((org-file (buffer-file-name source-buffer))
         (dir      (if org-file
                       (expand-file-name ".arrow-cache"
                                         (file-name-directory org-file))
                     (expand-file-name "arrow-cache" temporary-file-directory))))
    (unless (file-exists-p dir) (make-directory dir t))
    (setq arrow--cache-dir dir)))

(defun arrow--cache-key (body input-path)
  "Return a hex SHA256 key for BODY + shared module + input content.
Looks up the input file's hash from arrow--pkl-hashes (populated by
subprocesses) to avoid synchronous file I/O in Emacs.
Returns nil if INPUT-PATH is non-nil but its hash is unknown — the
caller must treat nil as 'caching not possible for this node'."
  (let* ((shared-hash (or (and arrow--shared-module-path
                               (gethash arrow--shared-module-path arrow--pkl-hashes))
                          ""))
         (input-hash  (cond
                       ((null input-path) "")
                       (t (gethash input-path arrow--pkl-hashes)))))
    ;; If the input file exists but its hash was never recorded, we
    ;; cannot produce a reliable key — return nil so callers skip
    ;; cache lookup/store rather than silently collapsing all keys.
    (when (or (null input-path) input-hash)
      (secure-hash 'sha256 (concat body shared-hash (or input-hash ""))))))

(defun arrow--cache-lookup (name body input-path)
  "Return (pkl-path . output-hash) for NAME if a valid cache entry exists, else nil.
Returns nil when the cache key cannot be computed (missing input hash)
or when the .hash sidecar is absent — the output hash is required so
downstream nodes can build their own cache keys."
  (when arrow--cache-dir
    (let ((key (arrow--cache-key body input-path)))
      (when key
        (let* ((path      (expand-file-name (format "%s-%s.pkl" name key) arrow--cache-dir))
               (hash-file (expand-file-name (format "%s-%s.hash" name key) arrow--cache-dir)))
          ;; Require BOTH the pkl AND its hash sidecar.  Without the
          ;; hash, downstream cache keys would fall back to "" and
          ;; silently collide across parallel map elements.
          (when (and (file-exists-p path)
                     (file-exists-p hash-file))
            (cons path
                  (string-trim (with-temp-buffer
                                 (insert-file-contents-literally hash-file)
                                 (buffer-string))))))))))

(defun arrow--cache-store (name body input-path result-path result-hash)
  "Copy RESULT-PATH into the cache for NAME.  Also write RESULT-HASH to a sidecar.
Skips silently when the cache key cannot be computed or RESULT-HASH is nil."
  (when (and arrow--cache-dir result-path (file-exists-p result-path) result-hash)
    (let ((key (arrow--cache-key body input-path)))
      (when key
        (let ((pkl-dest  (expand-file-name (format "%s-%s.pkl" name key) arrow--cache-dir))
              (hash-dest (expand-file-name (format "%s-%s.hash" name key) arrow--cache-dir)))
          (copy-file result-path pkl-dest t)
          (with-temp-file hash-dest
            (insert result-hash)))))))

;;; ─── Process-based block execution ──────────────────────────────────────────

(defvar arrow--pipeline-tempfiles nil
  "List of temp pickle files created during the current pipeline run.")

(defvar arrow--pipeline-defs nil
  "Alist of module definitions for the current pipeline run.
Set at pipeline start so that exec-map-fork can expand module-level maps.")

(defvar arrow--pkl-hashes (make-hash-table :test #'equal)
  "Hash table mapping pickle file path -> SHA256 hex digest.
Populated by subprocesses so that cache-key can look up input hashes
without synchronous file I/O in Emacs.")

(defvar arrow--shared-module-path nil
  "Path to the assembled shared-code .py file for the current pipeline run.
Nil if no :arrow shared blocks exist.  Reset at each pipeline start.")

(defun arrow--merge-script-body (paths-repr out-path)
  "Return Python script string that merges pickles and prints path + hash."
  (format "import sys, traceback
try:
    import dill as pickle, hashlib
    paths = [%s]
    data = [pickle.load(open(p,'rb')) for p in paths]
    with open(%S,'wb') as f: pickle.dump(data, f)
    with open(%S,'rb') as f: h = hashlib.sha256(f.read()).hexdigest()
    print(%S)
    print(h)
except Exception:
    traceback.print_exc(file=sys.stdout)
    sys.exit(1)
" paths-repr out-path out-path out-path))

(defun arrow--dict-merge-script-body (names-and-paths out-path)
  "Return Python script that merges pickles into a dict keyed by branch name.
NAMES-AND-PATHS is a list of (name . pickle-path) pairs."
  (let ((items-repr (mapconcat
                     (lambda (pair)
                       (format "(%S, %S)" (car pair) (cdr pair)))
                     names-and-paths ", ")))
    (format "import sys, traceback
try:
    import dill as pickle, hashlib
    items = [%s]
    data = {name: pickle.load(open(p,'rb')) for name, p in items}
    with open(%S,'wb') as f: pickle.dump(data, f)
    with open(%S,'rb') as f: h = hashlib.sha256(f.read()).hexdigest()
    print(%S)
    print(h)
except Exception:
    traceback.print_exc(file=sys.stdout)
    sys.exit(1)
" items-repr out-path out-path out-path)))

(defun arrow--lenient-merge-script-body (paths-repr out-path)
  "Like `arrow--merge-script-body' but tolerates None entries in the paths list.
None entries (from failed map elements) become None in the output list."
  (format "import sys, traceback
try:
    import dill as pickle, hashlib
    paths = [%s]
    data = [pickle.load(open(p,'rb')) if p is not None else None for p in paths]
    with open(%S,'wb') as f: pickle.dump(data, f)
    with open(%S,'rb') as f: h = hashlib.sha256(f.read()).hexdigest()
    print(%S)
    print(h)
except Exception:
    traceback.print_exc(file=sys.stdout)
    sys.exit(1)
" paths-repr out-path out-path out-path))

(defun arrow--lenient-dict-merge-script-body (names-and-paths out-path)
  "Like `arrow--dict-merge-script-body' but tolerates None pickle paths.
Failed branches become None values in the output dict."
  (let ((items-repr (mapconcat
                     (lambda (pair)
                       (format "(%S, %s)" (car pair)
                               (if (cdr pair) (format "%S" (cdr pair)) "None")))
                     names-and-paths ", ")))
    (format "import sys, traceback
try:
    import dill as pickle, hashlib
    items = [%s]
    data = {name: (pickle.load(open(p,'rb')) if p is not None else None) for name, p in items}
    with open(%S,'wb') as f: pickle.dump(data, f)
    with open(%S,'rb') as f: h = hashlib.sha256(f.read()).hexdigest()
    print(%S)
    print(h)
except Exception:
    traceback.print_exc(file=sys.stdout)
    sys.exit(1)
" items-repr out-path out-path out-path)))

(defun arrow--merge-sentinel (proc event merge-script proc-buf merge-out callback
                              &optional log-buffer log-msg)
  "Standard sentinel for merge subprocesses.  Parses path+hash from stdout."
  (when (string-match-p "finished\\|exited" event)
    (ignore-errors (delete-file merge-script))
    (let ((raw (with-current-buffer proc-buf (buffer-string))))
      (ignore-errors (kill-buffer proc-buf))
      (if (= (process-exit-status proc) 0)
          (let* ((lines (split-string (string-trim raw) "\n" t))
                 ;; Hash is always the LAST line; path is second-to-last.
                 (hash  (and (>= (length lines) 2)
                             (car (last lines)))))
            (when hash
              (puthash merge-out hash arrow--pkl-hashes))
            (when log-msg
              (arrow--exec-log log-buffer log-msg "[done]"))
            (funcall callback merge-out nil))
        (let ((detail (string-trim raw)))
          (funcall callback nil
                   (format "fork merge failed (exit %d): %s"
                           (process-exit-status proc)
                           (truncate-string-to-width
                            (if (string-empty-p detail) "no output" detail)
                            200 nil nil "..."))))))))

(defun arrow--exec-sentinel (proc event)
  "Sentinel for Arrow subprocess. Reads all context from process properties."
  (when (string-match-p "finished\\|exited" event)
    ;; A real subprocess ran — signal cache miss to map tracker
    (when arrow--map-saw-miss
      (setcar arrow--map-saw-miss t))
    (let* ((sf      (process-get proc :script-file))
           (wr      (process-get proc :wrapper))
           (pb      (process-get proc :proc-buf))
           (sb      (process-get proc :stderr-buf))
           (pyp     (process-get proc :python-p))
           (lb      (process-get proc :log-buffer))
           (nm      (process-get proc :name))
           (cb      (process-get proc :callback))
           (cp      (process-get proc :cache-p))
           (bd      (process-get proc :body))
           (ip      (process-get proc :input-path))
           (raw     (with-current-buffer pb (buffer-string)))
           (err-raw (if (and sb (buffer-live-p sb))
                        (with-current-buffer sb (buffer-string))
                      ""))
           (exit    (process-exit-status proc))
           (output  (string-trim raw))
           (printed (string-trim err-raw)))
      (ignore-errors (delete-file sf))
      (ignore-errors (delete-file wr))
      (ignore-errors (kill-buffer pb))
      (ignore-errors (when sb
                       (let ((sp (get-buffer-process sb)))
                         (when sp (kill-process sp)))
                       (kill-buffer sb)))
      (if (= exit 0)
          (let* ((out-lines (and pyp (split-string output "\n" t)))
                 ;; Path and hash are always the LAST two lines printed
                 ;; by the Python wrapper — earlier lines may come from
                 ;; C-level stdout in imported libraries.
                 (n-lines   (length out-lines))
                 (pkl-path  (if pyp (nth (max 0 (- n-lines 2)) out-lines) output))
                 (pkl-hash  (and pyp (>= n-lines 2)
                                 (nth (- n-lines 1) out-lines))))
            (when pyp
              (push pkl-path arrow--pipeline-tempfiles)
              (when pkl-hash
                (puthash pkl-path pkl-hash arrow--pkl-hashes)))
            (when (and pyp (not (string-empty-p printed)))
              (arrow--exec-log lb (format "%s output:\n%s" nm printed) "[print]"))
            ;; write to cache on success if this node was marked #
            (when (and cp pyp bd)
              (arrow--cache-store nm bd ip pkl-path pkl-hash))
            (arrow--exec-log
             lb
             (format "%s -> %s" nm
                     (if pyp
                         (format "[pkl:%s]" (file-name-nondirectory pkl-path))
                       (truncate-string-to-width pkl-path 60 nil nil "...")))
             "[done]")
            (funcall cb pkl-path nil))
        (let ((err-detail (if (string-empty-p printed) output
                            (concat output "\n" printed))))
          (arrow--exec-log
           lb
           (format "%s — exit %d: %s" nm exit
                   (truncate-string-to-width err-detail 80 nil nil "..."))
           "[error]")
          (funcall cb nil (format "%s failed (exit %d): %s" nm exit err-detail)))))))

(defun arrow--exec-block (name input source-buffer log-buffer callback
                          &optional cache-p force-p)
  "Execute block NAME as a subprocess.
CACHE-P means check/write cache; FORCE-P means skip cache lookup (rerun).
For Python: input is a pickle file path or nil; output is a new pickle path.
For others: input is a string; output is stdout string.
Calls CALLBACK with (result nil) or (nil err)."
  (let ((block-info (arrow--exec-find-block name source-buffer)))
    (if (null block-info)
        (progn
          (arrow--exec-log log-buffer (format "%s — block not found" name) "[error]")
          (funcall callback nil (format "Block not found: %s" name)))
      (let* ((lang     (nth 0 block-info))
             (body     (nth 1 block-info))
             (python-p (member lang '("python" "python3")))
             ;; cache lookup for python nodes only (non-python has no pkl output)
             (cache-hit (when (and cache-p (not force-p) python-p)
                          (arrow--cache-lookup name body input)))
             (interp   (condition-case e (arrow--interpreter lang)
                         (error
                          (arrow--exec-log log-buffer
                                           (format "%s — %s" name (error-message-string e))
                                           "[error]")
                          (funcall callback nil (error-message-string e))
                          nil))))
        ;; ── Cache hit: copy pkl to temp, skip subprocess ──────────────────
        (if cache-hit
            (let* ((cached-path (car cache-hit))
                   (cached-hash (cdr cache-hit))
                   (out (make-temp-file "arrow-data-" nil ".pkl")))
              (copy-file cached-path out t)
              (push out arrow--pipeline-tempfiles)
              (when cached-hash
                (puthash out cached-hash arrow--pkl-hashes))
              (arrow--exec-log log-buffer (format "%s [cache hit]" name) "[cached]")
              (let ((arrow--viz-cache-hit t))
                (funcall callback out nil)))
        ;; ── Cache miss or no caching: run normally ────────────────────────
        (when interp
          (let* ((script-file
                  (make-temp-file "arrow-block-" nil
                                  (if python-p ".py"
                                    (pcase lang
                                      ((or "sh" "bash") ".sh")
                                      ("ruby"           ".rb")
                                      ("perl"           ".pl")
                                      ("R"              ".R")
                                      ("julia"          ".jl")
                                      (_                ".tmp")))))
                 (proc-buf  (generate-new-buffer (format " *arrow-proc-%s*"  name)))
                 (stderr-buf (generate-new-buffer (format " *arrow-err-%s*"  name)))
                 (wrapper   (make-temp-file "arrow-wrap-" nil ".sh")))
            ;; write script
            (let ((abort-p nil))
              (with-temp-file script-file
                (if python-p
                    (insert (arrow--python-script
                             (when (and input (not (equal input ""))) input)
                             body
                             arrow--shared-module-path))
                  ;; non-python: prepend string preamble
                  (let ((input-str (cond ((stringp input) input)
                                         ((null input)    "")
                                         (t (format "%s" input)))))
                    (condition-case e
                        (insert (arrow--input-preamble lang input-str))
                      (error
                       (arrow--exec-log log-buffer
                                        (format "%s — %s" name (error-message-string e))
                                        "[error]")
                       (funcall callback nil (error-message-string e))
                       (setq abort-p t))))
                  (unless abort-p (insert body))))
              (unless abort-p
                (with-temp-file wrapper
                  (insert (format "#!/bin/bash\n'%s' '%s'\n" interp script-file)))
                (set-file-modes wrapper #o700)
                (arrow--exec-log log-buffer name "[running]")
                (let ((proc (make-process
                             :name    (format "arrow-%s" name)
                             :buffer  proc-buf
                             :stderr  stderr-buf
                             :command (list "bash" wrapper)
                             :sentinel #'arrow--exec-sentinel)))
                  ;; store all context on the process object — no closures needed
                  (process-put proc :script-file script-file)
                  (process-put proc :wrapper     wrapper)
                  (process-put proc :proc-buf    proc-buf)
                  (process-put proc :stderr-buf  stderr-buf)
                  (process-put proc :python-p    python-p)
                  (process-put proc :log-buffer  log-buffer)
                  (process-put proc :name        name)
                  (process-put proc :callback    callback)
                  (process-put proc :cache-p     cache-p)
                  (process-put proc :body        body)
                  (process-put proc :input-path  input)))))))))))

;;; ─── Parallel map execution ────────────────────────────────────────────
(defun arrow--exec-map-fork (name input source-buffer log-buffer callback
                            &optional flags)
  "Map block NAME over each element of INPUT in parallel, with a concurrency limit.
If NAME has a module definition in arrow--pipeline-defs, the expanded
sub-pipeline is executed per element instead of a single block.
FLAGS is a plist of :cache/:force flags from the node-map reference site,
propagated to leaf nodes in expanded sub-pipelines.
INPUT must be a pickle of a list or any iterable (e.g. numpy array, tuple).
Spawns one arrow--exec-block per element (up to arrow--map-max-workers at a time), 
collects results in order.
Fails fast: if any element errors, pending elements are cancelled and CALLBACK receives (nil err)."
  (if (not (and (stringp input)
                (string-suffix-p ".pkl" input)
                (file-exists-p input)))
      (progn
        (arrow--exec-log log-buffer
                         (format "%s* — input is not a pickle (got %S)" name input)
                         "[error]")
        (funcall callback nil
                 (format "%s*: expected a pickle as input, got: %S" name input)))
    (let* ((split-script (make-temp-file "arrow-split-" nil ".py"))
           (proc-buf     (generate-new-buffer " *arrow-split*")))
      (with-temp-file split-script
        (insert "import dill as pickle, tempfile, sys, hashlib\n")
        (insert (format "data = pickle.load(open(%S, 'rb'))\n" input))
        (insert
         "if isinstance(data, (str, bytes, dict)):\n"
         "    sys.stderr.write(f'not iterable as list: {type(data).__name__}\\n')\n"
         "    sys.exit(1)\n"
         "try:\n"
         "    items = list(data)\n"
         "except TypeError:\n"
         "    sys.stderr.write(f'not iterable: {type(data).__name__}\\n')\n"
         "    sys.exit(1)\n"
         "for item in items:\n"
         "    p = tempfile.mktemp(suffix='.pkl', prefix='arrow-elem-')\n"
         "    pickle.dump(item, open(p, 'wb'))\n"
         "    with open(p, 'rb') as f: h = hashlib.sha256(f.read()).hexdigest()\n"
         "    print(p + ' ' + h)\n"))
      (make-process
       :name    (format "arrow-split-%s" name)
       :buffer  proc-buf
       :command (list "python3" split-script)
       :sentinel
       (lambda (proc event)
         (when (string-match-p "finished\\|exited" event)
           (let* ((raw    (with-current-buffer proc-buf (buffer-string)))
                  (exit   (process-exit-status proc))
                  (output (string-trim raw)))
             (ignore-errors (delete-file split-script))
             (ignore-errors (kill-buffer proc-buf))
             (if (/= exit 0)
                 (progn
                   (arrow--exec-log log-buffer
                                    (format "%s* — split failed: %s" name output)
                                    "[error]")
                   (funcall callback nil
                            (format "%s*: input is not iterable: %s" name output)))
               (let* ((raw-lines  (split-string output "\n" t))
                      (elem-paths (mapcar (lambda (line)
                                            (car (split-string line " " t)))
                                          raw-lines))
                      (n          (length elem-paths)))
                 ;; Populate pkl-hashes for each element so cache keys
                 ;; are unique per element input
                 (dolist (line raw-lines)
                   (let* ((parts (split-string line " " t))
                          (p     (car parts))
                          (h     (cadr parts)))
                     (push p arrow--pipeline-tempfiles)
                     (when h
                       (puthash p h arrow--pkl-hashes))))
                 (if (= n 0)
                     (let* ((empty-out    (make-temp-file "arrow-data-" nil ".pkl"))
                            (empty-script (make-temp-file "arrow-empty-" nil ".py"))
                            (pb2 (generate-new-buffer " *arrow-empty*")))
                       (with-temp-file empty-script
                         (insert (format "import dill as pickle, hashlib\nwith open(%S,'wb') as f: pickle.dump([], f)\nwith open(%S,'rb') as f: h = hashlib.sha256(f.read()).hexdigest()\nprint(%S)\nprint(h)\n"
                                         empty-out empty-out empty-out)))
                       (push empty-out arrow--pipeline-tempfiles)
                       (make-process
                        :name    "arrow-empty-map"
                        :buffer  pb2
                        :command (list "python3" empty-script)
                        :sentinel (lambda (p2 ev2)
                                    (arrow--merge-sentinel
                                     p2 ev2 empty-script pb2 empty-out callback
                                     log-buffer
                                     (format "%s* — mapped 0 elements" name)))))
                   (arrow--exec-log log-buffer
                                    (format "%s* — mapping over %d elements (max %d concurrent)" 
                                            name n arrow--map-max-workers)
                                    "[map]")
                   ;; Seed progress cookie with 0/n now that we know the total
                   (when (functionp arrow--viz-map-hook)
                     (funcall arrow--viz-map-hook name 0 n))
                   (let* ((results (make-vector n nil))
                          (errors  (make-vector n nil))
                          (done-c  (list 0))
                          ;; Check if name has a module def -- if so, expand
                          (mod-def (and arrow--pipeline-defs
                                       (assoc name arrow--pipeline-defs)))
                          (sub-ast (when mod-def
                                     (let ((expanded (arrow--expand
                                                      (cdr mod-def)
                                                      arrow--pipeline-defs)))
                                       (if flags
                                           (arrow--apply-flags expanded flags)
                                         expanded))))
                          (mod-sec (and sub-ast
                                       arrow--module-secondary-edges
                                       (gethash name arrow--module-secondary-edges)))
                          ;; --- SCHEDULING ENGINE ---
                          (queue          (number-sequence 0 (1- n)))
                          (active-workers 0)
                          (spawn-next     nil))
                     
                     (setq spawn-next
                           (lambda ()
                             (while (and queue (< active-workers arrow--map-max-workers))
                               (let* ((idx       (pop queue))
                                      (elem-path (nth idx elem-paths))
                                      (elem-ctx  (when mod-sec (arrow--make-map-ctx mod-sec))))
                                 
                                 (cl-incf active-workers)
                                 
                                 (let ((elem-callback
                                        (lambda (result err)
                                          (aset results idx result)
                                          (aset errors  idx err)
                                          (setcar done-c (1+ (car done-c)))
                                          
                                          (when (functionp arrow--viz-map-hook)
                                            (funcall arrow--viz-map-hook name (car done-c) n))
                                          
                                          ;; Fast-fail: clear pending queue on error
                                          ;; (unless :lenient mode is active)
                                          (when (and err (not arrow--lenient))
                                            (setq queue nil))
                                          
                                          (when err
                                            (arrow--exec-log log-buffer
                                                             (format "%s*[%d] — %s%s" name idx err
                                                                     (if arrow--lenient " (continuing)" ""))
                                                             (if arrow--lenient "[warn]" "[error]")))
                                          
                                          ;; Free the slot before any further action
                                          (cl-decf active-workers)
                                          
                                          (if (= (car done-c) n)
                                              ;; We are the final worker to finish -> Merge!
                                              (let ((first-err (cl-find-if #'identity (append errors nil))))
                                                (if (and first-err (not arrow--lenient))
                                                    (funcall callback nil first-err)
                                                  ;; In lenient mode, replace failed items with
                                                  ;; a None pickle so downstream gets a clean list
                                                  (when (and first-err arrow--lenient)
                                                    (arrow--exec-log log-buffer
                                                                     (format "%s* — %d/%d elements failed (lenient: substituting None)"
                                                                             name
                                                                             (cl-count-if #'identity (append errors nil))
                                                                             n)
                                                                     "[warn]"))
                                                  (let* ((items        (append results nil))
                                                         (merge-out    (make-temp-file "arrow-data-" nil ".pkl"))
                                                         (merge-script (make-temp-file "arrow-mmap-" nil ".py"))
                                                         (paths-repr   (mapconcat
                                                                        (lambda (p)
                                                                          (if p (format "%S" p) "None"))
                                                                        items ", "))
                                                         (mb (generate-new-buffer " *arrow-mmap*")))
                                                    (with-temp-file merge-script
                                                      (insert (arrow--lenient-merge-script-body paths-repr merge-out)))
                                                    (push merge-out arrow--pipeline-tempfiles)
                                                    (make-process
                                                     :name    "arrow-mmap-merge"
                                                     :buffer  mb
                                                     :command (list "python3" merge-script)
                                                     :sentinel (lambda (mp mev)
                                                                 (arrow--merge-sentinel
                                                                  mp mev merge-script mb merge-out callback
                                                                  log-buffer
                                                                  (format "%s* -- collected %d results (%d failed)" name n
                                                                          (cl-count-if #'identity (append errors nil)))))))))
                                            ;; Not final yet: spawn next job from queue
                                            (funcall spawn-next)))))
                                   
                                   ;; Defer launch so Emacs stays responsive
                                   (run-at-time 0 nil
                                     (lambda ()
                                       (if sub-ast
                                           (arrow--exec-flow sub-ast elem-path source-buffer log-buffer elem-callback elem-ctx)
                                         (arrow--exec-block name elem-path source-buffer log-buffer elem-callback)))))))))
                     
                     ;; Kick off the first batch of workers
                     (funcall spawn-next))))))))))))


;;; ─── Recursive flow executor ─────────────────────────────────────────────────

(defun arrow--make-map-ctx (sec-edges)
  "Create a per-element execution context for mapped sub-pipelines.
SEC-EDGES is a list of (source target) pairs for module-internal
secondary arrows.  Returns a vector:
  [0] secondary-targets — hash table: target -> list of sources
  [1] output-table      — hash table: node-name -> pkl-path
  [2] spine-pred        — name of last completed spine node (string or nil)
  [3] spine-from-fork   — non-nil when spine input came from a fork"
  (vector (arrow--build-secondary-targets sec-edges)
          (make-hash-table :test #'equal)
          nil
          nil))

(defun arrow--exec-flow (ast input source-buffer log-buffer callback
                        &optional ctx)
  "Walk AST, executing nodes with INPUT threaded through.
INPUT is a pickle path (Python) or string (other langs), or nil for first node.
CTX, when non-nil, is a map-element context vector for secondary arrow support
inside parallel maps (see `arrow--make-map-ctx').
Calls (callback result nil) or (callback nil err) when done."
  (pcase ast
    (`(node ,name . ,rest)
     (let ((inner-cb (if ctx
                         (lambda (result err)
                           (when (and ctx result (not err))
                             (puthash name result (aref ctx 1)))
                           (funcall callback result err))
                       callback)))
       (arrow--exec-block name input source-buffer log-buffer inner-cb
                          (plist-get rest :cache) (plist-get rest :force))))
    (`(node-map ,name . ,rest)
     (let ((map-flags (cl-loop for (k v) on rest by #'cddr
                               when (memq k '(:cache :force))
                               append (list k v))))
       (arrow--exec-map-fork name input source-buffer log-buffer callback
                             map-flags)))
    (`(seq . ,steps)
     (arrow--exec-seq steps input source-buffer log-buffer callback ctx))
    (`(fork . ,branches)
     (arrow--exec-and-fork branches input source-buffer log-buffer callback ctx))
    (`(fork-or . ,branches)
     (arrow--exec-or-fork branches input source-buffer log-buffer callback))
    (`(tracks . ,_)
     (arrow--exec-flow (cadr ast) input source-buffer log-buffer callback ctx))
    (_ (funcall callback input nil))))

(defun arrow--exec-seq (steps input source-buffer log-buffer callback
                       &optional ctx)
  "Execute STEPS sequentially, threading results as input.
CTX, when non-nil, is a map-element context vector holding per-element
secondary arrow state (see `arrow--make-map-ctx')."
  (if (null steps)
      (funcall callback input nil)
    (let* ((step (car steps))
           (step-name (pcase step
                        (`(node ,name . ,_) name)
                        (`(node-map ,name . ,_) name)
                        (_ nil)))
           (step-is-fork (memq (car step) '(fork fork-or)))
           ;; Use per-element context when available, else globals
           (sec-targets (if ctx (aref ctx 0) arrow--secondary-targets))
           (has-sec     (and step-name sec-targets
                             (gethash step-name sec-targets))))
      (if has-sec
          (arrow--maybe-merge-secondary-input
           step-name input source-buffer log-buffer
           (lambda (merged-input err)
             (if err
                 (funcall callback nil err)
               (arrow--exec-flow
                step merged-input source-buffer log-buffer
                (lambda (result err2)
                  (if err2
                      (funcall callback nil err2)
                    (when step-name
                      (if ctx
                          (aset ctx 2 step-name)
                        (setq arrow--current-spine-predecessor step-name)))
                    (if ctx
                        (aset ctx 3 step-is-fork)
                      (setq arrow--spine-from-fork step-is-fork))
                    (arrow--exec-seq
                     (cdr steps) result source-buffer log-buffer callback ctx)))
                ctx)))
           ctx)
        (arrow--exec-flow
         step input source-buffer log-buffer
         (lambda (result err)
           (if err
               (funcall callback nil err)
             (when step-name
               (if ctx
                   (aset ctx 2 step-name)
                 (setq arrow--current-spine-predecessor step-name)))
             (if ctx
                 (aset ctx 3 step-is-fork)
               (setq arrow--spine-from-fork step-is-fork))
             (arrow--exec-seq
              (cdr steps) result source-buffer log-buffer callback ctx)))
         ctx)))))

;;; ─── Fork-merge deterministic hashing ────────────────────────────────────────
;;
;; The fork-merge step re-pickles branch outputs into a dict.  Because
;; dill.dump can produce non-deterministic bytes for the same logical data
;; (e.g. pandas DataFrames), the merge hash may differ across runs even
;; when the branch outputs are byte-identical.  This breaks downstream
;; cache keys — every node after a fork inherits a random hash component,
;; so its cache key never matches a previous run's entry, and the "or
;; empty-string" fallback in the old cache-key function collapsed all
;; parallel map elements to the same key.
;;
;; Fix: after the merge subprocess finishes, compute a DETERMINISTIC
;; "virtual" hash from the branch names and their output hashes (which
;; are always deterministic — they were computed by the subprocess that
;; created each pkl).  Store this virtual hash in arrow--pkl-hashes for
;; the merge output path.  Downstream cache keys are therefore stable
;; across runs regardless of dill's byte-level non-determinism.

(defun arrow--deterministic-merge-hash (names-and-paths)
  "Compute a deterministic hash for a fork merge from NAMES-AND-PATHS.
Each element is (name . pkl-path).  Returns SHA256 of the
concatenated name:hash pairs, or nil if any branch hash is unknown."
  (let ((parts nil)
        (ok t))
    (dolist (pair names-and-paths)
      (when ok
        (let* ((nm   (car pair))
               (path (cdr pair))
               (hash (and path (gethash path arrow--pkl-hashes))))
          (if hash
              (push (concat nm ":" hash) parts)
            (setq ok nil)))))
    (when (and ok parts)
      (secure-hash 'sha256
                   (concat "fork-merge|"
                           (mapconcat #'identity (nreverse parts) "|"))))))

(defun arrow--run-dict-merge (names-and-paths callback log-buffer)
  "Merge branch outputs into a dict pickle.
NAMES-AND-PATHS is a list of (name . pkl-path) pairs.
After the merge subprocess finishes, overrides the merge output hash
in `arrow--pkl-hashes' with a deterministic virtual hash so that
downstream cache keys are stable across runs.
Calls CALLBACK with (merge-out nil) or (nil err)."
  (let* ((merge-script (make-temp-file "arrow-merge-" nil ".py"))
         (merge-out    (make-temp-file "arrow-data-" nil ".pkl"))
         (proc-buf     (generate-new-buffer " *arrow-merge*"))
         ;; Capture for the sentinel closure
         (nap          names-and-paths))
    (with-temp-file merge-script
      (insert (arrow--dict-merge-script-body nap merge-out)))
    (push merge-out arrow--pipeline-tempfiles)
    (make-process
     :name    "arrow-merge"
     :buffer  proc-buf
     :command (list "python3" merge-script)
     :sentinel (lambda (proc event)
                 (when (string-match-p "finished\\|exited" event)
                   (ignore-errors (delete-file merge-script))
                   (let ((raw (with-current-buffer proc-buf (buffer-string))))
                     (ignore-errors (kill-buffer proc-buf))
                     (if (= (process-exit-status proc) 0)
                         (progn
                           ;; Override the merge output hash with a
                           ;; deterministic virtual hash so downstream
                           ;; cache keys are reproducible.
                           (let ((det-hash (arrow--deterministic-merge-hash nap)))
                             (when det-hash
                               (puthash merge-out det-hash arrow--pkl-hashes)))
                           (when log-buffer
                             (arrow--exec-log log-buffer "fork merge" "[done]"))
                           (funcall callback merge-out nil))
                       (let ((detail (string-trim raw)))
                         (funcall callback nil
                                  (format "fork merge failed (exit %d): %s"
                                          (process-exit-status proc)
                                          (truncate-string-to-width
                                           (if (string-empty-p detail) "no output" detail)
                                           200 nil nil "...")))))))))))

(defun arrow--run-lenient-dict-merge (names-and-paths callback log-buffer)
  "Like `arrow--run-dict-merge' but tolerates nil pickle paths.
Failed branches (where cdr is nil) become None in the output dict."
  (let* ((merge-script (make-temp-file "arrow-merge-" nil ".py"))
         (merge-out    (make-temp-file "arrow-data-" nil ".pkl"))
         (proc-buf     (generate-new-buffer " *arrow-merge*"))
         (nap          names-and-paths))
    (with-temp-file merge-script
      (insert (arrow--lenient-dict-merge-script-body nap merge-out)))
    (push merge-out arrow--pipeline-tempfiles)
    (make-process
     :name    "arrow-merge"
     :buffer  proc-buf
     :command (list "python3" merge-script)
     :sentinel (lambda (proc event)
                 (when (string-match-p "finished\\|exited" event)
                   (ignore-errors (delete-file merge-script))
                   (let ((raw (with-current-buffer proc-buf (buffer-string))))
                     (ignore-errors (kill-buffer proc-buf))
                     (if (= (process-exit-status proc) 0)
                         (progn
                           (when log-buffer
                             (arrow--exec-log log-buffer
                                              (format "fork merge (lenient, %d/%d succeeded)"
                                                      (cl-count-if #'cdr nap)
                                                      (length nap))
                                              "[done]"))
                           (funcall callback merge-out nil))
                       (let ((detail (string-trim raw)))
                         (funcall callback nil
                                  (format "fork merge failed (exit %d): %s"
                                          (process-exit-status proc)
                                          (truncate-string-to-width
                                           (if (string-empty-p detail) "no output" detail)
                                           200 nil nil "...")))))))))))

(defun arrow--exec-and-fork (branches input source-buffer log-buffer callback
                            &optional ctx)
  "Run all BRANCHES in parallel; wait for all.
Merges results into a dict keyed by each branch's terminal node name.
CTX, when non-nil, is a map-element context for secondary arrow support."
  (let* ((n            (length branches))
         (results      (make-vector n nil))
         (errors       (make-vector n nil))
         (done-c       (list 0))
         (branch-names (cl-loop for b in branches for i from 0
                                collect (or (arrow--terminal-node-name b)
                                            (format "branch_%d" i)))))
    (dotimes (i n)
      (let* ((idx        i)
             (branch     (nth i branches))
             (entry-name (pcase branch
                           (`(node ,--n . ,_)                --n)
                           (`(node-map ,--n . ,_)            --n)
                           (`(seq (node ,--n . ,_) . ,_)     --n)
                           (`(seq (node-map ,--n . ,_) . ,_) --n)
                           (_ nil)))
             (sec-tbl    (if ctx (aref ctx 0) arrow--secondary-targets))
             (has-sec    (and entry-name (gethash entry-name sec-tbl))))
        (cl-flet ((run-branch (inp)
                    (arrow--exec-flow
                     branch inp source-buffer log-buffer
                     (lambda (result err)
                       (aset results idx result)
                       (aset errors  idx err)
                       (setcar done-c (1+ (car done-c)))
                       (when (= (car done-c) n)
                         (let ((first-err (cl-find-if #'identity (append errors nil))))
                           (if (and first-err (not arrow--lenient))
                               (funcall callback nil first-err)
                             (let* ((items    (append results nil))
                                    (all-pkl  (cl-every
                                               (lambda (r)
                                                 (or (null r)  ; lenient: failed branch
                                                     (and (stringp r)
                                                          (string-suffix-p ".pkl" r)
                                                          (file-exists-p r))))
                                               items)))
                               (if all-pkl
                                   (let ((pairs (cl-mapcar #'cons branch-names items)))
                                     (if (and first-err arrow--lenient)
                                         (arrow--run-lenient-dict-merge pairs callback log-buffer)
                                       (arrow--run-dict-merge pairs callback log-buffer)))
                                 (funcall callback
                                          (concat "{"
                                                  (mapconcat
                                                   (lambda (nm v) (format "%S: %S" nm v))
                                                   branch-names items ", ")
                                                  "}")
                                          nil)))))))
                     ctx)))
          (if (not has-sec)
              (run-branch input)
            (arrow--maybe-merge-secondary-input
             entry-name input source-buffer log-buffer
             (lambda (merged err)
               (if err
                   (progn (aset errors idx err)
                          (setcar done-c (1+ (car done-c)))
                          (when (= (car done-c) n)
                            (funcall callback nil err)))
                 (run-branch merged)))
             ctx)))))))

(defun arrow--exec-and-fork-viz (branches input source-buffer log-buffer callback)
  "Viz-wrapper version of `arrow--exec-and-fork'."
  (let* ((n            (length branches))
         (results      (make-vector n nil))
         (errors       (make-vector n nil))
         (done-c       (list 0))
         (branch-names (cl-loop for b in branches for i from 0
                                collect (or (arrow--terminal-node-name b)
                                            (format "branch_%d" i)))))
    (dotimes (i n)
      (let* ((idx        i)
             (branch     (nth i branches))
             (entry-name (pcase branch
                           (`(node ,--n . ,_)                --n)
                           (`(node-map ,--n . ,_)            --n)
                           (`(seq (node ,--n . ,_) . ,_)     --n)
                           (`(seq (node-map ,--n . ,_) . ,_) --n)
                           (_ nil)))
             (has-sec    (and entry-name
                              (gethash entry-name arrow--secondary-targets))))
        (cl-flet ((run-branch (inp)
                    (arrow--exec-flow-viz
                     branch inp source-buffer log-buffer
                     (lambda (result err)
                       (aset results idx result)
                       (aset errors  idx err)
                       (setcar done-c (1+ (car done-c)))
                       (when (= (car done-c) n)
                         (let ((first-err (cl-find-if #'identity (append errors nil))))
                           (if (and first-err (not arrow--lenient))
                               (funcall callback nil first-err)
                             (let* ((items    (append results nil))
                                    (all-pkl  (cl-every
                                               (lambda (r)
                                                 (or (null r)  ; lenient: failed branch
                                                     (and (stringp r)
                                                          (string-suffix-p ".pkl" r)
                                                          (file-exists-p r))))
                                               items)))
                               (if all-pkl
                                   (let ((pairs (cl-mapcar #'cons branch-names items)))
                                     (if (and first-err arrow--lenient)
                                         (arrow--run-lenient-dict-merge pairs callback log-buffer)
                                       (arrow--run-dict-merge pairs callback log-buffer)))
                                 (funcall callback
                                          (concat "{"
                                                  (mapconcat
                                                   (lambda (nm v) (format "%S: %S" nm v))
                                                   branch-names items ", ")
                                                  "}")
                                          nil))))))))))
          (if (not has-sec)
              (run-branch input)
            (arrow--maybe-merge-secondary-input
             entry-name input source-buffer log-buffer
             (lambda (merged err)
               (if err
                   (progn (aset errors idx err)
                          (setcar done-c (1+ (car done-c)))
                          (when (= (car done-c) n)
                            (funcall callback nil err)))
                 (run-branch merged))))))))))

(defun arrow--exec-or-fork (branches input source-buffer log-buffer callback)
  "Run all BRANCHES in parallel; first to succeed wins."
  (let ((fired (list nil)))
    (dotimes (i (length branches))
      (arrow--exec-flow
       (nth i branches) input source-buffer log-buffer
       (lambda (result err)
         (unless (car fired)
           (when (null err)
             (setcar fired t)
             (funcall callback result nil))))))))
;;; ─── Live visualization ──────────────────────────────────────────────────────
;;
;; State is tracked in two hash tables reset at each pipeline run:
;;   arrow--viz-state        : node-name -> :pending | :running | :done | :error
;;   arrow--viz-map-progress : node-name -> (done . total)
;;
;; A repeating timer calls arrow--viz-redraw every 250ms while any node is
;; :running.  The viz buffer is *arrow* (already shown by C-c C-c).
;; The exec log buffer *arrow-exec* is hidden by default; C-c C-l in *arrow*
;; opens it.

(defvar arrow--viz-state        nil "Hash table: node name -> execution state.")
(defvar arrow--viz-map-progress nil "Hash table: node name -> (done . total).")
(defvar arrow--viz-timer        nil "Repeating redraw timer, or nil.")
(defvar arrow--viz-src          nil "Arrow src string for the running pipeline.")
(defvar arrow--viz-frame        0   "Animation frame counter.")
(defvar arrow--viz-log-buffer   nil "The *arrow-exec* buffer for the current run.")
(defvar arrow--viz-cache-hit    nil "Dynamically bound to t inside a cache-hit callback.")

(defvar arrow--viz-inspect-dir  nil "Directory holding persistent per-node inspect pickles.")
(defvar arrow--viz-output       nil "Hash table: node name -> stable inspect pkl path.")

(defvar arrow--viz-flow-name    nil
  "Name of the currently-running pipeline (from #+name: of the arrow block).
Used to scope the inspect directory and REPL buffer per flow.")

(defvar arrow--viz-map-hook     nil
  "If non-nil, a function (name done total) called on each map-element completion.
Set by arrow--viz-wrap-map-fork; cleared on pipeline end.")

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface arrow-viz-pending '((t :inherit shadow))
  "Face for pending (not-yet-run) nodes.")
(defface arrow-viz-running '((t :foreground "yellow" :weight bold))
  "Face for currently-executing nodes.")
(defface arrow-viz-done    '((t :foreground "green"  :weight bold))
  "Face for successfully completed nodes.")
(defface arrow-viz-cached  '((t :foreground "cyan"   :weight bold))
  "Face for nodes whose result was loaded from cache.")
(defface arrow-viz-error   '((t :foreground "red"    :weight bold))
  "Face for failed nodes.")

;;; ── State management ─────────────────────────────────────────────────────────

(defun arrow--viz-init (src log-buffer &optional flow-name)
  "Reset visualization state for a new pipeline run of SRC.
FLOW-NAME, if non-nil, scopes the inspect directory so each
named flow gets its own REPL and node outputs."
  (setq arrow--viz-state        (make-hash-table :test #'equal)
        arrow--viz-output       (make-hash-table :test #'equal)
        arrow--viz-map-progress (make-hash-table :test #'equal)
        arrow--viz-src          src
        arrow--viz-frame        0
        arrow--viz-log-buffer   log-buffer
        arrow--viz-map-hook     nil
        arrow--shared-module-path nil
        arrow--viz-flow-name    flow-name)

  ;; Scope the inspect directory per flow-name so each pipeline
  ;; gets its own REPL namespace and node outputs.
  (let ((dir-name (if flow-name
                      (format "arrow-inspect-%s" flow-name)
                    "arrow-inspect")))
    (setq arrow--viz-inspect-dir
          (expand-file-name dir-name temporary-file-directory)))
  (when (file-exists-p arrow--viz-inspect-dir)
    (dolist (f (directory-files arrow--viz-inspect-dir t
                               "\\.\\(pkl\\|body\\.py\\)\\'"))
      (ignore-errors (delete-file f))))
  (unless (file-exists-p arrow--viz-inspect-dir)
    (make-directory arrow--viz-inspect-dir t))
  ;; Seed all nodes as :pending
  (dolist (name (arrow--collect-exec-names src))
    (puthash name :pending arrow--viz-state))
  (arrow--viz-redraw))

(defun arrow--viz-set (name state)
  "Update NAME to STATE and trigger a redraw."
  (when arrow--viz-state
    (puthash name state arrow--viz-state)
    (arrow--viz-redraw)))

(defun arrow--viz-running-p ()
  "Return t if any node is currently :running."
  (when arrow--viz-state
    (let (found)
      (maphash (lambda (_k v) (when (eq v :running) (setq found t)))
               arrow--viz-state)
      found)))

(defun arrow--viz-start-timer ()
  "Start the animation timer if not already running."
  (unless (and arrow--viz-timer (timerp arrow--viz-timer))
    (setq arrow--viz-timer
          (run-with-timer 0.25 0.25 #'arrow--viz-tick))))

(defun arrow--viz-stop-timer ()
  "Stop the animation timer."
  (when (timerp arrow--viz-timer)
    (cancel-timer arrow--viz-timer))
  (setq arrow--viz-timer nil))

(defun arrow--viz-tick ()
  "Timer callback: advance frame and redraw if anything is still running."
  (if (arrow--viz-running-p)
      (progn
        (setq arrow--viz-frame (% (1+ arrow--viz-frame) 8))
        (arrow--viz-redraw))
    (arrow--viz-stop-timer)
    (arrow--viz-redraw)))  ; one final redraw to show terminal state

;;; ── Rendering ────────────────────────────────────────────────────────────────

(defun arrow--viz-redraw ()
  "Re-render the arrow graph into *arrow* with state-based colorization."
  (when (and arrow--viz-src (buffer-live-p (get-buffer "*arrow*")))
    (let* ((plain-lines (split-string (arrow--render-src arrow--viz-src) "\n"))
           (colored     (arrow--viz-colorize plain-lines)))
      (with-current-buffer (get-buffer "*arrow*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (mapconcat #'identity colored "\n")))))))

(defun arrow--viz-colorize (lines)
  "Apply faces to LINES based on current node states.
Returns a new list of propertized strings."
  (let* ((vec    (vconcat lines))
         (nlines (length vec))
         (faces  (make-vector nlines nil)))
    ;; Pass 1: colorize node boxes, record their faces
    (dotimes (i nlines)
      (let ((line (aref vec i)))
        ;; Detect [ NodeName ] or [ NodeName* ]
        (if (string-match "\\[\\*?\\([^]]+?\\)\\*?\\]" line)
            (let* ((raw-name (string-trim (match-string 1 line)))
                   ;; strip trailing *, #, ! suffixes (map, cache, force nodes)
                   (name     (string-trim
                              (replace-regexp-in-string "[*#!]+\\'" "" raw-name)))
                   (state    (and arrow--viz-state
                                  (gethash name arrow--viz-state)))
                   (face     (pcase state
                               (:pending 'arrow-viz-pending)
                               (:running 'arrow-viz-running)
                               (:done    'arrow-viz-done)
                               (:cached  'arrow-viz-cached)
                               (:error   'arrow-viz-error)
                               (_        'default))))
              (aset vec i (propertize line 'face face))
              (aset faces i face))
          ;; Non-node line: leave face nil for now
          (aset faces i nil))))
    ;; Pass 2: propagate faces to connector lines.
    ;; For each node line, walk upward through preceding non-node lines
    ;; (connectors like │ ▼ ├──▶ └──▶) and color them with that node's face.
    ;; This makes the connectors "leading into" a node share its color.
    (dotimes (i nlines)
      (when (aref faces i)
        (let ((j (1- i)))
          (while (and (>= j 0) (null (aref faces j)))
            (aset vec j (propertize (aref vec j) 'face (aref faces i)))
            (aset faces j (aref faces i))
            (setq j (1- j))))))
    ;; Pass 3: any remaining uncolored lines get default face
    (dotimes (i nlines)
      (unless (aref faces i)
        (aset vec i (propertize (aref vec i) 'face 'default))))
    ;; Append map-progress annotations
    (when arrow--viz-map-progress
      (maphash
       (lambda (name progress)
         (let* ((done   (car progress))
                (total  (cdr progress))
                (state  (and arrow--viz-state (gethash name arrow--viz-state)))
                (face   (if (memq state '(:done :cached)) 'arrow-viz-done 'arrow-viz-running)))
           (dotimes (i nlines)
             (when (string-match (format "\\[ %s\\*[#!]? \\]"
                                         (regexp-quote name))
                                 (aref vec i))
               (aset vec i
                     (concat (aref vec i)
                             (propertize (format "  %d/%d" done total)
                                         'face face)))))))
       arrow--viz-map-progress))
    (append vec nil)))
;;; ── Hooks into execution ─────────────────────────────────────────────────────

(defun arrow--viz-store-context (name input source-buffer)
  "Save node's input pickle and block body to the inspect directory.
These are used by the REPL's _run() command for interactive re-execution."
  (when arrow--viz-inspect-dir
    ;; Save input pickle
    (when (and input (stringp input)
               (string-suffix-p ".pkl" input)
               (file-exists-p input))
      (let ((dest (expand-file-name (format "%s.input.pkl" name)
                                    arrow--viz-inspect-dir)))
        (ignore-errors (copy-file input dest t))))
    ;; Save block body
    (let ((block-info (arrow--exec-find-block name source-buffer)))
      (when block-info
        (let ((body (nth 1 block-info))
              (dest (expand-file-name (format "%s.body.py" name)
                                      arrow--viz-inspect-dir)))
          (with-temp-file dest
            (insert body)))))))

(defun arrow--viz-wrap-exec-block (name input source-buffer log-buffer callback
                                   &optional cache-p force-p)
  "Wrapper around arrow--exec-block that updates visualization state."
  (arrow--viz-set name :running)
  (arrow--viz-start-timer)
  ;; Save context NOW (synchronously) before the async subprocess runs
  (arrow--viz-store-context name input source-buffer)
  (arrow--exec-block
   name input source-buffer log-buffer
   (lambda (result err)
     (arrow--viz-set name (cond (err                  :error)
                                (arrow--viz-cache-hit :cached)
                                (t                    :done)))
     (when (and result (not err))
       (arrow--viz-store-output name result))
     (funcall callback result err))
   cache-p force-p))

(defvar arrow--map-saw-miss nil
  "When non-nil, a cons cell (flag).  Set to t by exec-block when running
a subprocess (not cache hit).  Used by viz-wrap-map-fork to determine
whether the map node should show :cached or :done.")

(defun arrow--viz-wrap-map-fork (name input source-buffer log-buffer callback
                                &optional flags)
  "Wrapper around arrow--exec-map-fork that shows per-element progress."
  (arrow--viz-set name :running)
  (arrow--viz-start-timer)
  (puthash name (cons 0 0) arrow--viz-map-progress)
  (setq arrow--viz-map-hook
        (lambda (hook-name done total)
          (when (equal hook-name name)
            (puthash name (cons done total) arrow--viz-map-progress)
            (arrow--viz-redraw))))
  ;; Track whether any inner subprocess ran (i.e. cache miss).
  ;; The exec-sentinel sets (setcar arrow--map-saw-miss t) when
  ;; a real subprocess completes.
  (setq arrow--map-saw-miss (list nil))
  (arrow--exec-map-fork
   name input source-buffer log-buffer
   (lambda (result err)
     (arrow--viz-set name (cond (err                           :error)
                                ((car arrow--map-saw-miss)     :done)
                                (t                             :cached)))
     (setq arrow--viz-map-hook nil
           arrow--map-saw-miss nil)
     (when (and result (not err)) (arrow--viz-store-output name result))
     (funcall callback result err))
   flags))

;;; ── Patched exec-flow using viz wrappers ─────────────────────────────────────

(defun arrow--exec-flow-viz (ast input source-buffer log-buffer callback)
  "Like arrow--exec-flow but routes node execution through viz wrappers."
  (pcase ast
    (`(node ,name . ,rest)
     (arrow--viz-wrap-exec-block name input source-buffer log-buffer callback
                                 (plist-get rest :cache) (plist-get rest :force)))
    (`(node-map ,name . ,rest)
     (let ((map-flags (cl-loop for (k v) on rest by #'cddr
                               when (memq k '(:cache :force))
                               append (list k v))))
       (arrow--viz-wrap-map-fork name input source-buffer log-buffer callback
                                 map-flags)))
    (`(seq . ,steps)
     (arrow--exec-seq-viz steps input source-buffer log-buffer callback))
    (`(fork . ,branches)
     (arrow--exec-and-fork-viz branches input source-buffer log-buffer callback))
    (`(fork-or . ,branches)
     (arrow--exec-or-fork-viz branches input source-buffer log-buffer callback))
    (`(tracks . ,_)
     (arrow--exec-flow-viz (cadr ast) input source-buffer log-buffer callback))
    (_ (funcall callback input nil))))

(defun arrow--exec-seq-viz (steps input source-buffer log-buffer callback)
  (if (null steps)
      (funcall callback input nil)
    (let* ((step (car steps))
           (step-name (pcase step
                        (`(node ,name . ,_) name)
                        (`(node-map ,name . ,_) name)
                        (_ nil)))
           (step-is-fork (memq (car step) '(fork fork-or))))
      (if (and step-name arrow--secondary-targets
               (gethash step-name arrow--secondary-targets))
          (arrow--maybe-merge-secondary-input
           step-name input source-buffer log-buffer
           (lambda (merged-input err)
             (if err
                 (funcall callback nil err)
               (arrow--exec-flow-viz
                step merged-input source-buffer log-buffer
                (lambda (result err2)
                  (if err2
                      (funcall callback nil err2)
                    (when step-name
                      (setq arrow--current-spine-predecessor step-name))
                    (setq arrow--spine-from-fork step-is-fork)
                    (arrow--exec-seq-viz
                     (cdr steps) result source-buffer log-buffer callback)))))))
        (arrow--exec-flow-viz
         step input source-buffer log-buffer
         (lambda (result err)
           (if err
               (funcall callback nil err)
             (when step-name
               (setq arrow--current-spine-predecessor step-name))
             (setq arrow--spine-from-fork step-is-fork)
             (arrow--exec-seq-viz
              (cdr steps) result source-buffer log-buffer callback))))))))
(defun arrow--exec-or-fork-viz (branches input source-buffer log-buffer callback)
  (let ((fired (list nil)))
    (dotimes (i (length branches))
      (arrow--exec-flow-viz
       (nth i branches) input source-buffer log-buffer
       (lambda (result err)
         (unless (car fired)
           (when (null err)
             (setcar fired t)
             (funcall callback result nil))))))))
;;; ── Secondary arrow support ──────────────────────────────────────────────────
;;
;; Secondary arrows (e.g. A > E where both A and E are already in the
;; spine) create additional data-flow edges.  When a node has multiple
;; incoming edges (from fork-join convergence, secondary arrows, or both),
;; its `input` is a dict keyed by source node name.  Nodes with a single
;; input still receive a bare value for backward compatibility.

(defvar arrow--secondary-edges nil
  "List of (source-name target-name) pairs for the current pipeline run.
Set at pipeline start from secondary arrow classification.")

(defvar arrow--secondary-targets nil
  "Hash table: target-name -> list of source-names for secondary arrow inputs.
Built from arrow--secondary-edges at pipeline start.")

(defvar arrow--module-secondary-edges nil
  "Hash table: module-name -> list of (source-name target-name) pairs.
For dotted secondary arrows like Module.NodeA > Module.NodeB, these
edges are applied per-element when the module executes as a sub-pipeline.
The source and target names have the module prefix stripped.")

(defvar arrow--current-spine-predecessor nil
  "Name of the node whose output is currently being threaded as spine input.
Set dynamically during sequential execution so that secondary-arrow merging
can label the spine input in the dict.")

(defvar arrow--spine-from-fork nil
  "Non-nil if the current spine input came from a fork-join merge.
When true, the secondary merge will flatten the spine dict's entries
into the result.  When nil, the spine value is stored under the
predecessor's name, even if it happens to be a dict.")

(defun arrow--build-secondary-targets (sec-edges)
  "Build hash table mapping target node names to their secondary source names."
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (edge sec-edges)
      (let* ((src (car edge))
             (tgt (cadr edge))
             (existing (gethash tgt tbl)))
        (puthash tgt (append existing (list src)) tbl)))
    tbl))

(defun arrow--classify-secondary-edges (sec-edges)
  "Separate SEC-EDGES into top-level and module-internal edges.
Returns (top-level-edges . module-edges-table) where module-edges-table
is a hash table mapping module-name to list of (src tgt) with prefixes stripped.
Dotted edges like (\"Mod.A\" \"Mod.B\") become (\"A\" \"B\") under key \"Mod\".
Signals an error if a dotted edge references two different modules."
  (let ((top-level '())
        (mod-tbl (make-hash-table :test #'equal)))
    (dolist (edge sec-edges)
      (let* ((src (car edge))
             (tgt (cadr edge))
             (src-dot (string-match-p "\\." src))
             (tgt-dot (string-match-p "\\." tgt)))
        (if (or src-dot tgt-dot)
            ;; Dotted edge — extract module prefix
            (let* ((src-parts (split-string src "\\."))
                   (tgt-parts (split-string tgt "\\."))
                   (src-mod (if src-dot (car src-parts) nil))
                   (tgt-mod (if tgt-dot (car tgt-parts) nil))
                   (mod-name (or src-mod tgt-mod)))
              ;; Both must reference the same module
              (when (and src-mod tgt-mod (not (equal src-mod tgt-mod)))
                (error "Arrow: cross-module secondary arrow %s > %s — \
both nodes must be in the same module" src tgt))
              ;; Both must be dotted
              (unless (and src-dot tgt-dot)
                (error "Arrow: secondary arrow %s > %s — \
both sides must use Module.Node notation for intra-module arrows" src tgt))
              (let* ((inner-src (cadr src-parts))
                     (inner-tgt (cadr tgt-parts))
                     (existing (gethash mod-name mod-tbl)))
                (puthash mod-name
                         (append existing (list (list inner-src inner-tgt)))
                         mod-tbl)))
          ;; Plain top-level edge
          (push edge top-level))))
    (cons (nreverse top-level) mod-tbl)))

(defun arrow--collect-node-map-names (ast)
  "Return list of module names that appear as node-map (i.e. with *) in AST."
  (pcase ast
    (`(node . ,_) nil)
    (`(node-map ,name . ,_) (list name))
    (`(seq . ,steps)   (apply #'append (mapcar #'arrow--collect-node-map-names steps)))
    (`(fork . ,bs)     (apply #'append (mapcar #'arrow--collect-node-map-names bs)))
    (`(fork-or . ,bs)  (apply #'append (mapcar #'arrow--collect-node-map-names bs)))
    (`(tracks . ,fs)   (apply #'append (mapcar #'arrow--collect-node-map-names fs)))
    (_ nil)))

(defun arrow--promote-inlined-module-edges (mod-edges spine-flow)
  "Promote module-internal secondary edges to top-level for inlined modules.
Modules referenced without * are expanded inline, so their node-map entry
is absent from the AST.  For those modules, convert their internal edges
\(e.g. (\"LoadH5\" \"PreprocessSeizure\") under module \"Preprocess\") to
top-level edges (\"LoadH5\" \"PreprocessSeizure\").
Returns (promoted-top-edges . remaining-mod-edges) where remaining-mod-edges
is a new hash table with only the still-node-map modules."
  (let ((mapped-names (arrow--collect-node-map-names spine-flow))
        (promoted '())
        (remaining (make-hash-table :test #'equal)))
    (maphash
     (lambda (mod-name edges)
       (if (member mod-name mapped-names)
           ;; Module is used as node-map — keep as module-internal
           (puthash mod-name edges remaining)
         ;; Module is inlined — promote edges to top-level
         (dolist (edge edges)
           (push edge promoted))))
     mod-edges)
    (cons (nreverse promoted) remaining)))

(defun arrow--maybe-merge-secondary-input (name spine-input source-buffer
                                           log-buffer callback
                                           &optional ctx)
  "If NAME has secondary arrow inputs, merge them with SPINE-INPUT into a dict.
Secondary sources are keyed by their node name.  The spine input is either:
  - flattened into the dict if it is itself a dict (from a fork-join), or
  - keyed by the spine predecessor name otherwise.
This avoids key collisions when the spine predecessor is also a secondary source
\(e.g. H > (I, J) > K with H > K: K gets {\"I\":…, \"J\":…, \"H\":…}).
If NAME has no secondary inputs, pass SPINE-INPUT through unchanged.
CTX, when non-nil, provides per-element state for parallel map execution.
Calls CALLBACK with (merged-pkl-path nil) or (nil err)."
  (let* ((sec-targets (if ctx (aref ctx 0) arrow--secondary-targets))
         (sec-sources (and sec-targets (gethash name sec-targets))))
    (if (null sec-sources)
        ;; No secondary inputs — pass through unchanged
        (funcall callback spine-input nil)
      ;; Has secondary inputs — merge into a dict
      (let* ((all-pkl t)
             (sec-paths '())
             (output-tbl (if ctx (aref ctx 1) arrow--viz-output)))
        ;; Collect secondary source pickle paths
        (dolist (src sec-sources)
          (let* (;; For dotted references like "Module.Node", resolve to inner node
                 (dotted-p (string-match-p "\\." src))
                 (lookup-name (if dotted-p
                                  (cadr (split-string src "\\."))
                                src))
                 (dict-key (if dotted-p lookup-name src))
                 (src-path (and output-tbl
                                (gethash lookup-name output-tbl))))
            ;; For dotted refs, check that the module isn't a mapped node
            (when dotted-p
              (let* ((module-name (car (split-string src "\\.")))
                     (module-mapped-p
                      (cl-find-if (lambda (edge)
                                    (and (string-match-p
                                          (concat "\\`" (regexp-quote module-name) "\\*")
                                          (or (car edge) ""))))
                                  ;; Check if the module appears as node-map in the spine
                                  nil)))
                ;; We can't easily check the AST here, so we check if the
                ;; inner node output exists.  For mapped modules the inner
                ;; outputs get overwritten per-element so they're unreliable.
                ;; The validation in arrow-exec-pipeline catches this instead.
                nil))
            (if (and src-path (stringp src-path)
                     (string-suffix-p ".pkl" src-path)
                     (file-exists-p src-path))
                (push (cons dict-key src-path) sec-paths)
              (setq all-pkl nil)
              (arrow--exec-log log-buffer
                               (format "%s — secondary input from %s not available"
                                       name src)
                               "[warn]"))))
        (setq sec-paths (nreverse sec-paths))
        (if (not all-pkl)
            ;; Some secondary inputs missing — proceed with spine input only
            (funcall callback spine-input nil)
          ;; Build a Python script that merges spine + secondary inputs.
          ;; The script loads the spine pickle; if it's a dict, its entries
          ;; are flattened into the result.  Otherwise it's stored under the
          ;; spine predecessor key.  Secondary sources are then added, and
          ;; override any same-named key from a flattened spine dict, so
          ;; the direct secondary value wins over the fork-join passthrough.
          (let* ((merge-script (make-temp-file "arrow-secmerge-" nil ".py"))
                 (merge-out    (make-temp-file "arrow-data-" nil ".pkl"))
                 (proc-buf     (generate-new-buffer " *arrow-secmerge*"))
                 (spine-path   (or spine-input ""))
                 (spine-name   (or (if ctx (aref ctx 2) arrow--current-spine-predecessor)
                                   "spine"))
                 (flatten-p    (if ctx (aref ctx 3) arrow--spine-from-fork))
                 (sec-items    (mapconcat
                                (lambda (pair)
                                  (format "(%S, %S)" (car pair) (cdr pair)))
                                sec-paths ", ")))
            (with-temp-file merge-script
              (insert (format "import sys, traceback
try:
    import dill as pickle, hashlib
    result = {}
    # Load spine input
    spine_path = %S
    spine_name = %S
    flatten = %s
    if spine_path:
        spine_val = pickle.load(open(spine_path, 'rb'))
        if flatten and isinstance(spine_val, dict):
            # Fork-join produced a dict — flatten its entries
            result.update(spine_val)
        else:
            # Single predecessor — key by predecessor name
            result[spine_name] = spine_val
    # Load secondary sources (override any colliding flattened keys)
    for sec_name, sec_path in [%s]:
        result[sec_name] = pickle.load(open(sec_path, 'rb'))
    with open(%S, 'wb') as f:
        pickle.dump(result, f)
    with open(%S, 'rb') as f:
        h = hashlib.sha256(f.read()).hexdigest()
    print(%S)
    print(h)
except Exception:
    traceback.print_exc(file=sys.stdout)
    sys.exit(1)
" spine-path spine-name (if flatten-p "True" "False")
  sec-items merge-out merge-out merge-out)))
            (push merge-out arrow--pipeline-tempfiles)
            (make-process
             :name    "arrow-secmerge"
             :buffer  proc-buf
             :command (list "python3" merge-script)
             :sentinel (lambda (proc event)
                         (arrow--merge-sentinel
                          proc event merge-script proc-buf
                          merge-out callback
                          log-buffer
                          (format "%s — merged %d inputs (secondary)"
                                  name (1+ (length sec-paths))))))))))))

;;; ── Entry point (replaces arrow-exec-pipeline) ───────────────────────────────

(defun arrow-exec-pipeline (source-buffer &optional src flow-name explicit-params)
  "Execute the Arrow pipeline defined in SRC (or the current babel block).
FLOW-NAME, if non-nil, scopes the inspect directory and REPL buffer.
EXPLICIT-PARAMS, if non-nil, is the org-babel params alist for the block
(used when called programmatically via arrow-run so header args like
:workers are respected even though point is not inside the block).
Shows live graph visualization in *arrow*.  The execution log is
available in *arrow-exec* via \\[arrow-show-exec-log] (C-c C-l in *arrow*)."
  (interactive (list (current-buffer)))
  (let* ((info (and (null src) (org-babel-get-src-block-info)))
         (src (or src
                  (if info (nth 1 info)
                    (error "Not in a babel block"))))
         (flow-name (or flow-name
                        (and info
                             (save-excursion
                               (org-babel-goto-src-block-head)
                               (forward-line -1)
                               (when (looking-at "[ \t]*#\\+name:[ \t]*\\(.+?\\)[ \t]*$")
                                 (match-string-no-properties 1))))))
         (params (or explicit-params (and info (nth 2 info))))
         (arrow-param (and params (cdr (assq :arrow params))))
         (workers-param (and params (cdr (assq :workers params))))
         (lenient-param (and params (cdr (assq :lenient params)))))
    ;; Set the global directly so the value survives into async sentinels
    ;; and run-at-time callbacks, which execute outside this let* scope.
    (when workers-param
      (setq arrow--map-max-workers
            (cond ((integerp workers-param) workers-param)
                  ((stringp  workers-param) (string-to-number workers-param))
                  (t arrow--map-max-workers))))
    (setq arrow--lenient
          (and lenient-param
               (not (member (if (stringp lenient-param)
                                (downcase (string-trim lenient-param))
                              "")
                            '("no" "nil" "false" "0" "")))))
    (when (and arrow-param
               (string-match-p "noblocks" (string-trim arrow-param)))
      (error "Arrow: this block has :arrow noblocks — visual only, not executable"))
    ;; Set up the execution log buffer
    (let* ((log-buffer (get-buffer-create "*arrow-exec*"))
           (parsed     (arrow--parse-program (arrow--tokenise (arrow--strip-comments src))))
           (defs       (car  parsed))
           (all-flows  (cadr parsed))
           (expanded   (mapcar (lambda (f) (arrow--expand f defs)) all-flows))
           (known-names (mapcar #'car defs))
           (spine-flow  nil)
           (sec-edges   '()))
      (with-current-buffer log-buffer
        (read-only-mode -1)
        (erase-buffer)
        (read-only-mode 1))
      ;; Classify flows into spine and secondary
      (dolist (flow expanded)
        (if (arrow--secondary-p flow known-names)
            (setq sec-edges (append sec-edges (arrow--extract-secondary-edges flow)))
          (dolist (n (arrow--collect-names flow))
            (unless (member n known-names) (push n known-names)))
          (setq spine-flow
                (if spine-flow
                    (if (eq (car spine-flow) 'tracks)
                        `(tracks ,@(cdr spine-flow) ,flow)
                      `(tracks ,spine-flow ,flow))
                  flow))))
      (unless spine-flow
        (error "Arrow: nothing to execute"))
      ;; Initialise cache directory and visualisation state
      (arrow--cache-init source-buffer)
      (arrow--viz-init src log-buffer flow-name)
      (setq arrow--pipeline-tempfiles nil
            arrow--pipeline-defs      defs)
      ;; Build secondary-edge lookup tables
      (let* ((classified  (arrow--classify-secondary-edges sec-edges))
             (top-edges   (car classified))
             (mod-edges   (cdr classified))
             (promoted    (arrow--promote-inlined-module-edges mod-edges spine-flow))
             (top-edges   (append top-edges (car promoted))))
        (setq arrow--secondary-edges         top-edges
              arrow--secondary-targets       (arrow--build-secondary-targets top-edges)
              arrow--module-secondary-edges  (cdr promoted)
              arrow--current-spine-predecessor nil
              arrow--spine-from-fork          nil))
      ;; Assemble shared-code module (must happen after viz-init sets inspect dir)
      (setq arrow--shared-module-path
            (arrow--build-shared-module source-buffer))
      ;; Launch the pipeline
      (arrow--exec-flow-viz
       spine-flow nil source-buffer log-buffer
       (lambda (result err)
         (arrow--viz-stop-timer)
         (if err
             (arrow--exec-log log-buffer (format "Pipeline error: %s" err) "[error]")
           (arrow--exec-log log-buffer "Pipeline complete." "[done]")
           ;; Register this pipeline's outputs for :arrow load
           (when flow-name
             (puthash flow-name
                      (cons (copy-hash-table arrow--viz-output)
                            arrow--shared-module-path)
                      arrow--pipeline-registry)))
         (arrow--repl-reload)
         (run-at-time 5 nil
           (lambda ()
             (dolist (f arrow--pipeline-tempfiles)
               (ignore-errors (delete-file f)))
             (setq arrow--pipeline-tempfiles nil))))))))

(defun arrow--find-named-arrow-blocks (buffer)
  "Return list of (name body params) for all named arrow src blocks in BUFFER.
Excludes blocks with :arrow noblocks."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (results nil))
       (while (re-search-forward
               "^[ \t]*#\\+name:[ \t]*\\(.+?\\)[ \t]*$" nil t)
         (let ((name (match-string-no-properties 1)))
           (save-excursion
             (forward-line 1)
             (when (looking-at "[ \t]*#\\+begin_src[ \t]+arrow")
               (let ((info (org-babel-get-src-block-info 'no-eval)))
                 (when info
                   (let* ((params (nth 2 info))
                          (arrow-param (cdr (assq :arrow params))))
                     (unless (and arrow-param
                                  (string-match-p "noblocks"
                                                  (string-trim arrow-param)))
                       (push (list name (nth 1 info) params) results)))))))))
       (nreverse results)))))

(defun arrow-run (&optional name)
  "Run a named Arrow pipeline from anywhere in the buffer.
If NAME is nil, find all named arrow blocks.  If there is exactly one,
run it; otherwise prompt the user to choose."
  (interactive)
  (let* ((blocks (arrow--find-named-arrow-blocks (current-buffer)))
         (choice
          (cond
           ((null blocks)
            (error "No named arrow blocks found in this buffer"))
           ((= (length blocks) 1)
            (car blocks))
           (name
            (or (cl-find name blocks :key #'car :test #'equal)
                (error "No arrow block named %s" name)))
           (t
            (let ((picked (completing-read "Run pipeline: "
                                           (mapcar #'car blocks)
                                           nil t)))
              (cl-find picked blocks :key #'car :test #'equal))))))
    (message "Arrow: running pipeline %s" (car choice))
    (arrow-exec-pipeline (current-buffer) (cadr choice) (car choice) (caddr choice))))

(defun arrow-show-exec-log ()
  "Show the *arrow-exec* log buffer for the current pipeline."
  (interactive)
  (if (buffer-live-p arrow--viz-log-buffer)
      (display-buffer arrow--viz-log-buffer)
    (message "No arrow exec log available")))

;;; ─── REPL ────────────────────────────────────────────────────────────────────
;;
;; A single *arrow-repl* buffer gives access to all pipeline outputs via a
;; `nodes` dictionary (keys = node names, values = outputs).  Shared module
;; symbols are injected into the namespace.  `_reload()` refreshes everything
;; from the latest inspect pickles on disk.

(defvar arrow--repl-buffer nil
  "The single *arrow-repl* buffer, or nil.")

(defun arrow--repl-write-init ()
  "Write a PYTHONSTARTUP init file for the consolidated Arrow REPL.
Returns the file path.  Loads all node outputs into `nodes` dict and
injects shared module symbols."
  (let ((init (make-temp-file "arrow-repl-init-" nil ".py")))
    (with-temp-file init
      (insert "import dill as _pickle\n")
      (insert "import os as _os\n")
      (insert "import glob as _glob\n")
      (insert "\n")
      ;; inspect dir
      (insert (format "_inspect_dir = %S\n"
                      (or arrow--viz-inspect-dir "")))
      (insert "\n")
      ;; shared module
      (insert (format "_shared_path = %S\n"
                      (or arrow--shared-module-path "")))
      (insert "\n")
      (insert "def _load_shared():\n")
      (insert "    if not _shared_path or not _os.path.exists(_shared_path):\n")
      (insert "        return\n")
      (insert "    import importlib.util as _ilu\n")
      (insert "    _spec = _ilu.spec_from_file_location('_arrow_shared', _shared_path)\n")
      (insert "    _mod = _ilu.module_from_spec(_spec)\n")
      (insert "    _spec.loader.exec_module(_mod)\n")
      (insert "    for _k, _v in vars(_mod).items():\n")
      (insert "        if not _k.startswith('_'):\n")
      (insert "            globals()[_k] = _v\n")
      (insert "\n")
      ;; reload
      (insert "def _reload():\n")
      (insert "    \"\"\"Reload all node outputs from disk into `nodes` dict.\"\"\"\n")
      (insert "    _load_shared()\n")
      (insert "    d = {}\n")
      (insert "    if _inspect_dir and _os.path.isdir(_inspect_dir):\n")
      (insert "        for _f in sorted(_glob.glob(_os.path.join(_inspect_dir, '*.pkl'))):\n")
      (insert "            _base = _os.path.basename(_f)\n")
      (insert "            if _base.endswith('.input.pkl'):\n")
      (insert "                continue\n")
      (insert "            _name = _os.path.splitext(_base)[0]\n")
      (insert "            try:\n")
      (insert "                with open(_f, 'rb') as _fh:\n")
      (insert "                    d[_name] = _pickle.load(_fh)\n")
      (insert "            except Exception as _e:\n")
      (insert "                print(f'  [warn] {_name}: {_e}')\n")
      (insert "    globals()['nodes'] = d\n")
      (insert "    print(f'[arrow] loaded {len(d)} node outputs: {list(d.keys())}')\n")
      (insert "\n")
      ;; _run: re-execute a block in the REPL namespace
      (insert "def _run(name):\n")
      (insert "    \"\"\"Re-execute a node's block in the REPL namespace.\n")
      (insert "    Loads the node's input, exec's its body, and leaves all\n")
      (insert "    variables in scope for interactive inspection.\"\"\"\n")
      (insert "    import dill as _pkl\n")
      (insert "    _input_path = _os.path.join(_inspect_dir, f'{name}.input.pkl')\n")
      (insert "    _body_path = _os.path.join(_inspect_dir, f'{name}.body.py')\n")
      (insert "    if not _os.path.exists(_body_path):\n")
      (insert "        print(f'[arrow] no body found for {name}')\n")
      (insert "        print(f'        available: {[f.replace(\".body.py\",\"\") for f in _os.listdir(_inspect_dir) if f.endswith(\".body.py\")]}')\n")
      (insert "        return\n")
      (insert "    # Load input\n")
      (insert "    if _os.path.exists(_input_path):\n")
      (insert "        with open(_input_path, 'rb') as _f:\n")
      (insert "            globals()['input'] = _pkl.load(_f)\n")
      (insert "        print(f'[arrow] loaded input for {name}  type={type(globals()[\"input\"]).__name__}')\n")
      (insert "    else:\n")
      (insert "        globals()['input'] = None\n")
      (insert "        print(f'[arrow] no input for {name} (first node in pipeline?)')\n")
      (insert "    # Read and execute body\n")
      (insert "    with open(_body_path) as _f:\n")
      (insert "        _code = _f.read()\n")
      (insert "    exec(_code, globals())\n")
      (insert "    print(f'[arrow] executed {name} — variables are now in scope')\n")
      (insert "\n")
      ;; _ls: list available nodes for _run
      (insert "def _ls():\n")
      (insert "    \"\"\"List nodes available for _run().\"\"\"\n")
      (insert "    if not _inspect_dir or not _os.path.isdir(_inspect_dir):\n")
      (insert "        print('[arrow] no inspect directory')\n")
      (insert "        return\n")
      (insert "    bodies = sorted(f.replace('.body.py', '') for f in _os.listdir(_inspect_dir) if f.endswith('.body.py'))\n")
      (insert "    print(f'[arrow] {len(bodies)} nodes available for _run():')\n")
      (insert "    for b in bodies:\n")
      (insert "        has_input = _os.path.exists(_os.path.join(_inspect_dir, f'{b}.input.pkl'))\n")
      (insert "        has_output = _os.path.exists(_os.path.join(_inspect_dir, f'{b}.pkl'))\n")
      (insert "        flags = ('in' if has_input else '  ') + '+' + ('out' if has_output else '   ')\n")
      (insert "        print(f'  {b:30s} [{flags}]')\n")
      (insert "\n")
      ;; _clear: reset namespace and terminal
      (insert "def _clear():\n")
      (insert "    \"\"\"Clear the terminal and reset the namespace.\n")
      (insert "    Keeps nodes, shared module symbols, and arrow internals.\n")
      (insert "    Removes all user variables from _run() or interactive work.\"\"\"\n")
      (insert "    import types as _types\n")
      (insert "    # Collect names of shared module symbols to preserve\n")
      (insert "    _shared_names = set()\n")
      (insert "    if _shared_path and _os.path.exists(_shared_path):\n")
      (insert "        import importlib.util as _ilu2\n")
      (insert "        _sp2 = _ilu2.spec_from_file_location('_arrow_shared', _shared_path)\n")
      (insert "        _md2 = _ilu2.module_from_spec(_sp2)\n")
      (insert "        _sp2.loader.exec_module(_md2)\n")
      (insert "        _shared_names = {k for k in vars(_md2) if not k.startswith('_')}\n")
      (insert "    # Names to keep: underscore-prefixed, builtins, nodes, shared symbols,\n")
      (insert "    # and the arrow helper functions themselves\n")
      (insert "    _keep = {'nodes', 'input', 'output', '__builtins__'}\n")
      (insert "    _keep |= _shared_names\n")
      (insert "    _keep |= {k for k in globals() if k.startswith('_')}\n")
      (insert "    _to_del = [k for k in list(globals().keys())\n")
      (insert "               if k not in _keep and not k.startswith('_')]\n")
      (insert "    for _k in _to_del:\n")
      (insert "        del globals()[_k]\n")
      (insert "    # Reload nodes + shared symbols fresh\n")
      (insert "    _reload()\n")
      (insert "    # Print sentinel for Emacs to clear the buffer\n")
      (insert "    print('\\x1a__ARROW_CLEAR__')\n")
      (insert "    print('=== Arrow REPL ===')\n")
      (insert "    print('  nodes[\"Name\"]  — access any node output')\n")
      (insert "    print('  _run(\"Name\")   — re-execute a node, all vars in scope')\n")
      (insert "    print('  _ls()          — list nodes available for _run')\n")
      (insert "    print('  _reload()      — refresh after pipeline re-run')\n")
      (insert "    print('  _clear()       — reset namespace and clear screen')\n")
      (insert "    print(f'[arrow] namespace cleared — {len(_to_del)} user variables removed')\n")
      (insert "\n")
      ;; initial load + banner
      (insert "_reload()\n")
      (insert "print('=== Arrow REPL ===')\n")
      (insert "print('  nodes[\"Name\"]  — access any node output')\n")
      (insert "print('  _run(\"Name\")   — re-execute a node, all vars in scope')\n")
      (insert "print('  _ls()          — list nodes available for _run')\n")
      (insert "print('  _reload()      — refresh after pipeline re-run')\n")
      (insert "print('  _clear()       — reset namespace and clear screen')\n")
      ;; Self-delete after being sourced — reliable cleanup, no timer race
      (insert (format "_os.unlink(%S)\n" init)))
    init))

(defun arrow--repl-clear-filter (output)
  "Comint output filter: watch for the clear sentinel and erase buffer above it."
  (when (string-match-p "\x1a__ARROW_CLEAR__" output)
    (let ((buf (current-buffer)))
      (run-at-time 0 nil
        (lambda ()
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                ;; Find the sentinel and delete everything before it
                (goto-char (point-min))
                (when (search-forward "\x1a__ARROW_CLEAR__" nil t)
                  ;; Delete from start to end of the sentinel line
                  (beginning-of-line)
                  (delete-region (point-min) (progn (forward-line 1) (point))))
                (goto-char (point-max)))))))))
  output)

(defun arrow-open-repl ()
  "Open (or switch to) the Arrow REPL for the current flow.
Each named flow gets its own REPL buffer with independent state.
All node outputs are available in the `nodes` dict.  Shared module
symbols are injected into the namespace.  Call `_reload()` to refresh."
  (interactive)
  (unless arrow--viz-output
    (error "No pipeline output — run the pipeline first"))
  (let* ((buf-name (if arrow--viz-flow-name
                       (format "*arrow-repl<%s>*" arrow--viz-flow-name)
                     "*arrow-repl*"))
         (buf      (get-buffer buf-name))
         (proc     (and buf (get-buffer-process buf))))
    (if (and proc (process-live-p proc))
        (pop-to-buffer-same-window buf)
      ;; Fresh start
      (let* ((init     (arrow--repl-write-init))
             (repl-buf (get-buffer-create buf-name))
             (process-environment
              (cons (format "PYTHONSTARTUP=%s" init)
                    process-environment)))
        (with-current-buffer repl-buf
          (when (comint-check-proc repl-buf)
            (kill-process (get-buffer-process repl-buf)))
          (erase-buffer))
        (make-comint-in-buffer "arrow-repl" repl-buf "python3" nil "-i")
        (with-current-buffer repl-buf
          (comint-mode)
          (setq-local comint-prompt-regexp "^>>> \\|^\\.\\.\\. ")
          (add-hook 'comint-output-filter-functions #'arrow--repl-clear-filter nil t))
        (setq arrow--repl-buffer repl-buf)
        (pop-to-buffer-same-window repl-buf)))))

(defun arrow--repl-reload ()
  "If the consolidated REPL is running, send _reload() to refresh all outputs."
  (when (and arrow--repl-buffer (buffer-live-p arrow--repl-buffer))
    (let ((proc (get-buffer-process arrow--repl-buffer)))
      (when (and proc (process-live-p proc))
        (comint-send-string proc "_reload()\n")))))

;;; ── Pipeline output registry ─────────────────────────────────────────────────
;;
;; Maps flow-name -> snapshot of arrow--viz-output at pipeline completion.
;; Used by :arrow load to inject node outputs into downstream python blocks.

(defvar arrow--pipeline-registry (make-hash-table :test #'equal)
  "Hash table: flow-name -> (output-table . shared-module-path).
Populated when a named pipeline completes successfully.")

;;; ── Wire reload into viz-store-output ────────────────────────────────────────

(defun arrow--viz-store-output (name result)
  "Copy RESULT pkl to stable inspect dir and record it."
  (when (and arrow--viz-output result)
    (let ((stable
           (if (and (stringp result)
                    (string-suffix-p ".pkl" result)
                    (file-exists-p result)
                    arrow--viz-inspect-dir)
               (let ((dest (expand-file-name (format "%s.pkl" name)
                                             arrow--viz-inspect-dir)))
                 (ignore-errors (copy-file result dest t))
                 dest)
             result)))
      (puthash name stable arrow--viz-output))))

;;; ── Keybindings in *arrow* buffer ────────────────────────────────────────────

(defun arrow--setup-arrow-buffer-keys (buf)
  "Set up keybindings in the *arrow* display buffer BUF."
  (with-current-buffer buf
    (local-set-key (kbd "C-c C-l") #'arrow-show-exec-log)
    (local-set-key (kbd "RET")     #'arrow-open-repl)
    ;; Evil compatibility: bind RET in both normal and insert state if evil loaded
    (with-eval-after-load 'evil
      (evil-local-set-key 'normal (kbd "RET") #'arrow-open-repl)
      (evil-local-set-key 'insert (kbd "RET") #'arrow-open-repl))))

(arrow--setup-arrow-buffer-keys (get-buffer-create "*arrow*"))

;;; ── Export ───────────────────────────────────────────────────────────────────

(defun arrow-export (filename)
  "Export all node outputs to a zip-of-pickles file.
Prompts for FILENAME and appends .zip automatically.  Each node
output from the current pipeline becomes a separate entry (Name.pkl)
inside the zip, serialised with dill.  This gives you a single
portable file with selective loading:

  import zipfile, dill
  with zipfile.ZipFile(\"export.zip\") as zf:
      with zf.open(\"SomeNode.pkl\") as f:
          data = dill.load(f)

Requires the pipeline to have been run (so that the inspect
directory is populated with .pkl files)."
  (interactive
   (let ((proj-dir (if arrow--cache-dir
                       (file-name-directory (directory-file-name arrow--cache-dir))
                     default-directory)))
     (list (read-file-name "Export nodes to: "
                           proj-dir "arrow-export" nil "arrow-export"))))
  ;; Ensure .zip extension
  (unless (string-suffix-p ".zip" filename)
    (setq filename (concat filename ".zip")))
  ;; Validate state
  (unless arrow--viz-inspect-dir
    (user-error "No pipeline output — run the pipeline first"))
  (unless (file-directory-p arrow--viz-inspect-dir)
    (user-error "Inspect directory does not exist: %s" arrow--viz-inspect-dir))
  (let* ((pkl-files (directory-files arrow--viz-inspect-dir t "\\.pkl\\'"))
         ;; Filter out .input.pkl files — those are node inputs, not outputs
         (pkl-files (cl-remove-if
                     (lambda (f) (string-suffix-p ".input.pkl" f))
                     pkl-files))
         (dest (expand-file-name filename))
         (script (make-temp-file "arrow-export-" nil ".py")))
    (unless pkl-files
      (user-error "No node output .pkl files found in %s" arrow--viz-inspect-dir))
    ;; Write a small Python script that zips the pkl files
    (with-temp-file script
      (insert "import zipfile, os, sys\n")
      (insert (format "dest = %S\n" dest))
      (insert (format "src_dir = %S\n" arrow--viz-inspect-dir))
      (insert "pkls = [f for f in sorted(os.listdir(src_dir))\n")
      (insert "        if f.endswith('.pkl') and not f.endswith('.input.pkl')]\n")
      (insert "with zipfile.ZipFile(dest, 'w', zipfile.ZIP_DEFLATED) as zf:\n")
      (insert "    for f in pkls:\n")
      (insert "        zf.write(os.path.join(src_dir, f), f)\n")
      (insert "print(f'Exported {len(pkls)} nodes to {dest}')\n"))
    ;; Run the script asynchronously with a sentinel for feedback
    (let ((buf (generate-new-buffer " *arrow-export*")))
      (make-process
       :name "arrow-export"
       :buffer buf
       :command (list "python3" script)
       :sentinel
       (lambda (proc event)
         (unwind-protect
             (let ((exit-code (process-exit-status proc)))
               (if (zerop exit-code)
                   (message "Arrow: exported %d node%s to %s"
                            (length pkl-files)
                            (if (= (length pkl-files) 1) "" "s")
                            dest)
                 (message "Arrow export failed (exit %d): %s"
                          exit-code
                          (with-current-buffer buf
                            (string-trim (buffer-string))))))
           (ignore-errors (delete-file script))
           (kill-buffer buf)))))))

;;; ── Pipeline kill ────────────────────────────────────────────────────────────

(defun arrow-kill ()
  "Kill the currently running Arrow pipeline and all its subprocesses.
Kills every live process whose name starts with \"arrow-\" (node runners,
split/merge helpers, etc.), cleans up temp files, resets pipeline state,
and marks any in-flight nodes as :error in the visualisation."
  (interactive)
  ;; 1. Kill all live arrow subprocesses
  (let ((killed 0))
    (dolist (proc (process-list))
      (when (and (string-prefix-p "arrow-" (process-name proc))
                 ;; Spare the REPL — it's a persistent interactive process
                 (not (string-prefix-p "arrow-repl" (process-name proc)))
                 (process-live-p proc))
        ;; Suppress the sentinel so it doesn't fire callbacks on a killed proc
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (cl-incf killed)))
    ;; 2. Mark every :running node as :error in the visualisation
    (when arrow--viz-state
      (maphash (lambda (name state)
                 (when (eq state :running)
                   (puthash name :error arrow--viz-state)))
               arrow--viz-state))
    ;; 3. Stop the animation timer and do a final redraw
    (arrow--viz-stop-timer)
    (arrow--viz-redraw)
    ;; 4. Clean up temp files
    (dolist (f arrow--pipeline-tempfiles)
      (ignore-errors (delete-file f)))
    (setq arrow--pipeline-tempfiles nil)
    ;; 5. Reset in-flight scheduling state
    (setq arrow--viz-map-hook nil
          arrow--map-saw-miss nil
          arrow--lenient nil)
    ;; 6. Log and report
    (when (buffer-live-p arrow--viz-log-buffer)
      (arrow--exec-log arrow--viz-log-buffer
                       (format "Pipeline killed (%d subprocess%s terminated)."
                               killed (if (= killed 1) "" "es"))
                       "[killed]"))
    (message "Arrow: pipeline killed (%d subprocess%s terminated)."
             killed (if (= killed 1) "" "es"))))

(with-eval-after-load 'arrow-lang
  (with-current-buffer (get-buffer-create "*arrow*")
    (local-set-key (kbd "C-c C-k") #'arrow-kill))
  (with-eval-after-load 'evil
    (evil-define-key 'normal arrow-mode-map
      (kbd "C-c C-k") #'arrow-kill)))


(provide 'arrow-lang)
;;; arrow-lang.el ends here


;;; ─── Diagnostic ──────────────────────────────────────────────────────────────

(defun arrow-test-process ()
  "Run a trivial subprocess to verify make-process + sentinel work."
  (interactive)
  (let ((buf (generate-new-buffer "*arrow-test*")))
    (make-process
     :name "arrow-test"
     :buffer buf
     :command (list "python3" "-c" "print('hello from subprocess')")
     :sentinel (lambda (proc event)
                 (message "SENTINEL fired: %s | exit: %d | output: %S"
                          (string-trim event)
                          (process-exit-status proc)
                          (with-current-buffer buf
                            (buffer-string)))))))
