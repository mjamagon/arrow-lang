; Highlights for the Arrow flowchart language (tree-sitter)

; Comments
(comment) @comment

; Operators
(arrow_op) @operator
(arrow_query_op) @operator
(define_op) @keyword.operator
(semicolon) @punctuation.delimiter

; Punctuation in groups (forks)
["(" ")"] @punctuation.bracket
"," @punctuation.delimiter

; Node names — the main content
(annotated_node
  name: (node_name (identifier) @variable))

; Module name in definitions
(definition
  name: (module_name (identifier) @type.definition))

; Facilitator — the /Name annotation
(facilitator
  (slash) @punctuation.special
  name: (identifier) @function)

; Label — the :text annotation
(label
  (colon) @punctuation.special
  text: (identifier) @string)

; Parallel map star
(star) @keyword

; Cache flag
(cache_flag) @attribute

; Force-rerun flag
(force_flag) @keyword.exception
