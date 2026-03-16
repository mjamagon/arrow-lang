/// <reference types="tree-sitter-cli/dsl" />
// tree-sitter grammar for the Arrow flowchart language
//
// Syntax reference (from arrow-lang.el):
//   A > B                 sequence
//   (A, B)                parallel fork
//   A > B : label         labeled transition
//   A > B / Fac           facilitator on transition
//   A > B / Fac : label   combined
//   flow ; flow           independent tracks
//   name := flow          module definition (lowercase)
//   A > B >? C            or-join fork
//   A > B* > C            parallel map
//   A#                    cache flag
//   A!                    force-rerun flag
//   -- comment            line comment

module.exports = grammar({
  name: "arrow",

  extras: ($) => [/\s/],

  rules: {
    source_file: ($) =>
      repeat(
        choice(
          $.definition,
          $.track_line,
          $.comment
        )
      ),

    // -- comment
    comment: ($) => token(seq("--", /[^\n]*/)),

    // name := flow
    definition: ($) =>
      seq(
        field("name", $.module_name),
        $.define_op,
        field("body", $.flow)
      ),

    define_op: ($) => ":=",

    // flow ; flow  (independent tracks on one line)
    track_line: ($) =>
      seq(
        $.flow,
        repeat(seq($.semicolon, $.flow))
      ),

    semicolon: ($) => ";",

    // A > B > C  or  A >? (B, C) > D
    flow: ($) =>
      seq(
        $.step,
        repeat(seq(choice($.arrow_op, $.arrow_query_op), $.step))
      ),

    arrow_op: ($) => ">",
    arrow_query_op: ($) => token(prec(1, seq(">", "?"))),

    // a step is either a group (fork) or an annotated node
    step: ($) => choice($.group, $.annotated_node),

    // (A, B > C, D)
    group: ($) =>
      seq(
        "(",
        $.flow,
        repeat(seq(",", $.flow)),
        ")"
      ),

    // Node with optional annotations
    annotated_node: ($) =>
      seq(
        field("name", $.node_name),
        repeat(
          choice(
            $.facilitator,
            $.label,
            $.star,
            $.cache_flag,
            $.force_flag
          )
        )
      ),

    node_name: ($) => $.identifier,

    // Module name (used in definitions — lowercase start)
    module_name: ($) => $.identifier,

    // / FacilitatorName
    facilitator: ($) =>
      seq($.slash, field("name", $.identifier)),

    slash: ($) => "/",

    // : LabelText
    label: ($) => seq($.colon, field("text", $.identifier)),

    colon: ($) => ":",

    // * for parallel map
    star: ($) => "*",

    // # for cache
    cache_flag: ($) => "#",

    // ! for force-rerun
    force_flag: ($) => "!",

    // Identifiers: word chars, dots for Module.Node references
    // Multi-word node names like "My Node" are handled as adjacent identifiers
    // in the tree but highlighted as a single node_name
    identifier: ($) =>
      token(
        seq(
          /[A-Za-z_]/,
          repeat(/[A-Za-z0-9_]/),
          repeat(seq(".", /[A-Za-z_]/, repeat(/[A-Za-z0-9_]/)))
        )
      ),
  },
});
