package tree_sitter_arrow_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-arrow"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_arrow.Language())
	if language == nil {
		t.Errorf("Error loading Arrow grammar")
	}
}
