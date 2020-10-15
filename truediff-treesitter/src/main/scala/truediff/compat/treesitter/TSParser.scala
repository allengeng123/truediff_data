package truediff.compat.treesitter

import truediff.compat.treesitter.TreeSitterLibrary.lib

class TSParser(lang: TSLanguagePointer, tokenNodes: Set[String]) {
  private var parser = lib.ts_parser_new()
  lib.ts_parser_set_language(parser, lang);

  def parse(code: String): DiffableTSNode = {
    val tree = lib.ts_parser_parse_string(parser, null, code, code.length)
    val root = lib.ts_tree_root_node(tree)
    val cursor = lib.ts_tree_cursor_new(root)
    val cursorRef = cursor.asReference()
    val diffable = DiffableTSNode.importTreeSitter(cursorRef, root, tokenNodes, code)
    println(lib.ts_node_string(root))
    lib.ts_tree_cursor_delete(cursorRef)
    lib.ts_tree_delete(tree)
    diffable
  }

  def destroy(): Unit = {
    lib.ts_parser_delete(parser)
    parser = null
  }

}


