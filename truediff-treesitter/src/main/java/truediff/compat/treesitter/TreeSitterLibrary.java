package truediff.compat.treesitter;

import com.sun.jna.Library;

public interface TreeSitterLibrary extends Library {

    TreeSitterLibrary INSTANCE = NativeLoad.loadLibrary("tree-sitter", TreeSitterLibrary.class);
    TreeSitterLibrary lib = INSTANCE;

    /**
     * Create a new parser.
     */
    TSParserPointer ts_parser_new();

    /**
     * Delete the parser, freeing all of the memory that it used.
     */
    void ts_parser_delete(TSParserPointer parser);

    /**
     * Set the language that the parser should use for parsing.
     *
     * Returns a boolean indicating whether or not the language was successfully
     * assigned. True means assignment succeeded. False means there was a version
     * mismatch: the language was generated with an incompatible version of the
     * Tree-sitter CLI. Check the language's version using `ts_language_version`
     * and compare it to this library's `TREE_SITTER_LANGUAGE_VERSION` and
     * `TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION` constants.
     */
    boolean ts_parser_set_language(TSParserPointer self, TSLanguagePointer language);

    /**
     * Use the parser to parse some source code stored in one contiguous buffer.
     * The first two parameters are the same as in the `ts_parser_parse` function
     * above. The second two parameters indicate the location of the buffer and its
     * length in bytes.
     */
    TSTreePointer ts_parser_parse_string(TSParserPointer self, TSTreePointer old_tree, String string, int length);

    /**
     * Get the root node of the syntax tree.
     */
    TSNode.ByValue ts_tree_root_node(TSTreePointer self);

    /**
     * Delete the syntax tree, freeing all of the memory that it used.
     */
    void ts_tree_delete(TSTreePointer self);


    /******************/
    /* Section - Node */
    /******************/

    /**
     * Get the node's type as a null-terminated string.
     */
    String ts_node_type(TSNode.ByValue node);

    /**
     * Get the node's type as a numerical id.
     */
    int ts_node_symbol(TSNode.ByValue node);

    /**
     * Get the node's start byte.
     */
    int ts_node_start_byte(TSNode.ByValue node);

    /**
     * Get the node's end byte.
     */
    int ts_node_end_byte(TSNode.ByValue node);

    /**
     * Check if the node is *named*. Named nodes correspond to named rules in the
     * grammar, whereas *anonymous* nodes correspond to string literals in the
     * grammar.
     */
    boolean ts_node_is_named(TSNode.ByValue node);

    /**
     * Get the node's child at the given index, where zero represents the first
     * child.
     */
    TSNode.ByValue ts_node_child(TSNode.ByValue node, int index);

    /**
     * Get the node's number of children.
     */
    int ts_node_child_count(TSNode.ByValue node);

    /**
     * Get the node's *named* child at the given index.
     *
     * See also `ts_node_is_named`.
     */
    TSNode.ByValue ts_node_named_child(TSNode.ByValue node, int index);

    /**
     * Get the node's number of *named* children.
     *
     * See also `ts_node_is_named`.
     */
    int ts_node_named_child_count(TSNode.ByValue node);

    /**
     * Get the node's next / previous *named* sibling.
     */
    TSNode.ByValue ts_node_next_named_sibling(TSNode.ByValue node);
    TSNode.ByValue ts_node_prev_named_sibling(TSNode.ByValue node);

    /**
     * Get an S-expression representing the node as a string.
     *
     * This string is allocated with `malloc` and the caller is responsible for
     * freeing it using `free`.
     */
    String ts_node_string(TSNode.ByValue node);



    /************************/
    /* Section - TreeCursor */
    /************************/

    /**
     * Create a new tree cursor starting from the given node.
     *
     * A tree cursor allows you to walk a syntax tree more efficiently than is
     * possible using the `TSNode` functions. It is a mutable object that is always
     * on a certain syntax node, and can be moved imperatively to different nodes.
     */
    TSTreeCursor.ByValue ts_tree_cursor_new(TSNode.ByValue node);

    /**
     * Delete a tree cursor, freeing all of the memory that it used.
     */
    void ts_tree_cursor_delete(TSTreeCursor.ByReference cursor);

    /**
     * Re-initialize a tree cursor to start at a different node.
     */
    void ts_tree_cursor_reset(TSTreeCursor.ByReference cursor, TSNode node);

    /**
     * Get the tree cursor's current node.
     */
    TSNode.ByValue ts_tree_cursor_current_node(TSTreeCursor.ByReference cursor);

    /**
     * Get the field name of the tree cursor's current node.
     *
     * This returns `NULL` if the current node doesn't have a field.
     * See also `ts_node_child_by_field_name`.
     */
    String ts_tree_cursor_current_field_name(TSTreeCursor.ByReference cursor);

    /**
     * Get the field name of the tree cursor's current node.
     *
     * This returns zero if the current node doesn't have a field.
     * See also `ts_node_child_by_field_id`, `ts_language_field_id_for_name`.
     */
    int ts_tree_cursor_current_field_id(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the parent of its current node.
     *
     * This returns `true` if the cursor successfully moved, and returns `false`
     * if there was no parent node (the cursor was already on the root node).
     */
    boolean ts_tree_cursor_goto_parent(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the next sibling of its current node.
     *
     * This returns `true` if the cursor successfully moved, and returns `false`
     * if there was no next sibling node.
     */
    boolean ts_tree_cursor_goto_next_sibling(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the first child of its current node.
     *
     * This returns `true` if the cursor successfully moved, and returns `false`
     * if there were no children.
     */
    boolean ts_tree_cursor_goto_first_child(TSTreeCursor.ByReference cursor);

    /**
     * Move the cursor to the first child of its current node that extends beyond
     * the given byte offset.
     *
     * This returns the index of the child node if one was found, and returns -1
     * if no such child was found.
     */
    long ts_tree_cursor_goto_first_child_for_byte(TSTreeCursor.ByReference cursor, int pos);

    TSTreeCursor.ByReference ts_tree_cursor_copy(TSTreeCursor.ByReference cursor);



    /**********************/
    /* Section - Language */
    /**********************/

    /**
     * Get the number of distinct node types in the language.
     */
    int ts_language_symbol_count(TSLanguagePointer language);

    /**
     * Get a node type string for the given numerical id.
     */
    String ts_language_symbol_name(TSLanguagePointer language, int symbol);


    /**
     * Check whether the given node type id belongs to named nodes, anonymous nodes,
     * or a hidden nodes.
     *
     * See also `ts_node_is_named`. Hidden nodes are never returned from the API.
     */
    int ts_language_symbol_type(TSLanguagePointer language, int symbol);

}
