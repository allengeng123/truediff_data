package truediff.compat.treesitter;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"context", "id", "tree"})
public class TSNode extends Structure {
    public static class ByValue extends TSNode implements Structure.ByValue { }

    public final int[] context = new int[4];
    public Pointer id;
    public TSTreePointer tree;
}
