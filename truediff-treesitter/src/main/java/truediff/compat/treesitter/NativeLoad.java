package truediff.compat.treesitter;

import com.sun.jna.Function;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.NativeLibrary;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class NativeLoad {
    private static final Set<Object> retainedLibs = new HashSet<>();

    public static <T extends Library> T loadLibrary(String name, Class<T> cls) {
        try {
            String libfile = Native.extractFromResourcePath(name).getAbsolutePath();
            T lib = Native.load(libfile, cls);
            retainedLibs.add(lib);
            return lib;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static Function loadFunction(String name, String funname, Class<?> cls, String... dependencies) {
        try {
            for (String dep : dependencies) {
                String depfile = Native.extractFromResourcePath(dep).getAbsolutePath();
                NativeLibrary deplib = NativeLibrary.getInstance(depfile, cls.getClassLoader());
                retainedLibs.add(deplib);
            }
            String libfile = Native.extractFromResourcePath(name).getAbsolutePath();
            NativeLibrary lib = NativeLibrary.getInstance(libfile, cls.getClassLoader());
            retainedLibs.add(lib);
            return lib.getFunction(funname);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
