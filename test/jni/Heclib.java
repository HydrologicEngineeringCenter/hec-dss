package hec.heclib.util;

/*
 * Minimal stand-in for the real hec.heclib.util.Heclib: the class name and package
 * must match the JNI symbol naming in javaHeclib (Java_hec_heclib_util_Heclib_*).
 * We declare ONE native method so the test can load the published javaHeclib native
 * and bind a JNI symbol -- proving the artifact loads into a matching-arch JVM, its
 * dependencies resolve, and its exported JNI entry points are callable. This is the
 * JNI equivalent of the consume_hecdss C-API smoke test.
 */
public class Heclib {
    // Maps to Java_hec_heclib_util_Heclib_Hec_1zgetMessageLevel -- a side-effect-free
    // getter that self-initializes, so it is safe to call without opening a DSS file.
    public native int Hec_zgetMessageLevel(int group);

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("usage: Heclib <absolute-path-to-javaHeclib-native>");
            System.exit(2);
        }
        System.load(args[0]);                       // arch match + dependency resolution
        int level = new Heclib().Hec_zgetMessageLevel(0);  // forces JNI symbol binding
        System.out.println("JNI OK: loaded " + args[0] + "; Hec_zgetMessageLevel(0)=" + level);
        // Any UnsatisfiedLinkError / exception above propagates and exits non-zero.
    }
}
