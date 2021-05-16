package org.apache.commons.lang3.test.nullsafeequals;

public class NullSafeEqualsParent {

    private NullSafeEqualsChild nullSafeEqualsChildPrivate = new NullSafeEqualsChild();
    public NullSafeEqualsChild nullSafeEqualsChildPublic;
    NullSafeEqualsChild nullSafeEqualsChildPackageDefault;
    private String foo = "parentFoo";

    private static final String STATIC_FINAL_FOO = "foo";
    private static String STATIC_FOO = "foo";
    public static final String PUBLIC_STATIC_FINAL_FOO = "foo";
    public static String PUBLIC_STATIC_NULL_FOO;

    public NullSafeEqualsChild getNullSafeEqualsChildPrivate() {
        return nullSafeEqualsChildPrivate;
    }
}
