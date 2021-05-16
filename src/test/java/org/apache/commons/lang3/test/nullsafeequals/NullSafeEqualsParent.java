package org.apache.commons.lang3.test.nullsafeequals;

public class NullSafeEqualsParent {

    private NullSafeEqualsChild nullSafeEqualsChildPrivate = new NullSafeEqualsChild();
    public NullSafeEqualsChild nullSafeEqualsChildPublic;
    NullSafeEqualsChild nullSafeEqualsChildPackageDefault;
    private String foo = "parentFoo";

    public NullSafeEqualsChild getNullSafeEqualsChildPrivate() {
        return nullSafeEqualsChildPrivate;
    }
}
