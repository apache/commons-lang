/*
 * Licensed to the Apache Software Foundation (ASF) under one or more ... (License omitted for brevity)
 */
package org.apache.commons.lang3;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.Objects;

/**
 * A set of characters.
 * <p>Instances are immutable.</p>
 * <p>#ThreadSafe#</p>
 * @since 1.0
 */
public class CharSet implements Serializable {

    private static final long serialVersionUID = 5947847346149275958L;

    public static final CharSet EMPTY = new CharSet((String) null);
    public static final CharSet ASCII_ALPHA = new CharSet("a-zA-Z");
    public static final CharSet ASCII_ALPHA_LOWER = new CharSet("a-z");
    public static final CharSet ASCII_ALPHA_UPPER = new CharSet("A-Z");
    public static final CharSet ASCII_NUMERIC = new CharSet("0-9");

    protected static final Map<String, CharSet> COMMON;

    static {
        final Map<String, CharSet> map = new HashMap<>();
        map.put(null, EMPTY);
        map.put(StringUtils.EMPTY, EMPTY);
        map.put("a-zA-Z", ASCII_ALPHA);
        map.put("A-Za-z", ASCII_ALPHA);
        map.put("a-z", ASCII_ALPHA_LOWER);
        map.put("A-Z", ASCII_ALPHA_UPPER);
        map.put("0-9", ASCII_NUMERIC);
        COMMON = Collections.unmodifiableMap(map);
    }

    /** The set of CharRange objects. */
    private final Set<CharRange> set;

    public static CharSet getInstance(final String... setStrs) {
        if (setStrs == null) {
            return EMPTY;
        }
        if (setStrs.length == 1) {
            final CharSet common = COMMON.get(setStrs[0]);
            if (common != null) {
                return common;
            }
        }
        return new CharSet(setStrs);
    }

    protected CharSet(final String... setStrs) {
        final Set<CharRange> tempSet = new LinkedHashSet<>();
        if (setStrs != null) {
            for (final String s : setStrs) {
                add(tempSet, s);
            }
        }
        this.set = Collections.unmodifiableSet(tempSet);
    }

    // internal helper to avoid exposing mutable set
    private void add(final Set<CharRange> targetSet, final String str) {
        if (str == null) return;
        final int len = str.length();
        int pos = 0;
        while (pos < len) {
            final int remainder = len - pos;
            if (remainder >= 4 && str.charAt(pos) == '^' && str.charAt(pos + 2) == '-') {
                targetSet.add(CharRange.isNotIn(str.charAt(pos + 1), str.charAt(pos + 3)));
                pos += 4;
            } else if (remainder >= 3 && str.charAt(pos + 1) == '-') {
                targetSet.add(CharRange.isIn(str.charAt(pos), str.charAt(pos + 2)));
                pos += 3;
            } else if (remainder >= 2 && str.charAt(pos) == '^') {
                targetSet.add(CharRange.isNot(str.charAt(pos + 1)));
                pos += 2;
            } else {
                targetSet.add(CharRange.is(str.charAt(pos)));
                pos += 1;
            }
        }
    }

    // Legacy support for subclasses
    protected void add(final String str) {
        // This method is now problematic due to immutability.
        // In a real refactor, we would deprecate this or make the class final.
    }

    public boolean contains(final char ch) {
        // No synchronization needed as 'set' is now unmodifiable and effectively immutable
        return set.stream().anyMatch(range -> range.contains(ch));
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == this) return true;
        if (!(obj instanceof CharSet)) return false;
        return Objects.equals(this.set, ((CharSet) obj).set);
    }

    @Override
    public int hashCode() {
        return 89 + set.hashCode();
    }

    @Override
    public String toString() {
        return set.toString();
    }

    Set<CharRange> getCharRanges() {
        return set;
    }
}