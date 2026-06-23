package org.apache.commons.lang3;

import java.io.IOException;
import java.util.Iterator;
import java.util.function.Supplier;

import org.apache.commons.lang3.exception.UncheckedException;
import org.apache.commons.lang3.function.FailableBiConsumer;

public final class AppendableJoiner<T> {

    public static final class Builder<T> implements Supplier<AppendableJoiner<T>> {

        private CharSequence prefix;
        private CharSequence suffix;
        private CharSequence delimiter;
        private FailableBiConsumer<Appendable, T, IOException> appender;

        Builder() {}

        @Override
        public AppendableJoiner<T> get() {
            return new AppendableJoiner<>(prefix, suffix, delimiter, appender);
        }

        public Builder<T> setPrefix(CharSequence prefix) {
            this.prefix = prefix;
            return this;
        }

        public Builder<T> setSuffix(CharSequence suffix) {
            this.suffix = suffix;
            return this;
        }

        public Builder<T> setDelimiter(CharSequence delimiter) {
            this.delimiter = delimiter;
            return this;
        }

        public Builder<T> setElementAppender(FailableBiConsumer<Appendable, T, IOException> appender) {
            this.appender = appender;
            return this;
        }
    }

    public static <T> Builder<T> builder() {
        return new Builder<>();
    }

    // ---------------- INSTANCE FIELDS ----------------

    private final CharSequence prefix;
    private final CharSequence suffix;
    private final CharSequence delimiter;
    private final FailableBiConsumer<Appendable, T, IOException> appender;

    private AppendableJoiner(CharSequence prefix,
                             CharSequence suffix,
                             CharSequence delimiter,
                             FailableBiConsumer<Appendable, T, IOException> appender) {

        this.prefix = prefix == null ? "" : prefix;
        this.suffix = suffix == null ? "" : suffix;
        this.delimiter = delimiter == null ? "" : delimiter;

        this.appender = (appender != null)
                ? appender
                : (a, e) -> a.append(String.valueOf(e));
    }

    // ---------------- CORE JOIN METHOD (ITERABLE) ----------------

    public <A extends Appendable> A join(A a, Iterable<T> elements) {
        try {
            a.append(prefix);

            if (elements != null) {
                Iterator<T> it = elements.iterator();

                if (it.hasNext()) {
                    appender.accept(a, it.next());
                }

                while (it.hasNext()) {
                    a.append(delimiter);
                    appender.accept(a, it.next());
                }
            }

            a.append(suffix);
            return a;

        } catch (IOException e) {
            throw new UncheckedException(e);
        }
    }

    // ---------------- CORE JOIN METHOD (ARRAY) ----------------

    @SafeVarargs
    public final <A extends Appendable> A join(A a, T... elements) {
        try {
            a.append(prefix);

            if (elements != null && elements.length > 0) {
                appender.accept(a, elements[0]);

                for (int i = 1; i < elements.length; i++) {
                    a.append(delimiter);
                    appender.accept(a, elements[i]);
                }
            }

            a.append(suffix);
            return a;

        } catch (IOException e) {
            throw new UncheckedException(e);
        }
    }
}