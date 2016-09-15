package org.apache.commons.lang3.builder;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Use this annotation to exclude a field when it is null from being used by
 * the {@link ReflectionToStringBuilder}.
 */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface ToStringExcludeNullValue {

}
