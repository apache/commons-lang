package org.apache.commons.lang3.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Use this annotation to builds a String excluding the given field.
 */
@Retention(RUNTIME)
@Target(ElementType.FIELD)
public @interface ToStringExclude {

}
