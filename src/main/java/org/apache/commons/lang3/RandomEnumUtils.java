package org.apache.commons.lang3;

import java.util.List;

/**
 * Utility library to provide a means to operate with random enum
 * instances of a certain type.
 *
 * @since 3.4
 * @version $Id$
 */
public class RandomEnumUtils {

    /**
     * Returns a random instance of an Enum type
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @return random instance of an Enum type
     */
    public static <E extends Enum<E>> E randomEnum(final Class<E> enumClass) {
        List<E> enumList = EnumUtils.getEnumList(enumClass);
        return enumList.get(RandomUtils.nextInt(0, enumList.size()));
    }
}
