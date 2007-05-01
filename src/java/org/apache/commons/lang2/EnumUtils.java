package org.apache.commons.lang2;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Utility class for java.lang.Enum as found in Java 5.
 *
 * Lang1's EnumUtils.getEnum(Class,String) should be directly replaced by java.lang.Enum.valueOf(Class,String)
 * Lang1's EnumUtils.getEnumList(Class) is now the deprecated EnumUtils.asList(Class)
 */
public class EnumUtils {

    public static <E extends Enum<E>> Map<String, E> getEnumMap(Class<E> enumClass) {
        HashMap map = new HashMap<String, E>();
        Iterator itr = EnumSet.allOf(enumClass).iterator();
        while(itr.hasNext()) {
          Enum enm = (Enum) itr.next();
          map.put( enm.name(), enm );
        }
        return map;
    }
                  
    /**
     * @deprecated as coders should move to java.util.EnumSet.allOf(Class)
     */
    public static <E extends Enum<E>> List<E> asList(Class<E> enumClass) {
        return new ArrayList<E>( EnumSet.allOf(enumClass) );
    }
                      
    public static <E extends Enum<E>> Iterator<E> iterator(Class<E> enumClass) {
        return EnumSet.allOf(enumClass).iterator();
    }

}
