package org.apache.commons.lang;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Performs basic variable interpolation on a String for variables within 
 * a Map. Variables of the form, ${var}, are supported.
 *
 * @author Ken Fitzpatrick
 * @author Henri Yandell
 */
public class Interpolation {

    // QUERY: Anyway to escape the ${..} variable so it is not interpolated?

    // TODO: Consider making these configurable?
    private static final String SYMBOLIC_VALUE_MARKER_START = "${";
    private static final String SYMBOLIC_VALUE_MARKER_END = "}";

    /**
     *  <p>
     *  Returns a String that is the result of having performed
     *  variable interpolation on <code>templateString</code>,
     *  using the value set found in <code>values</code>.
     *  </p>
     *  <p>
     *  The solution is compatible with all JDK versions
     *  where Jakarta/Commons/Lang also is supported.
     *  </p>
     *  <p>
     *  The expected format of <code>templateString</code> is:
     *<code><pre>
     *   The ${animal} jumped over the ${target}.
     *</pre></code>
     *  such that the key/value pairs found in <code>values</code>
     *  are substituted into the string at the <code>${key-name}</code> markers.
     *  In the above example, <code>valuesMap</code> could have been populated as:
     *<code><pre>
     *   Map valuesMap = HashMap();
     *   valuesMap.put( "animal", "quick brown fox" );
     *   valuesMap.put( "target", "lazy dog" );
     *   String resolvedString = StringUtils.interpolate( templateString, valuesMap );
     *</pre></code>
     *  yielding:
     *<code><pre>
     *   The quick brown fox jumped over the lazy dog.
     *</pre></code>
     *  </p>
     *  <p>
     *  The same <code>templateString</code> from the above example could be reused as:
     *<code><pre>
     *   Map valuesMap = HashMap();
     *   valuesMap.put( "animal", "cow" );
     *   valuesMap.put( "target", "moon" );
     *   String resolvedString = StringUtils.interpolate( templateString, valuesMap );
     *</pre></code>
     *  yielding:
     *<code><pre>
     *   The cow jumped over the moon.
     *</pre></code>
     *  </p>
     *  <p>
     *  The value of <code>templateString</code> is returned in an unaltered if <code>templateString</code>
     *  is null, empty, or contains no marked variables that can be resolved by the key/value pairs found in
     *  <code>valuesMap</code>, or if <code>valuesMap</code> is null, empty or has no key/value pairs that can be
     *  applied to the marked variables within <code>templateString</code>.
     *  </p>
     * @param templateString String containing any mixture of variable and non-variable
     *      content, to be used as a template for the value substitution process
     * @param valuesMap Map containing the key/value pairs to be used to resolve
     *      the values of the marked variables found within <code>templateString</code>
     * @return String
     */
    public static String interpolate( String templateString, Map valuesMap ) {
        // pre-conditions
        if ( valuesMap == null )
            return templateString;
        if ( templateString == null )
            return templateString;
        if ( templateString.length() < 1 )
            return templateString;
        if ( valuesMap.isEmpty() )
            return templateString;

        // default the returned String to the templateString
        String returnString = templateString;
        String nextKey = null;
        Object substitutionBean = null;
        String substitutionValue = null;
        String nextValueToBeSubstituted = null;

        // get a list of substitution valuesMap
        Iterator keys = valuesMap.keySet().iterator();

        while( keys.hasNext() ) {
            nextKey = ( String ) keys.next();
            substitutionValue = StringUtils.defaultString( ( String ) valuesMap.get( nextKey ) );
            nextValueToBeSubstituted = SYMBOLIC_VALUE_MARKER_START + nextKey + SYMBOLIC_VALUE_MARKER_END;

            returnString = StringUtils.replace( returnString, nextValueToBeSubstituted, substitutionValue );
        }
        return returnString;
    }


    /**
     *  <p>
     *  Returns a String that is the result of having performed variable interpolation on
     *  <code>templateString</code>, using the value set found in <code>values</code>,
     *  repeatedly until there are no changes. 
     *  </p>
     *  <p>
     *  The expected format of <code>templateString</code> is:
     *<code><pre>
     *   The ${animal} jumped over the ${target}.
     *</pre></code>
     *  such that the key/value pairs found in <code>values</code> are substituted into the string at the
     *  <code>${key-name}</code> markers.  In the above example, <code>valuesMap</code>
     *  could have been populated as:
     *<code><pre>
     *   Map valuesMap = HashMap();
     *   valuesMap.put( "animal", "${critter}" );
     *   valuesMap.put( "target", "${pet}" );
     *   valuesMap.put( "pet", "${petCharacteristic} dog" );
     *   valuesMap.put( "petCharacteristic", "lazy" );
     *   valuesMap.put( "critter", "${critterSpeed} ${critterColor} ${critterType}" );
     *   valuesMap.put( "critterSpeed", "quick" );
     *   valuesMap.put( "critterColor", "brown" );
     *   valuesMap.put( "critterType", "fox" );
     *   String resolvedString = StringUtils.interpolate( templateString, valuesMap, true );
     *</pre></code>
     *  yielding:
     *<code><pre>
     *   The quick brown fox jumped over the lazy dog.
     *</pre></code>
     *  </p>
     *  yielding:
     *<code><pre>
     *   The cow jumped over the moon.
     *</pre></code>
     *  </p>
     *  <p>
     *  The value of <code>templateString</code> is returned in an unaltered form if
     *  <code>templateString</code> is null, empty, or
     *  contains no marked variables that can be resolved by the key/value pairs found in
     *  <code>valuesMap</code>, or if <code>valuesMap</code> is null, empty or has no key/value 
     *  pairs that can be applied to the marked variables within <code>templateString</code>.
     *  </p>
     * @param templateString String containing any mixture of variable and non-variable
     *      content, to be used as a template for the value substitution process
     * @param valuesMap Map containing the key/value pairs to be used to resolve
     *      the values of the marked variables found within <code>templateString</code>
     * @return String
     */
    public static String interpolateRepeatedly(
            String templateString,
            Map valuesMap)
    {
        // pre-conditions
        if ( valuesMap == null )
            return templateString;
        if ( templateString == null )
            return templateString;
        if ( templateString.length() < 1 )
            return templateString;
        if ( valuesMap.isEmpty() )
            return templateString;

        String currentResult = templateString;
        String previousResult = null;
        while( ! StringUtils.equals( currentResult, previousResult ) )
        {
            previousResult = currentResult;
            currentResult = Interpolation.interpolate( previousResult, valuesMap );
        }

        return currentResult;
    }

}
