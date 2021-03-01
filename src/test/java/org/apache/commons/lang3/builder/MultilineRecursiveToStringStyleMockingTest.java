package org.apache.commons.lang3.builder;

import org.junit.Before;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

import static org.mockito.Mockito.*;

//this junit test needs to call ReflectionStringBuilder
public class MultilineRecursiveToStringStyleMockingTest {

    private final String BR = System.lineSeparator();

    final WithArraysOne wa1 = new WithArraysOne();
    final WithArraysTwo wa2 = new WithArraysTwo();
    final WithArraysThree wa3 = new WithArraysThree();

    private ReflectionToStringBuilder reflectionToStringBuilder;

    private List<ReflectionToStringBuilder> reflectionToStringBuilderList;

    @Spy
    private ReflectionToStringBuilder builder1 = new ReflectionToStringBuilder(wa1, new MultilineRecursiveToStringStyle());

    @Spy
    private ReflectionToStringBuilder builder2 = new ReflectionToStringBuilder(wa2, new MultilineRecursiveToStringStyle());

    @Spy
    private ReflectionToStringBuilder builder3 = new ReflectionToStringBuilder(wa3, new MultilineRecursiveToStringStyle());


    @Before
    public void setup(){
        //Mocks are being created
        reflectionToStringBuilder = new ReflectionToStringBuilder(mock(ReflectionToStringBuilder.class));

        MockitoAnnotations.initMocks(this);
        reflectionToStringBuilderList = Arrays.asList(builder1,builder2,builder3);

    }

    @Test
    public void doubleArray() {
        wa1.doubleArray = new double[] { 1, 2 };
        final String exp = getClassPrefix(wa1) + "[" + BR
                + "  boolArray=<null>," + BR
                + "  charArray=<null>," + BR
                + "  doubleArray={" + BR
                + "    1.0," + BR
                + "    2.0" + BR
                + "  }," + BR
                + "  intArray=<null>," + BR
                + "  longArray=<null>," + BR
                + "  stringArray=<null>" + BR
                + "]";
        assertEquals(exp, builder1.toString());
    }

    @Test
    public void longArray() {
        wa2.longArray = new long[] { 1L, 2L };
        final String exp = getClassPrefix(wa2) + "[" + BR
                + "  boolArray=<null>," + BR
                + "  charArray=<null>," + BR
                + "  doubleArray=<null>," + BR
                + "  intArray=<null>," + BR
                + "  longArray={" + BR
                + "    1," + BR
                + "    2" + BR
                + "  }," + BR
                + "  stringArray=<null>" + BR
                + "]";
        assertEquals(exp, builder2.toString());
    }

    @Test
    public void stringArray() {
        wa3.stringArray = new String[] { "a", "A" };
        final String exp = getClassPrefix(wa3) + "[" + BR
                + "  boolArray=<null>," + BR
                + "  charArray=<null>," + BR
                + "  doubleArray=<null>," + BR
                + "  intArray=<null>," + BR
                + "  longArray=<null>," + BR
                + "  stringArray={" + BR
                + "    a," + BR
                + "    A" + BR
                + "  }" + BR
                + "]";
        assertEquals(exp, builder3.toString());
    }


    private String getClassPrefix(final Object object) {
        return object.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(object));
    }

    static class WithArraysOne {
        boolean[] boolArray;
        char[] charArray;
        double[] doubleArray;
        int[] intArray;
        long[] longArray;
        String[] stringArray;
    }

    static class WithArraysTwo{
        boolean[] boolArray;
        char[] charArray;
        double[] doubleArray;
        int[] intArray;
        long[] longArray;
        String[] stringArray;
    }

    static class WithArraysThree{
        boolean[] boolArray;
        char[] charArray;
        double[] doubleArray;
        int[] intArray;
        long[] longArray;
        String[] stringArray;
    }
}
