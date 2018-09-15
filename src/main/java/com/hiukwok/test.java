package com.hiukwok;


import org.apache.commons.lang3.StringUtils;

import java.text.Normalizer;

public class test {

    public static void main(String[] args) {

        String text = "\u0130x";
        String searchString = "I";
        String normalized = Normalizer.normalize(text, Normalizer.Form.NFD);
//        As u0130 got normalized to two char, which one of it is lower case of [i].
//        Which would lead to mismatch happen.
        System.out.println("Before: " + normalized);
        System.out.println(normalized + " -> [" + normalized.charAt(0) + "," + normalized.charAt(1) +"," + normalized.charAt(2) + "]"  );
//        Expected: Stay un-change, it doesn't
        System.out.println("After: " + StringUtils.replaceIgnoreCase(text, searchString, ""));
    }
}

