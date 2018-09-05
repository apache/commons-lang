package com.hiukwok;


import org.apache.commons.lang3.StringUtils;

import java.text.Normalizer;

public class test {

    public static void main(String[] args) {

        String text = "\u0130x\u0130x";
        String searchString = "i";
        String normalized = Normalizer.normalize(text, Normalizer.Form.NFD);
//        As u0130 got normalized to two char, which one of it is lower case of [i].
//        Which would lead to mismatch happen.
        System.out.println("Before: " + normalized);
//        Expected: Stay un-change, it doesn't
        System.out.println("After: " + StringUtils.replaceIgnoreCase(text, searchString, ""));
    }
}

