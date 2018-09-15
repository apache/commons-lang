package com.hiukwok;

import org.apache.commons.lang3.StringUtils;

public class CharToLowerCase {


    /**
     * Test
     * @param str
     * @return
     */
    public static String toLowerCase (String str) {
        StringBuilder sb = new StringBuilder();
        for (char ch : str.toCharArray()){
            sb.append(ch);
        }
        return sb.toString();
    }



    public static void main(String[] args) {
//        String text = "\u0130xix";
//        String str = "\u0130\u0130";
//        System.out.println(StringUtils.replaceIgnoreCase(text,str, ""));
    }
}
