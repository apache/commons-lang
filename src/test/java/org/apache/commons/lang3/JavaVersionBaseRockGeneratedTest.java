package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class JavaVersionBaseRockGeneratedTest {

    //BaseRock generated method id: ${getWhenVersionStrIsNull}, hash: A45BDD5CD82EB1BE318C0909B5B423B3
    @Test()
    void getWhenVersionStrIsNull() {
        /* Branches:
         * (versionStr == null) : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase0_9}, hash: F02868810AE6B3DF023DA45BC3218803
    @Test()
    void getWhenSwitchVersionStrCase0_9() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "0.9") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("0.9");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_0_9)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_1}, hash: A75A0E4CE501B83527E3AE246F825E67
    @Test()
    void getWhenSwitchVersionStrCase1_1() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.1") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_1)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_2}, hash: 303927EB53486AC1A32DD69451C2A041
    @Test()
    void getWhenSwitchVersionStrCase1_2() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.2") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_2)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_3}, hash: 7C2599159AC51554EB3F6437A7A57B19
    @Test()
    void getWhenSwitchVersionStrCase1_3() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.3") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.3");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_3)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_4}, hash: 87CC2A4A1C59ED5720191D2D5365AE2D
    @Test()
    void getWhenSwitchVersionStrCase1_4() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.4") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.4");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_4)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_5}, hash: 256CC787EFC702C4DC2689935CB929ED
    @Test()
    void getWhenSwitchVersionStrCase1_5() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.5") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.5");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_5)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_6}, hash: 9C4277E9281B2EA2566C9394100BA65E
    @Test()
    void getWhenSwitchVersionStrCase1_6() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.6") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.6");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_6)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_7}, hash: 70EE5177576215988BAC3481B92AD953
    @Test()
    void getWhenSwitchVersionStrCase1_7() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.7") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.7");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_7)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase1_8}, hash: 557C32C2DD24BC4EB6F8DAFEAA93FE35
    @Test()
    void getWhenSwitchVersionStrCase1_8() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "1.8") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("1.8");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_8)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase10}, hash: 8E6316E7D0A5A4707C05645E4926AA64
    @Test()
    void getWhenSwitchVersionStrCase10() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "10") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("10");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_10)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase11}, hash: E8F4C659CBAC6520A613D32074A6FEC2
    @Test()
    void getWhenSwitchVersionStrCase11() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "11") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("11");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_11)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase12}, hash: D7A22BD915996F1D3699051B2C60FCB2
    @Test()
    void getWhenSwitchVersionStrCase12() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "12") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("12");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_12)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase13}, hash: F53D3E1DA54BE65953B0B4B6DE132439
    @Test()
    void getWhenSwitchVersionStrCase13() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "13") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("13");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_13)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase14}, hash: 9313D0B362EDCF1E10D14EC4120264A1
    @Test()
    void getWhenSwitchVersionStrCase14() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "14") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("14");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_14)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase15}, hash: 6892426BA5C2EC0ACD7DBB7D086905DB
    @Test()
    void getWhenSwitchVersionStrCase15() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "15") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("15");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_15)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase16}, hash: 97BC28E91259336992552D409116BD27
    @Test()
    void getWhenSwitchVersionStrCase16() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "16") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("16");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_16)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase17}, hash: BD04C42204DA2876D1ED188B5C2382E9
    @Test()
    void getWhenSwitchVersionStrCase17() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "17") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("17");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_17)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase18}, hash: B3C114BFAAE8D570FF72E76A4283EB40
    @Test()
    void getWhenSwitchVersionStrCase18() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "18") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("18");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_18)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase19}, hash: 3CD7E3E037CEE65DA19A8ECC268CD11A
    @Test()
    void getWhenSwitchVersionStrCase19() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "19") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("19");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_19)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase20}, hash: ACEBA74EC5C4FAD91713378C8E1DD242
    @Test()
    void getWhenSwitchVersionStrCase20() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "20") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("20");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_20)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase21}, hash: E218DC642F2E81C988EF12F13A7DF89C
    @Test()
    void getWhenSwitchVersionStrCase21() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "21") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("21");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_21)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase22}, hash: B2BCAE8A76FB55E07C5BB1A00A7E8FDB
    @Test()
    void getWhenSwitchVersionStrCase22() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "22") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("22");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_22)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase23}, hash: 4589A5C5ED01BF8648891775E5E157ED
    @Test()
    void getWhenSwitchVersionStrCase23() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "23") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("23");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_23)));
    }

    //BaseRock generated method id: ${getWhenSwitchVersionStrCase9}, hash: 8B6F746F64DF9998AF7854573C1ABAD2
    @Test()
    void getWhenSwitchVersionStrCase9() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = "9") : true
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("9");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_9)));
    }

    //BaseRock generated method id: ${getWhenFloatParseFloatVersionStrSubstringFirstCommaPlus1EndGreaterThan_9f}, hash: 3F87C03D08F42D27A09B09134B57D908
    @Disabled()
    @Test()
    void getWhenFloatParseFloatVersionStrSubstringFirstCommaPlus1EndGreaterThan_9f() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = default) : true
         * (!value.contains(".")) : true  #  inside toFloatVersion method
         * (v - 1. < 1.) : true
         * (Float.parseFloat(versionStr.substring(firstComma + 1, end)) > .9f) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("versionStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_RECENT)));
    }

    //BaseRock generated method id: ${getWhenVNotGreaterThan10}, hash: C9459A11D8D24029256F020CBE43F80F
    @Disabled()
    @Test()
    void getWhenVNotGreaterThan10() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = default) : true
         * (!value.contains(".")) : false  #  inside toFloatVersion method
         * (toParse.length >= 2) : true  #  inside toFloatVersion method
         * (v - 1. < 1.) : false
         * (v > 10) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("versionStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getWhenVMinus1_LessThan1_AndFloatParseFloatVersionStrSubstringFirstCommaPlus1EndNotGreaterThan_9f}, hash: 5C1F6E0CC38A5E4EE2196BA0FAA7C18E
    @Disabled()
    @Test()
    void getWhenVMinus1_LessThan1_AndFloatParseFloatVersionStrSubstringFirstCommaPlus1EndNotGreaterThan_9f() {
        /* Branches:
         * (versionStr == null) : false
         * (switch(versionStr) = default) : true
         * (!value.contains(".")) : false  #  inside toFloatVersion method
         * (toParse.length >= 2) : false  #  inside toFloatVersion method
         * (v - 1. < 1.) : true
         * (Float.parseFloat(versionStr.substring(firstComma + 1, end)) > .9f) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.get("versionStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getJavaVersionWhenVersionStrIsNull}, hash: F37592C0B0F70F9176768C9F603AC909
    @Test()
    void getJavaVersionWhenVersionStrIsNull() {
        /* Branches:
         * (versionStr == null) : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase0_9}, hash: 4309A874E26BD4A1C9FF3D1E0F1CD568
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase0_9() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "0.9") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("0.9");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_0_9)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_1}, hash: B73E8952FB4F631833C0EF90AB04FE45
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_1() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.1") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_1)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_2}, hash: 91C95867C01EEE6533915E8F1E2898DA
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_2() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.2") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_2)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_3}, hash: AFD7D17FD47DC2985E454946B8F78199
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_3() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.3") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.3");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_3)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_4}, hash: FB613E4FA81BD7E773B06E4EB28D5397
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_4() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.4") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.4");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_4)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_5}, hash: DEA5F3DE2C3792C392DAFD5856689665
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_5() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.5") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.5");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_5)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_6}, hash: 0AFD2D23F8CA2B690AFCD9E4C68EAE9F
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_6() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.6") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.6");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_6)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_7}, hash: 278E311B76ACD7A95F8C40780BDF5F0E
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_7() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.7") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.7");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_7)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase1_8}, hash: 323814CB226D9D841C8DC485129F5F4E
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase1_8() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "1.8") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("1.8");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_1_8)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase10}, hash: 6D9E8DE26F1C52BE20456532082A2C3B
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase10() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "10") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("10");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_10)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase11}, hash: DCF8F599A84858E29E096F885A9873E5
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase11() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "11") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("11");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_11)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase12}, hash: 7501989570F9A90E769CC8650C4B4351
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase12() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "12") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("12");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_12)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase13}, hash: 8C409B7A1AFDD32962691341FF856D4E
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase13() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "13") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("13");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_13)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase14}, hash: C467A6312FA6D8C75538DB3E5A122BF3
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase14() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "14") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("14");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_14)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase15}, hash: 61AD68041B5C084B713716FD5304F73A
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase15() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "15") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("15");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_15)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase16}, hash: 2F74DD2DAB292F3D26F78BE24CF8B05C
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase16() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "16") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("16");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_16)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase17}, hash: DDABE672C27357251B54D7998555EF47
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase17() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "17") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("17");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_17)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase18}, hash: 7FF03A0D00AB22A31232C6A460085871
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase18() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "18") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("18");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_18)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase19}, hash: 3A6BDC58139F91BCB4A2D92688A1579A
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase19() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "19") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("19");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_19)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase20}, hash: E0ACA12AEE5678BE3A340E9E00389076
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase20() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "20") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("20");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_20)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase21}, hash: E81FA566C05594A27C3F30FDBB223AEE
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase21() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "21") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("21");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_21)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase22}, hash: 1779B06FDE2A78400D8C4C294A013DE4
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase22() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "22") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("22");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_22)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase23}, hash: 07344C0463C99B7378BDFE00870E95DB
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase23() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "23") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("23");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_23)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenSwitchVersionStrCase9}, hash: 79B0226122898DAB217A71C8695A82CE
    @Test()
    void getJavaVersionWhenSwitchVersionStrCase9() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = "9") : true  #  inside get method
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("9");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_9)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenFloatParseFloatVersionStrSubstringFirstCommaPlus1EndGreaterThan_9f}, hash: 9DA92E876E35FD84173ED9490FB4EDA6
    @Disabled()
    @Test()
    void getJavaVersionWhenFloatParseFloatVersionStrSubstringFirstCommaPlus1EndGreaterThan_9f() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = default) : true  #  inside get method
         * (!value.contains(".")) : true  #  inside toFloatVersion method
         * (v - 1. < 1.) : true  #  inside get method
         * (Float.parseFloat(versionStr.substring(firstComma + 1, end)) > .9f) : true  #  inside get method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("versionStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(JavaVersion.JAVA_RECENT)));
    }

    //BaseRock generated method id: ${getJavaVersionWhenVNotGreaterThan10}, hash: A01778660AB67ECF3C8CD69C60C848E9
    @Disabled()
    @Test()
    void getJavaVersionWhenVNotGreaterThan10() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = default) : true  #  inside get method
         * (!value.contains(".")) : false  #  inside toFloatVersion method
         * (toParse.length >= 2) : true  #  inside toFloatVersion method
         * (v - 1. < 1.) : false  #  inside get method
         * (v > 10) : false  #  inside get method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("versionStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getJavaVersionWhenVMinus1_LessThan1_AndFloatParseFloatVersionStrSubstringFirstCommaPlus1EndNotGreaterThan_9f}, hash: CABB23FBA994D20EA44D1436C6130C05
    @Disabled()
    @Test()
    void getJavaVersionWhenVMinus1_LessThan1_AndFloatParseFloatVersionStrSubstringFirstCommaPlus1EndNotGreaterThan_9f() {
        /* Branches:
         * (versionStr == null) : false  #  inside get method
         * (switch(versionStr) = default) : true  #  inside get method
         * (!value.contains(".")) : false  #  inside toFloatVersion method
         * (toParse.length >= 2) : false  #  inside toFloatVersion method
         * (v - 1. < 1.) : true  #  inside get method
         * (Float.parseFloat(versionStr.substring(firstComma + 1, end)) > .9f) : false  #  inside get method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        JavaVersion result = JavaVersion.getJavaVersion("versionStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitTest}, hash: B3234A9724D56EFDCFC2F48AC87B6C2F
    @Test()
    void splitTest() {
        //Act Statement(s)
        String[] result = JavaVersion.split("A");
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${atLeastWhenThisValueGreaterThanOrEqualsToRequiredVersionValue}, hash: DC1E21562905226B4BE6452A41466DC8
    @Disabled()
    @Test()
    void atLeastWhenThisValueGreaterThanOrEqualsToRequiredVersionValue() {
        /* Branches:
         * (this.value >= requiredVersion.value) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        JavaVersion target = JavaVersion.valueOf("name1");
        //Act Statement(s)
        boolean result = target.atLeast(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${atLeastWhenThisValueLessThanRequiredVersionValue}, hash: 2D339A29554E9EBA200E12E4798C7E29
    @Disabled()
    @Test()
    void atLeastWhenThisValueLessThanRequiredVersionValue() {
        /* Branches:
         * (this.value >= requiredVersion.value) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        JavaVersion target = JavaVersion.valueOf("JAVA_19");
        //Act Statement(s)
        boolean result = target.atLeast(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${atMostWhenThisValueLessThanOrEqualsToRequiredVersionValue}, hash: FE16EB34039BD2308D6FF246BA36047A
    @Disabled()
    @Test()
    void atMostWhenThisValueLessThanOrEqualsToRequiredVersionValue() {
        /* Branches:
         * (this.value <= requiredVersion.value) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        JavaVersion target = JavaVersion.valueOf("JAVA_19");
        //Act Statement(s)
        boolean result = target.atMost(JavaVersion.JAVA_1_8);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${atMostWhenThisValueGreaterThanRequiredVersionValue}, hash: D16AD93710B85615914D1D939D0FF766
    @Disabled()
    @Test()
    void atMostWhenThisValueGreaterThanRequiredVersionValue() {
        /* Branches:
         * (this.value <= requiredVersion.value) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        JavaVersion target = JavaVersion.valueOf("name1");
        //Act Statement(s)
        boolean result = target.atMost(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: A5C6EF1D44DD9735D3DED89DAB1E98CA
    @Disabled()
    @Test()
    void toStringTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        JavaVersion target = JavaVersion.valueOf("JAVA_16");
        //Act Statement(s)
        String result = target.toString();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }
}
