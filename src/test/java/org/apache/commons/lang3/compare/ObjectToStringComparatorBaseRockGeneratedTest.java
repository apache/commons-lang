package org.apache.commons.lang3.compare;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ObjectToStringComparatorBaseRockGeneratedTest {

    private final Object objectMock = mock(Object.class, "o1");

    private final Object objectMock2 = mock(Object.class, "o2");

    //BaseRock generated method id: ${compareWhenO2IsNull}, hash: 0A677BF656B6B38F05D1E9D5065898FA
    @Test()
    void compareWhenO2IsNull() {
        /* Branches:
         * (o1 == null) : true
         * (o2 == null) : true
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        Object object = null;
        Object object2 = null;
        
        //Act Statement(s)
        int result = target.compare(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compareWhenO1IsNull}, hash: 47153A3E604719B665D72AF2592AC48B
    @Test()
    void compareWhenO1IsNull() {
        /* Branches:
         * (o1 == null) : true
         * (o2 == null) : false
         * (o1 == null) : true
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        Object object = null;
        Object object2 = new Object();
        
        //Act Statement(s)
        int result = target.compare(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compareWhenO1IsNotNullAndO2IsNull}, hash: 30EEEEB9B5A2891395F64117E1CD8A7C
    @Test()
    void compareWhenO1IsNotNullAndO2IsNull() {
        /* Branches:
         * (o1 == null) : false
         * (o1 == null) : false
         * (o2 == null) : true
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        Object object = new Object();
        Object object2 = null;
        
        //Act Statement(s)
        int result = target.compare(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${compareWhenString2IsNull}, hash: 0614C776CD9DB1F721708C3D49DE7367
    @Test()
    void compareWhenString2IsNull() {
        /* Branches:
         * (o1 == null) : false
         * (o1 == null) : false
         * (o2 == null) : false
         * (string1 == null) : true
         * (string2 == null) : true
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        
        //Act Statement(s)
        int result = target.compare(objectMock, objectMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compareWhenString1IsNull}, hash: 46BB5255584D711D939A004CACE18443
    @Test()
    void compareWhenString1IsNull() {
        /* Branches:
         * (o1 == null) : false
         * (o1 == null) : false
         * (o2 == null) : false
         * (string1 == null) : true
         * (string2 == null) : false
         * (string1 == null) : true
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        
        //Act Statement(s)
        int result = target.compare(objectMock, objectMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compareWhenString1IsNotNullAndString2IsNull}, hash: AF00DC0F96F4019AC91BC56A0147C029
    @Test()
    void compareWhenString1IsNotNullAndString2IsNull() {
        /* Branches:
         * (o1 == null) : false
         * (o1 == null) : false
         * (o2 == null) : false
         * (string1 == null) : false
         * (string1 == null) : false
         * (string2 == null) : true
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        
        //Act Statement(s)
        int result = target.compare(objectMock, objectMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${compareWhenString2IsNotNull}, hash: 33B91EC568DEBEDB925AEAEFA4545D6E
    @Test()
    void compareWhenString2IsNotNull() {
        /* Branches:
         * (o1 == null) : false
         * (o1 == null) : false
         * (o2 == null) : false
         * (string1 == null) : false
         * (string1 == null) : false
         * (string2 == null) : false
         */
         //Arrange Statement(s)
        ObjectToStringComparator target = new ObjectToStringComparator();
        
        //Act Statement(s)
        int result = target.compare(objectMock, objectMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }
}
