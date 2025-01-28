package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableBooleanBaseRockGeneratedTest {

    //BaseRock generated method id: ${booleanValueWhenValue}, hash: F2BC9647978A0685918374DF5F63E8DA
    @Test()
    void booleanValueWhenValue() {
        /* Branches:
         * (value) : true
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(true);
        
        //Act Statement(s)
        boolean result = target.booleanValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${booleanValueWhenNotValue}, hash: 2CFB6E1050C5D53888277E9816E79571
    @Test()
    void booleanValueWhenNotValue() {
        /* Branches:
         * (value) : false
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        boolean result = target.booleanValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${compareToTest}, hash: 584EB20EA4544AFED13ACC87CD6EA357
    @Test()
    void compareToTest() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        MutableBoolean mutableBoolean = new MutableBoolean(false);
        
        //Act Statement(s)
        int result = target.compareTo(mutableBoolean);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfMutableBoolean}, hash: B5C075B0B922FACCF8F899F8D2CC597F
    @Test()
    void equalsWhenObjNotInstanceOfMutableBoolean() {
        /* Branches:
         * (obj instanceof MutableBoolean) : false
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenValueEqualsObjBooleanValue}, hash: FD49766E3078B1558872DBFDE9F9CBE0
    @Test()
    void equalsWhenValueEqualsObjBooleanValue() {
        /* Branches:
         * (obj instanceof MutableBoolean) : true
         * (value == ((MutableBoolean) obj).booleanValue()) : true
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        MutableBoolean mutableBoolean = new MutableBoolean();
        
        //Act Statement(s)
        boolean result = target.equals(mutableBoolean);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenValueNotEqualsObjBooleanValue}, hash: 6E698E695AE74D87556329E55B235AD0
    @Test()
    void equalsWhenValueNotEqualsObjBooleanValue() {
        /* Branches:
         * (obj instanceof MutableBoolean) : true
         * (value == ((MutableBoolean) obj).booleanValue()) : false
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(true);
        MutableBoolean mutableBoolean = new MutableBoolean();
        
        //Act Statement(s)
        boolean result = target.equals(mutableBoolean);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: F6B6E9785C87F5AB5BE5DAA5F966279A
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        Boolean result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isFalseWhenNotValue}, hash: FBBDC6A511B4A269BD62A9D0B2A1EB39
    @Test()
    void isFalseWhenNotValue() {
        /* Branches:
         * (!value) : true
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        boolean result = target.isFalse();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isFalseWhenValue}, hash: 3DB4B58E54C35DA8166A35FCDE6AA0FC
    @Test()
    void isFalseWhenValue() {
        /* Branches:
         * (!value) : false
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(true);
        
        //Act Statement(s)
        boolean result = target.isFalse();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isTrueWhenValue}, hash: 106AB193D12B3E55F02BB8BA68E13E51
    @Test()
    void isTrueWhenValue() {
        /* Branches:
         * (value) : true
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(true);
        
        //Act Statement(s)
        boolean result = target.isTrue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isTrueWhenNotValue}, hash: DA8344D5B4D97E734B423A9446293AEA
    @Test()
    void isTrueWhenNotValue() {
        /* Branches:
         * (value) : false
         */
         //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        boolean result = target.isTrue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setFalseTest}, hash: 13713E3AB75FB6726F3DE8286893E02B
    @Test()
    void setFalseTest() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        target.setFalse();
    }

    //BaseRock generated method id: ${setTrueTest}, hash: 049B27E5399A7B73242599D2FD9C42B5
    @Test()
    void setTrueTest() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        target.setTrue();
    }

    //BaseRock generated method id: ${setValueTest}, hash: 0D9548677A910A559F22441B9E2E484C
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        target.setValue(false);
    }

    //BaseRock generated method id: ${setValue1Test}, hash: E60EFC19B4C5C6BC89C0E3CD0E1A31C3
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        target.setValue(false);
    }

    //BaseRock generated method id: ${toBooleanTest}, hash: 664E9398190B6674AB0CFA51E861D44B
    @Test()
    void toBooleanTest() {
        //Arrange Statement(s)
        MutableBoolean target = spy(new MutableBoolean(false));
        doReturn(false).when(target).booleanValue();
        
        //Act Statement(s)
        Boolean result = target.toBoolean();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).booleanValue();
        });
    }

    //BaseRock generated method id: ${toStringTest}, hash: 81B4DA8E593FEBE1686F5EB47EAC1B98
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableBoolean target = new MutableBoolean(false);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("false")));
    }
}
