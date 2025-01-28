package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.Supplier;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class SuppliersBaseRockGeneratedTest {

    //BaseRock generated method id: ${getWhenSupplierIsNull}, hash: 83BD15EF6404BE40F983ACE964C653F0
    @Test()
    void getWhenSupplierIsNull() {
        /* Branches:
         * (supplier == null) : true
         */
         //Arrange Statement(s)
        Supplier<Object> supplier = null;
        
        //Act Statement(s)
        Object result = Suppliers.get(supplier);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getWhenSupplierIsNotNull}, hash: 2E83979DDA34831246FF52EAFB2883F2
    @Test()
    void getWhenSupplierIsNotNull() {
        /* Branches:
         * (supplier == null) : false
         */
         //Arrange Statement(s)
        Supplier<Object> supplierMock = mock(Supplier.class);
        Object object = new Object();
        doReturn(object).when(supplierMock).get();
        
        //Act Statement(s)
        Object result = Suppliers.get(supplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(supplierMock).get();
        });
    }

    //BaseRock generated method id: ${nulTest}, hash: B23E0575478F99762615D45E8C27116C
    @Test()
    void nulTest() {
        
        //Act Statement(s)
        Supplier result = Suppliers.nul();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
