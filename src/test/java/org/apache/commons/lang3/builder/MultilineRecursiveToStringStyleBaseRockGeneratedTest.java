package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MultilineRecursiveToStringStyleBaseRockGeneratedTest {

    //BaseRock generated method id: ${appendDetail1WhenAcceptValueGetClass}, hash: 087FAB0AC6B7C3D31990A1D74A074C51
    @Test()
    void appendDetail1WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(boolean[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        boolean[] booleanArray = new boolean[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", booleanArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(boolean[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail1WhenAcceptNotValueGetClass}, hash: 5E16AF63D5E339C4E490CE79C555FD23
    @Test()
    void appendDetail1WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(boolean[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        boolean[] booleanArray = new boolean[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", booleanArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(boolean[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail3WhenAcceptValueGetClass}, hash: F43FB11BE9CD293DFA715486DC7EE01E
    @Test()
    void appendDetail3WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(byte[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", byteArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(byte[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail3WhenAcceptNotValueGetClass}, hash: 704012B9469CE2196BDD142E667F9C5D
    @Test()
    void appendDetail3WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(byte[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", byteArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(byte[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail5WhenAcceptValueGetClass}, hash: 1B4581651C02AEB5218F9E9BC776FFF3
    @Test()
    void appendDetail5WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(char[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(char[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail5WhenAcceptNotValueGetClass}, hash: 667849EEBC8E430E2BF864D57C8115E8
    @Test()
    void appendDetail5WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(char[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(char[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail8WhenAcceptValueGetClass}, hash: 3D8B5F26869F2998C11B88602519D120
    @Test()
    void appendDetail8WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(double[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", doubleArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(double[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail8WhenAcceptNotValueGetClass}, hash: D980AB6EDD8AC374A36B2AF5E1D725FF
    @Test()
    void appendDetail8WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(double[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", doubleArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(double[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail10WhenAcceptValueGetClass}, hash: 070001F56AF159E67A58E35F5E1F8F67
    @Test()
    void appendDetail10WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(float[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", floatArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(float[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail10WhenAcceptNotValueGetClass}, hash: 7DDD93B92FE5A4AD127662DD3C5562A1
    @Test()
    void appendDetail10WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(float[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", floatArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(float[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail13WhenAcceptValueGetClass}, hash: 1814F82608FCF27D22CDB93BD45CE0A4
    @Test()
    void appendDetail13WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(int[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(int[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail13WhenAcceptNotValueGetClass}, hash: 09A0B7DC5D7437E72B3B351B828BC7D1
    @Test()
    void appendDetail13WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(int[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(int[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail15WhenAcceptValueGetClass}, hash: 05AE6E1385B7D63F942D6D2EF6460580
    @Test()
    void appendDetail15WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(long[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", longArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(long[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail15WhenAcceptNotValueGetClass}, hash: 7EDDB29087E82325615283C4D8888BC7
    @Test()
    void appendDetail15WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(long[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", longArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(long[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail17WhenAcceptValueGetClass}, hash: 0CCF1577D455FE4CE7639905A10D2F19
    @Test()
    void appendDetail17WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true
         * (!String.class.equals(value.getClass())) : true
         * (accept(value.getClass())) : true
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doReturn(true).when(target).accept(Object.class);
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", object);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).accept(Object.class);
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail17WhenStringNotEqualsValueGetClassAndAcceptValueGetClass}, hash: 62D836F7935636A2FFF33F86A91D4E9C
    @Test()
    void appendDetail17WhenStringNotEqualsValueGetClassAndAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true
         * (!String.class.equals(value.getClass())) : true
         * (accept(value.getClass())) : false
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doReturn(false, true).when(target).accept(Object.class);
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", object);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target, times(2)).accept(Object.class));
    }

    //BaseRock generated method id: ${appendDetail17WhenAcceptNotValueGetClass}, hash: BCA600559B1F750C3735187234DA05AF
    @Test()
    void appendDetail17WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true
         * (!String.class.equals(value.getClass())) : true
         * (accept(value.getClass())) : false
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doReturn(false).when(target).accept(Object.class);
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", object);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target, times(2)).accept(Object.class));
    }

    //BaseRock generated method id: ${appendDetail18WhenAcceptValueGetClass}, hash: F2E75CA22828ED79A07ADFBBB6C8889F
    @Test()
    void appendDetail18WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(Object[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(Object[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail18WhenAcceptNotValueGetClass}, hash: E654F09A8A3A1492F61A2B65EE3FE497
    @Test()
    void appendDetail18WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(Object[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(Object[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail20WhenAcceptValueGetClass}, hash: 5EC18EE49224A4D06EDF7BA2ED7B2CB7
    @Test()
    void appendDetail20WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : true  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(true).when(target).accept(short[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", shortArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(short[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${appendDetail20WhenAcceptNotValueGetClass}, hash: 6622FD90FA5EBEE71C7DC1C87C20757E
    @Test()
    void appendDetail20WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true  #  inside appendDetail method
         * (!String.class.equals(value.getClass())) : true  #  inside appendDetail method
         * (accept(value.getClass())) : false  #  inside appendDetail method
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doReturn(false).when(target).accept(short[].class);
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", shortArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).accept(short[].class);
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }

    //BaseRock generated method id: ${reflectionAppendArrayDetailTest}, hash: 3E445349A1C5C29EC7072DFC4145E82F
    @Test()
    void reflectionAppendArrayDetailTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MultilineRecursiveToStringStyle target = spy(new MultilineRecursiveToStringStyle());
        doNothing().when(target).setArrayStart("{\n    ");
        doNothing().when(target).setArraySeparator(",\n    ");
        doNothing().when(target).setArrayEnd("\n  }");
        doNothing().when(target).setContentStart("[\n    ");
        doNothing().when(target).setFieldSeparator(",\n    ");
        doNothing().when(target).setContentEnd("\n  ]");
        doNothing().when(target).setArrayStart("{\n  ");
        doNothing().when(target).setArraySeparator(",\n  ");
        doNothing().when(target).setArrayEnd("\n}");
        doNothing().when(target).setContentStart("[\n  ");
        doNothing().when(target).setFieldSeparator(",\n  ");
        doNothing().when(target).setContentEnd("\n]");
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        
        //Act Statement(s)
        target.reflectionAppendArrayDetail(stringBuffer, "fieldName1", object);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).setArrayStart("{\n    ");
            verify(target).setArraySeparator(",\n    ");
            verify(target).setArrayEnd("\n  }");
            verify(target).setContentStart("[\n    ");
            verify(target).setFieldSeparator(",\n    ");
            verify(target).setContentEnd("\n  ]");
            verify(target).setArrayStart("{\n  ");
            verify(target).setArraySeparator(",\n  ");
            verify(target).setArrayEnd("\n}");
            verify(target).setContentStart("[\n  ");
            verify(target).setFieldSeparator(",\n  ");
            verify(target).setContentEnd("\n]");
        });
    }
}
