package org.apache.commons.lang3.builder;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.Arrays;
import java.util.Collection;
import java.lang.reflect.Modifier;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import java.lang.reflect.AccessibleObject;
import java.util.Objects;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ArraySorter;
import java.util.HashSet;
import java.util.Set;
import java.lang.reflect.Field;
import static org.hamcrest.MatcherAssert.assertThat;
import java.util.Comparator;
import static org.hamcrest.Matchers.*;
import org.junit.jupiter.params.provider.CsvSource;
import org.apache.commons.lang3.Validate;
import static org.junit.jupiter.api.Assertions.*;
import org.apache.commons.lang3.ObjectUtils;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class HashCodeBuilderBaseRockGeneratedTest {

    private HashCodeBuilder builder;

    @BeforeEach
    void setUp() {
        builder = new HashCodeBuilder();
    }

    //BaseRock generated method id: ${testDefaultConstructor}, hash: D7005DBB83AD0EB00019F5B0276EE2A3
    @Test
    void testDefaultConstructor() {
        assertNotNull(builder);
        assertEquals(17, builder.toHashCode());
    }

    //BaseRock generated method id: ${testCustomConstructor}, hash: 8458FDFB33D635563BF6A5F3F56CAE3F
    @Test
    void testCustomConstructor() {
        HashCodeBuilder customBuilder = new HashCodeBuilder(41, 47);
        assertNotNull(customBuilder);
        assertEquals(41, customBuilder.toHashCode());
    }

    //BaseRock generated method id: ${testCustomConstructorWithInvalidArguments}, hash: D36304BA7D82CDE0015C51496650CF8F
    @Test
    void testCustomConstructorWithInvalidArguments() {
        assertThrows(IllegalArgumentException.class, () -> new HashCodeBuilder(42, 47));
        assertThrows(IllegalArgumentException.class, () -> new HashCodeBuilder(41, 48));
    }

    //BaseRock generated method id: ${testAppendBoolean}, hash: 06B8000FDF0BDB61C556B98EFA4FAC50
    @Test
    void testAppendBoolean() {
        assertEquals(1217, builder.append(true).toHashCode());
        assertEquals(45125, builder.append(false).toHashCode());
    }

    //BaseRock generated method id: ${testAppendBooleanArray}, hash: DCBB7332F1014B1419F58ABED9EDCE3A
    @Test
    void testAppendBooleanArray() {
        assertEquals(1217, builder.append(new boolean[] { true }).toHashCode());
        assertEquals(45125, builder.append(new boolean[] { true, false }).toHashCode());
        assertEquals(1670625, builder.append((boolean[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendByte}, hash: 5E6C1CFD73733E971A8872304E7D8714
    @Test
    void testAppendByte() {
        assertEquals(666, builder.append((byte) 10).toHashCode());
    }

    //BaseRock generated method id: ${testAppendByteArray}, hash: 95872DA43A8E0B114A593F3CCB1F5A19
    @Test
    void testAppendByteArray() {
        assertEquals(666, builder.append(new byte[] { 10 }).toHashCode());
        assertEquals(24642, builder.append(new byte[] { 10, 20 }).toHashCode());
        assertEquals(911754, builder.append((byte[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendChar}, hash: 6AD522F88BC409436CBD28428378C948
    @Test
    void testAppendChar() {
        assertEquals(666, builder.append('a').toHashCode());
    }

    //BaseRock generated method id: ${testAppendCharArray}, hash: 53F5940625ECF49647C0051F642D5CDA
    @Test
    void testAppendCharArray() {
        assertEquals(666, builder.append(new char[] { 'a' }).toHashCode());
        assertEquals(24642, builder.append(new char[] { 'a', 'b' }).toHashCode());
        assertEquals(911754, builder.append((char[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendDouble}, hash: AB7D2E66711739513CB931CCFE9D8AFE
    @Test
    void testAppendDouble() {
        assertEquals(1073741825, builder.append(1.0).toHashCode());
    }

    //BaseRock generated method id: ${testAppendDoubleArray}, hash: 477A85A5B214F30536F8E030CFCDB847
    @Test
    void testAppendDoubleArray() {
        assertEquals(1073741825, builder.append(new double[] { 1.0 }).toHashCode());
        assertEquals(39728713, builder.append(new double[] { 1.0, 2.0 }).toHashCode());
        assertEquals(1470025, builder.append((double[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendFloat}, hash: 3A7D3F66FF45E9D6C8EDA9E51EF46841
    @Test
    void testAppendFloat() {
        assertEquals(1065353217, builder.append(1.0f).toHashCode());
    }

    //BaseRock generated method id: ${testAppendFloatArray}, hash: F939C2D44B3DC9B3B3A04D4C28AE34C8
    @Test
    void testAppendFloatArray() {
        assertEquals(1065353217, builder.append(new float[] { 1.0f }).toHashCode());
        assertEquals(39415905, builder.append(new float[] { 1.0f, 2.0f }).toHashCode());
        assertEquals(1458025, builder.append((float[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendInt}, hash: A1B7F1E75C6322998932F62D63C09DA8
    @Test
    void testAppendInt() {
        assertEquals(666, builder.append(10).toHashCode());
    }

    //BaseRock generated method id: ${testAppendIntArray}, hash: 4C982EF93C0ED7A489C77A7E1A001ABD
    @Test
    void testAppendIntArray() {
        assertEquals(666, builder.append(new int[] { 10 }).toHashCode());
        assertEquals(24642, builder.append(new int[] { 10, 20 }).toHashCode());
        assertEquals(911754, builder.append((int[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendLong}, hash: F34A2364CC1570A80B4AE6529E0D0E13
    @Test
    void testAppendLong() {
        assertEquals(666, builder.append(10L).toHashCode());
    }

    //BaseRock generated method id: ${testAppendLongArray}, hash: 8B13D7EF25CD694BC68B46A9B04888B5
    @Test
    void testAppendLongArray() {
        assertEquals(666, builder.append(new long[] { 10L }).toHashCode());
        assertEquals(24642, builder.append(new long[] { 10L, 20L }).toHashCode());
        assertEquals(911754, builder.append((long[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendObject}, hash: E90CD3A898B41DA1CF53EF07D0286096
    @Test
    void testAppendObject() {
        assertEquals(666, builder.append("test").toHashCode());
        assertEquals(24642, builder.append(new Object()).toHashCode());
        assertEquals(911754, builder.append((Object) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendObjectArray}, hash: FF700F70785534FD62CC400C7F4B5CD4
    @Test
    void testAppendObjectArray() {
        assertEquals(666, builder.append(new Object[] { "test" }).toHashCode());
        assertEquals(24642, builder.append(new Object[] { "test", "test2" }).toHashCode());
        assertEquals(911754, builder.append((Object[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendShort}, hash: B641E819A214014BD091486FC223C297
    @Test
    void testAppendShort() {
        assertEquals(666, builder.append((short) 10).toHashCode());
    }

    //BaseRock generated method id: ${testAppendShortArray}, hash: 6EDAC8D769DAE7773C8E1161F176A2F1
    @Test
    void testAppendShortArray() {
        assertEquals(666, builder.append(new short[] { 10 }).toHashCode());
        assertEquals(24642, builder.append(new short[] { 10, 20 }).toHashCode());
        assertEquals(911754, builder.append((short[]) null).toHashCode());
    }

    //BaseRock generated method id: ${testAppendSuper}, hash: A3DBA8996E0CBA8D65E3A91F960F699B
    @Test
    void testAppendSuper() {
        assertEquals(666, builder.appendSuper(10).toHashCode());
    }

    //BaseRock generated method id: ${testBuild}, hash: FE04EA310C61B967890D269C9D5D2AB4
    @Test
    void testBuild() {
        assertEquals(Integer.valueOf(17), builder.build());
    }

    //BaseRock generated method id: ${testToHashCode}, hash: B68CC3DFD19EFE9676978B2831679ABF
    @Test
    void testToHashCode() {
        assertEquals(17, builder.toHashCode());
    }

    //BaseRock generated method id: ${testEquals}, hash: 326A12E19DD1094DFFE186ECD0FEE4DF
    @Test
    void testEquals() {
        HashCodeBuilder builder1 = new HashCodeBuilder();
        HashCodeBuilder builder2 = new HashCodeBuilder();
        assertEquals(builder1, builder2);
        assertNotEquals(builder1, null);
        assertNotEquals(builder1, new Object());
    }

    //BaseRock generated method id: ${testHashCode}, hash: 900801807103CD5D7789169E474ED4B4
    @Test
    void testHashCode() {
        HashCodeBuilder builder1 = new HashCodeBuilder();
        HashCodeBuilder builder2 = new HashCodeBuilder();
        assertEquals(builder1.hashCode(), builder2.hashCode());
    }

    //BaseRock generated method id: ${testReflectionHashCode}, hash: C80DCD29C806DC01E72FD57BC76F6550
    @Test
    void testReflectionHashCode() {
        TestObject obj = new TestObject();
        int hashCode = HashCodeBuilder.reflectionHashCode(obj);
        assertNotEquals(0, hashCode);
    }

    //BaseRock generated method id: ${testReflectionHashCodeWithTransients}, hash: BBEB23CA95776BAE2D94AD780DD6FB3F
    @Test
    void testReflectionHashCodeWithTransients() {
        TestObject obj = new TestObject();
        int hashCode = HashCodeBuilder.reflectionHashCode(obj, true);
        assertNotEquals(0, hashCode);
    }

    //BaseRock generated method id: ${testReflectionHashCodeWithExcludes}, hash: 7E80044F21BDC5544B9E007905111064
    @Test
    void testReflectionHashCodeWithExcludes() {
        TestObject obj = new TestObject();
        int hashCode = HashCodeBuilder.reflectionHashCode(obj, new String[] { "excludedField" });
        assertNotEquals(0, hashCode);
    }

    //BaseRock generated method id: ${testIsRegistered}, hash: 2BED8D557A02C2C09812ADDB33C16BAD
    @Test
    void testIsRegistered() {
        TestObject obj = new TestObject();
        assertFalse(HashCodeBuilder.isRegistered(obj));
    }

    private static class TestObject {

        private int field1 = 1;

        private String field2 = "test";

        private transient int excludedField = 2;
    }
}
