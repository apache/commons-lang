/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.reflect;

import static junit.framework.Assert.*;

import java.lang.reflect.Field;
import java.lang.reflect.TypeVariable;
import java.util.List;

import org.apache.commons.lang3.reflect.testbed.*;
import org.junit.Before;
import org.junit.Test;

/**
 * Test TypeUtils
 * @author mbenson
 * @version $Id$
 */
public class TypeUtilsTest {
    private Field stringParentField;
    private Field integerParentField;
    private Field foosField;
    private Field barParentsField;
    private TypeVariable<?> genericParentT;
    private TypeVariable<?> listType;
    private TypeVariable<?> iterableType;

    @Before
    public void setup() throws Exception {
        stringParentField = GenericTypeHolder.class.getDeclaredField("stringParent");
        integerParentField = GenericTypeHolder.class.getDeclaredField("integerParent");
        foosField = GenericTypeHolder.class.getDeclaredField("foos");
        barParentsField = GenericTypeHolder.class.getDeclaredField("barParents");
        genericParentT = GenericParent.class.getTypeParameters()[0];
        listType = List.class.getTypeParameters()[0];
        iterableType = Iterable.class.getTypeParameters()[0];
    }

    @Test
    public void testGetRawTypeClass() throws Exception {
        assertEquals(GenericParent.class, TypeUtils.getRawType(null, GenericParent.class));
    }

    @Test
    public void testGetRawTypeParameterizedType() throws Exception {
        assertEquals(GenericParent.class, TypeUtils.getRawType(GenericTypeHolder.class,
                stringParentField.getGenericType()));
        assertEquals(GenericParent.class, TypeUtils.getRawType(GenericTypeHolder.class,
                integerParentField.getGenericType()));
        assertEquals(List.class, TypeUtils.getRawType(GenericTypeHolder.class, foosField
                .getGenericType()));
    }

    @Test
    public void testGetRawTypeTypeVariable() throws Exception {
        assertEquals(String.class, TypeUtils.getRawType(StringParameterizedChild.class,
                genericParentT));
        assertEquals(String.class, TypeUtils.getRawType(stringParentField.getGenericType(),
                genericParentT));
        assertEquals(Foo.class, TypeUtils.getRawType(foosField.getGenericType(), iterableType));
        assertEquals(Foo.class, TypeUtils.getRawType(foosField.getGenericType(), listType));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetRawTypeUnresolvableTypeVariable() {
        TypeUtils.getRawType(GenericParent.class, genericParentT);
    }

    @Test
    public void testGetRawTypeGenericArray() throws Exception {
        assertEquals(GenericParent[].class, TypeUtils.getRawType(GenericTypeHolder.class,
                barParentsField.getGenericType()));
    }
}
