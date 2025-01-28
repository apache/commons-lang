package org.apache.commons.lang3.stream;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Enumeration;
import java.util.List;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.Collector;
import java.util.Collection;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StreamsBaseRockGeneratedTest {

    private final Stream<Object> streamMock = mock(Stream.class);

    //BaseRock generated method id: ${failableStreamTest}, hash: F122293E461BAA84173349AF454C3B4F
    @Test()
    void failableStreamTest() {
        //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream<Object> stream = Stream.empty();
            streams.when(() -> Streams.of(anyCollection())).thenReturn(stream);
            Stream stream2 = Stream.empty();
            Streams.FailableStream streamsFailableStream = new Streams.FailableStream(stream2);
            streams.when(() -> Streams.failableStream((Stream) any())).thenReturn(streamsFailableStream);
            Collection<Object> collection = new ArrayList<>();
            //Act Statement(s)
            Streams.FailableStream result = Streams.failableStream(collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(streamsFailableStream));
                streams.verify(() -> Streams.of(anyCollection()), atLeast(1));
                streams.verify(() -> Streams.failableStream((Stream) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${failableStream1Test}, hash: DC90C273261968BDEF74007585335F15
    @Test()
    void failableStream1Test() {
        //Arrange Statement(s)
        Stream<Object> stream = Stream.empty();
        
        //Act Statement(s)
        Streams.FailableStream result = Streams.failableStream(stream);
        
        //Assert statement(s)
        //TODO: Please implement equals method in FailableStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${failableStream2WhenValueIsNull}, hash: 2350D371F721665F5D37D072176E8914
    @Test()
    void failableStream2WhenValueIsNull() {
        /* Branches:
         * (value == null) : true  #  inside streamOf method
         */
         //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream stream = Stream.empty();
            Streams.FailableStream streamsFailableStream = new Streams.FailableStream(stream);
            streams.when(() -> Streams.failableStream(streamMock)).thenReturn(streamsFailableStream);
            Object object = null;
            //Act Statement(s)
            Streams.FailableStream result = Streams.failableStream(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(streamsFailableStream));
                streams.verify(() -> Streams.failableStream(streamMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${failableStream2WhenValueIsNotNull}, hash: ACB058A36AA4CCD475ED896E79B121EF
    @Test()
    void failableStream2WhenValueIsNotNull() {
        /* Branches:
         * (value == null) : false  #  inside streamOf method
         */
         //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream stream = Stream.empty();
            Streams.FailableStream streamsFailableStream = new Streams.FailableStream(stream);
            streams.when(() -> Streams.failableStream(streamMock)).thenReturn(streamsFailableStream);
            Object object = new Object();
            //Act Statement(s)
            Streams.FailableStream result = Streams.failableStream(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(streamsFailableStream));
                streams.verify(() -> Streams.failableStream(streamMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${failableStream3Test}, hash: 3694F6D4BA0C44A91B4011857E3E096E
    @Test()
    void failableStream3Test() {
        //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream<Object> stream = Stream.empty();
            Object[] objectArray = new Object[] {};
            streams.when(() -> Streams.of(objectArray)).thenReturn(stream);
            Stream stream2 = Stream.empty();
            Streams.FailableStream streamsFailableStream = new Streams.FailableStream(stream2);
            streams.when(() -> Streams.failableStream((Stream) any())).thenReturn(streamsFailableStream);
            //Act Statement(s)
            Streams.FailableStream result = Streams.failableStream(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(streamsFailableStream));
                streams.verify(() -> Streams.of(objectArray), atLeast(1));
                streams.verify(() -> Streams.failableStream((Stream) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${instancesOfWhenStreamIsNull}, hash: 9D05EF066DAE30AB7E8F8D8705D95938
    @Test()
    void instancesOfWhenStreamIsNull() {
        /* Branches:
         * (stream == null) : true  #  inside of method
         */
         //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            streams.when(() -> Streams.of(anyCollection())).thenReturn(null);
            Collection<Object> collection = new ArrayList<>();
            //Act Statement(s)
            Stream result = Streams.instancesOf(Object.class, collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                streams.verify(() -> Streams.of(anyCollection()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${instancesOfWhenStreamIsNotNull}, hash: 949A016D0C142971D76CE2CF468C0637
    @Test()
    void instancesOfWhenStreamIsNotNull() {
        /* Branches:
         * (stream == null) : false  #  inside of method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream stream = Stream.empty();
            streams.when(() -> Streams.of(anyCollection())).thenReturn(stream);
            Collection<Object> collection = new ArrayList<>();
            //Act Statement(s)
            Stream result = Streams.instancesOf(Object.class, collection);
            //Assert statement(s)
            //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                streams.verify(() -> Streams.of(anyCollection()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nonNullTest}, hash: 8F68C7EA544ABBB170E743E54FA82544
    @Test()
    void nonNullTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream<Object> stream = Stream.empty();
            streams.when(() -> Streams.of(anyCollection())).thenReturn(stream);
            Collection<Object> collection = new ArrayList<>();
            //Act Statement(s)
            Stream result = Streams.nonNull(collection);
            //Assert statement(s)
            //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                streams.verify(() -> Streams.of(anyCollection()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nonNull1WhenValueIsNull}, hash: 3AF471414B78BBD31E952AC463A8F1A5
    @Test()
    void nonNull1WhenValueIsNull() {
        /* Branches:
         * (value == null) : true  #  inside streamOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        Stream result = Streams.nonNull(object);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${nonNull1WhenValueIsNotNull}, hash: 55659BD25D6C975C4CDA0DCD3C0F39C9
    @Test()
    void nonNull1WhenValueIsNotNull() {
        /* Branches:
         * (value == null) : false  #  inside streamOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        Stream result = Streams.nonNull(object);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${nonNull2Test}, hash: C483AB7992BD6392198528E111E769F0
    @Test()
    void nonNull2Test() {
        //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream<Object> stream = Stream.empty();
            Object[] objectArray = new Object[] {};
            streams.when(() -> Streams.of(objectArray)).thenReturn(stream);
            Stream stream2 = Stream.empty();
            streams.when(() -> Streams.nonNull((Stream) any())).thenReturn(stream2);
            //Act Statement(s)
            Stream result = Streams.nonNull(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(stream2));
                streams.verify(() -> Streams.of(objectArray), atLeast(1));
                streams.verify(() -> Streams.nonNull((Stream) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nonNull3WhenStreamIsNull}, hash: 0ABE1DADD087D64FEB62EB723955522A
    @Test()
    void nonNull3WhenStreamIsNull() {
        /* Branches:
         * (stream == null) : true  #  inside of method
         */
         //Arrange Statement(s)
        Stream<Object> stream = null;
        
        //Act Statement(s)
        Stream result = Streams.nonNull(stream);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${nonNull3WhenStreamIsNotNull}, hash: 9DC73DB25026A5297AD5FABDC7A0291D
    @Test()
    void nonNull3WhenStreamIsNotNull() {
        /* Branches:
         * (stream == null) : false  #  inside of method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Stream<Object> stream = Stream.empty();
        
        //Act Statement(s)
        Stream result = Streams.nonNull(stream);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${ofWhenCollectionIsNull}, hash: E45B6012CF8DC25201F28B9D73032F1F
    @Test()
    void ofWhenCollectionIsNull() {
        /* Branches:
         * (collection == null) : true
         */
         //Arrange Statement(s)
        Collection<Object> collection = null;
        
        //Act Statement(s)
        Stream result = Streams.of(collection);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${ofWhenCollectionIsNotNull}, hash: 9AF00FF11D0FCBB5C7134871A3A3F613
    @Test()
    void ofWhenCollectionIsNotNull() {
        /* Branches:
         * (collection == null) : false
         */
         //Arrange Statement(s)
        Collection<Object> collection = new ArrayList<>();
        
        //Act Statement(s)
        Stream result = Streams.of(collection);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of1Test}, hash: 44BF35BF4310B51AB51F6195CA8D2BC6
    @Test()
    void of1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Enumeration<Object> enumerationMock = mock(Enumeration.class);
        
        //Act Statement(s)
        Stream result = Streams.of(enumerationMock);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of2WhenIterableIsNull}, hash: 2027C8C498EB300D8911D1ACB3CA2B6E
    @Test()
    void of2WhenIterableIsNull() {
        /* Branches:
         * (iterable == null) : true
         */
         //Arrange Statement(s)
        Iterable<Object> iterable = null;
        
        //Act Statement(s)
        Stream result = Streams.of(iterable);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of2WhenIterableIsNotNull}, hash: C49F91F10C3FC76EF69673062AAB3C87
    @Test()
    void of2WhenIterableIsNotNull() {
        /* Branches:
         * (iterable == null) : false
         */
         //Arrange Statement(s)
        Iterable<Object> iterable = new ArrayList<>();
        
        //Act Statement(s)
        Stream result = Streams.of(iterable);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of3WhenIteratorIsNull}, hash: D473A600E284A6DAE3863D2E7FA43FD7
    @Test()
    void of3WhenIteratorIsNull() {
        /* Branches:
         * (iterator == null) : true
         */
         //Arrange Statement(s)
        Iterator<Object> iterator = null;
        
        //Act Statement(s)
        Stream result = Streams.of(iterator);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of3WhenIteratorIsNotNull}, hash: 4FC3D85831A6481C869EFD87D1B97315
    @Test()
    void of3WhenIteratorIsNotNull() {
        /* Branches:
         * (iterator == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        List<Object> objectList = new ArrayList<>();
        Iterator<Object> iteratorIterator = objectList.iterator();
        
        //Act Statement(s)
        Stream result = Streams.of(iteratorIterator);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of5WhenValuesIsNull}, hash: CBAEBE63A3A4DAB8924619BC75622DF6
    @Test()
    void of5WhenValuesIsNull() {
        /* Branches:
         * (values == null) : true
         */
         //Arrange Statement(s)
        Object[] object = null;
        
        //Act Statement(s)
        Stream result = Streams.of(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of5WhenValuesIsNotNull}, hash: 3AF0CA434DA0250657826333B430B428
    @Test()
    void of5WhenValuesIsNotNull() {
        /* Branches:
         * (values == null) : false
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        Stream result = Streams.of(objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${streamTest}, hash: 5D6968C2E58D9857D4FE05B445888A98
    @Test()
    void streamTest() {
        //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream stream = Stream.empty();
            Streams.FailableStream streamsFailableStream = new Streams.FailableStream(stream);
            streams.when(() -> Streams.failableStream(anyCollection())).thenReturn(streamsFailableStream);
            Collection<Object> collection = new ArrayList<>();
            //Act Statement(s)
            Streams.FailableStream result = Streams.stream(collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(streamsFailableStream));
                streams.verify(() -> Streams.failableStream(anyCollection()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stream1Test}, hash: 1A559BF9CA66DC83825328515D13E8F3
    @Test()
    void stream1Test() {
        //Arrange Statement(s)
        try (MockedStatic<Streams> streams = mockStatic(Streams.class, CALLS_REAL_METHODS)) {
            Stream stream = Stream.empty();
            Streams.FailableStream streamsFailableStream = new Streams.FailableStream(stream);
            streams.when(() -> Streams.failableStream((Stream) any())).thenReturn(streamsFailableStream);
            Stream<Object> stream2 = Stream.empty();
            //Act Statement(s)
            Streams.FailableStream result = Streams.stream(stream2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(streamsFailableStream));
                streams.verify(() -> Streams.failableStream((Stream) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toArrayTest}, hash: 205BE87406F244020E0F444A5A471C56
    @Test()
    void toArrayTest() {
        
        //Act Statement(s)
        Collector<Object, ?, Object[]> result = Streams.toArray(Object.class);
        
        //Assert statement(s)
        //TODO: Please implement equals method in ArrayCollector for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
