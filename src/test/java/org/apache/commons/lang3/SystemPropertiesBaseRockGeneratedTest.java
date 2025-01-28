package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.LongSupplier;
import java.util.function.Supplier;
import org.mockito.MockedStatic;
import java.util.function.IntSupplier;
import java.util.function.BooleanSupplier;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.doReturn;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class SystemPropertiesBaseRockGeneratedTest {

    private final BooleanSupplier defaultIfAbsentMock = mock(BooleanSupplier.class);

    private final Supplier<String> supplierMock = mock(Supplier.class);

    //BaseRock generated method id: ${getAppleAwtEnableTemplateImagesTest}, hash: 8A1EDCB94CEA2FF2A757A4AC1982E2B0
    @Test()
    void getAppleAwtEnableTemplateImagesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("apple.awt.enableTemplateImages")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getAppleAwtEnableTemplateImages();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("apple.awt.enableTemplateImages"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAwtToolkitTest}, hash: 6E69442A989F9259418B3607A4EAF319
    @Test()
    void getAwtToolkitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("awt.toolkit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getAwtToolkit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("awt.toolkit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getBooleanWhenDefaultIfAbsentIsNotNull}, hash: 8ACBF0354B692FF8F0176F43452FADFA
    @Test()
    void getBooleanWhenDefaultIfAbsentIsNotNull() {
        /* Branches:
         * (str == null) : true
         * (defaultIfAbsent != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            doReturn(true).when(defaultIfAbsentMock).getAsBoolean();
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn(null);
            //Act Statement(s)
            boolean result = SystemProperties.getBoolean("key1", defaultIfAbsentMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                verify(defaultIfAbsentMock, atLeast(1)).getAsBoolean();
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getBooleanWhenDefaultIfAbsentIsNull}, hash: 190EA8D4F874A4893342B518050E4E5F
    @Test()
    void getBooleanWhenDefaultIfAbsentIsNull() {
        /* Branches:
         * (str == null) : true
         * (defaultIfAbsent != null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            doReturn(false).when(defaultIfAbsentMock).getAsBoolean();
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn(null);
            //Act Statement(s)
            boolean result = SystemProperties.getBoolean("key1", defaultIfAbsentMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                verify(defaultIfAbsentMock, atLeast(1)).getAsBoolean();
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getBooleanWhenStrIsNullDefaultIfAbsentIsNotNullAndDefaultIfAbsentGetAsBoolean}, hash: 9F6CA55809F10C1A22AD14889F6E4DFF
    @Disabled()
    @Test()
    void getBooleanWhenStrIsNullDefaultIfAbsentIsNotNullAndDefaultIfAbsentGetAsBoolean() {
        /* Branches:
         * (str == null) : false
         * (str == null ? defaultIfAbsent != null && defaultIfAbsent.getAsBoolean() : Boolean.parseBoolean(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BooleanSupplier booleanSupplierMock = mock(BooleanSupplier.class);
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            boolean result = SystemProperties.getBoolean("key1", booleanSupplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getComSunJndiLdapObjectTrustSerialDataTest}, hash: FD7F222C1AFAC71C3BE300077E661B45
    @Test()
    void getComSunJndiLdapObjectTrustSerialDataTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("com.sun.jndi.ldap.object.trustSerialData")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getComSunJndiLdapObjectTrustSerialData();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("com.sun.jndi.ldap.object.trustSerialData"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getComSunNetHttpServerHttpServerProviderTest}, hash: 1325A2B49777E56F66684575FBF069EA
    @Test()
    void getComSunNetHttpServerHttpServerProviderTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("com.sun.net.httpserver.HttpServerProvider")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getComSunNetHttpServerHttpServerProvider();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("com.sun.net.httpserver.HttpServerProvider"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFileEncodingTest}, hash: 32EA22A072A6C96A3CD6573EB4186401
    @Test()
    void getFileEncodingTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("file.encoding")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getFileEncoding();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("file.encoding"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFileSeparatorTest}, hash: AFD8E6B5DC56889F8F12D1F8B8ADD6F4
    @Test()
    void getFileSeparatorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("file.separator")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getFileSeparator();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("file.separator"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFtpNonProxyHostTest}, hash: B9D7EB02466BB8B9539BF8A03CBC99EF
    @Test()
    void getFtpNonProxyHostTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("ftp.nonProxyHosts")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getFtpNonProxyHost();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("ftp.nonProxyHosts"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFtpProxyHostTest}, hash: F269F24359BDE631F9FC8126F33BA6E9
    @Test()
    void getFtpProxyHostTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("ftp.proxyHost")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getFtpProxyHost();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("ftp.proxyHost"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFtpProxyPortTest}, hash: 12CFF5FB6055E4051577569794FFC26F
    @Test()
    void getFtpProxyPortTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("ftp.proxyPort")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getFtpProxyPort();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("ftp.proxyPort"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpAgentTest}, hash: 73B90C477EBC50DA2E6C0182AE1A81D5
    @Disabled()
    @Test()
    void getHttpAgentTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.agent")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpAgent();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.agent"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpAuthDigestCnonceRepeatTest}, hash: CA1438FD95CDFE7F148A8F1280152AD6
    @Test()
    void getHttpAuthDigestCnonceRepeatTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.auth.digest.cnonceRepeat")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpAuthDigestCnonceRepeat();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.auth.digest.cnonceRepeat"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpAuthDigestReenabledAlgorithmsTest}, hash: A3303B42A178B0DE2FB63A1BC8EFFDFE
    @Test()
    void getHttpAuthDigestReenabledAlgorithmsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.auth.digest.reEnabledAlgorithms")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpAuthDigestReenabledAlgorithms();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.auth.digest.reEnabledAlgorithms"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpAuthDigestValidateProxyTest}, hash: 0ECEA8749F7F342D62116040A49342BE
    @Test()
    void getHttpAuthDigestValidateProxyTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.auth.digest.validateProxy")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpAuthDigestValidateProxy();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.auth.digest.validateProxy"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpAuthDigestValidateServerTest}, hash: 41825A7A51FF1873A1F873260F0524ED
    @Test()
    void getHttpAuthDigestValidateServerTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.auth.digest.validateServer")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpAuthDigestValidateServer();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.auth.digest.validateServer"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpAuthNtlmDomainTest}, hash: AD1F931C8AE42EAFA871C012DC502232
    @Test()
    void getHttpAuthNtlmDomainTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.auth.ntlm.domain")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpAuthNtlmDomain();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.auth.ntlm.domain"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpKeepAliveTest}, hash: CE8CFC35775A82798A32579A4ADF1959
    @Test()
    void getHttpKeepAliveTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.keepAlive")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpKeepAlive();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.keepAlive"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpKeepAliveTimeProxyTest}, hash: 1D8FBD87B49F1AD22749711FDE522C2A
    @Test()
    void getHttpKeepAliveTimeProxyTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.keepAlive.time.proxy")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpKeepAliveTimeProxy();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.keepAlive.time.proxy"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpKeepAliveTimeServerTest}, hash: 22A1918FC57E3D0C3E59D742B216AE42
    @Test()
    void getHttpKeepAliveTimeServerTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.keepAlive.time.server")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpKeepAliveTimeServer();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.keepAlive.time.server"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpMaxConnectionsTest}, hash: 334476DB0E7621FCC063CFE157845FF1
    @Test()
    void getHttpMaxConnectionsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.maxConnections")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpMaxConnections();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.maxConnections"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpMaxRedirectsTest}, hash: 95AEEC57B6B9C594BC0A7C05A104A1DD
    @Test()
    void getHttpMaxRedirectsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.maxRedirects")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpMaxRedirects();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.maxRedirects"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpNonProxyHostsTest}, hash: 60E82441A2BED6A3680DAB67E37B1387
    @Test()
    void getHttpNonProxyHostsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.nonProxyHosts")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpNonProxyHosts();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.nonProxyHosts"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpProxyHostTest}, hash: 72763D80736BB4F953E8293E07BFB565
    @Test()
    void getHttpProxyHostTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.proxyHost")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpProxyHost();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.proxyHost"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpProxyPortTest}, hash: D1E9BB88EADACEA5EFD74401CDA9178E
    @Test()
    void getHttpProxyPortTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("http.proxyPort")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpProxyPort();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("http.proxyPort"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpsProxyHostTest}, hash: A79F1A21E0BE6445A3316B9F19D75F05
    @Test()
    void getHttpsProxyHostTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("https.proxyHost")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpsProxyHost();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("https.proxyHost"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getHttpsProxyPortTest}, hash: 2C35FE0995606C72AC3FA8DDAB8D7F7A
    @Test()
    void getHttpsProxyPortTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("https.proxyPort")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getHttpsProxyPort();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("https.proxyPort"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getIntWhenStrIsNotNull}, hash: A75A467AEEC0F03BBA04C230447D4040
    @Test()
    void getIntWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Arrange Statement(s)
        IntSupplier intSupplierMock = mock(IntSupplier.class);
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn("0");
            //Act Statement(s)
            int result = SystemProperties.getInt("key1", intSupplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getIntWhenDefaultIfAbsentIsNotNull}, hash: 300E1A42C19212BF16E16D6D20542140
    @Test()
    void getIntWhenDefaultIfAbsentIsNotNull() {
        /* Branches:
         * (str == null) : true
         * (defaultIfAbsent != null) : true
         */
        //Arrange Statement(s)
        IntSupplier defaultIfAbsentMock = mock(IntSupplier.class);
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(defaultIfAbsentMock).getAsInt();
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn(null);
            //Act Statement(s)
            int result = SystemProperties.getInt("key1", defaultIfAbsentMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                verify(defaultIfAbsentMock, atLeast(1)).getAsInt();
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getIntWhenDefaultIfAbsentIsNull}, hash: D8F356EE66C39EE48B9AEE5360C9DB44
    @Test()
    void getIntWhenDefaultIfAbsentIsNull() {
        /* Branches:
         * (str == null) : true
         * (defaultIfAbsent != null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn(null);
            IntSupplier intSupplier = null;
            //Act Statement(s)
            int result = SystemProperties.getInt("key1", intSupplier);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaAwtFontsTest}, hash: 26046AA2B8DFD251CE514DD79E04BA2B
    @Test()
    void getJavaAwtFontsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.awt.fonts")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaAwtFonts();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.awt.fonts"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaAwtGraphicsenvTest}, hash: 2EEA129E1DC22644811985B606F1EB2B
    @Test()
    void getJavaAwtGraphicsenvTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.awt.graphicsenv")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaAwtGraphicsenv();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.awt.graphicsenv"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaAwtHeadlessTest}, hash: 71A07FB939447EE20F6E22BEF8309E42
    @Test()
    void getJavaAwtHeadlessTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.awt.headless")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaAwtHeadless();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.awt.headless"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaAwtPrinterjobTest}, hash: F3390CE77492072869AC592EA83E66C9
    @Test()
    void getJavaAwtPrinterjobTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.awt.printerjob")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaAwtPrinterjob();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.awt.printerjob"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaClassPathTest}, hash: 561E792EA9CD09F2D0BD841B61F9E934
    @Test()
    void getJavaClassPathTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.class.path")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaClassPath();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.class.path"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaClassVersionTest}, hash: A0FAAB08B920E77C3F7A79B7D82394CE
    @Test()
    void getJavaClassVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.class.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaClassVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.class.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaCompilerTest}, hash: A5A744F7BAE02BE11CC0EE51FD433E11
    @Test()
    void getJavaCompilerTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.compiler")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaCompiler();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.compiler"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaContentHandlerPkgsTest}, hash: 727C69D001EF7C13ABFC3DA63DD8EDE7
    @Test()
    void getJavaContentHandlerPkgsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.content.handler.pkgs")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaContentHandlerPkgs();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.content.handler.pkgs"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaEndorsedDirsTest}, hash: 190C295E787B39A365DEF86B040EF3FA
    @Test()
    void getJavaEndorsedDirsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.endorsed.dirs")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaEndorsedDirs();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.endorsed.dirs"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaExtDirsTest}, hash: 32BB2D182CFD2EBA58A65304584AC09D
    @Test()
    void getJavaExtDirsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.ext.dirs")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaExtDirs();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.ext.dirs"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaHomeTest}, hash: 41E64301F536E2ECF4394EDC6A407E4A
    @Test()
    void getJavaHomeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.home")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaHome();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.home"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaIoTmpdirTest}, hash: 719A64CCD3D5518DA1D999FE0CCB0472
    @Test()
    void getJavaIoTmpdirTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.io.tmpdir")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaIoTmpdir();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.io.tmpdir"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaLibraryPathTest}, hash: A4204C68B291C207FC27578394A5245E
    @Test()
    void getJavaLibraryPathTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.library.path")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaLibraryPath();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.library.path"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaLocaleProvidersTest}, hash: A93C1A93338CAE88A75A6B43E447C519
    @Test()
    void getJavaLocaleProvidersTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.locale.providers")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaLocaleProviders();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.locale.providers"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaLocaleUseOldIsoCodesTest}, hash: E08F348539F4898EA83C4C0E1A84C2B0
    @Test()
    void getJavaLocaleUseOldIsoCodesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.locale.useOldISOCodes")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaLocaleUseOldIsoCodes();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.locale.useOldISOCodes"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNetPreferIpv4StackTest}, hash: 03E2A31FFA8166A6664F18BC91AD3D2B
    @Test()
    void getJavaNetPreferIpv4StackTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.net.preferIPv4Stack")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNetPreferIpv4Stack();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.net.preferIPv4Stack"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNetPreferIpv6AddressesTest}, hash: FD4315C1075313FD6189094420C9C799
    @Test()
    void getJavaNetPreferIpv6AddressesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.net.preferIPv6Addresses")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNetPreferIpv6Addresses();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.net.preferIPv6Addresses"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNetSocksPasswordTest}, hash: 3260FE81AF7224CC599BC11883AA2BDA
    @Test()
    void getJavaNetSocksPasswordTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.net.socks.password")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNetSocksPassword();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.net.socks.password"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNetSocksUserNameTest}, hash: 5091BBCB6B0E32D1028DB668B923A17F
    @Test()
    void getJavaNetSocksUserNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.net.socks.username")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNetSocksUserName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.net.socks.username"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNetUseSystemProxiesTest}, hash: B349EAC74E443E8547C11385A200E3D4
    @Test()
    void getJavaNetUseSystemProxiesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.net.useSystemProxies")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNetUseSystemProxies();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.net.useSystemProxies"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNioChannelsDefaultThreadPoolInitialSizeTest}, hash: 412E74BD3A9CC4F43F832F61C16D41D9
    @Test()
    void getJavaNioChannelsDefaultThreadPoolInitialSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.nio.channels.DefaultThreadPool.initialSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNioChannelsDefaultThreadPoolInitialSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.nio.channels.DefaultThreadPool.initialSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNioChannelsDefaultThreadPoolThreadFactoryTest}, hash: E2152F1B8E5BFE1A72DE349310C6C04A
    @Test()
    void getJavaNioChannelsDefaultThreadPoolThreadFactoryTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.nio.channels.DefaultThreadPool.threadFactory")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNioChannelsDefaultThreadPoolThreadFactory();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.nio.channels.DefaultThreadPool.threadFactory"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNioChannelsSpiAsynchronousChannelProviderTest}, hash: 502D2D5A6D2DFB76D91C7DCB9B337432
    @Test()
    void getJavaNioChannelsSpiAsynchronousChannelProviderTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.nio.channels.spi.AsynchronousChannelProvider")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNioChannelsSpiAsynchronousChannelProvider();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.nio.channels.spi.AsynchronousChannelProvider"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNioChannelsSpiSelectorProviderTest}, hash: FB316A5BC0DD7F381A46ACD9D4D98767
    @Test()
    void getJavaNioChannelsSpiSelectorProviderTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.nio.channels.spi.SelectorProvider")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNioChannelsSpiSelectorProvider();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.nio.channels.spi.SelectorProvider"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaNioFileSpiDefaultFileSystemProviderTest}, hash: 86D609CA1DC1DF7451F4546E76AABA72
    @Test()
    void getJavaNioFileSpiDefaultFileSystemProviderTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.nio.file.spi.DefaultFileSystemProvider")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaNioFileSpiDefaultFileSystemProvider();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.nio.file.spi.DefaultFileSystemProvider"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaPropertiesDateTest}, hash: 7E1BD81015B71ED69668B77BBEEE564E
    @Test()
    void getJavaPropertiesDateTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.properties.date")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaPropertiesDate();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.properties.date"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaProtocolHandlerPkgsTest}, hash: 7380D8D06501B2C88A08FA577C98EB59
    @Test()
    void getJavaProtocolHandlerPkgsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.protocol.handler.pkgs")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaProtocolHandlerPkgs();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.protocol.handler.pkgs"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaRmiServerCodebaseTest}, hash: 722BCAFD6976E0BCF969FD7AE3644DC1
    @Test()
    void getJavaRmiServerCodebaseTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.rmi.server.codebase")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaRmiServerCodebase();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.rmi.server.codebase"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaRmiServerHostNameTest}, hash: 7E43D2C7D867EA52815FE92B4F44B724
    @Test()
    void getJavaRmiServerHostNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.rmi.server.hostname")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaRmiServerHostName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.rmi.server.hostname"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaRmiServerRandomIdsTest}, hash: 17697FBC67859AB03CBBB6E3DFA55D6E
    @Test()
    void getJavaRmiServerRandomIdsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.rmi.server.randomIDs")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaRmiServerRandomIds();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.rmi.server.randomIDs"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaRmiServerRmiClassLoaderSpiTest}, hash: 58FA385739997ACAED0080F5977CA589
    @Test()
    void getJavaRmiServerRmiClassLoaderSpiTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.rmi.server.RMIClassLoaderSpi")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaRmiServerRmiClassLoaderSpi();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.rmi.server.RMIClassLoaderSpi"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaRuntimeNameTest}, hash: 912C4D8FDB9F4B6B7CC0124903CF8850
    @Test()
    void getJavaRuntimeNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.runtime.name")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaRuntimeName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.runtime.name"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaRuntimeVersionTest}, hash: B4E23DD4C12866DF219418212B45F3F6
    @Test()
    void getJavaRuntimeVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.runtime.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaRuntimeVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.runtime.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSecurityAuthLoginConfigTest}, hash: 9E7FC8926505D39B23B9C6357A680773
    @Test()
    void getJavaSecurityAuthLoginConfigTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.security.auth.login.config")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSecurityAuthLoginConfig();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.security.auth.login.config"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSecurityManagerTest}, hash: 25CB64510267B0F052A099C44E921A95
    @Test()
    void getJavaSecurityManagerTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.security.manager")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSecurityManager();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.security.manager"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSpecificationMaintenanceVersionTest}, hash: 1568203CDF592BFB98A93B7BE0195788
    @Test()
    void getJavaSpecificationMaintenanceVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.specification.maintenance.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSpecificationMaintenanceVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.specification.maintenance.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSpecificationNameTest}, hash: 3A67A73677A3D751D2710B4D65001C19
    @Test()
    void getJavaSpecificationNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.specification.name")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSpecificationName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.specification.name"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSpecificationVendorTest}, hash: 0033C9C2C6347B2DC6AB7300612040AF
    @Test()
    void getJavaSpecificationVendorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.specification.vendor")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSpecificationVendor();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.specification.vendor"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSpecificationVersionTest}, hash: D15BBF01092F1391017C0B45A57EE929
    @Test()
    void getJavaSpecificationVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.specification.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSpecificationVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.specification.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSpecificationVersion1Test}, hash: 6559165E2AF1085F805E77DA1F432577
    @Test()
    void getJavaSpecificationVersion1Test() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.specification.version", "defaultValue1")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSpecificationVersion("defaultValue1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.specification.version", "defaultValue1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaSystemClassLoaderTest}, hash: F46D67BB223F783C8B53CEC7F0DAC763
    @Test()
    void getJavaSystemClassLoaderTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.system.class.loader")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaSystemClassLoader();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.system.class.loader"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaTimeZoneDefaultZoneRulesProviderTest}, hash: 28E5FE0E8A0E6EA6C05A5B36962A1033
    @Test()
    void getJavaTimeZoneDefaultZoneRulesProviderTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.time.zone.DefaultZoneRulesProvider")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaTimeZoneDefaultZoneRulesProvider();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.time.zone.DefaultZoneRulesProvider"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilConcurrentForkJoinPoolCommonExceptionHandlerTest}, hash: 80429D162F99F75D8F80A6CFD677BB56
    @Test()
    void getJavaUtilConcurrentForkJoinPoolCommonExceptionHandlerTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.exceptionHandler")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilConcurrentForkJoinPoolCommonExceptionHandler();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.exceptionHandler"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilConcurrentForkJoinPoolCommonMaximumSparesTest}, hash: 941BF6284C2F7B5BA396222D73133E56
    @Test()
    void getJavaUtilConcurrentForkJoinPoolCommonMaximumSparesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.maximumSpares")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilConcurrentForkJoinPoolCommonMaximumSpares();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.maximumSpares"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilConcurrentForkJoinPoolCommonParallelismTest}, hash: 779FFDBE3F4168720B9C91D832CBBC54
    @Test()
    void getJavaUtilConcurrentForkJoinPoolCommonParallelismTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.parallelism")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilConcurrentForkJoinPoolCommonParallelism();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.parallelism"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilConcurrentForkJoinPoolCommonThreadFactoryTest}, hash: 94E9B436E28986601575C766F647FC0F
    @Test()
    void getJavaUtilConcurrentForkJoinPoolCommonThreadFactoryTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.threadFactory")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilConcurrentForkJoinPoolCommonThreadFactory();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.concurrent.ForkJoinPool.common.threadFactory"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilCurrencyDataTest}, hash: C81EDC370E622DA62BA872C2CE431C2E
    @Test()
    void getJavaUtilCurrencyDataTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.currency.data")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilCurrencyData();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.currency.data"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilLoggingConfigClassTest}, hash: 2134A7A040DA40836C9632F0938CCA03
    @Test()
    void getJavaUtilLoggingConfigClassTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.logging.config.class")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilLoggingConfigClass();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.logging.config.class"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilLoggingConfigFileTest}, hash: 4E34482C58D9255AB198BF65939B85C4
    @Test()
    void getJavaUtilLoggingConfigFileTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.logging.config.file")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilLoggingConfigFile();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.logging.config.file"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilLoggingSimpleFormatterFormatTest}, hash: 9EF9F1D5CB0FA3C16679EB99D16C97F8
    @Test()
    void getJavaUtilLoggingSimpleFormatterFormatTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.logging.simpleformatter.format")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilLoggingSimpleFormatterFormat();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.logging.simpleformatter.format"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilPrefsPreferencesFactoryTest}, hash: E88061068B127D34FED06C0167FB30FA
    @Test()
    void getJavaUtilPrefsPreferencesFactoryTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.prefs.PreferencesFactory")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilPrefsPreferencesFactory();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.prefs.PreferencesFactory"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaUtilPropertyResourceBundleEncodingTest}, hash: 6E11BC2BA89D6F97357ED6F093EE01FF
    @Test()
    void getJavaUtilPropertyResourceBundleEncodingTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.util.PropertyResourceBundle.encoding")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaUtilPropertyResourceBundleEncoding();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.util.PropertyResourceBundle.encoding"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVendorTest}, hash: C484B94BFF0E74020E32B7E668487296
    @Test()
    void getJavaVendorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vendor")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVendor();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vendor"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVendorUrlTest}, hash: F72BBBE06274BB46F33B9AC20539E3DD
    @Test()
    void getJavaVendorUrlTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vendor.url")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVendorUrl();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vendor.url"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVendorVersionTest}, hash: CD373428D8CA9BBEAE658C60AAA57191
    @Test()
    void getJavaVendorVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vendor.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVendorVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vendor.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVersionTest}, hash: 13BB2402337A070244A1F035956D9BAD
    @Test()
    void getJavaVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVersionDateTest}, hash: 89C5AA484B4ACF3D645261D0F300FA4D
    @Test()
    void getJavaVersionDateTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.version.date")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVersionDate();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.version.date"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmInfoTest}, hash: EC5E00D759A38A8967FAFD7407EB5FF4
    @Test()
    void getJavaVmInfoTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.info")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmInfo();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.info"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmNameTest}, hash: CA786A4BB0B7F79C5EA3EAA44D993A40
    @Test()
    void getJavaVmNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.name")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.name"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmSpecificationNameTest}, hash: 27E329EBABC14DE9A55A8529F14E678A
    @Test()
    void getJavaVmSpecificationNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.specification.name")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmSpecificationName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.specification.name"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmSpecificationVendorTest}, hash: 061DBE5933E2FEB1F3AA0BD3A25A2232
    @Test()
    void getJavaVmSpecificationVendorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.specification.vendor")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmSpecificationVendor();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.specification.vendor"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmSpecificationVersionTest}, hash: E8648723A4AFF7806EEB368AF7324696
    @Test()
    void getJavaVmSpecificationVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.specification.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmSpecificationVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.specification.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmVendorTest}, hash: 818418AE45CB707239144B5A2F51EFE8
    @Test()
    void getJavaVmVendorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.vendor")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmVendor();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.vendor"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaVmVersionTest}, hash: 9C0D5354E0D67AD64135E8356115E984
    @Test()
    void getJavaVmVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.vm.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaVmVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.vm.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaxAccessibilityAssistiveTechnologiesTest}, hash: 2FA1BB5BFCC998F933583F314A453375
    @Test()
    void getJavaxAccessibilityAssistiveTechnologiesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("javax.accessibility.assistive_technologies")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaxAccessibilityAssistiveTechnologies();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("javax.accessibility.assistive_technologies"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaXmlConfigFileTest}, hash: C0E3CCC37B1B3B04773F06C2CA70FCB0
    @Test()
    void getJavaXmlConfigFileTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("java.xml.config.file")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaXmlConfigFile();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("java.xml.config.file"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaxNetSslSessionCacheSizeTest}, hash: 0F998F1B71D7EB97321988E678F7234E
    @Test()
    void getJavaxNetSslSessionCacheSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("javax.net.ssl.sessionCacheSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaxNetSslSessionCacheSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("javax.net.ssl.sessionCacheSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaxRmiSslClientEnabledCipherSuitesTest}, hash: B55DD0A8AF7926A167248470AFDB7C05
    @Test()
    void getJavaxRmiSslClientEnabledCipherSuitesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("javax.rmi.ssl.client.enabledCipherSuites")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaxRmiSslClientEnabledCipherSuites();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("javax.rmi.ssl.client.enabledCipherSuites"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaxRmiSslClientEnabledProtocolsTest}, hash: 9D47CDCA458EBCC5DA961FA0F4F35794
    @Test()
    void getJavaxRmiSslClientEnabledProtocolsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("javax.rmi.ssl.client.enabledProtocols")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaxRmiSslClientEnabledProtocols();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("javax.rmi.ssl.client.enabledProtocols"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaxSecurityAuthUseSubjectCredsOnlyTest}, hash: 5BF5721550AE01200B83CB5C081E7042
    @Test()
    void getJavaxSecurityAuthUseSubjectCredsOnlyTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("javax.security.auth.useSubjectCredsOnly")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaxSecurityAuthUseSubjectCredsOnly();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("javax.security.auth.useSubjectCredsOnly"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJavaxSmartCardIoTerminalFactoryDefaultTypeTest}, hash: 6448E1685142C09176D206C31EB0DE5A
    @Test()
    void getJavaxSmartCardIoTerminalFactoryDefaultTypeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("javax.smartcardio.TerminalFactory.DefaultType")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJavaxSmartCardIoTerminalFactoryDefaultType();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("javax.smartcardio.TerminalFactory.DefaultType"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdbcDriversTest}, hash: 9B7DF9853A38780E8F32F7AD18B68269
    @Test()
    void getJdbcDriversTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdbc.drivers")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdbcDrivers();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdbc.drivers"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpAuthProxyingDisabledSchemesTest}, hash: C8E124621740F33CBE930BEF9A95C2FA
    @Test()
    void getJdkHttpAuthProxyingDisabledSchemesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.http.auth.proxying.disabledSchemes")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpAuthProxyingDisabledSchemes();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.http.auth.proxying.disabledSchemes"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpAuthTunnelingDisabledSchemesTest}, hash: D8C6806B1BB46749F8561AB1D147126D
    @Test()
    void getJdkHttpAuthTunnelingDisabledSchemesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.http.auth.tunneling.disabledSchemes")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpAuthTunnelingDisabledSchemes();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.http.auth.tunneling.disabledSchemes"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientAllowRestrictedHeadersTest}, hash: 531E007ED814278BACDDFDF2D48C532E
    @Test()
    void getJdkHttpClientAllowRestrictedHeadersTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.allowRestrictedHeaders")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientAllowRestrictedHeaders();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.allowRestrictedHeaders"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientAuthRetryLimitTest}, hash: ED12CCAF3789349A28986C4590A1F368
    @Test()
    void getJdkHttpClientAuthRetryLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.auth.retrylimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientAuthRetryLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.auth.retrylimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientBufSizeTest}, hash: 42788A846D8692C572DD5AF7B8577936
    @Test()
    void getJdkHttpClientBufSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.bufsize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientBufSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.bufsize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientConnectionPoolSizeTest}, hash: 779018DAF2D99B1D49ABCF4CC000F679
    @Test()
    void getJdkHttpClientConnectionPoolSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.connectionPoolSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientConnectionPoolSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.connectionPoolSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientConnectionWindowSizeTest}, hash: 8FAA3D75FDD8E3562F0B95E988183BB9
    @Test()
    void getJdkHttpClientConnectionWindowSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.connectionWindowSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientConnectionWindowSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.connectionWindowSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientDisableRetryConnectTest}, hash: 6D51D7E3866AC1670AF942B9CA1301D1
    @Test()
    void getJdkHttpClientDisableRetryConnectTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.disableRetryConnect")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientDisableRetryConnect();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.disableRetryConnect"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientEnableAllMethodRetryTest}, hash: C62CBA2D4CC955CEEC6E681FC05FBF45
    @Test()
    void getJdkHttpClientEnableAllMethodRetryTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.enableAllMethodRetry")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientEnableAllMethodRetry();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.enableAllMethodRetry"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientEnablePushTest}, hash: 2746D2ADEF0E7A78DE81B5B78A4CFB66
    @Test()
    void getJdkHttpClientEnablePushTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.enablepush")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientEnablePush();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.enablepush"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientHpackMaxHeaderTableSizeTest}, hash: 13A2395E91565FA278D15B983E52597C
    @Test()
    void getJdkHttpClientHpackMaxHeaderTableSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.hpack.maxheadertablesize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientHpackMaxHeaderTableSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.hpack.maxheadertablesize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientHttpClientLogTest}, hash: CA824FF1EECAB4DE149158AB21916844
    @Test()
    void getJdkHttpClientHttpClientLogTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.HttpClient.log")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientHttpClientLog();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.HttpClient.log"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientKeepAliveTimeoutTest}, hash: 3F0C9F28B635D33EE9C7FDFEC5C21C2D
    @Test()
    void getJdkHttpClientKeepAliveTimeoutTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.keepalive.timeout")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientKeepAliveTimeout();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.keepalive.timeout"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientKeepAliveTimeoutH2Test}, hash: 1C38950886AD0401F3BD6AC5F53BAF41
    @Test()
    void getJdkHttpClientKeepAliveTimeoutH2Test() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.keepalive.timeout.h2")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientKeepAliveTimeoutH2();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.keepalive.timeout.h2"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientMaxFrameSizeTest}, hash: 45D1B7D5CE8946364FE0B26843A0AC8D
    @Test()
    void getJdkHttpClientMaxFrameSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.maxframesize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientMaxFrameSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.maxframesize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientMaxStreamsTest}, hash: 7855BB6120DB63780584005C2838ACFD
    @Test()
    void getJdkHttpClientMaxStreamsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.maxstreams")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientMaxStreams();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.maxstreams"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientReceiveBufferSizeTest}, hash: 7595707467CCFF48A593DEE5A5E30840
    @Test()
    void getJdkHttpClientReceiveBufferSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.receiveBufferSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientReceiveBufferSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.receiveBufferSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientRedirectsRetryLimitTest}, hash: 006382AE8842160959FBA95FE71D63FD
    @Test()
    void getJdkHttpClientRedirectsRetryLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.redirects.retrylimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientRedirectsRetryLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.redirects.retrylimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientSendBufferSizeTest}, hash: D7FAC0413AE7185831177F2768D4B580
    @Test()
    void getJdkHttpClientSendBufferSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.sendBufferSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientSendBufferSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.sendBufferSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientWebSocketWriteBufferSizeTest}, hash: AFF2D7705DEE747A7BBDCB1C26616CD6
    @Test()
    void getJdkHttpClientWebSocketWriteBufferSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.websocket.writeBufferSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientWebSocketWriteBufferSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.websocket.writeBufferSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpClientWindowSizeTest}, hash: C0FFE0A901A429AC2FB7C40D6C8A5B95
    @Test()
    void getJdkHttpClientWindowSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpclient.windowsize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpClientWindowSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpclient.windowsize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpServerMaxConnectionsTest}, hash: AAB0AD91BDC8B5BCDC7D93C6EF65C0E0
    @Test()
    void getJdkHttpServerMaxConnectionsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.httpserver.maxConnections")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpServerMaxConnections();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.httpserver.maxConnections"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkHttpsNegotiateCbtTest}, hash: 03B77F8C30F7DA1CC3DBE03BD25F877E
    @Test()
    void getJdkHttpsNegotiateCbtTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.https.negotiate.cbt")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkHttpsNegotiateCbt();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.https.negotiate.cbt"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkIncludeInExceptionsTest}, hash: E44DC12FC1409712D685309806F55C05
    @Test()
    void getJdkIncludeInExceptionsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.includeInExceptions")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkIncludeInExceptions();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.includeInExceptions"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkInternalHttpClientDisableHostNameVerificationTest}, hash: 15C2EF29DF7E28DB65DBEF7BFF2A684B
    @Test()
    void getJdkInternalHttpClientDisableHostNameVerificationTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.internal.httpclient.disableHostnameVerification")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkInternalHttpClientDisableHostNameVerification();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.internal.httpclient.disableHostnameVerification"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkIoPermissionsUseCanonicalPathTest}, hash: AC28868AA72BE7C3FB21332C910710AC
    @Test()
    void getJdkIoPermissionsUseCanonicalPathTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.io.permissionsUseCanonicalPath")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkIoPermissionsUseCanonicalPath();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.io.permissionsUseCanonicalPath"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkJndiLdapObjectFactoriesFilterTest}, hash: E202E2D596FA6AD34A4D65A348E0DF3F
    @Test()
    void getJdkJndiLdapObjectFactoriesFilterTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.jndi.ldap.object.factoriesFilter")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkJndiLdapObjectFactoriesFilter();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.jndi.ldap.object.factoriesFilter"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkJndiObjectFactoriesFilterTest}, hash: 6228860E6EC05EE2FABE099A0F2189F1
    @Test()
    void getJdkJndiObjectFactoriesFilterTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.jndi.object.factoriesFilter")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkJndiObjectFactoriesFilter();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.jndi.object.factoriesFilter"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkJndiRmiObjectFactoriesFilterTest}, hash: 79443CE46396FDB64236B9CA2C69222E
    @Test()
    void getJdkJndiRmiObjectFactoriesFilterTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.jndi.rmi.object.factoriesFilter")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkJndiRmiObjectFactoriesFilter();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.jndi.rmi.object.factoriesFilter"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkModuleMainTest}, hash: 7502D2D5EBB6A29AC0E73A3314FC532C
    @Test()
    void getJdkModuleMainTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.module.main")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkModuleMain();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.module.main"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkModuleMainClassTest}, hash: 17358F3BB85CD2951CDE5BEA1BD25BDB
    @Test()
    void getJdkModuleMainClassTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.module.main.class")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkModuleMainClass();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.module.main.class"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkModulePathTest}, hash: 642708016DF3492ADD286B86A91BC419
    @Test()
    void getJdkModulePathTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.module.path")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkModulePath();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.module.path"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkModuleUpgradePathTest}, hash: B43E723D9B4EC11295DCE33DBF9A4AE4
    @Test()
    void getJdkModuleUpgradePathTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.module.upgrade.path")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkModuleUpgradePath();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.module.upgrade.path"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkNetUnixDomainTmpDirTest}, hash: D734F894FF1017BD9EE506AE33D4E0AE
    @Test()
    void getJdkNetUnixDomainTmpDirTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.net.unixdomain.tmpdir")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkNetUnixDomainTmpDir();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.net.unixdomain.tmpdir"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkNetUrlClassPathShowIgnoredClassPathEntriesTest}, hash: 2B41FF3BE7C74F7D6F2A9B298AF8C6FC
    @Test()
    void getJdkNetUrlClassPathShowIgnoredClassPathEntriesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.net.URLClassPath.showIgnoredClassPathEntries")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkNetUrlClassPathShowIgnoredClassPathEntries();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.net.URLClassPath.showIgnoredClassPathEntries"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkSerialFilterTest}, hash: A17EDBACC9670F4B94C19ACE83069A06
    @Test()
    void getJdkSerialFilterTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.serialFilter")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkSerialFilter();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.serialFilter"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkSerialFilterFactoryTest}, hash: C56A301975FC5CA7912179FA643C6693
    @Test()
    void getJdkSerialFilterFactoryTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.serialFilterFactory")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkSerialFilterFactory();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.serialFilterFactory"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkTlsClientSignatureSchemesTest}, hash: 5FC6D2D663F4C4BC0EC66AFBBC9B627B
    @Test()
    void getJdkTlsClientSignatureSchemesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.tls.client.SignatureSchemes")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkTlsClientSignatureSchemes();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.tls.client.SignatureSchemes"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkTlsNamedGroupsTest}, hash: 07A9CACD6CF876E980DFF3FA272653A0
    @Test()
    void getJdkTlsNamedGroupsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.tls.namedGroups")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkTlsNamedGroups();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.tls.namedGroups"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkTlsServerSignatureSchemesTest}, hash: F9B4106B4E8EEF42720B1AC07537AE5C
    @Test()
    void getJdkTlsServerSignatureSchemesTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.tls.server.SignatureSchemes")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkTlsServerSignatureSchemes();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.tls.server.SignatureSchemes"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkVirtualThreadSchedulerMaxPoolSizeTest}, hash: F97FCB2B2290791A90D9DA6906217320
    @Test()
    void getJdkVirtualThreadSchedulerMaxPoolSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.virtualThreadScheduler.maxPoolSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkVirtualThreadSchedulerMaxPoolSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.virtualThreadScheduler.maxPoolSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkVirtualThreadSchedulerParallelismTest}, hash: 88C7BA636278310E3CD8429BD8730FB3
    @Test()
    void getJdkVirtualThreadSchedulerParallelismTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.virtualThreadScheduler.parallelism")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkVirtualThreadSchedulerParallelism();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.virtualThreadScheduler.parallelism"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlCdataChunkSizeTest}, hash: 8F83999972374E02387038B2ADF5B235
    @Test()
    void getJdkXmlCdataChunkSizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.cdataChunkSize")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlCdataChunkSize();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.cdataChunkSize"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlDtdSupportTest}, hash: C81FB8668093546F2E3FD80B94908FCE
    @Test()
    void getJdkXmlDtdSupportTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.dtd.support")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlDtdSupport();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.dtd.support"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlElementAttributeLimitTest}, hash: 9F005821DE6F89F71B60529D12376B84
    @Test()
    void getJdkXmlElementAttributeLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.elementAttributeLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlElementAttributeLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.elementAttributeLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlEnableExtensionFunctionsTest}, hash: 379A57B43337CA5AE8421DEFD0F11CFD
    @Test()
    void getJdkXmlEnableExtensionFunctionsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.enableExtensionFunctions")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlEnableExtensionFunctions();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.enableExtensionFunctions"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlEntityExpansionLimitTest}, hash: 81B30E6B9A4E16101CEF0650A330C911
    @Test()
    void getJdkXmlEntityExpansionLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.entityExpansionLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlEntityExpansionLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.entityExpansionLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlEntityReplacementLimitTest}, hash: C74A69AEBDF34766C9C6997A8F878CFC
    @Test()
    void getJdkXmlEntityReplacementLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.entityReplacementLimi_t")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlEntityReplacementLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.entityReplacementLimi_t"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlIsStandaloneTest}, hash: 6C6B82107A8B2EEA0A7DBFAA11A57B9C
    @Test()
    void getJdkXmlIsStandaloneTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.isStandalone")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlIsStandalone();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.isStandalone"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlJdkCatalogResolveTest}, hash: 79532DA80326FE14E7FB17FD2B0C57FA
    @Test()
    void getJdkXmlJdkCatalogResolveTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.jdkcatalog.resolve")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlJdkCatalogResolve();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.jdkcatalog.resolve"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlMaxElementDepthTest}, hash: 8A535C8BA716433D45726BEBE71E2904
    @Test()
    void getJdkXmlMaxElementDepthTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.maxElementDepth")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlMaxElementDepth();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.maxElementDepth"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlMaxGeneralEntitySizeLimitTest}, hash: 0722660AAD0822068F20D5169E63BE82
    @Test()
    void getJdkXmlMaxGeneralEntitySizeLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.maxGeneralEntitySizeLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlMaxGeneralEntitySizeLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.maxGeneralEntitySizeLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlMaxOccurLimitTest}, hash: F1516686A94D61069A85FA95F2DEA3A8
    @Test()
    void getJdkXmlMaxOccurLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.maxOccurLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlMaxOccurLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.maxOccurLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlMaxParameterEntitySizeLimitTest}, hash: F8958078258200CD54303C34401137E7
    @Test()
    void getJdkXmlMaxParameterEntitySizeLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.maxParameterEntitySizeLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlMaxParameterEntitySizeLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.maxParameterEntitySizeLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlMaxXmlNameLimitTest}, hash: 21F61C3CCC4B5A223239940960AA0CAA
    @Test()
    void getJdkXmlMaxXmlNameLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.maxXMLNameLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlMaxXmlNameLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.maxXMLNameLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlOverrideDefaultParserTest}, hash: 5D3091E8FE4246FED95240236499B4B8
    @Test()
    void getJdkXmlOverrideDefaultParserTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.overrideDefaultParser")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlOverrideDefaultParser();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.overrideDefaultParser"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlResetSymbolTableTest}, hash: 6C08BE4081C808E987DEEE6055C4E28F
    @Test()
    void getJdkXmlResetSymbolTableTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.resetSymbolTable")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlResetSymbolTable();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.resetSymbolTable"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlTotalEntitySizeLimitTest}, hash: AA54FEE450A9D1C53C1282D4AC26536E
    @Test()
    void getJdkXmlTotalEntitySizeLimitTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.totalEntitySizeLimit")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlTotalEntitySizeLimit();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.totalEntitySizeLimit"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJdkXmlXsltcIsStandaloneTest}, hash: A59446D79D122D62536AF08CF8BB780A
    @Test()
    void getJdkXmlXsltcIsStandaloneTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("jdk.xml.xsltcIsStandalone")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getJdkXmlXsltcIsStandalone();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("jdk.xml.xsltcIsStandalone"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLineSeparatorTest}, hash: CD64E542324A38ECCAAF021F11C2966C
    @Test()
    void getLineSeparatorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("line.separator")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getLineSeparator();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("line.separator"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLineSeparator1Test}, hash: 9B4C805B1443BC244022C8FB5758F733
    @Test()
    void getLineSeparator1Test() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("line.separator", supplierMock)).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getLineSeparator(supplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("line.separator", supplierMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLongWhenStrIsNotNull}, hash: 5BF82F2D9EC570A10E49DE546DB7806D
    @Test()
    void getLongWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Arrange Statement(s)
        LongSupplier longSupplierMock = mock(LongSupplier.class);
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn("0");
            //Act Statement(s)
            long result = SystemProperties.getLong("key1", longSupplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0L));
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLongWhenDefaultIfAbsentIsNotNull}, hash: B13609D4CA838682660ED132A4833308
    @Test()
    void getLongWhenDefaultIfAbsentIsNotNull() {
        /* Branches:
         * (str == null) : true
         * (defaultIfAbsent != null) : true
         */
        //Arrange Statement(s)
        LongSupplier defaultIfAbsentMock = mock(LongSupplier.class);
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            doReturn(0L).when(defaultIfAbsentMock).getAsLong();
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn(null);
            //Act Statement(s)
            long result = SystemProperties.getLong("key1", defaultIfAbsentMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0L));
                verify(defaultIfAbsentMock, atLeast(1)).getAsLong();
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLongWhenDefaultIfAbsentIsNull}, hash: C041541848B7ED3A1813895849EB224C
    @Test()
    void getLongWhenDefaultIfAbsentIsNull() {
        /* Branches:
         * (str == null) : true
         * (defaultIfAbsent != null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("key1")).thenReturn(null);
            LongSupplier longSupplier = null;
            //Act Statement(s)
            long result = SystemProperties.getLong("key1", longSupplier);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0L));
                systemProperties.verify(() -> SystemProperties.getProperty("key1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getNativeEncodingTest}, hash: 694F18BD5A15A196B29CCD0166919F46
    @Test()
    void getNativeEncodingTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("native.encoding")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getNativeEncoding();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("native.encoding"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getNetworkAddressCacheNegativeTtlTest}, hash: BAF423C4C25333740291BA76E8C9112A
    @Test()
    void getNetworkAddressCacheNegativeTtlTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("networkaddress.cache.negative.ttl")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getNetworkAddressCacheNegativeTtl();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("networkaddress.cache.negative.ttl"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getNetworkAddressCacheStaleTtlTest}, hash: D1C6795FA0149AE2D1388BC9FE37581F
    @Test()
    void getNetworkAddressCacheStaleTtlTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("networkaddress.cache.stale.ttl")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getNetworkAddressCacheStaleTtl();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("networkaddress.cache.stale.ttl"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getNetworkAddressCacheTtlTest}, hash: 978B7CCBABAA5062CAF886A0F8459DD2
    @Test()
    void getNetworkAddressCacheTtlTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("networkaddress.cache.ttl")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getNetworkAddressCacheTtl();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("networkaddress.cache.ttl"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getOrgJcpXmlDsigSecureValidationTest}, hash: AB41579362FF7F63E29E251B229EBB9B
    @Test()
    void getOrgJcpXmlDsigSecureValidationTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("org.jcp.xml.dsig.securevalidation")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getOrgJcpXmlDsigSecureValidation();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("org.jcp.xml.dsig.securevalidation"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getOrgOpenJdkJavaUtilStreamTripwireTest}, hash: FFAA881E2E31B3722A71BF38094CBF0A
    @Test()
    void getOrgOpenJdkJavaUtilStreamTripwireTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("org.openjdk.java.util.stream.tripwire")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getOrgOpenJdkJavaUtilStreamTripwire();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("org.openjdk.java.util.stream.tripwire"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getOsArchTest}, hash: 9EA3FA84138BEC1CCC33E816645F64F8
    @Test()
    void getOsArchTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("os.arch")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getOsArch();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("os.arch"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getOsNameTest}, hash: 4B1E4AE245C1BF92002F2191DCDFEB53
    @Test()
    void getOsNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("os.name")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getOsName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("os.name"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getOsVersionTest}, hash: 18B54A72BF5F7A38EF05EE27AF6D28F1
    @Test()
    void getOsVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("os.version")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getOsVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("os.version"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPathSeparatorTest}, hash: 9140E3D1BB281604B5BF9C0FFFAE91BA
    @Test()
    void getPathSeparatorTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("path.separator")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getPathSeparator();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("path.separator"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPropertyTest}, hash: 872B3BA2595FC0082ABD7E262CB6142F
    @Test()
    void getPropertyTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty(eq("property1"), (Supplier) any())).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getProperty("property1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty(eq("property1"), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getProperty1Test}, hash: EFA8A39F6D961D80B0E91DB9C27A5D50
    @Test()
    void getProperty1Test() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty(eq("property1"), (Supplier) any())).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getProperty("property1", "defaultIfAbsent1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty(eq("property1"), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getProperty2WhenStringUtilsIsEmptyProperty}, hash: 657FE75544C784A20CA0AE2471A3A57C
    @Disabled()
    @Test()
    void getProperty2WhenStringUtilsIsEmptyProperty() {
        /* Branches:
         * (StringUtils.isEmpty(property)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = SystemProperties.getProperty("property1", supplierMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getProperty2WhenStringUtilsNotIsEmptyProperty}, hash: D6791305BA89E7DFD38C775D11176E95
    @Disabled()
    @Test()
    void getProperty2WhenStringUtilsNotIsEmptyProperty() {
        /* Branches:
         * (StringUtils.isEmpty(property)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  Method java.lang.System::getProperty has a unrepeatable behavior
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = SystemProperties.getProperty("A", supplierMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getProperty2WhenCaughtSecurityException}, hash: F72EDEA753C44DF8B8D6B86054BD9379
    @Disabled()
    @Test()
    void getProperty2WhenCaughtSecurityException() {
        /* Branches:
         * (StringUtils.isEmpty(property)) : true
         * (catch-exception (SecurityException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = SystemProperties.getProperty("property1", supplierMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getProperty2WhenStringUtilsNotIsEmptyPropertyAndCaughtSecurityException}, hash: 6B3AE3CF3174803C44E19E8A41155A9D
    @Disabled()
    @Test()
    void getProperty2WhenStringUtilsNotIsEmptyPropertyAndCaughtSecurityException() {
        /* Branches:
         * (StringUtils.isEmpty(property)) : false
         * (catch-exception (SecurityException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Method java.lang.System::getProperty has a unrepeatable behavior
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = SystemProperties.getProperty("A", supplierMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getSocksProxyHostTest}, hash: D5780812C2DA370B484AB08D4AE997A2
    @Test()
    void getSocksProxyHostTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("socksProxyHost")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSocksProxyHost();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("socksProxyHost"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSocksProxyPortTest}, hash: 64059B24376CEF645BE37D5AABE72133
    @Test()
    void getSocksProxyPortTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("socksProxyPort")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSocksProxyPort();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("socksProxyPort"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSocksProxyVersionTest}, hash: 4237525DAEE090419601E9343753F85E
    @Test()
    void getSocksProxyVersionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("socksProxyVersion")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSocksProxyVersion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("socksProxyVersion"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getStdErrEncodingTest}, hash: F83B37F0AB9C5BCD34F464E46F46B7C1
    @Test()
    void getStdErrEncodingTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("stderr.encoding")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getStdErrEncoding();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("stderr.encoding"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getStdOutEncodingTest}, hash: 497EDEC48F319B49391262C13DEC537D
    @Test()
    void getStdOutEncodingTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("stdout.encoding")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getStdOutEncoding();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("stdout.encoding"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerDrainAmountTest}, hash: 625A2C492BA0F2F1EE6E48C223AE6217
    @Test()
    void getSunNetHttpServerDrainAmountTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.drainAmount")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerDrainAmount();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.drainAmount"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerIdleIntervalTest}, hash: D73A411674E670944CF8FA811BBF3C0C
    @Test()
    void getSunNetHttpServerIdleIntervalTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.idleInterval")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerIdleInterval();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.idleInterval"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerMaxIdleConnectionsTest}, hash: C4B3DA039218E24E0443B3DFA634EBC8
    @Test()
    void getSunNetHttpServerMaxIdleConnectionsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.maxIdleConnections")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerMaxIdleConnections();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.maxIdleConnections"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerMaxReqHeadersTest}, hash: 3FE496DF4153CBF580A6E6F80938D64B
    @Test()
    void getSunNetHttpServerMaxReqHeadersTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.maxReqHeaders")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerMaxReqHeaders();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.maxReqHeaders"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerMaxReqTimeTest}, hash: 169E2D5DBE959A9F73CC6E66C28531F0
    @Test()
    void getSunNetHttpServerMaxReqTimeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.maxReqTime")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerMaxReqTime();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.maxReqTime"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerMaxRspTimeTest}, hash: 06A24F79CCFF831331D28EB0AE1CB049
    @Test()
    void getSunNetHttpServerMaxRspTimeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.maxRspTime")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerMaxRspTime();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.maxRspTime"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunNetHttpServerNoDelayTest}, hash: CCA20AAA0450D2695BD9BA174ADC8EDC
    @Test()
    void getSunNetHttpServerNoDelayTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.net.httpserver.nodelay")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunNetHttpServerNoDelay();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.net.httpserver.nodelay"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSunSecurityKrb5PrincipalTest}, hash: 11F24C587D52F0B2655F0E6A61FD5ED3
    @Test()
    void getSunSecurityKrb5PrincipalTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("sun.security.krb5.principal")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getSunSecurityKrb5Principal();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("sun.security.krb5.principal"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserCountryTest}, hash: 9D4C1CF66D9546433A3732052424EAB8
    @Test()
    void getUserCountryTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.country")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserCountry();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.country"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserDirTest}, hash: 2704EEE5F6F70281DD69D71243ECDF2A
    @Test()
    void getUserDirTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.dir")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserDir();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.dir"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserExtensionsTest}, hash: 5202B0AC2C2AC1D97E28FA4DCDE38026
    @Test()
    void getUserExtensionsTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.extensions")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserExtensions();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.extensions"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserHomeTest}, hash: 83AB2C16CDE353339A74B93100DB42C7
    @Test()
    void getUserHomeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.home")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserHome();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.home"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserLanguageTest}, hash: D06919C239E23A1849E1C75B9964A3F2
    @Test()
    void getUserLanguageTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.language")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserLanguage();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.language"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserNameTest}, hash: 01EDF98B60BEFD5254CAF86C5FE4D8A1
    @Test()
    void getUserNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.name")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserName();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.name"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserName1Test}, hash: A2B364BD835FA3B91808F6A568F72632
    @Test()
    void getUserName1Test() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.name", "defaultValue1")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserName("defaultValue1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.name", "defaultValue1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserRegionTest}, hash: B922AEBC978E9A07A7C7250865E7C957
    @Test()
    void getUserRegionTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.region")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserRegion();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.region"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserScriptTest}, hash: BEB5F0B51A9FC15E7E26A0C18C5C4618
    @Test()
    void getUserScriptTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.script")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserScript();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.script"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserTimezoneTest}, hash: 4347F9C7BCFACA13D8E9581B5149C675
    @Test()
    void getUserTimezoneTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.timezone")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserTimezone();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.timezone"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getUserVariantTest}, hash: 91B72419368A16E904F1697EB257DA6E
    @Test()
    void getUserVariantTest() {
        //Arrange Statement(s)
        try (MockedStatic<SystemProperties> systemProperties = mockStatic(SystemProperties.class, CALLS_REAL_METHODS)) {
            systemProperties.when(() -> SystemProperties.getProperty("user.variant")).thenReturn("return_of_getProperty1");
            //Act Statement(s)
            String result = SystemProperties.getUserVariant();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getProperty1"));
                systemProperties.verify(() -> SystemProperties.getProperty("user.variant"), atLeast(1));
            });
        }
    }
}
