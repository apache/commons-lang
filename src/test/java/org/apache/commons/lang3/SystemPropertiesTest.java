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

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import org.junit.jupiter.api.Test;

public class SystemPropertiesTest {

    private boolean isJava11OrGreater() {
        return SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11);
    }

    @Test
    public void testActualKeys() {
        assertNotNull(SystemProperties.APPLE_AWT_ENABLETEMPLATEIMAGES);
        assertNotNull(SystemProperties.AWT_TOOLKIT);
        assertNotNull(SystemProperties.COM_SUN_JNDI_LDAP_OBJECT_TRUSTSERIALDATA);
        assertNotNull(SystemProperties.COM_SUN_NET_HTTPSERVER_HTTPSERVERPROVIDER);
        assertNotNull(SystemProperties.FILE_ENCODING);
        assertNotNull(SystemProperties.FILE_SEPARATOR);
        assertNotNull(SystemProperties.FTP_NONPROXYHOST);
        assertNotNull(SystemProperties.FTP_PROXYHOST);
        assertNotNull(SystemProperties.FTP_PROXYPORT);
        assertNotNull(SystemProperties.HTTP_AGENT);
        assertNotNull(SystemProperties.HTTP_AUTH_DIGEST_CNONCEREPEAT);
        assertNotNull(SystemProperties.HTTP_AUTH_DIGEST_REENABLEDALGORITHMS);
        assertNotNull(SystemProperties.HTTP_AUTH_DIGEST_VALIDATEPROXY);
        assertNotNull(SystemProperties.HTTP_AUTH_DIGEST_VALIDATESERVER);
        assertNotNull(SystemProperties.HTTP_AUTH_NTLM_DOMAIN);
        assertNotNull(SystemProperties.HTTP_KEEPALIVE);
        assertNotNull(SystemProperties.HTTP_KEEPALIVE_TIME_PROXY);
        assertNotNull(SystemProperties.HTTP_KEEPALIVE_TIME_SERVER);
        assertNotNull(SystemProperties.HTTP_MAXCONNECTIONS);
        assertNotNull(SystemProperties.HTTP_MAXREDIRECTS);
        assertNotNull(SystemProperties.HTTP_NONPROXYHOSTS);
        assertNotNull(SystemProperties.HTTP_PROXYHOST);
        assertNotNull(SystemProperties.HTTP_PROXYPORT);
        assertNotNull(SystemProperties.HTTPS_PROXYHOST);
        assertNotNull(SystemProperties.HTTPS_PROXYPORT);
        assertNotNull(SystemProperties.JAVA_AWT_FONTS);
        assertNotNull(SystemProperties.JAVA_AWT_GRAPHICSENV);
        assertNotNull(SystemProperties.JAVA_AWT_HEADLESS);
        assertNotNull(SystemProperties.JAVA_AWT_PRINTERJOB);
        assertNotNull(SystemProperties.JAVA_CLASS_PATH);
        assertNotNull(SystemProperties.JAVA_CLASS_VERSION);
        assertNotNull(SystemProperties.JAVA_COMPILER);
        assertNotNull(SystemProperties.JAVA_CONTENT_HANDLER_PKGS);
        assertNotNull(SystemProperties.JAVA_ENDORSED_DIRS);
        assertNotNull(SystemProperties.JAVA_EXT_DIRS);
        assertNotNull(SystemProperties.JAVA_HOME);
        assertNotNull(SystemProperties.JAVA_IO_TMPDIR);
        assertNotNull(SystemProperties.JAVA_LIBRARY_PATH);
        assertNotNull(SystemProperties.JAVA_LOCALE_PROVIDERS);
        assertNotNull(SystemProperties.JAVA_LOCALE_USEOLDISOCODES);
        assertNotNull(SystemProperties.JAVA_NET_PREFERIPV4STACK);
        assertNotNull(SystemProperties.JAVA_NET_PREFERIPV6ADDRESSES);
        assertNotNull(SystemProperties.JAVA_NET_SOCKS_PASSWORD);
        assertNotNull(SystemProperties.JAVA_NET_SOCKS_USERNAME);
        assertNotNull(SystemProperties.JAVA_NET_USESYSTEMPROXIES);
        assertNotNull(SystemProperties.JAVA_NIO_CHANNELS_DEFAULTTHREADPOOL_INITIALSIZE);
        assertNotNull(SystemProperties.JAVA_NIO_CHANNELS_DEFAULTTHREADPOOL_THREADFACTORY);
        assertNotNull(SystemProperties.JAVA_NIO_CHANNELS_SPI_ASYNCHRONOUSCHANNELPROVIDER);
        assertNotNull(SystemProperties.JAVA_NIO_CHANNELS_SPI_SELECTORPROVIDER);
        assertNotNull(SystemProperties.JAVA_NIO_FILE_SPI_DEFAULTFILESYSTEMPROVIDER);
        assertNotNull(SystemProperties.JAVA_PROPERTIES_DATE);
        assertNotNull(SystemProperties.JAVA_PROTOCOL_HANDLER_PKGS);
        assertNotNull(SystemProperties.JAVA_RMI_SERVER_CODEBASE);
        assertNotNull(SystemProperties.JAVA_RMI_SERVER_HOSTNAME);
        assertNotNull(SystemProperties.JAVA_RMI_SERVER_RANDOMIDS);
        assertNotNull(SystemProperties.JAVA_RMI_SERVER_RMICLASSLOADERSPI);
        assertNotNull(SystemProperties.JAVA_RUNTIME_NAME);
        assertNotNull(SystemProperties.JAVA_RUNTIME_VERSION);
        assertNotNull(SystemProperties.JAVA_SECURITY_AUTH_LOGIN_CONFIG);
        assertNotNull(SystemProperties.JAVA_SECURITY_MANAGER);
        assertNotNull(SystemProperties.JAVA_SPECIFICATION_MAINTENANCE_VERSION);
        assertNotNull(SystemProperties.JAVA_SPECIFICATION_NAME);
        assertNotNull(SystemProperties.JAVA_SPECIFICATION_VENDOR);
        assertNotNull(SystemProperties.JAVA_SPECIFICATION_VERSION);
        assertNotNull(SystemProperties.JAVA_SYSTEM_CLASS_LOADER);
        assertNotNull(SystemProperties.JAVA_TIME_ZONE_DEFAULTZONERULESPROVIDER);
        assertNotNull(SystemProperties.JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_EXCEPTIONHANDLER);
        assertNotNull(SystemProperties.JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_MAXIMUMSPARES);
        assertNotNull(SystemProperties.JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_PARALLELISM);
        assertNotNull(SystemProperties.JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_THREADFACTORY);
        assertNotNull(SystemProperties.JAVA_UTIL_CURRENCY_DATA);
        assertNotNull(SystemProperties.JAVA_UTIL_LOGGING_CONFIG_CLASS);
        assertNotNull(SystemProperties.JAVA_UTIL_LOGGING_CONFIG_FILE);
        assertNotNull(SystemProperties.JAVA_UTIL_LOGGING_SIMPLEFORMATTER_FORMAT);
        assertNotNull(SystemProperties.JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
        assertNotNull(SystemProperties.JAVA_UTIL_PREFS_PREFERENCESFACTORY);
        assertNotNull(SystemProperties.JAVA_UTIL_PROPERTYRESOURCEBUNDLE_ENCODING);
        assertNotNull(SystemProperties.JAVA_VENDOR);
        assertNotNull(SystemProperties.JAVA_VENDOR_URL);
        assertNotNull(SystemProperties.JAVA_VENDOR_VERSION);
        assertNotNull(SystemProperties.JAVA_VERSION);
        assertNotNull(SystemProperties.JAVA_VERSION_DATE);
        assertNotNull(SystemProperties.JAVA_VM_INFO);
        assertNotNull(SystemProperties.JAVA_VM_NAME);
        assertNotNull(SystemProperties.JAVA_VM_SPECIFICATION_NAME);
        assertNotNull(SystemProperties.JAVA_VM_SPECIFICATION_VENDOR);
        assertNotNull(SystemProperties.JAVA_VM_SPECIFICATION_VERSION);
        assertNotNull(SystemProperties.JAVA_VM_VENDOR);
        assertNotNull(SystemProperties.JAVA_VM_VERSION);
        assertNotNull(SystemProperties.JAVA_XML_CONFIG_FILE);
        assertNotNull(SystemProperties.JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES);
        assertNotNull(SystemProperties.JAVAX_NET_SSL_SESSIONCACHESIZE);
        assertNotNull(SystemProperties.JAVAX_RMI_SSL_CLIENT_ENABLEDCIPHERSUITES);
        assertNotNull(SystemProperties.JAVAX_RMI_SSL_CLIENT_ENABLEDPROTOCOLS);
        assertNotNull(SystemProperties.JAVAX_SECURITY_AUTH_USESUBJECTCREDSONLY);
        assertNotNull(SystemProperties.JAVAX_SMARTCARDIO_TERMINALFACTORY_DEFAULTTYPE);
        assertNotNull(SystemProperties.JDBC_DRIVERS);
        assertNotNull(SystemProperties.JDK_HTTP_AUTH_PROXYING_DISABLEDSCHEMES);
        assertNotNull(SystemProperties.JDK_HTTP_AUTH_TUNNELING_DISABLEDSCHEMES);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_ALLOWRESTRICTEDHEADERS);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_AUTH_RETRYLIMIT);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_BUFSIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_CONNECTIONPOOLSIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_CONNECTIONWINDOWSIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_DISABLERETRYCONNECT);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_ENABLEALLMETHODRETRY);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_ENABLEPUSH);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_HPACK_MAXHEADERTABLESIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_HTTPCLIENT_LOG);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_KEEPALIVE_TIMEOUT);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_KEEPALIVE_TIMEOUT_H2);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_MAXFRAMESIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_MAXSTREAMS);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_RECEIVEBUFFERSIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_REDIRECTS_RETRYLIMIT);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_SENDBUFFERSIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_WEBSOCKET_WRITEBUFFERSIZE);
        assertNotNull(SystemProperties.JDK_HTTPCLIENT_WINDOWSIZE);
        assertNotNull(SystemProperties.JDK_HTTPS_NEGOTIATE_CBT);
        assertNotNull(SystemProperties.JDK_HTTPSERVER_MAXCONNECTIONS);
        assertNotNull(SystemProperties.JDK_INCLUDEINEXCEPTIONS);
        assertNotNull(SystemProperties.JDK_INTERNAL_HTTPCLIENT_DISABLEHOSTNAMEVERIFICATION);
        assertNotNull(SystemProperties.JDK_IO_PERMISSIONSUSECANONICALPATH);
        assertNotNull(SystemProperties.JDK_JNDI_LDAP_OBJECT_FACTORIESFILTER);
        assertNotNull(SystemProperties.JDK_JNDI_OBJECT_FACTORIESFILTER);
        assertNotNull(SystemProperties.JDK_JNDI_RMI_OBJECT_FACTORIESFILTER);
        assertNotNull(SystemProperties.JDK_MODULE_MAIN);
        assertNotNull(SystemProperties.JDK_MODULE_MAIN_CLASS);
        assertNotNull(SystemProperties.JDK_MODULE_PATH);
        assertNotNull(SystemProperties.JDK_MODULE_UPGRADE_PATH);
        assertNotNull(SystemProperties.JDK_NET_UNIXDOMAIN_TMPDIR);
        assertNotNull(SystemProperties.JDK_NET_URLCLASSPATH_SHOWIGNOREDCLASSPATHENTRIES);
        assertNotNull(SystemProperties.JDK_SERIALFILTER);
        assertNotNull(SystemProperties.JDK_SERIALFILTERFACTORY);
        assertNotNull(SystemProperties.JDK_TLS_CLIENT_SIGNATURESCHEMES);
        assertNotNull(SystemProperties.JDK_TLS_NAMEDGROUPS);
        assertNotNull(SystemProperties.JDK_TLS_SERVER_SIGNATURESCHEMES);
        assertNotNull(SystemProperties.JDK_VIRTUALTHREADSCHEDULER_MAXPOOLSIZE);
        assertNotNull(SystemProperties.JDK_VIRTUALTHREADSCHEDULER_PARALLELISM);
        assertNotNull(SystemProperties.JDK_XML_CDATACHUNKSIZE);
        assertNotNull(SystemProperties.JDK_XML_DTD_SUPPORT);
        assertNotNull(SystemProperties.JDK_XML_ELEMENTATTRIBUTELIMIT);
        assertNotNull(SystemProperties.JDK_XML_ENABLEEXTENSIONFUNCTIONS);
        assertNotNull(SystemProperties.JDK_XML_ENTITYEXPANSIONLIMIT);
        assertNotNull(SystemProperties.JDK_XML_ENTITYREPLACEMENTLIMIT);
        assertNotNull(SystemProperties.JDK_XML_ISSTANDALONE);
        assertNotNull(SystemProperties.JDK_XML_JDKCATALOG_RESOLVE);
        assertNotNull(SystemProperties.JDK_XML_MAXELEMENTDEPTH);
        assertNotNull(SystemProperties.JDK_XML_MAXGENERALENTITYSIZELIMIT);
        assertNotNull(SystemProperties.JDK_XML_MAXOCCURLIMIT);
        assertNotNull(SystemProperties.JDK_XML_MAXPARAMETERENTITYSIZELIMIT);
        assertNotNull(SystemProperties.JDK_XML_MAXXMLNAMELIMIT);
        assertNotNull(SystemProperties.JDK_XML_OVERRIDEDEFAULTPARSER);
        assertNotNull(SystemProperties.JDK_XML_RESETSYMBOLTABLE);
        assertNotNull(SystemProperties.JDK_XML_TOTALENTITYSIZELIMIT);
        assertNotNull(SystemProperties.JDK_XML_XSLTCISSTANDALONE);
        assertNotNull(SystemProperties.LINE_SEPARATOR);
        assertNotNull(SystemProperties.NATIVE_ENCODING);
        assertNotNull(SystemProperties.NETWORKADDRESS_CACHE_NEGATIVE_TTL);
        assertNotNull(SystemProperties.NETWORKADDRESS_CACHE_STALE_TTL);
        assertNotNull(SystemProperties.NETWORKADDRESS_CACHE_TTL);
        assertNotNull(SystemProperties.ORG_JCP_XML_DSIG_SECUREVALIDATION);
        assertNotNull(SystemProperties.ORG_OPENJDK_JAVA_UTIL_STREAM_TRIPWIRE);
        assertNotNull(SystemProperties.OS_ARCH);
        assertNotNull(SystemProperties.OS_NAME);
        assertNotNull(SystemProperties.OS_VERSION);
        assertNotNull(SystemProperties.PATH_SEPARATOR);
        assertNotNull(SystemProperties.SOCKSPROXYHOST);
        assertNotNull(SystemProperties.SOCKSPROXYPORT);
        assertNotNull(SystemProperties.SOCKSPROXYVERSION);
        assertNotNull(SystemProperties.STDERR_ENCODING);
        assertNotNull(SystemProperties.STDOUT_ENCODING);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_DRAINAMOUNT);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_IDLEINTERVAL);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_MAXIDLECONNECTIONS);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_MAXREQHEADERS);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_MAXREQTIME);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_MAXRSPTIME);
        assertNotNull(SystemProperties.SUN_NET_HTTPSERVER_NODELAY);
        assertNotNull(SystemProperties.SUN_SECURITY_KRB5_PRINCIPAL);
        assertNotNull(SystemProperties.USER_COUNTRY);
        assertNotNull(SystemProperties.USER_DIR);
        assertNotNull(SystemProperties.USER_EXTENSIONS);
        assertNotNull(SystemProperties.USER_HOME);
        assertNotNull(SystemProperties.USER_LANGUAGE);
        assertNotNull(SystemProperties.USER_NAME);
        assertNotNull(SystemProperties.USER_REGION);
        assertNotNull(SystemProperties.USER_SCRIPT);
        assertNotNull(SystemProperties.USER_TIMEZONE);
        assertNotNull(SystemProperties.USER_VARIANT);
    }

    @Test
    public void testGetAwtToolkit() {
        assertDoesNotThrow(SystemProperties::getAwtToolkit);
    }

    @Test
    public void testGetBoolean() {
        final String key = RandomStringUtils.random(10);
        final String absentKey = RandomStringUtils.random(10);
        assertNull(System.getProperty(absentKey));
        try {
            System.setProperty(key, Boolean.toString(Boolean.TRUE));
            assertEquals(Boolean.TRUE, SystemProperties.getBoolean(key, () -> false));
            assertEquals(Boolean.TRUE, SystemProperties.getBoolean(absentKey, () -> Boolean.TRUE));
            assertFalse(SystemProperties.getBoolean(absentKey, () -> false));
            assertFalse(SystemProperties.getBoolean(absentKey, null));
        } finally {
            System.clearProperty(key);
        }
    }

    @Test
    public void testGetFileEncoding() {
        assertNotNull(SystemProperties.getFileEncoding());
    }

    @Test
    public void testGetFileSeparator() {
        assertNotNull(SystemProperties.getFileSeparator());
    }

    @Test
    public void testGetInt() {
        final String key = RandomStringUtils.random(10);
        final String absentKey = RandomStringUtils.random(10);
        assertNull(System.getProperty(absentKey));
        try {
            System.setProperty(key, Integer.toString(Integer.MAX_VALUE));
            assertEquals(Integer.MAX_VALUE, SystemProperties.getInt(key, () -> 0));
            assertEquals(Integer.MAX_VALUE, SystemProperties.getInt(absentKey, () -> Integer.MAX_VALUE));
            assertEquals(0, SystemProperties.getInt(absentKey, () -> 0));
            assertEquals(0, SystemProperties.getInt(absentKey, null));
        } finally {
            System.clearProperty(key);
        }
    }

    @Test
    public void testGetJavaAwtFonts() {
        assertNull(SystemProperties.getJavaAwtFonts());
    }

    @Test
    public void testGetJavaAwtGraphicsenv() {
        assertDoesNotThrow(SystemProperties::getJavaAwtGraphicsenv);
    }

    @Test
    public void testGetJavaAwtHeadless() {
        assertNull(SystemProperties.getJavaAwtHeadless());
    }

    @Test
    public void testGetJavaAwtPrinterjob() {
        assertDoesNotThrow(SystemProperties::getJavaAwtPrinterjob);
    }

    @Test
    public void testGetJavaClassPath() {
        assertNotNull(SystemProperties.getJavaClassPath());
    }

    @Test
    public void testGetJavaClassVersion() {
        assertNotNull(SystemProperties.getJavaClassVersion());
    }

    @Test
    public void testGetJavaCompiler() {
        if (SystemUtils.IS_JAVA_14) {
            // Not in Java 11
            assertNotNull(SystemProperties.getJavaCompiler());
        }
    }

    @Test
    public void testGetJavaEndorsedDirs() {
        if (isJava11OrGreater()) {
            // Not in Java 11
            assertNull(SystemProperties.getJavaEndorsedDirs());
        } else {
            assertNotNull(SystemProperties.getJavaExtDirs());
        }
    }

    @Test
    public void testGetJavaExtDirs() {
        if (isJava11OrGreater()) {
            // Not in Java 11
            assertNull(SystemProperties.getJavaExtDirs());
        } else {
            assertNotNull(SystemProperties.getJavaExtDirs());
        }
    }

    @Test
    public void testGetJavaHome() {
        assertNotNull(SystemProperties.getJavaHome());
    }

    @Test
    public void testGetJavaIoTmpdir() {
        assertNotNull(SystemProperties.getJavaIoTmpdir());
    }

    @Test
    public void testGetJavaLibraryPath() {
        assertNotNull(SystemProperties.getJavaLibraryPath());
    }

    @Test
    public void testGetJavaLocaleProviders() {
        assumeTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
        // default is null
        assertNull(SystemProperties.getJavaLocaleProviders(), SystemProperties.getJavaVersion());
    }

    @Test
    public void testGetJavaRuntimeName() {
        assertNotNull(SystemProperties.getJavaRuntimeName());
    }

    @Test
    public void testGetJavaRuntimeVersion() {
        assertNotNull(SystemProperties.getJavaRuntimeVersion());
    }

    @Test
    public void testGetJavaSpecificationName() {
        assertNotNull(SystemProperties.getJavaSpecificationName());
    }

    @Test
    public void testGetJavaSpecificationVendor() {
        assertNotNull(SystemProperties.getJavaSpecificationVendor());
    }

    @Test
    public void testGetJavaSpecificationVersion() {
        assertNotNull(SystemProperties.getJavaSpecificationVersion());
    }

    @Test
    public void testGetJavaSpecificationVersionSupplier() {
        assertNotNull(SystemProperties.getJavaSpecificationVersion(() -> "99.0"));
    }

    @Test
    public void testGetJavaUtilPrefsPreferencesFactory() {
        assertNull(SystemProperties.getJavaUtilPrefsPreferencesFactory());
    }

    @Test
    public void testGetJavaVendor() {
        assertNotNull(SystemProperties.getJavaVendor());
    }

    @Test
    public void testGetJavaVendorUrl() {
        assertNotNull(SystemProperties.getJavaVendorUrl());
    }

    @Test
    public void testGetJavaVersion() {
        assertNotNull(SystemProperties.getJavaVersion());
    }

    @Test
    public void testGetJavaVmInfo() {
        assertNotNull(SystemProperties.getJavaVmInfo());
    }

    @Test
    public void testGetJavaVmName() {
        assertNotNull(SystemProperties.getJavaVmName());
    }

    @Test
    public void testGetJavaVmSpecificationName() {
        assertNotNull(SystemProperties.getJavaVmSpecificationName());
    }

    @Test
    public void testGetJavaVmSpecificationVendor() {
        assertNotNull(SystemProperties.getJavaVmSpecificationVendor());
    }

    @Test
    public void testGetJavaVmSpecificationVersion() {
        assertNotNull(SystemProperties.getJavaVmSpecificationVersion());
    }

    @Test
    public void testGetJavaVmVendor() {
        assertNotNull(SystemProperties.getJavaVmVendor());
    }

    @Test
    public void testGetJavaVmVersion() {
        assertNotNull(SystemProperties.getJavaVmVersion());
    }

    @Test
    public void testGetLineSeparator() {
        assertNotNull(SystemProperties.getLineSeparator());
        assertNotNull(SystemProperties.getLineSeparator(() -> ""));
        assertNotNull(SystemProperties.getLineSeparator(() -> "\n"));
        assertNotNull(SystemProperties.getLineSeparator(() -> null));
        assertNotNull(SystemProperties.getLineSeparator(null));
    }

    @Test
    public void testGetLong() {
        final String key = RandomStringUtils.random(10);
        final String absentKey = RandomStringUtils.random(10);
        assertNull(System.getProperty(absentKey));
        try {
            System.setProperty(key, Long.toString(Long.MAX_VALUE));
            assertEquals(Long.MAX_VALUE, SystemProperties.getLong(key, () -> 0));
            assertEquals(Long.MAX_VALUE, SystemProperties.getLong(absentKey, () -> Long.MAX_VALUE));
            assertEquals(0, SystemProperties.getLong(absentKey, () -> 0));
            assertEquals(0, SystemProperties.getLong(absentKey, null));
        } finally {
            System.clearProperty(key);
        }
    }

    @Test
    public void testGetOsArch() {
        assertNotNull(SystemProperties.getOsArch());
    }

    @Test
    public void testGetOsName() {
        assertNotNull(SystemProperties.getOsName());
    }

    @Test
    public void testGetOsVersion() {
        assertNotNull(SystemProperties.getOsVersion());
    }

    @Test
    public void testGetPathSeparator() {
        assertNotNull(SystemProperties.getPathSeparator());
    }

    @Test
    public void testGetUserCountry() {
        assertDoesNotThrow(SystemProperties::getUserCountry);
    }

    @Test
    public void testGetUserDir() {
        assertNotNull(SystemProperties.getUserDir());
    }

    @Test
    public void testGetUserHome() {
        assertNotNull(SystemProperties.getUserHome());
    }

    @Test
    public void testGetUserLanguage() {
        assertNotNull(SystemProperties.getUserLanguage());
    }

    @Test
    public void testGetUserName() {
        assertNotNull(SystemProperties.getUserName());
        assertNotNull(SystemProperties.getUserName(() -> ""));
        assertNotNull(SystemProperties.getUserName(() -> "User"));
        assertNotNull(SystemProperties.getUserName(() -> null));
        assertNotNull(SystemProperties.getUserName(null));
    }

    @Test
    public void testGetUserTimezone() {
        assertDoesNotThrow(SystemProperties::getUserTimezone);
    }

}
