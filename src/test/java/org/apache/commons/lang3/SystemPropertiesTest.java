/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.nio.file.Paths;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.ThrowingSupplier;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.junitpioneer.jupiter.SetSystemProperty.SetSystemProperties;

@SetSystemProperties({
    @SetSystemProperty(key = SystemPropertiesTest.KEY_SPACE_1, value = "value1"),
    @SetSystemProperty(key = SystemPropertiesTest.KEY_TAB_1, value = "value2") })
class SystemPropertiesTest {

    private static final String SIMPLE_NAME = SystemPropertiesTest.class.getSimpleName();
    static final String KEY_SPACE_1 = " ";
    static final String KEY_TAB_1 = "\t";

    private void basicKeyCheck(final String key) {
        assertNotNull(key);
        assertFalse(key.isEmpty());
        assertDoesNotThrow(() -> System.getProperties().get(key));
        assertDoesNotThrow(() -> System.getProperty(key));
        assertDoesNotThrow(() -> System.getProperty(key, ""));
// Debug/Info
//        if (!System.getProperties().containsKey(key)) {
//            System.out.printf("No key '%s'%n", key);
//        } else if (System.getProperty(key) == null) {
//            System.out.printf("Null value at key '%s'%n", key);
//        }
    }

    private boolean isJava11OrGreater() {
        return SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11);
    }

    @Test
    void testActualKeys() {
        basicKeyCheck(SystemProperties.APPLE_AWT_ENABLE_TEMPLATE_IMAGES);
        basicKeyCheck(SystemProperties.AWT_TOOLKIT);
        basicKeyCheck(SystemProperties.COM_SUN_JNDI_LDAP_OBJECT_TRUST_SERIAL_DATA);
        basicKeyCheck(SystemProperties.COM_SUN_NET_HTTP_SERVER_HTTP_SERVER_PROVIDER);
        basicKeyCheck(SystemProperties.FILE_ENCODING);
        basicKeyCheck(SystemProperties.FILE_SEPARATOR);
        basicKeyCheck(SystemProperties.FTP_NON_PROXY_HOST);
        basicKeyCheck(SystemProperties.FTP_PROXY_HOST);
        basicKeyCheck(SystemProperties.FTP_PROXY_PORT);
        basicKeyCheck(SystemProperties.HTTP_AGENT);
        basicKeyCheck(SystemProperties.HTTP_AUTH_DIGEST_CNONCE_REPEAT);
        basicKeyCheck(SystemProperties.HTTP_AUTH_DIGEST_RE_ENABLED_ALGORITHMS);
        basicKeyCheck(SystemProperties.HTTP_AUTH_DIGEST_VALIDATE_PROXY);
        basicKeyCheck(SystemProperties.HTTP_AUTH_DIGEST_VALIDATE_SERVER);
        basicKeyCheck(SystemProperties.HTTP_AUTH_NTLM_DOMAIN);
        basicKeyCheck(SystemProperties.HTTP_KEEP_ALIVE);
        basicKeyCheck(SystemProperties.HTTP_KEEP_ALIVE_TIME_PROXY);
        basicKeyCheck(SystemProperties.HTTP_KEEP_ALIVE_TIME_SERVER);
        basicKeyCheck(SystemProperties.HTTP_MAX_CONNECTIONS);
        basicKeyCheck(SystemProperties.HTTP_MAX_REDIRECTS);
        basicKeyCheck(SystemProperties.HTTP_NON_PROXY_HOSTS);
        basicKeyCheck(SystemProperties.HTTP_PROXY_HOST);
        basicKeyCheck(SystemProperties.HTTP_PROXY_PORT);
        basicKeyCheck(SystemProperties.HTTPS_PROXY_HOST);
        basicKeyCheck(SystemProperties.HTTPS_PROXY_PORT);
        basicKeyCheck(SystemProperties.JAVA_AWT_FONTS);
        basicKeyCheck(SystemProperties.JAVA_AWT_GRAPHICSENV);
        basicKeyCheck(SystemProperties.JAVA_AWT_HEADLESS);
        basicKeyCheck(SystemProperties.JAVA_AWT_PRINTERJOB);
        basicKeyCheck(SystemProperties.JAVA_CLASS_PATH);
        basicKeyCheck(SystemProperties.JAVA_CLASS_VERSION);
        basicKeyCheck(SystemProperties.JAVA_COMPILER);
        basicKeyCheck(SystemProperties.JAVA_CONTENT_HANDLER_PKGS);
        basicKeyCheck(SystemProperties.JAVA_ENDORSED_DIRS);
        basicKeyCheck(SystemProperties.JAVA_EXT_DIRS);
        basicKeyCheck(SystemProperties.JAVA_HOME);
        basicKeyCheck(SystemProperties.JAVA_IO_TMPDIR);
        basicKeyCheck(SystemProperties.JAVA_LIBRARY_PATH);
        basicKeyCheck(SystemProperties.JAVA_LOCALE_PROVIDERS);
        basicKeyCheck(SystemProperties.JAVA_LOCALE_USE_OLD_ISO_CODES);
        basicKeyCheck(SystemProperties.JAVA_NET_PREFER_IPV4_STACK);
        basicKeyCheck(SystemProperties.JAVA_NET_PREFER_IPV6_ADDRESSES);
        basicKeyCheck(SystemProperties.JAVA_NET_SOCKS_PASSWORD);
        basicKeyCheck(SystemProperties.JAVA_NET_SOCKS_USER_NAME);
        basicKeyCheck(SystemProperties.JAVA_NET_USE_SYSTEM_PROXIES);
        basicKeyCheck(SystemProperties.JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_INITIAL_SIZE);
        basicKeyCheck(SystemProperties.JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_THREAD_FACTORY);
        basicKeyCheck(SystemProperties.JAVA_NIO_CHANNELS_SPI_ASYNCHRONOUS_CHANNEL_PROVIDER);
        basicKeyCheck(SystemProperties.JAVA_NIO_CHANNELS_SPI_SELECTOR_PROVIDER);
        basicKeyCheck(SystemProperties.JAVA_NIO_FILE_SPI_DEFAULT_FILE_SYSTEM_PROVIDER);
        basicKeyCheck(SystemProperties.JAVA_PROPERTIES_DATE);
        basicKeyCheck(SystemProperties.JAVA_PROTOCOL_HANDLER_PKGS);
        basicKeyCheck(SystemProperties.JAVA_RMI_SERVER_CODEBASE);
        basicKeyCheck(SystemProperties.JAVA_RMI_SERVER_HOST_NAME);
        basicKeyCheck(SystemProperties.JAVA_RMI_SERVER_RANDOM_IDS);
        basicKeyCheck(SystemProperties.JAVA_RMI_SERVER_RMI_CLASS_LOADER_SPI);
        basicKeyCheck(SystemProperties.JAVA_RUNTIME_NAME);
        basicKeyCheck(SystemProperties.JAVA_RUNTIME_VERSION);
        basicKeyCheck(SystemProperties.JAVA_SECURITY_AUTH_LOGIN_CONFIG);
        basicKeyCheck(SystemProperties.JAVA_SECURITY_KERBEROS_CONF);
        basicKeyCheck(SystemProperties.JAVA_SECURITY_KERBEROS_KDC);
        basicKeyCheck(SystemProperties.JAVA_SECURITY_KERBEROS_REALM);
        basicKeyCheck(SystemProperties.JAVA_SECURITY_DEBUG);
        basicKeyCheck(SystemProperties.JAVA_SECURITY_MANAGER);
        basicKeyCheck(SystemProperties.JAVA_SPECIFICATION_MAINTENANCE_VERSION);
        basicKeyCheck(SystemProperties.JAVA_SPECIFICATION_NAME);
        basicKeyCheck(SystemProperties.JAVA_SPECIFICATION_VENDOR);
        basicKeyCheck(SystemProperties.JAVA_SPECIFICATION_VERSION);
        basicKeyCheck(SystemProperties.JAVA_SYSTEM_CLASS_LOADER);
        basicKeyCheck(SystemProperties.JAVA_TIME_ZONE_DEFAULT_ZONE_RULES_PROVIDER);
        basicKeyCheck(SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_EXCEPTION_HANDLER);
        basicKeyCheck(SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_MAXIMUM_SPARES);
        basicKeyCheck(SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_PARALLELISM);
        basicKeyCheck(SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_THREAD_FACTORY);
        basicKeyCheck(SystemProperties.JAVA_UTIL_CURRENCY_DATA);
        basicKeyCheck(SystemProperties.JAVA_UTIL_LOGGING_CONFIG_CLASS);
        basicKeyCheck(SystemProperties.JAVA_UTIL_LOGGING_CONFIG_FILE);
        basicKeyCheck(SystemProperties.JAVA_UTIL_LOGGING_SIMPLE_FORMATTER_FORMAT);
        basicKeyCheck(SystemProperties.JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
        basicKeyCheck(SystemProperties.JAVA_UTIL_PROPERTY_RESOURCE_BUNDLE_ENCODING);
        basicKeyCheck(SystemProperties.JAVA_VENDOR);
        basicKeyCheck(SystemProperties.JAVA_VENDOR_URL);
        basicKeyCheck(SystemProperties.JAVA_VENDOR_VERSION);
        basicKeyCheck(SystemProperties.JAVA_VERSION);
        basicKeyCheck(SystemProperties.JAVA_VERSION_DATE);
        basicKeyCheck(SystemProperties.JAVA_VM_INFO);
        basicKeyCheck(SystemProperties.JAVA_VM_NAME);
        basicKeyCheck(SystemProperties.JAVA_VM_SPECIFICATION_NAME);
        basicKeyCheck(SystemProperties.JAVA_VM_SPECIFICATION_VENDOR);
        basicKeyCheck(SystemProperties.JAVA_VM_SPECIFICATION_VERSION);
        basicKeyCheck(SystemProperties.JAVA_VM_VENDOR);
        basicKeyCheck(SystemProperties.JAVA_VM_VERSION);
        basicKeyCheck(SystemProperties.JAVA_XML_CONFIG_FILE);
        basicKeyCheck(SystemProperties.JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES);
        basicKeyCheck(SystemProperties.JAVAX_NET_SSL_SESSION_CACHE_SIZE);
        basicKeyCheck(SystemProperties.JAVAX_RMI_SSL_CLIENT_ENABLED_CIPHER_SUITES);
        basicKeyCheck(SystemProperties.JAVAX_RMI_SSL_CLIENT_ENABLED_PROTOCOLS);
        basicKeyCheck(SystemProperties.JAVAX_SECURITY_AUTH_USE_SUBJECT_CREDS_ONLY);
        basicKeyCheck(SystemProperties.JAVAX_SMART_CARD_IO_TERMINAL_FACTORY_DEFAULT_TYPE);
        basicKeyCheck(SystemProperties.JDBC_DRIVERS);
        basicKeyCheck(SystemProperties.JDK_HTTP_AUTH_PROXYING_DISABLED_SCHEMES);
        basicKeyCheck(SystemProperties.JDK_HTTP_AUTH_TUNNELING_DISABLED_SCHEMES);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_ALLOW_RESTRICTED_HEADERS);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_AUTH_RETRY_LIMIT);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_BUF_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_CONNECTION_POOL_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_CONNECTION_WINDOW_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_DISABLE_RETRY_CONNECT);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_ENABLE_ALL_METHOD_RETRY);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_ENABLE_PUSH);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_HPACK_MAX_HEADER_TABLE_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_HTTP_CLIENT_LOG);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT_H2);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_MAX_FRAME_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_MAX_STREAMS);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_RECEIVE_BUFFER_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_REDIRECTS_RETRY_LIMIT);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_SEND_BUFFER_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_WEB_SOCKET_WRITE_BUFFER_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTP_CLIENT_WINDOW_SIZE);
        basicKeyCheck(SystemProperties.JDK_HTTPS_NEGOTIATE_CBT);
        basicKeyCheck(SystemProperties.JDK_HTTP_SERVER_MAX_CONNECTIONS);
        basicKeyCheck(SystemProperties.JDK_INCLUDE_IN_EXCEPTIONS);
        basicKeyCheck(SystemProperties.JDK_INTERNAL_HTTP_CLIENT_DISABLE_HOST_NAME_VERIFICATION);
        basicKeyCheck(SystemProperties.JDK_IO_PERMISSIONS_USE_CANONICAL_PATH);
        basicKeyCheck(SystemProperties.JDK_JNDI_LDAP_OBJECT_FACTORIES_FILTER);
        basicKeyCheck(SystemProperties.JDK_JNDI_OBJECT_FACTORIES_FILTER);
        basicKeyCheck(SystemProperties.JDK_JNDI_RMI_OBJECT_FACTORIES_FILTER);
        basicKeyCheck(SystemProperties.JDK_MODULE_MAIN);
        basicKeyCheck(SystemProperties.JDK_MODULE_MAIN_CLASS);
        basicKeyCheck(SystemProperties.JDK_MODULE_PATH);
        basicKeyCheck(SystemProperties.JDK_MODULE_UPGRADE_PATH);
        basicKeyCheck(SystemProperties.JDK_NET_UNIX_DOMAIN_TMPDIR);
        basicKeyCheck(SystemProperties.JDK_NET_URL_CLASS_PATH_SHOW_IGNORED_CLASS_PATH_ENTRIES);
        basicKeyCheck(SystemProperties.JDK_SERIAL_FILTER);
        basicKeyCheck(SystemProperties.JDK_SERIAL_FILTER_FACTORY);
        basicKeyCheck(SystemProperties.JDK_TLS_CLIENT_SIGNATURE_SCHEMES);
        basicKeyCheck(SystemProperties.JDK_TLS_NAMED_GROUPS);
        basicKeyCheck(SystemProperties.JDK_TLS_SERVER_SIGNATURE_SCHEMES);
        basicKeyCheck(SystemProperties.JDK_VIRTUAL_THREAD_SCHEDULER_MAXPOOLSIZE);
        basicKeyCheck(SystemProperties.JDK_VIRTUAL_THREAD_SCHEDULER_PARALLELISM);
        basicKeyCheck(SystemProperties.JDK_XML_CDATA_CHUNK_SIZE);
        basicKeyCheck(SystemProperties.JDK_XML_DTD_SUPPORT);
        basicKeyCheck(SystemProperties.JDK_XML_ELEMENT_ATTRIBUTE_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_ENABLE_EXTENSION_FUNCTIONS);
        basicKeyCheck(SystemProperties.JDK_XML_ENTITY_EXPANSION_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_ENTITY_REPLACEMENT_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_IS_STANDALONE);
        basicKeyCheck(SystemProperties.JDK_XML_JDK_CATALOG_RESOLVE);
        basicKeyCheck(SystemProperties.JDK_XML_MAX_ELEMENT_DEPTH);
        basicKeyCheck(SystemProperties.JDK_XML_MAX_GENERAL_ENTITY_SIZE_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_MAX_OCCUR_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_MAX_PARAMETER_ENTITY_SIZE_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_MAX_XML_NAME_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_OVERRIDE_DEFAULT_PARSER);
        basicKeyCheck(SystemProperties.JDK_XML_RESET_SYMBOL_TABLE);
        basicKeyCheck(SystemProperties.JDK_XML_TOTAL_ENTITY_SIZE_LIMIT);
        basicKeyCheck(SystemProperties.JDK_XML_XSLTC_IS_STANDALONE);
        basicKeyCheck(SystemProperties.LINE_SEPARATOR);
        basicKeyCheck(SystemProperties.NATIVE_ENCODING);
        basicKeyCheck(SystemProperties.NETWORK_ADDRESS_CACHE_NEGATIVE_TTL);
        basicKeyCheck(SystemProperties.NETWORK_ADDRESS_CACHE_STALE_TTL);
        basicKeyCheck(SystemProperties.NETWORK_ADDRESS_CACHE_TTL);
        basicKeyCheck(SystemProperties.ORG_JCP_XML_DSIG_SECURE_VALIDATION);
        basicKeyCheck(SystemProperties.ORG_OPENJDK_JAVA_UTIL_STREAM_TRIPWIRE);
        basicKeyCheck(SystemProperties.OS_ARCH);
        basicKeyCheck(SystemProperties.OS_NAME);
        basicKeyCheck(SystemProperties.OS_VERSION);
        basicKeyCheck(SystemProperties.PATH_SEPARATOR);
        basicKeyCheck(SystemProperties.SOCKS_PROXY_HOST);
        basicKeyCheck(SystemProperties.SOCKS_PROXY_PORT);
        basicKeyCheck(SystemProperties.SOCKS_PROXY_VERSION);
        basicKeyCheck(SystemProperties.STDERR_ENCODING);
        basicKeyCheck(SystemProperties.STDOUT_ENCODING);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_DRAIN_AMOUNT);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_IDLE_INTERVAL);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_MAX_IDLE_CONNECTIONS);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_MAX_REQ_HEADERS);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_MAX_REQ_TIME);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_MAX_RSP_TIME);
        basicKeyCheck(SystemProperties.SUN_NET_HTTP_SERVER_NO_DELAY);
        basicKeyCheck(SystemProperties.SUN_SECURITY_KRB5_PRINCIPAL);
        basicKeyCheck(SystemProperties.USER_COUNTRY);
        basicKeyCheck(SystemProperties.USER_DIR);
        basicKeyCheck(SystemProperties.USER_EXTENSIONS);
        basicKeyCheck(SystemProperties.USER_HOME);
        basicKeyCheck(SystemProperties.USER_LANGUAGE);
        basicKeyCheck(SystemProperties.USER_NAME);
        basicKeyCheck(SystemProperties.USER_REGION);
        basicKeyCheck(SystemProperties.USER_SCRIPT);
        basicKeyCheck(SystemProperties.USER_TIMEZONE);
        basicKeyCheck(SystemProperties.USER_VARIANT);
    }

    /**
     * Tests that all names match expected system property names.
     *
     * @see #testEntityReplacementLimitConstantName
     */
    @Test
    void testConstants() {
        assertEquals("apple.awt.enableTemplateImages", SystemProperties.APPLE_AWT_ENABLE_TEMPLATE_IMAGES);
        assertEquals("awt.toolkit", SystemProperties.AWT_TOOLKIT);
        assertEquals("com.sun.jndi.ldap.object.trustSerialData", SystemProperties.COM_SUN_JNDI_LDAP_OBJECT_TRUST_SERIAL_DATA);
        assertEquals("com.sun.net.httpserver.HttpServerProvider", SystemProperties.COM_SUN_NET_HTTP_SERVER_HTTP_SERVER_PROVIDER);
        assertEquals("file.encoding", SystemProperties.FILE_ENCODING);
        assertEquals("file.separator", SystemProperties.FILE_SEPARATOR);
        assertEquals("ftp.nonProxyHosts", SystemProperties.FTP_NON_PROXY_HOST);
        assertEquals("ftp.proxyHost", SystemProperties.FTP_PROXY_HOST);
        assertEquals("ftp.proxyPort", SystemProperties.FTP_PROXY_PORT);
        assertEquals("http.agent", SystemProperties.HTTP_AGENT);
        assertEquals("http.auth.digest.cnonceRepeat", SystemProperties.HTTP_AUTH_DIGEST_CNONCE_REPEAT);
        assertEquals("http.auth.digest.reEnabledAlgorithms", SystemProperties.HTTP_AUTH_DIGEST_RE_ENABLED_ALGORITHMS);
        assertEquals("http.auth.digest.validateProxy", SystemProperties.HTTP_AUTH_DIGEST_VALIDATE_PROXY);
        assertEquals("http.auth.digest.validateServer", SystemProperties.HTTP_AUTH_DIGEST_VALIDATE_SERVER);
        assertEquals("http.auth.ntlm.domain", SystemProperties.HTTP_AUTH_NTLM_DOMAIN);
        assertEquals("http.keepAlive", SystemProperties.HTTP_KEEP_ALIVE);
        assertEquals("http.keepAlive.time.proxy", SystemProperties.HTTP_KEEP_ALIVE_TIME_PROXY);
        assertEquals("http.keepAlive.time.server", SystemProperties.HTTP_KEEP_ALIVE_TIME_SERVER);
        assertEquals("http.maxConnections", SystemProperties.HTTP_MAX_CONNECTIONS);
        assertEquals("http.maxRedirects", SystemProperties.HTTP_MAX_REDIRECTS);
        assertEquals("http.nonProxyHosts", SystemProperties.HTTP_NON_PROXY_HOSTS);
        assertEquals("http.proxyHost", SystemProperties.HTTP_PROXY_HOST);
        assertEquals("http.proxyPort", SystemProperties.HTTP_PROXY_PORT);
        assertEquals("https.proxyHost", SystemProperties.HTTPS_PROXY_HOST);
        assertEquals("https.proxyPort", SystemProperties.HTTPS_PROXY_PORT);
        assertEquals("java.awt.fonts", SystemProperties.JAVA_AWT_FONTS);
        assertEquals("java.awt.graphicsenv", SystemProperties.JAVA_AWT_GRAPHICSENV);
        assertEquals("java.awt.headless", SystemProperties.JAVA_AWT_HEADLESS);
        assertEquals("java.awt.printerjob", SystemProperties.JAVA_AWT_PRINTERJOB);
        assertEquals("java.class.path", SystemProperties.JAVA_CLASS_PATH);
        assertEquals("java.class.version", SystemProperties.JAVA_CLASS_VERSION);
        assertEquals("java.compiler", SystemProperties.JAVA_COMPILER);
        assertEquals("java.content.handler.pkgs", SystemProperties.JAVA_CONTENT_HANDLER_PKGS);
        assertEquals("java.endorsed.dirs", SystemProperties.JAVA_ENDORSED_DIRS);
        assertEquals("java.ext.dirs", SystemProperties.JAVA_EXT_DIRS);
        assertEquals("java.home", SystemProperties.JAVA_HOME);
        assertEquals("java.io.tmpdir", SystemProperties.JAVA_IO_TMPDIR);
        assertEquals("java.library.path", SystemProperties.JAVA_LIBRARY_PATH);
        assertEquals("java.locale.providers", SystemProperties.JAVA_LOCALE_PROVIDERS);
        assertEquals("java.locale.useOldISOCodes", SystemProperties.JAVA_LOCALE_USE_OLD_ISO_CODES);
        assertEquals("java.net.preferIPv4Stack", SystemProperties.JAVA_NET_PREFER_IPV4_STACK);
        assertEquals("java.net.preferIPv6Addresses", SystemProperties.JAVA_NET_PREFER_IPV6_ADDRESSES);
        assertEquals("java.net.socks.password", SystemProperties.JAVA_NET_SOCKS_PASSWORD);
        assertEquals("java.net.socks.username", SystemProperties.JAVA_NET_SOCKS_USER_NAME);
        assertEquals("java.net.useSystemProxies", SystemProperties.JAVA_NET_USE_SYSTEM_PROXIES);
        assertEquals("java.nio.channels.DefaultThreadPool.initialSize", SystemProperties.JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_INITIAL_SIZE);
        assertEquals("java.nio.channels.DefaultThreadPool.threadFactory", SystemProperties.JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_THREAD_FACTORY);
        assertEquals("java.nio.channels.spi.AsynchronousChannelProvider", SystemProperties.JAVA_NIO_CHANNELS_SPI_ASYNCHRONOUS_CHANNEL_PROVIDER);
        assertEquals("java.nio.channels.spi.SelectorProvider", SystemProperties.JAVA_NIO_CHANNELS_SPI_SELECTOR_PROVIDER);
        assertEquals("java.nio.file.spi.DefaultFileSystemProvider", SystemProperties.JAVA_NIO_FILE_SPI_DEFAULT_FILE_SYSTEM_PROVIDER);
        assertEquals("java.properties.date", SystemProperties.JAVA_PROPERTIES_DATE);
        assertEquals("java.protocol.handler.pkgs", SystemProperties.JAVA_PROTOCOL_HANDLER_PKGS);
        assertEquals("java.rmi.server.codebase", SystemProperties.JAVA_RMI_SERVER_CODEBASE);
        assertEquals("java.rmi.server.hostname", SystemProperties.JAVA_RMI_SERVER_HOST_NAME);
        assertEquals("java.rmi.server.randomIDs", SystemProperties.JAVA_RMI_SERVER_RANDOM_IDS);
        assertEquals("java.rmi.server.RMIClassLoaderSpi", SystemProperties.JAVA_RMI_SERVER_RMI_CLASS_LOADER_SPI);
        assertEquals("java.runtime.name", SystemProperties.JAVA_RUNTIME_NAME);
        assertEquals("java.runtime.version", SystemProperties.JAVA_RUNTIME_VERSION);
        assertEquals("java.security.auth.login.config", SystemProperties.JAVA_SECURITY_AUTH_LOGIN_CONFIG);
        assertEquals("java.security.debug", SystemProperties.JAVA_SECURITY_DEBUG);
        assertEquals("java.security.krb5.conf", SystemProperties.JAVA_SECURITY_KERBEROS_CONF);
        assertEquals("java.security.krb5.kdc", SystemProperties.JAVA_SECURITY_KERBEROS_KDC);
        assertEquals("java.security.krb5.realm", SystemProperties.JAVA_SECURITY_KERBEROS_REALM);
        assertEquals("java.security.manager", SystemProperties.JAVA_SECURITY_MANAGER);
        assertEquals("java.specification.maintenance.version", SystemProperties.JAVA_SPECIFICATION_MAINTENANCE_VERSION);
        assertEquals("java.specification.name", SystemProperties.JAVA_SPECIFICATION_NAME);
        assertEquals("java.specification.vendor", SystemProperties.JAVA_SPECIFICATION_VENDOR);
        assertEquals("java.specification.version", SystemProperties.JAVA_SPECIFICATION_VERSION);
        assertEquals("java.system.class.loader", SystemProperties.JAVA_SYSTEM_CLASS_LOADER);
        assertEquals("java.time.zone.DefaultZoneRulesProvider", SystemProperties.JAVA_TIME_ZONE_DEFAULT_ZONE_RULES_PROVIDER);
        assertEquals("java.util.concurrent.ForkJoinPool.common.exceptionHandler", SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_EXCEPTION_HANDLER);
        assertEquals("java.util.concurrent.ForkJoinPool.common.maximumSpares", SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_MAXIMUM_SPARES);
        assertEquals("java.util.concurrent.ForkJoinPool.common.parallelism", SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_PARALLELISM);
        assertEquals("java.util.concurrent.ForkJoinPool.common.threadFactory", SystemProperties.JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_THREAD_FACTORY);
        assertEquals("java.util.currency.data", SystemProperties.JAVA_UTIL_CURRENCY_DATA);
        assertEquals("java.util.logging.config.class", SystemProperties.JAVA_UTIL_LOGGING_CONFIG_CLASS);
        assertEquals("java.util.logging.config.file", SystemProperties.JAVA_UTIL_LOGGING_CONFIG_FILE);
        assertEquals("java.util.logging.simpleformatter.format", SystemProperties.JAVA_UTIL_LOGGING_SIMPLE_FORMATTER_FORMAT);
        assertEquals("java.util.prefs.PreferencesFactory", SystemProperties.JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
        assertEquals("java.util.PropertyResourceBundle.encoding", SystemProperties.JAVA_UTIL_PROPERTY_RESOURCE_BUNDLE_ENCODING);
        assertEquals("java.vendor", SystemProperties.JAVA_VENDOR);
        assertEquals("java.vendor.url", SystemProperties.JAVA_VENDOR_URL);
        assertEquals("java.vendor.version", SystemProperties.JAVA_VENDOR_VERSION);
        assertEquals("java.version", SystemProperties.JAVA_VERSION);
        assertEquals("java.version.date", SystemProperties.JAVA_VERSION_DATE);
        assertEquals("java.vm.info", SystemProperties.JAVA_VM_INFO);
        assertEquals("java.vm.name", SystemProperties.JAVA_VM_NAME);
        assertEquals("java.vm.specification.name", SystemProperties.JAVA_VM_SPECIFICATION_NAME);
        assertEquals("java.vm.specification.vendor", SystemProperties.JAVA_VM_SPECIFICATION_VENDOR);
        assertEquals("java.vm.specification.version", SystemProperties.JAVA_VM_SPECIFICATION_VERSION);
        assertEquals("java.vm.vendor", SystemProperties.JAVA_VM_VENDOR);
        assertEquals("java.vm.version", SystemProperties.JAVA_VM_VERSION);
        assertEquals("java.xml.config.file", SystemProperties.JAVA_XML_CONFIG_FILE);
        assertEquals("javax.accessibility.assistive_technologies", SystemProperties.JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES);
        assertEquals("javax.net.ssl.sessionCacheSize", SystemProperties.JAVAX_NET_SSL_SESSION_CACHE_SIZE);
        assertEquals("javax.rmi.ssl.client.enabledCipherSuites", SystemProperties.JAVAX_RMI_SSL_CLIENT_ENABLED_CIPHER_SUITES);
        assertEquals("javax.rmi.ssl.client.enabledProtocols", SystemProperties.JAVAX_RMI_SSL_CLIENT_ENABLED_PROTOCOLS);
        assertEquals("javax.security.auth.useSubjectCredsOnly", SystemProperties.JAVAX_SECURITY_AUTH_USE_SUBJECT_CREDS_ONLY);
        assertEquals("javax.smartcardio.TerminalFactory.DefaultType", SystemProperties.JAVAX_SMART_CARD_IO_TERMINAL_FACTORY_DEFAULT_TYPE);
        assertEquals("jdbc.drivers", SystemProperties.JDBC_DRIVERS);
        assertEquals("jdk.http.auth.proxying.disabledSchemes", SystemProperties.JDK_HTTP_AUTH_PROXYING_DISABLED_SCHEMES);
        assertEquals("jdk.http.auth.tunneling.disabledSchemes", SystemProperties.JDK_HTTP_AUTH_TUNNELING_DISABLED_SCHEMES);
        assertEquals("jdk.httpclient.allowRestrictedHeaders", SystemProperties.JDK_HTTP_CLIENT_ALLOW_RESTRICTED_HEADERS);
        assertEquals("jdk.httpclient.auth.retrylimit", SystemProperties.JDK_HTTP_CLIENT_AUTH_RETRY_LIMIT);
        assertEquals("jdk.httpclient.bufsize", SystemProperties.JDK_HTTP_CLIENT_BUF_SIZE);
        assertEquals("jdk.httpclient.connectionPoolSize", SystemProperties.JDK_HTTP_CLIENT_CONNECTION_POOL_SIZE);
        assertEquals("jdk.httpclient.connectionWindowSize", SystemProperties.JDK_HTTP_CLIENT_CONNECTION_WINDOW_SIZE);
        assertEquals("jdk.httpclient.disableRetryConnect", SystemProperties.JDK_HTTP_CLIENT_DISABLE_RETRY_CONNECT);
        assertEquals("jdk.httpclient.enableAllMethodRetry", SystemProperties.JDK_HTTP_CLIENT_ENABLE_ALL_METHOD_RETRY);
        assertEquals("jdk.httpclient.enablepush", SystemProperties.JDK_HTTP_CLIENT_ENABLE_PUSH);
        assertEquals("jdk.httpclient.hpack.maxheadertablesize", SystemProperties.JDK_HTTP_CLIENT_HPACK_MAX_HEADER_TABLE_SIZE);
        assertEquals("jdk.httpclient.HttpClient.log", SystemProperties.JDK_HTTP_CLIENT_HTTP_CLIENT_LOG);
        assertEquals("jdk.httpclient.keepalive.timeout", SystemProperties.JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT);
        assertEquals("jdk.httpclient.keepalive.timeout.h2", SystemProperties.JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT_H2);
        assertEquals("jdk.httpclient.maxframesize", SystemProperties.JDK_HTTP_CLIENT_MAX_FRAME_SIZE);
        assertEquals("jdk.httpclient.maxstreams", SystemProperties.JDK_HTTP_CLIENT_MAX_STREAMS);
        assertEquals("jdk.httpclient.receiveBufferSize", SystemProperties.JDK_HTTP_CLIENT_RECEIVE_BUFFER_SIZE);
        assertEquals("jdk.httpclient.redirects.retrylimit", SystemProperties.JDK_HTTP_CLIENT_REDIRECTS_RETRY_LIMIT);
        assertEquals("jdk.httpclient.sendBufferSize", SystemProperties.JDK_HTTP_CLIENT_SEND_BUFFER_SIZE);
        assertEquals("jdk.httpclient.websocket.writeBufferSize", SystemProperties.JDK_HTTP_CLIENT_WEB_SOCKET_WRITE_BUFFER_SIZE);
        assertEquals("jdk.httpclient.windowsize", SystemProperties.JDK_HTTP_CLIENT_WINDOW_SIZE);
        assertEquals("jdk.httpserver.maxConnections", SystemProperties.JDK_HTTP_SERVER_MAX_CONNECTIONS);
        assertEquals("jdk.https.negotiate.cbt", SystemProperties.JDK_HTTPS_NEGOTIATE_CBT);
        assertEquals("jdk.includeInExceptions", SystemProperties.JDK_INCLUDE_IN_EXCEPTIONS);
        assertEquals("jdk.internal.httpclient.disableHostnameVerification", SystemProperties.JDK_INTERNAL_HTTP_CLIENT_DISABLE_HOST_NAME_VERIFICATION);
        assertEquals("jdk.io.permissionsUseCanonicalPath", SystemProperties.JDK_IO_PERMISSIONS_USE_CANONICAL_PATH);
        assertEquals("jdk.jndi.ldap.object.factoriesFilter", SystemProperties.JDK_JNDI_LDAP_OBJECT_FACTORIES_FILTER);
        assertEquals("jdk.jndi.object.factoriesFilter", SystemProperties.JDK_JNDI_OBJECT_FACTORIES_FILTER);
        assertEquals("jdk.jndi.rmi.object.factoriesFilter", SystemProperties.JDK_JNDI_RMI_OBJECT_FACTORIES_FILTER);
        assertEquals("jdk.module.main", SystemProperties.JDK_MODULE_MAIN);
        assertEquals("jdk.module.main.class", SystemProperties.JDK_MODULE_MAIN_CLASS);
        assertEquals("jdk.module.path", SystemProperties.JDK_MODULE_PATH);
        assertEquals("jdk.module.upgrade.path", SystemProperties.JDK_MODULE_UPGRADE_PATH);
        assertEquals("jdk.net.unixdomain.tmpdir", SystemProperties.JDK_NET_UNIX_DOMAIN_TMPDIR);
        assertEquals("jdk.net.URLClassPath.showIgnoredClassPathEntries", SystemProperties.JDK_NET_URL_CLASS_PATH_SHOW_IGNORED_CLASS_PATH_ENTRIES);
        assertEquals("jdk.serialFilter", SystemProperties.JDK_SERIAL_FILTER);
        assertEquals("jdk.serialFilterFactory", SystemProperties.JDK_SERIAL_FILTER_FACTORY);
        assertEquals("jdk.tls.client.SignatureSchemes", SystemProperties.JDK_TLS_CLIENT_SIGNATURE_SCHEMES);
        assertEquals("jdk.tls.namedGroups", SystemProperties.JDK_TLS_NAMED_GROUPS);
        assertEquals("jdk.tls.server.SignatureSchemes", SystemProperties.JDK_TLS_SERVER_SIGNATURE_SCHEMES);
        assertEquals("jdk.virtualThreadScheduler.maxPoolSize", SystemProperties.JDK_VIRTUAL_THREAD_SCHEDULER_MAXPOOLSIZE);
        assertEquals("jdk.virtualThreadScheduler.parallelism", SystemProperties.JDK_VIRTUAL_THREAD_SCHEDULER_PARALLELISM);
        assertEquals("jdk.xml.cdataChunkSize", SystemProperties.JDK_XML_CDATA_CHUNK_SIZE);
        assertEquals("jdk.xml.dtd.support", SystemProperties.JDK_XML_DTD_SUPPORT);
        assertEquals("jdk.xml.elementAttributeLimit", SystemProperties.JDK_XML_ELEMENT_ATTRIBUTE_LIMIT);
        assertEquals("jdk.xml.enableExtensionFunctions", SystemProperties.JDK_XML_ENABLE_EXTENSION_FUNCTIONS);
        assertEquals("jdk.xml.entityExpansionLimit", SystemProperties.JDK_XML_ENTITY_EXPANSION_LIMIT);
        assertEquals("jdk.xml.entityReplacementLimit", SystemProperties.JDK_XML_ENTITY_REPLACEMENT_LIMIT);
        assertEquals("jdk.xml.isStandalone", SystemProperties.JDK_XML_IS_STANDALONE);
        assertEquals("jdk.xml.jdkcatalog.resolve", SystemProperties.JDK_XML_JDK_CATALOG_RESOLVE);
        assertEquals("jdk.xml.maxElementDepth", SystemProperties.JDK_XML_MAX_ELEMENT_DEPTH);
        assertEquals("jdk.xml.maxGeneralEntitySizeLimit", SystemProperties.JDK_XML_MAX_GENERAL_ENTITY_SIZE_LIMIT);
        assertEquals("jdk.xml.maxOccurLimit", SystemProperties.JDK_XML_MAX_OCCUR_LIMIT);
        assertEquals("jdk.xml.maxParameterEntitySizeLimit", SystemProperties.JDK_XML_MAX_PARAMETER_ENTITY_SIZE_LIMIT);
        assertEquals("jdk.xml.maxXMLNameLimit", SystemProperties.JDK_XML_MAX_XML_NAME_LIMIT);
        assertEquals("jdk.xml.overrideDefaultParser", SystemProperties.JDK_XML_OVERRIDE_DEFAULT_PARSER);
        assertEquals("jdk.xml.resetSymbolTable", SystemProperties.JDK_XML_RESET_SYMBOL_TABLE);
        assertEquals("jdk.xml.totalEntitySizeLimit", SystemProperties.JDK_XML_TOTAL_ENTITY_SIZE_LIMIT);
        assertEquals("jdk.xml.xsltcIsStandalone", SystemProperties.JDK_XML_XSLTC_IS_STANDALONE);
        assertEquals("line.separator", SystemProperties.LINE_SEPARATOR);
        assertEquals("native.encoding", SystemProperties.NATIVE_ENCODING);
        assertEquals("networkaddress.cache.negative.ttl", SystemProperties.NETWORK_ADDRESS_CACHE_NEGATIVE_TTL);
        assertEquals("networkaddress.cache.stale.ttl", SystemProperties.NETWORK_ADDRESS_CACHE_STALE_TTL);
        assertEquals("networkaddress.cache.ttl", SystemProperties.NETWORK_ADDRESS_CACHE_TTL);
        assertEquals("org.jcp.xml.dsig.securevalidation", SystemProperties.ORG_JCP_XML_DSIG_SECURE_VALIDATION);
        assertEquals("org.openjdk.java.util.stream.tripwire", SystemProperties.ORG_OPENJDK_JAVA_UTIL_STREAM_TRIPWIRE);
        assertEquals("os.arch", SystemProperties.OS_ARCH);
        assertEquals("os.name", SystemProperties.OS_NAME);
        assertEquals("os.version", SystemProperties.OS_VERSION);
        assertEquals("path.separator", SystemProperties.PATH_SEPARATOR);
        assertEquals("socksProxyHost", SystemProperties.SOCKS_PROXY_HOST);
        assertEquals("socksProxyPort", SystemProperties.SOCKS_PROXY_PORT);
        assertEquals("socksProxyVersion", SystemProperties.SOCKS_PROXY_VERSION);
        assertEquals("stderr.encoding", SystemProperties.STDERR_ENCODING);
        assertEquals("stdout.encoding", SystemProperties.STDOUT_ENCODING);
        assertEquals("sun.net.httpserver.drainAmount", SystemProperties.SUN_NET_HTTP_SERVER_DRAIN_AMOUNT);
        assertEquals("sun.net.httpserver.idleInterval", SystemProperties.SUN_NET_HTTP_SERVER_IDLE_INTERVAL);
        assertEquals("sun.net.httpserver.maxIdleConnections", SystemProperties.SUN_NET_HTTP_SERVER_MAX_IDLE_CONNECTIONS);
        assertEquals("sun.net.httpserver.maxReqHeaders", SystemProperties.SUN_NET_HTTP_SERVER_MAX_REQ_HEADERS);
        assertEquals("sun.net.httpserver.maxReqTime", SystemProperties.SUN_NET_HTTP_SERVER_MAX_REQ_TIME);
        assertEquals("sun.net.httpserver.maxRspTime", SystemProperties.SUN_NET_HTTP_SERVER_MAX_RSP_TIME);
        assertEquals("sun.net.httpserver.nodelay", SystemProperties.SUN_NET_HTTP_SERVER_NO_DELAY);
        assertEquals("sun.security.krb5.principal", SystemProperties.SUN_SECURITY_KRB5_PRINCIPAL);
        assertEquals("user.country", SystemProperties.USER_COUNTRY);
        assertEquals("user.dir", SystemProperties.USER_DIR);
        assertEquals("user.extensions", SystemProperties.USER_EXTENSIONS);
        assertEquals("user.home", SystemProperties.USER_HOME);
        assertEquals("user.language", SystemProperties.USER_LANGUAGE);
        assertEquals("user.name", SystemProperties.USER_NAME);
        assertEquals("user.region", SystemProperties.USER_REGION);
        assertEquals("user.script", SystemProperties.USER_SCRIPT);
        assertEquals("user.timezone", SystemProperties.USER_TIMEZONE);
        assertEquals("user.variant", SystemProperties.USER_VARIANT);
    }

    /**
     * Tests that {@link SystemProperties#JDK_XML_ENTITY_REPLACEMENT_LIMIT} is correct since it suffered from a typo when originally coded.
     */
    @Test
    @SetSystemProperty(key = "jdk.xml.entityReplacementLimit", value = "1234")
    void testEntityReplacementLimitConstantName() {
        assertEquals("jdk.xml.entityReplacementLimit", SystemProperties.JDK_XML_ENTITY_REPLACEMENT_LIMIT);
        assertEquals("1234", SystemProperties.getJdkXmlEntityReplacementLimit());
    }

    @Test
    void testGetAwtToolkit() {
        assertDoesNotThrow(SystemProperties::getAwtToolkit);
    }

    @Test
    void testGetBoolean() {
        final String key = RandomStringUtils.insecure().next(10);
        final String absentKey = RandomStringUtils.insecure().next(10);
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
    void testGetBooleanClass() {
        final String key = RandomStringUtils.insecure().next(10);
        final String absentKey = RandomStringUtils.insecure().next(10);
        final String keyFull = SIMPLE_NAME + "." + key;
        final String absentKeyFull = SIMPLE_NAME + "." + absentKey;
        assertNull(System.getProperty(absentKeyFull));
        try {
            System.setProperty(keyFull, Boolean.TRUE.toString());
            assertTrue(SystemProperties.getBoolean(SystemPropertiesTest.class, key, () -> false));
            assertTrue(SystemProperties.getBoolean(SystemPropertiesTest.class, absentKey, () -> true));
            assertFalse(SystemProperties.getBoolean(SystemPropertiesTest.class, absentKey, () -> false));
            assertTrue(SystemProperties.getBoolean(SystemPropertiesTest.class, absentKey, () -> true));
        } finally {
            System.clearProperty(keyFull);
        }
    }

    @Test
    void testGetDoesNotThrow() {
        assertDoesNotThrow(SystemProperties::getAppleAwtEnableTemplateImages);
        assertDoesNotThrow(SystemProperties::getAwtToolkit);
        assertDoesNotThrow(SystemProperties::getComSunJndiLdapObjectTrustSerialData);
        assertDoesNotThrow(SystemProperties::getComSunNetHttpServerHttpServerProvider);
        assertDoesNotThrow(SystemProperties::getFileEncoding);
        assertDoesNotThrow(SystemProperties::getFileSeparator);
        assertDoesNotThrow(SystemProperties::getFtpNonProxyHost);
        assertDoesNotThrow(SystemProperties::getFtpProxyHost);
        assertDoesNotThrow(SystemProperties::getFtpProxyPort);
        assertDoesNotThrow(SystemProperties::getHttpAgent);
        assertDoesNotThrow(SystemProperties::getHttpAuthDigestCnonceRepeat);
        assertDoesNotThrow(SystemProperties::getHttpAuthDigestReenabledAlgorithms);
        assertDoesNotThrow(SystemProperties::getHttpAuthDigestValidateProxy);
        assertDoesNotThrow(SystemProperties::getHttpAuthDigestValidateServer);
        assertDoesNotThrow(SystemProperties::getHttpAuthNtlmDomain);
        assertDoesNotThrow(SystemProperties::getHttpKeepAlive);
        assertDoesNotThrow(SystemProperties::getHttpKeepAliveTimeProxy);
        assertDoesNotThrow(SystemProperties::getHttpKeepAliveTimeServer);
        assertDoesNotThrow(SystemProperties::getHttpMaxConnections);
        assertDoesNotThrow(SystemProperties::getHttpMaxRedirects);
        assertDoesNotThrow(SystemProperties::getHttpNonProxyHosts);
        assertDoesNotThrow(SystemProperties::getHttpProxyHost);
        assertDoesNotThrow(SystemProperties::getHttpProxyPort);
        assertDoesNotThrow(SystemProperties::getHttpsProxyHost);
        assertDoesNotThrow(SystemProperties::getHttpsProxyPort);
        assertDoesNotThrow(SystemProperties::getJavaAwtFonts);
        assertDoesNotThrow(SystemProperties::getJavaAwtGraphicsenv);
        assertDoesNotThrow(SystemProperties::getJavaAwtHeadless);
        assertDoesNotThrow(SystemProperties::getJavaAwtPrinterjob);
        assertDoesNotThrow(SystemProperties::getJavaClassPath);
        assertDoesNotThrow(SystemProperties::getJavaClassVersion);
        assertDoesNotThrow(SystemProperties::getJavaCompiler);
        assertDoesNotThrow(SystemProperties::getJavaContentHandlerPkgs);
        assertDoesNotThrow(SystemProperties::getJavaEndorsedDirs);
        assertDoesNotThrow(SystemProperties::getJavaExtDirs);
        assertDoesNotThrow(SystemProperties::getJavaHome);
        assertDoesNotThrow(SystemProperties::getJavaIoTmpdir);
        assertDoesNotThrow(SystemProperties::getJavaLibraryPath);
        assertDoesNotThrow(SystemProperties::getJavaLocaleProviders);
        assertDoesNotThrow(SystemProperties::getJavaLocaleUseOldIsoCodes);
        assertDoesNotThrow(SystemProperties::getJavaNetPreferIpv4Stack);
        assertDoesNotThrow(SystemProperties::getJavaNetPreferIpv6Addresses);
        assertDoesNotThrow(SystemProperties::getJavaNetSocksPassword);
        assertDoesNotThrow(SystemProperties::getJavaNetSocksUserName);
        assertDoesNotThrow(SystemProperties::getJavaNetUseSystemProxies);
        assertDoesNotThrow(SystemProperties::getJavaNioChannelsDefaultThreadPoolInitialSize);
        assertDoesNotThrow(SystemProperties::getJavaNioChannelsDefaultThreadPoolThreadFactory);
        assertDoesNotThrow(SystemProperties::getJavaNioChannelsSpiAsynchronousChannelProvider);
        assertDoesNotThrow(SystemProperties::getJavaNioChannelsSpiSelectorProvider);
        assertDoesNotThrow(SystemProperties::getJavaNioFileSpiDefaultFileSystemProvider);
        assertDoesNotThrow(SystemProperties::getJavaPropertiesDate);
        assertDoesNotThrow(SystemProperties::getJavaProtocolHandlerPkgs);
        assertDoesNotThrow(SystemProperties::getJavaRmiServerCodebase);
        assertDoesNotThrow(SystemProperties::getJavaRmiServerHostName);
        assertDoesNotThrow(SystemProperties::getJavaRmiServerRandomIds);
        assertDoesNotThrow(SystemProperties::getJavaRmiServerRmiClassLoaderSpi);
        assertDoesNotThrow(SystemProperties::getJavaRuntimeName);
        assertDoesNotThrow(SystemProperties::getJavaRuntimeVersion);
        assertDoesNotThrow(SystemProperties::getJavaSecurityAuthLoginConfig);
        assertDoesNotThrow(SystemProperties::getJavaSecurityManager);
        assertDoesNotThrow(SystemProperties::getJavaSpecificationMaintenanceVersion);
        assertDoesNotThrow(SystemProperties::getJavaSpecificationName);
        assertDoesNotThrow(SystemProperties::getJavaSpecificationVendor);
        assertDoesNotThrow((ThrowingSupplier<String>) SystemProperties::getJavaSpecificationVersion);
        assertDoesNotThrow(SystemProperties::getJavaSystemClassLoader);
        assertDoesNotThrow(SystemProperties::getJavaTimeZoneDefaultZoneRulesProvider);
        assertDoesNotThrow(SystemProperties::getJavaUtilConcurrentForkJoinPoolCommonExceptionHandler);
        assertDoesNotThrow(SystemProperties::getJavaUtilConcurrentForkJoinPoolCommonMaximumSpares);
        assertDoesNotThrow(SystemProperties::getJavaUtilConcurrentForkJoinPoolCommonParallelism);
        assertDoesNotThrow(SystemProperties::getJavaUtilConcurrentForkJoinPoolCommonThreadFactory);
        assertDoesNotThrow(SystemProperties::getJavaUtilCurrencyData);
        assertDoesNotThrow(SystemProperties::getJavaUtilLoggingConfigClass);
        assertDoesNotThrow(SystemProperties::getJavaUtilLoggingConfigFile);
        assertDoesNotThrow(SystemProperties::getJavaUtilLoggingSimpleFormatterFormat);
        assertDoesNotThrow(SystemProperties::getJavaUtilPrefsPreferencesFactory);
        assertDoesNotThrow(SystemProperties::getJavaUtilPropertyResourceBundleEncoding);
        assertDoesNotThrow(SystemProperties::getJavaVendor);
        assertDoesNotThrow(SystemProperties::getJavaVendorUrl);
        assertDoesNotThrow(SystemProperties::getJavaVendorVersion);
        assertDoesNotThrow(SystemProperties::getJavaVersion);
        assertDoesNotThrow(SystemProperties::getJavaVersionDate);
        assertDoesNotThrow(SystemProperties::getJavaVmInfo);
        assertDoesNotThrow(SystemProperties::getJavaVmName);
        assertDoesNotThrow(SystemProperties::getJavaVmSpecificationName);
        assertDoesNotThrow(SystemProperties::getJavaVmSpecificationVendor);
        assertDoesNotThrow(SystemProperties::getJavaVmSpecificationVersion);
        assertDoesNotThrow(SystemProperties::getJavaVmVendor);
        assertDoesNotThrow(SystemProperties::getJavaVmVersion);
        assertDoesNotThrow(SystemProperties::getJavaxAccessibilityAssistiveTechnologies);
        assertDoesNotThrow(SystemProperties::getJavaXmlConfigFile);
        assertDoesNotThrow(SystemProperties::getJavaxNetSslSessionCacheSize);
        assertDoesNotThrow(SystemProperties::getJavaxRmiSslClientEnabledCipherSuites);
        assertDoesNotThrow(SystemProperties::getJavaxRmiSslClientEnabledProtocols);
        assertDoesNotThrow(SystemProperties::getJavaxSecurityAuthUseSubjectCredsOnly);
        assertDoesNotThrow(SystemProperties::getJavaxSmartCardIoTerminalFactoryDefaultType);
        assertDoesNotThrow(SystemProperties::getJdbcDrivers);
        assertDoesNotThrow(SystemProperties::getJdkHttpAuthProxyingDisabledSchemes);
        assertDoesNotThrow(SystemProperties::getJdkHttpAuthTunnelingDisabledSchemes);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientAllowRestrictedHeaders);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientAuthRetryLimit);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientBufSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientConnectionPoolSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientConnectionWindowSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientDisableRetryConnect);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientEnableAllMethodRetry);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientEnablePush);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientHpackMaxHeaderTableSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientHttpClientLog);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientKeepAliveTimeout);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientKeepAliveTimeoutH2);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientMaxFrameSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientMaxStreams);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientReceiveBufferSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientRedirectsRetryLimit);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientSendBufferSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientWebSocketWriteBufferSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpClientWindowSize);
        assertDoesNotThrow(SystemProperties::getJdkHttpServerMaxConnections);
        assertDoesNotThrow(SystemProperties::getJdkHttpsNegotiateCbt);
        assertDoesNotThrow(SystemProperties::getJdkIncludeInExceptions);
        assertDoesNotThrow(SystemProperties::getJdkInternalHttpClientDisableHostNameVerification);
        assertDoesNotThrow(SystemProperties::getJdkIoPermissionsUseCanonicalPath);
        assertDoesNotThrow(SystemProperties::getJdkJndiLdapObjectFactoriesFilter);
        assertDoesNotThrow(SystemProperties::getJdkJndiObjectFactoriesFilter);
        assertDoesNotThrow(SystemProperties::getJdkJndiRmiObjectFactoriesFilter);
        assertDoesNotThrow(SystemProperties::getJdkModuleMain);
        assertDoesNotThrow(SystemProperties::getJdkModuleMainClass);
        assertDoesNotThrow(SystemProperties::getJdkModulePath);
        assertDoesNotThrow(SystemProperties::getJdkModuleUpgradePath);
        assertDoesNotThrow(SystemProperties::getJdkNetUnixDomainTmpDir);
        assertDoesNotThrow(SystemProperties::getJdkNetUrlClassPathShowIgnoredClassPathEntries);
        assertDoesNotThrow(SystemProperties::getJdkSerialFilter);
        assertDoesNotThrow(SystemProperties::getJdkSerialFilterFactory);
        assertDoesNotThrow(SystemProperties::getJdkTlsClientSignatureSchemes);
        assertDoesNotThrow(SystemProperties::getJdkTlsNamedGroups);
        assertDoesNotThrow(SystemProperties::getJdkTlsServerSignatureSchemes);
        assertDoesNotThrow(SystemProperties::getJdkVirtualThreadSchedulerMaxPoolSize);
        assertDoesNotThrow(SystemProperties::getJdkVirtualThreadSchedulerParallelism);
        assertDoesNotThrow(SystemProperties::getJdkXmlCdataChunkSize);
        assertDoesNotThrow(SystemProperties::getJdkXmlDtdSupport);
        assertDoesNotThrow(SystemProperties::getJdkXmlElementAttributeLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlEnableExtensionFunctions);
        assertDoesNotThrow(SystemProperties::getJdkXmlEntityExpansionLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlEntityReplacementLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlIsStandalone);
        assertDoesNotThrow(SystemProperties::getJdkXmlJdkCatalogResolve);
        assertDoesNotThrow(SystemProperties::getJdkXmlMaxElementDepth);
        assertDoesNotThrow(SystemProperties::getJdkXmlMaxGeneralEntitySizeLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlMaxOccurLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlMaxParameterEntitySizeLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlMaxXmlNameLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlOverrideDefaultParser);
        assertDoesNotThrow(SystemProperties::getJdkXmlResetSymbolTable);
        assertDoesNotThrow(SystemProperties::getJdkXmlTotalEntitySizeLimit);
        assertDoesNotThrow(SystemProperties::getJdkXmlXsltcIsStandalone);
        assertDoesNotThrow((ThrowingSupplier<String>) SystemProperties::getLineSeparator);
        assertDoesNotThrow(SystemProperties::getNativeEncoding);
        assertDoesNotThrow(SystemProperties::getNetworkAddressCacheNegativeTtl);
        assertDoesNotThrow(SystemProperties::getNetworkAddressCacheStaleTtl);
        assertDoesNotThrow(SystemProperties::getNetworkAddressCacheTtl);
        assertDoesNotThrow(SystemProperties::getOrgJcpXmlDsigSecureValidation);
        assertDoesNotThrow(SystemProperties::getOrgOpenJdkJavaUtilStreamTripwire);
        assertDoesNotThrow(SystemProperties::getOsArch);
        assertDoesNotThrow(SystemProperties::getOsName);
        assertDoesNotThrow(SystemProperties::getOsVersion);
        assertDoesNotThrow(SystemProperties::getPathSeparator);
        assertDoesNotThrow(SystemProperties::getSocksProxyHost);
        assertDoesNotThrow(SystemProperties::getSocksProxyPort);
        assertDoesNotThrow(SystemProperties::getSocksProxyVersion);
        assertDoesNotThrow(SystemProperties::getStdErrEncoding);
        assertDoesNotThrow(SystemProperties::getStdOutEncoding);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerDrainAmount);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerIdleInterval);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerMaxIdleConnections);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerMaxReqHeaders);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerMaxReqTime);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerMaxRspTime);
        assertDoesNotThrow(SystemProperties::getSunNetHttpServerNoDelay);
        assertDoesNotThrow(SystemProperties::getSunSecurityKrb5Principal);
        assertDoesNotThrow(SystemProperties::getUserCountry);
        assertDoesNotThrow(SystemProperties::getUserDir);
        assertDoesNotThrow(SystemProperties::getUserExtensions);
        assertDoesNotThrow(SystemProperties::getUserHome);
        assertDoesNotThrow(SystemProperties::getUserLanguage);
        assertDoesNotThrow((ThrowingSupplier<String>) SystemProperties::getUserName);
        assertDoesNotThrow(SystemProperties::getUserRegion);
        assertDoesNotThrow(SystemProperties::getUserScript);
        assertDoesNotThrow(SystemProperties::getUserTimezone);
        assertDoesNotThrow(SystemProperties::getUserVariant);
    }

    @Test
    void testGetFileEncoding() {
        basicKeyCheck(SystemProperties.getFileEncoding());
    }

    @Test
    void testGetFileSeparator() {
        assertNotNull(SystemProperties.getFileSeparator());
    }

    @Test
    void testGetInt() {
        final String key = RandomStringUtils.insecure().next(10);
        final String absentKey = RandomStringUtils.insecure().next(10);
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
    void testGetIntClass() {
        final String key = RandomStringUtils.insecure().next(10);
        final String absentKey = RandomStringUtils.insecure().next(10);
        final String keyFull = SIMPLE_NAME + "." + key;
        final String absentKeyFull = SIMPLE_NAME + "." + absentKey;
        assertNull(System.getProperty(absentKeyFull));
        try {
            System.setProperty(keyFull, Long.toString(Integer.MAX_VALUE));
            assertEquals(Integer.MAX_VALUE, SystemProperties.getInt(SystemPropertiesTest.class, key, () -> 0));
            assertEquals(Integer.MAX_VALUE, SystemProperties.getInt(SystemPropertiesTest.class, absentKey, () -> Integer.MAX_VALUE));
            assertEquals(0, SystemProperties.getInt(SystemPropertiesTest.class, absentKey, () -> 0));
            assertEquals(1, SystemProperties.getInt(SystemPropertiesTest.class, absentKey, () -> 1));
        } finally {
            System.clearProperty(keyFull);
        }
    }

    @Test
    void testGetJavaAwtFonts() {
        assertNull(SystemProperties.getJavaAwtFonts());
    }

    @Test
    void testGetJavaAwtGraphicsenv() {
        assertDoesNotThrow(SystemProperties::getJavaAwtGraphicsenv);
    }

    @Test
    void testGetJavaAwtHeadless() {
        assertNull(SystemProperties.getJavaAwtHeadless());
    }

    @Test
    void testGetJavaAwtPrinterjob() {
        assertDoesNotThrow(SystemProperties::getJavaAwtPrinterjob);
    }

    @Test
    void testGetJavaClassPath() {
        assertNotNull(SystemProperties.getJavaClassPath());
    }

    @Test
    void testGetJavaClassVersion() {
        assertNotNull(SystemProperties.getJavaClassVersion());
    }

    @Test
    void testGetJavaCompiler() {
        if (SystemUtils.IS_JAVA_14) {
            // Not in Java 11
            assertNotNull(SystemProperties.getJavaCompiler());
        }
    }

    @Test
    void testGetJavaEndorsedDirs() {
        if (isJava11OrGreater()) {
            // Not in Java 11
            assertNull(SystemProperties.getJavaEndorsedDirs());
        } else {
            assertNotNull(SystemProperties.getJavaExtDirs());
        }
    }

    @Test
    void testGetJavaExtDirs() {
        if (isJava11OrGreater()) {
            // Not in Java 11
            assertNull(SystemProperties.getJavaExtDirs());
        } else {
            assertNotNull(SystemProperties.getJavaExtDirs());
        }
    }

    @Test
    void testGetJavaHome() {
        assertNotNull(SystemProperties.getJavaHome());
    }

    @Test
    void testGetJavaIoTmpdir() {
        assertNotNull(SystemProperties.getJavaIoTmpdir());
    }

    @Test
    void testGetJavaLibraryPath() {
        assertNotNull(SystemProperties.getJavaLibraryPath());
    }

    @Test
    void testGetJavaLocaleProviders() {
        assumeTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
        // default is null
        assertNull(SystemProperties.getJavaLocaleProviders(), SystemProperties.getJavaVersion());
    }

    @Test
    void testGetJavaRuntimeName() {
        assertNotNull(SystemProperties.getJavaRuntimeName());
    }

    @Test
    void testGetJavaRuntimeVersion() {
        assertNotNull(SystemProperties.getJavaRuntimeVersion());
    }

    @Test
    void testGetJavaSpecificationName() {
        assertNotNull(SystemProperties.getJavaSpecificationName());
    }

    @Test
    void testGetJavaSpecificationVendor() {
        assertNotNull(SystemProperties.getJavaSpecificationVendor());
    }

    @Test
    void testGetJavaSpecificationVersion() {
        assertNotNull(SystemProperties.getJavaSpecificationVersion());
    }

    @Test
    void testGetJavaSpecificationVersionSupplier() {
        assertNotNull(SystemProperties.getJavaSpecificationVersion("99.0"));
    }

    @Test
    void testGetJavaUtilPrefsPreferencesFactory() {
        assertNull(SystemProperties.getJavaUtilPrefsPreferencesFactory());
    }

    @Test
    void testGetJavaVendor() {
        assertNotNull(SystemProperties.getJavaVendor());
    }

    @Test
    void testGetJavaVendorUrl() {
        assertNotNull(SystemProperties.getJavaVendorUrl());
    }

    @Test
    void testGetJavaVersion() {
        assertNotNull(SystemProperties.getJavaVersion());
    }

    @Test
    void testGetJavaVmInfo() {
        assertNotNull(SystemProperties.getJavaVmInfo());
    }

    @Test
    void testGetJavaVmName() {
        assertNotNull(SystemProperties.getJavaVmName());
    }

    @Test
    void testGetJavaVmSpecificationName() {
        assertNotNull(SystemProperties.getJavaVmSpecificationName());
    }

    @Test
    void testGetJavaVmSpecificationVendor() {
        assertNotNull(SystemProperties.getJavaVmSpecificationVendor());
    }

    @Test
    void testGetJavaVmSpecificationVersion() {
        assertNotNull(SystemProperties.getJavaVmSpecificationVersion());
    }

    @Test
    void testGetJavaVmVendor() {
        assertNotNull(SystemProperties.getJavaVmVendor());
    }

    @Test
    void testGetJavaVmVersion() {
        assertNotNull(SystemProperties.getJavaVmVersion());
    }

    @Test
    void testGetLineSeparator() {
        assertNotNull(SystemProperties.getLineSeparator());
        assertNotNull(SystemProperties.getLineSeparator(null));
        assertNotNull(SystemProperties.getLineSeparator(() -> ""));
        assertNotNull(SystemProperties.getLineSeparator(() -> "\n"));
        assertNotNull(SystemProperties.getLineSeparator(() -> null));
        assertNotNull(SystemProperties.getLineSeparator(null));
    }

    @Test
    void testGetLong() {
        final String key = RandomStringUtils.insecure().next(10);
        final String absentKey = RandomStringUtils.insecure().next(10);
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
    void testGetLongClass() {
        final String key = RandomStringUtils.insecure().next(10);
        final String absentKey = RandomStringUtils.insecure().next(10);
        final String keyFull = SIMPLE_NAME + "." + key;
        final String absentKeyFull = SIMPLE_NAME + "." + absentKey;
        assertNull(System.getProperty(absentKeyFull));
        try {
            System.setProperty(keyFull, Long.toString(Long.MAX_VALUE));
            assertEquals(Long.MAX_VALUE, SystemProperties.getLong(SystemPropertiesTest.class, key, () -> 0));
            assertEquals(Long.MAX_VALUE, SystemProperties.getLong(SystemPropertiesTest.class, absentKey, () -> Long.MAX_VALUE));
            assertEquals(0, SystemProperties.getLong(SystemPropertiesTest.class, absentKey, () -> 0));
            assertEquals(1, SystemProperties.getLong(SystemPropertiesTest.class, absentKey, () -> 1));
        } finally {
            System.clearProperty(keyFull);
        }
    }

    @Test
    void testGetOsArch() {
        assertNotNull(SystemProperties.getOsArch());
    }

    @Test
    void testGetOsName() {
        assertNotNull(SystemProperties.getOsName());
    }

    @Test
    void testGetOsVersion() {
        assertNotNull(SystemProperties.getOsVersion());
    }

    @Test
    void testGetPath() {
        assertNull(SystemProperties.getPath(null, null));
        assertNull(SystemProperties.getPath(null, () -> null));
        assertNull(SystemProperties.getPath(StringUtils.EMPTY, null));
        assertEquals(Paths.get("value1"), SystemProperties.getPath(KEY_SPACE_1, null));
        assertEquals(Paths.get("value2"), SystemProperties.getPath(KEY_TAB_1, null));
        assertEquals(Paths.get("value1"), SystemProperties.getPath(null, () -> Paths.get("value1")));
    }

    @Test
    void testGetPathSeparator() {
        assertNotNull(SystemProperties.getPathSeparator());
    }

    @Test
    void testGetProperty() {
        assertNull(SystemProperties.getProperty(null));
        assertNull(SystemProperties.getProperty(StringUtils.EMPTY));
        assertEquals("value1", SystemProperties.getProperty(KEY_SPACE_1));
        assertEquals("value2", SystemProperties.getProperty(KEY_TAB_1));
    }

    @Test
    void testGetPropertyStringString() {
        assertNull(SystemProperties.getProperty(null, StringUtils.NULL));
        assertNull(SystemProperties.getProperty(StringUtils.EMPTY, StringUtils.NULL));
        assertEquals("value1", SystemProperties.getProperty(KEY_SPACE_1, StringUtils.NULL));
        assertEquals("value2", SystemProperties.getProperty("\t", StringUtils.NULL));
        assertEquals("x", SystemProperties.getProperty(null, "x"));
        assertEquals("x", SystemProperties.getProperty(StringUtils.EMPTY, "x"));
        assertEquals("value1", SystemProperties.getProperty(KEY_SPACE_1, "v"));
        assertEquals("value2", SystemProperties.getProperty("\t", "v"));
    }

    @Test
    void testGetPropertyStringSupplier() {
        assertNull(SystemProperties.getProperty(null, (Supplier<String>) null));
        assertNull(SystemProperties.getProperty(StringUtils.EMPTY, (Supplier<String>) null));
        assertEquals("value1", SystemProperties.getProperty(KEY_SPACE_1, (Supplier<String>) null));
        assertEquals("value2", SystemProperties.getProperty("\t", (Supplier<String>) null));
        assertEquals("x", SystemProperties.getProperty(null, () -> "x"));
        assertEquals("x", SystemProperties.getProperty(StringUtils.EMPTY, () -> "x"));
        assertEquals("value1", SystemProperties.getProperty(KEY_SPACE_1, () -> "v"));
        assertEquals("value2", SystemProperties.getProperty("\t", () -> "v"));
    }

    @Test
    void testGetUserCountry() {
        assertDoesNotThrow(SystemProperties::getUserCountry);
    }

    @Test
    void testGetUserDir() {
        assertNotNull(SystemProperties.getUserDir());
    }

    @Test
    void testGetUserHome() {
        assertNotNull(SystemProperties.getUserHome());
    }

    @Test
    void testGetUserLanguage() {
        assertNotNull(SystemProperties.getUserLanguage());
    }

    @Test
    void testGetUserName() {
        assertNotNull(SystemProperties.getUserName());
        assertNotNull(SystemProperties.getUserName(""));
        assertNotNull(SystemProperties.getUserName("User"));
        assertNotNull(SystemProperties.getUserName(null));
    }

    @Test
    void testGetUserTimezone() {
        assertDoesNotThrow(SystemProperties::getUserTimezone);
    }

    @ParameterizedTest
    @ValueSource(strings = { KEY_SPACE_1, KEY_TAB_1 })
    void testIsPropertySet(final String property) {
        assertTrue(SystemProperties.isPropertySet(property));
    }

    @Test
    void testIsPropertySetEdges() {
        assertFalse(SystemProperties.isPropertySet(StringUtils.NULL));
        assertFalse(SystemProperties.isPropertySet(StringUtils.EMPTY));
    }

}
