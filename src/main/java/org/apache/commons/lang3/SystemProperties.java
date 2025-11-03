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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.BooleanSupplier;
import java.util.function.IntSupplier;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

import org.apache.commons.lang3.function.Suppliers;

/**
 * Accesses current <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Property</a> names and values.
 *
 * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
 * @since 3.13.0
 */
public final class SystemProperties {

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.desktop/java/awt/TrayIcon.html#apple.awt.enableTemplateImages">apple.awt.enableTemplateImages</a>
     * @since 3.15.0
     */
    public static final String APPLE_AWT_ENABLE_TEMPLATE_IMAGES = "apple.awt.enableTemplateImages";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String AWT_TOOLKIT = "awt.toolkit";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.naming/module-summary.html#com.sun.jndi.ldap.object.trustSerialData">com.sun.jndi.ldap.object.trustSerialData</a>
     * @since 3.15.0
     */
    public static final String COM_SUN_JNDI_LDAP_OBJECT_TRUST_SERIAL_DATA = "com.sun.jndi.ldap.object.trustSerialData";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/com/sun/net/httpserver/spi/HttpServerProvider.html#com.sun.net.httpserver.HttpServerProvider">com.sun.net.httpserver.HttpServerProvider</a>
     * @since 3.15.0
     */
    public static final String COM_SUN_NET_HTTP_SERVER_HTTP_SERVER_PROVIDER = "com.sun.net.httpserver.HttpServerProvider";

    /**
     * The System property name {@value}.
     */
    public static final String FILE_ENCODING = "file.encoding";

    /**
     * The System property name {@value}.
     */
    public static final String FILE_SEPARATOR = "file.separator";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#ftp.nonProxyHosts">ftp.nonProxyHosts</a>
     * @since 3.15.0
     */
    public static final String FTP_NON_PROXY_HOST = "ftp.nonProxyHosts";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#ftp.proxyHost">ftp.proxyHost</a>
     * @since 3.15.0
     */
    public static final String FTP_PROXY_HOST = "ftp.proxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#ftp.proxyPort">ftp.proxyPort</a>
     * @since 3.15.0
     */
    public static final String FTP_PROXY_PORT = "ftp.proxyPort";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.agent">http.agent</a>
     * @since 3.15.0
     */
    public static final String HTTP_AGENT = "http.agent";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#auth.digest.cnonceRepeat">auth.digest.cnonceRepeat</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_CNONCE_REPEAT = "http.auth.digest.cnonceRepeat";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#reEnabledAlgorithms">http.auth.digest.reEnabledAlgorithms</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_RE_ENABLED_ALGORITHMS = "http.auth.digest.reEnabledAlgorithms";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.auth.digest.validateProxy">http.auth.digest.validateProxy</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_VALIDATE_PROXY = "http.auth.digest.validateProxy";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.auth.digest.validateServer">http.auth.digest.validateServer</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_VALIDATE_SERVER = "http.auth.digest.validateServer";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.auth.ntlm.domain">http.auth.ntlm.domain</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_NTLM_DOMAIN = "http.auth.ntlm.domain";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.keepAlive">http.keepAlive</a>
     * @since 3.15.0
     */
    public static final String HTTP_KEEP_ALIVE = "http.keepAlive";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.keepAlive.time.proxy">http.keepAlive.time.proxy</a>
     * @since 3.15.0
     */
    public static final String HTTP_KEEP_ALIVE_TIME_PROXY = "http.keepAlive.time.proxy";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.keepAlive.time.server">http.keepAlive.time.server</a>
     * @since 3.15.0
     */
    public static final String HTTP_KEEP_ALIVE_TIME_SERVER = "http.keepAlive.time.server";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.maxConnections">http.maxConnections</a>
     * @since 3.15.0
     */
    public static final String HTTP_MAX_CONNECTIONS = "http.maxConnections";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.maxRedirects">http.maxRedirects</a>
     * @since 3.15.0
     */
    public static final String HTTP_MAX_REDIRECTS = "http.maxRedirects";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.nonProxyHosts">http.nonProxyHosts</a>
     * @since 3.15.0
     */
    public static final String HTTP_NON_PROXY_HOSTS = "http.nonProxyHosts";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.proxyHost">http.proxyHost</a>
     * @since 3.15.0
     */
    public static final String HTTP_PROXY_HOST = "http.proxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#http.proxyPort">http.proxyPort</a>
     * @since 3.15.0
     */
    public static final String HTTP_PROXY_PORT = "http.proxyPort";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#https.proxyHost">https.proxyHost</a>
     * @since 3.15.0
     */
    public static final String HTTPS_PROXY_HOST = "https.proxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#https.proxyPort">https.proxyPort</a>
     * @since 3.15.0
     */
    public static final String HTTPS_PROXY_PORT = "https.proxyPort";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_AWT_FONTS = "java.awt.fonts";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_AWT_GRAPHICSENV = "java.awt.graphicsenv";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_AWT_HEADLESS = "java.awt.headless";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_AWT_PRINTERJOB = "java.awt.printerjob";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">java.class.path</a>
     */
    public static final String JAVA_CLASS_PATH = "java.class.path";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_CLASS_VERSION = "java.class.version";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://bugs.openjdk.org/browse/JDK-8305998">JDK-8305998</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_COMPILER = "java.compiler";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/ContentHandler.html#java.content.handler.pkgs">java.content.handler.pkgs</a>
     * @since 3.15.0
     */
    public static final String JAVA_CONTENT_HANDLER_PKGS = "java.content.handler.pkgs";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_ENDORSED_DIRS = "java.endorsed.dirs";

    /**
     * The System property name {@value}.
     * <p>
     * Not in Java 17 and above according to <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a> Javadoc.
     * </p>
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static final String JAVA_EXT_DIRS = "java.ext.dirs";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_HOME = "java.home";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_IO_TMPDIR = "java.io.tmpdir";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_LIBRARY_PATH = "java.library.path";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_LOCALE_PROVIDERS = "java.locale.providers";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/Locale.html#java.locale.useOldISOCodes">java.locale.useOldISOCodes</a>
     * @since 3.15.0
     */
    public static final String JAVA_LOCALE_USE_OLD_ISO_CODES = "java.locale.useOldISOCodes";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#java.net.preferIPv4Stack">java.net.preferIPv4Stack</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_PREFER_IPV4_STACK = "java.net.preferIPv4Stack";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#java.net.preferIPv6Addresses">java.net.preferIPv6Addresses</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_PREFER_IPV6_ADDRESSES = "java.net.preferIPv6Addresses";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#java.net.socks.password">java.net.socks.password</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_SOCKS_PASSWORD = "java.net.socks.password";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#java.net.socks.username">java.net.socks.username</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_SOCKS_USER_NAME = "java.net.socks.username";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/doc-files/net-properties.html#java.net.useSystemProxies">java.net.useSystemProxies</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_USE_SYSTEM_PROXIES = "java.net.useSystemProxies";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/channels/AsynchronousChannelGroup.html#java.nio.channels.DefaultThreadPool.initialSize">java.nio.channels.DefaultThreadPool.initialSize</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_INITIAL_SIZE = "java.nio.channels.DefaultThreadPool.initialSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/channels/AsynchronousChannelGroup.html#java.nio.channels.DefaultThreadPool.threadFactory">java.nio.channels.DefaultThreadPool.threadFactory</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_THREAD_FACTORY = "java.nio.channels.DefaultThreadPool.threadFactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/channels/AsynchronousChannelGroup.html#java.nio.channels.DefaultThreadPool.initialSize">java.nio.channels.DefaultThreadPool.initialSize</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_SPI_ASYNCHRONOUS_CHANNEL_PROVIDER = "java.nio.channels.spi.AsynchronousChannelProvider";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/channels/spi/SelectorProvider.html#java.nio.channels.spi.SelectorProvider">java.nio.channels.spi.SelectorProvider</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_SPI_SELECTOR_PROVIDER = "java.nio.channels.spi.SelectorProvider";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/file/spi/FileSystemProvider.html#java.nio.file.spi.DefaultFileSystemProvider">java.nio.file.spi.DefaultFileSystemProvider</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_FILE_SPI_DEFAULT_FILE_SYSTEM_PROVIDER = "java.nio.file.spi.DefaultFileSystemProvider";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/Properties.html#java.properties.date">java.properties.date</a>
     * @since 3.15.0
     */
    public static final String JAVA_PROPERTIES_DATE = "java.properties.date";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/net/URL.html#java.protocol.handler.pkgs">java.protocol.handler.pkgs</a>
     * @since 3.15.0
     */
    public static final String JAVA_PROTOCOL_HANDLER_PKGS = "java.protocol.handler.pkgs";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.rmi/java/rmi/server/RMIClassLoader.html#java.rmi.server.codebase">java.rmi.server.codebase</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_CODEBASE = "java.rmi.server.codebase";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.rmi/java/rmi/server/RMISocketFactory.html#java.rmi.server.hostname">java.rmi.server.hostname</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_HOST_NAME = "java.rmi.server.hostname";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.rmi/java/rmi/server/ObjID.html#java.rmi.server.randomIDs">java.rmi.server.randomIDs</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_RANDOM_IDS = "java.rmi.server.randomIDs";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.rmi/java/rmi/server/RMIClassLoader.html#java.rmi.server.RMIClassLoaderSpi">java.rmi.server.RMIClassLoaderSpi</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_RMI_CLASS_LOADER_SPI = "java.rmi.server.RMIClassLoaderSpi";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_RUNTIME_NAME = "java.runtime.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_RUNTIME_VERSION = "java.runtime.version";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.security.auth/com/sun/security/auth/login/ConfigFile.html#java.security.auth.login.config">java.security.auth.login.config</a>
     * @since 3.15.0
     */
    public static final String JAVA_SECURITY_AUTH_LOGIN_CONFIG = "java.security.auth.login.config";

    /**
     * The System property name {@value}.
     *
     * @see <a href= "https://docs.oracle.com/en/java/javase/24/docs/api/system-properties.html">java.security.krb5.conf</a>
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/24/docs/api/java.security.jgss/javax/security/auth/kerberos/package-summary.html#java.security.krb5.conf">package
     *      javax.security.auth.kerberos conf</a>
     * @since 3.18.0
     */
    public static final String JAVA_SECURITY_KERBEROS_CONF = "java.security.krb5.conf";

    /**
     * The System property name {@value}.
     *
     * @see <a href= "https://docs.oracle.com/en/java/javase/24/docs/api/system-properties.html">java.security.krb5.kdc</a>
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/24/docs/api/java.security.jgss/javax/security/auth/kerberos/package-summary.html#java.security.krb5.kdc">package
     *      javax.security.auth.kerberos KDC</a>
     * @since 3.18.0
     */
    public static final String JAVA_SECURITY_KERBEROS_KDC = "java.security.krb5.kdc";

    /**
     * The System property name {@value}.
     *
     * @see <a href= "https://docs.oracle.com/en/java/javase/24/docs/api/system-properties.html">java.security.krb5.realm</a>
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/24/docs/api/java.security.jgss/javax/security/auth/kerberos/package-summary.html#java.security.krb5.realm">package
     *      javax.security.auth.kerberos realm</a>
     * @since 3.18.0
     */
    public static final String JAVA_SECURITY_KERBEROS_REALM = "java.security.krb5.realm";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/24/security/troubleshooting-security.html">java.security.debug</a>
     * @since 3.18.0
     */
    public static final String JAVA_SECURITY_DEBUG = "java.security.debug";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/SecurityManager.html#java.security.manager">java.security.manager</a>
     * @since 3.15.0
     */
    public static final String JAVA_SECURITY_MANAGER = "java.security.manager";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/System.html#java.specification.maintenance.version">java.specification.maintenance.version</a>
     * @since 3.15.0
     */
    public static final String JAVA_SPECIFICATION_MAINTENANCE_VERSION = "java.specification.maintenance.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_SPECIFICATION_NAME = "java.specification.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_SPECIFICATION_VENDOR = "java.specification.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_SPECIFICATION_VERSION = "java.specification.version";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/ClassLoader.html#java.system.class.loader">java.system.class.loader</a>
     * @since 3.15.0
     */
    public static final String JAVA_SYSTEM_CLASS_LOADER = "java.system.class.loader";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/time/zone/ZoneRulesProvider.html#java.time.zone.DefaultZoneRulesProvider">java.time.zone.DefaultZoneRulesProvider</a>
     * @since 3.15.0
     */
    public static final String JAVA_TIME_ZONE_DEFAULT_ZONE_RULES_PROVIDER = "java.time.zone.DefaultZoneRulesProvider";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ForkJoinPool.html#java.util.concurrent.ForkJoinPool.common.exceptionHandler">java.util.concurrent.ForkJoinPool.common.exceptionHandler</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_EXCEPTION_HANDLER = "java.util.concurrent.ForkJoinPool.common.exceptionHandler";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ForkJoinPool.html#java.util.concurrent.ForkJoinPool.common.maximumSpares">java.util.concurrent.ForkJoinPool.common.maximumSpares</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_MAXIMUM_SPARES = "java.util.concurrent.ForkJoinPool.common.maximumSpares";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ForkJoinPool.html#java.util.concurrent.ForkJoinPool.common.parallelism">java.util.concurrent.ForkJoinPool.common.parallelism</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_PARALLELISM = "java.util.concurrent.ForkJoinPool.common.parallelism";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ForkJoinPool.html#java.util.concurrent.ForkJoinPool.common.threadFactory">java.util.concurrent.ForkJoinPool.common.threadFactory</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_THREAD_FACTORY = "java.util.concurrent.ForkJoinPool.common.threadFactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/Currency.html#java.util.currency.data">java.util.currency.data</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CURRENCY_DATA = "java.util.currency.data";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.logging/java/util/logging/LogManager.html#java.util.logging.config.class">java.util.logging.config.class</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_LOGGING_CONFIG_CLASS = "java.util.logging.config.class";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.logging/java/util/logging/LogManager.html#java.util.logging.config.file">java.util.logging.config.file</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_LOGGING_CONFIG_FILE = "java.util.logging.config.file";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.logging/java/util/logging/SimpleFormatter.html#java.util.logging.SimpleFormatter.format">java.util.logging.SimpleFormatter.format</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_LOGGING_SIMPLE_FORMATTER_FORMAT = "java.util.logging.simpleformatter.format";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.prefs/java/util/prefs/Preferences.html#java.util.prefs.PreferencesFactory">java.util.prefs.PreferencesFactory</a>
     */
    public static final String JAVA_UTIL_PREFS_PREFERENCES_FACTORY = "java.util.prefs.PreferencesFactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/PropertyResourceBundle.html#java.util.PropertyResourceBundle.encoding">java.util.PropertyResourceBundle.encoding</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_PROPERTY_RESOURCE_BUNDLE_ENCODING = "java.util.PropertyResourceBundle.encoding";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VENDOR = "java.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VENDOR_URL = "java.vendor.url";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/System.html#java.vendor.version">java.vendor.version</a>
     * @since 3.15.0
     */
    public static final String JAVA_VENDOR_VERSION = "java.vendor.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VERSION = "java.version";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/System.html#java.version.date">java.version.date</a>
     * @since 3.15.0
     */
    public static final String JAVA_VERSION_DATE = "java.version.date";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_INFO = "java.vm.info";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_NAME = "java.vm.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = "java.vm.specification.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = "java.vm.specification.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = "java.vm.specification.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_VENDOR = "java.vm.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_VERSION = "java.vm.version";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.xml/module-summary.html#java.xml.config.file">java.xml</a>
     * @since 3.15.0
     */
    public static final String JAVA_XML_CONFIG_FILE = "java.xml.config.file";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.desktop/java/awt/Toolkit.html#javax.accessibility.assistive_technologies">javax.accessibility.assistive_technologies</a>
     * @since 3.15.0
     */
    public static final String JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES = "javax.accessibility.assistive_technologies";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/javax/net/ssl/SSLSessionContext.html#javax.net.ssl.sessionCacheSize">javax.net.ssl.sessionCacheSize</a>
     * @since 3.15.0
     */
    public static final String JAVAX_NET_SSL_SESSION_CACHE_SIZE = "javax.net.ssl.sessionCacheSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.rmi/javax/rmi/ssl/SslRMIClientSocketFactory.html#javax.rmi.ssl.client.enabledCipherSuites">javax.rmi.ssl.client.enabledCipherSuites</a>
     * @since 3.15.0
     */
    public static final String JAVAX_RMI_SSL_CLIENT_ENABLED_CIPHER_SUITES = "javax.rmi.ssl.client.enabledCipherSuites";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.rmi/javax/rmi/ssl/SslRMIClientSocketFactory.html#javax.rmi.ssl.client.enabledProtocols">javax.rmi.ssl.client.enabledProtocols</a>
     * @since 3.15.0
     */
    public static final String JAVAX_RMI_SSL_CLIENT_ENABLED_PROTOCOLS = "javax.rmi.ssl.client.enabledProtocols";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.security.jgss/org/ietf/jgss/package-summary.html#javax.security.auth.useSubjectCredsOnly">javax.security.auth.useSubjectCredsOnly</a>
     * @since 3.15.0
     */
    public static final String JAVAX_SECURITY_AUTH_USE_SUBJECT_CREDS_ONLY = "javax.security.auth.useSubjectCredsOnly";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.smartcardio/javax/smartcardio/TerminalFactory.html#javax.smartcardio.TerminalFactory.DefaultType">javax.smartcardio.TerminalFactory.DefaultType</a>
     * @since 3.15.0
     */
    public static final String JAVAX_SMART_CARD_IO_TERMINAL_FACTORY_DEFAULT_TYPE = "javax.smartcardio.TerminalFactory.DefaultType";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/java.sql/java/sql/DriverManager.html#jdbc.drivers">jdbc.drivers</a>
     * @since 3.15.0
     */
    public static final String JDBC_DRIVERS = "jdbc.drivers";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.http.auth.proxying.disabledSchemes</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_AUTH_PROXYING_DISABLED_SCHEMES = "jdk.http.auth.proxying.disabledSchemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.http.auth.tunneling.disabledSchemes</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_AUTH_TUNNELING_DISABLED_SCHEMES = "jdk.http.auth.tunneling.disabledSchemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.allowRestrictedHeaders</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_ALLOW_RESTRICTED_HEADERS = "jdk.httpclient.allowRestrictedHeaders";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.auth.retrylimit</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_AUTH_RETRY_LIMIT = "jdk.httpclient.auth.retrylimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.bufsize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_BUF_SIZE = "jdk.httpclient.bufsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.connectionPoolSize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_CONNECTION_POOL_SIZE = "jdk.httpclient.connectionPoolSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.connectionWindowSize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_CONNECTION_WINDOW_SIZE = "jdk.httpclient.connectionWindowSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.disableRetryConnect</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_DISABLE_RETRY_CONNECT = "jdk.httpclient.disableRetryConnect";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.enableAllMethodRetry</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_ENABLE_ALL_METHOD_RETRY = "jdk.httpclient.enableAllMethodRetry";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.enablepush</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_ENABLE_PUSH = "jdk.httpclient.enablepush";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.hpack.maxheadertablesize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_HPACK_MAX_HEADER_TABLE_SIZE = "jdk.httpclient.hpack.maxheadertablesize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.HttpClient.log</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_HTTP_CLIENT_LOG = "jdk.httpclient.HttpClient.log";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.keepalive.timeout</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT = "jdk.httpclient.keepalive.timeout";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.keepalive.timeout.h2</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT_H2 = "jdk.httpclient.keepalive.timeout.h2";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.maxframesize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_MAX_FRAME_SIZE = "jdk.httpclient.maxframesize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.maxstreams</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_MAX_STREAMS = "jdk.httpclient.maxstreams";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.receiveBufferSize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_RECEIVE_BUFFER_SIZE = "jdk.httpclient.receiveBufferSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.redirects.retrylimit</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_REDIRECTS_RETRY_LIMIT = "jdk.httpclient.redirects.retrylimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.sendBufferSize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_SEND_BUFFER_SIZE = "jdk.httpclient.sendBufferSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.websocket.writeBufferSize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_WEB_SOCKET_WRITE_BUFFER_SIZE = "jdk.httpclient.websocket.writeBufferSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpclient.windowsize</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_CLIENT_WINDOW_SIZE = "jdk.httpclient.windowsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.httpserver.maxConnections</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_SERVER_MAX_CONNECTIONS = "jdk.httpserver.maxConnections";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.https.negotiate.cbt</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPS_NEGOTIATE_CBT = "jdk.https.negotiate.cbt";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.includeInExceptions</a>
     * @since 3.15.0
     */
    public static final String JDK_INCLUDE_IN_EXCEPTIONS = "jdk.includeInExceptions";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.internal.httpclient.disableHostnameVerification</a>
     * @since 3.15.0
     */
    public static final String JDK_INTERNAL_HTTP_CLIENT_DISABLE_HOST_NAME_VERIFICATION = "jdk.internal.httpclient.disableHostnameVerification";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.io.permissionsUseCanonicalPath</a>
     * @since 3.15.0
     */
    public static final String JDK_IO_PERMISSIONS_USE_CANONICAL_PATH = "jdk.io.permissionsUseCanonicalPath";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.jndi.ldap.object.factoriesFilter</a>
     * @since 3.15.0
     */
    public static final String JDK_JNDI_LDAP_OBJECT_FACTORIES_FILTER = "jdk.jndi.ldap.object.factoriesFilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.jndi.object.factoriesFilter</a>
     * @since 3.15.0
     */
    public static final String JDK_JNDI_OBJECT_FACTORIES_FILTER = "jdk.jndi.object.factoriesFilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.jndi.rmi.object.factoriesFilter</a>
     * @since 3.15.0
     */
    public static final String JDK_JNDI_RMI_OBJECT_FACTORIES_FILTER = "jdk.jndi.rmi.object.factoriesFilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.module.main</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_MAIN = "jdk.module.main";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.module.main.class</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_MAIN_CLASS = "jdk.module.main.class";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.module.path</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_PATH = "jdk.module.path";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.module.upgrade.path</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_UPGRADE_PATH = "jdk.module.upgrade.path";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.net.unixdomain.tmpdir</a>
     * @since 3.15.0
     */
    public static final String JDK_NET_UNIX_DOMAIN_TMPDIR = "jdk.net.unixdomain.tmpdir";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_NET_URL_CLASS_PATH_SHOW_IGNORED_CLASS_PATH_ENTRIES = "jdk.net.URLClassPath.showIgnoredClassPathEntries";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.serialFilter</a>
     * @since 3.15.0
     */
    public static final String JDK_SERIAL_FILTER = "jdk.serialFilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.serialFilterFactory</a>
     * @since 3.15.0
     */
    public static final String JDK_SERIAL_FILTER_FACTORY = "jdk.serialFilterFactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.tls.client.SignatureSchemes</a>
     * @since 3.15.0
     */
    public static final String JDK_TLS_CLIENT_SIGNATURE_SCHEMES = "jdk.tls.client.SignatureSchemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.tls.namedGroups</a>
     * @since 3.15.0
     */
    public static final String JDK_TLS_NAMED_GROUPS = "jdk.tls.namedGroups";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.tls.server.SignatureSchemes</a>
     * @since 3.15.0
     */
    public static final String JDK_TLS_SERVER_SIGNATURE_SCHEMES = "jdk.tls.server.SignatureSchemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.virtualThreadScheduler.maxPoolSize</a>
     * @since 3.15.0
     */
    public static final String JDK_VIRTUAL_THREAD_SCHEDULER_MAXPOOLSIZE = "jdk.virtualThreadScheduler.maxPoolSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.virtualThreadScheduler.parallelism</a>
     * @since 3.15.0
     */
    public static final String JDK_VIRTUAL_THREAD_SCHEDULER_PARALLELISM = "jdk.virtualThreadScheduler.parallelism";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.cdataChunkSize</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_CDATA_CHUNK_SIZE = "jdk.xml.cdataChunkSize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.dtd.support</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_DTD_SUPPORT = "jdk.xml.dtd.support";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.elementAttributeLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ELEMENT_ATTRIBUTE_LIMIT = "jdk.xml.elementAttributeLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.enableExtensionFunctions</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ENABLE_EXTENSION_FUNCTIONS = "jdk.xml.enableExtensionFunctions";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.entityExpansionLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ENTITY_EXPANSION_LIMIT = "jdk.xml.entityExpansionLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.entityReplacementLimi_t</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ENTITY_REPLACEMENT_LIMIT = "jdk.xml.entityReplacementLimi_t";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.isStandalone</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_IS_STANDALONE = "jdk.xml.isStandalone";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.jdkcatalog.resolve</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_JDK_CATALOG_RESOLVE = "jdk.xml.jdkcatalog.resolve";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.maxElementDepth</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAX_ELEMENT_DEPTH = "jdk.xml.maxElementDepth";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.maxGeneralEntitySizeLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAX_GENERAL_ENTITY_SIZE_LIMIT = "jdk.xml.maxGeneralEntitySizeLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.maxOccurLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAX_OCCUR_LIMIT = "jdk.xml.maxOccurLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.maxParameterEntitySizeLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAX_PARAMETER_ENTITY_SIZE_LIMIT = "jdk.xml.maxParameterEntitySizeLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.maxXMLNameLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAX_XML_NAME_LIMIT = "jdk.xml.maxXMLNameLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.overrideDefaultParser</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_OVERRIDE_DEFAULT_PARSER = "jdk.xml.overrideDefaultParser";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.resetSymbolTable</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_RESET_SYMBOL_TABLE = "jdk.xml.resetSymbolTable";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.totalEntitySizeLimit</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_TOTAL_ENTITY_SIZE_LIMIT = "jdk.xml.totalEntitySizeLimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">jdk.xml.xsltcIsStandalone</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_XSLTC_IS_STANDALONE = "jdk.xml.xsltcIsStandalone";

    /**
     * The System property name {@value}.
     */
    public static final String LINE_SEPARATOR = "line.separator";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">native.encoding</a>
     * @since 3.15.0
     */
    public static final String NATIVE_ENCODING = "native.encoding";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">networkaddress.cache.negative.ttl</a>
     * @since 3.15.0
     */
    public static final String NETWORK_ADDRESS_CACHE_NEGATIVE_TTL = "networkaddress.cache.negative.ttl";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">networkaddress.cache.stale.ttl</a>
     * @since 3.15.0
     */
    public static final String NETWORK_ADDRESS_CACHE_STALE_TTL = "networkaddress.cache.stale.ttl";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">networkaddress.cache.ttl</a>
     * @since 3.15.0
     */
    public static final String NETWORK_ADDRESS_CACHE_TTL = "networkaddress.cache.ttl";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">org.jcp.xml.dsig.securevalidation</a>
     * @since 3.15.0
     */
    public static final String ORG_JCP_XML_DSIG_SECURE_VALIDATION = "org.jcp.xml.dsig.securevalidation";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">org.openjdk.java.util.stream.tripwire</a>
     * @since 3.15.0
     */
    public static final String ORG_OPENJDK_JAVA_UTIL_STREAM_TRIPWIRE = "org.openjdk.java.util.stream.tripwire";

    /**
     * The System property name {@value}.
     */
    public static final String OS_ARCH = "os.arch";

    /**
     * The System property name {@value}.
     */
    public static final String OS_NAME = "os.name";

    /**
     * The System property name {@value}.
     */
    public static final String OS_VERSION = "os.version";

    /**
     * The System property name {@value}.
     */
    public static final String PATH_SEPARATOR = "path.separator";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SOCKS_PROXY_HOST = "socksProxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SOCKS_PROXY_PORT = "socksProxyPort";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SOCKS_PROXY_VERSION = "socksProxyVersion";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String STDERR_ENCODING = "stderr.encoding";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String STDOUT_ENCODING = "stdout.encoding";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.drainAmount">sun.net.httpserver.drainAmount</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_DRAIN_AMOUNT = "sun.net.httpserver.drainAmount";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.idleInterval">sun.net.httpserver.idleInterval</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_IDLE_INTERVAL = "sun.net.httpserver.idleInterval";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.maxIdleConnections">sun.net.httpserver.maxIdleConnections</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_MAX_IDLE_CONNECTIONS = "sun.net.httpserver.maxIdleConnections";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.maxReqHeaders">sun.net.httpserver.maxReqHeaders</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_MAX_REQ_HEADERS = "sun.net.httpserver.maxReqHeaders";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.maxReqTime">sun.net.httpserver.maxReqTime</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_MAX_REQ_TIME = "sun.net.httpserver.maxReqTime";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.maxRspTime">sun.net.httpserver.maxRspTime</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_MAX_RSP_TIME = "sun.net.httpserver.maxRspTime";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.net.httpserver.nodelay">sun.net.httpserver.nodelay</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTP_SERVER_NO_DELAY = "sun.net.httpserver.nodelay";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/module-summary.html#sun.security.krb5.principal">sun.security.krb5.principal</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_SECURITY_KRB5_PRINCIPAL = "sun.security.krb5.principal";

    /**
     * The System property name {@value}.
     */
    public static final String USER_COUNTRY = "user.country";

    /**
     * The System property name {@value}.
     */
    public static final String USER_DIR = "user.dir";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String USER_EXTENSIONS = "user.extensions";

    /**
     * The System property name {@value}.
     */
    public static final String USER_HOME = "user.home";

    /**
     * The System property name {@value}.
     */
    public static final String USER_LANGUAGE = "user.language";

    /**
     * The System property name {@value}.
     */
    public static final String USER_NAME = "user.name";

    /**
     * The System property name {@value}.
     */
    public static final String USER_REGION = "user.region";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String USER_SCRIPT = "user.script";

    /**
     * The System property name {@value}.
     */
    public static final String USER_TIMEZONE = "user.timezone";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String USER_VARIANT = "user.variant";

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getAppleAwtEnableTemplateImages() {
        return getProperty(APPLE_AWT_ENABLE_TEMPLATE_IMAGES);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @see #AWT_TOOLKIT
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getAwtToolkit() {
        return getProperty(AWT_TOOLKIT);
    }

    /**
     * Gets the current value for the property named {@code "SimpleClassName.Key"} as a {@code boolean}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param clazz           The Class to use for the SimpleClassName.
     * @param key             The subkey.
     * @param defaultIfAbsent The default value.
     * @return an int or {@code defaultIfAbsent}'s value.
     * @see Class#getSimpleName()
     * @since 3.19.0
     */
    public static boolean getBoolean(final Class<?> clazz, final String key, final BooleanSupplier defaultIfAbsent) {
        return getBoolean(toKey(clazz, key, true), defaultIfAbsent);
    }

    /**
     * Gets the current value for the property named {@code key} as a {@code boolean}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param key             The key.
     * @param defaultIfAbsent The default value.
     * @return a {@code boolean} or {@code defaultIfAbsent}'s value.
     */
    public static boolean getBoolean(final String key, final BooleanSupplier defaultIfAbsent) {
        final String str = getProperty(key);
        return str == null ? defaultIfAbsent != null && defaultIfAbsent.getAsBoolean() : Boolean.parseBoolean(str);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getComSunJndiLdapObjectTrustSerialData() {
        return getProperty(COM_SUN_JNDI_LDAP_OBJECT_TRUST_SERIAL_DATA);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getComSunNetHttpServerHttpServerProvider() {
        return getProperty(COM_SUN_NET_HTTP_SERVER_HTTP_SERVER_PROVIDER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getFileEncoding() {
        return getProperty(FILE_ENCODING);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getFileSeparator() {
        return getProperty(FILE_SEPARATOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getFtpNonProxyHost() {
        return getProperty(FTP_NON_PROXY_HOST);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getFtpProxyHost() {
        return getProperty(FTP_PROXY_HOST);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getFtpProxyPort() {
        return getProperty(FTP_PROXY_PORT);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpAgent() {
        return getProperty(HTTP_AGENT);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpAuthDigestCnonceRepeat() {
        return getProperty(HTTP_AUTH_DIGEST_CNONCE_REPEAT);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpAuthDigestReenabledAlgorithms() {
        return getProperty(HTTP_AUTH_DIGEST_RE_ENABLED_ALGORITHMS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpAuthDigestValidateProxy() {
        return getProperty(HTTP_AUTH_DIGEST_VALIDATE_PROXY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpAuthDigestValidateServer() {
        return getProperty(HTTP_AUTH_DIGEST_VALIDATE_SERVER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpAuthNtlmDomain() {
        return getProperty(HTTP_AUTH_NTLM_DOMAIN);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpKeepAlive() {
        return getProperty(HTTP_KEEP_ALIVE);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpKeepAliveTimeProxy() {
        return getProperty(HTTP_KEEP_ALIVE_TIME_PROXY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpKeepAliveTimeServer() {
        return getProperty(HTTP_KEEP_ALIVE_TIME_SERVER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpMaxConnections() {
        return getProperty(HTTP_MAX_CONNECTIONS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpMaxRedirects() {
        return getProperty(HTTP_MAX_REDIRECTS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpNonProxyHosts() {
        return getProperty(HTTP_NON_PROXY_HOSTS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpProxyHost() {
        return getProperty(HTTP_PROXY_HOST);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpProxyPort() {
        return getProperty(HTTP_PROXY_PORT);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpsProxyHost() {
        return getProperty(HTTPS_PROXY_HOST);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getHttpsProxyPort() {
        return getProperty(HTTPS_PROXY_PORT);
    }

    /**
     * Gets the current value for the property named {@code "SimpleClassName.Key"} as an {@code int}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param clazz           The Class to use for the SimpleClassName.
     * @param key             The subkey.
     * @param defaultIfAbsent The default value.
     * @return an int or {@code defaultIfAbsent}'s value.
     * @see Class#getSimpleName()
     * @since 3.19.0
     */
    public static int getInt(final Class<?> clazz, final String key, final IntSupplier defaultIfAbsent) {
        return getInt(toKey(clazz, key, true), defaultIfAbsent);
    }

    /**
     * Gets the current value for the property named {@code key} as an {@code int}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param key             The key.
     * @param defaultIfAbsent The default value.
     * @return an {@code int} or {@code defaultIfAbsent}'s value.
     */
    public static int getInt(final String key, final IntSupplier defaultIfAbsent) {
        final String str = getProperty(key);
        return str == null ? defaultIfAbsent != null ? defaultIfAbsent.getAsInt() : 0 : Integer.parseInt(str);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @see #JAVA_AWT_FONTS
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaAwtFonts() {
        return getProperty(JAVA_AWT_FONTS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaAwtGraphicsenv() {
        return getProperty(JAVA_AWT_GRAPHICSENV);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @see #JAVA_AWT_HEADLESS
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaAwtHeadless() {
        return getProperty(JAVA_AWT_HEADLESS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @see #JAVA_AWT_PRINTERJOB
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaAwtPrinterjob() {
        return getProperty(JAVA_AWT_PRINTERJOB);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">java.class.path</a>
     * @see #JAVA_CLASS_PATH
     */
    public static String getJavaClassPath() {
        return getProperty(JAVA_CLASS_PATH);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaClassVersion() {
        return getProperty(JAVA_CLASS_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://bugs.openjdk.org/browse/JDK-8305998">JDK-8305998</a>
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaCompiler() {
        return getProperty(JAVA_COMPILER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaContentHandlerPkgs() {
        return getProperty(JAVA_CONTENT_HANDLER_PKGS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaEndorsedDirs() {
        return getProperty(JAVA_ENDORSED_DIRS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/api/system-properties.html">System Properties</a>
     * @see #JAVA_EXT_DIRS
     * @deprecated Deprecated without replacement.
     */
    @Deprecated
    public static String getJavaExtDirs() {
        return getProperty(JAVA_EXT_DIRS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaHome() {
        return getProperty(JAVA_HOME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaIoTmpdir() {
        return getProperty(JAVA_IO_TMPDIR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaLibraryPath() {
        return getProperty(JAVA_LIBRARY_PATH);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     * <p>
     * Java 9 and above.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaLocaleProviders() {
        return getProperty(JAVA_LOCALE_PROVIDERS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaLocaleUseOldIsoCodes() {
        return getProperty(JAVA_LOCALE_USE_OLD_ISO_CODES);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNetPreferIpv4Stack() {
        return getProperty(JAVA_NET_PREFER_IPV4_STACK);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNetPreferIpv6Addresses() {
        return getProperty(JAVA_NET_PREFER_IPV6_ADDRESSES);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNetSocksPassword() {
        return getProperty(JAVA_NET_SOCKS_PASSWORD);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNetSocksUserName() {
        return getProperty(JAVA_NET_SOCKS_USER_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNetUseSystemProxies() {
        return getProperty(JAVA_NET_USE_SYSTEM_PROXIES);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNioChannelsDefaultThreadPoolInitialSize() {
        return getProperty(JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_INITIAL_SIZE);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNioChannelsDefaultThreadPoolThreadFactory() {
        return getProperty(JAVA_NIO_CHANNELS_DEFAULT_THREAD_POOL_THREAD_FACTORY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNioChannelsSpiAsynchronousChannelProvider() {
        return getProperty(JAVA_NIO_CHANNELS_SPI_ASYNCHRONOUS_CHANNEL_PROVIDER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNioChannelsSpiSelectorProvider() {
        return getProperty(JAVA_NIO_CHANNELS_SPI_SELECTOR_PROVIDER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaNioFileSpiDefaultFileSystemProvider() {
        return getProperty(JAVA_NIO_FILE_SPI_DEFAULT_FILE_SYSTEM_PROVIDER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaPropertiesDate() {
        return getProperty(JAVA_PROPERTIES_DATE);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaProtocolHandlerPkgs() {
        return getProperty(JAVA_PROTOCOL_HANDLER_PKGS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaRmiServerCodebase() {
        return getProperty(JAVA_RMI_SERVER_CODEBASE);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaRmiServerHostName() {
        return getProperty(JAVA_RMI_SERVER_HOST_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaRmiServerRandomIds() {
        return getProperty(JAVA_RMI_SERVER_RANDOM_IDS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaRmiServerRmiClassLoaderSpi() {
        return getProperty(JAVA_RMI_SERVER_RMI_CLASS_LOADER_SPI);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaRuntimeName() {
        return getProperty(JAVA_RUNTIME_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaRuntimeVersion() {
        return getProperty(JAVA_RUNTIME_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaSecurityAuthLoginConfig() {
        return getProperty(JAVA_SECURITY_AUTH_LOGIN_CONFIG);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaSecurityManager() {
        return getProperty(JAVA_SECURITY_MANAGER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaSpecificationMaintenanceVersion() {
        return getProperty(JAVA_SPECIFICATION_MAINTENANCE_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaSpecificationName() {
        return getProperty(JAVA_SPECIFICATION_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaSpecificationVendor() {
        return getProperty(JAVA_SPECIFICATION_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaSpecificationVersion() {
        return getProperty(JAVA_SPECIFICATION_VERSION);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_SPECIFICATION_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @param defaultValue get this Supplier when the property is empty or throws SecurityException.
     * @return The system property value or {@code defaultValue} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaSpecificationVersion(final String defaultValue) {
        return getProperty(JAVA_SPECIFICATION_VERSION, defaultValue);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_SYSTEM_CLASS_LOADER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaSystemClassLoader() {
        return getProperty(JAVA_SYSTEM_CLASS_LOADER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_TIME_ZONE_DEFAULT_ZONE_RULES_PROVIDER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaTimeZoneDefaultZoneRulesProvider() {
        return getProperty(JAVA_TIME_ZONE_DEFAULT_ZONE_RULES_PROVIDER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_EXCEPTION_HANDLER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilConcurrentForkJoinPoolCommonExceptionHandler() {
        return getProperty(JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_EXCEPTION_HANDLER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_MAXIMUM_SPARES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilConcurrentForkJoinPoolCommonMaximumSpares() {
        return getProperty(JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_MAXIMUM_SPARES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_PARALLELISM}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilConcurrentForkJoinPoolCommonParallelism() {
        return getProperty(JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_PARALLELISM);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_THREAD_FACTORY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilConcurrentForkJoinPoolCommonThreadFactory() {
        return getProperty(JAVA_UTIL_CONCURRENT_FORK_JOIN_POOL_COMMON_THREAD_FACTORY);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_CURRENCY_DATA}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilCurrencyData() {
        return getProperty(JAVA_UTIL_CURRENCY_DATA);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_LOGGING_CONFIG_CLASS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilLoggingConfigClass() {
        return getProperty(JAVA_UTIL_LOGGING_CONFIG_CLASS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_LOGGING_CONFIG_FILE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilLoggingConfigFile() {
        return getProperty(JAVA_UTIL_LOGGING_CONFIG_FILE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_LOGGING_SIMPLE_FORMATTER_FORMAT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilLoggingSimpleFormatterFormat() {
        return getProperty(JAVA_UTIL_LOGGING_SIMPLE_FORMATTER_FORMAT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_PREFS_PREFERENCES_FACTORY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaUtilPrefsPreferencesFactory() {
        return getProperty(JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_UTIL_PROPERTY_RESOURCE_BUNDLE_ENCODING}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaUtilPropertyResourceBundleEncoding() {
        return getProperty(JAVA_UTIL_PROPERTY_RESOURCE_BUNDLE_ENCODING);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VENDOR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVendor() {
        return getProperty(JAVA_VENDOR);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VENDOR_URL}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVendorUrl() {
        return getProperty(JAVA_VENDOR_URL);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VENDOR_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaVendorVersion() {
        return getProperty(JAVA_VENDOR_VERSION);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVersion() {
        return getProperty(JAVA_VERSION);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VERSION_DATE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaVersionDate() {
        return getProperty(JAVA_VERSION_DATE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_INFO}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmInfo() {
        return getProperty(JAVA_VM_INFO);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_NAME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmName() {
        return getProperty(JAVA_VM_NAME);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_SPECIFICATION_NAME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmSpecificationName() {
        return getProperty(JAVA_VM_SPECIFICATION_NAME);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_SPECIFICATION_VENDOR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmSpecificationVendor() {
        return getProperty(JAVA_VM_SPECIFICATION_VENDOR);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_SPECIFICATION_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmSpecificationVersion() {
        return getProperty(JAVA_VM_SPECIFICATION_VERSION);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_VENDOR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmVendor() {
        return getProperty(JAVA_VM_VENDOR);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_VM_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getJavaVmVersion() {
        return getProperty(JAVA_VM_VERSION);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaxAccessibilityAssistiveTechnologies() {
        return getProperty(JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVA_XML_CONFIG_FILE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaXmlConfigFile() {
        return getProperty(JAVA_XML_CONFIG_FILE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVAX_NET_SSL_SESSION_CACHE_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaxNetSslSessionCacheSize() {
        return getProperty(JAVAX_NET_SSL_SESSION_CACHE_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVAX_RMI_SSL_CLIENT_ENABLED_CIPHER_SUITES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaxRmiSslClientEnabledCipherSuites() {
        return getProperty(JAVAX_RMI_SSL_CLIENT_ENABLED_CIPHER_SUITES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVAX_RMI_SSL_CLIENT_ENABLED_PROTOCOLS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaxRmiSslClientEnabledProtocols() {
        return getProperty(JAVAX_RMI_SSL_CLIENT_ENABLED_PROTOCOLS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVAX_SECURITY_AUTH_USE_SUBJECT_CREDS_ONLY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaxSecurityAuthUseSubjectCredsOnly() {
        return getProperty(JAVAX_SECURITY_AUTH_USE_SUBJECT_CREDS_ONLY);
    }

    /**
     * Gets the current value from the system properties map for {@value #JAVAX_SMART_CARD_IO_TERMINAL_FACTORY_DEFAULT_TYPE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJavaxSmartCardIoTerminalFactoryDefaultType() {
        return getProperty(JAVAX_SMART_CARD_IO_TERMINAL_FACTORY_DEFAULT_TYPE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDBC_DRIVERS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdbcDrivers() {
        return getProperty(JDBC_DRIVERS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_AUTH_PROXYING_DISABLED_SCHEMES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpAuthProxyingDisabledSchemes() {
        return getProperty(JDK_HTTP_AUTH_PROXYING_DISABLED_SCHEMES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_AUTH_TUNNELING_DISABLED_SCHEMES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpAuthTunnelingDisabledSchemes() {
        return getProperty(JDK_HTTP_AUTH_TUNNELING_DISABLED_SCHEMES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_ALLOW_RESTRICTED_HEADERS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientAllowRestrictedHeaders() {
        return getProperty(JDK_HTTP_CLIENT_ALLOW_RESTRICTED_HEADERS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_AUTH_RETRY_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientAuthRetryLimit() {
        return getProperty(JDK_HTTP_CLIENT_AUTH_RETRY_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_BUF_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientBufSize() {
        return getProperty(JDK_HTTP_CLIENT_BUF_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_CONNECTION_POOL_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientConnectionPoolSize() {
        return getProperty(JDK_HTTP_CLIENT_CONNECTION_POOL_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_CONNECTION_WINDOW_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientConnectionWindowSize() {
        return getProperty(JDK_HTTP_CLIENT_CONNECTION_WINDOW_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_DISABLE_RETRY_CONNECT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientDisableRetryConnect() {
        return getProperty(JDK_HTTP_CLIENT_DISABLE_RETRY_CONNECT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_ENABLE_ALL_METHOD_RETRY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientEnableAllMethodRetry() {
        return getProperty(JDK_HTTP_CLIENT_ENABLE_ALL_METHOD_RETRY);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_ENABLE_PUSH}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientEnablePush() {
        return getProperty(JDK_HTTP_CLIENT_ENABLE_PUSH);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_HPACK_MAX_HEADER_TABLE_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientHpackMaxHeaderTableSize() {
        return getProperty(JDK_HTTP_CLIENT_HPACK_MAX_HEADER_TABLE_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_HTTP_CLIENT_LOG}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientHttpClientLog() {
        return getProperty(JDK_HTTP_CLIENT_HTTP_CLIENT_LOG);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientKeepAliveTimeout() {
        return getProperty(JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT_H2}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientKeepAliveTimeoutH2() {
        return getProperty(JDK_HTTP_CLIENT_KEEP_ALIVE_TIMEOUT_H2);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_MAX_FRAME_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientMaxFrameSize() {
        return getProperty(JDK_HTTP_CLIENT_MAX_FRAME_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_MAX_STREAMS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientMaxStreams() {
        return getProperty(JDK_HTTP_CLIENT_MAX_STREAMS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_RECEIVE_BUFFER_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientReceiveBufferSize() {
        return getProperty(JDK_HTTP_CLIENT_RECEIVE_BUFFER_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_REDIRECTS_RETRY_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientRedirectsRetryLimit() {
        return getProperty(JDK_HTTP_CLIENT_REDIRECTS_RETRY_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_SEND_BUFFER_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientSendBufferSize() {
        return getProperty(JDK_HTTP_CLIENT_SEND_BUFFER_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_WEB_SOCKET_WRITE_BUFFER_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientWebSocketWriteBufferSize() {
        return getProperty(JDK_HTTP_CLIENT_WEB_SOCKET_WRITE_BUFFER_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_CLIENT_WINDOW_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpClientWindowSize() {
        return getProperty(JDK_HTTP_CLIENT_WINDOW_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTP_SERVER_MAX_CONNECTIONS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpServerMaxConnections() {
        return getProperty(JDK_HTTP_SERVER_MAX_CONNECTIONS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_HTTPS_NEGOTIATE_CBT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkHttpsNegotiateCbt() {
        return getProperty(JDK_HTTPS_NEGOTIATE_CBT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_INCLUDE_IN_EXCEPTIONS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkIncludeInExceptions() {
        return getProperty(JDK_INCLUDE_IN_EXCEPTIONS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_INTERNAL_HTTP_CLIENT_DISABLE_HOST_NAME_VERIFICATION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkInternalHttpClientDisableHostNameVerification() {
        return getProperty(JDK_INTERNAL_HTTP_CLIENT_DISABLE_HOST_NAME_VERIFICATION);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_IO_PERMISSIONS_USE_CANONICAL_PATH}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkIoPermissionsUseCanonicalPath() {
        return getProperty(JDK_IO_PERMISSIONS_USE_CANONICAL_PATH);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_JNDI_LDAP_OBJECT_FACTORIES_FILTER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkJndiLdapObjectFactoriesFilter() {
        return getProperty(JDK_JNDI_LDAP_OBJECT_FACTORIES_FILTER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_JNDI_OBJECT_FACTORIES_FILTER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkJndiObjectFactoriesFilter() {
        return getProperty(JDK_JNDI_OBJECT_FACTORIES_FILTER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_JNDI_RMI_OBJECT_FACTORIES_FILTER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkJndiRmiObjectFactoriesFilter() {
        return getProperty(JDK_JNDI_RMI_OBJECT_FACTORIES_FILTER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_MODULE_MAIN}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkModuleMain() {
        return getProperty(JDK_MODULE_MAIN);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_MODULE_MAIN_CLASS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkModuleMainClass() {
        return getProperty(JDK_MODULE_MAIN_CLASS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_MODULE_PATH}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkModulePath() {
        return getProperty(JDK_MODULE_PATH);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_MODULE_UPGRADE_PATH}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkModuleUpgradePath() {
        return getProperty(JDK_MODULE_UPGRADE_PATH);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_NET_UNIX_DOMAIN_TMPDIR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkNetUnixDomainTmpDir() {
        return getProperty(JDK_NET_UNIX_DOMAIN_TMPDIR);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_NET_URL_CLASS_PATH_SHOW_IGNORED_CLASS_PATH_ENTRIES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkNetUrlClassPathShowIgnoredClassPathEntries() {
        return getProperty(JDK_NET_URL_CLASS_PATH_SHOW_IGNORED_CLASS_PATH_ENTRIES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_SERIAL_FILTER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkSerialFilter() {
        return getProperty(JDK_SERIAL_FILTER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_SERIAL_FILTER_FACTORY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkSerialFilterFactory() {
        return getProperty(JDK_SERIAL_FILTER_FACTORY);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_TLS_CLIENT_SIGNATURE_SCHEMES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkTlsClientSignatureSchemes() {
        return getProperty(JDK_TLS_CLIENT_SIGNATURE_SCHEMES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_TLS_NAMED_GROUPS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkTlsNamedGroups() {
        return getProperty(JDK_TLS_NAMED_GROUPS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_TLS_SERVER_SIGNATURE_SCHEMES}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkTlsServerSignatureSchemes() {
        return getProperty(JDK_TLS_SERVER_SIGNATURE_SCHEMES);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_VIRTUAL_THREAD_SCHEDULER_MAXPOOLSIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkVirtualThreadSchedulerMaxPoolSize() {
        return getProperty(JDK_VIRTUAL_THREAD_SCHEDULER_MAXPOOLSIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_VIRTUAL_THREAD_SCHEDULER_PARALLELISM}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkVirtualThreadSchedulerParallelism() {
        return getProperty(JDK_VIRTUAL_THREAD_SCHEDULER_PARALLELISM);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_CDATA_CHUNK_SIZE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlCdataChunkSize() {
        return getProperty(JDK_XML_CDATA_CHUNK_SIZE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_DTD_SUPPORT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlDtdSupport() {
        return getProperty(JDK_XML_DTD_SUPPORT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_ELEMENT_ATTRIBUTE_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlElementAttributeLimit() {
        return getProperty(JDK_XML_ELEMENT_ATTRIBUTE_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_ENABLE_EXTENSION_FUNCTIONS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlEnableExtensionFunctions() {
        return getProperty(JDK_XML_ENABLE_EXTENSION_FUNCTIONS);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_ENTITY_EXPANSION_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlEntityExpansionLimit() {
        return getProperty(JDK_XML_ENTITY_EXPANSION_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_ENTITY_REPLACEMENT_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlEntityReplacementLimit() {
        return getProperty(JDK_XML_ENTITY_REPLACEMENT_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_IS_STANDALONE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlIsStandalone() {
        return getProperty(JDK_XML_IS_STANDALONE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_JDK_CATALOG_RESOLVE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlJdkCatalogResolve() {
        return getProperty(JDK_XML_JDK_CATALOG_RESOLVE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_MAX_ELEMENT_DEPTH}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlMaxElementDepth() {
        return getProperty(JDK_XML_MAX_ELEMENT_DEPTH);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_MAX_GENERAL_ENTITY_SIZE_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlMaxGeneralEntitySizeLimit() {
        return getProperty(JDK_XML_MAX_GENERAL_ENTITY_SIZE_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_MAX_OCCUR_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlMaxOccurLimit() {
        return getProperty(JDK_XML_MAX_OCCUR_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_MAX_PARAMETER_ENTITY_SIZE_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlMaxParameterEntitySizeLimit() {
        return getProperty(JDK_XML_MAX_PARAMETER_ENTITY_SIZE_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_MAX_XML_NAME_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlMaxXmlNameLimit() {
        return getProperty(JDK_XML_MAX_XML_NAME_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_OVERRIDE_DEFAULT_PARSER}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlOverrideDefaultParser() {
        return getProperty(JDK_XML_OVERRIDE_DEFAULT_PARSER);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_RESET_SYMBOL_TABLE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlResetSymbolTable() {
        return getProperty(JDK_XML_RESET_SYMBOL_TABLE);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_TOTAL_ENTITY_SIZE_LIMIT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlTotalEntitySizeLimit() {
        return getProperty(JDK_XML_TOTAL_ENTITY_SIZE_LIMIT);
    }

    /**
     * Gets the current value from the system properties map for {@value #JDK_XML_XSLTC_IS_STANDALONE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getJdkXmlXsltcIsStandalone() {
        return getProperty(JDK_XML_XSLTC_IS_STANDALONE);
    }

    /**
     * Gets the current value from the system properties map for {@value #LINE_SEPARATOR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getLineSeparator() {
        return getProperty(LINE_SEPARATOR);
    }

    /**
     * Gets the current value from the system properties map for {@value #LINE_SEPARATOR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @param defaultIfAbsent get this Supplier when the property is empty or throws SecurityException.
     * @return the current value from the system properties map.
     * @since 3.15.0
     */
    public static String getLineSeparator(final Supplier<String> defaultIfAbsent) {
        return getProperty(LINE_SEPARATOR, defaultIfAbsent);
    }

    /**
     * Gets the current value for the property named {@code "SimpleClassName.Key"} as a {@code long}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param clazz           The Class to use for the SimpleClassName.
     * @param key             The subkey.
     * @param defaultIfAbsent The default value.
     * @return a long or {@code defaultIfAbsent}'s value.
     * @see Class#getSimpleName()
     * @since 3.19.0
     */
    public static long getLong(final Class<?> clazz, final String key, final LongSupplier defaultIfAbsent) {
        return getLong(toKey(clazz, key, true), defaultIfAbsent);
    }

    /**
     * Gets the current value for the property named {@code key} as a {@code long}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param key             The key.
     * @param defaultIfAbsent The default value.
     * @return a {@code long} or {@code defaultIfAbsent}'s value.
     */
    public static long getLong(final String key, final LongSupplier defaultIfAbsent) {
        final String str = getProperty(key);
        return str == null ? defaultIfAbsent != null ? defaultIfAbsent.getAsLong() : 0 : Long.parseLong(str);
    }

    /**
     * Gets the current value from the system properties map for {@value #NATIVE_ENCODING}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getNativeEncoding() {
        return getProperty(NATIVE_ENCODING);
    }

    /**
     * Gets the current value from the system properties map for {@value #NETWORK_ADDRESS_CACHE_NEGATIVE_TTL}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getNetworkAddressCacheNegativeTtl() {
        return getProperty(NETWORK_ADDRESS_CACHE_NEGATIVE_TTL);
    }

    /**
     * Gets the current value from the system properties map for {@value #NETWORK_ADDRESS_CACHE_STALE_TTL}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getNetworkAddressCacheStaleTtl() {
        return getProperty(NETWORK_ADDRESS_CACHE_STALE_TTL);
    }

    /**
     * Gets the current value from the system properties map for {@value #NETWORK_ADDRESS_CACHE_TTL}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getNetworkAddressCacheTtl() {
        return getProperty(NETWORK_ADDRESS_CACHE_TTL);
    }

    /**
     * Gets the current value from the system properties map for {@value #ORG_JCP_XML_DSIG_SECURE_VALIDATION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getOrgJcpXmlDsigSecureValidation() {
        return getProperty(ORG_JCP_XML_DSIG_SECURE_VALIDATION);
    }

    /**
     * Gets the current value from the system properties map for {@value #ORG_OPENJDK_JAVA_UTIL_STREAM_TRIPWIRE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getOrgOpenJdkJavaUtilStreamTripwire() {
        return getProperty(ORG_OPENJDK_JAVA_UTIL_STREAM_TRIPWIRE);
    }

    /**
     * Gets the current value from the system properties map for {@value #OS_ARCH}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getOsArch() {
        return getProperty(OS_ARCH);
    }

    /**
     * Gets the current value from the system properties map for {@value #OS_NAME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getOsName() {
        return getProperty(OS_NAME);
    }

    /**
     * Gets the current value from the system properties map for {@value #OS_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getOsVersion() {
        return getProperty(OS_VERSION);
    }

    /**
     * Gets the current value for the property named {@code key} as a {@link Path}.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param key             The key.
     * @param defaultIfAbsent The default value.
     * @return a {@link Path} or {@code defaultIfAbsent}'s value.
     * @since 3.20.0
     */
    public static Path getPath(final String key, final Supplier<Path> defaultIfAbsent) {
        final String str = getProperty(key);
        return str == null ? defaultIfAbsent != null ? defaultIfAbsent.get() : null : Paths.get(str);
    }

    /**
     * Gets the current value from the system properties map for {@value #PATH_SEPARATOR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getPathSeparator() {
        return getProperty(PATH_SEPARATOR);
    }

    /**
     * Gets a System property, defaulting to {@code null} if the property cannot be read.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param property The system property name.
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getProperty(final String property) {
        return getProperty(property, Suppliers.nul());
    }

    /**
     * Gets a System property, defaulting to {@code null} if the property cannot be read.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param property        the system property name.
     * @param defaultIfAbsent use this value when the property is empty or throws SecurityException.
     * @return the system property value or {@code null} if a security problem occurs.
     */
    static String getProperty(final String property, final String defaultIfAbsent) {
        return getProperty(property, () -> defaultIfAbsent);
    }

    /**
     * Gets a System property, defaulting to {@code null} if the property cannot be read.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param property        the system property name.
     * @param defaultIfAbsent get this Supplier when the property is empty or throws SecurityException.
     * @return the system property value or {@code null} if a security problem occurs.
     */
    static String getProperty(final String property, final Supplier<String> defaultIfAbsent) {
        try {
            if (StringUtils.isEmpty(property)) {
                return Suppliers.get(defaultIfAbsent);
            }
            return StringUtils.getIfEmpty(System.getProperty(property), defaultIfAbsent);
        } catch (final SecurityException ignore) {
            // We are not allowed to look at this property.
            //
            // System.err.println("Caught a SecurityException reading the system property '" + property
            // + "'; the SystemUtils property value will default to null.");
            return defaultIfAbsent.get();
        }
    }

    /**
     * Gets the current value from the system properties map for {@value #SOCKS_PROXY_HOST}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSocksProxyHost() {
        return getProperty(SOCKS_PROXY_HOST);
    }

    /**
     * Gets the current value from the system properties map for {@value #SOCKS_PROXY_PORT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSocksProxyPort() {
        return getProperty(SOCKS_PROXY_PORT);
    }

    /**
     * Gets the current value from the system properties map for {@value #SOCKS_PROXY_VERSION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSocksProxyVersion() {
        return getProperty(SOCKS_PROXY_VERSION);
    }

    /**
     * Gets the current value from the system properties map for {@value #STDERR_ENCODING}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getStdErrEncoding() {
        return getProperty(STDERR_ENCODING);
    }

    /**
     * Gets the current value from the system properties map for {@value #STDOUT_ENCODING}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getStdOutEncoding() {
        return getProperty(STDOUT_ENCODING);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_DRAIN_AMOUNT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerDrainAmount() {
        return getProperty(SUN_NET_HTTP_SERVER_DRAIN_AMOUNT);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_IDLE_INTERVAL}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerIdleInterval() {
        return getProperty(SUN_NET_HTTP_SERVER_IDLE_INTERVAL);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_MAX_IDLE_CONNECTIONS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerMaxIdleConnections() {
        return getProperty(SUN_NET_HTTP_SERVER_MAX_IDLE_CONNECTIONS);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_MAX_REQ_HEADERS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerMaxReqHeaders() {
        return getProperty(SUN_NET_HTTP_SERVER_MAX_REQ_HEADERS);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_MAX_REQ_TIME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerMaxReqTime() {
        return getProperty(SUN_NET_HTTP_SERVER_MAX_REQ_TIME);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_MAX_RSP_TIME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerMaxRspTime() {
        return getProperty(SUN_NET_HTTP_SERVER_MAX_RSP_TIME);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_NET_HTTP_SERVER_NO_DELAY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunNetHttpServerNoDelay() {
        return getProperty(SUN_NET_HTTP_SERVER_NO_DELAY);
    }

    /**
     * Gets the current value from the system properties map for {@value #SUN_SECURITY_KRB5_PRINCIPAL}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getSunSecurityKrb5Principal() {
        return getProperty(SUN_SECURITY_KRB5_PRINCIPAL);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_COUNTRY}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getUserCountry() {
        return getProperty(USER_COUNTRY);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_DIR}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getUserDir() {
        return getProperty(USER_DIR);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_EXTENSIONS}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getUserExtensions() {
        return getProperty(USER_EXTENSIONS);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_HOME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getUserHome() {
        return getProperty(USER_HOME);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_LANGUAGE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getUserLanguage() {
        return getProperty(USER_LANGUAGE);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_NAME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getUserName() {
        return getProperty(USER_NAME);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_NAME}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @param defaultValue get this Supplier when the property is empty or throws SecurityException.
     * @return The system property value or {@code defaultValue} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getUserName(final String defaultValue) {
        return getProperty(USER_NAME, defaultValue);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_REGION}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getUserRegion() {
        return getProperty(USER_REGION);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_SCRIPT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getUserScript() {
        return getProperty(USER_SCRIPT);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_TIMEZONE}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     */
    public static String getUserTimezone() {
        return getProperty(USER_TIMEZONE);
    }

    /**
     * Gets the current value from the system properties map for {@value #USER_VARIANT}.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return The system property value or {@code null} if the property is absent or a security problem occurs.
     * @since 3.15.0
     */
    public static String getUserVariant() {
        return getProperty(USER_VARIANT);
    }

    /**
     * Tests whether the given property is set.
     * <p>
     * Short-hand for {@code getProperty(property) != null}.
     * </p>
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code false}.
     * </p>
     *
     * @param property the system property name.
     * @return whether the given property is set.
     * @since 3.18.0
     */
    public static boolean isPropertySet(final String property) {
        return getProperty(property) != null;
    }

    private static String toKey(final Class<?> clazz, final String key, final boolean simpleKey) {
        return ClassUtils.getName(clazz, StringUtils.EMPTY, simpleKey) + "." + key;
    }

    /**
     * Make private in 4.0.
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public SystemProperties() {
        // empty
    }
}
