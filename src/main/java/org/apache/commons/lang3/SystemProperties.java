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

import java.util.function.BooleanSupplier;
import java.util.function.IntSupplier;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

import org.apache.commons.lang3.function.Suppliers;

/**
 * Accesses current system property names and values.
 *
 * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
 * @since 3.13.0
 */
public final class SystemProperties {

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.desktop/java/awt/TrayIcon.html#apple.awt.enableTemplateImages">TrayIcon</a>
     * @since 3.15.0
     */
    public static final String APPLE_AWT_ENABLETEMPLATEIMAGES = "apple.awt.enableTemplateImages";

    /**
     * The System property name {@value}.
     */
    public static final String AWT_TOOLKIT = "awt.toolkit";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/22/docs/api/java.naming/module-summary.html#com.sun.jndi.ldap.object.trustSerialData">java.naming</a>
     * @since 3.15.0
     */
    public static final String COM_SUN_JNDI_LDAP_OBJECT_TRUSTSERIALDATA = "com.sun.jndi.ldap.object.trustSerialData";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/22/docs/api/jdk.httpserver/com/sun/net/httpserver/spi/HttpServerProvider.html#com.sun.net.httpserver.HttpServerProvider">HttpServerProvider</a>
     * @since 3.15.0
     */
    public static final String COM_SUN_NET_HTTPSERVER_HTTPSERVERPROVIDER = "com.sun.net.httpserver.HttpServerProvider";

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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#ftp.nonProxyHosts">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String FTP_NONPROXYHOST = "ftp.nonProxyHosts";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#ftp.proxyHost">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String FTP_PROXYHOST = "ftp.proxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#ftp.proxyPort">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String FTP_PROXYPORT = "ftp.proxyPort";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.agent">Networking Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_AGENT = "http.agent";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#auth.digest.cnonceRepeat">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_CNONCEREPEAT = "http.auth.digest.cnonceRepeat";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#reEnabledAlgorithms">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_REENABLEDALGORITHMS = "http.auth.digest.reEnabledAlgorithms";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.auth.digest.validateProxy">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_VALIDATEPROXY = "http.auth.digest.validateProxy";

    /**
     * The System property name {@value}.
     *
     * @see <a href=
     *      "https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.auth.digest.validateServer">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_DIGEST_VALIDATESERVER = "http.auth.digest.validateServer";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.auth.ntlm.domain">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_AUTH_NTLM_DOMAIN = "http.auth.ntlm.domain";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.keepAlive">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_KEEPALIVE = "http.keepAlive";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.keepAlive.time.proxy">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_KEEPALIVE_TIME_PROXY = "http.keepAlive.time.proxy";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.keepAlive.time.server">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_KEEPALIVE_TIME_SERVER = "http.keepAlive.time.server";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.maxConnections">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_MAXCONNECTIONS = "http.maxConnections";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.maxRedirects">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_MAXREDIRECTS = "http.maxRedirects";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.nonProxyHosts">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_NONPROXYHOSTS = "http.nonProxyHosts";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.proxyHost">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_PROXYHOST = "http.proxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#http.proxyPort">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTP_PROXYPORT = "http.proxyPort";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#https.proxyHost">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTPS_PROXYHOST = "https.proxyHost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.base/java/net/doc-files/net-properties.html#https.proxyPort">Networking
     *      Properties</a>
     * @since 3.15.0
     */
    public static final String HTTPS_PROXYPORT = "https.proxyPort";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_FONTS = "java.awt.fonts";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_GRAPHICSENV = "java.awt.graphicsenv";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_HEADLESS = "java.awt.headless";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_PRINTERJOB = "java.awt.printerjob";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_CLASS_PATH = "java.class.path";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_CLASS_VERSION = "java.class.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_COMPILER = "java.compiler";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_CONTENT_HANDLER_PKGS = "java.content.handler.pkgs";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_ENDORSED_DIRS = "java.endorsed.dirs";

    /**
     * The System property name {@value}.
     */
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_LOCALE_USEOLDISOCODES = "java.locale.useoldisocodes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_PREFERIPV4STACK = "java.net.preferipv4stack";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_PREFERIPV6ADDRESSES = "java.net.preferipv6addresses";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_SOCKS_PASSWORD = "java.net.socks.password";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_SOCKS_USERNAME = "java.net.socks.username";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NET_USESYSTEMPROXIES = "java.net.usesystemproxies";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_DEFAULTTHREADPOOL_INITIALSIZE = "java.nio.channels.defaultthreadpool.initialsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_DEFAULTTHREADPOOL_THREADFACTORY = "java.nio.channels.defaultthreadpool.threadfactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_SPI_ASYNCHRONOUSCHANNELPROVIDER = "java.nio.channels.spi.asynchronouschannelprovider";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_CHANNELS_SPI_SELECTORPROVIDER = "java.nio.channels.spi.selectorprovider";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_NIO_FILE_SPI_DEFAULTFILESYSTEMPROVIDER = "java.nio.file.spi.defaultfilesystemprovider";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_PROPERTIES_DATE = "java.properties.date";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_PROTOCOL_HANDLER_PKGS = "java.protocol.handler.pkgs";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_CODEBASE = "java.rmi.server.codebase";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_HOSTNAME = "java.rmi.server.hostname";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_RANDOMIDS = "java.rmi.server.randomids";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_RMI_SERVER_RMICLASSLOADERSPI = "java.rmi.server.rmiclassloaderspi";

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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_SECURITY_AUTH_LOGIN_CONFIG = "java.security.auth.login.config";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_SECURITY_MANAGER = "java.security.manager";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_SYSTEM_CLASS_LOADER = "java.system.class.loader";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_TIME_ZONE_DEFAULTZONERULESPROVIDER = "java.time.zone.defaultzonerulesprovider";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_EXCEPTIONHANDLER = "java.util.concurrent.forkjoinpool.common.exceptionhandler";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_MAXIMUMSPARES = "java.util.concurrent.forkjoinpool.common.maximumspares";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_PARALLELISM = "java.util.concurrent.forkjoinpool.common.parallelism";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CONCURRENT_FORKJOINPOOL_COMMON_THREADFACTORY = "java.util.concurrent.forkjoinpool.common.threadfactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_CURRENCY_DATA = "java.util.currency.data";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_LOGGING_CONFIG_CLASS = "java.util.logging.config.class";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_LOGGING_CONFIG_FILE = "java.util.logging.config.file";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_LOGGING_SIMPLEFORMATTER_FORMAT = "java.util.logging.simpleformatter.format";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_UTIL_PREFS_PREFERENCES_FACTORY = "java.util.prefs.PreferencesFactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_PREFS_PREFERENCESFACTORY = "java.util.prefs.preferencesfactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVA_UTIL_PROPERTYRESOURCEBUNDLE_ENCODING = "java.util.propertyresourcebundle.encoding";

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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/java.xml/module-summary.html#java.xml.config.file">java.xml</a>
     * @since 3.15.0
     */
    public static final String JAVA_XML_CONFIG_FILE = "java.xml.config.file";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVAX_ACCESSIBILITY_ASSISTIVE_TECHNOLOGIES = "javax.accessibility.assistive_technologies";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVAX_NET_SSL_SESSIONCACHESIZE = "javax.net.ssl.sessioncachesize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVAX_RMI_SSL_CLIENT_ENABLEDCIPHERSUITES = "javax.rmi.ssl.client.enabledciphersuites";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVAX_RMI_SSL_CLIENT_ENABLEDPROTOCOLS = "javax.rmi.ssl.client.enabledprotocols";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVAX_SECURITY_AUTH_USESUBJECTCREDSONLY = "javax.security.auth.usesubjectcredsonly";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JAVAX_SMARTCARDIO_TERMINALFACTORY_DEFAULTTYPE = "javax.smartcardio.terminalfactory.defaulttype";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDBC_DRIVERS = "jdbc.drivers";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_AUTH_PROXYING_DISABLEDSCHEMES = "jdk.http.auth.proxying.disabledschemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTP_AUTH_TUNNELING_DISABLEDSCHEMES = "jdk.http.auth.tunneling.disabledschemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_ALLOWRESTRICTEDHEADERS = "jdk.httpclient.allowrestrictedheaders";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_AUTH_RETRYLIMIT = "jdk.httpclient.auth.retrylimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_BUFSIZE = "jdk.httpclient.bufsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_CONNECTIONPOOLSIZE = "jdk.httpclient.connectionpoolsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_CONNECTIONWINDOWSIZE = "jdk.httpclient.connectionwindowsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_DISABLERETRYCONNECT = "jdk.httpclient.disableretryconnect";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_ENABLEALLMETHODRETRY = "jdk.httpclient.enableallmethodretry";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_ENABLEPUSH = "jdk.httpclient.enablepush";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_HPACK_MAXHEADERTABLESIZE = "jdk.httpclient.hpack.maxheadertablesize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_HTTPCLIENT_LOG = "jdk.httpclient.httpclient.log";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_KEEPALIVE_TIMEOUT = "jdk.httpclient.keepalive.timeout";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_KEEPALIVE_TIMEOUT_H2 = "jdk.httpclient.keepalive.timeout.h2";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_MAXFRAMESIZE = "jdk.httpclient.maxframesize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_MAXSTREAMS = "jdk.httpclient.maxstreams";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_RECEIVEBUFFERSIZE = "jdk.httpclient.receivebuffersize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_REDIRECTS_RETRYLIMIT = "jdk.httpclient.redirects.retrylimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_SENDBUFFERSIZE = "jdk.httpclient.sendbuffersize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_WEBSOCKET_WRITEBUFFERSIZE = "jdk.httpclient.websocket.writebuffersize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPCLIENT_WINDOWSIZE = "jdk.httpclient.windowsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPS_NEGOTIATE_CBT = "jdk.https.negotiate.cbt";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_HTTPSERVER_MAXCONNECTIONS = "jdk.httpserver.maxconnections";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_INCLUDEINEXCEPTIONS = "jdk.includeinexceptions";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_INTERNAL_HTTPCLIENT_DISABLEHOSTNAMEVERIFICATION = "jdk.internal.httpclient.disablehostnameverification";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_IO_PERMISSIONSUSECANONICALPATH = "jdk.io.permissionsusecanonicalpath";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_JNDI_LDAP_OBJECT_FACTORIESFILTER = "jdk.jndi.ldap.object.factoriesfilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_JNDI_OBJECT_FACTORIESFILTER = "jdk.jndi.object.factoriesfilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_JNDI_RMI_OBJECT_FACTORIESFILTER = "jdk.jndi.rmi.object.factoriesfilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_MAIN = "jdk.module.main";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_MAIN_CLASS = "jdk.module.main.class";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_PATH = "jdk.module.path";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_MODULE_UPGRADE_PATH = "jdk.module.upgrade.path";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_NET_UNIXDOMAIN_TMPDIR = "jdk.net.unixdomain.tmpdir";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_NET_URLCLASSPATH_SHOWIGNOREDCLASSPATHENTRIES = "jdk.net.urlclasspath.showignoredclasspathentries";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_SERIALFILTER = "jdk.serialfilter";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_SERIALFILTERFACTORY = "jdk.serialfilterfactory";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_TLS_CLIENT_SIGNATURESCHEMES = "jdk.tls.client.signatureschemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_TLS_NAMEDGROUPS = "jdk.tls.namedgroups";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_TLS_SERVER_SIGNATURESCHEMES = "jdk.tls.server.signatureschemes";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_VIRTUALTHREADSCHEDULER_MAXPOOLSIZE = "jdk.virtualthreadscheduler.maxpoolsize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_VIRTUALTHREADSCHEDULER_PARALLELISM = "jdk.virtualthreadscheduler.parallelism";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_CDATACHUNKSIZE = "jdk.xml.cdatachunksize";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_DTD_SUPPORT = "jdk.xml.dtd.support";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ELEMENTATTRIBUTELIMIT = "jdk.xml.elementattributelimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ENABLEEXTENSIONFUNCTIONS = "jdk.xml.enableextensionfunctions";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ENTITYEXPANSIONLIMIT = "jdk.xml.entityexpansionlimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ENTITYREPLACEMENTLIMIT = "jdk.xml.entityreplacementlimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_ISSTANDALONE = "jdk.xml.isstandalone";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_JDKCATALOG_RESOLVE = "jdk.xml.jdkcatalog.resolve";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAXELEMENTDEPTH = "jdk.xml.maxelementdepth";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAXGENERALENTITYSIZELIMIT = "jdk.xml.maxgeneralentitysizelimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAXOCCURLIMIT = "jdk.xml.maxoccurlimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAXPARAMETERENTITYSIZELIMIT = "jdk.xml.maxparameterentitysizelimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_MAXXMLNAMELIMIT = "jdk.xml.maxxmlnamelimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_OVERRIDEDEFAULTPARSER = "jdk.xml.overridedefaultparser";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_RESETSYMBOLTABLE = "jdk.xml.resetsymboltable";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_TOTALENTITYSIZELIMIT = "jdk.xml.totalentitysizelimit";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String JDK_XML_XSLTCISSTANDALONE = "jdk.xml.xsltcisstandalone";

    /**
     * The System property name {@value}.
     */
    public static final String LINE_SEPARATOR = "line.separator";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String NATIVE_ENCODING = "native.encoding";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String NETWORKADDRESS_CACHE_NEGATIVE_TTL = "networkaddress.cache.negative.ttl";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String NETWORKADDRESS_CACHE_STALE_TTL = "networkaddress.cache.stale.ttl";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String NETWORKADDRESS_CACHE_TTL = "networkaddress.cache.ttl";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String ORG_JCP_XML_DSIG_SECUREVALIDATION = "org.jcp.xml.dsig.securevalidation";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SOCKSPROXYHOST = "socksproxyhost";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SOCKSPROXYPORT = "socksproxyport";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SOCKSPROXYVERSION = "socksproxyversion";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String STDERR_ENCODING = "stderr.encoding";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String STDOUT_ENCODING = "stdout.encoding";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_DRAINAMOUNT = "sun.net.httpserver.drainamount";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_IDLEINTERVAL = "sun.net.httpserver.idleinterval";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_MAXIDLECONNECTIONS = "sun.net.httpserver.maxidleconnections";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_MAXREQHEADERS = "sun.net.httpserver.maxreqheaders";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_MAXREQTIME = "sun.net.httpserver.maxreqtime";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_MAXRSPTIME = "sun.net.httpserver.maxrsptime";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String SUN_NET_HTTPSERVER_NODELAY = "sun.net.httpserver.nodelay";

    /**
     * The System property name {@value}.
     *
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
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
     * @see <a href="https://docs.oracle.com/en/java/javase/22/docs/api/system-properties.html">System Properties</a>
     * @since 3.15.0
     */
    public static final String USER_VARIANT = "user.variant";

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getAwtToolkit() {
        return getProperty(AWT_TOOLKIT);
    }

    /**
     * Gets the current value for the property named {@code key} as an {@code boolean}.
     *
     * @param key             The key
     * @param defaultIfAbsent The default value
     * @return an {@code boolean} or defaultIfAbsent
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
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
     */
    public static String getFileSeparator() {
        return getProperty(FILE_SEPARATOR);
    }

    /**
     * Gets the current value for the property named {@code key} as an {@code int}.
     *
     * @param key             The key
     * @param defaultIfAbsent The default value
     * @return an {@code int} or defaultIfAbsent
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
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtFonts() {
        return getProperty(JAVA_AWT_FONTS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtGraphicsenv() {
        return getProperty(JAVA_AWT_GRAPHICSENV);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtHeadless() {
        return getProperty(JAVA_AWT_HEADLESS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtPrinterjob() {
        return getProperty(JAVA_AWT_PRINTERJOB);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
     */
    public static String getJavaCompiler() {
        return getProperty(JAVA_COMPILER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaEndorsedDirs() {
        return getProperty(JAVA_ENDORSED_DIRS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaExtDirs() {
        return getProperty(JAVA_EXT_DIRS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
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
     * @return the current value from the system properties map.
     */
    public static String getJavaSpecificationVersion() {
        return getProperty(JAVA_SPECIFICATION_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @param defaultValue get this Supplier when the property is empty or throws SecurityException.
     * @return the current value from the system properties map.
     * @since 3.15.0
     */
    public static String getJavaSpecificationVersion(final Supplier<String> defaultValue) {
        return getProperty(JAVA_SPECIFICATION_VERSION, defaultValue);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaUtilPrefsPreferencesFactory() {
        return getProperty(JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVendor() {
        return getProperty(JAVA_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVendorUrl() {
        return getProperty(JAVA_VENDOR_URL);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVersion() {
        return getProperty(JAVA_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmInfo() {
        return getProperty(JAVA_VM_INFO);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmName() {
        return getProperty(JAVA_VM_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmSpecificationName() {
        return getProperty(JAVA_VM_SPECIFICATION_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmSpecificationVendor() {
        return getProperty(JAVA_VM_SPECIFICATION_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmSpecificationVersion() {
        return getProperty(JAVA_VM_SPECIFICATION_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmVendor() {
        return getProperty(JAVA_VM_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmVersion() {
        return getProperty(JAVA_VM_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getLineSeparator() {
        return getProperty(LINE_SEPARATOR);
    }

    /**
     * Gets the current value from the system properties map.
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
     * Gets the current value for the property named {@code key} as a {@code long}.
     *
     * @param key             The key
     * @param defaultIfAbsent The default value
     * @return a {@code long} or defaultIfAbsent
     */
    public static long getLong(final String key, final LongSupplier defaultIfAbsent) {
        final String str = getProperty(key);
        return str == null ? defaultIfAbsent != null ? defaultIfAbsent.getAsLong() : 0 : Long.parseLong(str);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getOsArch() {
        return getProperty(OS_ARCH);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getOsName() {
        return getProperty(OS_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getOsVersion() {
        return getProperty(OS_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
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
     * @param property the system property name
     * @return the system property value or {@code null} if a security problem occurs
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
     * @param defaultIfAbsent get this Supplier when the property is empty or throws SecurityException.
     * @return the system property value or {@code null} if a security problem occurs
     */
    static String getProperty(final String property, final Supplier<String> defaultIfAbsent) {
        try {
            if (StringUtils.isEmpty(property)) {
                return defaultIfAbsent.get();
            }
            final String value = System.getProperty(property);
            return StringUtils.getIfEmpty(value, defaultIfAbsent);
        } catch (final SecurityException ignore) {
            // We are not allowed to look at this property.
            //
            // System.err.println("Caught a SecurityException reading the system property '" + property
            // + "'; the SystemUtils property value will default to null.");
            return defaultIfAbsent.get();
        }
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserCountry() {
        return getProperty(USER_COUNTRY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserDir() {
        return getProperty(USER_DIR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserHome() {
        return getProperty(USER_HOME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserLanguage() {
        return getProperty(USER_LANGUAGE);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserName() {
        return getProperty(USER_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @param defaultValue get this Supplier when the property is empty or throws SecurityException.
     * @return the current value from the system properties map.
     * @since 3.15.0
     */
    public static String getUserName(final Supplier<String> defaultValue) {
        return getProperty(USER_NAME, defaultValue);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserTimezone() {
        return getProperty(USER_TIMEZONE);
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
