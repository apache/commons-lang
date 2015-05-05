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
/**
 * <p>Provides support classes for multi-threaded programming.
 * This package is intended to be an extension to {@link java.util.concurrent}.
 * These classes are thread-safe.</p>
 *
 * <h3></h3>
 *
 * <p> A group of classes deals with the correct creation and initialization of objects that are accessed by multiple threads.
 * All these classes implement the {@link org.apache.commons.lang3.concurrent.ConcurrentInitializer} interface which provides just a
 * single method:
 * </p>
 *
 * <pre>
 * <code>
 * public interface ConcurrentInitializer&lt;T&gt; {
 *    T get() throws ConcurrentException;
 * }
 * </code>
 * </pre>
 *
 * <p>A <code>ConcurrentInitializer</code> produces an object.
 * By calling the {@link org.apache.commons.lang3.concurrent.ConcurrentInitializer#get() get()} method the object managed by the initializer can be obtained.
 * There are different implementations of the interface available
 * addressing various use cases:
 * </p>
 *
 * <p> {@link org.apache.commons.lang3.concurrent.ConstantInitializer} is a very straightforward implementation of the <code>ConcurrentInitializer</code> interface:
 * An instance is passed an object when it is constructed.
 * In its <code>get()</code> method it simply returns this object.
 * This is useful, for instance in unit tests or in cases when you want to pass a specific object to a component which expects a <code>ConcurrentInitializer</code>.
 * </p>
 *
 * <p>The {@link org.apache.commons.lang3.concurrent.LazyInitializer} class can be used to defer the creation of an object until it is actually used.
 * This makes sense, for instance, if the creation of the object is expensive and would slow down application startup or if the object is needed only for special executions.
 * <code>LazyInitializer</code> implements the <em>double-check idiom for an instance field</em> as discussed in Joshua Bloch's "Effective Java", 2nd edition, item 71.
 * It uses <strong>volatile</strong> fields to reduce the amount of synchronization.
 * Note that this idiom is appropriate for instance fields only.
 * For <strong>static</strong> fields there are superior alternatives.</p>
 *
 * <p>We provide an example use case to demonstrate the usage of this class:
 * A server application uses multiple worker threads to process client requests.
 * If such a request causes a fatal error, an administrator is to be notified using a special messaging service.
 * We assume that the creation of the messaging service is an expensive operation.
 * So it should only be performed if an error actually occurs.
 * Here is where <code>LazyInitializer</code> comes into play.
 * We create a specialized subclass for creating and initializing an instance of our messaging service.
 * <code>LazyInitializer</code> declares an abstract {@link org.apache.commons.lang3.concurrent.LazyInitializer#initialize() initialize()} method which we have to implement to create the messaging service object:</p>
 *
 * <pre>
 * <code>
 * public class MessagingServiceInitializer extends LazyInitializer&lt;MessagingService&gt; {
 *   protected MessagingService initialize() throws ConcurrentException {
 *     // Do all necessary steps to create and initialize the service object
 *     MessagingService service = ...
 *     return service;
 *   }
 * }
 * </code>
 * </pre>
 *
 * <p> Now each server thread is passed a reference to a shared instance of our new <code>MessagingServiceInitializer</code> class.
 * The threads run in a loop processing client requests. If an error is detected, the messaging service is obtained from the initializer, and the administrator is notified:</p>
 *
 * <pre>
 * <code>
 * public class ServerThread implements Runnable {
 *  // The initializer for obtaining the messaging service.
 *  private final ConcurrentInitializer&lt;MessagingService&gt; initializer;
 *
 *  public ServerThread(ConcurrentInitializer&lt;MessagingService&gt; init) {
 *    initializer = init;
 *  }
 *
 *  public void run() {
 *    while (true) {
 *      try {
 *        // wait for request
 *        // process request
 *      } catch (FatalServerException ex) {
 *        // get messaging service
 *        try {
 *          MessagingService svc = initializer.get();
 *          svc.notifyAdministrator(ex);
 *        } catch (ConcurrentException cex) {
 *          cex.printStackTrace();
 *        }
 *      }
 *    }
 *  }
 * }
 * </code>
 * </pre>
 *
 * <p>The {@link org.apache.commons.lang3.concurrent.AtomicInitializer} class is very similar to <code>LazyInitializer</code>.
 * It serves the same purpose: to defer the creation of an object until it is needed.
 * The internal structure is also very similar.
 * Again there is an abstract {@link org.apache.commons.lang3.concurrent.AtomicInitializer#initialize() initialize()} method which has to be implemented by concrete subclasses in order to create and initialize the managed object.
 * Actually, in our example above we can turn the <code>MessagingServiceInitializer</code> into an atomic initializer by simply changing the <strong>extends</strong> declaration to refer to <code>AtomicInitializer&lt;MessagingService&gt;</code> as super class.</p>
 *
 * <p>With {@link org.apache.commons.lang3.concurrent.AtomicSafeInitializer} there is yet another variant implementing the lazy initializing pattern.
 * Its implementation is close to <code>AtomicInitializer</code>; it also uses atomic variables internally and therefore does not need synchronization.
 * The name &quot;Safe&quot; is derived from the fact that it implements an additional check which guarantees that the {@link org.apache.commons.lang3.concurrent.AtomicSafeInitializer#initialize() initialize()} method is called only once.
 * So it behaves exactly in the same way as <code>LazyInitializer</code>.</p>
 *
 * <p>Now, which one of the lazy initializer implementations should you use?
 * First of all we have to state that is is problematic to give general recommendations regarding the performance of these classes.
 * The initializers  make use of low-level functionality whose efficiency depends on multiple factors including the target platform and the number of concurrent threads.
 * So developers should make their own benchmarks in scenarios close to their specific use cases.
 * The following statements are rules of thumb which have to be verified in practice.</p>
 *
 * <p><code>AtomicInitializer</code> is probably the most efficient implementation due to its lack of synchronization and further checks.
 * Its main drawback is that the <code>initialize()</code> method can be called multiple times.
 * In cases where this is not an issue <code>AtomicInitializer</code> is a good choice.
 * <code>AtomicSafeInitializer</code> and  <code>LazyInitializer</code> both guarantee that the initialization method is called only once.
 * Because <code>AtomicSafeInitializer</code> does not use synchronization it is probably slightly more efficient than <code>LazyInitializer</code>, but the concrete numbers might depend on the level of concurrency.</p>
 *
 * <p>Another implementation of the <code>ConcurrentInitializer</code> interface is {@link org.apache.commons.lang3.concurrent.BackgroundInitializer}.
 * It is again an abstract base class with an {@link org.apache.commons.lang3.concurrent.BackgroundInitializer#initialize() initialize()} method that has to be defined by concrete subclasses.
 * The idea of <code>BackgroundInitializer</code> is that it calls the <code>initialize()</code> method in a separate worker thread.
 * An application creates a background initializer and starts it.
 * Then it can continue with its work while the initializer runs in parallel.
 * When the application needs the results of the initializer it calls its <code>get()</code> method.
 * <code>get()</code> blocks until the initialization is complete.
 * This is useful for instance at application startup.
 * Here initialization steps (e.g. reading configuration files, opening a database connection, etc.) can be run in background threads while the application shows a splash screen and constructs its UI.</p>
 *
 * <p>As a concrete example consider an application that has to read the content of a URL - maybe a page with news - which is to be displayed to the user after login.
 * Because loading the data over the network can take some time a specialized implementation of <code>BackgroundInitializer</code> can be created for this purpose:</p>
 *
 * <pre>
 * <code>
 * public class URLLoader extends BackgroundInitializer&lt;String&gt; {
 *   // The URL to be loaded.
 *   private final URL url;
 *
 *   public URLLoader(URL u) {
 *     url = u;
 *   }
 *
 *   protected String initialize() throws ConcurrentException {
 *     try {
 *       InputStream in = url.openStream();
 *       // read content into string
 *       ...
 *       return content;
 *     } catch (IOException ioex) {
 *       throw new ConcurrentException(ioex);
 *     }
 *   }
 * }
 * </code>
 * </pre>
 *
 * <p>An application creates an instance of <code>URLLoader</code> and starts it.
 * Then it can do other things.
 * When it needs the content of the URL it calls the initializer's <code>get()</code> method:</p>
 *
 * <pre>
 * <code>
 * URL url = new URL("http://www.application-home-page.com/");
 * URLLoader loader = new URLLoader(url);
 * loader.start();  // this starts the background initialization
 *
 * // do other stuff
 * ...
 * // now obtain the content of the URL
 * String content;
 * try {
 *   content = loader.get();  // this may block
 * } catch (ConcurrentException cex) {
 *   content = "Error when loading URL " + url;
 * }
 * // display content
 * </code>
 * </pre>
 *
 * <p> Related to <code>BackgroundInitializer</code> is the {@link org.apache.commons.lang3.concurrent.MultiBackgroundInitializer} class.
 * As the name implies, this class can handle multiplie initializations in parallel.
 * The basic usage scenario is that a <code>MultiBackgroundInitializer</code> instance is created.
 * Then an arbitrary number of <code>BackgroundInitializer</code> objects is added using the {@link org.apache.commons.lang3.concurrent.MultiBackgroundInitializer#addInitializer(String, BackgroundInitializer)} method.
 * When adding an initializer a string has to be provided which is later used to obtain the result for this initializer.
 * When all initializers have been added the {@link org.apache.commons.lang3.concurrent.MultiBackgroundInitializer#start()} method is called.
 * This starts processing of all initializers.
 * Later the <code>get()</code> method can be called.
 * It waits until all initializers have finished their initialization.
 * <code>get()</code> returns an object of type {@link org.apache.commons.lang3.concurrent.MultiBackgroundInitializer.MultiBackgroundInitializerResults}.
 * This object provides information about all initializations that have been performed.
 * It can be checked whether a specific initializer was successful or threw an exception.
 * Of course, all initialization results can be queried.</p>
 *
 * <p>With <code>MultiBackgroundInitializer</code> we can extend our example to perform multiple initialization steps.
 * Suppose that in addition to loading a web site we also want to create a JPA entity manager factory and read a configuration file.
 * We assume that corresponding <code>BackgroundInitializer</code> implementations exist.
 * The following example fragment shows the usage of <code>MultiBackgroundInitializer</code> for this purpose:</p>
 *
 * <pre>
 * <code>
 * MultiBackgroundInitializer initializer = new MultiBackgroundInitializer();
 * initializer.addInitializer("url", new URLLoader(url));
 * initializer.addInitializer("jpa", new JPAEMFInitializer());
 * initializer.addInitializer("config", new ConfigurationInitializer());
 * initializer.start();  // start background processing
 *
 * // do other interesting things in parallel
 * ...
 * // evaluate the results of background initialization
 * MultiBackgroundInitializer.MultiBackgroundInitializerResults results =
 * initializer.get();
 * String urlContent = (String) results.getResultObject("url");
 * EntityManagerFactory emf =
 * (EntityManagerFactory) results.getResultObject("jpa");
 * ...
 * </code>
 * </pre>
 *
 * <p>The child initializers are added to the multi initializer and are assigned a unique name.
 * The object returned by the <code>get()</code> method is then queried for the single results using these unique names.</p>
 *
 * <p> If background initializers - including <code>MultiBackgroundInitializer</code> - are created using the standard constructor, they create their own {@link java.util.concurrent.ExecutorService} which is used behind the scenes to execute the worker tasks.
 * It is also possible to pass in an <code>ExecutorService</code> when the initializer is constructed.
 * That way client code can configure the <code>ExecutorService</code> according to its specific needs; for instance, the number of threads available could be limited.</p>
 *
 * <h3>Utility Classes</h3>
 *
 * <p>Another group of classes in the new <code>concurrent</code> package offers some generic functionality related to concurrency.
 * There is the {@link org.apache.commons.lang3.concurrent.ConcurrentUtils} class with a bunch of static utility methods.
 * One focus of this class is dealing with exceptions thrown by JDK classes.
 * Many JDK classes of the executor framework throw exceptions of type {@link java.util.concurrent.ExecutionException} if something goes wrong.
 * The root cause of these exceptions can also be a runtime exception or even an error.
 * In typical Java programming you often do not want to deal with runtime exceptions directly; rather you let them fall through the hierarchy of method invocations until they reach a central exception handler.
 * Checked exceptions in contrast are usually handled close to their occurrence.
 * With <code>ExecutionException</code> this principle is violated.
 * Because it is a checked exception, an application is forced to handle it even if the cause is a runtime exception.
 * So you typically have to inspect the cause of the <code>ExecutionException</code> and test whether it is a checked exception which has to be handled. If this is not the case, the causing exception can be rethrown.
 * </p>
 *
 * <p>The {@link org.apache.commons.lang3.concurrent.ConcurrentUtils#extractCause(java.util.concurrent.ExecutionException)} method does this work for you.
 * It is passed an <code>ExecutionException</code> and tests its root cause.
 * If this is an error or a runtime exception, it is directly rethrown.
 * Otherwise, an instance of {@link org.apache.commons.lang3.concurrent.ConcurrentException} is created and initialized with the root cause
 * (<code>ConcurrentException</code> is a new exception class in the <code>o.a.c.l.concurrent</code> package).
 * So if you get such a <code>ConcurrentException</code>, you can be sure that the original cause for the <code>ExecutionException</code> was a checked exception.
 * For users who prefer runtime exceptions in general there is also an {@link org.apache.commons.lang3.concurrent.ConcurrentUtils#extractCauseUnchecked(java.util.concurrent.ExecutionException)} method which behaves like <code>extractCause()</code>, but returns the unchecked exception {@link org.apache.commons.lang3.concurrent.ConcurrentRuntimeException} instead.</p>
 *
 * <p>In addition to the <code>extractCause()</code> methods there are corresponding {@link org.apache.commons.lang3.concurrent.ConcurrentUtils#handleCause(java.util.concurrent.ExecutionException)} and {@link org.apache.commons.lang3.concurrent.ConcurrentUtils#handleCauseUnchecked(java.util.concurrent.ExecutionException)} methods.
 * These methods extract the cause of the passed in <code>ExecutionException</code> and throw the resulting <code>ConcurrentException</code> or <code>ConcurrentRuntimeException</code>.
 * This makes it easy to transform an <code>ExecutionException</code> into a <code>ConcurrentException</code> ignoring unchecked exceptions:</p>
 *
 * <pre>
 * <code>
 * Future&lt;Object&gt; future = ...;
 * try {
 *   Object result = future.get();
 *   ...
 * } catch (ExecutionException eex) {
 *   ConcurrentUtils.handleCause(eex);
 * }
 * </code>
 * </pre>
 *
 * <p>There is also some support for the concurrent initializers introduced in the last sub section.
 * The <code>initialize()</code> method is passed a <code>ConcurrentInitializer</code> object and returns the object created by this initializer.
 * It is null-safe.
 * The <code>initializeUnchecked()</code> method works analogously, but a <code>ConcurrentException</code> throws by the initializer is rethrown as a <code>ConcurrentRuntimeException</code>.
 * This is especially useful if the specific <code>ConcurrentInitializer</code> does not throw checked exceptions.
 * Using this method the code for requesting the object of an initializer becomes less verbose.
 * The direct invocation looks as follows:</p>
 *
 * <pre>
 * <code>
 * ConcurrentInitializer&lt;MyClass&gt; initializer = ...;
 * try {
 *   MyClass obj = initializer.get();
 *   // do something with obj
 * } catch (ConcurrentException cex) {
 *   // exception handling
 * }
 * </code>
 * </pre>
 *
 * <p>Using the {@link org.apache.commons.lang3.concurrent.ConcurrentUtils#initializeUnchecked(ConcurrentInitializer)} method, this becomes:</p>
 *
 * <pre>
 * <code>
 * ConcurrentInitializer&lt;MyClass&gt; initializer = ...;
 * MyClass obj = ConcurrentUtils.initializeUnchecked(initializer);
 * // do something with obj
 * </code>
 * </pre>
 *
 * <p>Another utility class deals with the creation of threads.
 * When using the <em>Executor</em> framework new in JDK 1.5 the developer usually does not have to care about creating threads; the executors create the threads they need on demand.
 * However, sometimes it is desired to set some properties of the newly created worker threads.
 * This is possible through the {@link java.util.concurrent.ThreadFactory} interface; an implementation of this interface has to be created and passed to an executor on creation time.
 * Currently, the JDK does not provide an implementation of <code>ThreadFactory</code>, so one has to start from scratch. </p>
 *
 * <p> With {@link org.apache.commons.lang3.concurrent.BasicThreadFactory} Commons Lang has an implementation of <code>ThreadFactory</code> that works out of the box for many common use cases.
 * For instance, it is possible to set a naming pattern for the new threads, set the daemon flag and a priority, or install a handler for uncaught exceptions.
 * Instances of <code>BasicThreadFactory</code> are created and configured using the nested {@link org.apache.commons.lang3.concurrent.BasicThreadFactory.Builder} class.
 * The following example shows a typical usage scenario:</p>
 *
 * <pre>
 * <code>
 * BasicThreadFactory factory = new BasicThreadFactory.Builder()
 *   .namingPattern("worker-thread-%d")
 *   .daemon(true)
 *   .uncaughtExceptionHandler(myHandler)
 *   .build();
 * ExecutorService exec = Executors.newSingleThreadExecutor(factory);
 * </code>
 * </pre>
 *
 * <p>The nested <code>Builder</code> class defines some methods for configuring the new <code>BasicThreadFactory</code> instance.
 * Objects of this class are immutable, so these attributes cannot be changed later.
 * The naming pattern is a string which can be passed to <code>String.format()</code>.
 * The placeholder <em>%d</em> is replaced by an increasing counter value.
 * An instance can wrap another <code>ThreadFactory</code> implementation; this is achieved by calling the builder's {@link org.apache.commons.lang3.concurrent.BasicThreadFactory.Builder#wrappedFactory(java.util.concurrent.ThreadFactory) wrappedFactory(ThreadFactory)} method.
 * This factory is then used for creating new threads; after that the specific attributes are applied to the new thread.
 * If no wrapped factory is set, the default factory provided by the JDK is used.</p>
 *
 * <h3>Synchronization objects</h3>
 *
 * <p>The <code>concurrent</code> package also provides some support for specific synchronization problems with threads.</p>
 *
 * <p> {@link org.apache.commons.lang3.concurrent.TimedSemaphore} allows restricted access to a resource in a given time frame.
 * Similar to a semaphore, a number of permits can be acquired.
 * What is new is the fact that the permits available are related to a given time unit.
 * For instance, the timed semaphore can be configured to allow 10 permits in a second.
 * Now multiple threads access the semaphore and call its {@link org.apache.commons.lang3.concurrent.TimedSemaphore#acquire()} method.
 * The semaphore keeps track about the number of granted permits in the current time frame.
 * Only 10 calls are allowed; if there are further callers, they are blocked until the time frame (one second in this example) is over.
 * Then all blocking threads are released, and the counter of available permits is reset to 0.
 * So the game can start anew.</p>
 *
 * <p> What are use cases for <code>TimedSemaphore</code>?
 * One example is to artificially limit the load produced by multiple threads.
 * Consider a batch application accessing a database to extract statistical data.
 * The application runs multiple threads which issue database queries in parallel and perform some calculation on the results.
 * If the database to be processed is huge and is also used by a production system, multiple factors have to be balanced:
 * On one hand, the time required for the statistical evaluation should not take too long.
 * Therefore you will probably use a larger number of threads because most of its life time a thread will just wait for the database to return query results.
 * On the other hand, the load on the database generated by all these threads should be limited so that the responsiveness of the production system is not affected.
 * With a <code>TimedSemaphore</code> object this can be achieved.
 * The semaphore can be configured to allow e.g. 100 queries per second.
 * After these queries have been sent to the database the threads have to wait until the second is over - then they can query again.
 * By fine-tuning the limit enforced by the semaphore a good balance between performance and database load can be established.
 * It is even possible to chang? the number of available permits at runtime.
 * So this number can be reduced during the typical working hours and increased at night.</p>
 *
 * <p>The following code examples demonstrate parts of the implementation of such a scenario.
 * First the batch application has to create an instance of <code>TimedSemaphore</code> and to initialize its properties with default values:</p>
 *
 * <code>TimedSemaphore semaphore = new TimedSemaphore(1, TimeUnit.SECONDS, 100);</code>
 *
 * <p>Here we specify that the semaphore should allow 100 permits in one second.
 * This is effectively the limit of database queries per second in our example use case.
 * Next the server threads issuing database queries and performing statistical operations can be initialized.
 * They are passed a reference to the semaphore at creation time. Before they execute a query they have to acquire a permit.</p>
 *
 * <pre>
 * <code>
 * public class StatisticsTask implements Runnable {
 * // The semaphore for limiting database load.
 *   private final TimedSemaphore semaphore;
 *
 *   public StatisticsTask(TimedSemaphore sem, Connection con) {
 *     semaphore = sem;
 *      ...
 *   }
 *
 *   //The main processing method. Executes queries and evaluates their results.
 *   public void run() {
 *     try {
 *       while (!isDone()) {
 *         semaphore.acquire();    // enforce the load limit
 *         executeAndEvaluateQuery();
 *       }
 *     } catch (InterruptedException iex) {
 *       // fall through
 *     }
 *   }
 * }
 * </code>
 * </pre>
 *
 * <p>The important line here is the call to <code>semaphore.acquire()</code>.
 * If the number of permits in the current time frame has not yet been reached, the call returns immediately.
 * Otherwise, it blocks until the end of the time frame.
 * The last piece missing is a scheduler service which adapts the number of permits allowed by the semaphore according to the time of day.
 * We assume that this service is pretty simple and knows only two different time slots:
 * working shift and night shift.
 * The service is triggered periodically.
 * It then determines the current time slot and configures the timed semaphore accordingly.</p>
 *
 * <pre>
 * <code>
 * public class SchedulerService {
 *   // The semaphore for limiting database load.
 *   private final TimedSemaphore semaphore;
 *     ...
 *
 *   // Configures the timed semaphore based on the current time of day. This method is called periodically.
 *   public void configureTimedSemaphore() {
 *      int limit;
 *      if (isWorkshift()) {
 *        limit = 50;    // low database load
 *      } else {
 *        limit = 250;   // high database load
 *      }
 *
 *      semaphore.setLimit(limit);
 *   }
 * }
 * </code>
 * </pre>
 *
 * <p>With the {@link org.apache.commons.lang3.concurrent.TimedSemaphore#setLimit(int)} method the number of permits allowed for a time frame can be changed.
 * There are some other methods for querying the internal state of a timed semaphore.
 * Also some statistical data is available, e.g. the average number of <code>acquire()</code> calls per time frame.
 * When a timed semaphore is no more needed, its <code>shutdown()</code> method has to be called.</p>
 */
package org.apache.commons.lang3.concurrent;
