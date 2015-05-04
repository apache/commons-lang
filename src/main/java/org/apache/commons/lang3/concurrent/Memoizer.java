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
package org.apache.commons.lang3.concurrent;

import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

/**
 * <p>Definition of an interface for a wrapper around a calculation that takes a single parameter and returns a result.
 * The results for the calculation will be cached for future requests.</p>
 * <p/>
 * <p>This is not a fully functional cache, there is no way of limiting or removing results once they have been generated.
 * However, it is possible to get the implementation to regenerate the result for a given parameter, if an error was
 * thrown during the previous calculation, by setting the option during the construction of the class. If this is not
 * set the class will return the cached exception.</p>
 * <p/>
 * <p>Thanks should go to Brian Goetz, Tim Peierls and the members of JCP JSR-166 Expert Group for coming up with the
 * original implementation of the class. It was also published within Java Concurreny in Practice as a sample.</p>
 *
 * @param <A> the type of the input to the calculation
 * @param <V> the type of the output of the calculation
 */
public class Memoizer<A, V> implements Computable<A, V> {
	private final ConcurrentMap<A, Future<V>> cache
			= new ConcurrentHashMap<A, Future<V>>();
	private final Computable<A, V> c;
	private final boolean recalculate;

	/**
	 * <p>Constructs a Memoizer for the provided Computable calculation.</p>
	 * <p/>
	 * <p>If a calculation is thrown an exception for any reason, this exception will be cached and returned for
	 * all future calls with the provided parameter.</p>
	 *
	 * @param c the computation whose results should be memorized
	 */
	public Memoizer(Computable<A, V> c) {
		this(c, false);
	}

	/**
	 * <p>Constructs a Memoizer for the provided Computable calculation, with the option of whether a Computation
	 * that experiences an error should recalculate on subsequent calls or return the same cached exception.</p>
	 *
	 * @param c           the computation whose results should be memorized
	 * @param recalculate determines whether the computation should be recalculated on subsequent calls if the previous
	 *                    call failed
	 */
	public Memoizer(Computable<A, V> c, boolean recalculate) {
		this.c = c;
		this.recalculate = recalculate;
	}

	/**
	 * <p>This method will return the result of the calculation and cache it, if it has not previously been calculated.</p>
	 * <p/>
	 * <p>This cache will also cache exceptions that occur during the computation if the {@code recalculate} parameter is
	 * the constructor was set to {@code false}, or not set. Otherwise, if an exception happened on the previous
	 * calculation, the method will attempt again to generate a value.</p>
	 *
	 * @param arg the argument for the calculation
	 * @return the result of the calculation
	 * @throws InterruptedException  thrown if the calculation is interrupted
	 * @throws IllegalStateException a wrapper around any checked exception that occurs during the computation of the result
	 */
	public V compute(final A arg) throws InterruptedException, IllegalStateException {
		while (true) {
			Future<V> f = cache.get(arg);
			if (f == null) {
				Callable<V> eval = new Callable<V>() {
					public V call() throws InterruptedException {
						return c.compute(arg);
					}
				};
				FutureTask<V> ft = new FutureTask<V>(eval);
				f = cache.putIfAbsent(arg, ft);
				if (f == null) {
					f = ft;
					ft.run();
				}
			}
			try {
				return f.get();
			}
			catch (CancellationException e) {
				cache.remove(arg, f);
			}
			catch (ExecutionException e) {
				if (recalculate) {
					cache.remove(arg, f);
				}

				throw launderException(e.getCause());
			}
		}
	}

	/**
	 * <p>This method launders a Throwable to either a RuntimeException, Error or any other Exception wrapped
	 * in an IllegalStateException.</p>
	 *
	 * @param t the throwable to laundered
	 * @return a RuntimeException, Error or an IllegalStateException
	 */
	private RuntimeException launderException(Throwable t) {
		if (t instanceof RuntimeException) {
			return (RuntimeException) t;
		} else if (t instanceof Error) {
			throw (Error) t;
		} else {
			throw new IllegalStateException("Unchecked exception", t);
		}
	}
}
