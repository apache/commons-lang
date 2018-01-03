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

package org.apache.commons.lang3.time;

import org.apache.commons.lang3.Validate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

/**
 * <p>
 * The {@code StackWatch}, provides a wrapper around the {@code StopWatch} for creating multiple and
 * possibly nested named timings.
 * </p>
 * <p>
 * While the {@link StopWatch} provides functionality to time the length of operations, there is no
 * context or name to go with the time tracked. It is also not possible to time nested calls with
 * the {@code StopWatch}.
 * </p>
 * <p>
 * {@code StackWatch} provides that functionality, allowing successive calls to
 * {@link StackWatch#startTiming} to track nested calls.
 * </p>
 * <p>
 * At the end of a timing 'run', a visitor interface provides the ability to visit all the timing
 * 'nodes' and capture their output, including the level of the call if nested. The visitor is passed
 * the {@code level} of the current node, it's path elements, and a {@link Timing} instance.
 * </p>
 * <p>
 * Through the {@link Timing} the visitor can access the {@link StopWatch} instance for the timing.
 * </p>
 * <p>
 * The {@link TimingNodeInternal} provide a tree structure in support of nesting.
 * A {@code Deque} is use to track the current time node.
 * </p>
 * <p>
 * The generic type {@code N} is used to type the {@link TimingNodeInternal#name}.
 * The generic type {@code T} is used to type the {@link TimingNodeInternal#tags}.
 * </p>
 * <pre>
 *   <code>
 *    private void outerFunction() {
 *      try {
 *        StackWatch{@code <String,String> watch = new StackWatch<>("OuterFunction");}
 *        watch.start();
 *        functionOne(watch);
 *        watch.stop();
 *        watch.visit({@code new StackWatch.TimingVisitor<String,String>()} {
 *         {@literal @}Override
 *          public void visitTiming({@code int level, List<String> path,  StackWatch.Timing<String,String>} timing) {
 *            System.out.println("Visit level " + level + " timing: " + timing.getName()) + " path: " + path + " time: " + timing.getStopWatch().getNanoTime() + "ns");
 *          }
 *        });
 *      } catch (Exception e){}
 *    }
 *
 *    private void functionOne({@code StackWatch<String,String>} watch) throws Exception {
 *      watch.startTiming("One", "tagOne");
 *      functionOneOne(watch);
 *      watch.stopTiming();
 *    }
 *
 *    private void functionOneOne({@code StackWatch<String,String>} watch) throws Exception {
 *      watch.startTiming("OneOne", "tagTwo");
 *      functionOneTwo(watch);
 *      watch.stopTiming();
 *    }
 *
 *    private void functionOneTwo({@code StackWatch<String,String>} watch) throws Exception {
 *      watch.startTiming("OneTwo", "tagThree");
 *      watch.stopTiming();
 *    }
 *   </code>
 * </pre>
 * <p>
 * The example above would result in the following output.
 * </p>
 * <pre>
 *   <code>
 *    Visit level 0 timing: OuterFunction path: [OuterFunction] time: 316032287ns
 *    Visit level 1 timing: One path: [OuterFunction, One] time: 156596831ns
 *    Visit level 2 timing: OneOne path: [OuterFunction, One, OneOne] time: 106207892ns
 *    Visit level 3 timing: OneTwo path: [OuterFunction, One, OneOne, OneTwo] time: 52851139ns
 *   </code>
 * </pre>
 * <p>
 * This class is not thread safe, and is meant to track timings across multiple calls on the same
 * thread
 * </p>
 *
 * @since 3.8
 */
public class StackWatch<N,T> {
    /**
     * The Deque used to track the timings
     */
    private Deque<TimingNode> deque = new LinkedList<>();

    /**
     * The root {@code TimingNodeInternal}.
     * The root node represents the root of a tree structure, and contains all {@code TimingNodeInternal}
     * instances.
     */
    private TimingNode<N,T> rootNode;

    /**
     * <p>
     * Constructor
     * </p>
     * <p>
     * The top most timing will be created with the rootName on {@link StackWatch#start()} ()}
     * </p>
     * @param rootName the root name
     * @throws NullPointerException if rootName is null
     */
    public StackWatch(N rootName) {
        Validate.notNull(rootName,"The rootName cannot be null");
        rootNode = new TimingNodeInternal<>(rootName);
    }

    /**
     * <p>
     * Returns the root name.
     * </p>
     *
     * @return the root name.
     */
    public N getRootName() {
        return this.rootNode.getName();
    }

    /**
     * <p>
     * Starts the {@code StackWatch}.
     * </p>
     * <p>
     * A root timing will be created named for the rootName and started.
     * </p>
     * <p>
     * If not called before the first {@link  StackWatch#startTiming} call, then the
     * {@code StackWatch} will be started at that time.
     * </p>
     *
     * @throws IllegalStateException if the {@code StackWatch} has already been started.
     */
    public void start() {
        if(rootNode.isRunning()) {
            throw new IllegalStateException("StackWatch already started");
        }
        rootNode.start();
        deque.push(rootNode);
    }

    /**
     * <p>
     * Start a timing.  If {@link StackWatch#start()} has not been called, the {@code StackWatch} will be
     * started.
     * </p>
     * <p>
     * This may be called multiple times, before a {@link StackWatch#stopTiming()} call is made, if calls are nested,
     * for example:
     * </p>
     * <pre>
     *   <code>
     *    private void functionOne({@code StackWatch<String,String>} watch) throws Exception {
     *      watch.startTiming("One", "tagOne");
     *      functionOneOne(watch);
     *      watch.stopTiming();
     *    }
     *
     *    private void functionOneOne({@code StackWatch<String,String>} watch) throws Exception {
     *      watch.startTiming("OneOne", "tagTwo");
     *      functionOneTwo(watch);
     *      watch.stopTiming();
     *    }
     *
     *    private void functionOneTwo({@code StackWatch<String,String>} watch) throws Exception {
     *      watch.startTiming("OneTwo", "tagThree");
     *      watch.stopTiming();
     *    }
     *   </code>
     * </pre>
     * <p>
     * Starting a timing, when it's parent timing is not running results in an
     * {@code IllegalStateException}.
     * </p>
     * <p>
     * For example, this code, although contrived, would throw an {@code IllegalStateException}, because
     * functionOne is not running:
     * </p>
     * <pre>
     *   <code>
     *    private void functionOne({@code StackWatch<String,String>} watch) throws Exception {
     *      watch.startTiming("One", "tagOne");
     *      watch.visit({@code new StackWatch.TimingVisitor<String,String>}() {
     *       {@literal @}Override
     *        public void visitTiming(int level, {@code List<String> path, StackWatch.Timing<String,String>} timing) {
     *          timing.getStopWatch().stop();
     *        }
     *      });
     *      functionOneOne(watch);
     *    }
     *
     *    private void functionOneOne({@code StackWatch<String,String>} watch) throws Exception {
     *      watch.startTiming("OneOne", "tagTwo");
     *      functionOneTwo(watch);
     *      watch.stopTiming();
     *    }
     *   </code>
     * </pre>
     * <p>
     *
     * @param name the name of this timing
     * @param tags the tags to associate with this timing
     * @throws IllegalStateException if the parent timing is not running or there is an attempt to start
     *                               a new timing after creating a number of timings and closing them all.
     * @throws NullPointerException if the name is null
     */
    @SafeVarargs
    @SuppressWarnings("unchecked")
    public final void startTiming(N name, T... tags) {
        final TimingNode<N,T> parentNode;
        // If the deque is empty, then the root needs to be added and started
        if (deque.isEmpty()) {
            // we are starting a timing, having not started the StackWatch
            // it needs to be started.
            start();
            parentNode = rootNode;
        } else {
            // if the current node is not running, then this is an InvalidStateException, as the parent
            // cannot close before it's children
            if (!deque.peek().isRunning()) {
                throw new IllegalStateException(String
                        .format("Parent TimingNodeInternal %s is not running", deque.peek().getName().toString()));
            }
            // this is a nested start, add as child to current running
            parentNode = deque.peek();
        }

        // request the current node to create a new child with this timing name and start it
        TimingNode<N,T> node = parentNode.createChild(name, tags);
        node.start();
        // this node is now top of the stack
        deque.push(node);
    }

    /**
     * <p>
     * Stop the current timing.
     * </p>
     * <p>
     * In the case of nested timings, the current timing is stopped and removed from the {@code Deque}
     * causing the parent node to be the top of the stack.
     * </p>
     * <p>
     * If the timing being stopped has running child timings an {@code IllegalStateException} will
     * be thrown.
     * </p>
     *
     * @throws IllegalStateException if stopping a timing with running child timings
     */
    public void stopTiming() {
        if (deque.isEmpty()) {
            throw new IllegalStateException(
                    "Trying to stop time, there are no running records in deque");
        }
        deque.pop().stop();
    }

    /**
     * Stops the {@code StackWatch}.
     *
     * @throws IllegalStateException if there are running timings other than the root timing
     */
    public void stop() {
        if (deque.size() > 1) {
            throw new IllegalStateException("Stopping with running timings");
        }
        stopTiming();
    }

    /**
     * Clears the stack and the root node.
     */
    public void clear() {
        deque.clear();
        if(rootNode.isRunning()) {
            rootNode.stop();
        }

        rootNode = new TimingNodeInternal<>(rootNode.getName());
    }

    /**
     * <p>
     * Initiate the visitation of the nodes in this timing.
     * </p>
     * <p>
     * The {@link TimingVisitor} will be called back for each node in the tree, and will
     * pass the level of the node in the tree. The root level is 0.
     * </p>
     *
     * @param visitor callback interface.
     */
    public void visit(TimingVisitor<N,T> visitor) {
        rootNode.visit(0, new LinkedList<N>(), visitor);
    }

    public interface TimingVisitor<N,T> {

        /**
         * Visit a {@code TimingNodeInternal}.
         *
         * @param level the depth level of this node
         * @param path the names from this node to root
         * @param node the {@code Timing}
         */
        void visitTiming(int level, List<N> path, Timing<N,T> node);
    }

    public interface TimingNode<N, T> extends Timing<N,T>{

        /**
         * Returns this node's children.
         * @return List of child nodes
         */
        List<TimingNode<N,T>> getChildren();

        /**
         * Return the status of the internally manage {@link StopWatch}.
         * @return true if it is running
         */
        boolean isRunning();

        /**
         * Starts the node and it's {@link StopWatch}.
         */
        void start();

        /**
         * <p>
         * Stops the StopWatch.
         * </p>
         * <p>
         * If this node has running children, an {@code IllegalStateException} will result.
         * </p>
         *
         * @throws IllegalStateException if stop is called on a node with running children
         */
        void stop();

        /**
         * Creates a new child node to this node.
         * If the current node is not started, then this operation results in an
         * {@code IllegalStateException}
         *
         * @param childName the name of the child
         * @param tags the tags for this timing
         * @return the child node created
         * @throws IllegalStateException if the current node is not started.
         * @throws IllegalArgumentException if the node name is null or empty.
         */
        TimingNode<N,T> createChild(N childName, T... tags)
                throws IllegalStateException;

        /**
         * Visits the current node and each of its children in turn.
         * The provided {@link TimingVisitor} will be called this node, and passed to each
         * child node in descent.
         *
         * @param level the level of this node.
         * @param path the path
         * @param visitor the visitor callback
         */
        void visit(int level, LinkedList<N> path, StackWatch.TimingVisitor<N, T> visitor);
    }

    public interface Timing<N,T> {

        /**
         * Returns the name of this {@code Timing}.
         * @return the name
         */
        N getName();

        /**
         * Returns the {@link StopWatch} for this record.
         * @return {@link StopWatch}
         */
        StopWatch getStopWatch();

        /**
         * Returns the tags of type T for this record.
         * @return List of tags
         */
        List<T> getTags();

    }

    /**
     * The tree node to track time and children.
     * The {@code StopWatch} class is used for timings
     *
     * The generic type {@code N} is used for the node's name.
     * The generic type {@code T} is used for the node's tags.
     *
     * @since 3.8
     */
    private static final class TimingNodeInternal<N,T> implements TimingNode<N, T> {

        /**
         * The name of this node.
         */
        private N name;

        /**
         * The tags associated with this timing.
         */
        private List<T> tags;

        /**
         * The child nodes of this node.
         */
        private List<TimingNode<N,T>> children = new ArrayList<>();

        /**
         * The {@code StopWatch} for this node.
         */
        private StopWatch stopWatch = new StopWatch();

        /**
         * <p>
         * Constructor.
         * </p>
         * <p>
         * Creates a new TimingNodeInternal for a given parent name, with a given name.
         * </p>
         *
         * @param name the name of the timing
         * @param tags the tags to associate with this timing
         * @throws NullPointerException if the name is null.
         */
        @SafeVarargs
        TimingNodeInternal(N name, T... tags) {
            Validate.notNull(name,"name cannot be null");
            this.name = name;

            // if tags were not passed, this will be an Object[0] and not T
            if(tags.length == 0) {
                this.tags = new ArrayList<>();
            }else{
                this.tags = new ArrayList<>(Arrays.asList(tags));
            }
        }

        /**
         * Returns the node's timing name.
         *
         * @return the node timing name
         */
        @Override
        public N getName() {
            return name;
        }

        @Override
        public List<TimingNode<N,T>> getChildren() {
            return Collections.unmodifiableList(children);
        }

        /**
         * Return if the node's StopWatch is running.
         *
         * @return true if it is running, false if not
         */
        @Override
        public boolean isRunning() {
            return stopWatch.isStarted();
        }

        /**
         * Starts the StopWatch.
         */
        @Override
        public void start() {
            if (!stopWatch.isStarted()) {
                stopWatch.reset();
                stopWatch.start();
            }
        }

        /**
         * <p>
         * Stops the StopWatch.
         * </p>
         * <p>
         * If this node has running children, an {@code IllegalStateException} will result.
         * </p>
         *
         * @throws IllegalStateException if stop is called on a node with running children
         */
        @Override
        public void stop() {
            for (TimingNode child : children) {
                if (child.isRunning()) {
                    throw new IllegalStateException("Cannot stop a timing with running children");
                }
            }
            if(stopWatch.isStarted()) {
                stopWatch.stop();
            }
        }

        /**
         * Returns the {@link StopWatch} for this node.
         *
         * @return {@link StopWatch}
         */
        @Override
        public StopWatch getStopWatch() {
            return stopWatch;
        }

        /**
         * The tags associated with this timing.
         *
         * @return List of tags
         */
        @Override
        public List<T> getTags() {
            return Collections.unmodifiableList(tags);
        }

        /**
         * Creates a new child node to this node.
         * If the current node is not started, then this operation results in an
         * {@code IllegalStateException}
         *
         * @param childName the name of the child
         * @param tags the tags for this timing
         * @return the child node created
         * @throws IllegalStateException if the current node is not started.
         * @throws IllegalArgumentException if the node name is null or empty.
         */
        @Override
        @SafeVarargs
        public final TimingNode<N,T> createChild(N childName, T... tags)
                throws IllegalStateException {
            if (!stopWatch.isStarted()) {
                throw new IllegalStateException("Adding a child to a non-started parent");
            }
            TimingNodeInternal<N,T> child = new TimingNodeInternal<>(childName, tags);
            children.add(child);
            return child;
        }

        /**
         * Visits the current node and each of its children in turn.
         * The provided {@link TimingVisitor} will be called this node, and passed to each
         * child node in descent.
         *
         * @param level the level of this node.
         * @param path the path
         * @param visitor the visitor callback
         */
        @Override
        @SuppressWarnings("unchecked")
        public void visit(int level, LinkedList<N> path, TimingVisitor<N, T> visitor) {
            path.addLast(name);
            visitor.visitTiming(level, Collections.unmodifiableList(path),this);
            for (TimingNode child : children) {
                child.visit(level + 1, path, visitor);
            }
            path.removeLast();

        }
    }
}
