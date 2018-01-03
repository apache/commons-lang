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

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class StackWatchTest {
    private static final String NODE_NAME = "testFunction";
    private static final String TAG_ONE = "tag_one";
    private static final String TAG_TWO = "tag_two";
    private static final String CHILD_NAME = "the child";
    private static final String ROOT_NAME = "root";
    @Test
    public void testRootNameConstructor() {
        StackWatch<String,String> watch = new StackWatch<>(ROOT_NAME);
        assertEquals(ROOT_NAME, watch.getRootName());
    }

    @Test(expected = NullPointerException.class)
    public void testRootNameWithNullThrows() {
        new StackWatch<>(null);
    }

    @Test
    public void start() {
        final StopWatch stopWatch = new StopWatch();
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        stopWatch.start();
        stopWatch.stop();
        watch.stop();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                assertTrue(node.getStopWatch().getNanoTime() > stopWatch.getNanoTime());
            }
        });
    }

    @Test(expected = IllegalStateException.class)
    public void testStartASecondTimeThrowsException() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.start();
    }

    @Test
    public void startTiming() {
        final StopWatch stopWatch = new StopWatch();
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        stopWatch.start();
        stopWatch.stop();
        watch.stopTiming();
        watch.stop();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    assertTrue(node.getStopWatch().getNanoTime() > stopWatch.getNanoTime());
                }
            }
        });
    }

    @Test
    public void stopTiming() throws Exception {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        Thread.sleep(100);
        watch.stopTiming();
        watch.stop();
        final ArrayList<Long> times = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path,StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    times.add(node.getStopWatch().getNanoTime());
                }
            }
        });

        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    times.add(node.getStopWatch().getNanoTime());
                }
            }
        });

        assertEquals(times.size(), 2);
        assertEquals(times.get(0), times.get(1));
    }

    @Test(expected = IllegalStateException.class)
    public void stopWithoutStopTimingThrowsException() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.stop();
    }

    @Test(expected = IllegalStateException.class)
    public void clearBeforeStopThrowsException() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.stopTiming();
        watch.clear();
        watch.stop();
    }

    @Test
    public void testNonStringNameTiming() throws Exception {
        StackWatch<Integer,String> watch = new StackWatch<>(99);
        watch.start();
        watch.startTiming(100);
        Thread.sleep(100);
        watch.stopTiming();
        watch.stop();
        final ArrayList<Long> times = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<Integer, String>() {
            @Override
            public void visitTiming(int level, List<Integer> path, StackWatch.Timing<Integer,String> node) {
                if (level > 0) {
                    times.add(node.getStopWatch().getNanoTime());
                    assertTrue(node.getName().getClass() == Integer.class);
                }
            }
        });

        watch.visit(new StackWatch.TimingVisitor<Integer, String>() {
            @Override
            public void visitTiming(int level, List<Integer> path, StackWatch.Timing<Integer,String> node) {
                if (level > 0) {
                    times.add(node.getStopWatch().getNanoTime());
                }
            }
        });

        assertEquals(times.size(), 2);
        assertEquals(times.get(0), times.get(1));
    }

    @Test
    public void testStackWatch() throws Exception {
        // General test, call three top level functions, the first of two having
        // nested calls
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        // root timing
        watch.startTiming("Test");
        functionOne(watch);
        functionTwo(watch);
        functionThree(watch);
        watch.stopTiming();
        watch.stop();
        final ArrayList<Integer> levels = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                levels.add(level);
            }
        });
        // validate that we have the right number of 'timings'
        assertEquals(levels.size(), 9);
    }

    @Test
    public void testStackWatchWithoutStarting() throws Exception {
        // General test, call three top level functions, the first of two having
        // nested calls
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        // root timing
        watch.startTiming("Test");
        functionOne(watch);
        functionTwo(watch);
        functionThree(watch);
        watch.stopTiming();
        watch.stop();
        final ArrayList<Integer> levels = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                levels.add(level);
            }
        });
        // validate that we have the right number of 'timings'
        assertEquals(levels.size(), 9);
    }

    @Test
    public void testStackWatchFiltered() throws Exception {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        final List<String> filter = new ArrayList<>();
        filter.add("ThreeFunc");
        watch.startTiming("Test");
        functionOne(watch);
        functionTwo(watch);
        functionThree(watch);
        watch.stopTiming();
        watch.stop();
        final ArrayList<Integer> levels = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                List<String> tags = node.getTags();
                if (tags != null) {
                    if (tags.containsAll(filter)) {
                        levels.add(level);
                    }
                }
            }
        });

        // validate that we have the right number of 'timings'
        // there is only one ThreeFunc
        assertEquals(levels.size(), 1);
    }

    @Test
    public void testNonStartOuter() throws Exception {
        // Test a case where we are doing timings, but don't give it an 'outer' time
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        functionOne(watch);
        functionTwo(watch);
        functionThree(watch);
        watch.stop();

        final ArrayList<Integer> levels = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                levels.add(level);
            }
        });
        assertEquals(levels.size(), 8);
    }

    @Test(expected = IllegalStateException.class)
    public void testMissMatchedStopThrowsException() throws Exception {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        functionOne(watch);
        functionTwo(watch);
        functionThree(watch);
        // we are stopping when we didn't explicitly start, so this will stop the root
        // and empty the queue
        watch.stopTiming();
        // exception
        watch.stop();
    }

    @Test
    public void testDidNotStopAll() throws Exception {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.startTiming("Test");
        functionOne(watch);
        functionTwo(watch);
        functionThree(watch);
        functionNoStop(watch);
        watch.stopTiming();
        final ArrayList<Integer> levels = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String, String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                levels.add(level);
            }
        });

        assertEquals(levels.size(), 10);
    }

    @Test(expected = NullPointerException.class)
    public void testNullNameThrowsException() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.startTiming(null);
    }

    @Test(expected = IllegalStateException.class)
    public void testParentNotRunningThrowsException() throws Exception {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.startTiming("test");
        functionOneCloseParent(watch);
    }

    @Test
    public void testStartingSecondSetOfTimingsClear() throws Exception {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.startTiming("Test");
        functionOne(watch);
        watch.stopTiming();
        watch.stop();
        watch.clear();
        watch.startTiming("More Test");
        watch.stopTiming();
        final ArrayList<Integer> levels = new ArrayList<>();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                levels.add(level);
            }
        });
        watch.stop();
        assertEquals(levels.size(),2);
    }

    private void functionOne(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("One", "OneFunc");
        Thread.sleep(50);
        functionOneOne(watch);
        watch.stopTiming();
    }

    private void functionOneCloseParent(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("One", "OneFunc");
        Thread.sleep(50);
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                node.getStopWatch().stop();
            }
        });
        functionOneOne(watch);
    }

    private void functionOneOne(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("OneOne", "OneFunc");
        Thread.sleep(50);
        functionOneTwo(watch);
        watch.stopTiming();

    }

    private void functionOneTwo(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("OneTwo", "OneFunc");
        Thread.sleep(50);
        watch.stopTiming();
    }

    private void functionTwo(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("Two", "TwoFunc");
        Thread.sleep(50);
        functionTwoOne(watch);
        watch.stopTiming();
    }

    private void functionTwoOne(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("TwoOne", "TwoFunc");
        Thread.sleep(50);
        functionTwoTwo(watch);
        watch.stopTiming();
    }

    private void functionTwoTwo(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("TwoTwo", "TwoFunc");
        Thread.sleep(50);
        watch.stopTiming();
    }

    private void functionThree(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("Three", "ThreeFunc");
        Thread.sleep(50);
        watch.stopTiming();
    }

    private void functionNoStop(StackWatch<String,String> watch) throws Exception {
        watch.startTiming("NoStop");
        Thread.sleep(50);
    }

    @Test(expected = NullPointerException.class)
    public void testNullTimingNodeNameThrowsException() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    ((StackWatch.TimingNode<String,String>)node).createChild(null);
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test
    public void testGetTimingName() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.stopTiming();
        watch.stop();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    assertNotNull(node.getName());
                }
            }
        });
    }

    @Test
    public void testIsRunning() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    assertTrue(((StackWatch.TimingNode<String,String>)node).isRunning());
                }
            }
        });
        watch.stopTiming();
        watch.stop();
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    assertFalse(((StackWatch.TimingNode<String,String>)node).isRunning());
                }
            }
        });
    }

    @Test
    public void testStartStart() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    ((StackWatch.TimingNode<String,String>)node).start();
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test
    public void testStop() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    ((StackWatch.TimingNode<String,String>)node).stop();
                    assertFalse(((StackWatch.TimingNode<String,String>)node).isRunning());
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }


    @Test(expected = IllegalStateException.class)
    @SuppressWarnings("unchecked")
    public void testStopWhileAddingChildrenThrowsException() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level > 0) {
                    ((StackWatch.TimingNode<String,String>)node).stop();
                    ((StackWatch.TimingNode<String,String>)node).createChild("boom");
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test(expected = IllegalStateException.class)
    @SuppressWarnings("unchecked")
    public void testStopWithRunningChildrenThrowsException() {
           StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (level == 0) {
                    ((StackWatch.TimingNode<String,String>)node).stop();
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testCreateChild() {
        StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                if (node.getName().equals("one")) {
                    StackWatch.TimingNode<String,String> child = ((StackWatch.TimingNode<String,String>)node)
                            .createChild("child");
                    assertNotNull(child);
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test(expected = IllegalStateException.class)
    @SuppressWarnings("unchecked")
    public void testCreateChildWhenNotStartedThrowsException() {
      StackWatch<String,String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String,String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String,String> node) {
                // this has the effect of trying to add a child to.. child
                if (level > 0) {
                    StackWatch.TimingNode<String,String> child = ((StackWatch.TimingNode<String,String>)node)
                            .createChild("child");
                    assertNotNull(child);
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test
    public void getStopWatch() {
        StackWatch<String, String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming("one");
        watch.visit(new StackWatch.TimingVisitor<String, String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String, String> node) {
                assertNotNull(node.getStopWatch());
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test
    public void getTags() {
        StackWatch<String, String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming(NODE_NAME, TAG_ONE, TAG_TWO);
        watch.visit(new StackWatch.TimingVisitor<String, String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String, String> node) {
                if (node.getName().equals(NODE_NAME)) {
                    assertEquals(2, node.getTags().size());
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    @Test
    public void getTagsWithNoTags() {
        StackWatch<String, String> watch = new StackWatch<>("testStackWatch");
        watch.start();
        watch.startTiming(NODE_NAME);
        watch.visit(new StackWatch.TimingVisitor<String, String>() {
            @Override
            public void visitTiming(int level, List<String> path, StackWatch.Timing<String, String> node) {
                if (node.getName().equals(NODE_NAME)) {
                    assertNotNull(node.getTags());
                    assertEquals(0, node.getTags().size());
                }
            }
        });
        watch.stopTiming();
        watch.stop();
    }

    //@Test
    // function for testing java doc code
    public void outerFunction() {
        try {
            StackWatch<String, String> watch = new StackWatch<>("OuterFunction");
            watch.start();
            functionOne(watch);
            functionTwo(watch);
            watch.stop();
            watch.visit(new StackWatch.TimingVisitor<String, String>() {
                @Override
                public void visitTiming(int level, List<String> path, StackWatch.Timing<String, String> timing) {
                    System.out.println("Visit level " + level + " timing: " + timing.getName() + " path: " + path
                    + " time: " + timing.getStopWatch().getNanoTime() + "ns");
                }
            });
        } catch (Exception e) {
        }
    }
}

