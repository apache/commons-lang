/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.functor;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.lang.functor.Predicate;
import org.apache.commons.lang.functor.PredicateUtils;
import org.apache.commons.lang.functor.Transformer;
import org.apache.commons.lang.functor.TransformerException;
import org.apache.commons.lang.functor.TransformerUtils;
/**
 * <code>ExecutorUtils</code> provides reference implementations and utilities
 * for the Executor pattern interface. The supplied commands are:
 * <ul>
 * <li>Invoker - invokes a method on the input object
 * <li>For - repeatedly calls a command for a fixed number of times
 * <li>While - repeatedly calls a command while a predicate is true
 * <li>DoWhile - repeatedly calls a command while a predicate is true
 * <li>Chained - chains two or more commands together
 * <li>Switch - calls one command based on one or more predicates
 * <li>SwitchMap - calls one command looked up from a Map
 * <li>Transformer - wraps a Transformer as a Predicate
 * <li>NOP - does nothing
 * <li>Exception - always throws an exception
 * </ul>
 * All the supplied commands are Serializable.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: ExecutorUtils.java,v 1.1 2002/11/06 19:15:40 bayard Exp $
 */
public class ExecutorUtils {

    /**
     * A Executor that always throws an exception
     */
    private static final Executor EXCEPTION_COMMAND = new ExceptionExecutor();
    /**
     * A Executor that does nothing
     */
    private static final Executor NOP_COMMAND = new NOPExecutor();

    /**
     * Restrictive constructor
     */
    protected ExecutorUtils() {
        super();
    }

    /**
     * Gets a Executor that always throws an exception.
     * This could be useful during testing as a placeholder.
     *
     * @return the command
     */
    public static Executor exceptionExecutor() {
        return EXCEPTION_COMMAND;
    }

    /**
     * Gets a Executor that will do nothing.
     * This could be useful during testing as a placeholder.
     *
     * @return the command
     */
    public static Executor nopExecutor() {
        return NOP_COMMAND;
    }

    /**
     * Creates a Executor that calls a Factory each time the transformer is used.
     * The transformer will return the value returned by the factory.
     *
     * @param transformer  the transformer to run each time in the command
     * @return the command.
     */
    public static Executor asExecutor(Transformer transformer) {
        if (transformer == null) {
            throw new IllegalArgumentException("TransformerExecutor: The transformer must not be null");
        }
        return new TransformerExecutor(transformer);
    }

    /**
     * Creates a Executor that will call the command <code>count</code> times.
     *
     * @param count  the number of times to loop
     * @param command  the command to call repeatedly
     * @return the command
     * @throws IllegalArgumentException if either argument is null
     */
    public static Executor forExecutor(int count, Executor command) {
        if (count < 0) {
            throw new IllegalArgumentException("ForExecutor: The loop count must not be less than zero, it was " + count);
        }
        if (command == null) {
            throw new IllegalArgumentException("ForExecutor: The command must not be null");
        }
        return new ForExecutor(count, command);
    }

    /**
     * Creates a Executor that will call the command repeatedly until the 
     * predicate returns false.
     *
     * @param predicate  the predicate to use as an end of loop test
     * @param command  the command to call repeatedly
     * @return the command
     * @throws IllegalArgumentException if either argument is null
     */
    public static Executor whileExecutor(Predicate predicate, Executor command) {
        if (predicate == null) {
            throw new IllegalArgumentException("WhileExecutor: The predicate must not be null");
        }
        if (command == null) {
            throw new IllegalArgumentException("WhileExecutor: The command must not be null");
        }
        return new WhileExecutor(predicate, command, false);
    }

    /**
     * Creates a Executor that will call the command once and then repeatedly
     * until the predicate returns false.
     *
     * @param command  the command to call repeatedly
     * @param predicate  the predicate to use as an end of loop test
     * @return the command
     * @throws IllegalArgumentException if either argument is null
     */
    public static Executor doWhileExecutor(Executor command, Predicate predicate) {
        if (command == null) {
            throw new IllegalArgumentException("DoWhileExecutor: The command must not be null");
        }
        if (predicate == null) {
            throw new IllegalArgumentException("DoWhileExecutor: The predicate must not be null");
        }
        return new WhileExecutor(predicate, command, true);
    }

    /**
     * Creates a Executor that will invoke a specific method on the command's
     * input object by reflection.
     *
     * @param methodName  the name of the method
     * @return the command
     * @throws IllegalArgumentException if the method name is null
     */
    public static Executor invokerExecutor(String methodName) {
        return asExecutor(TransformerUtils.invokerTransformer(methodName, null, null));
    }

    /**
     * Creates a Executor that will invoke a specific method on the command's
     * input object by reflection.
     *
     * @param methodName  the name of the method
     * @param paramTypes  the parameter types
     * @param args  the arguments
     * @return the command
     * @throws IllegalArgumentException if the method name is null
     * @throws IllegalArgumentException if the paramTypes and args don't match
     */
    public static Executor invokerExecutor(String methodName, Class[] paramTypes, Object[] args) {
        return asExecutor(TransformerUtils.invokerTransformer(methodName, paramTypes, args));
    }

    /**
     * Create a new Executor that calls two Executors, passing the result of
     * the first into the second.
     * 
     * @param command1  the first command
     * @param command2  the second command
     * @return the command
     * @throws IllegalArgumentException if either command is null
     */
    public static Executor chainedExecutor(Executor command1, Executor command2) {
        Executor[] commands = new Executor[] { command1, command2 };
        validate(commands);
        return new ChainedExecutor(commands);
    }

    /**
     * Create a new Executor that calls each command in turn, passing the 
     * result into the next command.
     * 
     * @param commands  an array of commands to chain
     * @return the command
     * @throws IllegalArgumentException if the commands array is null
     * @throws IllegalArgumentException if the commands array has 0 elements
     * @throws IllegalArgumentException if any command in the array is null
     */
    public static Executor chainedExecutor(Executor[] commands) {
        commands = copy(commands);
        validate(commands);
        return new ChainedExecutor(commands);
    }

    /**
     * Create a new Executor that calls each command in turn, passing the 
     * result into the next command. The ordering is that of the iterator()
     * method on the collection.
     * 
     * @param commands  a collection of commands to chain
     * @return the command
     * @throws IllegalArgumentException if the commands collection is null
     * @throws IllegalArgumentException if the commands collection is empty
     * @throws IllegalArgumentException if any command in the collection is null
     */
    public static Executor chainedExecutor(Collection commands) {
        if (commands == null) {
            throw new IllegalArgumentException("ChainedExecutor: The command collection must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Executor[] cmds = new Executor[commands.size()];
        int i = 0;
        for (Iterator it = commands.iterator(); it.hasNext();) {
            cmds[i++] = (Executor) it.next();
        }
        validate(cmds);
        return new ChainedExecutor(cmds);
    }

    /**
     * Create a new Executor that calls one of two commands depending 
     * on the specified predicate.
     * 
     * @param predicate  the predicate to switch on
     * @param trueExecutor  the command called if the predicate is true
     * @param falseExecutor  the command called if the predicate is false
     * @return the command
     * @throws IllegalArgumentException if the predicate is null
     * @throws IllegalArgumentException if either command is null
     */
    public static Executor switchExecutor(Predicate predicate, Executor trueExecutor, Executor falseExecutor) {
        return switchExecutorInternal(new Predicate[] { predicate }, new Executor[] { trueExecutor }, falseExecutor);
    }

    /**
     * Create a new Executor that calls one of the commands depending 
     * on the predicates. The command at array location 0 is called if the
     * predicate at array location 0 returned true. Each predicate is evaluated
     * until one returns true.
     * 
     * @param predicates  an array of predicates to check
     * @param commands  an array of commands to call
     * @return the command
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Executor switchExecutor(Predicate[] predicates, Executor[] commands) {
        return switchExecutorInternal(copy(predicates), copy(commands), null);
    }

    /**
     * Create a new Executor that calls one of the commands depending 
     * on the predicates. The command at array location 0 is called if the
     * predicate at array location 0 returned true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * command is called.
     * 
     * @param predicates  an array of predicates to check
     * @param commands  an array of commands to call
     * @param defaultExecutor  the default to call if no predicate matches
     * @return the command
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Executor switchExecutor(Predicate[] predicates, Executor[] commands, Executor defaultExecutor) {
        return switchExecutorInternal(copy(predicates), copy(commands), defaultExecutor);
    }
    
    /**
     * Create a new Executor that calls one of the commands depending 
     * on the predicates. 
     * <p>
     * The Map consists of Predicate keys and Executor values. A command 
     * is called if its matching predicate returns true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * command is called. The default command is set in the map with a 
     * null key. The ordering is that of the iterator() method on the entryset 
     * collection of the map.
     * 
     * @param predicatesAndExecutors  a map of predicates to commands
     * @return the command
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any command in the map is null
     * @throws ClassCastException  if the map elements are of the wrong type
     */
    public static Executor switchExecutor(Map predicatesAndExecutors) {
        Executor[] trs = null;
        Predicate[] preds = null;
        if (predicatesAndExecutors == null) {
            throw new IllegalArgumentException("SwitchExecutor: The predicate and command map must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Executor def = (Executor) predicatesAndExecutors.remove(null);
        int size = predicatesAndExecutors.size();
        trs = new Executor[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = predicatesAndExecutors.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = (Predicate) entry.getKey();
            trs[i] = (Executor) entry.getValue();
            i++;
        }
        return switchExecutorInternal(preds, trs, def);
    }

    /**
     * Validate input and create command
     */
    private static Executor switchExecutorInternal(Predicate[] predicates, Executor[] commands, Executor defaultExecutor) {
        validate(predicates);
        validate(commands);
        if (predicates.length != commands.length) {
            throw new IllegalArgumentException("SwitchExecutor: The predicate and command arrays must be the same size");
        }
        if (defaultExecutor == null) {
            defaultExecutor = nopExecutor();
        }
        return new SwitchExecutor(predicates, commands, defaultExecutor);
    }

    /**
     * Create a new Executor that uses the input object as a key to find the
     * command to call. 
     * <p>
     * The Map consists of object keys and Executor values. A command 
     * is called if the input object equals the key. If there is no match, the
     * default command is called. The default command is set in the map
     * using a null key.
     * 
     * @param objectsAndExecutors  a map of objects to commands
     * @return the command
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any command in the map is null
     */
    public static Executor switchMapExecutor(Map objectsAndExecutors) {
        Executor[] trs = null;
        Predicate[] preds = null;
        if (objectsAndExecutors == null) {
            throw new IllegalArgumentException("SwitchEqualsExecutor: The obejct and command map must not be null");
        }
        Executor def = (Executor) objectsAndExecutors.remove(null);
        int size = objectsAndExecutors.size();
        trs = new Executor[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = objectsAndExecutors.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = PredicateUtils.equalPredicate(entry.getKey());
            trs[i] = (Executor) entry.getValue();
            i++;
        }
        return switchExecutor(preds, trs, def);
    }

    /**
     * Copy method
     * 
     * @param predicates  the predicates to copy
     */
    private static Predicate[] copy(Predicate[] predicates) {
        if (predicates == null) {
            return null;
        }
        return (Predicate[]) predicates.clone();
    }
    
    /**
     * Validate method
     * 
     * @param predicates  the predicates to validate
     */
    private static void validate(Predicate[] predicates) {
        if (predicates == null) {
            throw new IllegalArgumentException("ExecutorUtils: The predicate array must not be null");
        }
        if (predicates.length < 1) {
            throw new IllegalArgumentException(
                "ExecutorUtils: At least 1 predicate must be specified in the predicate array, size was " + predicates.length);
        }
        for (int i = 0; i < predicates.length; i++) {
            if (predicates[i] == null) {
                throw new IllegalArgumentException("ExecutorUtils: The predicate array must not contain a null predicate, index " + i + " was null");
            }
        }
    }

    /**
     * Copy method
     * 
     * @param commands  the commands to copy
     */
    private static Executor[] copy(Executor[] commands) {
        if (commands == null) {
            return null;
        }
        return (Executor[]) commands.clone();
    }
    
    /**
     * Validate method
     * 
     * @param commands  the commands to validate
     */
    private static void validate(Executor[] commands) {
        if (commands == null) {
            throw new IllegalArgumentException("ExecutorUtils: The command array must not be null");
        }
        if (commands.length < 1) {
            throw new IllegalArgumentException(
                "ExecutorUtils: At least 1 command must be specified in the command array, size was " + commands.length);
        }
        for (int i = 0; i < commands.length; i++) {
            if (commands[i] == null) {
                throw new IllegalArgumentException("ExecutorUtils: The command array must not contain a null command, index " + i + " was null");
            }
        }
    }

    // ExceptionExecutor
    //----------------------------------------------------------------------------------

    /**
     * ExceptionExecutor always throws an exception
     */
    private static class ExceptionExecutor implements Executor, Serializable {

        /**
         * Constructor
         */
        private ExceptionExecutor() {
            super();
        }

        /**
         * Always throw an exception
         */
        public void execute(Object input) {
            throw new ExecutorException("ExceptionExecutor invoked");
        }
    }

    // NOPExecutor
    //----------------------------------------------------------------------------------

    /**
     * NOPExecutor does nothing
     */
    private static class NOPExecutor implements Executor, Serializable {

        /**
         * Constructor
         */
        private NOPExecutor() {
            super();
        }

        /**
         * Do nothing
         */
        public void execute(Object input) {
            // do nothing
        }
    }

    // TransformerExecutor
    //----------------------------------------------------------------------------------

    /**
     * TransformerExecutor returns the result of calling a Transformer.
     */
    private static class TransformerExecutor implements Executor, Serializable {

        private final Transformer iTransformer;

        /**
         * Constructor to store factory
         */
        private TransformerExecutor(Transformer transformer) {
            super();
            iTransformer = transformer;
        }

        /**
         * Return the result of calling the factory
         */
        public void execute(Object input) {
            try {
                iTransformer.transform(input);

            } catch (TransformerException ex) {
                throw new ExecutorException("TransformerExecutor: " + ex.getMessage(), ex);
            }
        }
    }

    // ChainedExecutor
    //----------------------------------------------------------------------------------

    /**
     * ChainedExecutor calls a list of commands.
     */
    private static class ChainedExecutor implements Executor, Serializable {

        private final Executor[] iExecutors;

        /**
         * Constructor to store params
         */
        private ChainedExecutor(Executor[] commands) {
            super();
            iExecutors = commands;
        }

        /**
         * Execute a list of commands
         */
        public void execute(Object input) {
            for (int i = 0; i < iExecutors.length; i++) {
                iExecutors[i].execute(input);
            }
        }
    }

    // SwitchExecutor
    //----------------------------------------------------------------------------------

    /**
     * SwitchExecutor calls the command whose predicate returns true.
     */
    private static class SwitchExecutor implements Executor, Serializable {

        private final Predicate[] iPredicates;
        private final Executor[] iExecutors;
        private final Executor iDefault;

        /**
         * Constructor to store params
         */
        private SwitchExecutor(Predicate[] predicates, Executor[] commands, Executor defaultExecutor) {
            super();
            iPredicates = predicates;
            iExecutors = commands;
            iDefault = defaultExecutor;
        }

        /**
         * Execute the command whose predicate returns true
         */
        public void execute(Object input) {
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(input) == true) {
                    iExecutors[i].execute(input);
                    return;
                }
            }
            iDefault.execute(input);
        }
    }

    // ForExecutor
    //----------------------------------------------------------------------------------

    /**
     * ForExecutor calls the command a fixed nunmber of times.
     */
    private static class ForExecutor implements Executor, Serializable {

        private final int iCount;
        private final Executor iExecutor;

        /**
         * Constructor to store params
         */
        private ForExecutor(int count, Executor command) {
            super();
            iCount = count;
            iExecutor = command;
        }

        /**
         * Execute the command count times
         */
        public void execute(Object input) {
            for (int i = 0; i < iCount; i++) {
                iExecutor.execute(input);
            }
        }
    }

    // WhileExecutor
    //----------------------------------------------------------------------------------

    /**
     * WhileExecutor calls the command until the predicate is false.
     */
    private static class WhileExecutor implements Executor, Serializable {

        private final Predicate iPredicate;
        private final Executor iExecutor;
        private final boolean iDoLoop;

        /**
         * Constructor to store params
         */
        private WhileExecutor(Predicate predicate, Executor command, boolean doLoop) {
            super();
            iPredicate = predicate;
            iExecutor = command;
            iDoLoop = doLoop;
        }

        /**
         * Execute the command until the predicate is false
         */
        public void execute(Object input) {
            if (iDoLoop) {
                iExecutor.execute(input);
            }
            while (iPredicate.evaluate(input)) {
                iExecutor.execute(input);
            }
        }
    }

}
