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
 * <code>ClosureUtils</code> provides reference implementations and utilities
 * for the Closure pattern interface. The supplied commands are:
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
 * @version $Id: ClosureUtils.java,v 1.1 2002/11/05 16:44:28 bayard Exp $
 */
public class ClosureUtils {

    /**
     * A Closure that always throws an exception
     */
    private static final Closure EXCEPTION_COMMAND = new ExceptionClosure();
    /**
     * A Closure that does nothing
     */
    private static final Closure NOP_COMMAND = new NOPClosure();

    /**
     * Restrictive constructor
     */
    protected ClosureUtils() {
        super();
    }

    /**
     * Gets a Closure that always throws an exception.
     * This could be useful during testing as a placeholder.
     *
     * @return the command
     */
    public static Closure exceptionClosure() {
        return EXCEPTION_COMMAND;
    }

    /**
     * Gets a Closure that will do nothing.
     * This could be useful during testing as a placeholder.
     *
     * @return the command
     */
    public static Closure nopClosure() {
        return NOP_COMMAND;
    }

    /**
     * Creates a Closure that calls a Factory each time the transformer is used.
     * The transformer will return the value returned by the factory.
     *
     * @param transformer  the transformer to run each time in the command
     * @return the command.
     */
    public static Closure asClosure(Transformer transformer) {
        if (transformer == null) {
            throw new IllegalArgumentException("TransformerClosure: The transformer must not be null");
        }
        return new TransformerClosure(transformer);
    }

    /**
     * Creates a Closure that will call the command <code>count</code> times.
     *
     * @param count  the number of times to loop
     * @param command  the command to call repeatedly
     * @return the command
     * @throws IllegalArgumentException if either argument is null
     */
    public static Closure forClosure(int count, Closure command) {
        if (count < 0) {
            throw new IllegalArgumentException("ForClosure: The loop count must not be less than zero, it was " + count);
        }
        if (command == null) {
            throw new IllegalArgumentException("ForClosure: The command must not be null");
        }
        return new ForClosure(count, command);
    }

    /**
     * Creates a Closure that will call the command repeatedly until the 
     * predicate returns false.
     *
     * @param predicate  the predicate to use as an end of loop test
     * @param command  the command to call repeatedly
     * @return the command
     * @throws IllegalArgumentException if either argument is null
     */
    public static Closure whileClosure(Predicate predicate, Closure command) {
        if (predicate == null) {
            throw new IllegalArgumentException("WhileClosure: The predicate must not be null");
        }
        if (command == null) {
            throw new IllegalArgumentException("WhileClosure: The command must not be null");
        }
        return new WhileClosure(predicate, command, false);
    }

    /**
     * Creates a Closure that will call the command once and then repeatedly
     * until the predicate returns false.
     *
     * @param command  the command to call repeatedly
     * @param predicate  the predicate to use as an end of loop test
     * @return the command
     * @throws IllegalArgumentException if either argument is null
     */
    public static Closure doWhileClosure(Closure command, Predicate predicate) {
        if (command == null) {
            throw new IllegalArgumentException("DoWhileClosure: The command must not be null");
        }
        if (predicate == null) {
            throw new IllegalArgumentException("DoWhileClosure: The predicate must not be null");
        }
        return new WhileClosure(predicate, command, true);
    }

    /**
     * Creates a Closure that will invoke a specific method on the command's
     * input object by reflection.
     *
     * @param methodName  the name of the method
     * @return the command
     * @throws IllegalArgumentException if the method name is null
     */
    public static Closure invokerClosure(String methodName) {
        return asClosure(TransformerUtils.invokerTransformer(methodName, null, null));
    }

    /**
     * Creates a Closure that will invoke a specific method on the command's
     * input object by reflection.
     *
     * @param methodName  the name of the method
     * @param paramTypes  the parameter types
     * @param args  the arguments
     * @return the command
     * @throws IllegalArgumentException if the method name is null
     * @throws IllegalArgumentException if the paramTypes and args don't match
     */
    public static Closure invokerClosure(String methodName, Class[] paramTypes, Object[] args) {
        return asClosure(TransformerUtils.invokerTransformer(methodName, paramTypes, args));
    }

    /**
     * Create a new Closure that calls two Closures, passing the result of
     * the first into the second.
     * 
     * @param command1  the first command
     * @param command2  the second command
     * @return the command
     * @throws IllegalArgumentException if either command is null
     */
    public static Closure chainedClosure(Closure command1, Closure command2) {
        Closure[] commands = new Closure[] { command1, command2 };
        validate(commands);
        return new ChainedClosure(commands);
    }

    /**
     * Create a new Closure that calls each command in turn, passing the 
     * result into the next command.
     * 
     * @param commands  an array of commands to chain
     * @return the command
     * @throws IllegalArgumentException if the commands array is null
     * @throws IllegalArgumentException if the commands array has 0 elements
     * @throws IllegalArgumentException if any command in the array is null
     */
    public static Closure chainedClosure(Closure[] commands) {
        commands = copy(commands);
        validate(commands);
        return new ChainedClosure(commands);
    }

    /**
     * Create a new Closure that calls each command in turn, passing the 
     * result into the next command. The ordering is that of the iterator()
     * method on the collection.
     * 
     * @param commands  a collection of commands to chain
     * @return the command
     * @throws IllegalArgumentException if the commands collection is null
     * @throws IllegalArgumentException if the commands collection is empty
     * @throws IllegalArgumentException if any command in the collection is null
     */
    public static Closure chainedClosure(Collection commands) {
        if (commands == null) {
            throw new IllegalArgumentException("ChainedClosure: The command collection must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Closure[] cmds = new Closure[commands.size()];
        int i = 0;
        for (Iterator it = commands.iterator(); it.hasNext();) {
            cmds[i++] = (Closure) it.next();
        }
        validate(cmds);
        return new ChainedClosure(cmds);
    }

    /**
     * Create a new Closure that calls one of two commands depending 
     * on the specified predicate.
     * 
     * @param predicate  the predicate to switch on
     * @param trueClosure  the command called if the predicate is true
     * @param falseClosure  the command called if the predicate is false
     * @return the command
     * @throws IllegalArgumentException if the predicate is null
     * @throws IllegalArgumentException if either command is null
     */
    public static Closure switchClosure(Predicate predicate, Closure trueClosure, Closure falseClosure) {
        return switchClosureInternal(new Predicate[] { predicate }, new Closure[] { trueClosure }, falseClosure);
    }

    /**
     * Create a new Closure that calls one of the commands depending 
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
    public static Closure switchClosure(Predicate[] predicates, Closure[] commands) {
        return switchClosureInternal(copy(predicates), copy(commands), null);
    }

    /**
     * Create a new Closure that calls one of the commands depending 
     * on the predicates. The command at array location 0 is called if the
     * predicate at array location 0 returned true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * command is called.
     * 
     * @param predicates  an array of predicates to check
     * @param commands  an array of commands to call
     * @param defaultClosure  the default to call if no predicate matches
     * @return the command
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Closure switchClosure(Predicate[] predicates, Closure[] commands, Closure defaultClosure) {
        return switchClosureInternal(copy(predicates), copy(commands), defaultClosure);
    }
    
    /**
     * Create a new Closure that calls one of the commands depending 
     * on the predicates. 
     * <p>
     * The Map consists of Predicate keys and Closure values. A command 
     * is called if its matching predicate returns true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * command is called. The default command is set in the map with a 
     * null key. The ordering is that of the iterator() method on the entryset 
     * collection of the map.
     * 
     * @param predicatesAndClosures  a map of predicates to commands
     * @return the command
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any command in the map is null
     * @throws ClassCastException  if the map elements are of the wrong type
     */
    public static Closure switchClosure(Map predicatesAndClosures) {
        Closure[] trs = null;
        Predicate[] preds = null;
        if (predicatesAndClosures == null) {
            throw new IllegalArgumentException("SwitchClosure: The predicate and command map must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Closure def = (Closure) predicatesAndClosures.remove(null);
        int size = predicatesAndClosures.size();
        trs = new Closure[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = predicatesAndClosures.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = (Predicate) entry.getKey();
            trs[i] = (Closure) entry.getValue();
            i++;
        }
        return switchClosureInternal(preds, trs, def);
    }

    /**
     * Validate input and create command
     */
    private static Closure switchClosureInternal(Predicate[] predicates, Closure[] commands, Closure defaultClosure) {
        validate(predicates);
        validate(commands);
        if (predicates.length != commands.length) {
            throw new IllegalArgumentException("SwitchClosure: The predicate and command arrays must be the same size");
        }
        if (defaultClosure == null) {
            defaultClosure = nopClosure();
        }
        return new SwitchClosure(predicates, commands, defaultClosure);
    }

    /**
     * Create a new Closure that uses the input object as a key to find the
     * command to call. 
     * <p>
     * The Map consists of object keys and Closure values. A command 
     * is called if the input object equals the key. If there is no match, the
     * default command is called. The default command is set in the map
     * using a null key.
     * 
     * @param objectsAndClosures  a map of objects to commands
     * @return the command
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any command in the map is null
     */
    public static Closure switchMapClosure(Map objectsAndClosures) {
        Closure[] trs = null;
        Predicate[] preds = null;
        if (objectsAndClosures == null) {
            throw new IllegalArgumentException("SwitchEqualsClosure: The obejct and command map must not be null");
        }
        Closure def = (Closure) objectsAndClosures.remove(null);
        int size = objectsAndClosures.size();
        trs = new Closure[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = objectsAndClosures.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = PredicateUtils.equalPredicate(entry.getKey());
            trs[i] = (Closure) entry.getValue();
            i++;
        }
        return switchClosure(preds, trs, def);
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
            throw new IllegalArgumentException("ClosureUtils: The predicate array must not be null");
        }
        if (predicates.length < 1) {
            throw new IllegalArgumentException(
                "ClosureUtils: At least 1 predicate must be specified in the predicate array, size was " + predicates.length);
        }
        for (int i = 0; i < predicates.length; i++) {
            if (predicates[i] == null) {
                throw new IllegalArgumentException("ClosureUtils: The predicate array must not contain a null predicate, index " + i + " was null");
            }
        }
    }

    /**
     * Copy method
     * 
     * @param commands  the commands to copy
     */
    private static Closure[] copy(Closure[] commands) {
        if (commands == null) {
            return null;
        }
        return (Closure[]) commands.clone();
    }
    
    /**
     * Validate method
     * 
     * @param commands  the commands to validate
     */
    private static void validate(Closure[] commands) {
        if (commands == null) {
            throw new IllegalArgumentException("ClosureUtils: The command array must not be null");
        }
        if (commands.length < 1) {
            throw new IllegalArgumentException(
                "ClosureUtils: At least 1 command must be specified in the command array, size was " + commands.length);
        }
        for (int i = 0; i < commands.length; i++) {
            if (commands[i] == null) {
                throw new IllegalArgumentException("ClosureUtils: The command array must not contain a null command, index " + i + " was null");
            }
        }
    }

    // ExceptionClosure
    //----------------------------------------------------------------------------------

    /**
     * ExceptionClosure always throws an exception
     */
    private static class ExceptionClosure implements Closure, Serializable {

        /**
         * Constructor
         */
        private ExceptionClosure() {
            super();
        }

        /**
         * Always throw an exception
         */
        public void execute(Object input) {
            throw new ClosureException("ExceptionClosure invoked");
        }
    }

    // NOPClosure
    //----------------------------------------------------------------------------------

    /**
     * NOPClosure does nothing
     */
    private static class NOPClosure implements Closure, Serializable {

        /**
         * Constructor
         */
        private NOPClosure() {
            super();
        }

        /**
         * Do nothing
         */
        public void execute(Object input) {
            // do nothing
        }
    }

    // TransformerClosure
    //----------------------------------------------------------------------------------

    /**
     * TransformerClosure returns the result of calling a Transformer.
     */
    private static class TransformerClosure implements Closure, Serializable {

        private final Transformer iTransformer;

        /**
         * Constructor to store factory
         */
        private TransformerClosure(Transformer transformer) {
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
                throw new ClosureException("TransformerClosure: " + ex.getMessage(), ex);
            }
        }
    }

    // ChainedClosure
    //----------------------------------------------------------------------------------

    /**
     * ChainedClosure calls a list of commands.
     */
    private static class ChainedClosure implements Closure, Serializable {

        private final Closure[] iClosures;

        /**
         * Constructor to store params
         */
        private ChainedClosure(Closure[] commands) {
            super();
            iClosures = commands;
        }

        /**
         * Execute a list of commands
         */
        public void execute(Object input) {
            for (int i = 0; i < iClosures.length; i++) {
                iClosures[i].execute(input);
            }
        }
    }

    // SwitchClosure
    //----------------------------------------------------------------------------------

    /**
     * SwitchClosure calls the command whose predicate returns true.
     */
    private static class SwitchClosure implements Closure, Serializable {

        private final Predicate[] iPredicates;
        private final Closure[] iClosures;
        private final Closure iDefault;

        /**
         * Constructor to store params
         */
        private SwitchClosure(Predicate[] predicates, Closure[] commands, Closure defaultClosure) {
            super();
            iPredicates = predicates;
            iClosures = commands;
            iDefault = defaultClosure;
        }

        /**
         * Execute the command whose predicate returns true
         */
        public void execute(Object input) {
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(input) == true) {
                    iClosures[i].execute(input);
                    return;
                }
            }
            iDefault.execute(input);
        }
    }

    // ForClosure
    //----------------------------------------------------------------------------------

    /**
     * ForClosure calls the command a fixed nunmber of times.
     */
    private static class ForClosure implements Closure, Serializable {

        private final int iCount;
        private final Closure iClosure;

        /**
         * Constructor to store params
         */
        private ForClosure(int count, Closure command) {
            super();
            iCount = count;
            iClosure = command;
        }

        /**
         * Execute the command count times
         */
        public void execute(Object input) {
            for (int i = 0; i < iCount; i++) {
                iClosure.execute(input);
            }
        }
    }

    // WhileClosure
    //----------------------------------------------------------------------------------

    /**
     * WhileClosure calls the command until the predicate is false.
     */
    private static class WhileClosure implements Closure, Serializable {

        private final Predicate iPredicate;
        private final Closure iClosure;
        private final boolean iDoLoop;

        /**
         * Constructor to store params
         */
        private WhileClosure(Predicate predicate, Closure command, boolean doLoop) {
            super();
            iPredicate = predicate;
            iClosure = command;
            iDoLoop = doLoop;
        }

        /**
         * Execute the command until the predicate is false
         */
        public void execute(Object input) {
            if (iDoLoop) {
                iClosure.execute(input);
            }
            while (iPredicate.evaluate(input)) {
                iClosure.execute(input);
            }
        }
    }

}
