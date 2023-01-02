package org.apache.commons.lang3.concurrent;

import java.util.EventObject;

public class StateChangeEvent extends EventObject {

    private final AbstractCircuitBreaker.State state;

    public StateChangeEvent(Object source, AbstractCircuitBreaker.State state) {
        super(source);
        this.state = state;
    }

    public AbstractCircuitBreaker.State getState() {
        return state;
    }
}
