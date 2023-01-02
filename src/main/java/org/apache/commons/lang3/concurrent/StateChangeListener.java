package org.apache.commons.lang3.concurrent;

import java.util.EventListener;

public interface StateChangeListener extends EventListener {

    void stateChange(StateChangeEvent evt);

}
