package org.apache.commons.lang3.exception;

import org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Set;

public abstract class ContextLabels extends RuntimeException implements ExceptionContext{

    /** The context where the data is stored. */
    protected ExceptionContext exceptionContext;

    protected ContextLabels() {
        this.exceptionContext =  new DefaultExceptionContext();
    }

    public ContextLabels(String message, Throwable cause) {
        super(message, cause);
        exceptionContext = new DefaultExceptionContext();
    }

    public ContextLabels(Throwable cause) {
        super(cause);
        exceptionContext =  new DefaultExceptionContext();
    }

    public ContextLabels(String message) {
        super(message);
        exceptionContext =  new DefaultExceptionContext();
    }


    @Override
    public abstract ExceptionContext addContextValue(String label, Object value) ;

    @Override
    public abstract ExceptionContext setContextValue(String label, Object value) ;

    @Override
    public abstract List<Object> getContextValues(String label);

    @Override
    public abstract Object getFirstContextValue(String label);

    @Override
    public Set<String> getContextLabels() {
        return exceptionContext.getContextLabels();
    }

    @Override
    public abstract List<Pair<String, Object>> getContextEntries();

    @Override
    public abstract String getFormattedExceptionMessage(String baseMessage);
}
