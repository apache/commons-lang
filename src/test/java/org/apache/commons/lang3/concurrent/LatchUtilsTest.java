package org.apache.commons.lang3.concurrent;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests {@link LatchUtils}.
 */
class LatchUtilsTest extends AbstractLangTest {

    private ExecutorService executorService = Executors.newFixedThreadPool(2);

    @Test
    void testSubmitTask1() {
        LatchUtils.submitTask(executorService, () -> {
            System.out.println("task1");
        });
        LatchUtils.submitTask(executorService, () -> {
            System.out.println("task2");

        });
        LatchUtils.submitTask(executorService, () -> {
            System.out.println("task3");

        });
        assertTrue(LatchUtils.waitFor(1L, TimeUnit.SECONDS
        ));
    }
}