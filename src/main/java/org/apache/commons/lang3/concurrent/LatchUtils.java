package org.apache.commons.lang3.concurrent;


import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

/**
 * the utility class for {@link CountDownLatch}
 * Provide a safer, more user-friendly, and concise wrapper for CountDownLatch's creation, countDown(), and await() operations
 * To achieve separation between business logic and CountDownLatch
 */
public class LatchUtils {

    private static final ThreadLocal<List<TaskInfo>> THREADLOCAL = ThreadLocal.withInitial(LinkedList::new);

    private static final class TaskInfo {
        private Executor executor;
        private Runnable runnable;

        public TaskInfo(Executor executor, Runnable runnable) {
            this.executor = executor;
            this.runnable = runnable;
        }
    }

    /**
     * @param executor the threadpool for the task will run in
     * @param runnable the task
     */
    public static void submitTask(Executor executor, Runnable runnable) {
        THREADLOCAL.get().add(new TaskInfo(executor, runnable));
    }

    private static List<TaskInfo> popTask() {
        List<TaskInfo> taskInfos = THREADLOCAL.get();
        THREADLOCAL.remove();
        return taskInfos;
    }

    /**
     * start task and wait for task finish
     *
     * @param timeout
     * @return
     */
    public static boolean waitFor(Long timeout, TimeUnit timeUnit) {
        List<TaskInfo> taskInfos = popTask();
        if (taskInfos.isEmpty()) {
            return true;
        }

        CountDownLatch latch = new CountDownLatch(taskInfos.size());
        for (TaskInfo taskInfo : taskInfos) {
            Executor executor = taskInfo.executor;
            Runnable runnable = taskInfo.runnable;
            executor.execute(() -> {
                try {
                    runnable.run();
                } finally {
                    latch.countDown();
                }
            });
        }

        boolean await = false;
        try {
            if (timeout != null) {
                await = latch.await(timeout, timeUnit != null ? timeUnit : TimeUnit.SECONDS);
            } else {
                latch.await();
                await = true;
            }
        } catch (Exception e) {

        }

        return await;
    }

}
