package org.apache.commons.lang3.vm;

import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.*;

public class JVMLaunchersTest implements Serializable
{
    @Test
    public void newJvm()
            throws JVMException, IOException, ClassNotFoundException
    {
        System.out.println("--- vm test ---");
        JVMLauncher<Integer> launcher = JVMLaunchers.<Integer>newJvm()
                .setCallable(new VmCallable<Integer>(){
                    @Override
                    public Integer call()
                            throws Exception
                    {
                        System.out.println("************ Compile start ***************");
                        TimeUnit.SECONDS.sleep(1);
                        System.out.println("************ Compile stop ***************");
                        return 1;
                    }
                })
                .addUserjars(Arrays.<URL>asList())
                .setConsole(new Consumer<String>() {
                    @Override
                    public void accept(String msg)
                    {
                        System.err.println(msg);
                    }
                })
                .build();

        VmFuture<Integer> out = launcher.startAndGet();
        Assert.assertEquals(out.get().intValue(), 1);
    }
}