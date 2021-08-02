package test.call_graph.implement;

import java.security.SecureRandom;

/**
 * @author adrninistrator
 * @date 2021/8/1
 * @description:
 */

public class ImplClass2 implements Interface1 {
    @Override
    public void f1() {
        new SecureRandom().nextInt();
    }

    @Override
    public void f2() {
        try {
            Thread.sleep(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
