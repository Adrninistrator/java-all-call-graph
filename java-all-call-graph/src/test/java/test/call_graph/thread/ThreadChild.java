package test.call_graph.thread;

/**
 * @author adrninistrator
 * @date 2021/7/28
 * @description:
 */

public class ThreadChild extends Thread {

    @Override
    public void run() {
        System.out.println(System.currentTimeMillis());
    }
}
