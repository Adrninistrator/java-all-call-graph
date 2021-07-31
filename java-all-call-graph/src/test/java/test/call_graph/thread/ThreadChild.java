package test.call_graph.thread;

public class ThreadChild extends Thread {

    @Override
    public void run() {
        System.out.println(System.currentTimeMillis());
    }
}
