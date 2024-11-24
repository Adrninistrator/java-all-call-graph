package test.callgraph.error;

/**
 * @author adrninistrator
 * @date 2024/11/18
 * @description:
 */
public class TestLoop2 {
    private static long time;

    public void test() {
        Thread daemon = new Thread(() -> {
            while (true) {
                time = System.currentTimeMillis();
            }
        });
        daemon.start();
    }
}
