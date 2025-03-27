package test.callgraph.methodcall;

/**
 * @author adrninistrator
 * @date 2025/3/19
 * @description:
 */
public class TestCalleeThis {

    public void test() {
        notFound(null);
        this.notFound(null);
    }

    private void notFound(Long l) {
    }
}
