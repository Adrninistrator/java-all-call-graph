package test.call_graph.extend_complex2;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public class EC2BService extends EC2AService {

    protected void get() {
        getB();
    }

    private void getB() {
        System.out.println("b");
    }
}
