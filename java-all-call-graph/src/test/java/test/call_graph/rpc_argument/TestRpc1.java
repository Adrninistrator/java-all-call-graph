package test.call_graph.rpc_argument;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */
public class TestRpc1 {

    public void test1() {
        RpcCall1.call1("0.0.1", "1234", "{}", 1234);
    }

    public void test2() {
        RpcCall1.call1("0.0.1", str(), "{}", 2000L);
    }

    public void test3() {
        RpcCall1.call1(str(), "1234", "{}", 3000L);
    }

    public void test4() {
        String version = str();
        RpcCall1.call1(version, "1234", "{}", 4000L);
    }

    public void test5() {
        RpcCall1.call1("1234", "1234", "{}", 5000L);
    }

    private String str() {
        return "";
    }
}
