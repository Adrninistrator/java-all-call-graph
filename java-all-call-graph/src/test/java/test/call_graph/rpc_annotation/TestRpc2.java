package test.call_graph.rpc_annotation;

import test.call_graph.method_call.TestMCCallee;
import test.call_graph.rpc_annotation.req.TestReq1;
import test.call_graph.rpc_annotation.req.TestReq11;
import test.call_graph.rpc_annotation.req.TestReq12;
import test.call_graph.rpc_annotation.req.TestReq2;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */
public class TestRpc2 {

    private TestReq1 globalTestReq1;
    private static TestReq1 globalStaticTestReq1;

    static {
        globalStaticTestReq1 = new TestReq1();
        globalStaticTestReq1.setData("");
    }

    public TestRpc2() {
        globalTestReq1 = new TestReq1();
        globalTestReq1.setData("");
    }

    public void test1() {
        RpcCall2.call2(new TestReq1());
    }

    public void test11() {
        RpcCall2.call2(new TestReq11());
    }

    public void test12() {
        RpcCall2.call2(new TestReq12());
    }

    public void test13() {
        RpcCall2.
                call2(new TestReq2());
    }

    public void test14() {
        RpcCall2.

                call2(new TestReq1("", 1L));
    }

    public void test1a1() {
        RpcCall2.call2(genReq());
    }

    public void test1a2() {
        TestReq1 testReq1 = new TestReq1();
        RpcCall2.call2(genReq());
    }

    public void test1b() {
        TestReq1 testReq1 = new TestReq1();
        RpcCall2.
                call2(
                        RpcCall2.
                                genReq());
    }

    public void test2() {
        TestReq1 testReq1 = new TestReq1();
        RpcCall2.
                call2(testReq1);
    }

    public void test3() {
        TestReq1 testReq1 = new TestReq1();
        testReq1.setData("");
        TestMCCallee testMCCallee = new TestMCCallee();
        str();
        RpcCall2.call2(testReq1);
        System.out.println(testMCCallee);
    }

    public void test3a() {
        TestReq1 testReq1 = new TestReq1();
        operReq(testReq1);
        str();
        RpcCall2.call2(testReq1);
    }

    public void test3b() {
        TestReq1 testReq1 = genReq();
        str();
        RpcCall2.call2(testReq1);
    }

    public void test3c() {
        TestReq1 testReq1 = RpcCall2.genReq();
        str();
        RpcCall2.call2(testReq1);
    }

    public void test3d() {
        Object testReq1 = genReq2();
        str();
        RpcCall2.call2(testReq1);
    }

    public void test3e() {
        Object o = genReq2();
        TestReq1 testReq1 = (TestReq1) o;
        str();
        RpcCall2.call2(testReq1);
    }

    public void test3f() {
        Object o = genReq2();
        RpcCall2.call2((TestReq1) o);
    }

    public void test4(int a, TestReq1 testReq1, String s) {
        RpcCall2.call2(testReq1);
    }

    public static void test4a(int a, TestReq1 testReq1) {
        RpcCall2.call2(testReq1);
    }

    public void test4b(int a, String s, TestReq1 testReq1) {
        String str = RpcCall2.str();
        RpcCall2.call2(testReq1);
    }

    public void test5(int a, String s, TestReq1 testReq1) {
        String str = RpcCall2.str();
        RpcCall2.call2(globalTestReq1);
    }

    public void test6(int a, String s, TestReq1 testReq1) {
        String str = RpcCall2.str();
        RpcCall2.call2(globalStaticTestReq1);
    }

    private String str() {
        return "";
    }

    private TestReq1 genReq() {
        return new TestReq1();
    }

    private Object genReq2() {
        return new TestReq1();
    }

    private void operReq(TestReq1 req) {
        req.setData("");
    }
}
