package test.call_graph.rpc_annotation;

import test.call_graph.rpc_annotation.req.TestReq1;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */
public class RpcCall2 {

    public static String call2(Object req) {
        return "";
    }

    public static String str() {
        return "";
    }

    public static TestReq1 genReq() {
        return new TestReq1();
    }
}
