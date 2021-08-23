package test.call_graph.rpc_annotation.req;

import test.call_graph.rpc_annotation.annotation.Annotation1;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */

@Annotation1(version = "123", serviceId = "ss1", value3 = 3456)
public class TestReq1 {

    private String data;

    private long toValue;

    public TestReq1() {
    }

    public TestReq1(String data, long toValue) {
        this.data = data;
        this.toValue = toValue;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public long getToValue() {
        return toValue;
    }

    public void setToValue(long toValue) {
        this.toValue = toValue;
    }
}
