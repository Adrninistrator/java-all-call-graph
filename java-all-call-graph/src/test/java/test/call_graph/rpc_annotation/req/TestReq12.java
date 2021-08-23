package test.call_graph.rpc_annotation.req;

import test.call_graph.rpc_annotation.annotation.Annotation2;
import test.call_graph.rpc_annotation.annotation.FieldType;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */

@Annotation2(value1 = "555-", value2 = "vvv2")
public class TestReq12 extends TestReq1 {

    @FieldType(type = FieldType.Type.TIMEOUT)
    private long toValue;

    @Override
    public long getToValue() {
        return toValue;
    }

    @Override
    public void setToValue(long toValue) {
        this.toValue = toValue;
    }
}
