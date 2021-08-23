package test.call_graph.rpc_annotation.req;

import test.call_graph.rpc_annotation.annotation.Annotation1;
import test.call_graph.rpc_annotation.annotation.AnnotationTimeout1;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */

@Annotation1(version = "888", serviceId = "ss2", value3 = 3456)
@AnnotationTimeout1(timeout = 6666L)
public class TestReq2 {

    private String data;

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }
}
