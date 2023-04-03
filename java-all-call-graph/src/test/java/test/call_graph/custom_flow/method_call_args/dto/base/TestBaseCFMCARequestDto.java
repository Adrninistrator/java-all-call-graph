package test.call_graph.custom_flow.method_call_args.dto.base;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public abstract class TestBaseCFMCARequestDto {
    protected String data;

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }
}
