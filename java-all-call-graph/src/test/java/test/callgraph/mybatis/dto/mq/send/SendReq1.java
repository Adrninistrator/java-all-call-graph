package test.callgraph.mybatis.dto.mq.send;

/**
 * @author adrninistrator
 * @date 2021/8/10
 * @description:
 */

public class SendReq1 {

    private String data;

    private String data2;

    private long toValue;

    public SendReq1() {
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public String getData2() {
        return data2;
    }

    public void setData2(String data2) {
        this.data2 = data2;
    }

    public long getTimeoutValue() {
        return toValue;
    }

    public void setTimeoutValue(long timeoutValue) {
        this.toValue = timeoutValue;
    }
}
