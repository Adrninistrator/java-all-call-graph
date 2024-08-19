package test.callgraph.branch.dto;

/**
 * @author adrninistrator
 * @date 2024/8/15
 * @description:
 */
public class BRD {

    private String resultCode;
    private String resultMsg;

    private Object data;

    public String getResultCode() {
        return resultCode;
    }

    public void setResultCode(String resultCode) {
        this.resultCode = resultCode;
    }

    public String getResultMsg() {
        return resultMsg;
    }

    public void setResultMsg(String resultMsg) {
        this.resultMsg = resultMsg;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }
}
