package test.callgraph.field.dto;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/5
 * @description:
 */
public class TestFieldDto1 {

    private String fileInfo;

    private List<String> strList1;

    public String getFileInfo() {
        return fileInfo;
    }

    public void setFileInfo(String fileInfo) {
        this.fileInfo = fileInfo;
    }

    public List<String> getStrList1() {
        return strList1;
    }

    public void setStrList1(List<String> strList1) {
        this.strList1 = strList1;
    }
}
