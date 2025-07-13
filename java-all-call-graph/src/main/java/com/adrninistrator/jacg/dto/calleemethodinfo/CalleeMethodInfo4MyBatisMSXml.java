package com.adrninistrator.jacg.dto.calleemethodinfo;

/**
 * @author adrninistrator
 * @date 2025/6/12
 * @description: 被调用方法对应MyBatis XML（支持MySQL）
 */
public class CalleeMethodInfo4MyBatisMSXml extends BaseCalleeMethodInfo {

    public static final String TYPE = "myb_ms_xml";

    // XML文件路径
    private String xmlFilePath;

    public CalleeMethodInfo4MyBatisMSXml() {
        type = TYPE;
    }

    public String getXmlFilePath() {
        return xmlFilePath;
    }

    public void setXmlFilePath(String xmlFilePath) {
        this.xmlFilePath = xmlFilePath;
    }
}
