package test.callgraph.innerclass;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/4/4
 * @description:
 */
public class TestOutClass {
    private String testString;

    private List<String> testStringList1;

    private Map<Integer, String> testMap1;

    private Map<Integer, Map<Integer, String>> testMap2;

    private Integer testInteger;

    private TestInClass.TestInInnerData testInInnerData;

    private List<TestInClass.TestInInnerData> testInInnerDataList;

    public String getTestString() {
        return testString;
    }

    public void setTestString(String testString) {
        this.testString = testString;
    }

    public List<String> getTestStringList1() {
        return testStringList1;
    }

    public void setTestStringList1(List<String> testStringList1) {
        this.testStringList1 = testStringList1;
    }

    public Map<Integer, String> getTestMap1() {
        return testMap1;
    }

    public void setTestMap1(Map<Integer, String> testMap1) {
        this.testMap1 = testMap1;
    }

    public Map<Integer, Map<Integer, String>> getTestMap2() {
        return testMap2;
    }

    public void setTestMap2(Map<Integer, Map<Integer, String>> testMap2) {
        this.testMap2 = testMap2;
    }

    public Integer getTestInteger() {
        return testInteger;
    }

    public void setTestInteger(Integer testInteger) {
        this.testInteger = testInteger;
    }

    public TestInClass.TestInInnerData getTestInInnerData() {
        return testInInnerData;
    }

    public void setTestInInnerData(TestInClass.TestInInnerData testInInnerData) {
        this.testInInnerData = testInInnerData;
    }

    public List<TestInClass.TestInInnerData> getTestInInnerDataList() {
        return testInInnerDataList;
    }

    public void setTestInInnerDataList(List<TestInClass.TestInInnerData> testInInnerDataList) {
        this.testInInnerDataList = testInInnerDataList;
    }
}
