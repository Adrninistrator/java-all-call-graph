package test.callgraph.innerclass;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/4
 * @description:
 */
public class TestInClass {
    private String testString;

    private Integer testInteger;

    private TestInInnerData testInInnerData;

    private List<TestInInnerData> testInInnerDataList;

    private TestInInnerData.TestInInnerData2 testInInnerData2;

    private List<TestInInnerData.TestInInnerData2> testInInnerDataList2;

    public static class TestInInnerData {
        private String testString;

        private Integer testInteger;

        public static class TestInInnerData2 {
            private String testString;

            private Integer testInteger;

            public String getTestString() {
                return testString;
            }

            public void setTestString(String testString) {
                this.testString = testString;
            }

            public Integer getTestInteger() {
                return testInteger;
            }

            public void setTestInteger(Integer testInteger) {
                this.testInteger = testInteger;
            }
        }

        public String getTestString() {
            return testString;
        }

        public void setTestString(String testString) {
            this.testString = testString;
        }

        public Integer getTestInteger() {
            return testInteger;
        }

        public void setTestInteger(Integer testInteger) {
            this.testInteger = testInteger;
        }
    }

    public String getTestString() {
        return testString;
    }

    public void setTestString(String testString) {
        this.testString = testString;
    }

    public Integer getTestInteger() {
        return testInteger;
    }

    public void setTestInteger(Integer testInteger) {
        this.testInteger = testInteger;
    }

    public TestInInnerData getTestInInnerData() {
        return testInInnerData;
    }

    public void setTestInInnerData(TestInInnerData testInInnerData) {
        this.testInInnerData = testInInnerData;
    }

    public List<TestInInnerData> getTestInInnerDataList() {
        return testInInnerDataList;
    }

    public void setTestInInnerDataList(List<TestInInnerData> testInInnerDataList) {
        this.testInInnerDataList = testInInnerDataList;
    }

    public TestInInnerData.TestInInnerData2 getTestInInnerData2() {
        return testInInnerData2;
    }

    public void setTestInInnerData2(TestInInnerData.TestInInnerData2 testInInnerData2) {
        this.testInInnerData2 = testInInnerData2;
    }

    public List<TestInInnerData.TestInInnerData2> getTestInInnerDataList2() {
        return testInInnerDataList2;
    }

    public void setTestInInnerDataList2(List<TestInInnerData.TestInInnerData2> testInInnerDataList2) {
        this.testInInnerDataList2 = testInInnerDataList2;
    }
}
