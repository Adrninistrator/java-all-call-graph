package test.call_graph.inner_class;

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

    public static class TestInInnerData {
        private String testString;

        private Integer testInteger;
    }
}
