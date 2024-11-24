package test.callgraph.field;

import com.fasterxml.jackson.annotation.JsonProperty;
import test.callgraph.field.dto.TestFieldDto1;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/9/17
 * @description:
 */
public class TestField1 {
    public TestField2 testField2a = new TestField2();

    public TestField2 testField2b = new TestField2();

    private TestFieldDto1 testFieldDto1;

    @JsonProperty("vvv-1")
    private String data;

    @JsonProperty("data")
    private BigDecimal bigDecimal;

    private int int1;

    private static String STRING1 = "111";

    private List<TestField2> testField2List;

    private Map<String, TestField2> testField2Map1;

    private Map<TestField2, Map<TestField2, String>> testField2Map2;

    public void test() {
        testField2a.data = "a";

        System.out.println(testField2a.data);

        data = "b";
        bigDecimal = BigDecimal.ONE;
        int1 = 123;

        System.out.println(data);
        System.out.println(bigDecimal);
        System.out.println(int1);
    }

    public void test1() {
        data = "111";

        TestField1 testField1 = new TestField1();
        testField1.data = "222";
        System.out.println(data);
        System.out.println(testField1.data);
    }

    public void test2() {
        System.out.println(STRING1);
        STRING1 = "222";
        System.out.println(STRING1);

        System.out.println(TestField2.STRING1);
        TestField2.STRING1 = "bbb";
        System.out.println(TestField2.STRING1);
    }

    public void test3() {
        System.out.println(TestField2.INSTANCE);
    }

    public void test4() {
        TestField2 testField2;

        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            testField2 = testField2a;
            testField2.test1();
        } else {
            testField2 = testField2b;
            testField2.test1();
        }
        testField2.test1();
    }

    public TestFieldDto1 getTestFieldDto1() {
        return testFieldDto1;
    }

    public void setTestFieldDto1(TestFieldDto1 testFieldDto1) {
        this.testFieldDto1 = testFieldDto1;
    }

    public TestField2 getTestField2a() {
        return testField2a;
    }

    public void setTestField2a(TestField2 testField2a) {
        this.testField2a = testField2a;
    }

    public TestField2 getTestField2b() {
        return testField2b;
    }

    public void setTestField2b(TestField2 testField2b) {
        this.testField2b = testField2b;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public BigDecimal getBigDecimal() {
        return bigDecimal;
    }

    public void setBigDecimal(BigDecimal bigDecimal) {
        this.bigDecimal = bigDecimal;
    }

    public int getInt1() {
        return int1;
    }

    public void setInt1(int int1) {
        this.int1 = int1;
    }

    public static String getSTRING1() {
        return STRING1;
    }

    public static void setSTRING1(String STRING1) {
        TestField1.STRING1 = STRING1;
    }

    public List<TestField2> getTestField2List() {
        return testField2List;
    }

    public void setTestField2List(List<TestField2> testField2List) {
        this.testField2List = testField2List;
    }

    public Map<String, TestField2> getTestField2Map1() {
        return testField2Map1;
    }

    public void setTestField2Map1(Map<String, TestField2> testField2Map1) {
        this.testField2Map1 = testField2Map1;
    }

    public Map<TestField2, Map<TestField2, String>> getTestField2Map2() {
        return testField2Map2;
    }

    public void setTestField2Map2(Map<TestField2, Map<TestField2, String>> testField2Map2) {
        this.testField2Map2 = testField2Map2;
    }
}
