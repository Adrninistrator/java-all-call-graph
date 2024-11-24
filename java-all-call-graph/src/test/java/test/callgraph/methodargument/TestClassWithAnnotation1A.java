package test.callgraph.methodargument;

import test.callgraph.annotation.TestAnnotation1;
import test.callgraph.field.TestField1;

/**
 * @author adrninistrator
 * @date 2023/4/8
 * @description:
 */
@TestAnnotation1(name = "1A", desc = "desc-1A")
public class TestClassWithAnnotation1A {

    private int int1;

    private String data;

    private TestField1 testField1;

    public int getInt1() {
        return int1;
    }

    public void setInt1(int int1) {
        this.int1 = int1;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public TestField1 getTestField1() {
        return testField1;
    }

    public void setTestField1(TestField1 testField1) {
        this.testField1 = testField1;
    }
}
