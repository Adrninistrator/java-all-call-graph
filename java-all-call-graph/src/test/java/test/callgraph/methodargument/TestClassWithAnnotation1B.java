package test.callgraph.methodargument;

import test.callgraph.annotation.TestAnnotation1;
import test.callgraph.field.TestField2;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/8
 * @description:
 */
@TestAnnotation1(name = "1B", desc = "desc-1B")
public class TestClassWithAnnotation1B {

    private int int1;

    private String data;

    private List<TestField2> testField2List;

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

    public List<TestField2> getTestField2List() {
        return testField2List;
    }

    public void setTestField2List(List<TestField2> testField2List) {
        this.testField2List = testField2List;
    }
}
