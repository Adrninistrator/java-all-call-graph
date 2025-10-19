package test.callgraph.stringappend.methodreturn;

/**
 * @author adrninistrator
 * @date 2025/9/21
 * @description:
 */
public class TestStringAppendReturnSimpleClassName {

    public String test1() {
        return TestStringAppendReturnSimpleClassName.class.getSimpleName();
    }

    public String test2() {
        return "" + TestStringAppendReturnSimpleClassName.class.getSimpleName() + "";
    }

    public String test3() {
        return new StringBuilder(TestStringAppendReturnSimpleClassName.class.getSimpleName()).append("").toString();
    }

    public String test4() {
        return new StringBuffer().append("").append(TestStringAppendReturnSimpleClassName.class.getSimpleName()).append("").toString();
    }
}
