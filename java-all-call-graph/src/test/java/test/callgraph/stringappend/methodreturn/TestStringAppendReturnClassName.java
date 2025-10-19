package test.callgraph.stringappend.methodreturn;

/**
 * @author adrninistrator
 * @date 2025/9/20
 * @description:
 */
public class TestStringAppendReturnClassName {

    public String test1() {
        return TestStringAppendReturnClassName.class.getName();
    }

    public String test2() {
        return "" + TestStringAppendReturnClassName.class.getName() + "";
    }

    public String test3() {
        return new StringBuilder(TestStringAppendReturnClassName.class.getName()).append("").toString();
    }

    public String test4() {
        return new StringBuffer().append("").append(TestStringAppendReturnClassName.class.getName()).append("").toString();
    }
}
