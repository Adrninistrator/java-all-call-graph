package test.callgraph.stringappend.argument;

/**
 * @author adrninistrator
 * @date 2025/9/20
 * @description:
 */
public class TestStringAppendArgClassName {

    public void test1() {
        System.out.println(TestStringAppendArgClassName.class.getName());
    }

    public void test2() {
        System.out.println("" + TestStringAppendArgClassName.class.getName() + "");
    }

    public void test3() {
        String string = new StringBuilder(TestStringAppendArgClassName.class.getName()).append("").toString();
        System.out.println(string);
    }

    public void test4() {
        String string = new StringBuffer().append("").append(TestStringAppendArgClassName.class.getName()).append("").toString();
        System.out.println(string);
    }
}
