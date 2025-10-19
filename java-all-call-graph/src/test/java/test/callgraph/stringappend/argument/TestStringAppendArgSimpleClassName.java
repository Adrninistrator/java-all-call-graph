package test.callgraph.stringappend.argument;

/**
 * @author adrninistrator
 * @date 2025/9/21
 * @description:
 */
public class TestStringAppendArgSimpleClassName {

    public void test1() {
        System.out.println(TestStringAppendArgSimpleClassName.class.getSimpleName());
    }

    public void test2() {
        System.out.println("" + TestStringAppendArgSimpleClassName.class.getSimpleName() + "");
    }

    public void test3() {
        String string = new StringBuilder(TestStringAppendArgSimpleClassName.class.getSimpleName()).append("").toString();
        System.out.println(string);
    }

    public void test4() {
        String string = new StringBuffer().append("").append(TestStringAppendArgSimpleClassName.class.getSimpleName()).append("").toString();
        System.out.println(string);
    }
}
