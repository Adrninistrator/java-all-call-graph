package test.callgraph.stringappend.argument;

/**
 * @author adrninistrator
 * @date 2025/9/11
 * @description:
 */
public class TestStringAppendArgOther {

    public void test1() {
        StringBuffer stringBuffer1 = new StringBuffer("a");
        StringBuffer stringBuffer2 = new StringBuffer("a");
        stringBuffer1.append("b").append("c");
        stringBuffer2.append("b").append("c");
        System.out.println(stringBuffer1.append("d"));
        stringBuffer1.append("");
        stringBuffer2.append("b").append("c");
        System.out.println(stringBuffer1.append(""));
    }
}
