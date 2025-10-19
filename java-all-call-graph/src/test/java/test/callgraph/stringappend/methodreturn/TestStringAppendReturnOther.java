package test.callgraph.stringappend.methodreturn;

/**
 * @author adrninistrator
 * @date 2025/9/11
 * @description:
 */
public class TestStringAppendReturnOther {

    public String test1() {
        StringBuffer stringBuffer1 = new StringBuffer("a");
        StringBuffer stringBuffer2 = new StringBuffer("a");
        stringBuffer1.append("b").append("c");
        stringBuffer2.append("b").append("c");
        return stringBuffer1.append("d").toString();
    }
}
