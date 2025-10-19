package test.callgraph.stringappend.methodreturn;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description:
 */
public class TestStringAppendReturnOnlyConstants {
    // 场景1: new StringBuffer无初始字符串，不调用append，保存到变量
    public static String scenario1() {
        StringBuffer sb = new StringBuffer();
        return sb.append("a").append("b").append("c").append("d").toString();
    }

    // 场景2: new StringBuffer有初始字符串"a"，不调用append，保存到变量
    public static String scenario2() {
        StringBuffer sb = new StringBuffer("a");
        return sb.append("b").append("c").append("d").toString();
    }

    // 场景3: new StringBuffer无初始字符串，调用append一次，保存到变量
    public static String scenario3() {
        StringBuffer sb = new StringBuffer();
        sb.append("a");
        return sb.append("b").append("c").append("d").toString();
    }

    // 场景4: new StringBuffer有初始字符串"a"，调用append多次，保存到变量
    public static String scenario4() {
        StringBuffer sb = new StringBuffer("a");
        sb.append("b").append("c");
        return sb.append("d").toString();
    }

    // 场景5: new StringBuffer无初始字符串，不调用append，直接打印
    public static String scenario5() {
        return new StringBuffer().append("a").append("b").append("c").append("d").toString();
    }

    // 场景6: new StringBuffer有初始字符串"a"，不调用append，直接打印
    public static String scenario6() {
        return new StringBuffer("a").append("b").append("c").append("d").toString();
    }

    // 场景7: new StringBuffer无初始字符串，调用append一次，直接打印
    public static String scenario7() {
        return new StringBuffer().append("a").append("b").append("c").append("d").toString();
    }

    // 场景8: new StringBuffer有初始字符串"a"，调用append多次，直接打印
    public static String scenario8() {
        return new StringBuffer("a").append("b").append("c").append("d").toString();
    }

    // 场景9: 使用StringBuffer.append方法返回值作为参数
    public static String scenario9() {
        StringBuffer sb = new StringBuffer("a");
        return sb.append("b").append("c").append("d").toString();
    }

    // 场景10: 使用StringBuffer.toString方法返回值作为参数
    public static String scenario10() {
        StringBuffer sb = new StringBuffer("a").append("b").append("c").append("d");
        return sb.toString();
    }

    public static void main(String[] args) {
        scenario1();
        scenario2();
        scenario3();
        scenario4();
        scenario5();
        scenario6();
        scenario7();
        scenario8();
        scenario9();
        scenario10();
    }
}
