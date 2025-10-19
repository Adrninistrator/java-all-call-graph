package test.callgraph.stringappend.argument;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description:
 */
public class TestStringAppendArgOnlyConstants {
    // 场景1: new StringBuffer无初始字符串，不调用append，保存到变量
    public static void scenario1() {
        StringBuffer sb = new StringBuffer();
        System.out.println(sb.append("a").append("b").append("c").append("d"));
    }

    // 场景2: new StringBuffer有初始字符串"a"，不调用append，保存到变量
    public static void scenario2() {
        StringBuffer sb = new StringBuffer("a");
        System.out.println(sb.append("b").append("c").append("d"));
    }

    // 场景3: new StringBuffer无初始字符串，调用append一次，保存到变量
    public static void scenario3() {
        StringBuffer sb = new StringBuffer();
        sb.append("a");
        System.out.println(sb.append("b").append("c").append("d"));
    }

    // 场景4: new StringBuffer有初始字符串"a"，调用append多次，保存到变量
    public static void scenario4() {
        StringBuffer sb = new StringBuffer("a");
        sb.append("b").append("c");
        System.out.println(sb.append("d"));
    }

    // 场景5: new StringBuffer无初始字符串，不调用append，直接打印
    public static void scenario5() {
        System.out.println(new StringBuffer().append("a").append("b").append("c").append("d"));
    }

    // 场景6: new StringBuffer有初始字符串"a"，不调用append，直接打印
    public static void scenario6() {
        System.out.println(new StringBuffer("a").append("b").append("c").append("d"));
    }

    // 场景7: new StringBuffer无初始字符串，调用append一次，直接打印
    public static void scenario7() {
        System.out.println(new StringBuffer().append("a").append("b").append("c").append("d"));
    }

    // 场景8: new StringBuffer有初始字符串"a"，调用append多次，直接打印
    public static void scenario8() {
        System.out.println(new StringBuffer("a").append("b").append("c").append("d"));
    }

    // 场景9: 使用StringBuffer.append方法返回值作为参数
    public static void scenario9() {
        StringBuffer sb = new StringBuffer("a");
        System.out.println(sb.append("b").append("c").append("d"));
    }

    // 场景10: 使用StringBuffer.toString方法返回值作为参数
    public static void scenario10() {
        StringBuffer sb = new StringBuffer("a").append("b").append("c").append("d");
        System.out.println(sb.toString());
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
