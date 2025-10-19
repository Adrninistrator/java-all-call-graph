package test.callgraph.stringappend.argument;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description:
 */
public class TestStringAppendArgSupportEnumMCR {

    // 场景1: new StringBuffer未指定初始字符串，不调用append，直接使用
    public static void testCase1() {
        System.out.println(new StringBuffer().append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d"));
    }

    // 场景2: new StringBuffer有指定初始字符串，不调用append，保存到变量
    public static void testCase2() {
        StringBuffer sb = new StringBuffer("a");
        sb.append("b").append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d");
        System.out.println(sb);
    }

    // 场景3: new StringBuffer未指定初始字符串，调用一次append，直接使用
    public static void testCase3() {
        System.out.println(new StringBuffer().append("a")
                .append("b").append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d"));
    }

    // 场景4: new StringBuffer有指定初始字符串，调用多次append，保存到变量
    public static void testCase4() {
        StringBuffer sb = new StringBuffer("a");
        sb.append("b");
        sb.append(DbStatementEnum.DSE_UPDATE.getStatement());
        sb.append("c");
        sb.append("d");
        System.out.println(sb);
    }

    // 场景5: new StringBuffer未指定初始字符串，调用多次append，直接使用
    public static void testCase5() {
        System.out.println(new StringBuffer()
                .append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d"));
    }

    // 场景6: new StringBuffer有指定初始字符串，不调用append，直接使用
    public static void testCase6() {
        System.out.println(new StringBuffer("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d"));
    }

    // 场景7: StringBuffer.append返回值直接使用
    public static void testCase7() {
        StringBuffer sb = new StringBuffer();
        System.out.println(sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d"));
    }

    // 场景8: StringBuffer.toString返回值直接使用
    public static void testCase8() {
        StringBuffer sb = new StringBuffer();
        System.out.println(sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d").toString());
    }

    // 场景9: StringBuffer.append返回值保存到变量
    public static void testCase9() {
        StringBuffer sb = new StringBuffer();
        StringBuffer sb2 = sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d");
        System.out.println(sb2);
    }

    // 场景10: StringBuffer.toString返回值保存到变量
    public static void testCase10() {
        StringBuffer sb = new StringBuffer();
        String result = sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.getStatement())
                .append("c").append("d").toString();
        System.out.println(result);
    }

    public static void main(String[] args) {
        testCase1();
        testCase2();
        testCase3();
        testCase4();
        testCase5();
        testCase6();
        testCase7();
        testCase8();
        testCase9();
        testCase10();
    }
}
