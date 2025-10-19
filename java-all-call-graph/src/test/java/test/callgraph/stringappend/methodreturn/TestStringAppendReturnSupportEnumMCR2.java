package test.callgraph.stringappend.methodreturn;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/9/27
 * @description:
 */
public class TestStringAppendReturnSupportEnumMCR2 {

    // 场景1: new StringBuffer未指定初始字符串，不调用append，直接使用
    public static String testCase1() {
        return new StringBuffer().append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
    }

    // 场景2: new StringBuffer有指定初始字符串，不调用append，保存到变量
    public static String testCase2() {
        StringBuffer sb = new StringBuffer("a");
        sb.append("b").append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d");
        return sb.toString();
    }

    // 场景3: new StringBuffer未指定初始字符串，调用一次append，直接使用
    public static String testCase3() {
        return new StringBuffer().append("a")
                .append("b").append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
    }

    // 场景4: new StringBuffer有指定初始字符串，调用多次append，保存到变量
    public static String testCase4() {
        StringBuffer sb = new StringBuffer("a");
        sb.append("b");
        sb.append(DbStatementEnum.DSE_UPDATE.name());
        sb.append("c");
        sb.append("d");
        return sb.toString();
    }

    // 场景5: new StringBuffer未指定初始字符串，调用多次append，直接使用
    public static String testCase5() {
        return new StringBuffer()
                .append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
    }

    // 场景6: new StringBuffer有指定初始字符串，不调用append，直接使用
    public static String testCase6() {
        return new StringBuffer("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
    }

    // 场景7: StringBuffer.append返回值直接使用
    public static String testCase7() {
        StringBuffer sb = new StringBuffer();
        return sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
    }

    // 场景8: StringBuffer.toString返回值直接使用
    public static String testCase8() {
        StringBuffer sb = new StringBuffer();
        return sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
    }

    // 场景9: StringBuffer.append返回值保存到变量
    public static String testCase9() {
        StringBuffer sb = new StringBuffer();
        StringBuffer sb2 = sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d");
        return sb2.toString();
    }

    // 场景10: StringBuffer.toString返回值保存到变量
    public static String testCase10() {
        StringBuffer sb = new StringBuffer();
        String result = sb.append("a").append("b")
                .append(DbStatementEnum.DSE_UPDATE.name())
                .append("c").append("d").toString();
        return result;
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
