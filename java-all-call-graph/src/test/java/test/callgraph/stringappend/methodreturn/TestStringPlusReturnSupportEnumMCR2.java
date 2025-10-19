package test.callgraph.stringappend.methodreturn;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/9/27
 * @description:
 */
public class TestStringPlusReturnSupportEnumMCR2 {

    public String testStringPlus1() {
        String str = "a" + "b" + DbStatementEnum.DSE_UPDATE.name() + "c" + "d";
        return str;
    }

    public String testStringPlus2() {
        return "a" + "b" + DbStatementEnum.DSE_UPDATE.name() + "c" + "d";
    }
}
