package test.callgraph.stringappend.methodreturn;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description:
 */
public class TestStringPlusReturnSupportEnumMCR {

    public String testStringPlus1() {
        String str = "a" + "b" + DbStatementEnum.DSE_UPDATE.getStatement() + "c" + "d";
        return str;
    }

    public String testStringPlus2() {
        return "a" + "b" + DbStatementEnum.DSE_UPDATE.getStatement() + "c" + "d";
    }
}
