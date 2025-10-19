package test.callgraph.stringappend.argument;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description:
 */
public class TestStringPlusArgSupportEnumMCR {

    public void testStringPlus1() {
        String str = "a" + "b" + DbStatementEnum.DSE_UPDATE.getStatement() + "c" + "d";
        System.out.println(str);
    }

    public void testStringPlus2() {
        System.out.println("a" + "b" + DbStatementEnum.DSE_UPDATE.getStatement() + "c" + "d");
    }
}
