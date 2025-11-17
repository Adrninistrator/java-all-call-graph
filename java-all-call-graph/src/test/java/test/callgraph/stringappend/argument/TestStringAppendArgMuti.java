package test.callgraph.stringappend.argument;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/9/11
 * @description:
 */
public class TestStringAppendArgMuti {

    public void test1() {
        StringBuffer stringBuffer1;
        if (System.getProperty("") != null) {
            stringBuffer1 = new StringBuffer("a");
            stringBuffer1.append("b").append("c").append("d");
        } else {
            stringBuffer1 = new StringBuffer("a");
            stringBuffer1.append("b");
            stringBuffer1.append(DbStatementEnum.DSE_UPDATE.getStatement());
            stringBuffer1.append("c").append("d");
        }
        System.out.println(stringBuffer1);
    }
}
