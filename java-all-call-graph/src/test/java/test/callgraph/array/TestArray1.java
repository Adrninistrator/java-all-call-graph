package test.callgraph.array;

import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2025/1/2
 * @description:
 */
public class TestArray1 {

    private void test1() {
        DbStatementEnum[] dbStatementEnums = DbStatementEnum.values();
        DbStatementEnum[] newDbStatementEnums = dbStatementEnums;
        System.out.println(newDbStatementEnums);
        DbStatementEnum[] dbStatementEnums2 = new DbStatementEnum[]{};
        System.out.println(dbStatementEnums2);
    }

    private void test2() {
        DbStatementEnum[][] dbStatementEnums = new DbStatementEnum[][]{};
        DbStatementEnum[][] newDbStatementEnums = null;
        System.out.println(newDbStatementEnums);
        dbStatementEnums.notify();
    }

    private void test3() {
        DbStatementEnum[][][] dbStatementEnums = new DbStatementEnum[][][]{};
        DbStatementEnum[][][] newDbStatementEnums = null;
        System.out.println(newDbStatementEnums);
        dbStatementEnums.notify();
    }
}
