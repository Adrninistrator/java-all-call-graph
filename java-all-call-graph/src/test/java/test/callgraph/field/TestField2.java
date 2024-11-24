package test.callgraph.field;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.fasterxml.jackson.annotation.JsonProperty;
import test.callgraph.enums.DbStatementEnum;
import test.callgraph.field.dto.TestFieldDto1;
import test.callgraph.innerclass.TestInClass;
import test.callgraph.innerclass.TestOutClass;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/9/17
 * @description:
 */
public class TestField2 {
    private List<TestFieldDto1> testFieldDto1List;

    @JsonProperty("vvv-2")
    public String data;

    public static String STRING1 = "aaa";

    public static TestField2 INSTANCE = new TestField2();

    private TestOutClass testOutClass;

    private List<TestInClass> testInClassList;

    private int[] intArray1;

    private int[][] intArray2;

    public void test1() {
        ConfigKeyEnum configKeyEnum;
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            configKeyEnum = ConfigKeyEnum.CKE_THREAD_NUM;
            System.out.println(configKeyEnum);
        } else if (i == 2) {
            configKeyEnum = ConfigKeyEnum.CKE_THREAD_NUM;
        } else {
            configKeyEnum = ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL;
        }

        System.out.println(configKeyEnum);
        System.out.println(configKeyEnum.getKey());
    }

    public void test2() {
        DbStatementEnum dbStatementEnum;
        if (System.getProperty("") != null) {
            dbStatementEnum = DbStatementEnum.DSE_DELETE;
        } else {
            dbStatementEnum = DbStatementEnum.DSE_UPDATE;
        }
        System.out.println(dbStatementEnum.getStatement());
        System.out.println(dbStatementEnum);

        System.out.println(DbStatementEnum.DSE_INSERT.getStatement());
        System.out.println(DbStatementEnum.DSE_INSERT);
    }

    public List<TestFieldDto1> getTestFieldDto1List() {
        return testFieldDto1List;
    }

    public void setTestFieldDto1List(List<TestFieldDto1> testFieldDto1List) {
        this.testFieldDto1List = testFieldDto1List;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public TestOutClass getTestOutClass() {
        return testOutClass;
    }

    public void setTestOutClass(TestOutClass testOutClass) {
        this.testOutClass = testOutClass;
    }

    public List<TestInClass> getTestInClassList() {
        return testInClassList;
    }

    public void setTestInClassList(List<TestInClass> testInClassList) {
        this.testInClassList = testInClassList;
    }

    public int[] getIntArray1() {
        return intArray1;
    }

    public void setIntArray1(int[] intArray1) {
        this.intArray1 = intArray1;
    }

    public int[][] getIntArray2() {
        return intArray2;
    }

    public void setIntArray2(int[][] intArray2) {
        this.intArray2 = intArray2;
    }
}
