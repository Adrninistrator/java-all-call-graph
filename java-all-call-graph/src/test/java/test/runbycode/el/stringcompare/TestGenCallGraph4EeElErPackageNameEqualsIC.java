package test.runbycode.el.stringcompare;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Test;
import test.runbycode.base.TestElRunByCodeBase;
import test.runbycode.el.gencallgraph.TestGenCallGraph4EeElErPackageName;

/**
 * @author adrninistrator
 * @date 2025/9/22
 * @description:
 */
public class TestGenCallGraph4EeElErPackageNameEqualsIC extends TestElRunByCodeBase {

    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL;
    }

    @Override
    protected boolean example4StringCompare() {
        return true;
    }

    @Override
    protected String chooseElText() {
        return "string.equalsIC(" + CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", '" + TestGenCallGraph4EeElErPackageName.ER_PACKAGE_NAME_UPPER + "')";
    }

    @Override
    protected String chooseTitle() {
        return "判断等于关键字（忽略大小写）";
    }

    @Override
    protected String chooseDesc() {
        return "在生成向上方法完整调用链时，判断调用类包名是否等于指定关键字（忽略大小写），忽略匹配的方法调用";
    }

    @Test
    public void test() {
        genCalleeGraphAndCheckNoEr();
    }
}
