package test.runbycode.el.jardiff;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import org.junit.Test;
import test.diffjar.controller.TestController1;
import test.diffjar.dao.TestJarDiffTableMapper;
import test.diffjar.service.TestService1;
import test.diffjar.task.TestTask1;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/10/1
 * @description:
 */
public class TestGenJarDiffCallerGraph4MethodArgNum extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM.getVariableName() + " == 0";
    }

    @Override
    protected String chooseTitle() {
        return "JarDiff生成向下完整方法调用链判断发生变化的方法参数数量";
    }

    @Override
    protected String chooseDesc() {
        return "通过JarDiff生成向下完整方法调用链时，判断发生变化的方法参数数量是否等于指定值，忽略匹配的方法";
    }

    @Test
    public void test() {
        genJarDiffCallGraph(false, new String[]{
                TestJarDiffTableMapper.class.getName(),
                TestController1.class.getName() + ":post(",
        }, new String[]{
                TestTask1.class.getName(),
                TestService1.class.getName(),
                TestController1.class.getName() + ":get1()",
                TestController1.class.getName() + ":get2()"
        });
    }
}