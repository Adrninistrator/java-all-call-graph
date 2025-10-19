package test.runbycode.callgraph.calleeargtypepolymorphism;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import test.annotation.JACGExample;
import test.callgraph.calleeargtypepolymorphism.CalleeArgTypePolymorphismService1;
import test.callgraph.calleeargtypepolymorphism.CalleeArgTypePolymorphismTool1;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismChild1A;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismChild1B;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismChild2;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismChild3;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismInterface1;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismInterface2;
import test.callgraph.calleeargtypepolymorphism.superchild.CalleeArgTypePolymorphismInterface3;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/10/12
 * @description:
 */
@JACGExample(title = "生成向下完整方法调用链时，验证方法参数作为被调用对象涉及多态时的类型替换", desc = {})
public class TestGenCallerGraphCalleeArgTypePolymorphism extends TestRunByCodeBase {

    private final static String CALLEE_METHOD1_WITH_ARG_TYPES = JavaCG2ClassMethodUtil.formatMethodWithArgTypesStr("run1", String.class.getName(),
            CalleeArgTypePolymorphismInterface1.class.getName(),
            String.class.getName());
    private final static String CALLEE_METHOD2_WITH_ARG_TYPES = JavaCG2ClassMethodUtil.formatMethodWithArgTypesStr("run2", CalleeArgTypePolymorphismInterface1.class.getName(),
            CalleeArgTypePolymorphismInterface2.class.getName(),
            CalleeArgTypePolymorphismInterface3.class.getName());
    private final static String CALLEE_FULL_METHOD1 = JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(CalleeArgTypePolymorphismTool1.class.getName(),
            CALLEE_METHOD1_WITH_ARG_TYPES);
    private final static String CALLEE_FULL_METHOD2 = JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(CalleeArgTypePolymorphismTool1.class.getName(),
            CALLEE_METHOD2_WITH_ARG_TYPES);

    private final static String[] INTERFACE_FULL_METHODS = new String[]{
            CalleeArgTypePolymorphismInterface1.class.getName() + ":cmd1()",
            CalleeArgTypePolymorphismInterface1.class.getName() + ":cmd2()"
    };

    @Before
    public void init() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());
    }

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testRun1Use1A() {
        String[] methodArray = new String[]{
                CalleeArgTypePolymorphismChild1A.class.getName() + ":cmd1()",
                CalleeArgTypePolymorphismChild1A.class.getName() + ":cmd2()"
        };
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                CalleeArgTypePolymorphismService1.class.getName() + ":testRun1Use1A()");
        // 指定参数后，生成向下完整方法调用链时，方法参数作为被调用对象涉及多态时的类型完成替换
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM,
                CALLEE_FULL_METHOD1 + JavaCG2Constants.FLAG_EQUAL + "2"
        );
        List<MethodCallLineData4Er> methodCallLineData4ErList = genOneCallerGraph(1);
        checkCallerGraphContainsCalleeAll(methodCallLineData4ErList, methodArray);
        checkCallerGraphContainsCalleeNone(methodCallLineData4ErList, INTERFACE_FULL_METHODS);

        // 未指定参数时，生成向下完整方法调用链时，方法参数作为被调用对象涉及多态时的类型未替换
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM);
        List<MethodCallLineData4Er> methodCallLineData4ErList2 = genOneCallerGraph(1);
        checkCallerGraphContainsCalleeAll(methodCallLineData4ErList2, INTERFACE_FULL_METHODS);
    }

    @Test
    public void testRun1Use1B() {
        String[] methodArray = new String[]{
                CalleeArgTypePolymorphismChild1B.class.getName() + ":cmd1()",
                CalleeArgTypePolymorphismChild1B.class.getName() + ":cmd2()"
        };
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                CalleeArgTypePolymorphismService1.class.getName() + ":testRun1Use1B()");
        // 指定参数后，生成向下完整方法调用链时，方法参数作为被调用对象涉及多态时的类型完成替换
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM,
                CALLEE_FULL_METHOD1 + JavaCG2Constants.FLAG_EQUAL + "2"
        );
        List<MethodCallLineData4Er> methodCallLineData4ErList = genOneCallerGraph(1);
        checkCallerGraphContainsCalleeAll(methodCallLineData4ErList, methodArray);
        checkCallerGraphContainsCalleeNone(methodCallLineData4ErList, INTERFACE_FULL_METHODS);

        // 未指定参数时，生成向下完整方法调用链时，方法参数作为被调用对象涉及多态时的类型未替换
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM);
        List<MethodCallLineData4Er> methodCallLineData4ErList2 = genOneCallerGraph(1);
        checkCallerGraphContainsCalleeAll(methodCallLineData4ErList2, INTERFACE_FULL_METHODS);
    }

    @Test
    public void testRun2Use1B23() {
        String[] methodArrayContains = new String[]{
                CalleeArgTypePolymorphismChild1B.class.getName() + ":cmd1()",
                CalleeArgTypePolymorphismChild1B.class.getName() + ":cmd2()",
                CalleeArgTypePolymorphismChild2.class.getName() + ":cmd1()",
                Logger.class.getName() + ":info(java.lang.String,java.lang.Object)",
                CalleeArgTypePolymorphismChild3.class.getName() + ":hashCode()"
        };
        String[] methodArrayNotContains = new String[]{
                CalleeArgTypePolymorphismChild2.class.getName() + ":cmd2()",
                CalleeArgTypePolymorphismChild3.class.getName() + ":cmd1()",
                CalleeArgTypePolymorphismChild3.class.getName() + ":cmd2()",
                CalleeArgTypePolymorphismChild3.class.getName() + ":info(java.lang.String,java.lang.Object)"
        };
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                CalleeArgTypePolymorphismService1.class.getName() + ":testRun2Use1B23()");
        // 指定参数后，生成向下完整方法调用链时，方法参数作为被调用对象涉及多态时的类型完成替换
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM,
                CALLEE_FULL_METHOD2 + JavaCG2Constants.FLAG_EQUAL + "1",
                CALLEE_FULL_METHOD2 + JavaCG2Constants.FLAG_EQUAL + "2",
                CALLEE_FULL_METHOD2 + JavaCG2Constants.FLAG_EQUAL + "3"
        );
        List<MethodCallLineData4Er> methodCallLineData4ErList = genOneCallerGraph(1);
        checkCallerGraphContainsCalleeAll(methodCallLineData4ErList, methodArrayContains);
        checkCallerGraphContainsCalleeNone(methodCallLineData4ErList, methodArrayNotContains);
        checkCallerGraphContainsCalleeNone(methodCallLineData4ErList, INTERFACE_FULL_METHODS);

        // 未指定参数时，生成向下完整方法调用链时，方法参数作为被调用对象涉及多态时的类型未替换
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM);
        List<MethodCallLineData4Er> methodCallLineData4ErList2 = genOneCallerGraph(1);
        checkCallerGraphContainsCalleeAll(methodCallLineData4ErList2, INTERFACE_FULL_METHODS);
    }

    // 调用被调用方法时使用了多种子类类型，不支持处理
    @Test
    public void testNotSupport() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                CalleeArgTypePolymorphismService1.class.getName() + ":testNotSupport()");
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM,
                CALLEE_FULL_METHOD1 + JavaCG2Constants.FLAG_EQUAL + "2"
        );
        Assert.assertThrows(AssertionError.class, () -> genOneCallerGraph(1));
    }

    // 测试为被调用方法生成向下的完整方法调用链
    @Test
    public void testCalleeMethod1() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                CALLEE_FULL_METHOD1);
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM,
                CALLEE_FULL_METHOD1 + JavaCG2Constants.FLAG_EQUAL + "2"
        );
        genOneCallerGraph(1);

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM);
        genOneCallerGraph(1);
    }
}
