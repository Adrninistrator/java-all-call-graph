package test.runbycode.handler.spring;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.field.TestField1;
import test.callgraph.field.cycle.TestUseFieldGenericsCycle1;
import test.callgraph.spring.bean.define.impl.SpringServiceImplA1;
import test.callgraph.spring.mvc.TestSpringController1;
import test.callgraph.spring.mvc.TestSpringController2;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class TestSpringHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testQueryAllControllerInfo() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<SpringControllerInfo> springControllerInfoList = springHandler.queryAllControllerInfo();
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(springControllerInfoList));
            printListContent(springControllerInfoList);
        }
    }

    @Test
    public void testQueryAllTaskMethod() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> springTaskMethodList = springHandler.queryAllTaskMethod();
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(springTaskMethodList));
            printListContent(springTaskMethodList);
        }
    }

    @Test
    public void testQueryControllerUriList() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            doQueryControllerUriList(springHandler, JavaCG2ClassMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "<init>"), "void", false);
            doQueryControllerUriList(springHandler, JavaCG2ClassMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "test1"), "void", true);
            doQueryControllerUriList(springHandler, JavaCG2ClassMethodUtil.formatFullMethodStr(TestSpringController1.class.getName(), "test2",
                    TestUseFieldGenericsCycle1.class.getName()), "void", true);
            doQueryControllerUriList(springHandler, JavaCG2ClassMethodUtil.formatFullMethodStr(TestSpringController2.class.getName(), "test1", TestField1.class.getName()), "void"
                    , true);
        }
    }

    private void doQueryControllerUriList(SpringHandler springHandler, String fullMethod, String returnType, boolean exists) {
        List<String> controllerUriList = springHandler.queryControllerUriList(fullMethod, returnType);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(controllerUriList));
        printListContent(controllerUriList, fullMethod);

        String controllerUri = springHandler.queryControllerUri(fullMethod, returnType);
        Assert.assertEquals(exists, StringUtils.isNotBlank(controllerUri));
        printObjectContent(controllerUri, fullMethod);
    }

    @Test
    public void testQueryControllerBySCN() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> simpleClassNameList = springHandler.queryAllControllerSCN();
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(simpleClassNameList));
            for (String simpleClassName : simpleClassNameList) {
                List<WriteDbData4SpringController> list = springHandler.queryControllerBySCN(simpleClassName);
                printListContent(list, simpleClassName);
            }
        }
    }

    @Test
    public void testCheckSpringTask() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            doCheckSpringTask(springHandler, JavaCG2ClassMethodUtil.formatFullMethodStr(SpringServiceImplA1.class.getName(), "test1"), "void");
            doCheckSpringTask(springHandler, JavaCG2ClassMethodUtil.formatFullMethodStr(SpringServiceImplA1.class.getName(), "test2"), String.class.getName());
        }
    }

    private void doCheckSpringTask(SpringHandler springHandler, String fullMethod, String returnType) {
        boolean isSpringTask = springHandler.checkSpringTask(fullMethod, returnType);
        printObjectContent(isSpringTask, fullMethod, returnType);
    }
}
