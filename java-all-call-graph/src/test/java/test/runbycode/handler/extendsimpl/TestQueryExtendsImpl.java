package test.runbycode.handler.extendsimpl;

import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.extend.I3_1_1_2;
import test.callgraph.future.FutureImpl;
import test.callgraph.interfaces.interfaces.InterfaceSuper1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/5
 * @description:
 */
public class TestQueryExtendsImpl extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testGetChildClassList() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestGetChildClassList(jacgExtendsImplHandler, InterfaceSuper1.class.getName());
        }
    }

    private void doTestGetChildClassList(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        List<String> list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, true, true, true);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list));
        printListContent(list, className, "子接口+子类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, false, false, false);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list));
        printListContent(list, className, "接口");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, true);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list));
        printListContent(list, className, "类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, false);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list));
        printListContent(list, className, "抽象类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, false, true);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list));
        printListContent(list, className, "非抽象类");
    }

    @Test
    public void testQeryAllSuperClassName() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            List<String> allSuperClassNameList = jacgExtendsImplHandler.queryAllSuperClassName(I3_1_1_2.class.getName());
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(allSuperClassNameList));
            printListContent(allSuperClassNameList, "1");

            List<String> allSuperClassNameList2 = jacgExtendsImplHandler.queryAllSuperClassName(FutureImpl.class.getName());
            Assert.assertTrue(JavaCGUtil.isCollectionEmpty(allSuperClassNameList2));
            printListContent(allSuperClassNameList2, "2");
        }
    }
}
