package test.runbycode.handler.extendsimpl;

import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndType;
import com.adrninistrator.jacg.handler.dto.extendsimpl.ExtendsImplInfo;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.extend.I3_1_1_2;
import test.callgraph.future.FutureImpl;
import test.callgraph.interfaces.AbstractMapper;
import test.callgraph.interfaces.classes.ExtendsClass1A;
import test.callgraph.interfaces.classes.ImplChildClass2A;
import test.callgraph.interfaces.classes.SuperItfChild1;
import test.callgraph.interfaces.interfaces.InterfaceChild3;
import test.callgraph.interfaces.interfaces.InterfaceSuper1;
import test.callgraph.interfaces.interfaces.InterfaceSuper2;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/5
 * @description:
 */
public class TestQueryExtendsImpl extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestQueryExtendsImpl.class);

    public static final String PROMPT_CLASS_GRAPH_COMMON = "为以下Java中的类和接口的关系生成mermaid格式的类图文本，" +
            "以classDiagram开头，剩下内容只指定类和类之间的继承关系、类和接口之间实现关系、接口与接口之间的继承关系，不要class 、interface等定义；" +
            "生成结果中类名或接口名去掉包名。";

    public static final String PROMPT_CLASS_GRAPH_DOWNWARD = PROMPT_CLASS_GRAPH_COMMON +
            "以下JSON的格式说明：key代表一个子类，value中的classType代表子类的类型，value中的extendsImplClassInfoList代表父类或接口的类名及类型";

    public static final String PROMPT_CLASS_GRAPH_UPWARD = PROMPT_CLASS_GRAPH_COMMON +
            "以下JSON的格式说明：key代表一个父类或实现的接口，value中的classType代表父类或实现的接口的类型，value中的extendsImplClassInfoList代表子类或实现类的名称及类型";

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
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        printListContent(list, className, "子接口+子类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, true, false, false, false);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        printListContent(list, className, "接口");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, true);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        printListContent(list, className, "类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, true, false);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        printListContent(list, className, "抽象类");

        list = jacgExtendsImplHandler.queryChildClassListByFull(className, false, true, false, true);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        printListContent(list, className, "非抽象类");
    }

    @Test
    public void testQueryUpwardByClassName() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryUpwardByClassName(jacgExtendsImplHandler, I3_1_1_2.class.getName());
            doTestQueryUpwardByClassName(jacgExtendsImplHandler, SuperItfChild1.class.getName());
            doTestQueryUpwardByClassName(jacgExtendsImplHandler, InterfaceChild3.class.getName());
        }
    }

    private void doTestQueryUpwardByClassName(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        List<ClassNameAndType> classNameAndTypeList = jacgExtendsImplHandler.queryUpwardByClassName(className);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(classNameAndTypeList));
        printListContent(classNameAndTypeList, className);
    }

    @Test
    public void testQueryDownwardByClassName() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryDownwardByClassName(jacgExtendsImplHandler, InterfaceSuper1.class.getName());
            doTestQueryDownwardByClassName(jacgExtendsImplHandler, InterfaceSuper2.class.getName());
        }
    }

    private void doTestQueryDownwardByClassName(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        List<ClassNameAndType> classNameAndTypeList = jacgExtendsImplHandler.queryDownwardByClassName(className);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(classNameAndTypeList));
        printListContent(classNameAndTypeList, className);
    }

    @Test
    public void testQueryAllSuperClassName() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryAllSuperClassName(jacgExtendsImplHandler, I3_1_1_2.class.getName(),false);
            doTestQueryAllSuperClassName(jacgExtendsImplHandler, FutureImpl.class.getName(),true);
        }
    }

    private void doTestQueryAllSuperClassName(JACGExtendsImplHandler jacgExtendsImplHandler, String className, boolean empty) {
        List<String> allSuperClassNameList = jacgExtendsImplHandler.queryAllSuperClassName(className);
        Assert.assertEquals(empty, JavaCG2Util.isCollectionEmpty(allSuperClassNameList));
        printListContent(allSuperClassNameList, className);
    }

    @Test
    public void testQueryTopCLassList() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryTopCLassList(jacgExtendsImplHandler, I3_1_1_2.class.getName());
            doTestQueryTopCLassList(jacgExtendsImplHandler, FutureImpl.class.getName());
            doTestQueryTopCLassList(jacgExtendsImplHandler, SuperItfChild1.class.getName());
        }
    }

    private void doTestQueryTopCLassList(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        List<ClassNameAndType> classNameAndTypeList = jacgExtendsImplHandler.queryTopCLassList(className);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(classNameAndTypeList));
        printListContent(classNameAndTypeList, className);
    }

    @Test
    public void testQueryExtendsImplInfoUpward() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryExtendsImplInfoUpward(jacgExtendsImplHandler, ExtendsClass1A.class.getName());
            doTestQueryExtendsImplInfoUpward(jacgExtendsImplHandler, SuperItfChild1.class.getName());
        }
    }

    private void doTestQueryExtendsImplInfoUpward(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        Map<String, ExtendsImplInfo> extendsImplInfoMap = jacgExtendsImplHandler.queryExtendsImplInfoUpward(className);
        Assert.assertFalse(JavaCG2Util.isMapEmpty(extendsImplInfoMap));
        printMapContent(extendsImplInfoMap, className);
        logger.info("{}\n{}", PROMPT_CLASS_GRAPH_UPWARD, JACGJsonUtil.getJsonStrPretty(extendsImplInfoMap));
    }

    @Test
    public void testQueryExtendsImplInfoDownward() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryExtendsImplInfoDownward(jacgExtendsImplHandler, InterfaceSuper1.class.getName());
            doTestQueryExtendsImplInfoDownward(jacgExtendsImplHandler, InterfaceSuper2.class.getName());
        }
    }

    private void doTestQueryExtendsImplInfoDownward(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        Map<String, ExtendsImplInfo> extendsImplInfoMap = jacgExtendsImplHandler.queryExtendsImplInfoDownward(className);
        Assert.assertFalse(JavaCG2Util.isMapEmpty(extendsImplInfoMap));
        printMapContent(extendsImplInfoMap, className);
        logger.info("{}\n{}", PROMPT_CLASS_GRAPH_DOWNWARD, JACGJsonUtil.getJsonStrPretty(extendsImplInfoMap));
    }

    @Test
    public void testQueryExtendsImplInfoDownwardFromTop() {
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            doTestQueryExtendsImplInfoDownwardFromTop(jacgExtendsImplHandler, ImplChildClass2A.class.getName());
            doTestQueryExtendsImplInfoDownwardFromTop(jacgExtendsImplHandler, AbstractMapper.class.getName());
        }
    }

    private void doTestQueryExtendsImplInfoDownwardFromTop(JACGExtendsImplHandler jacgExtendsImplHandler, String className) {
        Map<String, ExtendsImplInfo> extendsImplInfoMap = jacgExtendsImplHandler.queryExtendsImplInfoDownwardFromTop(className);
        Assert.assertFalse(JavaCG2Util.isMapEmpty(extendsImplInfoMap));
        printMapContent(extendsImplInfoMap, className);
        logger.info("{}\n{}", PROMPT_CLASS_GRAPH_DOWNWARD, JACGJsonUtil.getJsonStrPretty(extendsImplInfoMap));
    }
}
