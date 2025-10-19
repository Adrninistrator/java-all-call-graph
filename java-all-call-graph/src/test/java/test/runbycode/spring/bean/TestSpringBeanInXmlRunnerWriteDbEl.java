package test.runbycode.spring.bean;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringBean;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.spring.profile.bean.DefaultMessageService1;
import test.callgraph.spring.profile.bean.DevMessageService1;
import test.callgraph.spring.profile.bean.DevMessageService2A;
import test.callgraph.spring.profile.bean.DevMessageService2B;
import test.callgraph.spring.profile.bean.ProdMessageService1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/10/19
 * @description:
 */
@JACGExample(title = "通过表达式配置需要处理的XML中的Spring Bean",
        desc = {"支持通过Bean名称、Bean类名、profile进行过滤"})
public class TestSpringBeanInXmlRunnerWriteDbEl extends TestRunByCodeBase {

    private SpringHandler springHandler;

    @Before
    public void init() {
        // 所有的类都不解析
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS, Boolean.TRUE.toString());
        springHandler = new SpringHandler(configureWrapper);
    }

    @After
    public void after() {
        springHandler.close();
    }

    @JACGExample(title = "全部都处理", desc = {})
    @Test
    public void fixedFalseParseAll() {
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML, Boolean.FALSE.toString());
        commonWriteDbForce();
        List<WriteDbData4SpringBean> springBeanList = springHandler.queryAllSpringBean();
        String[] allClassNames = new String[]{
                DefaultMessageService1.class.getName(),
                DevMessageService1.class.getName(),
                DevMessageService2A.class.getName(),
                DevMessageService2B.class.getName(),
                ProdMessageService1.class.getName()
        };
        checkContainsClassNameAll(springBeanList, allClassNames);
        Assert.assertTrue(springBeanList.size() > allClassNames.length);
    }

    @JACGExample(title = "仅处理Bean名称匹配的记录", desc = {})
    @Test
    public void onlyHandleBeanNameMatches() {
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML,
                JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME.getVariableName() + " != 'messageService1'");
        commonWriteDbForce();
        List<WriteDbData4SpringBean> springBeanList = springHandler.queryAllSpringBean();
        String[] messageService1ClassNames = new String[]{
                DefaultMessageService1.class.getName(),
                DevMessageService1.class.getName(),
                ProdMessageService1.class.getName()
        };
        checkContainsClassNameAll(springBeanList, messageService1ClassNames);
        Assert.assertEquals(springBeanList.size(), messageService1ClassNames.length);
    }

    @JACGExample(title = "仅处理Bean类名匹配的记录", desc = {})
    @Test
    public void onlyHandleClassNameMatches() {
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML,
                JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME.getVariableName() + " != '" + DefaultMessageService1.class.getName() + "'");
        commonWriteDbForce();
        List<WriteDbData4SpringBean> springBeanList = springHandler.queryAllSpringBean();
        checkContainsClassNameAll(springBeanList, DefaultMessageService1.class.getName());
        Assert.assertEquals(1, springBeanList.size());
    }

    @JACGExample(title = "仅处理profile为空的记录", desc = {})
    @Test
    public void onlyHandleProfileEmpty() {
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML,
                JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName() + " != ''");
        commonWriteDbForce();
        List<WriteDbData4SpringBean> springBeanList = springHandler.queryAllSpringBean();
        String[] profileNotEmptyClassNames = new String[]{
                DevMessageService1.class.getName(),
                DevMessageService2A.class.getName(),
                DevMessageService2B.class.getName(),
                ProdMessageService1.class.getName()
        };
        checkContainsClassNameAll(springBeanList, DefaultMessageService1.class.getName());
        Assert.assertTrue(springBeanList.size() > 1);
        checkContainsClassNameNone(springBeanList, profileNotEmptyClassNames);
    }

    @JACGExample(title = "仅处理profile匹配的记录，一级", desc = {})
    @Test
    public void onlyHandleProfileMatchesLevelOne() {
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML,
                JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName() + " != 'prod'");
        commonWriteDbForce();
        List<WriteDbData4SpringBean> springBeanList = springHandler.queryAllSpringBean();
        checkContainsClassNameAll(springBeanList, ProdMessageService1.class.getName());
        Assert.assertEquals(1, springBeanList.size());
    }

    @JACGExample(title = "仅处理profile匹配的记录，二级", desc = {})
    @Test
    public void onlyHandleProfileMatchesLevelTwo() {
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML,
                JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName() + " != 'dev,dev2A'");
        commonWriteDbForce();
        List<WriteDbData4SpringBean> springBeanList = springHandler.queryAllSpringBean();
        checkContainsClassNameAll(springBeanList, DevMessageService2A.class.getName());
        Assert.assertEquals(1, springBeanList.size());
    }

    private boolean checkContainsClassName(List<WriteDbData4SpringBean> springBeanList, String className) {
        for (WriteDbData4SpringBean springBean : springBeanList) {
            if (springBean.getClassName().equals(className)) {
                return true;
            }
        }
        return false;
    }

    private void checkContainsClassNameAll(List<WriteDbData4SpringBean> springBeanList, String... classNames) {
        for (String className : classNames) {
            Assert.assertTrue(className, checkContainsClassName(springBeanList, className));
        }
    }

    private void checkContainsClassNameNone(List<WriteDbData4SpringBean> springBeanList, String... classNames) {
        for (String className : classNames) {
            Assert.assertFalse(className, checkContainsClassName(springBeanList, className));
        }
    }
}