package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassName;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.charset.TestCharset;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.handler.methodcall.handler.ShowAllMethodCallByERDetailHandler;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/4
 * @description:
 */
public class TestCharset1MethodCallByERDetailHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        configureWrapper.setAllowAllClasses();
        commonWriteDb();
    }

    @Test
    public void test() {
        String callerFullMethod = JavaCGClassMethodUtil.formatFullMethod(TestCharset.class.getName(), "test", new Type[]{});

        try (ClassInfoHandler classInfoHandler = new ClassInfoHandler(configureWrapper);
             ShowAllMethodCallByERDetailHandler showAllMethodCallByERDetailHandler = new ShowAllMethodCallByERDetailHandler(configureWrapper)) {
            List<WriteDbData4ClassName> writeDbData4ClassNameList = classInfoHandler.queryClassNameEndsWith(StringUtils.class.getSimpleName());

            List<String> calleeClassNameList = new ArrayList<>();
            calleeClassNameList.add(String.class.getName());
            for (WriteDbData4ClassName writeDbData4ClassName : writeDbData4ClassNameList) {
                calleeClassNameList.add(writeDbData4ClassName.getClassName());
            }

            Assert.assertTrue(showAllMethodCallByERDetailHandler.handleMethodCallByER(callerFullMethod, calleeClassNameList));
        }
    }
}
