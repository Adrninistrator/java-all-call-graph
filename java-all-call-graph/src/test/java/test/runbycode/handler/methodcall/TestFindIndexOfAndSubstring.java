package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassName;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.handler.methodcall.handler.RecordCallerMethodCallByEEDetailHandler;
import test.runbycode.handler.methodcall.handler.ShowAllMethodCallByERDetailHandler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/4
 * @description:
 */
public class TestFindIndexOfAndSubstring extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test2() {
        List<String> methodNameList = Arrays.asList("indexOf", "IndexOf", "substring");
        try (ClassInfoHandler classInfoHandler = new ClassInfoHandler(configureWrapper);
             RecordCallerMethodCallByEEDetailHandler recordCallerMethodCallByEEDetailHandler = new RecordCallerMethodCallByEEDetailHandler(configureWrapper);
             ShowAllMethodCallByERDetailHandler showAllMethodCallByERDetailHandler = new ShowAllMethodCallByERDetailHandler(configureWrapper)) {
            // 查找被调用的StringUtil相关类名
            List<WriteDbData4ClassName> writeDbData4ClassNameList = classInfoHandler.queryClassNameEndsWith("StringUtil");
            List<String> stringUtilCalleeClassNameList = new ArrayList<>();
            for (WriteDbData4ClassName writeDbData4ClassName : writeDbData4ClassNameList) {
                stringUtilCalleeClassNameList.add(writeDbData4ClassName.getClassName());
            }

            // 获取有调用String及StringUtil相关类的indexOf、substring方法的调用方法
            recordCallerMethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(String.class.getName(), methodNameList);
            for (String calleeStringUtilClassName : stringUtilCalleeClassNameList) {
                Assert.assertTrue(recordCallerMethodCallByEEDetailHandler.handleMethodCallByEECMKeyword(calleeStringUtilClassName, methodNameList));
            }

            Set<FullMethodWithReturnType> indexOfCallerMethodSet = recordCallerMethodCallByEEDetailHandler.getIndexOfCallerMethodSet();
            Set<FullMethodWithReturnType> substringCallerMethodSet = recordCallerMethodCallByEEDetailHandler.getSubstringCallerMethodSet();
            if (indexOfCallerMethodSet.isEmpty() || substringCallerMethodSet.isEmpty()) {
                return;
            }

            List<FullMethodWithReturnType> commonCallerFullMethodList = new ArrayList<>();
            for (FullMethodWithReturnType indexOfMethod : indexOfCallerMethodSet) {
                if (substringCallerMethodSet.contains(indexOfMethod)) {
                    commonCallerFullMethodList.add(indexOfMethod);
                }
            }
            if (commonCallerFullMethodList.isEmpty()) {
                return;
            }

            // 处理同时调用了String及StringUtil相关类的indexOf、substring方法的调用方法的方法调用
            List<String> allCalleeClassNameList = new ArrayList<>(stringUtilCalleeClassNameList.size() + 1);
            allCalleeClassNameList.add(String.class.getName());
            allCalleeClassNameList.addAll(stringUtilCalleeClassNameList);

            for (FullMethodWithReturnType commonCallerFullMethod : commonCallerFullMethodList) {
                Assert.assertTrue(showAllMethodCallByERDetailHandler.handleMethodCallByER(commonCallerFullMethod.getFullMethod(), commonCallerFullMethod.getReturnType(),
                        allCalleeClassNameList));
            }
        }
    }
}
