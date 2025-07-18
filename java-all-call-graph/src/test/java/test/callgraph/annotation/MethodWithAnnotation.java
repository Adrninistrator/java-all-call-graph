package test.callgraph.annotation;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import test.callgraph.cyclecall.TestCycleCall1;
import test.callgraph.methodargument.TestArgumentGenerics1;
import test.callgraph.methodreturn.TestMethodReturnGenericsType1;

/**
 * @author adrninistrator
 * @date 2022/8/20
 * @description:
 */

@TestAnnotation(
        strValue = "aaa\r\n",
        intValue = 111,
        intArrayValue = {1, 2, 3, 4},
        clazz1 = DbOperator.class,
        enum1 = ConfigKeyEnum.CKE_APP_NAME,
        annotation1 = @TestAnnotationInner(valueA = "Cva1", valueB = "Cvb1"))
public class MethodWithAnnotation extends TestParentClass1 {

    @TestAnnotation(
            strValue = "bbb\r\n",
            intValue = 222,
            intArrayValue = {11, 12, 13, 14},
            clazz1 = DbOperWrapper.class,
            enum1 = ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL,
            annotation1 = @TestAnnotationInner(valueA = "va1", valueB = "vb1"))
    public void test1() {
        System.out.println("");
        clone();
        test2();
        test3();
    }

    @TestAnnotationOuter(value = "aaa",
            annotations = {
                    @TestAnnotationInner(valueA = "va1", valueB = "vb1\r\n"),
                    @TestAnnotationInner(valueA = "va2", valueB = "va2")
            }
    )
    public void test2() {
        System.out.println("");
        test1();
        clone();
    }

    @TestAnnotationOuter2(value = "333",
            annotations = {
                    @TestAnnotationOuter(value = "aaa",
                            annotations = {
                                    @TestAnnotationInner(valueA = "va1", valueB = "va1"),
                                    @TestAnnotationInner(valueA = "va2", valueB = "va2\r\n")
                            }
                    ),
                    @TestAnnotationOuter(value = "bbb",
                            annotations = {
                                    @TestAnnotationInner(valueA = "vb1", valueB = "vb1"),
                                    @TestAnnotationInner(valueA = "vb2", valueB = "vb2")
                            }
                    )
            }
    )
    public void test3() {
        System.out.println("");
        test2();
        clone();
        TestArgumentGenerics1.testAll(0, null);
        new TestCycleCall1().test1();
        new TestMethodReturnGenericsType1().test3();
    }

    @TestAnnotationOuter(value = "aaa", annotations = {})
    @Override
    public MethodWithAnnotation clone() {
        return new MethodWithAnnotation();
    }

    private void useClone() {
        Object object1 = this.clone();
        TestParentClass1 testParentClass1 = this.clone();
        MethodWithAnnotation methodWithAnnotation1 = this.clone();

        System.out.println(object1.hashCode());
        System.out.println(testParentClass1.hashCode());
        System.out.println(methodWithAnnotation1.hashCode());
    }
}
