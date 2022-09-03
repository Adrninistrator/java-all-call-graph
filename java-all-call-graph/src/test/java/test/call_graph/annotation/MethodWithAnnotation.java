package test.call_graph.annotation;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;

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
        enum1 = ConfigKeyEnum.CKE_CALL_GRAPH_JAR_LIST,
        annotation1 = @TestAnnotationInner(valueA = "Cva1", valueB = "Cvb1"))
public class MethodWithAnnotation {

    @TestAnnotation(
            strValue = "bbb\r\n",
            intValue = 222,
            intArrayValue = {11, 12, 13, 14},
            clazz1 = DbOperWrapper.class,
            enum1 = ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION,
            annotation1 = @TestAnnotationInner(valueA = "va1", valueB = "vb1"))
    public void test1() {
        System.out.println("");
    }

    @TestAnnotationOuter(value = "aaa",
            annotations = {
                    @TestAnnotationInner(valueA = "va1", valueB = "vb1\r\n"),
                    @TestAnnotationInner(valueA = "va2", valueB = "va2")
            }
    )
    public void test2() {
        System.out.println("");
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
    }
}
