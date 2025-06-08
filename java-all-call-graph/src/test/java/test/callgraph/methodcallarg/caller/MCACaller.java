package test.callgraph.methodcallarg.caller;

import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import test.callgraph.methodcallarg.common.MCAConstants1;
import test.callgraph.methodcallarg.common.MCAConstants2;
import test.callgraph.methodcallarg.common.enums.MCAEnum1;
import test.callgraph.methodcallarg.util.MCAUtil;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description:
 */
public class MCACaller {

    public void testString1() {
        MCAUtil.run("ok", 1, "a");
    }

    public void testString2() {
        MCAUtil.run(MCAConstants1.FLAG1, 1, 2, 3);
    }

    public void testString3() {
        MCAUtil.run("ok" + MCAConstants1.FLAG1, 1, new Object[]{1, "a"});
    }

    public void testString4() {
        MCAUtil.run(MCAConstants2.FLAG_COMBINED, 1, "a", "b", "c");
    }

    public void testString5() {
        MCAUtil.run("ok" + MCAConstants2.FLAG_COMBINED, 1, "a", 1, "b");
    }

    public void testEnum() {
        MCAUtil.run2(JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD);
    }

    public void testEnum2() {
        boolean flag = System.currentTimeMillis() % 7 == 1;
        MCAUtil.run2(flag ? JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD : JavaCG2CallTypeEnum.CTE_MANUAL_ADDED);
    }

    public void testEnumName() {
        MCAUtil.run(MCAEnum1.ENUM_1.name(), 1);
    }

    public void testEnumGet1() {
        MCAUtil.run(MCAEnum1.ENUM_1.getKey(), MCAEnum1.ENUM_1.getSeq());
    }

    public void testEnumToString() {
        MCAUtil.run(MCAEnum1.ENUM_1.toString(), 1);
    }

    public void testNotAllowedFunction1() {
        MCAUtil.run(MCAUtil.getFlag(), 1);
    }
}
