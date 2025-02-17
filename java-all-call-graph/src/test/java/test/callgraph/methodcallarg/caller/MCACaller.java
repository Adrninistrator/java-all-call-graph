package test.callgraph.methodcallarg.caller;

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
        MCAUtil.run("ok", 1);
    }

    public void testString2() {
        MCAUtil.run(MCAConstants1.FLAG1, 1);
    }

    public void testString3() {
        MCAUtil.run("ok" + MCAConstants1.FLAG1, 1);
    }

    public void testString4() {
        MCAUtil.run(MCAConstants2.FLAG_COMBINED, 1);
    }

    public void testString5() {
        MCAUtil.run("ok" + MCAConstants2.FLAG_COMBINED, 1);
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
