package test.runbycode.jardiffcallgraph.filter;

import com.adrninistrator.jacg.jardiff.dto.method.ModifiedMethodInfo;
import com.adrninistrator.jacg.jardiff.filter.ModifiedMethodFilterInterface;

/**
 * @author adrninistrator
 * @date 2025/6/8
 * @description:
 */
public class TestModifiedMethodFilter4CallerGraph1 implements ModifiedMethodFilterInterface {
    @Override
    public boolean skipMethod(ModifiedMethodInfo modifiedMethodInfo) {
        // 假如发生变化的方法是Service则跳过
        return modifiedMethodInfo.getFullMethod().startsWith("test.diffjar.service.");
    }
}
