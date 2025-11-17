package test.runbycode.el.compatibilityfast;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import org.junit.Test;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/10/30
 * @description:
 */
public class TestCompatibilityCheckFastEePackageName extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " != 'java.lang'";
    }

    @Override
    protected String chooseTitle() {
        return "Jar兼容性检查快速模式判断被引用类包名";
    }

    @Override
    protected String chooseDesc() {
        return "Jar兼容性检查快速模式，判断被引用类包名是否等于指定值，仅处理匹配的类引用关系";
    }

    @Test
    public void test() {
        runCompatibilityCheckFast(true);
    }
}
