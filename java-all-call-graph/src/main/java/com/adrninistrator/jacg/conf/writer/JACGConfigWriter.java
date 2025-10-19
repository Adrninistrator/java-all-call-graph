package com.adrninistrator.jacg.conf.writer;

import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.writer.BaseConfigWriter;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description:
 */
public class JACGConfigWriter extends BaseConfigWriter {

    public JACGConfigWriter(String rootDirPath) {
        super(rootDirPath);
    }

    // 选择使用的主要配置参数值
    @Override
    protected String chooseMainConfig(MainConfigInterface mainConfig) {
        if (baseConfigureWrapper != null) {
            Object value = baseConfigureWrapper.getMainConfig(mainConfig, false);
            String printValue = JavaCG2Util.getObjectPrintValue(value);
            if (ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH == mainConfig) {
                // H2数据库文件需要保存绝对路径形式，且分隔符需要使用/
                File file = new File(printValue);
                printValue = JavaCG2FileUtil.getCanonicalPath(file);
                printValue = JavaCG2FileUtil.replaceFilePath2Slash(printValue);
            }
            return printValue;
        }
        return mainConfig.getDefaultValue();
    }

    @Override
    protected String chooseElExampleText() {
        return JavaCG2Constants.NEW_LINE_WINDOWS + "字符串比较的表达式示例可参考 " + InputDirEnum.IDE_EL_EXAMPLE.getDirName() + "/" + JavaCG2Constants.EL_STRING_COMPARE_MD_FILE_NAME +
                JavaCG2Constants.NEW_LINE_WINDOWS +
                JavaCG2Constants.NEW_LINE_WINDOWS + "不同场景的表达式示例可参考 " + InputDirEnum.IDE_EL_EXAMPLE.getDirName() + " 目录中子目录的对应文件"
                ;
    }

    @Override
    protected String chooseElDebugModeText() {
        return "将配置文件 " + ConfigKeyEnum.CKE_EL_DEBUG_MODE.getFileName() + " 的 " + ConfigKeyEnum.CKE_EL_DEBUG_MODE.getKey() + " 参数设置为 " +
                Boolean.TRUE + " 可以使表达式执行时开启调试模式，会在应用日志中输出表达式执行时的详细信息";
    }
}
