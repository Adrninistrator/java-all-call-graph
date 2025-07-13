package com.adrninistrator.jacg.conf.writer;

import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
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
        // todo 后续补充
        return "";
    }

    @Override
    protected String chooseElDebugModeText() {
        // todo 后续补充
        return "";
    }
}
