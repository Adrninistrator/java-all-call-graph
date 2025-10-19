package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/9/18
 * @description:
 */
public class PropertiesConfCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(PropertiesConfCodeParser.class);

    public static final String FILE_NAME = "properties_conf";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_PROPERTIES};
    }

    @Override
    public boolean parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        logger.info("处理{}文件 {}", JACGConstants.EXT_PROPERTIES, jarEntryPath);
        Properties properties = new Properties();
        try {
            properties.load(inputStream);
            Set<Object> keySet = properties.keySet();
            List<String> keyList = new ArrayList<>();
            for (Object keyObj : keySet) {
                keyList.add((String) keyObj);
            }
            Collections.sort(keyList);

            for (String key : keyList) {
                String value = properties.getProperty(key);
                JavaCG2YesNoEnum useBase64 = JavaCG2YesNoEnum.NO;
                if (JavaCG2Util.checkNeedBase64(value)) {
                    // 假如properties文件的值中包含回车换行Tab，则需要进行BASE64编码
                    value = JavaCG2Util.base64Encode(value);
                    useBase64 = JavaCG2YesNoEnum.YES;
                }
                writeData2File(jarEntryPath, useBase64.getStrValue(), key, value);
            }
        } catch (Exception e) {
            logger.warn("error ", e);
        }
        // 某些.properties文件会处理失败，都返回处理成功
        return true;
    }
}
