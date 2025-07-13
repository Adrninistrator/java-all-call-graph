package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisMySqlInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisResultMap;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisResultMapResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 从MyBatis的XML文件获取Entity与数据库字段名（支持MySQL数据库）
 */
public class MyBatisMySqlColumnInfoCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlColumnInfoCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_column";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    // 指定不需要处理文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return null;
    }

    // 不需要处理文件
    @Override
    public boolean parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        return true;
    }

    public void handleMyBatisMySqlInfo(MyBatisMySqlInfo myBatisMySqlInfo, String mybatisXmlFilePath) {
        try {
            String entityClassName = myBatisMySqlInfo.getEntityClassName();

            Map<String, MyBatisResultMap> resultMapMap = myBatisMySqlInfo.getResultMapMap();
            if (!JavaCG2Util.isMapEmpty(resultMapMap)) {
                List<String> resultMapIdList = new ArrayList<>(resultMapMap.keySet());
                Collections.sort(resultMapIdList);
                for (String resultMapId : resultMapIdList) {
                    MyBatisResultMap myBatisResultMap = resultMapMap.get(resultMapId);
                    for (MyBatisResultMapResult myBatisResultMapResult : myBatisResultMap.getResultMapResultList()) {
                        writeData2File(entityClassName, resultMapId, myBatisResultMapResult.getJavaEntityFieldName(), myBatisResultMapResult.getDbColumnName(),
                                myBatisResultMapResult.getDbColumnType(), mybatisXmlFilePath);
                    }
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
