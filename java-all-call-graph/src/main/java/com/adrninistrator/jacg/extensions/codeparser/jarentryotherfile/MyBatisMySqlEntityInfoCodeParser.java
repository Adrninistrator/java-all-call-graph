package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisMySqlInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 从MyBatis的XML文件获取Entity与Mapper、表名（支持MySQL数据库）
 */
public class MyBatisMySqlEntityInfoCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlEntityInfoCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_entity";

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
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath) {
    }

    public void handleMyBatisMySqlInfo(MyBatisMySqlInfo myBatisMySqlInfo, String mybatisXmlFilePath) {
        try {
            String mapperInterfaceName = myBatisMySqlInfo.getMapperInterfaceName();
            String entityClassName = myBatisMySqlInfo.getEntityClassName();
            String tableName = myBatisMySqlInfo.getPossibleTableName();
            JavaCG2FileUtil.write2FileWithTab(writer, mapperInterfaceName, entityClassName, tableName, mybatisXmlFilePath);
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
