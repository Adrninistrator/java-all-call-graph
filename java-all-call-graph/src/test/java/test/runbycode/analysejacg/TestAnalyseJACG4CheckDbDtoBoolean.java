package test.runbycode.analysejacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.ogm.annotation.Transient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/11/15
 * @description:
 */
public class TestAnalyseJACG4CheckDbDtoBoolean {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJACG4CheckDbDtoBoolean.class);

    @Test
    public void test() throws Exception {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, this);
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
             FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(dbOperWrapper);
             AnnotationHandler annotationHandler = new AnnotationHandler(dbOperWrapper)) {
            List<String> writeDbDataClassNameList = jacgExtendsImplHandler.queryChildClassListByFull(BaseWriteDbData.class.getName(), false, true, false, true);
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(writeDbDataClassNameList));
            for (String writeDbDataClassName : writeDbDataClassNameList) {
                List<WriteDbData4FieldInfo> writeDbData4FieldInfoList = fieldInfoHandler.queryFieldInfoByClassName(writeDbDataClassName);
                if (JavaCG2Util.isCollectionEmpty(writeDbData4FieldInfoList)) {
                    continue;
                }
                for (WriteDbData4FieldInfo fieldInfo : writeDbData4FieldInfoList) {
                    if (JavaCG2YesNoEnum.isYes(fieldInfo.getStaticFlag()) || JavaCG2YesNoEnum.isYes(fieldInfo.getFinalFlag())) {
                        continue;
                    }
                    if (!StringUtils.equalsAny(fieldInfo.getFieldType(), boolean.class.getName(), Boolean.class.getName())) {
                        continue;
                    }
                    Map<String, BaseAnnotationAttribute> transientAnnotationMap = annotationHandler.queryAnnotationAttributes4Field(writeDbDataClassName,
                            fieldInfo.getFieldName(), Transient.class.getName());
                    if (JavaCG2Util.isMapEmpty(transientAnnotationMap)) {
                        logger.error("数据库dto字段类型不允许使用 boolean {} {} {}", writeDbDataClassName, fieldInfo.getFieldName(), fieldInfo.getFieldType());
                        Assert.fail("数据库dto字段类型不允许使用 boolean");
                    }
                }
            }
        }

        logger.info("检查通过");
    }
}
