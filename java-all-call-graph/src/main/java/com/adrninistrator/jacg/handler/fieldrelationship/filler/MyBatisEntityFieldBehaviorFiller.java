package com.adrninistrator.jacg.handler.fieldrelationship.filler;

import com.adrninistrator.jacg.common.JACGSqlStatementConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSGetSetDb;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipFlagsEnum;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipIdTypeEnum;
import com.adrninistrator.jacg.handler.common.enums.MyBatisColumnRelateDescEnum;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior;
import com.adrninistrator.jacg.handler.dto.field.FieldBehavior4MyBatisEntity;
import com.adrninistrator.jacg.handler.mybatis.MyBatisMSMapperEntityHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/30
 * @description: 为MyBatis的Entity字段行为填充信息的类
 */
public class MyBatisEntityFieldBehaviorFiller extends BaseHandler implements FieldBehaviorFillerInterface {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisEntityFieldBehaviorFiller.class);

    private final MyBatisMSMapperEntityHandler myBatisMSMapperEntityHandler;

    public MyBatisEntityFieldBehaviorFiller(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
    }

    public MyBatisEntityFieldBehaviorFiller(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
    }

    @Override
    public List<FieldBehavior> fillIn(FieldBehavior fieldBehavior, FieldRelationshipIdTypeEnum fieldRelationshipIdTypeEnum, int id) {
        int relationShipFlags = fieldBehavior.getRelationShipFlags();
        // 仅当使用通过get/set方法的字段关系关系ID查询时，需要判断字段关联关系的标志
        boolean needCheckFlags = FieldRelationshipIdTypeEnum.FRITE_FIELD_RELATIONSHIP_ID == fieldRelationshipIdTypeEnum;
        List<FieldBehavior> fieldBehaviorList = null;
        if (fieldBehavior.isGetOrSet() && (!needCheckFlags || FieldRelationshipFlagsEnum.FRF_GET_MYBATIS_SELECT_OBJECT.checkFlag(relationShipFlags))) {
            // 当前对应get方法，处理MyBatis Mapper方法返回对应的数据库信息
            fieldBehaviorList = handleMyBatisMapperArgReturnDbInfo(fieldBehavior, fieldRelationshipIdTypeEnum, id);
        }
        if (!fieldBehavior.isGetOrSet() && (!needCheckFlags || FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_MAPPER_ARG_OPERATE.checkFlag(relationShipFlags))) {
            // 当前对应set方法，处理MyBatis Mapper方法参数对应的数据库信息
            fieldBehaviorList = handleMyBatisMapperArgReturnDbInfo(fieldBehavior, fieldRelationshipIdTypeEnum, id);
        }
        if (!JavaCG2Util.isCollectionEmpty(fieldBehaviorList)) {
            return fieldBehaviorList;
        }

        String className = fieldBehavior.getClassName();
        // 尝试通过当前类名查询对应的数据库表名
        String tableName = myBatisMSMapperEntityHandler.queryTableNameByMyBatisEntity(className);
        if (tableName == null) {
            return null;
        }
        // 根据MyBatis的Entity的类名及字段名，查询对应的数据库表列名
        String columnName = myBatisMSMapperEntityHandler.queryColumnNameByMyBatisEntity(className, fieldBehavior.getFieldName());
        String dbOperate = null;
        // 假如是使用了get/set方法调用ID，则以下使用的字段关联关系标志为0，无法获取到对应的数据库操作，暂时2影响不大
        if (FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_INSERT_ENTITY.checkFlag(relationShipFlags)) {
            dbOperate = JACGSqlStatementConstants.INSERT;
        } else if (FieldRelationshipFlagsEnum.FRF_GET_MYBATIS_SELECT_ENTITY.checkFlag(relationShipFlags)) {
            dbOperate = JACGSqlStatementConstants.SELECT;
        } else if (FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_UPDATE_SET_ENTITY.checkFlag(relationShipFlags)) {
            dbOperate = JACGSqlStatementConstants.UPDATE_SET;
        } else if (FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_UPDATE_WHERE_ENTITY.checkFlag(relationShipFlags)) {
            dbOperate = JACGSqlStatementConstants.UPDATE_WHERE;
        }
        return Collections.singletonList(new FieldBehavior4MyBatisEntity(fieldBehavior, tableName, columnName, dbOperate, MyBatisColumnRelateDescEnum.MBCRD_ENTITY.getType()));
    }

    // 处理MyBatis Mapper方法参数或返回对应的数据库信息
    private List<FieldBehavior> handleMyBatisMapperArgReturnDbInfo(FieldBehavior fieldBehavior, FieldRelationshipIdTypeEnum fieldRelationshipIdTypeEnum, int id) {
        // 从MyBatis的Mapper方法参数所对应的数据库信息表根据通过get/set方法关联的字段关系ID查询相关信息
        List<WriteDbData4MybatisMSGetSetDb> mybatisMSGetSetDbList = null;
        if (FieldRelationshipIdTypeEnum.FRITE_FIELD_RELATIONSHIP_ID == fieldRelationshipIdTypeEnum) {
            mybatisMSGetSetDbList = myBatisMSMapperEntityHandler.queryMybatisMSGetSetDbInfoByFldRelationshipId(id);
        } else if (FieldRelationshipIdTypeEnum.FRITE_GET_METHOD_CALL_ID == fieldRelationshipIdTypeEnum) {
            mybatisMSGetSetDbList = myBatisMSMapperEntityHandler.queryMybatisMSGetSetDbInfoByGetMethodCallId(id);
        } else if (FieldRelationshipIdTypeEnum.FRITE_SET_METHOD_CALL_ID == fieldRelationshipIdTypeEnum) {
            mybatisMSGetSetDbList = myBatisMSMapperEntityHandler.queryMybatisMSGetSetDbInfoBySetMethodCallId(id);
        }
        if (JavaCG2Util.isCollectionEmpty(mybatisMSGetSetDbList)) {
            logger.warn("从MyBatis的Mapper方法参数所对应的数据库信息表根据通过get/set方法关联的字段关系ID查询相关信息不存在 {} {}", fieldRelationshipIdTypeEnum, id);
            return null;
        }
        List<FieldBehavior> fieldBehaviorList = new ArrayList<>();
        for (WriteDbData4MybatisMSGetSetDb mybatisMSGetSetDb : mybatisMSGetSetDbList) {
            FieldBehavior4MyBatisEntity fieldBehavior4MyBatisEntity = new FieldBehavior4MyBatisEntity(fieldBehavior, mybatisMSGetSetDb.getTableName(),
                    mybatisMSGetSetDb.getColumnName(), mybatisMSGetSetDb.getDbOperate(), mybatisMSGetSetDb.getColumnRelateDesc());
            fieldBehaviorList.add(fieldBehavior4MyBatisEntity);
        }
        return fieldBehaviorList;
    }
}
