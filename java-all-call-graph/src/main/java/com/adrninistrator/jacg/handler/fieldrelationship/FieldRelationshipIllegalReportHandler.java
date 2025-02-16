package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSEntity;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.mybatis.MyBatisMSMapperEntityHandler;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/10/2
 * @description: 字段关联关系非法情况报告生成类（get/set方法关联关系）
 */
public class FieldRelationshipIllegalReportHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(FieldRelationshipIllegalReportHandler.class);

    private final MyBatisMSMapperEntityHandler myBatisMSMapperEntityHandler;

    public FieldRelationshipIllegalReportHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
    }

    public FieldRelationshipIllegalReportHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
    }

    /**
     * 生成字段关联关系非法情况报告
     *
     * @param reportFilePath 生成的报告文件路径
     * @return true: 生成成功 false: 生成失败
     */
    public boolean genIllegalReport(String reportFilePath) {
        try (MarkdownWriter markdownWriter = new MarkdownWriter(reportFilePath, true)) {
            // 查询在mybatis_ms_entity表中一个Entity对应多个数据库表的情况
            Map<String, List<WriteDbData4MybatisMSEntity>> entityWithMultiTableMap = myBatisMSMapperEntityHandler.queryEntityWithMultiTableMap();
            if (entityWithMultiTableMap.isEmpty()) {
                return true;
            }
            markdownWriter.addTitle(1, "一个Entity对应多个数据库表的情况");
            List<String> multiEntitySimpleClassNameList = new ArrayList<>(entityWithMultiTableMap.keySet());
            Collections.sort(multiEntitySimpleClassNameList);
            for (String multiEntitySimpleClassName : multiEntitySimpleClassNameList) {
                markdownWriter.addTitle(2, multiEntitySimpleClassName);
                List<WriteDbData4MybatisMSEntity> mybatisMSEntityList = entityWithMultiTableMap.get(multiEntitySimpleClassName);
                markdownWriter.addListWithNewLine("Entity完整类名");
                markdownWriter.addLineWithNewLine(mybatisMSEntityList.get(0).getEntityClassName());
                markdownWriter.addTableHead("数据库表名", "MyBatis XML文件路径");
                mybatisMSEntityList.sort(Comparator.comparing(WriteDbData4MybatisMSEntity::getTableName));
                for (WriteDbData4MybatisMSEntity mybatisMSEntity : mybatisMSEntityList) {
                    markdownWriter.addTableBody(mybatisMSEntity.getTableName(), mybatisMSEntity.getXmlFilePath());
                }
                markdownWriter.addEmptyLine();
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }
}
