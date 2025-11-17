package com.adrninistrator.jacg.compatibility.handler;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassReference;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/8/10
 * @description: 检查被引用的类处理类
 */
public class ClassReferenceCheckHandler extends BaseCompatibilityCheckHandler implements QueryByPageCallBack<String> {

    // 代表当前查询的唯一类名
    private String currentSimpleClassName = null;

    private Map<Integer, WriteDbData4JarInfo> jarInfoMap;

    private WriterSupportHeader writer;

    private ElManager elManager;

    public ClassReferenceCheckHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 检查类中引用的类是否存在
    public boolean check() {
        return QueryByPageHandler.queryAndHandle(this, 0);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        // 当前方法的返回值不使用，返回任意值
        return 0;
    }

    @Override
    public List<String> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        List<String> list;
        if (currentSimpleClassName == null) {
            // 首次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CR_QUERY_SIMPLE_CLASS_NAME_FIRST;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select distinct(" + DC.CR_SIMPLE_CLASS_NAME + ")" +
                        " from " + DbTableInfoEnum.DTIE_CLASS_REFERENCE.getTableName() +
                        " order by " + DC.CR_SIMPLE_CLASS_NAME +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            list = dbOperator.queryListOneColumn(sql, String.class, JACGConstants.DB_PAGE_HANDLE_SIZE);
        } else {
            // 非首次查询
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CR_QUERY_SIMPLE_CLASS_NAME_BY_PAGE;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = "select distinct(" + DC.CR_SIMPLE_CLASS_NAME + ")" +
                        " from " + DbTableInfoEnum.DTIE_CLASS_REFERENCE.getTableName() +
                        " where " + DC.CR_SIMPLE_CLASS_NAME + " > ?" +
                        " order by " + DC.CR_SIMPLE_CLASS_NAME +
                        " limit ?";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }
            list = dbOperator.queryListOneColumn(sql, String.class, currentSimpleClassName, JACGConstants.DB_PAGE_HANDLE_SIZE);
        }

        if (!JavaCG2Util.isCollectionEmpty(list)) {
            // 查询结果非空，修改当前查询的唯一类名
            currentSimpleClassName = list.get(list.size() - 1);
        }
        return list;
    }

    @Override
    public boolean handleDataList(List<String> dataList, Object... argsByPage) throws Exception {
        for (String simpleClassName : dataList) {
            // 查询完整类名
            String className = dbOperWrapper.queryClassNameBySimple(simpleClassName);
            // 通过类名查询被引用的类名
            List<WriteDbData4ClassReference> classReferenceList = classInfoHandler.queryReferencedClassInfo(className);
            if (classReferenceList.isEmpty()) {
                continue;
            }
            for (WriteDbData4ClassReference classReference : classReferenceList) {
                // 检查指定的类是否存在
                if (checkClassExists(classReference.getReferencedClassName())) {
                    continue;
                }
                // 被引用的类不存在
                WriteDbData4ClassInfo classInfo = classInfoHandler.queryClassInfoByClassName(className);
                if (elManager.checkIgnoreJCCClassReference(className, classReference.getReferencedClassName())) {
                    // 忽略当前类引用关系
                    continue;
                }

                String conditionalOnClassValue = queryConditionalOnClassValueStr(className);
                WriteDbData4JarInfo jarInfo = jarInfoMap.get(classReference.getJarNum());
                writer.writeDataInLine(className,
                        conditionalOnClassValue,
                        classInfo.getClassPathInJar(),
                        jarInfo.getJarFullPath(),
                        jarInfo.getInnerJarPath(),
                        classReference.getReferencedClassName());
            }
        }
        return true;
    }

    // 查询结果为空时需要结束循环查询
    @Override
    public boolean exitWhenQueryEmpty() {
        return true;
    }

    public void setJarInfoMap(Map<Integer, WriteDbData4JarInfo> jarInfoMap) {
        this.jarInfoMap = jarInfoMap;
    }

    public void setWriter(WriterSupportHeader writer) {
        this.writer = writer;
    }

    public void setElManager(ElManager elManager) {
        this.elManager = elManager;
    }
}
