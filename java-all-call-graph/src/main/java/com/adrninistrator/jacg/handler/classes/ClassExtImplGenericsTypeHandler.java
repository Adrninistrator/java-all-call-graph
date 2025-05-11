package com.adrninistrator.jacg.handler.classes;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassExtImplGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassSignatureGenericsType;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 类的继承或实现的泛型信息查询处理类
 */
public class ClassExtImplGenericsTypeHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(ClassExtImplGenericsTypeHandler.class);

    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public ClassExtImplGenericsTypeHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        this.jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public ClassExtImplGenericsTypeHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 查询指定类在继承指定父类/实现指定接口时对应泛型类型的类名列表
     *
     * @param className
     * @param superOrItfClassName
     * @return
     */
    private List<String> queryExtImplGenericsTypeList(String className, String superOrItfClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CEIGT_QUERY_GENERICS_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CEIGT_GENERICS_TYPE_NAD +
                    " from " + DbTableInfoEnum.DTIE_CLASS_EXT_IMPL_GENERICS_TYPE.getTableName() +
                    " where " + DC.CEIGT_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.CEIGT_SUPER_ITF_CLASS_NAME + " = ?" +
                    " and " + DC.CEIGT_GENERICS_TYPE_NAD + " != ''" +
                    " order by " + JACGSqlUtil.joinColumns(DC.CEIGT_SEQ, DC.CEIGT_GENERICS_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(className), superOrItfClassName);
    }

    /**
     * 查询指定类在继承指定父类/实现指定接口时对应泛型类型的信息列表
     *
     * @param className
     * @param superOrItfClassName
     * @return
     */
    private List<WriteDbData4ClassExtImplGenericsType> queryExtImplGenericsInfoList(String className, String superOrItfClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CEIGT_QUERY_ALL;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_EXT_IMPL_GENERICS_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_EXT_IMPL_GENERICS_TYPE.getTableName() +
                    " where " + DC.CEIGT_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.CEIGT_SUPER_ITF_CLASS_NAME + " = ?" +
                    " order by " + JACGSqlUtil.joinColumns(DC.CEIGT_SEQ, DC.CEIGT_GENERICS_SEQ);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4ClassExtImplGenericsType.class, dbOperWrapper.querySimpleClassName(className), superOrItfClassName);
    }

    /**
     * 获取 fromClassName 类在直接或间接继承或实现 toClassName 类时，类签名中的泛型类型列表
     *
     * @param fromClassName
     * @param toClassName
     * @return
     */
    public List<String> getExtImplGenericsType4ClassTo(String fromClassName, String toClassName) {
        // 获取 fromClassName 逐层向上继承或实现到 toClassName 中间经过的父类或接口类名
        List<String> classNameList = jacgExtendsImplHandler.getPathToSuperImplClassByFull(fromClassName, toClassName);
        if (JavaCG2Util.isCollectionEmpty(classNameList)) {
            return Collections.emptyList();
        }
        if (classNameList.size() == 1) {
            // fromClassName 直接继承或实现 toClassName，直接查询
            return queryExtImplGenericsTypeList(fromClassName, toClassName);
        }
        // fromClassName 间接继承或实现了 toClassName

        // 获得类在直接或间接继承或实现时，类的签名中的泛型类型序号的映射关系
        Map<Integer, Integer> genericsTypeSeqMap = getExtImplGenericsTypeSeqMapping(classNameList);
        if (JavaCG2Util.isMapEmpty(genericsTypeSeqMap)) {
            return Collections.emptyList();
        }

        // 查询 fromClassName 直接继承/实现的类时泛型类型列表
        List<String> genericsTypeList = queryExtImplGenericsTypeList(fromClassName, classNameList.get(0));

        List<String> returnGenericsTypeList = new ArrayList<>(genericsTypeList.size());

        List<Integer> seqList = new ArrayList<>(genericsTypeSeqMap.keySet());
        Collections.sort(seqList);
        for (Integer upwardSeq : seqList) {
            Integer downwardSeq = genericsTypeSeqMap.get(upwardSeq);
            if (downwardSeq >= genericsTypeList.size()) {
                // 对于继承/实现 toClassName 时需要指定的泛型类型， fromClassName 在继承/实现对应的父类/接口时，不存在指定序号的泛型类型
                return Collections.emptyList();
            }
            returnGenericsTypeList.add(genericsTypeList.get(downwardSeq));
        }
        return returnGenericsTypeList;
    }

    /**
     * 获得类在直接或间接继承或实现时，类的签名中的泛型类型序号的映射关系
     *
     * @param classNameList JACGExtendsImplHandler#getPathToSuperImplClassByFull 方法返回值
     * @return key fromClassName 在直接或间接继承或实现 toClassName 时的泛型类型序号
     * value fromClassName 的类签名定义中的泛型类型序号
     */
    private Map<Integer, Integer> getExtImplGenericsTypeSeqMapping(List<String> classNameList) {
        int classNameListSize = classNameList.size();
        List<Map<Integer, Integer>> seqMappingList = new ArrayList<>(classNameListSize - 1);
        // 循环次数=列表数量-1
        for (int i = 0; i < classNameListSize - 1; i++) {
            String upwardClassName = classNameList.get(classNameListSize - i - 1);
            String downwardClassName = classNameList.get(classNameListSize - i - 2);
            // 查询子类/实现类的签名中的泛型信息
            List<WriteDbData4ClassSignatureGenericsType> downwardClassSignatureGenericsTypeList = queryClassSignatureGenericsTypeList(downwardClassName);
            if (JavaCG2Util.isCollectionEmpty(downwardClassSignatureGenericsTypeList)) {
                return Collections.emptyMap();
            }
            // 查询子类/实现类在继承指定父类/实现指定接口时对应泛型类型的信息列表
            List<WriteDbData4ClassExtImplGenericsType> classExtImplGenericsTypeList = queryExtImplGenericsInfoList(downwardClassName, upwardClassName);

            Map<Integer, Integer> currentMap = new HashMap<>();
            for (WriteDbData4ClassExtImplGenericsType classExtImplGenericsType : classExtImplGenericsTypeList) {
                // 获取继承/实现关系中，父类/接口签名中某个泛型类型变量名称在子类/实现类继承/实现时的序号
                Integer downwardSeq = getDownwardClassSignatureGenericsTypeMappingSeq(classExtImplGenericsType, downwardClassSignatureGenericsTypeList);
                if (downwardSeq != null) {
                    currentMap.put(classExtImplGenericsType.getSeq(), downwardSeq);
                }
            }
            seqMappingList.add(currentMap);
        }
        // 按照每一层继承/实现中泛型类型变量名称映射关系进行处理
        Map<Integer, Integer> returnMap = new HashMap<>();
        for (Map<Integer, Integer> tmpMap : seqMappingList) {
            if (returnMap.isEmpty()) {
                returnMap.putAll(tmpMap);
                continue;
            }
            for (Map.Entry<Integer, Integer> entry : returnMap.entrySet()) {
                Integer seqUpward = entry.getKey();
                Integer seqDownward = entry.getValue();
                Integer nextSeqDownward = tmpMap.get(seqDownward);
                if (nextSeqDownward == null) {
                    returnMap.remove(seqUpward);
                } else {
                    returnMap.put(seqUpward, nextSeqDownward);
                }
            }
        }

        return returnMap;
    }

    // 查询类的签名中的泛型信息
    private List<WriteDbData4ClassSignatureGenericsType> queryClassSignatureGenericsTypeList(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CSGT_QUERY_BY_SCN;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_CLASS_SIGNATURE_GENERICS_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_SIGNATURE_GENERICS_TYPE.getTableName() +
                    " where " + DC.CSGT_SIMPLE_CLASS_NAME + " = ?" +
                    " order by " + DC.CSGT_SEQ;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4ClassSignatureGenericsType.class, dbOperWrapper.querySimpleClassName(className));
    }

    // 获取继承/实现关系中，父类/接口签名中某个泛型类型变量名称在子类/实现类继承/实现时的序号
    private Integer getDownwardClassSignatureGenericsTypeMappingSeq(WriteDbData4ClassExtImplGenericsType classExtImplGenericsType,
                                                                    List<WriteDbData4ClassSignatureGenericsType> downwardClassSignatureGenericsTypeList) {
        String typeVariablesName = classExtImplGenericsType.getTypeVariablesName();
        for (WriteDbData4ClassSignatureGenericsType downwardClassSignatureGenericsType : downwardClassSignatureGenericsTypeList) {
            if (typeVariablesName.equals(downwardClassSignatureGenericsType.getTypeVariablesName())) {
                return downwardClassSignatureGenericsType.getSeq();
            }
        }
        return null;
    }
}
