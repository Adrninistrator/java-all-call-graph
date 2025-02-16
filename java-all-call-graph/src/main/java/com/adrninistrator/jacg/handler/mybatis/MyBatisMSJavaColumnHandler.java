package com.adrninistrator.jacg.handler.mybatis;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.JACGSqlStatementConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSSelectColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSTable;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSEntity;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSGetSetDb;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipFlagsEnum;
import com.adrninistrator.jacg.handler.common.enums.MyBatisColumnRelateDescEnum;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.AbstractMyBatisMapperArg;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMSMapperParamDbInfo;
import com.adrninistrator.jacg.handler.dto.mybatis.mapper.MyBatisMapperArgAndParamDbInfo;
import com.adrninistrator.jacg.handler.fieldrelationship.FieldRelationshipHandler;
import com.adrninistrator.jacg.handler.fieldrelationship.GetSetMethodHandler;
import com.adrninistrator.jacg.handler.fieldrelationship.ManualAddFieldRelationshipHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSGetSetDb;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2FieldRelationshipTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.adrninistrator.mybatismysqltableparser.common.enums.MySqlStatementEnum;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/10/31
 * @description: 写入数据库，MyBatis XML文件中sql脚本的字段与Java代码的关联关系（使用MySQL）
 */
public class MyBatisMSJavaColumnHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4MybatisMSEntity> {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMSJavaColumnHandler.class);

    private final MyBatisMSMapperEntityHandler myBatisMSMapperEntityHandler;
    private final MethodInfoHandler methodInfoHandler;
    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;
    private final FieldRelationshipHandler fieldRelationshipHandler;
    private final ManualAddFieldRelationshipHandler manualAddFieldRelationshipHandler;
    private final GetSetMethodHandler getSetMethodHandler;

    private final JavaCG2Counter recordId = new JavaCG2Counter(-1);

    private WriteDbHandler4MybatisMSGetSetDb writeDbHandler4MybatisMSGetSetDb;

    public MyBatisMSJavaColumnHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        manualAddFieldRelationshipHandler = new ManualAddFieldRelationshipHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
    }

    public MyBatisMSJavaColumnHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        myBatisMSMapperEntityHandler = new MyBatisMSMapperEntityHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        manualAddFieldRelationshipHandler = new ManualAddFieldRelationshipHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return queryEndIdByPage(currentStartId, DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY, DC.MME_RECORD_ID);
    }

    @Override
    public List<WriteDbData4MybatisMSEntity> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        return myBatisMSMapperEntityHandler.queryMyBatisEntityByPage(lastQuery, currentStartId, currentEndId);
    }

    @Override
    public boolean handleDataList(List<WriteDbData4MybatisMSEntity> dataList, Object... argsByPage) throws Exception {
        for (WriteDbData4MybatisMSEntity mybatisMSEntity : dataList) {
            // 执行处理
            doHandle(mybatisMSEntity);
        }
        return true;
    }

    /**
     * 处理MyBatis XML文件中sql脚本的字段与Java代码的关联关系
     *
     * @return
     */
    public boolean handle(String javaCG2OutputPath) {
        long startTime = System.currentTimeMillis();
        logger.info("MyBatis XML文件中sql脚本的字段与Java代码的关联关系（使用MySQL）-开始处理");

        // 人工增加get/set方法字段关联关系前的处理
        if (!manualAddFieldRelationshipHandler.beforeAdd()) {
            // 在try...finally之前返回
            return false;
        }
        try {
            writeDbHandler4MybatisMSGetSetDb.beforeHandle(javaCG2OutputPath);
            // 分页查询并处理
            return QueryByPageHandler.queryAndHandle(this, JavaCG2Constants.RECORD_ID_MIN_BEFORE);
        } catch (Exception e) {
            logger.error("出现异常 ", e);
            return false;
        } finally {
            // 写入最后的数据
            writeDbHandler4MybatisMSGetSetDb.afterHandle();
            // 人工增加get/set方法字段关联关系后的处理
            manualAddFieldRelationshipHandler.afterAdd();
            logger.info("MyBatis XML文件中sql脚本的字段与Java代码的关联关系（使用MySQL）-处理完毕，耗时: {} 秒", JavaCG2Util.getSpendSeconds(startTime));
        }
    }

    // 执行处理
    private void doHandle(WriteDbData4MybatisMSEntity mybatisMSEntity) {
        // 根据MyBatis Mapper唯一类名查询 mybatis_ms_table 表中的Mapper方法名与sql语句类型
        List<WriteDbData4MyBatisMSTable> myBatisMSTableList = myBatisMSMapperEntityHandler.queryMapperMethodAndSqlStatement(mybatisMSEntity.getMapperSimpleClassName());
        if (JavaCG2Util.isCollectionEmpty(myBatisMSTableList)) {
            logger.info("未查询到Mapper方法名与sql语句类型 {}", mybatisMSEntity.getMapperSimpleClassName());
            return;
        }

        // 保存MyBatis Mapper不同操作的方法名称
        List<String> insertMapperMethodNameList = new ArrayList<>();
        List<String> updateMapperMethodNameList = new ArrayList<>();
        List<String> deleteMapperMethodNameList = new ArrayList<>();
        List<String> selectMapperMethodNameList = new ArrayList<>();
        List<String> onlySelectMapperMethodNameList = new ArrayList<>();

        // 查找包含指定SQL语句类型的Mapper方法
        findMethodByStatement(myBatisMSTableList, insertMapperMethodNameList, MySqlStatementEnum.DSSE_INSERT, MySqlStatementEnum.DSSE_INSERT_IGNORE,
                MySqlStatementEnum.DSSE_INSERT_OR_UPDATE, MySqlStatementEnum.DSSE_REPLACE);
        findMethodByStatement(myBatisMSTableList, updateMapperMethodNameList, MySqlStatementEnum.DSSE_UPDATE);
        findMethodByStatement(myBatisMSTableList, deleteMapperMethodNameList, MySqlStatementEnum.DSSE_DELETE);
        findMethodByStatement(myBatisMSTableList, selectMapperMethodNameList, MySqlStatementEnum.DSSE_SELECT, MySqlStatementEnum.DSSE_SELECT_4_UPDATE);
        for (String selectMapperMethodName : selectMapperMethodNameList) {
            if (insertMapperMethodNameList.contains(selectMapperMethodName) || updateMapperMethodNameList.contains(selectMapperMethodName) || deleteMapperMethodNameList.contains(selectMapperMethodName)) {
                continue;
            }
            onlySelectMapperMethodNameList.add(selectMapperMethodName);
        }

        // 处理insert相关的MyBatis Mapper方法
        for (String insertMapperMethodName : insertMapperMethodNameList) {
            handleInsertMapperMethod(mybatisMSEntity, insertMapperMethodName);
        }

        // 处理select相关的MyBatis Mapper方法
        for (String onlySelectMapperMethodName : onlySelectMapperMethodNameList) {
            handleSelectMapperMethod(mybatisMSEntity, onlySelectMapperMethodName);
        }

        // 处理update相关的MyBatis Mapper方法
        for (String updateMapperMethodName : updateMapperMethodNameList) {
            handleUpdateMapperMethod(mybatisMSEntity, updateMapperMethodName);
        }
    }

    /**
     * 查找包含指定SQL语句类型的Mapper方法
     *
     * @param myBatisMSTableList
     * @param mapperMethodNameList
     * @param mySqlStatementEnums
     */
    private void findMethodByStatement(List<WriteDbData4MyBatisMSTable> myBatisMSTableList, List<String> mapperMethodNameList, MySqlStatementEnum... mySqlStatementEnums) {
        for (WriteDbData4MyBatisMSTable myBatisMSTable : myBatisMSTableList) {
            for (MySqlStatementEnum mySqlStatementEnum : mySqlStatementEnums) {
                if (!mySqlStatementEnum.getInitials().equals(myBatisMSTable.getSqlStatement())) {
                    continue;
                }

                if (!mapperMethodNameList.contains(myBatisMSTable.getMapperMethodName())) {
                    mapperMethodNameList.add(myBatisMSTable.getMapperMethodName());
                }
                break;
            }
        }
    }

    /**
     * 检查Mapper方法是否参数0为Entity类型
     *
     * @param mapperFullMethod
     * @param entityClassName
     * @return null 不是Entity类型 true: 是Entity类型 false: 是Object类型，还需要继续判断
     */
    private Boolean checkMapperMethodArg0Entity(String mapperFullMethod, String entityClassName) {
        List<String> methodArgTypeList = JACGClassMethodUtil.genMethodArgTypeList(mapperFullMethod);
        // 判断是否为MyBatis Mapper用于插入Entity数据的方法
        if (methodArgTypeList.size() != 1) {
            // 方法参数数量不为1，不是使用Entity
            return null;
        }
        // 是否需要继续检查方法参数类型
        if (StringUtils.equals(entityClassName, methodArgTypeList.get(0))) {
            // 方法参数类型为Entity
            return Boolean.TRUE;
        }
        if (JavaCG2CommonNameConstants.CLASS_NAME_OBJECT.equals(methodArgTypeList.get(0))) {
            // 方法参数类型为Object，需要继续判断
            return Boolean.FALSE;
        }
        // 方法参数不是Entity
        return null;
    }

    /**
     * 判断方法的第1个参数是否使用Entity类型
     *
     * @param methodCallInfoList
     * @param entityClassName
     * @return true: 是 false: 否
     */
    private boolean checkMethodArg0UseEntity(List<WriteDbData4MethodCallInfo> methodCallInfoList, String entityClassName) {
        boolean matchesEntityType = false;
        List<String> mismatchEntityTypeList = new ArrayList<>(1);
        // 继续检查方法参数类型
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(methodCallInfo.getType())) {
                if (StringUtils.equals(entityClassName, methodCallInfo.getTheValue())) {
                    matchesEntityType = true;
                    break;
                }
                mismatchEntityTypeList.add(methodCallInfo.getTheValue());
            }
        }
        if (!matchesEntityType) {
            logger.info("当前方法不是MyBatis使用Entity数据的方法 {} [{}]", entityClassName, StringUtils.join(mismatchEntityTypeList, "\n"));
            return false;
        }
        return true;
    }

    /**
     * 查询以Mapper接口的方法参数作为被调用对象的方法调用序号
     *
     * @param methodCall
     * @param methodCallInfo
     * @return
     */
    private List<Integer> queryMapperArgAsObjCallIdList(WriteDbData4MethodCall methodCall, WriteDbData4MethodCallInfo methodCallInfo) {
        if (StringUtils.equalsAny(methodCallInfo.getType(), JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ.getType(),
                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ_EQC.getType())) {
            // 以Mapper接口的方法参数是调用方法参数，查询以其作为被调用对象的方法调用序号
            return methodCallInfoHandler.queryCallIdInCaller4ObjByMethodCallArg(methodCall.getCallerMethodHash(), false, Integer.parseInt(methodCallInfo.getTheValue()), true,
                    methodCall.getCallId());
        }
        if (StringUtils.equalsAny(methodCallInfo.getType(), JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType(),
                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID_EQC.getType())) {
            // 以Mapper接口的方法参数是方法调用返回值，查询以其作为被调用对象的方法调用序号
            return methodCallInfoHandler.queryCallIdInCaller4ObjByMethodCallArg(methodCall.getCallerMethodHash(), true, Integer.parseInt(methodCallInfo.getTheValue()), true,
                    methodCall.getCallId());
        }
        return null;
    }

    /**
     * 对于set方法序号匹配的字段关系，设置MyBatis相关的标志
     *
     * @param mapperArgAsObjCallIdList
     */
    private void addMyBatisInsertFlags4FieldRelationshipBySet(List<Integer> mapperArgAsObjCallIdList) {
        for (Integer methodCallId : mapperArgAsObjCallIdList) {
            // 查询对应的方法调用
            WriteDbData4MethodCall methodCall = methodCallHandler.queryMethodCallByCallId(methodCallId);
            if (!JACGClassMethodUtil.calleeMatchesSetMethod(methodCall)) {
                // 通过名称判断不是set方法，跳过
                logger.info("通过名称判断不是set方法，跳过 {} {}", methodCallId, methodCall.getCalleeFullMethod());
                continue;
            }

            // 通过Mapper接口的方法参数对应的set方法调用序号查找匹配的字段关联关系
            List<WriteDbData4FieldRelationship> fieldRelationshipList = fieldRelationshipHandler.queryDirectlyRelationshipBySetMethodCallId(methodCallId);
            if (JavaCG2Util.isCollectionEmpty(fieldRelationshipList)) {
                logger.info("通过Mapper接口的方法参数对应的set方法调用序号，未查找到匹配的字段关联关系 {}", methodCallId);
                continue;
            }
            for (WriteDbData4FieldRelationship fieldRelationship : fieldRelationshipList) {
                logger.info("修改字段关联关系表的标志 {} {} {} {} {}", FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_INSERT_ENTITY, methodCallId, fieldRelationship.getFldRelationshipId(),
                        fieldRelationship.getCallerFullMethod(), fieldRelationship.getCallerLineNumber());
                fieldRelationshipHandler.updateFieldRelationshipAddFlag(fieldRelationship.getFldRelationshipId(), fieldRelationship.getRelationshipFlags(),
                        FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_INSERT_ENTITY);
            }
        }
    }

    /**
     * 处理insert相关的MyBatis Mapper方法
     *
     * @param mybatisMSEntity
     * @param insertMapperMethodName
     */
    private void handleInsertMapperMethod(WriteDbData4MybatisMSEntity mybatisMSEntity, String insertMapperMethodName) {
        logger.info("处理insert相关的MyBatis Mapper方法 {} {}", mybatisMSEntity.getMapperClassName(), insertMapperMethodName);
        // 查询Mapper方法信息
        List<WriteDbData4MethodInfo> mapperMethodInfoList = methodInfoHandler.queryMethodByClassMethodUpper(mybatisMSEntity.getMapperClassName(), insertMapperMethodName);
        if (JavaCG2Util.isCollectionEmpty(mapperMethodInfoList)) {
            return;
        }
        for (WriteDbData4MethodInfo mapperMethodInfo : mapperMethodInfoList) {
            // 检查Mapper方法是否参数0为Entity类型检查Mapper方法是否参数0为Entity类型
            Boolean argTypeIsEntity = checkMapperMethodArg0Entity(mapperMethodInfo.getFullMethod(), mybatisMSEntity.getEntityClassName());
            if (argTypeIsEntity == null) {
                continue;
            }

            logger.info("找到MyBatis Mapper用于插入Entity数据的方法 {}", mapperMethodInfo.getFullMethod());
            // 查询调用以上Mapper方法的方法
            List<WriteDbData4MethodCall> insertEntityMethodCallList = methodCallHandler.queryMethodCallByCalleeFullMethod(mapperMethodInfo.getFullMethod());
            if (JavaCG2Util.isCollectionEmpty(insertEntityMethodCallList)) {
                // 当前Mapper方法未找到调用方法
                continue;
            }
            for (WriteDbData4MethodCall insertEntityMethodCall : insertEntityMethodCallList) {
                // 处理MyBatis insert Entity操作相关的方法调用
                handleInsertEntityMethodCall(insertEntityMethodCall, argTypeIsEntity, mybatisMSEntity.getEntityClassName());
            }
        }
    }

    /**
     * 处理MyBatis insert Entity操作相关的方法调用
     *
     * @param methodCall
     * @param argTypeIsEntity
     * @param entityClassName
     */
    private void handleInsertEntityMethodCall(WriteDbData4MethodCall methodCall, boolean argTypeIsEntity, String entityClassName) {
        // 查找Mapper接口的方法被调用时的参数信息
        List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryMethodCallInfoObjArg(methodCall.getCallId(), 1);
        if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
            return;
        }

        // 判断方法的第1个参数是否使用Entity类型
        if (!argTypeIsEntity && !checkMethodArg0UseEntity(methodCallInfoList, entityClassName)) {
            return;
        }

        logger.info("当前方法是MyBatis用于插入Entity数据的方法 {} {} {} {}", methodCall.getCallerFullMethod(), methodCall.getCallerLineNumber(),
                methodCall.getCalleeFullMethod(), entityClassName);
        for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
            // 查询以Mapper接口的方法参数作为被调用对象的方法调用序号
            List<Integer> mapperArgAsObjCallIdList = queryMapperArgAsObjCallIdList(methodCall, methodCallInfo);
            if (JavaCG2Util.isCollectionEmpty(mapperArgAsObjCallIdList)) {
                continue;
            }

            logger.info("对于set方法序号匹配的字段关系，设置MyBatis相关的标志 {} {}", methodCallInfo.getCallId(), mapperArgAsObjCallIdList.size());
            // 对于set方法序号匹配的字段关系，设置MyBatis相关的标志
            addMyBatisInsertFlags4FieldRelationshipBySet(mapperArgAsObjCallIdList);
        }
    }

    /**
     * 处理select相关的MyBatis Mapper方法
     *
     * @param mybatisMSEntity
     * @param selectMapperMethodName
     */
    private void handleSelectMapperMethod(WriteDbData4MybatisMSEntity mybatisMSEntity, String selectMapperMethodName) {
        logger.info("处理select相关的MyBatis Mapper方法 {} {}", mybatisMSEntity.getMapperClassName(), selectMapperMethodName);
        // 查询Mapper方法信息
        List<WriteDbData4MethodInfo> mapperMethodInfoList = methodInfoHandler.queryMethodByClassMethodUpper(mybatisMSEntity.getMapperClassName(), selectMapperMethodName);
        if (JavaCG2Util.isCollectionEmpty(mapperMethodInfoList)) {
            return;
        }
        String entityClassName = mybatisMSEntity.getEntityClassName();
        for (WriteDbData4MethodInfo mapperMethodInfo : mapperMethodInfoList) {
            boolean returnTypeIsEntity = StringUtils.equals(entityClassName, mapperMethodInfo.getReturnType());
            // 查询调用以上Mapper方法的方法
            List<WriteDbData4MethodCall> selectMethodCallList = methodCallHandler.queryMethodCallByCalleeFullMethod(mapperMethodInfo.getFullMethod());
            if (JavaCG2Util.isCollectionEmpty(selectMethodCallList)) {
                // 当前Mapper方法未找到调用方法
                continue;
            }
            for (WriteDbData4MethodCall selectMethodCall : selectMethodCallList) {
                if (returnTypeIsEntity || StringUtils.equals(entityClassName, selectMethodCall.getActualReturnType())) {
                    // 若方法返回类型为Entity，或当前处理的方法调用实际返回类型为Entity时的处理
                    logger.info("当前方法是MyBatis用于查询Entity数据的方法 {} {} {} {}", selectMethodCall.getCallerFullMethod(), selectMethodCall.getCallerLineNumber(),
                            selectMethodCall.getCalleeFullMethod(), selectMethodCall.getActualReturnType());
                    // 处理MyBatis select Entity操作相关的方法调用
                    handleSelectEntityMethodCall(selectMethodCall);
                } else if (JavaCG2ClassMethodUtil.isCustomType(mapperMethodInfo.getReturnType())) {
                    // 返回类型非Entity，属于自定义对象时的处理
                    logger.info("当前方法是MyBatis用于查询自定义对象的方法 {} {} {} {}", selectMethodCall.getCallerFullMethod(), selectMethodCall.getCallerLineNumber(),
                            selectMethodCall.getCalleeFullMethod(), mapperMethodInfo.getReturnType());
                    handleSelectObjectMethodCall(mapperMethodInfo, selectMethodCall);
                }

                List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Where = new ArrayList<>();
                // 查询指定MyBatis Mapper方法的参数所对应的数据库信息，包括对应的set与where的字段与数据库信息
                List<AbstractMyBatisMapperArg> myBatisMapperArgList = myBatisMSMapperEntityHandler.queryParamDbInfo4MyBatisMapperMethod(mybatisMSEntity.getMapperClassName(),
                        selectMapperMethodName, mapperMethodInfo.getFullMethod(), myBatisMapperArgAndParamDbInfoList4Where, null);
                if (JavaCG2Util.isCollectionEmpty(myBatisMapperArgList)) {
                    // select方法无参数，不处理
                    continue;
                }
                // 处理select方法参数
                handleSelectMethodArg(selectMethodCall, myBatisMapperArgList, myBatisMapperArgAndParamDbInfoList4Where);
            }
        }
    }

    // 处理MyBatis select Entity操作相关的方法调用
    private void handleSelectEntityMethodCall(WriteDbData4MethodCall selectMethodCall) {
        // 查询以Mapper接口的select方法返回值，作为方法调用被调用对象时的方法调用序号
        List<Integer> mapperSelectReturnAsObjCallIdList = methodCallInfoHandler.queryCallIdInCaller4ObjByMethodCallArg(selectMethodCall.getCallerMethodHash(), true,
                selectMethodCall.getCallId(), false, selectMethodCall.getCallId());
        if (JavaCG2Util.isCollectionEmpty(mapperSelectReturnAsObjCallIdList)) {
            return;
        }
        // 为MyBatis select操作增加信息
        for (Integer selectReturnCallId : mapperSelectReturnAsObjCallIdList) {
            // 通过Mapper接口的select方法参数对应的get方法调用序号查找匹配的字段关联关系
            List<WriteDbData4FieldRelationship> fieldRelationshipList = fieldRelationshipHandler.queryRelationshipByGetMethodCallId(selectReturnCallId);
            if (JavaCG2Util.isCollectionEmpty(fieldRelationshipList)) {
                continue;
            }

            for (WriteDbData4FieldRelationship fieldRelationship : fieldRelationshipList) {
                logger.info("修改字段关联关系表的标志 {} {} {} {} {}", FieldRelationshipFlagsEnum.FRF_GET_MYBATIS_SELECT_ENTITY, selectReturnCallId, fieldRelationship.getFldRelationshipId(),
                        selectMethodCall.getCallerFullMethod(), selectMethodCall.getCallerLineNumber());
                fieldRelationshipHandler.updateFieldRelationshipAddFlag(fieldRelationship.getFldRelationshipId(), fieldRelationship.getRelationshipFlags(),
                        FieldRelationshipFlagsEnum.FRF_GET_MYBATIS_SELECT_ENTITY);
            }
        }
    }

    // 处理MyBatis select对象操作相关的方法调用
    private void handleSelectObjectMethodCall(WriteDbData4MethodInfo mapperMethodInfo, WriteDbData4MethodCall selectMethodCall) {
        String mapperClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(mapperMethodInfo.getFullMethod());
        String mapperMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(mapperMethodInfo.getFullMethod());
        List<WriteDbData4MyBatisMSSelectColumn> selectColumnList = myBatisMSMapperEntityHandler.queryMybatisMSSelectDbInfo(mapperClassName, mapperMethodName);
        if (JavaCG2Util.isCollectionEmpty(selectColumnList)) {
            logger.info("未查询到Mapper方法select返回的字段信息 {} {}", mapperClassName, mapperMethodName);
            return;
        }
        // 查询以Mapper接口的select方法返回值，作为方法调用被调用对象时的方法调用序号
        List<Integer> mapperSelectReturnAsObjCallIdList = methodCallInfoHandler.queryCallIdInCaller4ObjByMethodCallArg(selectMethodCall.getCallerMethodHash(), true,
                selectMethodCall.getCallId(), false, selectMethodCall.getCallId());
        if (JavaCG2Util.isCollectionEmpty(mapperSelectReturnAsObjCallIdList)) {
            return;
        }
        for (Integer selectReturnCallId : mapperSelectReturnAsObjCallIdList) {
            // 查询以Mapper接口的select方法返回值，作为方法调用被调用对象时的方法调用
            WriteDbData4MethodCall selectReturnAsObjMethodCall = methodCallHandler.queryMethodCallByCallId(selectReturnCallId);
            if (!JACGClassMethodUtil.calleeMatchesGetMethod(selectReturnAsObjMethodCall)) {
                // 被调用方法不满足get方法，不处理
                continue;
            }
            String selectReturnAsObjCalleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(selectReturnAsObjMethodCall.getCalleeFullMethod());
            // 查询Mapper接口的select方法返回值对应的get方法
            BaseWriteDbData4GetSetMethod getMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(true, selectReturnAsObjCalleeClassName,
                    selectReturnAsObjMethodCall.getCalleeMethodName());
            if (getMethod == null) {
                continue;
            }

            // 从MyBatis的XML中select的字段信息表记录中根据字段名查找匹配的记录
            WriteDbData4MyBatisMSSelectColumn selectColumn = findMatchesSelectColumn(selectColumnList, getMethod.getFieldName());
            if (selectColumn == null) {
                continue;
            }

            // 通过Mapper接口的select方法参数对应的get方法调用序号查找匹配的字段关联关系
            List<WriteDbData4FieldRelationship> fieldRelationshipList = fieldRelationshipHandler.queryRelationshipByGetMethodCallId(selectReturnCallId);
            if (JavaCG2Util.isCollectionEmpty(fieldRelationshipList)) {
                continue;
            }

            for (WriteDbData4FieldRelationship fieldRelationship : fieldRelationshipList) {
                logger.info("修改字段关联关系表的标志 {} {} {} {} {}", FieldRelationshipFlagsEnum.FRF_GET_MYBATIS_SELECT_OBJECT, selectReturnCallId, fieldRelationship.getFldRelationshipId(),
                        selectMethodCall.getCallerFullMethod(), selectMethodCall.getCallerLineNumber());
                fieldRelationshipHandler.updateFieldRelationshipAddFlag(fieldRelationship.getFldRelationshipId(), fieldRelationship.getRelationshipFlags(),
                        FieldRelationshipFlagsEnum.FRF_GET_MYBATIS_SELECT_OBJECT);

                // 插入使用MyBatis时get/set方法所关联的数据库信息表
                insertMybatisMsGetSetDb(fieldRelationship.getFldRelationshipId(), true, fieldRelationship.getGetMethodCallId(), null, JACGSqlStatementConstants.SELECT,
                        selectColumn.getTableName(), selectColumn.getColumnName(), MyBatisColumnRelateDescEnum.MBCRD_OBJECT);
            }
        }
    }

    // 从MyBatis的XML中select的字段信息表记录中根据字段名查找匹配的记录
    private WriteDbData4MyBatisMSSelectColumn findMatchesSelectColumn(List<WriteDbData4MyBatisMSSelectColumn> selectColumnList, String fieldName) {
        for (WriteDbData4MyBatisMSSelectColumn selectColumn : selectColumnList) {
            // 若select的字段的别名或名称匹配则返回
            if (StringUtils.equals(fieldName, selectColumn.getColumnAlias()) || StringUtils.equals(fieldName, selectColumn.getColumnName())) {
                return selectColumn;
            }
        }
        return null;
    }

    // 处理select方法参数
    private void handleSelectMethodArg(WriteDbData4MethodCall selectMethodCall, List<AbstractMyBatisMapperArg> myBatisMapperArgList,
                                       List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Where) {
        for (int argSeq = 0; argSeq < myBatisMapperArgList.size(); argSeq++) {
            // 查找Mapper接口的方法被调用时的参数信息，参数序号从1开始，需要加1
            List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryMethodCallInfoObjArg(selectMethodCall.getCallId(), argSeq + 1);
            if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
                continue;
            }
            // 当前处理的Mapper方法参数
            AbstractMyBatisMapperArg myBatisMapperArg = myBatisMapperArgList.get(argSeq);
            // 当前参数对应的Where字段信息
            MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Where = JACGUtil.getListElement(myBatisMapperArgAndParamDbInfoList4Where, argSeq);
            // 判断当前参数是否为对象
            boolean mapperArgIsObject = JavaCG2ClassMethodUtil.isCustomType(myBatisMapperArg.getArgType());
            for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                if (mapperArgIsObject) {
                    // 处理MyBatis的Mapper方法参数，使用对象的情况
                    handleMapperArgUseObject(false, false, methodCallInfo, selectMethodCall, myBatisMapperArgAndParamDbInfo4Where, null);
                } else {
                    // 处理MyBatis的Mapper方法参数，使用基本类型的情况
                    handleMapperArgUseBaseType(false, methodCallInfo, myBatisMapperArgAndParamDbInfo4Where, null);
                }
            }
        }
    }

    /**
     * 处理update相关的MyBatis Mapper方法
     *
     * @param mybatisMSEntity
     * @param updateMapperMethodName
     */
    private void handleUpdateMapperMethod(WriteDbData4MybatisMSEntity mybatisMSEntity, String updateMapperMethodName) {
        logger.info("处理update相关的MyBatis Mapper方法 {} {}", mybatisMSEntity.getMapperClassName(), updateMapperMethodName);
        // 查询Mapper方法信息
        List<WriteDbData4MethodInfo> mapperMethodInfoList = methodInfoHandler.queryMethodByClassMethodUpper(mybatisMSEntity.getMapperClassName(), updateMapperMethodName);
        if (JavaCG2Util.isCollectionEmpty(mapperMethodInfoList)) {
            return;
        }

        for (WriteDbData4MethodInfo mapperMethodInfo : mapperMethodInfoList) {
            List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Where = new ArrayList<>();
            List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Set = new ArrayList<>();
            // 查询指定MyBatis Mapper方法的参数所对应的数据库信息，包括对应的set与where的字段与数据库信息
            List<AbstractMyBatisMapperArg> myBatisMapperArgList = myBatisMSMapperEntityHandler.queryParamDbInfo4MyBatisMapperMethod(mybatisMSEntity.getMapperClassName(),
                    updateMapperMethodName, mapperMethodInfo.getFullMethod(), myBatisMapperArgAndParamDbInfoList4Where, myBatisMapperArgAndParamDbInfoList4Set);
            if (JavaCG2Util.isCollectionEmpty(myBatisMapperArgList)) {
                // update方法无参数，不处理
                return;
            }

            // 查询调用以上Mapper方法的方法
            List<WriteDbData4MethodCall> updateMethodCallList = methodCallHandler.queryMethodCallByCalleeFullMethod(mapperMethodInfo.getFullMethod());
            if (JavaCG2Util.isCollectionEmpty(updateMethodCallList)) {
                // 当前Mapper方法未找到调用方法
                return;
            }

            // 处理MyBatis update Mapper某个方法调用
            for (WriteDbData4MethodCall updateMethodCall : updateMethodCallList) {
                handleUpdateMethodCall(mybatisMSEntity.getEntityClassName(), updateMethodCall, myBatisMapperArgList, myBatisMapperArgAndParamDbInfoList4Where,
                        myBatisMapperArgAndParamDbInfoList4Set);
            }
        }
    }

    /**
     * 处理MyBatis某个update的Mapper方法调用
     *
     * @param entityClassName
     * @param updateMethodCall
     * @param myBatisMapperArgList
     * @param myBatisMapperArgAndParamDbInfoList4Where
     * @param myBatisMapperArgAndParamDbInfoList4Set
     */
    private void handleUpdateMethodCall(String entityClassName, WriteDbData4MethodCall updateMethodCall, List<AbstractMyBatisMapperArg> myBatisMapperArgList,
                                        List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Where,
                                        List<MyBatisMapperArgAndParamDbInfo> myBatisMapperArgAndParamDbInfoList4Set) {
        for (int argSeq = 0; argSeq < myBatisMapperArgList.size(); argSeq++) {
            // 查找Mapper接口的方法被调用时的参数信息，参数序号从1开始，需要加1
            List<WriteDbData4MethodCallInfo> methodCallInfoList = methodCallInfoHandler.queryMethodCallInfoObjArg(updateMethodCall.getCallId(), argSeq + 1);
            if (JavaCG2Util.isCollectionEmpty(methodCallInfoList)) {
                continue;
            }
            // 当前处理的Mapper方法参数
            AbstractMyBatisMapperArg myBatisMapperArg = myBatisMapperArgList.get(argSeq);
            // 当前参数对应的Where与Set字段信息
            MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Where = JACGUtil.getListElement(myBatisMapperArgAndParamDbInfoList4Where, argSeq);
            MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Set = JACGUtil.getListElement(myBatisMapperArgAndParamDbInfoList4Set, argSeq);
            // 判断当前参数类型是否为Entity
            boolean mapperArgIsEntity = entityClassName.equals(myBatisMapperArg.getArgType());
            // 判断当前参数是否为对象
            boolean mapperArgIsObject = JavaCG2ClassMethodUtil.isCustomType(myBatisMapperArg.getArgType());
            if (!mapperArgIsEntity || !mapperArgIsObject) {
                // 有可能参数类型定义为Object，实际传入的参数类型是对象，判断实际传入的参数类型是否为对象
                for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                    if (JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE.getType().equals(methodCallInfo.getType())) {
                        if (entityClassName.equals(methodCallInfo.getTheValue())) {
                            mapperArgIsEntity = true;
                        }
                        if (JavaCG2ClassMethodUtil.isCustomType(methodCallInfo.getTheValue())) {
                            mapperArgIsObject = true;
                        }
                    }
                }
            }

            if (mapperArgIsObject) {
                // 处理MyBatis的Mapper方法参数，使用对象的情况
                for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                    handleMapperArgUseObject(true, mapperArgIsEntity, methodCallInfo, updateMethodCall, myBatisMapperArgAndParamDbInfo4Where, myBatisMapperArgAndParamDbInfo4Set);
                }
            } else {
                // 处理MyBatis的Mapper方法参数，使用基本类型的情况
                for (WriteDbData4MethodCallInfo methodCallInfo : methodCallInfoList) {
                    handleMapperArgUseBaseType(true, methodCallInfo, myBatisMapperArgAndParamDbInfo4Where, myBatisMapperArgAndParamDbInfo4Set);
                }
            }
        }
    }

    // 处理MyBatis的Mapper方法参数，使用对象的情况
    private void handleMapperArgUseObject(boolean updateOrSelect, boolean mapperArgIsEntity, WriteDbData4MethodCallInfo methodCallInfo,
                                          WriteDbData4MethodCall updateOrSelectMethodCall,
                                          MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Where, MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Set) {
        // 查询以Mapper接口的方法参数作为被调用对象的方法调用序号
        List<Integer> mapperArgAsObjCallIdList = queryMapperArgAsObjCallIdList(updateOrSelectMethodCall, methodCallInfo);
        if (JavaCG2Util.isCollectionEmpty(mapperArgAsObjCallIdList)) {
            return;
        }

        for (Integer mapperArgAsObjCallId : mapperArgAsObjCallIdList) {
            // 查询Mapper接口方法参数对象对应的方法调用
            WriteDbData4MethodCall mapperArgMethodCall = methodCallHandler.queryMethodCallByCallId(mapperArgAsObjCallId);
            if (!JACGClassMethodUtil.calleeMatchesSetMethod(mapperArgMethodCall)) {
                // 当前方法调用不符合set方法，跳过
                continue;
            }
            String mapperArgCalleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(mapperArgMethodCall.getCalleeFullMethod());
            // 查询当前set方法对应的字段
            BaseWriteDbData4GetSetMethod setMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(false, mapperArgCalleeClassName,
                    mapperArgMethodCall.getCalleeMethodName());
            if (setMethod == null) {
                // 未查询到set方法，跳过
                continue;
            }
            // 根据set方法设置的字段名称，判断对应的where或set子句对应的数据库字段
            List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList4Where = findParamDbInfoBySetFieldName(myBatisMapperArgAndParamDbInfo4Where, setMethod.getFieldName());
            // 根据set方法设置的字段名称，判断对应的set子句对应的数据库字段
            List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList4Set = findParamDbInfoBySetFieldName(myBatisMapperArgAndParamDbInfo4Set, setMethod.getFieldName());
            if (JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList4Where) && JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList4Set)) {
                // 当前的set方法设置的字段未查找到where、set子句对应的数据库字段
                continue;
            }
            // 通过Mapper接口的方法参数对应的set方法调用序号查找匹配的字段关联关系
            List<WriteDbData4FieldRelationship> fieldRelationshipList = fieldRelationshipHandler.queryDirectlyRelationshipBySetMethodCallId(mapperArgMethodCall.getCallId());
            if (JavaCG2Util.isCollectionEmpty(fieldRelationshipList)) {
                continue;
            }

            FieldRelationshipFlagsEnum fieldRelationshipFlagsEnum = FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_MAPPER_ARG_OPERATE;
            if (updateOrSelect && mapperArgIsEntity) {
                // update方法，且当前参数是Entity
                if (!JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList4Where)) {
                    fieldRelationshipFlagsEnum = FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_UPDATE_WHERE_ENTITY;
                } else {
                    fieldRelationshipFlagsEnum = FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_UPDATE_SET_ENTITY;
                }
            }

            for (WriteDbData4FieldRelationship fieldRelationship : fieldRelationshipList) {
                logger.info("修改字段关联关系表的标志 {} {} {} {} {}", fieldRelationshipFlagsEnum, mapperArgMethodCall.getCallId(), fieldRelationship.getFldRelationshipId(),
                        fieldRelationship.getCallerFullMethod(), fieldRelationship.getCallerLineNumber());
                fieldRelationshipHandler.updateFieldRelationshipAddFlag(fieldRelationship.getFldRelationshipId(), fieldRelationship.getRelationshipFlags(),
                        fieldRelationshipFlagsEnum);
                if (!mapperArgIsEntity) {
                    // 方法参数类型不是Entity
                    String whereOperate = updateOrSelect ? JACGSqlStatementConstants.UPDATE_WHERE : JACGSqlStatementConstants.SELECT_WHERE;
                    String setOperate = updateOrSelect ? JACGSqlStatementConstants.UPDATE_SET : null;
                    // 方法参数类型非Entity时，插入MyBatis的Mapper方法参数所对应的数据库信息
                    insertMybatisMsGetSetDbList(myBatisMSMapperParamDbInfoList4Where, fieldRelationship.getFldRelationshipId(), fieldRelationship.getSetMethodCallId(),
                            whereOperate, MyBatisColumnRelateDescEnum.MBCRD_OBJECT);
                    insertMybatisMsGetSetDbList(myBatisMSMapperParamDbInfoList4Set, fieldRelationship.getFldRelationshipId(), fieldRelationship.getSetMethodCallId(), setOperate,
                            MyBatisColumnRelateDescEnum.MBCRD_OBJECT);
                }
            }
        }
    }

    // 处理MyBatis的Mapper方法参数，使用基本类型的情况
    private void handleMapperArgUseBaseType(boolean updateOrSelect, WriteDbData4MethodCallInfo methodCallInfo, MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Where,
                                            MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo4Set) {
        if (!JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID.getType().equals(methodCallInfo.getType())) {
            // 以Mapper接口的方法参数不是方法调用返回值
            return;
        }
        int callId = Integer.parseInt(methodCallInfo.getTheValue());
        // 查询Mapper方法参数使用方法调用返回值，查询对应的方法调用
        WriteDbData4MethodCall methodCallReturnAsMapperArg = methodCallHandler.queryMethodCallByCallId(callId);
        if (!JACGClassMethodUtil.calleeMatchesGetMethod(methodCallReturnAsMapperArg)) {
            return;
        }
        String returnAsMapperArgClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCallReturnAsMapperArg.getCalleeFullMethod());
        // 被调用方法满足get方法形式，查询对应的dto的get方法
        BaseWriteDbData4GetSetMethod getMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(true, returnAsMapperArgClassName,
                methodCallReturnAsMapperArg.getCalleeMethodName());
        if (getMethod == null) {
            return;
        }
        // 判断当前的参数对应where、set子句的数据库字段
        List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList4Where = myBatisMapperArgAndParamDbInfo4Where == null ? null :
                myBatisMapperArgAndParamDbInfo4Where.getMyBatisMSMapperParamDbInfoList();
        List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList4Set = myBatisMapperArgAndParamDbInfo4Set == null ? null :
                myBatisMapperArgAndParamDbInfo4Set.getMyBatisMSMapperParamDbInfoList();
        if (JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList4Where) && JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList4Set)) {
            // 当前的参数未查找到对应where、set子句的数据库字段
            return;
        }

        // 查询到对应的dto的get方法，人工增加一条通过get/set方法关联的字段关系，set方法信息设为空
        Integer insertFRRecordId = manualAddFieldRelationship4Get(methodCallReturnAsMapperArg, returnAsMapperArgClassName, getMethod.getMethodName());
        if (insertFRRecordId == null) {
            return;
        }
        String whereOperate = updateOrSelect ? JACGSqlStatementConstants.UPDATE_WHERE : JACGSqlStatementConstants.SELECT_WHERE;
        String setOperate = updateOrSelect ? JACGSqlStatementConstants.UPDATE_SET : null;
        // 插入MyBatis的Mapper方法参数所对应的数据库信息
        insertMybatisMsGetSetDbList(myBatisMSMapperParamDbInfoList4Where, insertFRRecordId, callId, whereOperate, MyBatisColumnRelateDescEnum.MBCRD_BASE_TYPE);
        insertMybatisMsGetSetDbList(myBatisMSMapperParamDbInfoList4Set, insertFRRecordId, callId, setOperate, MyBatisColumnRelateDescEnum.MBCRD_BASE_TYPE);
    }

    // 插入MyBatis的Mapper方法参数所对应的数据库信息
    private void insertMybatisMsGetSetDbList(List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList, int fldRelationshipId, int getMethodCallId,
                                             String sqlStatement, MyBatisColumnRelateDescEnum myBatisColumnRelateDescEnum) {
        if (JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList)) {
            return;
        }
        for (MyBatisMSMapperParamDbInfo myBatisMSMapperParamDbInfo : myBatisMSMapperParamDbInfoList) {
            // 插入使用MyBatis时get/set方法所关联的数据库信息表
            insertMybatisMsGetSetDb(fldRelationshipId, true, getMethodCallId, null, sqlStatement, myBatisMSMapperParamDbInfo.getTableName(),
                    myBatisMSMapperParamDbInfo.getColumnName(), myBatisColumnRelateDescEnum);
        }
    }

    // 插入使用MyBatis时get/set方法所关联的数据库信息表
    private void insertMybatisMsGetSetDb(int fldRelationshipId, boolean getOrSet, Integer getMethodCallId, Integer setMethodCallId, String sqlStatement, String tableName,
                                         String columnName, MyBatisColumnRelateDescEnum myBatisColumnRelateDescEnum) {
        WriteDbData4MybatisMSGetSetDb writeDbData4MybatisMsGetSetDb = new WriteDbData4MybatisMSGetSetDb();
        writeDbData4MybatisMsGetSetDb.setRecordId(recordId.addAndGet());
        writeDbData4MybatisMsGetSetDb.setFldRelationshipId(fldRelationshipId);
        writeDbData4MybatisMsGetSetDb.setGetOrSet(getOrSet ? JavaCG2Constants.METHOD_PREFIX_GET : JavaCG2Constants.METHOD_PREFIX_SET);
        writeDbData4MybatisMsGetSetDb.setGetMethodCallId(getOrSet ? getMethodCallId : JavaCG2Constants.RECORD_ID_MIN_BEFORE);
        writeDbData4MybatisMsGetSetDb.setSetMethodCallId(getOrSet ? JavaCG2Constants.RECORD_ID_MIN_BEFORE : setMethodCallId);
        writeDbData4MybatisMsGetSetDb.setDbOperate(sqlStatement);
        writeDbData4MybatisMsGetSetDb.setTableName(tableName);
        writeDbData4MybatisMsGetSetDb.setColumnName(columnName);
        writeDbData4MybatisMsGetSetDb.setColumnRelateDesc(myBatisColumnRelateDescEnum.getType());
        writeDbHandler4MybatisMSGetSetDb.addData(writeDbData4MybatisMsGetSetDb);
        writeDbHandler4MybatisMSGetSetDb.tryInsertDb();
    }

    /**
     * 根据set方法设置的字段名称，判断对应的where或set子句对应的数据库字段
     *
     * @param myBatisMapperArgAndParamDbInfo
     * @param setFieldName
     */
    private List<MyBatisMSMapperParamDbInfo> findParamDbInfoBySetFieldName(MyBatisMapperArgAndParamDbInfo myBatisMapperArgAndParamDbInfo, String setFieldName) {
        if (myBatisMapperArgAndParamDbInfo == null) {
            return null;
        }
        List<MyBatisMSMapperParamDbInfo> myBatisMSMapperParamDbInfoList = myBatisMapperArgAndParamDbInfo.getMyBatisMSMapperParamDbInfoList();
        if (JavaCG2Util.isCollectionEmpty(myBatisMSMapperParamDbInfoList)) {
            return null;
        }
        List<MyBatisMSMapperParamDbInfo> returnMyBatisMSMapperParamDbInfoList = new ArrayList<>();
        for (MyBatisMSMapperParamDbInfo myBatisMSMapperParamDbInfo : myBatisMSMapperParamDbInfoList) {
            if (StringUtils.equals(myBatisMSMapperParamDbInfo.getParamName(), setFieldName)) {
                returnMyBatisMSMapperParamDbInfoList.add(myBatisMSMapperParamDbInfo);
            }
        }
        return returnMyBatisMSMapperParamDbInfoList;
    }

    //  人工增加一条通过get / set方法关联的字段关系，set方法信息设为空
    private Integer manualAddFieldRelationship4Get(WriteDbData4MethodCall methodCallReturnAsMapperArg, String getClassName, String getMethodName) {
        return manualAddFieldRelationshipHandler.manualAddFieldRelationship(methodCallReturnAsMapperArg.getCallerFullMethod(), methodCallReturnAsMapperArg.getCallerLineNumber(),
                methodCallReturnAsMapperArg.getCallId(), JavaCG2Constants.RECORD_ID_MIN_BEFORE, getClassName, getMethodName, JACGConstants.EMPTY_CLASS_METHOD,
                JACGConstants.EMPTY_CLASS_METHOD, JavaCG2FieldRelationshipTypeEnum.FRTE_MYBATIS_MAPPER_ARG_DB,
                FieldRelationshipFlagsEnum.FRF_SET_MYBATIS_MAPPER_ARG_OPERATE.getFlag());
    }

    public void setWriteDbHandler4MybatisMSGetSetDb(WriteDbHandler4MybatisMSGetSetDb writeDbHandler4MybatisMSGetSetDb) {
        this.writeDbHandler4MybatisMSGetSetDb = writeDbHandler4MybatisMSGetSetDb;
    }
}
