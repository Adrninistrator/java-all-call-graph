package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.dto.field.JACGFieldMethodInfo;
import com.adrninistrator.jacg.handler.methodcall.BaseMethodCallByEEDetailHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2FieldRelationshipTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description: 人工增加get/set方法字段关联关系处理类
 * <p>
 * 假如使用 handleMethodCallByEEEntry 方法人工增加记录，则按照以下顺序调用
 * beforeAdd
 * manualAddFieldRelationship
 * afterAdd
 * <p>
 * 假如使用 manualAddFieldRelationship 方法人工增加记录，则需要按照以下顺序调用
 * beforeAdd
 * manualAddFieldRelationship
 * afterAdd
 */
public class ManualAddFieldRelationshipHandler extends BaseMethodCallByEEDetailHandler {
    private static final Logger logger = LoggerFactory.getLogger(ManualAddFieldRelationshipHandler.class);

    // 不定义为static，避免在Web应用环境中执行时不同实例之间有影响
    private final AtomicBoolean RUNNING_FLAG = new AtomicBoolean(false);

    private final FieldRelationshipHandler fieldRelationshipHandler;

    private final List<WriteDbData4FieldRelationship> fieldRelationshipList;

    private int currentMaxRecordId;

    public ManualAddFieldRelationshipHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        fieldRelationshipList = new ArrayList<>(dbOperWrapper.getDbInsertBatchSize());
    }

    public ManualAddFieldRelationshipHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        fieldRelationshipList = new ArrayList<>(dbOperWrapper.getDbInsertBatchSize());
    }

    /**
     * 执行增加操作前的处理
     * 当前方法不允许并发调用
     */
    public boolean beforeAdd() {
        if (!RUNNING_FLAG.compareAndSet(false, true)) {
            // 以下需要查询方法调用表最大序号，加1并使用，因此不能并发调用
            logger.error("当前方法不允许并发调用");
            return false;
        }

        // 查询数据库通过get/set方法关联的字段关系表最大记录id
        currentMaxRecordId = queryMaxRecordId();
        if (currentMaxRecordId == JACGConstants.RECORD_ID_ILLEGAL) {
            return false;
        }
        return true;
    }

    /**
     * 执行增加操作后的处理
     */
    public void afterAdd() {
        if (!RUNNING_FLAG.get()) {
            logger.error("需要先调用beforeAdd方法后再执行当前方法");
            return;
        }
        try {
            // 写入数据库
            insertDb();
        } finally {
            RUNNING_FLAG.set(false);
        }
    }

    /**
     * 根据被调用方法进行处理，人工增加get/set方法字段关联关系入口方法
     *
     * @param calleeClassName 指定的被调用类名，需要是完整类名
     * @param methodNameList  指定的被调用方法名列表
     * @param setClassArgSeq  set方法对应的类在Bean拷贝方法中的参数序号，从1开始
     * @param getClassArgSeq  get方法对应的类在Bean拷贝方法中的参数序号，从1开始
     * @return
     */
    public boolean handleMethodCallByEEEntry(String calleeClassName, List<String> methodNameList, int setClassArgSeq, int getClassArgSeq) {
        if (!RUNNING_FLAG.get()) {
            logger.error("需要先调用beforeAdd方法后再执行当前方法");
            return false;
        }
        if (setClassArgSeq == getClassArgSeq || setClassArgSeq < 1 || getClassArgSeq < 1) {
            logger.error("get/set方法对应的类在Bean拷贝方法中的参数序号非法 {} {}", setClassArgSeq, getClassArgSeq);
            return false;
        }

        return handleMethodCallByEECM(calleeClassName, methodNameList, setClassArgSeq, getClassArgSeq);
    }

    /**
     * 选择指定类及超类中的全部字段信息
     *
     * @param className      类名
     * @param queryGetMethod true: 查询get方法对应的字段 false: 查询set方法对应的字段
     * @return
     */
    protected Map<String, JACGFieldMethodInfo> chooseAllFieldMethodInfo(String className, boolean queryGetMethod) {
        return fieldRelationshipHandler.queryAllFieldMethodInfo(className, queryGetMethod, false);
    }

    @Override
    protected void handleMethodWithArgs(WriteDbData4MethodCall methodCall, MethodDetailNoReturnType callerMethodDetailNoReturnType,
                                        MethodDetailNoReturnType calleeMethodDetailNoReturnType, ObjArgsInfoInMethodCall objArgsInfoInMethodCall, Object... args) {
        Integer setClassArgSeq = JACGUtil.getArgAt(0, args);
        Integer getClassArgSeq = JACGUtil.getArgAt(1, args);
        if (objArgsInfoInMethodCall == null) {
            logger.warn("未获取到方法调用信息 {}", methodCall.genPrintInfo());
            return;
        }

        // 获取set方法对应类在方法调用参数信息中的类型
        MethodCallInfo setMethodCallInfo = objArgsInfoInMethodCall.getSingleArgMethodCallInfo(setClassArgSeq);
        if (setMethodCallInfo == null) {
            logger.warn("未获取到set方法对应类在方法调用信息 {} {}", methodCall.genPrintInfo(), setClassArgSeq);
            return;
        }

        // 获取get方法对应类在方法调用参数信息中的类型
        MethodCallInfo getMethodCallInfo = objArgsInfoInMethodCall.getSingleArgMethodCallInfo(getClassArgSeq);
        if (getMethodCallInfo == null) {
            logger.warn("未获取到get方法对应类在方法调用信息 {} {}", methodCall.genPrintInfo(), getClassArgSeq);
            return;
        }

        String getClassName = getMethodCallInfo.getType();
        String setClassName = setMethodCallInfo.getType();
        if (getClassName == null || setClassName == null) {
            logger.warn("未获取到set/get方法对应类在方法调用参数信息中的类型 {}", methodCall.genPrintInfo());
            return;
        }

        // 为指定的get/set方法对应的类增加关联关系
        addFieldRelationship4BeanUtil(methodCall, getClassName, setClassName);
    }

    /**
     * 为指定的get/set方法对应的类增加关联关系，使用BeanUtil的情况下
     *
     * @param methodCall   对应的方法调用
     * @param getClassName get方法对应的类名
     * @param setClassName set方法对应的类名
     */
    protected void addFieldRelationship4BeanUtil(WriteDbData4MethodCall methodCall, String getClassName, String setClassName) {
        // 选择set/get方法对应类及超类中的全部字段信息
        Map<String, JACGFieldMethodInfo> getFieldBehaviorInfoMap = chooseAllFieldMethodInfo(getClassName, true);
        Map<String, JACGFieldMethodInfo> setFieldBehaviorInfoMap = chooseAllFieldMethodInfo(setClassName, false);

        if (JavaCG2Util.isMapEmpty(getFieldBehaviorInfoMap) || JavaCG2Util.isMapEmpty(setFieldBehaviorInfoMap)) {
            logger.error("从指定类中未获取到字段信息 {} {}", getClassName, setClassName);
            return;
        }

        // 遍历set方法类对应的字段，尝试在get方法类对应的字段中找到同名的
        for (Map.Entry<String, JACGFieldMethodInfo> setEntry : setFieldBehaviorInfoMap.entrySet()) {
            String setFieldName = setEntry.getKey();
            JACGFieldMethodInfo getFieldMethodInfo = getFieldBehaviorInfoMap.get(setFieldName);
            if (getFieldMethodInfo == null) {
                continue;
            }
            // 在set/get方法对应类中找到同名字段
            JACGFieldMethodInfo setFieldMethodInfo = setEntry.getValue();
            // 向通过get/set方法关联的字段关系表插入数据，用于BeanUtil方法
            addFieldRelationship4BeanUtil(methodCall.getCallerFullMethod(), methodCall.getCallerLineNumber(), getClassName, getFieldMethodInfo.getMethodName(), setClassName,
                    setFieldMethodInfo.getMethodName(), JavaCG2FieldRelationshipTypeEnum.FRTE_BEAN_UTIL, 0, methodCall.getCallId(), methodCall.getCalleeFullMethod());
        }
    }

    /**
     * 人工向通过get/set方法关联的字段关系表插入数据
     *
     * @param callerFullMethod
     * @param callerLineNumber
     * @param getMethodCallId
     * @param setMethodCallId
     * @param getClassName
     * @param getMethodName
     * @param setClassName
     * @param setMethodName
     * @param fieldRelationshipTypeEnum
     * @param relationshipFlags
     * @return
     */
    public Integer manualAddFieldRelationship(String callerFullMethod, int callerLineNumber, int getMethodCallId, int setMethodCallId, String getClassName, String getMethodName
            , String setClassName, String setMethodName, JavaCG2FieldRelationshipTypeEnum fieldRelationshipTypeEnum, int relationshipFlags) {
        return doAddFieldRelationship(callerFullMethod, callerLineNumber, getMethodCallId, setMethodCallId, getClassName, getMethodName, setClassName, setMethodName,
                fieldRelationshipTypeEnum, relationshipFlags, JavaCG2Constants.RECORD_ID_MIN_BEFORE, null);
    }

    /**
     * 向通过get/set方法关联的字段关系表插入数据，用于BeanUtil方法
     *
     * @param callerFullMethod
     * @param callerLineNumber
     * @param getClassName
     * @param getMethodName
     * @param setClassName
     * @param setMethodName
     * @param fieldRelationshipTypeEnum
     * @param relationshipFlags
     * @param beanUtilCallId
     * @param beanUtilMethod
     * @return
     */
    public Integer addFieldRelationship4BeanUtil(String callerFullMethod, int callerLineNumber, String getClassName, String getMethodName, String setClassName,
                                                 String setMethodName, JavaCG2FieldRelationshipTypeEnum fieldRelationshipTypeEnum, int relationshipFlags, int beanUtilCallId,
                                                 String beanUtilMethod) {
        return doAddFieldRelationship(callerFullMethod, callerLineNumber, JavaCG2Constants.RECORD_ID_MIN_BEFORE, JavaCG2Constants.RECORD_ID_MIN_BEFORE, getClassName, getMethodName
                , setClassName, setMethodName, fieldRelationshipTypeEnum, relationshipFlags, beanUtilCallId, beanUtilMethod);
    }

    /**
     * 向通过get/set方法关联的字段关系表插入数据
     *
     * @param callerFullMethod 调用方，完整方法
     * @param callerLineNumber 调用方，源代码行号
     * @param getMethodCallId  get方法调用序号
     * @param setMethodCallId  set方法调用序号
     * @param getClassName     get方法完整类名
     * @param getMethodName    get方法方法名
     * @param setClassName     set方法完整类名
     * @param setMethodName    set方法方法名
     * @param beanUtilCallId   BeanUtil方法调用序号
     * @param beanUtilMethod   BeanUtil方法
     * @return
     */
    private Integer doAddFieldRelationship(String callerFullMethod, int callerLineNumber, int getMethodCallId, int setMethodCallId,
                                           String getClassName, String getMethodName, String setClassName, String setMethodName,
                                           JavaCG2FieldRelationshipTypeEnum fieldRelationshipTypeEnum, int relationshipFlags, int beanUtilCallId, String beanUtilMethod) {
        if (!RUNNING_FLAG.get()) {
            logger.error("需要先调用beforeAdd方法后再执行当前方法");
            return null;
        }
        currentMaxRecordId++;
        logger.info("人工向通过get/set方法关联的字段关系表加入数据 {} {} {} {} {} {} {} {}", currentMaxRecordId, callerFullMethod, callerLineNumber, getClassName, getMethodName, setClassName,
                setMethodName, beanUtilMethod);

        WriteDbData4FieldRelationship fieldRelationship = new WriteDbData4FieldRelationship();
        fieldRelationship.setFldRelationshipId(currentMaxRecordId);
        fieldRelationship.setGetMethodCallId(getMethodCallId);
        fieldRelationship.setSetMethodCallId(setMethodCallId);
        fieldRelationship.setCallerFullMethod(callerFullMethod);
        fieldRelationship.setCallerLineNumber(callerLineNumber);
        fieldRelationship.setSetSimpleClassName(dbOperWrapper.querySimpleClassName(setClassName));
        fieldRelationship.setSetMethodName(setMethodName);
        fieldRelationship.setSetClassName(setClassName);
        fieldRelationship.setGetSimpleClassName(dbOperWrapper.querySimpleClassName(getClassName));
        fieldRelationship.setGetMethodName(getMethodName);
        fieldRelationship.setGetClassName(getClassName);
        fieldRelationship.setValid(JavaCG2YesNoEnum.YES.getIntValue());
        fieldRelationship.setType(fieldRelationshipTypeEnum.getType());
        fieldRelationship.setRelationshipFlags(relationshipFlags);
        fieldRelationship.setBeanUtilCallId(beanUtilCallId);
        fieldRelationship.setBeanUtilMethod(beanUtilMethod);
        fieldRelationshipList.add(fieldRelationship);

        // 尝试写入数据库
        if (!tryInsertDb()) {
            return null;
        }
        return currentMaxRecordId;
    }

    // 尝试写入数据库
    private boolean tryInsertDb() {
        if (fieldRelationshipList.size() >= dbOperWrapper.getDbInsertBatchSize()) {
            return insertDb();
        }
        return true;
    }

    // 写入数据库
    private boolean insertDb() {
        if (fieldRelationshipList.isEmpty()) {
            return true;
        }

        List<Object[]> objectList = new ArrayList<>(fieldRelationshipList.size());
        for (WriteDbData4FieldRelationship fieldRelationship : fieldRelationshipList) {
            objectList.add(JACGSqlUtil.genFieldRelationshipObjectArray(fieldRelationship));
        }
        fieldRelationshipList.clear();

        String sql = dbOperWrapper.genAndCacheInsertSql(DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP, DbInsertMode.DIME_INSERT);
        boolean success = dbOperator.batchInsert(sql, objectList);
        if (!success) {
            logger.error("插入失败 {}", currentMaxRecordId);
            return false;
        }
        return true;
    }

    /**
     * 查询数据库通过get/set方法关联的字段关系表最大记录id
     *
     * @return
     */
    private int queryMaxRecordId() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.FR_QUERY_MAX_RECORD_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            // 记录id从1开始，因此默认使用0
            // pg不支持ifnull，使用COALESCE
            sql = "select COALESCE(max(" + DC.FR_FLD_RELATIONSHIP_ID + "),?) from " + DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Integer maxId = dbOperator.queryObjectOneColumn(sql, Integer.class, JavaCG2Constants.RECORD_ID_MIN_BEFORE);
        if (maxId == null) {
            logger.error("查询数据库通过get/set方法关联的字段关系表最大记录id结果为null");
            return JACGConstants.RECORD_ID_ILLEGAL;
        }
        return maxId;
    }
}
