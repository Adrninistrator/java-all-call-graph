package com.adrninistrator.jacg.handler.fieldrelationship;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.methodcall.parsed.AbstractMethodCallInfoParsed;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4Constant;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4Field;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4MCReturnCallId;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4MethodArg;
import com.adrninistrator.jacg.dto.methodcall.parsed.MethodCallInfoParsed4StaticField;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnArgSeq;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnCallId;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SetMethodAssignInfo;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.common.enums.FieldRelationshipFlagsEnum;
import com.adrninistrator.jacg.handler.common.enums.SetMethodAssignFlagEnum;
import com.adrninistrator.jacg.handler.dto.field.MethodCallPassedFRNode;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethodAssignInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2FieldRelationshipTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.stack.ListAsStack;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/11/25
 * @description: 处理通过方法调用传递的字段关联关系
 */
public class MethodCallPassedFieldRelationshipHandler extends BaseHandler implements QueryByPageCallBack<WriteDbData4ClassInfo> {
    private static final Logger logger = LoggerFactory.getLogger(MethodCallPassedFieldRelationshipHandler.class);

    private final ClassInfoHandler classInfoHandler;
    private final GetSetMethodHandler getSetMethodHandler;
    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;
    private final FieldRelationshipHandler fieldRelationshipHandler;
    private final MethodArgReturnHandler methodArgReturnHandler;
    private final ManualAddFieldRelationshipHandler manualAddFieldRelationshipHandler;

    private WriteDbHandler4SetMethodAssignInfo writeDbHandler4SetMethodAssignInfo;

    public MethodCallPassedFieldRelationshipHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);

        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        manualAddFieldRelationshipHandler = new ManualAddFieldRelationshipHandler(dbOperWrapper);
    }

    public MethodCallPassedFieldRelationshipHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);

        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
        getSetMethodHandler = new GetSetMethodHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        fieldRelationshipHandler = new FieldRelationshipHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        manualAddFieldRelationshipHandler = new ManualAddFieldRelationshipHandler(dbOperWrapper);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return queryEndIdByPage(currentStartId, DbTableInfoEnum.DTIE_CLASS_INFO, DC.CI_RECORD_ID);
    }

    @Override
    public List<WriteDbData4ClassInfo> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        return classInfoHandler.queryClassesInfoByPage(lastQuery, currentStartId, currentEndId);
    }

    @Override
    public boolean handleDataList(List<WriteDbData4ClassInfo> dataList, Object... argsByPage) throws Exception {
        for (WriteDbData4ClassInfo classInfo : dataList) {
            // 执行处理
            doHandle(classInfo);
        }
        return true;
    }

    public boolean handle(String javaCG2OutputPath) {
        long startTime = System.currentTimeMillis();
        logger.info("处理通过方法调用传递的get/set方法关联关系-开始处理");

        // 人工增加get/set方法字段关联关系前的处理
        if (!manualAddFieldRelationshipHandler.beforeAdd()) {
            // 在try...finally之前返回
            return false;
        }
        try {
            writeDbHandler4SetMethodAssignInfo.beforeHandle(javaCG2OutputPath);
            // 分页查询并处理
            return QueryByPageHandler.queryAndHandle(this, JavaCG2Constants.RECORD_ID_MIN_BEFORE);
        } catch (Exception e) {
            logger.error("出现异常 ", e);
            return false;
        } finally {
            // 写入最后的数据
            writeDbHandler4SetMethodAssignInfo.afterHandle();
            // 人工增加get/set方法字段关联关系后的处理
            manualAddFieldRelationshipHandler.afterAdd();
            logger.info("处理通过方法调用传递的get/set方法关联关系-处理完毕，耗时: {} 秒", JACGUtil.getSpendSeconds(startTime));
        }
    }

    // 执行处理
    private void doHandle(WriteDbData4ClassInfo classInfo) {
        JavaCG2AccessFlags accessFlags = new JavaCG2AccessFlags(classInfo.getAccessFlags());
        if (accessFlags.isEnum() || accessFlags.isInterface() || accessFlags.isAnnotation()) {
            // 对于枚举、接口、注解，跳过
            return;
        }

        // 查询当前类及超类中的set方法
        List<BaseWriteDbData4GetSetMethod> setMethodList = getSetMethodHandler.queryGetSetMethodByClassNameSuper(false, classInfo.getClassName());
        if (JavaCG2Util.isCollectionEmpty(setMethodList)) {
            return;
        }

        for (BaseWriteDbData4GetSetMethod setMethod : setMethodList) {
            if (!JavaCG2Constants.FILE_KEY_CATEGORY_JDK.equals(setMethod.getFieldCategory())) {
                // 假如set方法对应字段类型不是JDK中的类型，则跳过
                continue;
            }

            // 查询set方法的被调用情况
            List<WriteDbData4MethodCall> setMethodCallList = methodCallHandler.queryMethodCallByCalleeFullMethod(setMethod.getFullMethod());
            if (JavaCG2Util.isCollectionEmpty(setMethodCallList)) {
                // set方法未被调用，不向dto的set方法被调用时的赋值信息表写入数据，因为当set方法在父类中时，会被子类写入多次，属于重复数据
                continue;
            }
            for (WriteDbData4MethodCall setMethodCall : setMethodCallList) {
                // 查询set方法被调用时的参数1信息解析后的数据
                List<AbstractMethodCallInfoParsed> methodCallInfoParsedList = methodCallInfoHandler.queryMethodCallInfoParsedObjArg(setMethodCall.getCallId(),
                        JavaCG2Constants.METHOD_CALL_ARGUMENTS_START_SEQ, false);

                // 查询set方法对应的直接赋值字段关联关系
                List<WriteDbData4FieldRelationship> fieldRelationshipList = fieldRelationshipHandler.queryDirectlyRelationshipBySetMethodCallId(setMethodCall.getCallId());
                if (methodCallInfoParsedList == null || fieldRelationshipList == null) {
                    // 查询到的数据为null，属于异常情况
                    continue;
                }
                if (methodCallInfoParsedList.size() <= fieldRelationshipList.size()) {
                    // set方法被调用时的参数1信息解析后的数据数量，小于等于set方法对应的字段关联关系数量，说明set方法都使用了get方法返回值作为参数
                    // 向dto的set方法被调用时的赋值信息表写入数据
                    insertSetMethodAssignInfo(null, setMethodCall.getCallId(), setMethod, setMethodCall, new JavaCG2Counter(0), 0, "", SetMethodAssignFlagEnum.SMAFE_GET, null,
                            null);
                    continue;
                }
                // 处理set方法被调用时的参数1信息解析后的数据列表
                handleSetMethodCallInfoParsedList(setMethod, setMethodCall, methodCallInfoParsedList, fieldRelationshipList);
            }
        }
    }

    // 处理set方法被调用时的参数1信息解析后的数据列表
    private void handleSetMethodCallInfoParsedList(BaseWriteDbData4GetSetMethod setMethod, WriteDbData4MethodCall setMethodCall,
                                                   List<AbstractMethodCallInfoParsed> methodCallInfoParsedList, List<WriteDbData4FieldRelationship> fieldRelationshipList) {
        logger.info("处理set方法被调用时的参数1信息解析后的数据列表 {} {}", setMethod, setMethodCall);
        int setMethodCallId = setMethodCall.getCallId();
        // 记录字段关联关系中get方法的调用id
        Set<Integer> fieldRelationshipGetMethodCallIdSet = new HashSet<>();
        for (WriteDbData4FieldRelationship fieldRelationship : fieldRelationshipList) {
            fieldRelationshipGetMethodCallIdSet.add(fieldRelationship.getGetMethodCallId());
        }

        // 记录当前处理的字段关联关系相关信息的栈
        ListAsStack<MethodCallPassedFRNode> methodCallPassedFRStack = new ListAsStack<>();
        /*
            添加初始数据
            当前处理的set方法对应的方法调用列表下标，设置为0
            当前处理的set方法对应的方法调用解析后信息列表下标，设置为-1，以下每次循环中都需要加1
         */
        methodCallPassedFRStack.push(new MethodCallPassedFRNode(setMethodCall, methodCallInfoParsedList));
        // 代表当前处理的路径序号，初始为0
        JavaCG2Counter seq = new JavaCG2Counter(0);
        /*
            记录已经写入数据库的seq+step
            key:    seq
            value:  step集合
         */
        Map<Integer, Set<Integer>> addedSuperSeqStepMap = new HashMap<>();
        // 开始循环处理
        while (!methodCallPassedFRStack.isEmpty()) {
            MethodCallPassedFRNode currentNode = methodCallPassedFRStack.peek();
            // 增加节点当前处理的set方法对应的方法调用解析后信息列表下标
            if (!currentNode.addMethodCallInfoParsedListIndex()) {
                // 当前节点处理的set方法对应的方法调用解析后信息列表已处理完毕
                // 序号加1
                seq.addAndGet();
                // 出栈
                methodCallPassedFRStack.pop();
                continue;
            }

            // 当前的步骤，等于栈的层级
            int step = methodCallPassedFRStack.getHead();
            WriteDbData4MethodCall currentMethodCall = currentNode.getCurrentMethodCall();

            // 处理栈中上层的节点
            if (!handleUpperStackNode(addedSuperSeqStepMap, setMethodCallId, setMethod, methodCallPassedFRStack, seq, currentMethodCall)) {
                // 出现了方法循环调用，当前节点不处理
                continue;
            }

            // 处理单个方法调用解析后的信息
            handleOneMethodCallInfoParsed(addedSuperSeqStepMap, setMethodCallId, setMethod, methodCallPassedFRStack, currentMethodCall, seq, step, currentNode,
                    fieldRelationshipGetMethodCallIdSet);
        }
    }

    /**
     * 处理单个方法调用解析后的信息
     *
     * @param addedSuperSeqStepMap
     * @param setMethodCallId
     * @param setMethod
     * @param methodCallPassedFRStack
     * @param currentMethodCall
     * @param seq
     * @param step
     * @param currentNode
     * @param fieldRelationshipGetMethodCallIdSet
     */
    private void handleOneMethodCallInfoParsed(Map<Integer, Set<Integer>> addedSuperSeqStepMap, int setMethodCallId, BaseWriteDbData4GetSetMethod setMethod,
                                               ListAsStack<MethodCallPassedFRNode> methodCallPassedFRStack, WriteDbData4MethodCall currentMethodCall,
                                               JavaCG2Counter seq, int step, MethodCallPassedFRNode currentNode, Set<Integer> fieldRelationshipGetMethodCallIdSet) {
        AbstractMethodCallInfoParsed currentMethodCallInfoParsed = currentNode.getCurrentMethodCallInfoParsed();
//        logger.info("handleOneMethodCallInfoParsed {} {} {} {} {}", setMethodCallId, seq, step, currentMethodCall, currentMethodCallInfoParsed);
        // 检查是否需要终止步骤
        if (checkTerminateStep(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, currentNode, seq, step, currentMethodCallInfoParsed)) {
            return;
        }

        if (currentMethodCallInfoParsed instanceof MethodCallInfoParsed4MCReturnCallId) {
            // 当前的方法调用解析后数据属于方法调用返回
            MethodCallInfoParsed4MCReturnCallId methodCallInfoParsed4MCReturnCallId = (MethodCallInfoParsed4MCReturnCallId) currentMethodCallInfoParsed;
            if (fieldRelationshipGetMethodCallIdSet != null && fieldRelationshipGetMethodCallIdSet.contains(methodCallInfoParsed4MCReturnCallId.getMethodCallId())) {
                // 当前的方法调用信息存在对应的字段关联关系时执行
                // 向dto的set方法被调用时的赋值信息表写入数据
                insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, "", SetMethodAssignFlagEnum.SMAFE_GET, null,
                        currentNode);
                return;
            }
            // 当前的方法调用信息不存在对应的字段关联关系时执行
            // 处理set方法，参数为方法调用返回值
            handleSetMethodWithMethodCallReturn(addedSuperSeqStepMap, setMethodCallId, setMethod, methodCallPassedFRStack, methodCallInfoParsed4MCReturnCallId,
                    currentNode, seq, step);
            return;
        }
        if (currentMethodCallInfoParsed instanceof MethodCallInfoParsed4MethodArg) {
            // 当前的方法调用解析后数据属于方法参数
            MethodCallInfoParsed4MethodArg methodCallInfoParsed4MethodArg = (MethodCallInfoParsed4MethodArg) currentMethodCallInfoParsed;
            // 处理set方法，参数为调用方法参数
            handleSetMethodWithMethodArg(addedSuperSeqStepMap, setMethodCallId, setMethod, methodCallPassedFRStack, currentMethodCall, methodCallInfoParsed4MethodArg,
                    currentNode, seq, step);
        }
    }

    /**
     * 处理栈中上层的节点
     *
     * @param setMethodCallId
     * @param setMethod
     * @param methodCallPassedFRStack
     * @param seq
     * @param currentMethodCall
     * @return true: 继续处理 false: 当前节点不处理
     */
    private boolean handleUpperStackNode(Map<Integer, Set<Integer>> addedSuperSeqStepMap, int setMethodCallId, BaseWriteDbData4GetSetMethod setMethod,
                                         ListAsStack<MethodCallPassedFRNode> methodCallPassedFRStack, JavaCG2Counter seq, WriteDbData4MethodCall currentMethodCall) {
        if (seq.getCount() == 0 && methodCallPassedFRStack.getHead() == 0) {
            // 当序号等于0，且处理栈中栈底元素时，不需要后续处理
            return true;
        }

        // 从栈底开始向上遍历，到栈顶的下一层为止
        for (int i = 0; i < methodCallPassedFRStack.getHead(); i++) {
            // 获取当前处理的节点
            MethodCallPassedFRNode methodCallPassedFRNode = methodCallPassedFRStack.getElementAt(i);
            // 获取当前处理的方法调用
            WriteDbData4MethodCall upperMethodCall = methodCallPassedFRNode.getCurrentMethodCall();
            if (currentMethodCall.getCallId() == upperMethodCall.getCallId()) {
                // 出现了方法循环调用，当前节点不处理
                logger.warn("出现了方法循环调用，当前节点不处理 {} {} {} {}", currentMethodCall.getCallId(), currentMethodCall.getCallerFullMethod(), currentMethodCall.getCallerLineNumber(),
                        currentMethodCall.getCalleeFullMethod());
                return false;
            }

            // seq从0开始，当seq大于等于1时，将栈中的上层节点的信息写入数据库
            AbstractMethodCallInfoParsed methodCallInfoParsed = methodCallPassedFRNode.getCurrentMethodCallInfoParsed();
            if (methodCallInfoParsed instanceof MethodCallInfoParsed4MCReturnCallId) {
                // 当前处理的方法调用解析后信息属于通过方法调用返回值进行传递
                MethodCallInfoParsed4MCReturnCallId methodCallInfoParsed4MCReturnCallId = (MethodCallInfoParsed4MCReturnCallId) methodCallInfoParsed;
                WriteDbData4MethodCall tmpMethodCall = methodCallHandler.queryMethodCallByCallId(methodCallInfoParsed4MCReturnCallId.getMethodCallId());
                // 向dto的set方法被调用时的赋值信息表写入数据
                insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, upperMethodCall, seq, i, tmpMethodCall.getCalleeFullMethod(),
                        SetMethodAssignFlagEnum.SMAFE_METHOD_CALL_RETURN, null, methodCallPassedFRNode);
            } else if (methodCallInfoParsed instanceof MethodCallInfoParsed4MethodArg) {
                // 当前处理的方法调用解析后信息属于通过方法调用参数进行传递
                MethodCallInfoParsed4MethodArg methodCallInfoParsed4MethodArg = (MethodCallInfoParsed4MethodArg) methodCallInfoParsed;
                // 向dto的set方法被调用时的赋值信息表写入数据
                insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, upperMethodCall, seq, i,
                        String.valueOf(methodCallInfoParsed4MethodArg.getMethodArgSeq()), SetMethodAssignFlagEnum.SMAFE_METHOD_CALL_ARGS, null, methodCallPassedFRNode);
            }
        }
        return true;
    }

    // 处理set方法，参数为方法调用返回值
    private void handleSetMethodWithMethodCallReturn(Map<Integer, Set<Integer>> addedSuperSeqStepMap, int setMethodCallId, BaseWriteDbData4GetSetMethod setMethod,
                                                     ListAsStack<MethodCallPassedFRNode> methodCallPassedFRStack,
                                                     MethodCallInfoParsed4MCReturnCallId methodCallInfoParsed4MCReturnCallId, MethodCallPassedFRNode currentNode,
                                                     JavaCG2Counter seq, int step) {
        // 查询当前的方法调用
        WriteDbData4MethodCall currentMethodCall = methodCallHandler.queryMethodCallByCallId(methodCallInfoParsed4MCReturnCallId.getMethodCallId());
        String currentCalleeClassName = JACGClassMethodUtil.getClassNameFromMethod(currentMethodCall.getCalleeFullMethod());

        if (JACGClassMethodUtil.calleeMatchesGetMethod(currentMethodCall)) {
            // 被调用方法符合get方法
            // 查询对应的get方法
            BaseWriteDbData4GetSetMethod currentGetMethod = getSetMethodHandler.queryGetSetMethodByClassMethodSuper(true, currentCalleeClassName,
                    currentMethodCall.getCalleeMethodName());
            if (currentGetMethod != null) {
                // set方法的参数的来源属于get方法
                // 判断栈中当前处理的各层节点的方法调用解析后信息是否有属于等值转换的情况
                boolean equivalentConversionFlag = checkStackNodeEQC(methodCallPassedFRStack);
                JavaCG2FieldRelationshipTypeEnum relationshipTypeEnum = equivalentConversionFlag ? JavaCG2FieldRelationshipTypeEnum.FRTE_METHOD_CALL_PASSED_EQC :
                        JavaCG2FieldRelationshipTypeEnum.FRTE_METHOD_CALL_PASSED;
                // 人工添加字段关联关系
                Integer fldRelationshipId = manualAddFieldRelationshipHandler.manualAddFieldRelationship(currentMethodCall.getCallerFullMethod(),
                        currentMethodCall.getCallerLineNumber(), currentMethodCall.getCallId(), setMethodCallId, currentGetMethod.getClassName(),
                        currentGetMethod.getMethodName(), setMethod.getClassName(), setMethod.getMethodName(), relationshipTypeEnum,
                        FieldRelationshipFlagsEnum.FRF_SET_METHOD_CALL_PASSED.getFlag());
                // 向dto的set方法被调用时的赋值信息表写入数据
                insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, currentMethodCall.getCalleeFullMethod(),
                        SetMethodAssignFlagEnum.SMAFE_GET_BY_MCP, fldRelationshipId, currentNode);
                return;
            }
        }

        // 被调用方法不符合get方法，继续处理
        List<AbstractMethodCallInfoParsed> add2StackMethodCallInfoParsedList = new ArrayList<>();
        // 查询set方法参数对应方法返回的方法参数序号
        List<WriteDbData4MethodReturnArgSeq> methodReturnArgSeqList = methodArgReturnHandler.queryMethodReturnArgSeq(currentMethodCall.getCalleeFullMethod());
        if (!JavaCG2Util.isCollectionEmpty(methodReturnArgSeqList)) {
            for (WriteDbData4MethodReturnArgSeq setMethodArgMethodArgSeq : methodReturnArgSeqList) {
                // set方法参数来源为方法调用返回值，查询被调用方法返回对应的方法参数
                List<AbstractMethodCallInfoParsed> setMethodArgMethodCallInfoParsed = methodCallInfoHandler.queryMethodCallInfoParsedObjArg(currentMethodCall.getCallId(),
                        setMethodArgMethodArgSeq.getReturnArgSeq(), JavaCG2YesNoEnum.isYes(setMethodArgMethodArgSeq.getEquivalentConversion()));
                if (setMethodArgMethodCallInfoParsed != null) {
                    add2StackMethodCallInfoParsedList.addAll(setMethodArgMethodCallInfoParsed);
                }
            }
        }

        // 查询set方法参数对应方法返回的方法调用ID
        List<WriteDbData4MethodReturnCallId> methodReturnCallIdList = methodArgReturnHandler.queryMethodReturnCallId(currentMethodCall.getCalleeFullMethod());
        if (!JavaCG2Util.isCollectionEmpty(methodReturnCallIdList)) {
            for (WriteDbData4MethodReturnCallId methodReturnCallId : methodReturnCallIdList) {
                MethodCallInfoParsed4MCReturnCallId methodCallInfoParsed =
                        new MethodCallInfoParsed4MCReturnCallId(JavaCG2YesNoEnum.isYes(methodReturnCallId.getEquivalentConversion()));
                methodCallInfoParsed.setMethodCallId(methodReturnCallId.getReturnCallId());
                add2StackMethodCallInfoParsedList.add(methodCallInfoParsed);
            }
        }

        if (add2StackMethodCallInfoParsedList.isEmpty()) {
            // 查询set方法参数对应的方法调用返回的调用信息为空
            SetMethodAssignFlagEnum setMethodAssignFlagEnum = SetMethodAssignFlagEnum.SMAFE_MC_NO_RETURN;
            Integer classAccessFlag = classInfoHandler.queryClassAccessFlag(currentCalleeClassName);
            if (classAccessFlag != null && JavaCG2ByteCodeUtil.isEnumFlag(classAccessFlag)) {
                setMethodAssignFlagEnum = SetMethodAssignFlagEnum.SMAFE_MC_ENUM;
            }
            // 向dto的set方法被调用时的赋值信息表写入数据
            insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, currentMethodCall.getCalleeFullMethod(),
                    setMethodAssignFlagEnum, null, currentNode);
            // 当前序号加1
            seq.addAndGet();
            return;
        }
        // 向dto的set方法被调用时的赋值信息表写入数据
        insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, currentMethodCall.getCalleeFullMethod(),
                SetMethodAssignFlagEnum.SMAFE_METHOD_CALL_RETURN, null, currentNode);
        // 查询到set方法参数对应的方法调用返回的方法调用ID及方法参数序号非空，入栈
        methodCallPassedFRStack.push(new MethodCallPassedFRNode(currentMethodCall, add2StackMethodCallInfoParsedList));
    }

    /**
     * 判断栈中当前处理的各层节点的方法调用解析后信息是否有属于等值转换的情况
     *
     * @param methodCallPassedFRStack
     * @return true: 存在属于等值转换的情况 false: 不存在属于等值转换的情况
     */
    private boolean checkStackNodeEQC(ListAsStack<MethodCallPassedFRNode> methodCallPassedFRStack) {
        for (int i = 0; i <= methodCallPassedFRStack.getHead(); i++) {
            MethodCallPassedFRNode methodCallPassedFRNode = methodCallPassedFRStack.getElementAt(i);
            AbstractMethodCallInfoParsed methodCallInfoParsed = methodCallPassedFRNode.getCurrentMethodCallInfoParsed();
            if (methodCallInfoParsed.isEquivalentConversion()) {
                return true;
            }
        }
        return false;
    }

    // 处理set方法，参数为调用方法参数
    private void handleSetMethodWithMethodArg(Map<Integer, Set<Integer>> addedSuperSeqStepMap, int setMethodCallId, BaseWriteDbData4GetSetMethod setMethod,
                                              ListAsStack<MethodCallPassedFRNode> methodCallPassedFRStack, WriteDbData4MethodCall currentMethodCall,
                                              MethodCallInfoParsed4MethodArg methodCallInfoParsed4MethodArg, MethodCallPassedFRNode currentNode, JavaCG2Counter seq,
                                              int step) {
        // 查询当前方法的被调用情况
        List<WriteDbData4MethodCall> callerMethodCallList = methodCallHandler.queryMethodCallByCalleeFullMethod(currentMethodCall.getCallerFullMethod());
        if (JavaCG2Util.isCollectionEmpty(callerMethodCallList)) {
            // 向dto的set方法被调用时的赋值信息表写入数据
            insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, currentMethodCall.getCallerFullMethod(),
                    SetMethodAssignFlagEnum.SMAFE_ARG_NO_MC, null, currentNode);
            return;
        }

        List<WriteDbData4MethodCall> allMethodCallList = new ArrayList<>();
        List<AbstractMethodCallInfoParsed> allMethodCallInfoParsedList = new ArrayList<>();
        // set方法，参数为调用方法参数，查询方法被调用时的参数被调用信息解析后的数据
        for (WriteDbData4MethodCall callerMethodCall : callerMethodCallList) {
            // set方法参数来源为方法参数，查询对应方法对应参数被调用时的信息
            List<AbstractMethodCallInfoParsed> methodCallInfoParsedList = methodCallInfoHandler.queryMethodCallInfoParsedObjArg(callerMethodCall.getCallId(),
                    methodCallInfoParsed4MethodArg.getMethodArgSeq(), methodCallInfoParsed4MethodArg.isEquivalentConversion());
            if (!JavaCG2Util.isCollectionEmpty(methodCallInfoParsedList)) {
                for (AbstractMethodCallInfoParsed methodCallInfoParsed : methodCallInfoParsedList) {
                    allMethodCallList.add(callerMethodCall);
                    allMethodCallInfoParsedList.add(methodCallInfoParsed);
                }
            }
        }

        if (allMethodCallInfoParsedList.isEmpty()) {
            // 向dto的set方法被调用时的赋值信息表写入数据
            insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, currentMethodCall.getCallerFullMethod(),
                    SetMethodAssignFlagEnum.SMAFE_ARG_NO_MCI, null, currentNode);
            return;
        }
        // 向dto的set方法被调用时的赋值信息表写入数据
        insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, String.valueOf(methodCallInfoParsed4MethodArg.getMethodArgSeq()),
                SetMethodAssignFlagEnum.SMAFE_METHOD_CALL_ARGS, null, currentNode);
        // set方法参数来源为方法参数，查询对应方法对应参数被调用时的信息非空，入栈
        methodCallPassedFRStack.push(new MethodCallPassedFRNode(allMethodCallList, allMethodCallInfoParsedList));
    }

    /**
     * 判断是否需要终止步骤
     *
     * @param addedSuperSeqStepMap
     * @param setMethodCallId
     * @param setMethod
     * @param currentMethodCall
     * @param seq
     * @param step
     * @param currentMethodCallInfoParsed
     * @return true: 需要终止步骤 false: 不终止步骤
     */
    private boolean checkTerminateStep(Map<Integer, Set<Integer>> addedSuperSeqStepMap, int setMethodCallId, BaseWriteDbData4GetSetMethod setMethod,
                                       WriteDbData4MethodCall currentMethodCall, MethodCallPassedFRNode currentNode, JavaCG2Counter seq, int step,
                                       AbstractMethodCallInfoParsed currentMethodCallInfoParsed) {
        String assignInfo = null;
        SetMethodAssignFlagEnum setMethodAssignFlagEnum = null;
        if (currentMethodCallInfoParsed instanceof MethodCallInfoParsed4Constant) {
            MethodCallInfoParsed4Constant methodCallInfoParsed4Constant = (MethodCallInfoParsed4Constant) currentMethodCallInfoParsed;
            assignInfo = methodCallInfoParsed4Constant.getConstValue();
            setMethodAssignFlagEnum = SetMethodAssignFlagEnum.SMAFE_CONSTANT;
        } else if (currentMethodCallInfoParsed instanceof MethodCallInfoParsed4Field) {
            MethodCallInfoParsed4Field methodCallInfoParsed4Field = (MethodCallInfoParsed4Field) currentMethodCallInfoParsed;
            assignInfo = methodCallInfoParsed4Field.getFieldName();
            setMethodAssignFlagEnum = SetMethodAssignFlagEnum.SMAFE_FIELD;
        } else if (currentMethodCallInfoParsed instanceof MethodCallInfoParsed4StaticField) {
            MethodCallInfoParsed4StaticField methodCallInfoParsed4StaticField = (MethodCallInfoParsed4StaticField) currentMethodCallInfoParsed;
            assignInfo = methodCallInfoParsed4StaticField.getClassFieldName();
            setMethodAssignFlagEnum = SetMethodAssignFlagEnum.SMAFE_STATIC_FIELD;
        }
        if (setMethodAssignFlagEnum != null) {
            // 向dto的set方法被调用时的赋值信息表写入数据
            insertSetMethodAssignInfo(addedSuperSeqStepMap, setMethodCallId, setMethod, currentMethodCall, seq, step, assignInfo, setMethodAssignFlagEnum, null, currentNode);
            return true;
        }
        return false;
    }

    // 向dto的set方法被调用时的赋值信息表写入数据
    private void insertSetMethodAssignInfo(Map<Integer, Set<Integer>> addedSuperSeqStepMap,
                                           int setMethodCallId,
                                           BaseWriteDbData4GetSetMethod setMethod,
                                           WriteDbData4MethodCall currMethodCall,
                                           JavaCG2Counter seq,
                                           int step,
                                           String assignInfo,
                                           SetMethodAssignFlagEnum setMethodAssignFlagEnum,
                                           Integer fldRelationshipId,
                                           MethodCallPassedFRNode methodCallPassedFRNode) {
        if (addedSuperSeqStepMap != null) {
            // 判断当前记录是否有写入过数据库表
            Set<Integer> addedSuperStepSet = addedSuperSeqStepMap.computeIfAbsent(seq.getCount(), k -> new HashSet<>());
            if (!addedSuperStepSet.add(step)) {
                return;
            }
        }

        boolean equivalentConversion = false;
        if (methodCallPassedFRNode != null) {
            AbstractMethodCallInfoParsed methodCallInfoParsed = methodCallPassedFRNode.getCurrentMethodCallInfoParsed();
            equivalentConversion = methodCallInfoParsed.isEquivalentConversion();
        }

        WriteDbData4SetMethodAssignInfo writeDbData4SetMethodAssignInfo = new WriteDbData4SetMethodAssignInfo();
        writeDbData4SetMethodAssignInfo.setSetRecordId(setMethod.getRecordId());
        writeDbData4SetMethodAssignInfo.setSetMethodCallId(setMethodCallId);
        writeDbData4SetMethodAssignInfo.setSeq(seq.getCount());
        writeDbData4SetMethodAssignInfo.setStep(step);
        writeDbData4SetMethodAssignInfo.setFldRelationshipId(fldRelationshipId);
        writeDbData4SetMethodAssignInfo.setCurrCallId(currMethodCall == null ? 0 : currMethodCall.getCallId());
        writeDbData4SetMethodAssignInfo.setCallerMethodHash(currMethodCall == null ? "" : currMethodCall.getCallerMethodHash());
        writeDbData4SetMethodAssignInfo.setCallerFullMethod(currMethodCall == null ? "" : currMethodCall.getCallerFullMethod());
        writeDbData4SetMethodAssignInfo.setCallerLineNumber(currMethodCall == null ? 0 : currMethodCall.getCallerLineNumber());
        writeDbData4SetMethodAssignInfo.setCalleeFullMethod(currMethodCall == null ? "" : currMethodCall.getCalleeFullMethod());
        writeDbData4SetMethodAssignInfo.setSetMethodHash(setMethod.getMethodHash());
        writeDbData4SetMethodAssignInfo.setSetFullMethod(setMethod.getFullMethod());
        writeDbData4SetMethodAssignInfo.setSetMethodInSuper(JavaCG2YesNoEnum.parseIntValue(setMethod.isInSuperClass()));
        writeDbData4SetMethodAssignInfo.setFlag(setMethodAssignFlagEnum.getFlag());
        writeDbData4SetMethodAssignInfo.setFlagDesc(setMethodAssignFlagEnum.getDesc());
        writeDbData4SetMethodAssignInfo.setAssignInfo(assignInfo);
        writeDbData4SetMethodAssignInfo.setEquivalentConversion(JavaCG2YesNoEnum.parseIntValue(equivalentConversion));

//        logger.info("### {}", writeDbData4SetMethodAssignInfo);
        writeDbHandler4SetMethodAssignInfo.addData(writeDbData4SetMethodAssignInfo);
        writeDbHandler4SetMethodAssignInfo.tryInsertDb();
    }

    public void setWriteDbHandler4SetMethodAssignInfo(WriteDbHandler4SetMethodAssignInfo writeDbHandler4SetMethodAssignInfo) {
        this.writeDbHandler4SetMethodAssignInfo = writeDbHandler4SetMethodAssignInfo;
    }
}
