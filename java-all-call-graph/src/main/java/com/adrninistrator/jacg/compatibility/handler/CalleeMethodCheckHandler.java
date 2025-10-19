package com.adrninistrator.jacg.compatibility.handler;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.compatibility.CompatibilityHandlerDto;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.querybypage.QueryByPageHandler;
import com.adrninistrator.jacg.handler.querybypage.callback.QueryByPageCallBack;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/8/1
 * @description: 检查方法调用中的调用方法处理类
 */
public class CalleeMethodCheckHandler extends BaseCompatibilityCheckHandler implements QueryByPageCallBack<WriteDbData4MethodCall> {
    private static final Logger logger = LoggerFactory.getLogger(CalleeMethodCheckHandler.class);

    public static final String FILE_NAME = "b.被调用方法存在问题" + JavaCG2Constants.EXT_MD;
    public static final String[] FILE_HEADER_ARRAY = new String[]{
            "问题描述",
            "调用方法",
            "调用方法返回类型",
            "调用类名",
            "调用方法代码行号",
            "方法调用类型",
            "调用类所在jar文件路径",
            "调用类所在jar文件内部路径",
            "被调用方法",
            "被调用方法返回类型",
            "被调用类名",
            "被调用方法所在jar文件路径",
            "被调用方法所在jar文件内部路径"
    };
    public static final String FILE_HEADER = StringUtils.join(FILE_HEADER_ARRAY, JavaCG2Constants.FLAG_TAB);

    private WriterSupportHeader writer;

    public CalleeMethodCheckHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 检查方法调用中的被调用方法
    public boolean check() {
        String outputFilePath;
        try {
            outputFilePath = currentOutputDirPath + File.separator + FILE_NAME;
            writer = new WriterSupportHeader(outputFilePath, FILE_HEADER);
            if (!QueryByPageHandler.queryAndHandle(this, 0)) {
                return false;
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        } finally {
            IOUtils.closeQuietly(writer);
        }
        return textFileToExcel(outputFilePath);
    }

    @Override
    public int queryCurrentEndId(int currentStartId, Object... argsByPage) {
        return super.queryEndIdByPage(currentStartId, DbTableInfoEnum.DTIE_METHOD_CALL, DC.MC_CALL_ID);
    }

    @Override
    public List<WriteDbData4MethodCall> queryDataByPage(int currentStartId, int currentEndId, boolean lastQuery, Object... argsByPage) {
        List<String> existsInstructionList = JavaCG2CallTypeEnum.getExistsInstructionList();
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_NORMAL_MC_BY_PAGE_CALL_ID;
        List<Object> argList = new ArrayList<>();
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " > ?" +
                    " and " + DC.MC_CALL_ID + " <= ?" +
                    " and " + DC.MC_CALL_TYPE + " in " + JACGSqlUtil.genQuestionString(existsInstructionList.size());
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        argList.add(currentStartId);
        argList.add(currentEndId);
        argList.addAll(existsInstructionList);
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    @Override
    public boolean handleDataList(List<WriteDbData4MethodCall> dataList, Object... argsByPage) throws Exception {
        for (WriteDbData4MethodCall methodCall : dataList) {
            // 处理一个方法调用
            handleOneMethodCall(methodCall);
        }
        return true;
    }

    // 处理一个被调用类
    private void handleOneMethodCall(WriteDbData4MethodCall methodCall) throws IOException {
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
        if (JavaCG2ConstantTypeEnum.getFromType(calleeClassName).isPrimitive()) {
            // 被调用类为基本类型，跳过以下检查
            return;
        }

        // 检查被调用类是否存在
        if (!checkClassExists(calleeClassName)) {
            // 记录存在问题的被调用方法
            recordErrorCalleeMethod(methodCall, "方法调用中的被调用类未找到");
            return;
        }
        // 被调用类存在
        // 检查被调用方法是否存在
        WriteDbData4MethodInfo methodInfo = checkCalleeMethodExists(methodCall);
        if (methodInfo == null) {
            return;
        }
        // 检查存在的方法
        checkExistedMethod(methodCall, methodInfo);
    }

    /**
     * 检查被调用方法是否存在
     *
     * @param methodCall 方法调用对象
     * @return
     */
    private WriteDbData4MethodInfo checkCalleeMethodExists(WriteDbData4MethodCall methodCall) throws IOException {
        String returnType = methodCall.getRawReturnType();
        String methodNameAndArgTypes = JACGClassMethodUtil.getMethodNameWithArgsFromFull(methodCall.getCalleeFullMethod());
        List<FullMethodWithReturnType> fullMethodWithReturnTypeList = new ArrayList<>();
        // 从当前类到父类/接口开始尝试，判断对应方法是否存在
        fullMethodWithReturnTypeList.add(new FullMethodWithReturnType(methodCall.getCalleeFullMethod(), returnType));
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
        if (!JavaCG2CommonNameConstants.CLASS_NAME_OBJECT.equals(calleeClassName)) {
            // 当被调用类不是Object时，添加Object类对应方法
            String objectMethod = JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(JavaCG2CommonNameConstants.CLASS_NAME_OBJECT, methodNameAndArgTypes);
            fullMethodWithReturnTypeList.add(new FullMethodWithReturnType(objectMethod, returnType));
        }

        for (CompatibilityHandlerDto compatibilityHandlerDto : compatibilityHandlerDtoList) {
            MethodInfoHandler tmpMethodInfoHandler = compatibilityHandlerDto.getMethodInfoHandler();
            List<String> superClassNameList = new ArrayList<>();
            for (FullMethodWithReturnType fullMethodWithReturnType : fullMethodWithReturnTypeList) {
                // 根据完整方法查询方法信息，若在当前类中未查找到，则在父类与接口中查找
                WriteDbData4MethodInfo methodInfo = tmpMethodInfoHandler.queryMethodByFullMethodSuperInterface(fullMethodWithReturnType.getFullMethod(),
                        fullMethodWithReturnType.getReturnType(), superClassNameList);
                if (methodInfo != null) {
                    return methodInfo;
                }
            }
            // 添加父类/接口相关方法
            for (String superClassName : superClassNameList) {
                String superMethod = JavaCG2ClassMethodUtil.formatFullMethodWithArgTypes(superClassName, methodNameAndArgTypes);
                FullMethodWithReturnType superFullMethodWithReturnType = new FullMethodWithReturnType(superMethod, returnType);
                if (!fullMethodWithReturnTypeList.contains(superFullMethodWithReturnType)) {
                    fullMethodWithReturnTypeList.add(superFullMethodWithReturnType);
                }
            }
        }
        // 记录存在问题的被调用方法
        recordErrorCalleeMethod(methodCall, "未找到被调用方法");
        return null;
    }

    // 检查存在的方法
    private void checkExistedMethod(WriteDbData4MethodCall methodCall, WriteDbData4MethodInfo methodInfo) throws IOException {
        JavaCG2AccessFlags methodAccessFlags = new JavaCG2AccessFlags(methodInfo.getAccessFlags());

        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCallerFullMethod());
        // 被调用类使用被调用方法所在的类
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
        // 检查方法修饰符
        if (methodAccessFlags.isPrivate()) {
            // 检查是否为当前类调用，或内部类调用
            if (!callerClassName.equals(calleeClassName)
                    && !classInfoHandler.checkInnerClassAny(callerClassName, calleeClassName)) {
                // 记录存在问题的被调用方法
                recordErrorCalleeMethod(methodCall, "被调用方法为private方法，无法被其他类调用");
            }
        } else if (methodAccessFlags.isProtected()) {
            // 处理protected被调用方法
            String calleeMethodNameWithArgType = JACGClassMethodUtil.getMethodNameWithArgsFromFull(methodCall.getCalleeFullMethod());
            if (!callerClassName.equals(calleeClassName)
                    && !checkExtendsImplClass(calleeClassName, callerClassName)
                    && !JACGCommonNameConstants.METHOD_NAME_WITH_ARG_TYPE_CLONE.equals(calleeMethodNameWithArgType)
                    && !JavaCG2ClassMethodUtil.checkSamePackage(callerClassName, methodInfo.getClassName())
                    && !checkExtendsImplClass(methodInfo.getClassName(), callerClassName)
            ) {
                /*
                    不满足以下情况下调用protected方法时认为异常
                    调用类与被调用类是同一个类
                    调用类是被调用类子类
                    被调用方法为clone()
                    调用方法所在类与被调用方法定义的类同包名
                    调用方法所在类为被调用方法定义的类的子类
                 */
                // 记录存在问题的被调用方法
                recordErrorCalleeMethod(methodCall, "被调用方法为protected方法，调用方法不满足以下任意情况：1、相同的类，2、相同包中的类，3、子类或实现类");
            }
        } else if (!methodAccessFlags.isPublic()) {
            if (!callerClassName.equals(calleeClassName)
                    && !JavaCG2ClassMethodUtil.checkSamePackage(calleeClassName, callerClassName)) {
                // 记录存在问题的被调用方法
                recordErrorCalleeMethod(methodCall, "被调用方法修饰符为default，调用方法不满足以下任意情况：1、相同的类，2、相同包中的类");
            }
        }

        // 检查方法是否静态/非静态
        if (JavaCG2CallTypeEnum.CTE_RAW_INVOKE_STATIC.getType().equals(methodCall.getCallType()) && !methodAccessFlags.isStatic()) {
            // 记录存在问题的被调用方法
            recordErrorCalleeMethod(methodCall, "被调用方法编译时属于静态方法，但目前是非静态方法");
        } else if (!JavaCG2CallTypeEnum.CTE_RAW_INVOKE_STATIC.getType().equals(methodCall.getCallType()) && methodAccessFlags.isStatic()) {
            // 记录存在问题的被调用方法
            recordErrorCalleeMethod(methodCall, "被调用方法编译时属于非静态方法，但目前是静态方法");
        }
    }

    // 记录存在问题的被调用方法
    private void recordErrorCalleeMethod(WriteDbData4MethodCall methodCall, String reason) throws IOException {
        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCallerFullMethod());
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(methodCall.getCalleeFullMethod());
        WriteDbData4JarInfo callerJarInfo = jarInfoMap.get(methodCall.getCallerJarNum());
        String calleeJarPath = "";
        String calleeJarInnerPath = "";
        if (methodCall.getCalleeJarNum() != null) {
            WriteDbData4JarInfo calleeJarInfo = jarInfoMap.get(methodCall.getCallerJarNum());
            calleeJarPath = calleeJarInfo.getJarFullPath();
            calleeJarInnerPath = calleeJarInfo.getInnerJarPath();
        }
        writer.writeDataInLine(
                reason,
                methodCall.getCallerFullMethod(),
                methodCall.getCallerReturnType(),
                callerClassName,
                String.valueOf(methodCall.getCallerLineNumber()),
                methodCall.getCallType(),
                callerJarInfo.getJarFullPath(),
                callerJarInfo.getInnerJarPath(),
                methodCall.getCalleeFullMethod(),
                methodCall.getRawReturnType(),
                calleeClassName,
                calleeJarPath,
                calleeJarInnerPath
        );
    }

    /**
     * 需要生成excel文件
     *
     * @return
     */
    @Override
    protected boolean needGenerateExcel() {
        return true;
    }
}