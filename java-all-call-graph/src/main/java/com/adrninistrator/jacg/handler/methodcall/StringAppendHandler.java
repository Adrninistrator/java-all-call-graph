package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnCallId;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnConstValue;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallObjArgValueAndSource;
import com.adrninistrator.jacg.handler.dto.string.StringAppendParseResult;
import com.adrninistrator.jacg.handler.dto.string.StringAppendParseResultInner;
import com.adrninistrator.jacg.handler.dto.string.element.BaseStringElement;
import com.adrninistrator.jacg.handler.dto.string.element.StringElementClassGetName;
import com.adrninistrator.jacg.handler.dto.string.element.StringElementConstant;
import com.adrninistrator.jacg.handler.dto.string.element.StringElementStaticFieldMethodCallReturn;
import com.adrninistrator.jacg.handler.enums.EnumsHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/9/2
 * @description: 解析字符串拼接处理类
 */
public class StringAppendHandler extends BaseHandler {

    private static final Logger logger = LoggerFactory.getLogger(StringAppendHandler.class);

    private final EnumsHandler enumsHandler;
    private final MethodArgReturnHandler methodArgReturnHandler;
    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;
    private final MethodCallClassFieldHandler methodCallClassFieldHandler;
    private final MethodInfoHandler methodInfoHandler;

    public StringAppendHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        enumsHandler = new EnumsHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodCallClassFieldHandler = new MethodCallClassFieldHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    public StringAppendHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        enumsHandler = new EnumsHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        methodCallClassFieldHandler = new MethodCallClassFieldHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    /**
     * 解析指定方法调用的指定序号的参数（或被调用对象）对应的字符串，支持字符串拼接，支持解析枚举常量方法调用返回常量
     *
     * @param useArgStringMethodCallId 方法调用参数中使用字符串拼接结果的方法调用ID
     * @param objArgSeq                需要解析的被调用对象或参数序号，0代表被调用对象，1开始为参数
     * @return
     */
    public StringAppendParseResult parseStringAppend4MethodArg(int useArgStringMethodCallId, int objArgSeq) {
        StringAppendParseResult stringAppendParseResult = new StringAppendParseResult();
        // 查询指定序号的方法调用
        WriteDbData4MethodCall useArgStringMethodCall = methodCallHandler.queryMethodCallByCallId(useArgStringMethodCallId);
        if (useArgStringMethodCall == null) {
            logger.warn("通过方法调用ID未查询到对应的方法调用 {}", useArgStringMethodCallId);
            return stringAppendParseResult;
        }

        // 查询方法调用中被调用对象与参数对应的常量值或来源（包括使用方法调用的返回值，或方法参数）
        MethodCallObjArgValueAndSource methodCallObjArgValueAndSource = methodCallInfoHandler.queryMethodCallObjArgValueAndSource(useArgStringMethodCallId, objArgSeq);
        if (methodCallObjArgValueAndSource.isContainsMultiType() || methodCallObjArgValueAndSource.getOneTypeDataNum() > 1) {
            logger.warn("方法调用参数存在多种情况，不支持识别对应的字符串拼接 {} {}", objArgSeq, useArgStringMethodCall.genPrintInfo());
            return stringAppendParseResult;
        }
        if (methodCallObjArgValueAndSource.getOneTypeDataNum() == 0) {
            logger.warn("方法调用参数信息未查询到，不支持识别对应的字符串拼接 {} {}", objArgSeq, useArgStringMethodCall.genPrintInfo());
            return stringAppendParseResult;
        }

        if (JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE == methodCallObjArgValueAndSource.getMethodCallInfoTypeEnum()) {
            // 方法调用参数为常量，直接返回
            String stringValue = methodCallObjArgValueAndSource.getConstantValueList().get(0);
            stringAppendParseResult.setRawString(JavaCG2Util.wrapWithQuotes(stringValue));
            stringAppendParseResult.setParsedValue(stringValue);
            return stringAppendParseResult;
        }

        if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID == methodCallObjArgValueAndSource.getMethodCallInfoTypeEnum()) {
            // 方法调用参数为方法调用返回值
            Integer argMethodCallReturnId = methodCallObjArgValueAndSource.getUseMethodCallReturnCallIdList().get(0);
            parseStringAppendMethodCallReturn(useArgStringMethodCallId, argMethodCallReturnId, stringAppendParseResult);
            return stringAppendParseResult;
        }

        logger.warn("方法调用参数不支持识别对应的字符串拼接 {}", useArgStringMethodCall.genPrintInfo());
        return stringAppendParseResult;
    }

    /**
     * 解析指定方法返回的字符串，支持字符串拼接，支持解析枚举常量方法调用返回常量
     *
     * @param fullMethod
     * @param returnType
     * @return
     */
    public StringAppendParseResult parseStringAppend4MethodReturn(String fullMethod, String returnType) {
        StringAppendParseResult stringAppendParseResult = new StringAppendParseResult();
        if (!JavaCG2CommonNameConstants.CLASS_NAME_STRING.equals(returnType)) {
            logger.warn("只支持返回 {} 类型的方法 {} {}", JavaCG2CommonNameConstants.CLASS_NAME_STRING, fullMethod, returnType);
            return stringAppendParseResult;
        }
        // 查找对应的方法
        WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(fullMethod, returnType);
        if (methodInfo == null) {
            logger.warn("未找到指定的方法 {} {}", fullMethod, returnType);
            return stringAppendParseResult;
        }

        List<String> returnConstantList = new ArrayList<>();
        List<Integer> returnMethodCallIdList = new ArrayList<>();

        // 查询方法返回的常量
        List<WriteDbData4MethodReturnConstValue> methodReturnConstValueList = methodArgReturnHandler.queryMethodReturnConstValue(methodInfo.getMethodHash());
        for (WriteDbData4MethodReturnConstValue methodReturnConstValue : methodReturnConstValueList) {
            returnConstantList.add(methodReturnConstValue.getConstValue());
        }
        // 查询方法返回的方法调用ID
        List<WriteDbData4MethodReturnCallId> methodReturnCallIdList = methodArgReturnHandler.queryMethodReturnCallId(methodInfo.getMethodHash());
        for (WriteDbData4MethodReturnCallId methodReturnCallId : methodReturnCallIdList) {
            returnMethodCallIdList.add(methodReturnCallId.getReturnCallId());
        }
        if (returnConstantList.size() > 1 || returnMethodCallIdList.size() > 1) {
            logger.error("方法返回的常量或方法调用包含多种情况，不支持处理 {} {}", fullMethod, returnType);
            return stringAppendParseResult;
        }
        if (returnConstantList.isEmpty() && returnMethodCallIdList.isEmpty()) {
            logger.error("未查询到方法返回的常量或方法调用，不支持处理 {} {}", fullMethod, returnType);
            return stringAppendParseResult;
        }

        if (returnConstantList.size() == 1) {
            // 方法返回为常量，直接返回
            String stringValue = returnConstantList.get(0);
            stringAppendParseResult.setRawString(JavaCG2Util.wrapWithQuotes(stringValue));
            stringAppendParseResult.setParsedValue(stringValue);
            return stringAppendParseResult;
        }

        // 方法返回为方法调用返回值
        Integer argMethodCallReturnId = returnMethodCallIdList.get(0);
        parseStringAppendMethodCallReturn(argMethodCallReturnId, argMethodCallReturnId, stringAppendParseResult);
        return stringAppendParseResult;
    }

    /**
     * 解析指定的方法调用的指定序号的参数（或被调用对象）对应的拼接的字符串，参数为方法调用返回值时的处理
     *
     * @param useStringMethodCallReturnId 需要解析的使用字符串拼接结果的方法调用ID
     * @param methodCallReturnId          需要解析的方法调用ID
     * @param stringAppendParseResult     字符串拼接结果解析
     */
    private void parseStringAppendMethodCallReturn(int useStringMethodCallReturnId, int methodCallReturnId, StringAppendParseResult stringAppendParseResult) {
        WriteDbData4MethodCall useStringMethodCall = methodCallHandler.queryMethodCallByCallId(methodCallReturnId);
        MethodDetailNoReturnType methodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(useStringMethodCall.getCalleeFullMethod());
        if (!StringUtils.equalsAny(methodDetailNoReturnType.getClassName(), JACGCommonNameConstants.CLASS_NAME_STRING_BUILDER, JACGCommonNameConstants.CLASS_NAME_STRING_BUFFER)
                && !StringUtils.equalsAny(methodDetailNoReturnType.getMethodName(), JavaCG2CommonNameConstants.METHOD_NAME_INIT, JACGCommonNameConstants.METHOD_NAME_APPEND,
                JACGCommonNameConstants.METHOD_NAME_TO_STRING)) {
            StringAppendParseResultInner stringAppendParseResultInner = new StringAppendParseResultInner();
            // 尝试解析获取类名的字符串
            if (!parseClassGetName(useStringMethodCall, stringAppendParseResultInner)) {
                return;
            }
            if (stringAppendParseResultInner.getStringElement() != null) {
                stringAppendParseResult.setRawString(stringAppendParseResultInner.getRawString());
                stringAppendParseResult.setParsedValue(stringAppendParseResultInner.getParsedValue());
                stringAppendParseResult.setStringElementList(Collections.singletonList(stringAppendParseResultInner.getStringElement()));
                return;
            }
            logger.warn("被调用方法不是字符串拼接，不支持处理 {} {}", methodCallReturnId, useStringMethodCall.getCalleeFullMethod());
            return;
        }

        // 记录StringBuilder/StringBuffer对象创建时的方法调用
        WriteDbData4MethodCall stringInstanceInitMethodCall;
        if (JavaCG2CommonNameConstants.METHOD_NAME_INIT.equals(methodDetailNoReturnType.getMethodName())) {
            // 参数传递的方法调用ID对应StringBuilder/StringBuffer对象创建时的方法调用，直接使用
            stringInstanceInitMethodCall = useStringMethodCall;
        } else {
            // 参数传递的方法调用ID对应StringBuilder/StringBuffer对象append、toString时的方法调用，查询StringBuilder/StringBuffer对象创建时的方法调用
            stringInstanceInitMethodCall = queryStringInstanceInitCallId(useStringMethodCall);
            if (stringInstanceInitMethodCall == null) {
                return;
            }
        }
        // 解析字符串拼接的值
        parseStringAppendValue(useStringMethodCallReturnId, stringInstanceInitMethodCall, useStringMethodCall, stringAppendParseResult);
    }

    // 查询StringBuilder/StringBuffer对象创建时的方法调用
    private WriteDbData4MethodCall queryStringInstanceInitCallId(WriteDbData4MethodCall methodCall) {
        String calleeSimpleClassName = methodCall.getCalleeSimpleClassName();
        WriteDbData4MethodCall currentMethodCall = methodCall;
        while (true) {
            // 查询当前StringBuilder/StringBuffer对象方法调用中被调用对象的信息
            MethodCallObjArgValueAndSource methodCallObjArgValueAndSource = methodCallInfoHandler.queryMethodCallObjArgValueAndSource(currentMethodCall.getCallId(),
                    JavaCG2Constants.METHOD_CALL_OBJECT_SEQ);
            if (methodCallObjArgValueAndSource.isContainsMultiType() || methodCallObjArgValueAndSource.getOneTypeDataNum() > 1) {
                logger.warn("StringBuilder/StringBuffer对象方法调用参数存在多种情况，不支持识别对应的字符串拼接 {} {}", JavaCG2Constants.METHOD_CALL_OBJECT_SEQ, currentMethodCall.genPrintInfo());
                return null;
            }
            if (methodCallObjArgValueAndSource.getOneTypeDataNum() == 0) {
                logger.warn("StringBuilder/StringBuffer对象方法调用参数信息未查询到，不支持识别对应的字符串拼接 {} {}", JavaCG2Constants.METHOD_CALL_OBJECT_SEQ, currentMethodCall.genPrintInfo());
                return null;
            }
            if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID != methodCallObjArgValueAndSource.getMethodCallInfoTypeEnum()) {
                // 被调用对象不属于方法调用返回值时跳过
                return null;
            }
            // 被调用对象属于方法调用返回值时，进行处理
            currentMethodCall = methodCallHandler.queryMethodCallByCallId(methodCallObjArgValueAndSource.getUseMethodCallReturnCallIdList().get(0));
            if (!calleeSimpleClassName.equals(currentMethodCall.getCalleeSimpleClassName())) {
                // 被调用类为StringBuilder/StringBuffer时跳过
                continue;
            }
            // 被调用类为StringBuilder/StringBuffer时进行处理
            if (JavaCG2CommonNameConstants.METHOD_NAME_INIT.equals(currentMethodCall.getCalleeMethodName())) {
                return currentMethodCall;
            }
            if (!JACGCommonNameConstants.METHOD_NAME_APPEND.equals(currentMethodCall.getCalleeMethodName())) {
                logger.warn("字符串拼接使用了不支持处理的方法 {}", currentMethodCall.genPrintInfo());
                return null;
            }
            // 字符串拼接使用append方法，继续处理
        }
    }

    /**
     * 解析字符串拼接的值
     *
     * @param useArgStringMethodCallId     需要解析的使用字符串拼接结果的方法调用ID
     * @param stringInstanceInitMethodCall StringBuilder/StringBuffer对象创建时的方法调用
     * @param useArgStringMethodCall       方法调用参数中使用字符串拼接结果的方法调用
     * @param stringAppendParseResult      字符串拼接解析结果
     */
    private void parseStringAppendValue(int useArgStringMethodCallId, WriteDbData4MethodCall stringInstanceInitMethodCall, WriteDbData4MethodCall useArgStringMethodCall,
                                        StringAppendParseResult stringAppendParseResult) {
        List<String> rawStringList = new ArrayList<>();
        List<String> parsedValueList = new ArrayList<>();
        List<BaseStringElement> stringElementList = new ArrayList<>();

        StringAppendParseResultInner initStringAppendParseResultInner = new StringAppendParseResultInner();
        // 查询StringBuilder/StringBuffer对象创建时的初始化字符串
        if (!queryStringInstanceArgValue(stringInstanceInitMethodCall, initStringAppendParseResultInner, 1, true)) {
            logger.warn("查询StringBuilder/StringBuffer对象创建时的初始化字符串失败 {}", stringInstanceInitMethodCall.genPrintInfo());
            return;
        }

        if (StringUtils.isNotEmpty(initStringAppendParseResultInner.getRawString())) {
            rawStringList.add(initStringAppendParseResultInner.getRawString());
            parsedValueList.add(initStringAppendParseResultInner.getParsedValue());
        }

        // 记录StringBuilder/StringBuffer对象创建及append产生的对象的方法调用ID集合
        Set<Integer> methodCallIdSet = new HashSet<>();
        methodCallIdSet.add(stringInstanceInitMethodCall.getCallId());

        // 查询指定方法中对StringBuilder/StringBuffer对象的使用
        List<WriteDbData4MethodCall> stringInstanceMethodCallList = methodCallHandler.queryMethodCallInCallerByCallee(useArgStringMethodCall.getCallerMethodHash(),
                useArgStringMethodCall.getCalleeSimpleClassName(), stringInstanceInitMethodCall.getCallId(), useArgStringMethodCallId);
        for (WriteDbData4MethodCall stringInstanceMethodCall : stringInstanceMethodCallList) {
            // 查询被调用的StringBuilder/StringBuffer对象信息
            MethodCallObjArgValueAndSource methodCallObjArgValueAndSource = methodCallInfoHandler.queryMethodCallObjArgValueAndSource(stringInstanceMethodCall.getCallId(),
                    JavaCG2Constants.METHOD_CALL_OBJECT_SEQ);
            if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID != methodCallObjArgValueAndSource.getMethodCallInfoTypeEnum()) {
                // 当对StringBuilder/StringBuffer对象的方法调用的被调用对象不属于方法调用返回值时，跳过
                continue;
            }
            // 当对StringBuilder/StringBuffer对象的方法调用的被调用对象属于方法调用返回值时，进行处理
            Integer stringInstanceCallId = methodCallObjArgValueAndSource.getUseMethodCallReturnCallIdList().get(0);
            if (!methodCallIdSet.contains(stringInstanceCallId)) {
                // 当前StringBuilder/StringBuffer对象的方法调用不属于需要处理的对象，或append返回的对象，跳过
                continue;
            }
            // 以上内容需要先判断，排除不需要处理的方法调用
            // 记录StringBuilder/StringBuffer对象调用append方法时的方法调用ID
            methodCallIdSet.add(stringInstanceMethodCall.getCallId());

            if (methodCallObjArgValueAndSource.isContainsMultiType() || methodCallObjArgValueAndSource.getOneTypeDataNum() > 1) {
                logger.warn("StringBuilder/StringBuffer对象调用append方法调用参数存在多种情况，不支持识别对应的字符串拼接 {} {}", JavaCG2Constants.METHOD_CALL_OBJECT_SEQ,
                        stringInstanceMethodCall.genPrintInfo());
                return;
            }

            if (methodCallObjArgValueAndSource.getOneTypeDataNum() == 0) {
                logger.warn("StringBuilder/StringBuffer对象调用append方法调用参数信息未查询到，不支持识别对应的字符串拼接 {} {}", JavaCG2Constants.METHOD_CALL_OBJECT_SEQ,
                        stringInstanceMethodCall.genPrintInfo());
                return;
            }

            if (!JACGCommonNameConstants.METHOD_NAME_APPEND.equals(stringInstanceMethodCall.getCalleeMethodName())) {
                logger.warn("当前StringBuilder/StringBuffer对象调用的方法不支持处理 {}", stringInstanceMethodCall.genPrintInfo());
                continue;
            }
            StringAppendParseResultInner stringAppendParseResultInner = new StringAppendParseResultInner();
            // 查询StringBuilder/StringBuffer对象append的值
            if (!queryStringInstanceArgValue(stringInstanceMethodCall, stringAppendParseResultInner, 1, false)) {
                logger.warn("查询StringBuilder/StringBuffer对象append的值失败 {}", stringInstanceMethodCall.genPrintInfo());
                return;
            }
            // 记录StringBuilder/StringBuffer对象append的值
            rawStringList.add(stringAppendParseResultInner.getRawString());
            parsedValueList.add(stringAppendParseResultInner.getParsedValue());
            stringElementList.add(stringAppendParseResultInner.getStringElement());
        }
        String rawString = StringUtils.join(rawStringList, " + ");
        String parsedValue = StringUtils.join(parsedValueList, "");
        stringAppendParseResult.setRawString(rawString);
        stringAppendParseResult.setParsedValue(parsedValue);
        stringAppendParseResult.setStringElementList(stringElementList);
    }

    // 查询调用指定方法时指定参数的值，支持常量或枚举常量方法调用返回值
    private boolean queryStringInstanceArgValue(WriteDbData4MethodCall useArgMethodCall, StringAppendParseResultInner stringAppendParseResultInner, int argSeq,
                                                boolean allowEmpty) {
        // 查询指定参数
        MethodCallObjArgValueAndSource methodCallObjArgValueAndSource = methodCallInfoHandler.queryMethodCallObjArgValueAndSource(useArgMethodCall.getCallId(), argSeq);
        if (methodCallObjArgValueAndSource.isContainsMultiType() || methodCallObjArgValueAndSource.getOneTypeDataNum() > 1) {
            logger.warn("参数{}存在多种情况，不支持识别对应的字符串拼接 {}", argSeq, useArgMethodCall.genPrintInfo());
            return false;
        }
        if (methodCallObjArgValueAndSource.getOneTypeDataNum() == 0) {
            if (allowEmpty) {
                return true;
            }
            logger.warn("未查询到参数{}相关信息，不支持处理 {}", argSeq, useArgMethodCall.genPrintInfo());
            return false;
        }
        if (JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE == methodCallObjArgValueAndSource.getMethodCallInfoTypeEnum()) {
            // 对应参数是常量
            String value = methodCallObjArgValueAndSource.getConstantValueList().get(0);
            stringAppendParseResultInner.setRawString(JavaCG2Util.wrapWithQuotes(value));
            stringAppendParseResultInner.setParsedValue(value);
            StringElementConstant stringElementConstant = new StringElementConstant();
            stringElementConstant.setConstantValue(value);
            stringAppendParseResultInner.setStringElement(stringElementConstant);
            return true;
        }
        if (JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID == methodCallObjArgValueAndSource.getMethodCallInfoTypeEnum()) {
            // 对应参数是方法调用返回值
            // 查询对应参数使用的静态字段方法调用返回值
            List<WriteDbData4MethodCallStaticFieldMCR> methodCallStaticFieldMCRList =
                    methodCallClassFieldHandler.queryMethodCallStaticFieldMCR4MethodCall(useArgMethodCall.getCallId(), 1);
            if (JavaCG2Util.isCollectionEmpty(methodCallStaticFieldMCRList)) {
                Integer argMethodCallReturnId = methodCallObjArgValueAndSource.getUseMethodCallReturnCallIdList().get(0);
                WriteDbData4MethodCall argMethodCall = methodCallHandler.queryMethodCallByCallId(argMethodCallReturnId);
                if (argMethodCall == null) {
                    logger.error("未查询到参数{}对应的方法调用 {} {}", argSeq, useArgMethodCall.genPrintInfo(), argMethodCallReturnId);
                    return true;
                }
                // 尝试解析获取类名的字符串
                if (!parseClassGetName(argMethodCall, stringAppendParseResultInner)) {
                    return false;
                }
                if (stringAppendParseResultInner.getStringElement() != null) {
                    return true;
                }

                logger.warn("未查询到参数{}对应的静态字段方法调用返回值，不支持处理 {}", argSeq, useArgMethodCall.genPrintInfo());
                return false;
            }
            if (methodCallStaticFieldMCRList.size() > 1) {
                logger.warn("查询到参数{}包含多种静态字段方法调用返回值，不支持处理 {} {}", argSeq, methodCallStaticFieldMCRList.size(), useArgMethodCall.genPrintInfo());
                return false;
            }
            WriteDbData4MethodCallStaticFieldMCR methodCallStaticFieldMCR = methodCallStaticFieldMCRList.get(0);
            // 尝试查询方法调用对应的枚举常量方法返回值
            String enumConstantMethodReturnValue = enumsHandler.queryEnumConstantFieldMethodReturnValue(methodCallStaticFieldMCR.getFieldType(),
                    methodCallStaticFieldMCR.getFieldName(), methodCallStaticFieldMCR.getCalleeFullMethod(), methodCallStaticFieldMCR.getCalleeReturnType());
            if (enumConstantMethodReturnValue == null) {
                logger.warn("参数{}对应的方法调用未查询到枚举常量方法返回值，不支持处理 {} {}", argSeq, useArgMethodCall.genPrintInfo(), methodCallStaticFieldMCR);
                return false;
            }
            // 记录字符串
            String rawString = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(methodCallStaticFieldMCR.getFieldType()) + JavaCG2Constants.FLAG_DOT +
                    methodCallStaticFieldMCR.getFieldName() + JavaCG2Constants.FLAG_DOT + methodCallStaticFieldMCR.getCalleeMethodName() + JavaCG2Constants.FLAG_LEFT_RIGHT_BRACKET;
            stringAppendParseResultInner.setRawString(rawString);
            stringAppendParseResultInner.setParsedValue(enumConstantMethodReturnValue);
            StringElementStaticFieldMethodCallReturn stringElementStaticFieldMethodCallReturn = genStringElementStaticFieldMethodCallReturn(methodCallStaticFieldMCR);
            stringAppendParseResultInner.setStringElement(stringElementStaticFieldMethodCallReturn);
            return true;
        }
        logger.warn("参数{}不支持处理 {}", argSeq, useArgMethodCall.genPrintInfo());
        return false;
    }

    private StringElementStaticFieldMethodCallReturn genStringElementStaticFieldMethodCallReturn(WriteDbData4MethodCallStaticFieldMCR methodCallStaticFieldMCR) {
        StringElementStaticFieldMethodCallReturn stringElementStaticFieldMethodCallReturn = new StringElementStaticFieldMethodCallReturn();
        stringElementStaticFieldMethodCallReturn.setClassName(methodCallStaticFieldMCR.getClassName());
        stringElementStaticFieldMethodCallReturn.setFieldType(methodCallStaticFieldMCR.getFieldType());
        stringElementStaticFieldMethodCallReturn.setFieldName(methodCallStaticFieldMCR.getFieldName());
        stringElementStaticFieldMethodCallReturn.setFullMethod(methodCallStaticFieldMCR.getCalleeFullMethod());
        stringElementStaticFieldMethodCallReturn.setMethodReturnType(methodCallStaticFieldMCR.getCalleeReturnType());
        return stringElementStaticFieldMethodCallReturn;
    }

    /**
     * 尝试解析获取类名的字符串
     *
     * @param classGetNameMethodCall
     * @param stringAppendParseResultInner
     * @return true: 解析成功 false: 解析失败
     */
    private boolean parseClassGetName(WriteDbData4MethodCall classGetNameMethodCall, StringAppendParseResultInner stringAppendParseResultInner) {
        if (!JavaCG2CallTypeEnum.isCalleeReplaceType(classGetNameMethodCall.getCallType())) {
            // 方法调用中被调用类未被替换，返回
            return true;
        }
        String calleeMethodNameWithArgType = JACGClassMethodUtil.getMethodNameWithArgsFromFull(classGetNameMethodCall.getCalleeFullMethod());
        if (!StringUtils.equalsAny(calleeMethodNameWithArgType, JACGCommonNameConstants.METHOD_NAME_WITH_ARG_TYPE_GET_NAME,
                JACGCommonNameConstants.METHOD_NAME_WITH_ARG_TYPE_GET_SIMPLE_NAME)) {
            // 被调用方法不是获取类名的方法，返回
            return true;
        }
        // 查询被调用类被替换前的类名
        String rawCalleeClassName = methodCallHandler.queryRawCalleeClassName(classGetNameMethodCall.getCallId());
        if (StringUtils.isBlank(rawCalleeClassName)) {
            logger.error("未查询到方法调用中被调用类被替换前的类名 {}", classGetNameMethodCall.genPrintInfo());
            return false;
        }
        if (!JavaCG2CommonNameConstants.CLASS_NAME_CLASS.equals(rawCalleeClassName)) {
            // 被调用类原始类不是Class，返回
            return true;
        }
        StringElementClassGetName stringElementClassGetName = new StringElementClassGetName();
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(classGetNameMethodCall.getCalleeFullMethod());
        stringElementClassGetName.setClassName(calleeClassName);
        stringElementClassGetName.setMethodName(classGetNameMethodCall.getCalleeMethodName());

        stringAppendParseResultInner.setRawString(calleeClassName + ".class." + calleeMethodNameWithArgType);
        if (JACGCommonNameConstants.METHOD_NAME_WITH_ARG_TYPE_GET_NAME.equals(calleeMethodNameWithArgType)) {
            stringAppendParseResultInner.setParsedValue(calleeClassName);
        } else {
            String calleeSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(calleeClassName);
            stringAppendParseResultInner.setParsedValue(calleeSimpleClassName);
        }
        stringAppendParseResultInner.setStringElement(stringElementClassGetName);
        return true;
    }
}
