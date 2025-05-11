package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo;
import com.adrninistrator.jacg.dto.methodcall.MethodCallInfo4Read;
import com.adrninistrator.jacg.dto.methodcall.ObjArgsInfoInMethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/7/3
 * @description: 方法调用信息工具类
 */
public class JACGMethodCallInfoUtil {
    private static final Logger logger = LoggerFactory.getLogger(JACGMethodCallInfoUtil.class);

    /**
     * 添加方法调用信息
     *
     * @param methodCallInfo
     * @param type
     * @param value
     */
    public static void addMethodCallInfo(MethodCallInfo methodCallInfo, String type, String valueType, String value) {
        JavaCG2MethodCallInfoTypeEnum javaCG2MethodCallInfoTypeEnum = JavaCG2MethodCallInfoTypeEnum.getFromType(type);
        switch (javaCG2MethodCallInfoTypeEnum) {
            case MCIT_TYPE:
                methodCallInfo.setType(value);
                break;
            case MCIT_VALUE:
            case MCIT_BASE64_VALUE:
                methodCallInfo.setValueType(valueType);
                methodCallInfo.setValue(value);
                break;
            case MCIT_STATIC_FIELD:
                methodCallInfo.setStaticField(value);
                break;
            case MCIT_STATIC_FIELD_METHOD_CALL:
                methodCallInfo.setStaticFieldMethodCall(value);
                break;
            case MCIT_NAME_OF_FIELD:
                methodCallInfo.setNameOfField(value);
                break;
            case MCIT_NAME_OF_VARIABLE:
                methodCallInfo.setNameOfVariable(value);
                break;
            case MCIT_METHOD_ARG_SEQ:
                methodCallInfo.setMethodArgSeq(Integer.parseInt(value));
                break;
            case MCIT_METHOD_CALL_RETURN_CALL_ID:
                methodCallInfo.setMethodCallReturnId(value);
                break;
            case MCIT_METHOD_ARG_SEQ_EQC:
                methodCallInfo.setMethodArgSeqEQC(Integer.parseInt(value));
                break;
            case MCIT_METHOD_CALL_RETURN_CALL_ID_EQC:
                methodCallInfo.setMethodCallReturnIdEQC(value);
                break;
            case MCIT_METHOD_CATCH_EXCEPTION_FROM_OFFSET:
                // 什么也不做
                break;
            default:
                logger.error("未知类型 {}", type);
        }
    }

    /**
     * 对方法调用信息的值进行转换
     *
     * @param methodCallInfo
     * @return
     */
    public static void transferValue(WriteDbData4MethodCallInfo methodCallInfo) {
        if (JavaCG2ConstantTypeEnum.CONSTTE_CHAR.getType().equals(methodCallInfo.getValueType())) {
            // 将char类型的参数值转换为字符串
            String charValue = String.valueOf((char) Integer.parseInt(methodCallInfo.getTheValue()));
            methodCallInfo.setOrigValue(methodCallInfo.getTheValue());
            methodCallInfo.setTheValue(charValue);
        }
    }

    /**
     * 生成用于人工查看的方法调用中使用的相关信息
     *
     * @param methodCallInfo
     * @return
     */
    public static MethodCallInfo4Read genMethodCallInfo4Read(MethodCallInfo methodCallInfo) {
        if (methodCallInfo.getValue() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE, methodCallInfo.getValue());
        }
        if (methodCallInfo.getStaticField() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD, methodCallInfo.getStaticField());
        }
        if (methodCallInfo.getStaticFieldMethodCall() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD_METHOD_CALL, methodCallInfo.getStaticFieldMethodCall());
        }
        if (methodCallInfo.getNameOfField() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_FIELD, methodCallInfo.getNameOfField());
        }
        if (methodCallInfo.getNameOfVariable() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_VARIABLE, methodCallInfo.getNameOfVariable());
        }
        if (methodCallInfo.getMethodArgSeq() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ, String.valueOf(methodCallInfo.getMethodArgSeq()));
        }
        if (methodCallInfo.getMethodCallReturnId() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID, methodCallInfo.getMethodCallReturnId());
        }
        if (methodCallInfo.getMethodArgSeqEQC() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ_EQC, String.valueOf(methodCallInfo.getMethodArgSeqEQC()));
        }
        if (methodCallInfo.getMethodCallReturnIdEQC() != null) {
            return new MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID_EQC, methodCallInfo.getMethodCallReturnIdEQC());
        }
        return null;
    }

    /**
     * 根据被调用对象或参数的序号生成对应的中文描述
     *
     * @param seq
     * @return
     */
    public static String genObjArgDesc(Integer seq) {
        if (seq == 0) {
            return "被调用对象";
        }
        return "参数" + seq;
    }

    /**
     * 获取类型匹配的被调用对象及参数的用于人工查看的方法调用中使用的相关信息
     *
     * @param calleeMethodDetailNoReturnType      被调用方法详情
     * @param objArgsInfoInMethodCall 被调用对象与参数使用的信息
     * @param expectedArgTypeList     需要处理的参数类型列表，被调用对象类型也需要指定
     * @return Map的key含义: 0代表被调用对象，1开始为参数
     */
    public static Map<Integer, List<MethodCallInfo4Read>> genMethodCallInfo4ReadMapByArgType(MethodDetailNoReturnType calleeMethodDetailNoReturnType, ObjArgsInfoInMethodCall objArgsInfoInMethodCall,
                                                                                             List<String> expectedArgTypeList) {
        Map<Integer, List<MethodCallInfo4Read>> methodCallInfo4ReadMap = new HashMap<>();
        if (objArgsInfoInMethodCall == null) {
            return methodCallInfo4ReadMap;
        }

        // 处理被调用对象
        if (expectedArgTypeList.contains(calleeMethodDetailNoReturnType.getClassName())) {
            // 被调用对象类型与需要处理的参数类型匹配，进行处理
            List<MethodCallInfo4Read> methodCallInfo4ReadList = genMethodCallInfo4ReadList(objArgsInfoInMethodCall.getObjInfo());
            if (!JavaCG2Util.isCollectionEmpty(methodCallInfo4ReadList)) {
                methodCallInfo4ReadMap.put(JavaCG2Constants.METHOD_CALL_OBJECT_SEQ, methodCallInfo4ReadList);
            }
        }

        // 处理参数
        List<String> calleeArgTypeList = calleeMethodDetailNoReturnType.getArgTypeList();
        for (int i = 0; i < calleeArgTypeList.size(); i++) {
            if (!expectedArgTypeList.contains(calleeArgTypeList.get(i))) {
                // 当前参数的类型不需要处理
                continue;
            }
            // 当前参数的类型需要处理
            // 参数序号从1开始
            int argSeq = i + 1;
            List<MethodCallInfo4Read> methodCallInfo4ReadList = genMethodCallInfo4ReadList(objArgsInfoInMethodCall.getArgMethodCallInfo(argSeq));
            if (!JavaCG2Util.isCollectionEmpty(methodCallInfo4ReadList)) {
                methodCallInfo4ReadMap.put(argSeq, methodCallInfo4ReadList);
            }
        }

        return methodCallInfo4ReadMap;
    }

    // 生成用于人工查看的方法调用中使用的相关信息列表
    private static List<MethodCallInfo4Read> genMethodCallInfo4ReadList(List<MethodCallInfo> methodCallInfoList) {
        if (methodCallInfoList == null) {
            return null;
        }

        List<MethodCallInfo4Read> methodCallInfo4ReadList = new ArrayList<>();
        for (MethodCallInfo methodCallInfo : methodCallInfoList) {
            MethodCallInfo4Read methodCallInfo4Read = JACGMethodCallInfoUtil.genMethodCallInfo4Read(methodCallInfo);
            if (methodCallInfo4Read != null) {
                methodCallInfo4ReadList.add(methodCallInfo4Read);
            }
        }
        return methodCallInfo4ReadList;
    }

    private JACGMethodCallInfoUtil() {
        throw new IllegalStateException("illegal");
    }
}
