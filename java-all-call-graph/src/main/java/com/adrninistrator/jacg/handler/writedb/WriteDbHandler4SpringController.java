package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.method.MethodDetailNoReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.conf.ConfigHandler;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: 写入数据库，Spring Controller信息
 * dependsWriteDbTableEnums 中不需要指定 DbTableInfoEnum.DTIE_METHOD_ANNOTATION ，因为这两个表的写入顺序不固定，检查是否写入可能不通过
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_CONTROLLER,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_SPRING_CONTROLLER,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_METHOD_INFO,
                DbTableInfoEnum.DTIE_METHOD_RETURN_GENERICS_TYPE,
                DbTableInfoEnum.DTIE_METHOD_CALL_INFO,
                DbTableInfoEnum.DTIE_CONFIG}
)
public class WriteDbHandler4SpringController extends AbstractWriteDbHandler<WriteDbData4SpringController> {

    private MethodInfoHandler methodInfoHandler;
    private MethodArgReturnHandler methodArgReturnHandler;
    private MethodCallInfoHandler methodCallInfoHandler;

    // 使用 java-callgraph2 组件解析方法调用时是否解析被调用对象和参数可能的类型与值
    private boolean javaCG2ParseMethodCallTypeValue = false;

    public WriteDbHandler4SpringController(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
        methodArgReturnHandler = new MethodArgReturnHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
        // 判断使用 java-callgraph2 组件解析方法调用时是否解析被调用对象和参数可能的类型与值
        try (ConfigHandler configHandler = new ConfigHandler(dbOperWrapper)) {
            if (configHandler.checkParseMethodCallTypeValue()) {
                javaCG2ParseMethodCallTypeValue = true;
            }
        }
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "记录id，从1开始",
                "方法hash+字节数",
                "序号，从0开始，大于0代表有多种可能",
                "用于显示的URI",
                "类上的注解path属性原始值",
                "方法上的注解path属性原始值",
                "注解类名",
                "唯一类名",
                "方法可能用于文件上传，1:是，0:否",
                "方法可能用于文件下载，1:是，0:否",
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring Controller信息";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"通过类及方法注解定义的Spring Controller信息，包括对应的方法、Controller的URI",
                "方法可能用于文件上传字段，通过方法请求参数判断",
                "方法可能用于文件下载字段，通过方法请求参数与返回类型判断"};
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringController data) {
        // 通过Spring Controller方法参数判断是否可能用于文件上传
        JavaCG2YesNoEnum maybeFileUpload = checkMaybeFileUpload(data.getFullMethod(), data.getReturnType());
        // 通过Spring Controller方法参数与返回类型判断是否可能用于文件下载
        JavaCG2YesNoEnum maybeFileDownload = checkMaybeFileDownload(data.getFullMethod(), data.getReturnType());
        data.setMaybeFileUpload(maybeFileUpload.getIntValue());
        data.setMaybeFileDownload(maybeFileDownload.getIntValue());

        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSeq(),
                data.getShowUri(),
                data.getClassPath(),
                data.getMethodPath(),
                data.getAnnotationName(),
                data.getSimpleClassName(),
                data.getMaybeFileUpload(),
                data.getMaybeFileDownload(),
                data.getFullMethod(),
                data.getReturnType()
        };
    }


    /**
     * 通过Spring Controller方法参数判断是否可能用于文件上传
     *
     * @param fullMethod 完整方法
     * @param returnType 方法返回类型
     * @return
     */
    private JavaCG2YesNoEnum checkMaybeFileUpload(String fullMethod, String returnType) {
        MethodDetailNoReturnType methodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(fullMethod);
        List<String> argTypeList = methodDetailNoReturnType.getArgTypeList();
        for (int argSeq = 0; argSeq < argTypeList.size(); argSeq++) {
            String argType = argTypeList.get(argSeq);
            String argTypeStr = JavaCG2ClassMethodUtil.removeAllArrayFlag(argType);
            // 判断当前方法参数是否为 MultipartFile
            if (!StringUtils.equalsAny(argTypeStr, JACGCommonNameConstants.SPRING_MULTI_PART_FILE_CLASS, JACGCommonNameConstants.SPRING_COMMONS_MULTI_PART_FILE_CLASS)) {
                continue;
            }
            // 判断使用 java-callgraph2 组件解析方法调用时是否解析被调用对象和参数可能的类型与值
            if (!javaCG2ParseMethodCallTypeValue) {
                return JavaCG2YesNoEnum.NOT_SURE;
            }
            // 判断方法参数是否有被使用
            if (methodCallInfoHandler.checkMethodArgUsed(fullMethod, returnType, argSeq + 1)) {
                return JavaCG2YesNoEnum.YES;
            }
        }
        return JavaCG2YesNoEnum.NO;
    }

    /**
     * 通过Spring Controller方法参数与返回类型判断是否可能用于文件下载
     *
     * @param fullMethod 完整方法
     * @param returnType 方法返回类型
     * @return
     */
    private JavaCG2YesNoEnum checkMaybeFileDownload(String fullMethod, String returnType) {
        // 通过方法参数判断
        MethodDetailNoReturnType methodDetailNoReturnType = JACGClassMethodUtil.genMethodDetailNoReturnType(fullMethod);
        List<String> argTypeList = methodDetailNoReturnType.getArgTypeList();
        for (int argSeq = 0; argSeq < argTypeList.size(); argSeq++) {
            String argType = argTypeList.get(argSeq);
            String argTypeStr = JavaCG2ClassMethodUtil.removeAllArrayFlag(argType);
            // 判断当前方法参数是否为 HttpServletResponse
            if (!StringUtils.equals(argTypeStr, JACGCommonNameConstants.JAVAX_HTTP_SERVLET_RESPONSE)) {
                continue;
            }
            // 判断使用 java-callgraph2 组件解析方法调用时是否解析被调用对象和参数可能的类型与值
            if (!javaCG2ParseMethodCallTypeValue) {
                return JavaCG2YesNoEnum.NOT_SURE;
            }
            // 判断方法参数是否有被使用
            if (methodCallInfoHandler.checkMethodArgUsed(fullMethod, returnType, argSeq + 1)) {
                return JavaCG2YesNoEnum.YES;
            }
        }

        // 通过返回类型判断
        WriteDbData4MethodInfo methodInfo = methodInfoHandler.queryMethodInfoByFullMethod(fullMethod, returnType);
        if (methodInfo == null || !StringUtils.equalsAny(methodInfo.getReturnTypeNad(), JACGCommonNameConstants.SPRING_HTTP_ENTITY_CLASS,
                JACGCommonNameConstants.SPRING_RESPONSE_ENTITY_CLASS)) {
            return JavaCG2YesNoEnum.NO;
        }
        // 返回类型为HttpEntity或ResponseEntity
        List<WriteDbData4MethodReturnGenericsType> methodReturnGenericsTypeList = methodArgReturnHandler.queryReturnGenericsTypeByMethod(fullMethod, returnType);
        if (methodReturnGenericsTypeList == null || methodReturnGenericsTypeList.size() != 1) {
            return JavaCG2YesNoEnum.NO;
        }
        // 返回类型泛型类型非String时，认为用于文件下载
        if (JavaCG2CommonNameConstants.CLASS_NAME_STRING.equals(methodReturnGenericsTypeList.get(0).getGenericsTypeNad())) {
            return JavaCG2YesNoEnum.NO;
        }
        return JavaCG2YesNoEnum.YES;
    }
}
