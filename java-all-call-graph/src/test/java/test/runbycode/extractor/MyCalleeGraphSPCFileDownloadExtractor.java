package test.runbycode.extractor;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.method.MethodArgAndCommonFieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.extractor.entry.CalleeGraphSPCFileDownloadExtractor;
import com.adrninistrator.jacg.handler.dto.field.CommonFieldInfoInClass;
import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/11/22
 * @description:
 */
public class MyCalleeGraphSPCFileDownloadExtractor extends CalleeGraphSPCFileDownloadExtractor {

    public static final String FIELD_NAME_FILE_INFO = "fileInfo";

    /**
     * 对（可能的）Spring Controller文件下载方法的处理，可重载进行自定义处理
     *
     * @param fileDownloadControllerFullMethodList
     * @param configureWrapper
     * @return
     */
    @Override
    protected List<String> handleFileDownloadControllerFullMethodList(List<String> fileDownloadControllerFullMethodList, ConfigureWrapper configureWrapper) {
        List<String> newMethodList = new ArrayList<>();

        try (MethodArgReturnHandler methodArgReturnHandler = new MethodArgReturnHandler(configureWrapper)) {
            // 对于（可能的）Spring Controller文件下载方法，仅处理方法参数、方法参数的类型中的字段、方法参数的泛型类型中的字段存在名称为 fileId 的方法
            for (String fileDownloadControllerFullMethod : fileDownloadControllerFullMethodList) {
                List<MethodArgAndCommonFieldInfo> methodArgAndCommonFieldInfoList = methodArgReturnHandler.queryMethodArgAndCommonFieldInfo(fileDownloadControllerFullMethod);
                // 判断指定的方法是否需要处理
                if (checkNeedHandleMethod(methodArgAndCommonFieldInfoList)) {
                    newMethodList.add(fileDownloadControllerFullMethod);
                }
            }
        }

        return newMethodList;
    }

    /**
     * 判断指定的方法是否需要处理
     *
     * @param methodArgAndCommonFieldInfoList
     * @return true: 需要处理 false: 不需要处理
     */
    private boolean checkNeedHandleMethod(List<MethodArgAndCommonFieldInfo> methodArgAndCommonFieldInfoList) {
        if (JavaCG2Util.isCollectionEmpty(methodArgAndCommonFieldInfoList)) {
            return false;
        }
        for (MethodArgAndCommonFieldInfo methodArgAndCommonFieldInfo : methodArgAndCommonFieldInfoList) {
            WriteDbData4MethodArgument methodArgument = methodArgAndCommonFieldInfo.getMethodArgument();
            List<CommonFieldInfoInClass> commonFieldInfoInClassList = methodArgAndCommonFieldInfo.getCommonFieldInfoInClassList();
            if (JavaCG2Util.isCollectionEmpty(commonFieldInfoInClassList)) {
                if (FIELD_NAME_FILE_INFO.equals(methodArgument.getArgName())) {
                    // 方法参数名称匹配
                    return true;
                }
                continue;
            }
            for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                WriteDbData4FieldInfo fieldInfo = commonFieldInfoInClass.getFieldInfo();
                if (FIELD_NAME_FILE_INFO.equals(fieldInfo.getFieldName())) {
                    // 方法参数的类型中的字段或方法参数的泛型类型中的字段名称匹配
                    return true;
                }
            }
        }
        return false;
    }
}
