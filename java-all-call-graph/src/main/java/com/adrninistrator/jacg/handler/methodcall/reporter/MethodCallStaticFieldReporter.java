package com.adrninistrator.jacg.handler.methodcall.reporter;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticField;
import com.adrninistrator.jacg.handler.common.JACGReportConstants;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithStaticField;
import com.adrninistrator.jacg.handler.methodcall.MethodCallStaticFieldHandler;
import com.adrninistrator.jacg.handler.reporter.AbstractReporter;
import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description: 生成方法调用使用静态字段信息报告，可用于获取枚举的使用情况
 */
public class MethodCallStaticFieldReporter extends AbstractReporter {
    private static final Logger logger = LoggerFactory.getLogger(MethodCallStaticFieldReporter.class);

    public static final String METHOD_CALL_STATIC_FIELD_USAGE_REPORT = "方法调用中静态字段使用情况.md";

    public static final String[] FILE_HEADER_STATIC_FIELD_ARRAY = new String[]{
            JACGReportConstants.COLUMN_CALLER_FULL_METHOD,
            JACGReportConstants.COLUMN_CALLER_LINE_NUMBER,
            JACGReportConstants.COLUMN_CALLEE_FULL_METHOD,
            "静态字段在方法调用中的使用方式",
            "静态字段所在的类名",
            "静态字段名称"
    };

    public static final String FILE_HEADER_STATIC_FIELD = StringUtils.join(FILE_HEADER_STATIC_FIELD_ARRAY, JavaCGConstants.FLAG_TAB);

    public MethodCallStaticFieldReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile, boolean skipWriteDb) {
        super(configureWrapper, reportDirPath, appendReportFile, skipWriteDb);
    }

    /**
     * 选择需要使用的文件头，可重载
     *
     * @return
     */
    protected String chooseFileHeader() {
        return FILE_HEADER_STATIC_FIELD;
    }

    /**
     * 在方法调用使用静态字段信息报告文件中写入数据，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4MethodCallStaticField(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 生成报告
     *
     * @param className
     * @param fieldNames
     * @return
     */
    public boolean generate(String className, String... fieldNames) {
        // 公共预处理，包含写数据库步骤
        if (!commonPreHandle()) {
            return false;
        }

        String reportFilePath = genReportFilePath(METHOD_CALL_STATIC_FIELD_USAGE_REPORT);
        try (MethodCallStaticFieldHandler methodCallStaticFieldHandler = new MethodCallStaticFieldHandler(configureWrapper);
             WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader(), appendReportFile)) {
            // 查询在方法调用中使用了指定类中的静态字段的信息，包含对应的方法调用
            List<MethodCallWithStaticField> methodCallWithStaticFieldList = methodCallStaticFieldHandler.queryMethodCallWithStaticFieldList(className, fieldNames);
            if (JavaCGUtil.isCollectionEmpty(methodCallWithStaticFieldList)) {
                logger.info("未查询到查询在方法调用中使用了指定类中的静态字段的信息 {} {}", className, StringUtils.join(fieldNames, " "));
                return true;
            }

            for (MethodCallWithStaticField methodCallWithStaticField : methodCallWithStaticFieldList) {
                WriteDbData4MethodCall methodCall = methodCallWithStaticField.getMethodCall();
                WriteDbData4MethodCallStaticField methodCallStaticField = methodCallWithStaticField.getMethodCallStaticField();

                List<String> stringList = new ArrayList<>();
                stringList.add(methodCall.getCallerFullMethod());
                stringList.add(String.valueOf(methodCall.getCallerLineNumber()));
                stringList.add(methodCall.getCalleeFullMethod());
                stringList.add(JACGMethodCallInfoUtil.genObjArgDesc(methodCallStaticField.getObjArgsSeq()));
                stringList.add(methodCallStaticField.getClassName());
                stringList.add(methodCallStaticField.getFieldName());

                // 在报告文件写入一行
                writeData4MethodCallStaticField(writerSupportHeader, stringList);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }
}
