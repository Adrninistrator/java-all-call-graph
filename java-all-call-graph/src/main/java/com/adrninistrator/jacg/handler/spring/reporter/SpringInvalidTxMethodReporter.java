package com.adrninistrator.jacg.handler.spring.reporter;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.handler.common.JACGReportConstants;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethod;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethodCall;
import com.adrninistrator.jacg.handler.reporter.AbstractReporter;
import com.adrninistrator.jacg.handler.spring.SpringTxHandler;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: 生成非法的Spring事务注解方法信息报告
 */
public class SpringInvalidTxMethodReporter extends AbstractReporter {
    private static final Logger logger = LoggerFactory.getLogger(SpringInvalidTxMethodReporter.class);

    public static final String FILE_NAME_SP_INVALID_METHOD_CALL = "Spring事务注解方法调用非法——调用当前实例方法.md";
    public static final String FILE_NAME_SP_INVALID_METHOD = "Spring事务注解方法非法.md";

    public static final String FILE_HEADER_SP_INVALID_METHOD_CALL;
    public static final String FILE_HEADER_SP_INVALID_METHOD;

    static {
        FILE_HEADER_SP_INVALID_METHOD_CALL = StringUtils.joinWith(JavaCG2Constants.FLAG_TAB,
                JACGReportConstants.COLUMN_CALLER_FULL_METHOD,
                JACGReportConstants.COLUMN_CALLER_LINE_NUMBER,
                "调用方法是否存在事务注解",
                "调用方法事务传播行为",
                JACGReportConstants.COLUMN_CALLEE_FULL_METHOD,
                "被调用方法事务传播行为"
        );

        FILE_HEADER_SP_INVALID_METHOD = StringUtils.joinWith(JavaCG2Constants.FLAG_TAB,
                JACGReportConstants.COLUMN_FULL_METHOD,
                JACGReportConstants.COLUMN_METHOD_FLAGS
        );
    }

    public SpringInvalidTxMethodReporter(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile,
                                         boolean copyFileInSeparateDir) {
        super(javaCG2ConfigureWrapper, configureWrapper, reportDirPath, appendReportFile, copyFileInSeparateDir);
    }

    /**
     * 选择需要使用的文件头，Spring事务注解方法调用非法——调用当前实例方法，可重载
     *
     * @return
     */
    protected String chooseFileHeader4InvalidMethodCall() {
        return FILE_HEADER_SP_INVALID_METHOD_CALL;
    }

    /**
     * 选择需要使用的文件头，Spring事务注解方法非法，可重载
     *
     * @return
     */
    protected String chooseFileHeader4InvalidMethod() {
        return FILE_HEADER_SP_INVALID_METHOD;
    }

    /**
     * 在Spring事务注解方法非法调用对应报告文件中写入数据，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4InvalidMethodCall(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 在Spring非法事务注解方法对应报告文件中写入数据，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4InvalidMethod(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 生成Spring事务注解方法非法调用（调用当前实例的@Transactional注解方法）
     *
     * @return
     */
    public boolean generateInvalidMethodCallReport() {
        // 公共预处理，包含写数据库步骤
        if (!commonPreHandle()) {
            return false;
        }

        // 查询Spring事务注解方法非法调用（调用当前实例的@Transactional注解方法）
        String reportFilePath = genReportFilePath(FILE_NAME_SP_INVALID_METHOD_CALL);
        try (SpringTxHandler springTxHandler = new SpringTxHandler(configureWrapper);
             WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader4InvalidMethodCall(), appendReportFile)) {
            List<SpringInvalidTxAnnotationMethodCall> springInvalidTxAnnotationMethodCallList = springTxHandler.querySpringInvalidTxAnnotationMethodCall();
            if (JavaCG2Util.isCollectionEmpty(springInvalidTxAnnotationMethodCallList)) {
                return true;
            }

            for (SpringInvalidTxAnnotationMethodCall springInvalidTxAnnotationMethodCall : springInvalidTxAnnotationMethodCallList) {
                List<String> stringList = new ArrayList<>();
                stringList.add(springInvalidTxAnnotationMethodCall.getCallerFullMethod());
                stringList.add(String.valueOf(springInvalidTxAnnotationMethodCall.getCallerLineNumber()));
                stringList.add(JavaCG2YesNoEnum.parseDesc(springInvalidTxAnnotationMethodCall.isCallerWithSpringTx()));
                stringList.add(springInvalidTxAnnotationMethodCall.getCallerTxPropagation());
                stringList.add(springInvalidTxAnnotationMethodCall.getCalleeFullMethod());
                stringList.add(springInvalidTxAnnotationMethodCall.getCalleeTxPropagation());

                // 在报告文件写入一行
                writeData4InvalidMethodCall(writerSupportHeader, stringList);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
        return true;
    }

    /**
     * 生成Spring非法事务注解方法（@Transactional注解方法为private/protected、static、final方法）
     *
     * @return
     */
    public boolean generateInvalidMethodReport() {
        // 公共预处理，包含写数据库步骤
        if (!commonPreHandle()) {
            return false;
        }

        // 查询Spring非法事务注解方法（@Transactional注解方法为private/protected、static、final方法）
        String reportFilePath = genReportFilePath(FILE_NAME_SP_INVALID_METHOD);
        try (SpringTxHandler springTxHandler = new SpringTxHandler(configureWrapper);
             WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader4InvalidMethod(), appendReportFile)) {
            List<SpringInvalidTxAnnotationMethod> springInvalidTxAnnotationMethodList = springTxHandler.querySpringInvalidTxAnnotationMethod();
            if (JavaCG2Util.isCollectionEmpty(springInvalidTxAnnotationMethodList)) {
                return true;
            }

            for (SpringInvalidTxAnnotationMethod springInvalidTxAnnotationMethod : springInvalidTxAnnotationMethodList) {
                List<String> stringList = new ArrayList<>();
                stringList.add(springInvalidTxAnnotationMethod.getFullMethod());
                stringList.add(springInvalidTxAnnotationMethod.getMethodFlagsDesc());

                // 在报告文件写入一行
                writeData4InvalidMethod(writerSupportHeader, stringList);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
        return true;
    }
}
