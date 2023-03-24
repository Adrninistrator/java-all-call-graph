package com.adrninistrator.jacg.reporter.entry.spring;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethod;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethodCall;
import com.adrninistrator.jacg.handler.spring.SpringTxHandler;
import com.adrninistrator.jacg.reporter.common.JACGReportConstants;
import com.adrninistrator.jacg.reporter.entry.base.AbstractReporter;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileNotFoundException;
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
        FILE_HEADER_SP_INVALID_METHOD_CALL = StringUtils.joinWith(JACGConstants.FLAG_TAB,
                JACGReportConstants.COLUMN_CALLER_FULL_METHOD,
                JACGReportConstants.COLUMN_CALLER_LINE_NUMBER,
                "调用方法是否存在事务注解",
                "调用方法事务传播行为",
                JACGReportConstants.COLUMN_CALLEE_FULL_METHOD,
                "被调用方法事务传播行为"
        );

        FILE_HEADER_SP_INVALID_METHOD = StringUtils.joinWith(JACGConstants.FLAG_TAB,
                JACGReportConstants.COLUMN_FULL_METHOD,
                JACGReportConstants.COLUMN_METHOD_FLAGS
        );
    }

    public SpringInvalidTxMethodReporter() {
        super();
    }

    public SpringInvalidTxMethodReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean copyFileInSeparateDir, boolean skipWriteDb) {
        super(configureWrapper, reportDirPath, copyFileInSeparateDir, skipWriteDb);
    }

    /**
     * 生成Spring事务注解方法非法调用（调用当前实例的@Transactional注解方法）
     *
     * @return
     */
    public boolean generateInvalidMethodCallReport() {
        String reportDirPath = getReportDirPath();
        if (!reportDirPath.isEmpty() && !JACGFileUtil.isDirectoryExists(reportDirPath)) {
            return false;
        }

        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        WriterSupportHeader writerSupportHeader = null;
        // 查询Spring事务注解方法非法调用（调用当前实例的@Transactional注解方法）
        try (SpringTxHandler springTxHandler = new SpringTxHandler(configureWrapper)) {
            List<SpringInvalidTxAnnotationMethodCall> springInvalidTxAnnotationMethodCallList = springTxHandler.querySpringInvalidTxAnnotationMethodCall();
            if (JavaCGUtil.isCollectionEmpty(springInvalidTxAnnotationMethodCallList)) {
                return true;
            }

            // 生成Writer
            writerSupportHeader = genWriter4InvalidMethodCall();
            for (SpringInvalidTxAnnotationMethodCall springInvalidTxAnnotationMethodCall : springInvalidTxAnnotationMethodCallList) {
                List<String> stringList = new ArrayList<>();
                stringList.add(springInvalidTxAnnotationMethodCall.getCallerFullMethod());
                stringList.add(String.valueOf(springInvalidTxAnnotationMethodCall.getCallerLineNumber()));
                stringList.add(JavaCGYesNoEnum.parseDesc(springInvalidTxAnnotationMethodCall.isCallerWithSpringTx()));
                stringList.add(springInvalidTxAnnotationMethodCall.getCallerTxPropagation());
                stringList.add(springInvalidTxAnnotationMethodCall.getCalleeFullMethod());
                stringList.add(springInvalidTxAnnotationMethodCall.getCalleeTxPropagation());

                // 在报告文件写入一行
                writeData4InvalidMethodCall(writerSupportHeader, stringList);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        } finally {
            // 关闭Writer
            closeWriter4InvalidMethodCall(writerSupportHeader);
        }
        return true;
    }

    /**
     * 生成Spring事务注解方法非法调用对应报告文件的Writer，可重载
     *
     * @return
     * @throws FileNotFoundException
     */
    protected WriterSupportHeader genWriter4InvalidMethodCall() throws FileNotFoundException {
        String reportFilePath = genReportFilePath(FILE_NAME_SP_INVALID_METHOD_CALL);
        return new WriterSupportHeader(JavaCGFileUtil.genBufferedWriter(reportFilePath), FILE_HEADER_SP_INVALID_METHOD_CALL);
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
     * 关闭Spring事务注解方法非法调用对应报告文件的Writer，可重载
     *
     * @param writerSupportHeader
     */
    protected void closeWriter4InvalidMethodCall(WriterSupportHeader writerSupportHeader) {
        IOUtils.closeQuietly(writerSupportHeader);
    }

    /**
     * 生成Spring非法事务注解方法（@Transactional注解方法为private/protected、static、final方法）
     *
     * @return
     */
    public boolean generateInvalidMethodReport() {
        String reportDirPath = getReportDirPath();
        if (!reportDirPath.isEmpty() && !JACGFileUtil.isDirectoryExists(reportDirPath)) {
            return false;
        }

        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        WriterSupportHeader writerSupportHeader = null;
        // 查询Spring非法事务注解方法（@Transactional注解方法为private/protected、static、final方法）
        try (SpringTxHandler springTxHandler = new SpringTxHandler(configureWrapper)) {
            List<SpringInvalidTxAnnotationMethod> springInvalidTxAnnotationMethodList = springTxHandler.querySpringInvalidTxAnnotationMethod();
            if (JavaCGUtil.isCollectionEmpty(springInvalidTxAnnotationMethodList)) {
                return true;
            }

            // 生成Writer
            writerSupportHeader = genWriter4InvalidMethod();
            for (SpringInvalidTxAnnotationMethod springInvalidTxAnnotationMethod : springInvalidTxAnnotationMethodList) {
                List<String> stringList = new ArrayList<>();
                stringList.add(springInvalidTxAnnotationMethod.getFullMethod());
                stringList.add(springInvalidTxAnnotationMethod.getMethodFlagsDesc());

                // 在报告文件写入一行
                writeData4InvalidMethod(writerSupportHeader, stringList);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        } finally {
            // 关闭Writer
            closeWriter4InvalidMethod(writerSupportHeader);
        }
        return true;
    }

    /**
     * 生成Spring非法事务注解方法对应报告文件的Writer，可重载
     *
     * @return
     * @throws FileNotFoundException
     */
    protected WriterSupportHeader genWriter4InvalidMethod() throws FileNotFoundException {
        String reportFilePath = genReportFilePath(FILE_NAME_SP_INVALID_METHOD);
        return new WriterSupportHeader(JavaCGFileUtil.genBufferedWriter(reportFilePath), FILE_HEADER_SP_INVALID_METHOD);
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
     * 关闭Spring非法事务注解方法对应报告文件的Writer，可重载
     *
     * @param writerSupportHeader
     */
    protected void closeWriter4InvalidMethod(WriterSupportHeader writerSupportHeader) {
        IOUtils.closeQuietly(writerSupportHeader);
    }
}
