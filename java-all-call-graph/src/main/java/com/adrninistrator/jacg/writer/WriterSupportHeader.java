package com.adrninistrator.jacg.writer;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Writer;

/**
 * @author adrninistrator
 * @date 2023/2/28
 * @description: 包装后的Writer，若未写入文件则不写入文件头（第一行），保持空文件
 */
public class WriterSupportHeader implements Closeable {
    private static final Logger logger = LoggerFactory.getLogger(WriterSupportHeader.class);

    private final Writer writer;

    private final String header;

    /*
        记录当前文件是否为第一次写入，用于判断是否需要写入文件头（可能在处理多个jar包后将结果写到同一个文件中，第一次使用非追加模式，之后使用追加模式）
        假如Writer使用非追加模式创建，则当前对象创建时属于第一次写入
        假如Writer使用追加模式创建，则再判断文件大小，若文件不存在或大小为0，则当前对象创建时属于第一次写入
     */
    private boolean firstTime = false;

    /**
     * @param filePath 需要生成的文件路径
     * @param header   文件头（第一行）内容
     * @param append   true: 在文件后面追加（不写入文件头） false: 覆盖已有文件，且（若需要则）写入文件头
     */
    public WriterSupportHeader(String filePath, String header, boolean append) throws FileNotFoundException {
        if (!append) {
            // Writer使用非追加模式创建
            firstTime = true;
            logger.info("使用非追加模式写文件 {}", filePath);
        } else {
            // Writer使用追加模式创建
            File file = new File(filePath);
            if (!file.exists()) {
                logger.info("使用追加模式写文件，文件还不存在 {}", filePath);
                firstTime = true;
            } else {
                if (file.isDirectory()) {
                    logger.error("需要写入的文件存在同名目录 {}", filePath);
                    throw new FileNotFoundException("需要写入的文件存在同名目录 " + filePath);
                }

                if (file.length() == 0) {
                    logger.info("使用追加模式写文件，文件大小为0 {}", filePath);
                    firstTime = true;
                } else {
                    logger.info("使用追加模式写文件，文件存在且非空 {}", filePath);
                }
            }
        }
        this.writer = JavaCG2FileUtil.genBufferedWriter(filePath, append);
        this.header = header;
    }

    @Override
    public void close() throws IOException {
        if (writer != null) {
            IOUtils.close(writer);
        }
    }

    public void writeLine(String data) throws IOException {
        if (firstTime) {
            // 向文件中写入文件头
            writer.write(header + JavaCG2Constants.NEW_LINE);
            firstTime = false;
        }

        writer.write(data + JavaCG2Constants.NEW_LINE);
    }
}
