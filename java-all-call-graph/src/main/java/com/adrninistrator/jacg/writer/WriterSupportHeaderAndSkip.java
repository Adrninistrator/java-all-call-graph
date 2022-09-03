package com.adrninistrator.jacg.writer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;

/**
 * @author adrninistrator
 * @date 2022/8/26
 * @description: 写文件前支持先写入文件头，若未写入过文件内容，则不创建文件
 */
public class WriterSupportHeaderAndSkip implements Closeable {
    private static final Logger logger = LoggerFactory.getLogger(WriterSupportHeaderAndSkip.class);

    private String filePath;

    private String fileHeader;

    private Writer writer;

    private boolean firstTime = true;

    public static WriterSupportHeaderAndSkip genWriterSupportHeaderAndSkip(String filePath, String fileHeader) {
        WriterSupportHeaderAndSkip writerSupportHeaderAndSkip = new WriterSupportHeaderAndSkip();
        writerSupportHeaderAndSkip.setFilePath(filePath);
        writerSupportHeaderAndSkip.setFileHeader(fileHeader);
        return writerSupportHeaderAndSkip;
    }

    public void write(String data) throws IOException {
        if (firstTime) {
            firstTime = false;
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filePath), StandardCharsets.UTF_8));
            writer.write(fileHeader);
        }

        if (writer == null) {
            return;
        }

        writer.write(data);
    }

    @Override
    public void close() {
        if (writer == null) {
            return;
        }

        try {
            writer.flush();
            writer.close();
        } catch (IOException e) {
            logger.error("error ", e);
        }
    }
    //

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setFileHeader(String fileHeader) {
        this.fileHeader = fileHeader;
    }

    public boolean isFirstTime() {
        return firstTime;
    }
}
