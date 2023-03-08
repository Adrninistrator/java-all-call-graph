package com.adrninistrator.jacg.writer;

import com.adrninistrator.jacg.common.JACGConstants;
import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.Writer;

/**
 * @author adrninistrator
 * @date 2023/2/28
 * @description: 包装后的Writer，若未写入文件则不写入文件头（第一行），保持空文件
 */
public class WriterSupportHeader implements Closeable {
    private final Writer writer;

    private final String header;

    private boolean headWritten = false;

    public WriterSupportHeader(Writer writer, String header) {
        this.writer = writer;
        this.header = header;
    }

    @Override
    public void close() throws IOException {
        if (writer != null) {
            IOUtils.close(writer);
        }
    }

    public void writeLine(String data) throws IOException {
        if (!headWritten) {
            writer.write(header + JACGConstants.NEW_LINE);
            headWritten = true;
        }

        writer.write(data + JACGConstants.NEW_LINE);
    }
}
