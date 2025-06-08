package test.runbycode.parsedcustomdata.parser;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.AbstractCodeParserWithCustomData;
import org.apache.commons.lang3.StringUtils;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2025/6/6
 * @description:
 */
public class TestCodeParserWithCustomData1 extends AbstractCodeParserWithCustomData {

    private TestParser4XmlCommand testParser4XmlCommand;

    @Override
    public void initCodeParser() {
        testParser4XmlCommand = new TestParser4XmlCommand(this);
    }

    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_XML};
    }

    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        if (StringUtils.endsWithIgnoreCase(jarEntryPath, JACGConstants.EXT_XML)) {
            testParser4XmlCommand.handle(inputStream, jarEntryPath, jarEntryName);
        }
    }
}
