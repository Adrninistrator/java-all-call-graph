package test.other;

import com.adrninistrator.jacg.markdown.enums.MDCodeBlockTypeEnum;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import org.junit.Test;

import java.io.IOException;

/**
 * @author adrninistrator
 * @date 2022/11/27
 * @description:
 */
public class TestMd {
    @Test
    public void test() {
        try (MarkdownWriter markdownWriter1 = new MarkdownWriter("build/test-1.md", false);
             MarkdownWriter markdownWriter2 = new MarkdownWriter("build/test-2.md", true)) {
            doTest(markdownWriter1);
            doTest(markdownWriter2);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void doTest(MarkdownWriter markdownWriter) throws IOException {
        markdownWriter.addTitle(1, "@");
        markdownWriter.addTitle(1, "@");

        markdownWriter.addCodeBlock(MDCodeBlockTypeEnum.MDCBTE_SQL);
        markdownWriter.addLine("select");
        markdownWriter.addCodeBlock();

        markdownWriter.addTitle(2, "@");

        markdownWriter.addList("1");
        markdownWriter.addList("2");
        markdownWriter.addList("3");

        markdownWriter.addCodeBlock();
        markdownWriter.addLine("test1");
        markdownWriter.addLine("test2");
        markdownWriter.addCodeBlock();

        markdownWriter.addTitle(2, "@");

        markdownWriter.addLineWithNewLine("@");
        markdownWriter.addLineWithNewLine("@");
        markdownWriter.addLineWithNewLine("@");

        markdownWriter.addTitle(2, "@");
        markdownWriter.addTitle(2, "@");
        markdownWriter.addTitle(3, "@");
        markdownWriter.addTitle(3, "@");
        markdownWriter.addTitle(4, "@");
        markdownWriter.addTitle(5, "@");
        markdownWriter.addTitle(2, "@");
        markdownWriter.addTitle(1, "@");
        markdownWriter.addTitle(2, "@");
        markdownWriter.addTitle(3, "@");
        markdownWriter.addTitle(4, "@");
        markdownWriter.addTitle(5, "@");
        markdownWriter.addTitle(1, "@");
        markdownWriter.addTitle(2, "@");
        markdownWriter.addTitle(3, "@");
        markdownWriter.addTitle(4, "@");
        markdownWriter.addTitle(3, "@");
        markdownWriter.addTitle(2, "@");
        markdownWriter.addTitle(1, "@");
    }
}
