package test.runbycode.handler.entrymethodinfo;

import com.adrninistrator.jacg.dto.entrymethodinfo.BaseEntryMethodInfo;
import com.adrninistrator.jacg.dto.entrymethodinfo.EntryMethodInfo4SpringController;
import com.adrninistrator.jacg.dto.entrymethodinfo.EntryMethodInfo4SpringTask;
import com.adrninistrator.jacg.handler.entrymethodinfo.EntryMethodInfoFiller4Spring;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.methodcall.TestMCCaller;
import test.callgraph.spring.bean.use.springtask.TestSpringTask;
import test.callgraph.spring.mvc.TestSpringController2;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description:
 */
public class TestEntryMethodInfoFiller extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (EntryMethodInfoFiller4Spring entryMethodInfoFiller4Spring = new EntryMethodInfoFiller4Spring(configureWrapper, null)) {
            BaseEntryMethodInfo entryMethodInfo1 = entryMethodInfoFiller4Spring.query(TestMCCaller.class.getName() + ":test1a()", "void");
            Assert.assertNull(entryMethodInfo1);

            BaseEntryMethodInfo entryMethodInfo2 = entryMethodInfoFiller4Spring.query(TestSpringController2.class.getName() + ":get1()", String.class.getName());
            Assert.assertTrue(entryMethodInfo2 instanceof EntryMethodInfo4SpringController);

            BaseEntryMethodInfo entryMethodInfo3 = entryMethodInfoFiller4Spring.query(TestSpringTask.class.getName() + ":run1()", "void");
            Assert.assertTrue(entryMethodInfo3 instanceof EntryMethodInfo4SpringTask);
        }
    }
}
