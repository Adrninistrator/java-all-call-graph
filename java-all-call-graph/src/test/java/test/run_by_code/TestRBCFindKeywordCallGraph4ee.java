package test.run_by_code;

import com.adrninistrator.jacg.find_keyword.FindKeywordCallGraph;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCFindKeywordCallGraph4ee extends TestRunByCodeBase {
    @Test
    public void test() {
        new FindKeywordCallGraph().find(true, configureWrapper);
    }
}
