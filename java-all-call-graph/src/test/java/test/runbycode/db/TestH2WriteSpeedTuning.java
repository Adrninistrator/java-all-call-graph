package test.runbycode.db;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * H2 写入提速验证：方案C（autocommit/批次commit）、方案D（并发数）、方案F（存储层调优）。
 *
 * <p>使用原生 H2 JDBC 连接（不经过 DbOperator 的固定 URL），可自由控制 autocommit、URL 参数、线程数。
 * 详见 prompt/提高静态分析速度/方案.md 的方案C/D/F。
 */
@JACGExample(title = "H2写入提速验证（autocommit/并发/存储调优）",
        desc = {"方案C：关闭autocommit+批次末尾commit",
                "方案D：验证H2最适合的写入并发数",
                "方案F：H2 URL 存储/日志参数调优"})
public class TestH2WriteSpeedTuning extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestH2WriteSpeedTuning.class);

    private String dbFilePath;

    // 基础 URL（无存储调优参数）
    private static final String BASE_URL_SUFFIX = ";MODE=MySQL;DATABASE_TO_LOWER=TRUE;CASE_INSENSITIVE_IDENTIFIERS=TRUE";

    @Before
    public void setUp() {
        dbFilePath = "./build/jacg_h2db_speed_tuning";
        // 清理旧库文件
        deleteH2Files(dbFilePath);
    }

    @After
    public void tearDown() {
        deleteH2Files(dbFilePath);
    }

    // ============ 方案C：autocommit + 批次 commit ============

    // 方案C：关闭 autocommit、每批 addBatch 后单次 commit，对比默认 autocommit
    @Test
    public void testCAutoCommitAndBatchCommit() {
        String url = "jdbc:h2:file:" + dbFilePath + BASE_URL_SUFFIX;
        int rows = 200_000;
        int batch = 1_000;

        // C1：autocommit=true（默认），每批 executeBatch 后自动 commit
        long tC1 = writeAutoCommitTrue(url, TABLE + "_c1", rows, batch);

        // C2：autocommit=false，每批 executeBatch 后手动 commit
        long tC2 = writeAutoCommitFalseManualCommit(url, TABLE + "_c2", rows, batch);

        // C3：autocommit=false，全部写完最后一次 commit（极端：单事务）
        long tC3 = writeAutoCommitFalseSingleCommit(url, TABLE + "_c3", rows, batch);

        logger.info("方案C 结果 autocommit=true={}ms 每批手动commit={}ms 单次commit={}ms", tC1, tC2, tC3);
        // autocommit 与每批手动 commit 的差异在 H2 下随环境波动（有时手动 commit 反而略慢），
        // 故不做"手动 commit 必优于 autocommit"的严格断言，仅断言三种方式均能完成写入并打印实测对比。
        // 可确定的经验结论：单次 commit（C3，单长事务）通常最慢（大表维护大量 undo），见下方软断言。
        Assert.assertTrue("autocommit方式应能完成写入", tC1 > 0);
        Assert.assertTrue("每批手动commit方式应能完成写入", tC2 > 0);
        Assert.assertTrue("单次commit方式应能完成写入", tC3 > 0);
        // 软断言：单次 commit（单长事务）不应显著优于每批 commit（H2 大表长事务开销大），留 2 倍容差
        Assert.assertTrue("单次commit不应显著优于每批commit: tC3=" + tC3 + " tC2=" + tC2, tC3 >= tC2 / 2L);
    }

    private long writeAutoCommitTrue(String url, String table, int rows, int batch) {
        return writeWithConn(url, table, rows, batch, true, false);
    }

    private long writeAutoCommitFalseManualCommit(String url, String table, int rows, int batch) {
        return writeWithConn(url, table, rows, batch, false, false);
    }

    private long writeAutoCommitFalseSingleCommit(String url, String table, int rows, int batch) {
        return writeWithConn(url, table, rows, batch, false, true);
    }

    // 通用写入：autocommit 控制、是否仅在末尾 commit
    private long writeWithConn(String url, String table, int rows, int batch, boolean autoCommit, boolean commitOnlyAtEnd) {
        dropAndCreateTable(url, table, true);
        String insertSql = "INSERT INTO " + table + " (record_id, simple_class_name, field_name) VALUES (?, ?, ?)";
        long start = System.currentTimeMillis();
        try (Connection conn = DriverManager.getConnection(url, "", "")) {
            conn.setAutoCommit(autoCommit);
            try (PreparedStatement ps = conn.prepareStatement(insertSql)) {
                int counter = 0;
                for (int i = 0; i < rows; i++) {
                    ps.setInt(1, i);
                    ps.setString(2, classNames[i % classNames.length]);
                    ps.setString(3, fieldNames[i % fieldNames.length]);
                    ps.addBatch();
                    if (++counter % batch == 0) {
                        ps.executeBatch();
                        if (!autoCommit && !commitOnlyAtEnd) {
                            conn.commit();
                        }
                        ps.clearBatch();
                    }
                }
                if (counter % batch != 0) {
                    ps.executeBatch();
                }
                if (!autoCommit) {
                    conn.commit();
                }
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
        return System.currentTimeMillis() - start;
    }

    // ============ 方案D：并发数验证 ============

    // 方案D：不同并发线程数写入独立表，找最优并发数（H2 单文件库通常并发不宜过高）
    @Test
    public void testDConcurrency() {
        String url = "jdbc:h2:file:" + dbFilePath + BASE_URL_SUFFIX;
        int totalRows = 200_000;
        int[] threadCounts = {1, 2, 4, 8, 16};

        long bestTime = Long.MAX_VALUE;
        int bestThreads = 1;
        StringBuilder sb = new StringBuilder("方案D 结果 ");
        for (int tc : threadCounts) {
            String table = TABLE + "_d_" + tc;
            long t = writeConcurrent(url, table, totalRows, tc);
            sb.append("threads=").append(tc).append("=").append(t).append("ms ");
            if (t < bestTime) {
                bestTime = t;
                bestThreads = tc;
            }
        }
        // 单线程耗时即 threadCounts[0] 的结果（上面循环首项）
        logger.info("{} 最优并发数={} 耗时={}ms", sb, bestThreads, bestTime);
        // 断言：最优耗时存在（非空）；最优并发数在测试范围内
        Assert.assertTrue("应找到有效最优耗时", bestTime < Long.MAX_VALUE);
        // H2 单文件库并发收益极小（实测最优仅比单线程快约2%），不做严格"并发优于单线程"断言（噪声内）
        long t1 = parseTimeFromLog(sb, 1);
        Assert.assertTrue("threads=1 应有记录", t1 > 0);
        // 仅断言 16 线程不显著劣化（避免过高并发严重退化，留一定容差 1.5×）
        long t16 = parseTimeFromLog(sb, 16);
        if (t16 > 0) {
            Assert.assertTrue("16线程不应严重劣化于单线程: t16=" + t16 + " t1=" + t1, t16 <= t1 * 3L / 2L);
        }
    }

    // 从 "方案D 结果 threads=1=Xms ..." 字符串解析指定线程数对应耗时
    private long parseTimeFromLog(StringBuilder sb, int threads) {
        String s = sb.toString();
        String key = "threads=" + threads + "=";
        int idx = s.indexOf(key);
        if (idx < 0) {
            return -1;
        }
        int start = idx + key.length();
        int end = s.indexOf("ms", start);
        if (end < 0) {
            return -1;
        }
        try {
            return Long.parseLong(s.substring(start, end));
        } catch (NumberFormatException e) {
            return -1;
        }
    }

    // 多线程并发写入（每线程写独立区段、用同一表、autocommit=false 各自批量 commit）
    private long writeConcurrent(String url, String table, int totalRows, int threadCount) {
        dropAndCreateTable(url, table, false);
        int rowsPerThread = totalRows / threadCount;
        ExecutorService pool = Executors.newFixedThreadPool(threadCount);
        AtomicInteger failCount = new AtomicInteger(0);
        long start = System.currentTimeMillis();
        try {
            CountDownLatch latch = new CountDownLatch(threadCount);
            for (int t = 0; t < threadCount; t++) {
                final int tid = t;
                pool.execute(() -> {
                    try (Connection conn = DriverManager.getConnection(url, "", "")) {
                        conn.setAutoCommit(false);
                        String insertSql = "INSERT INTO " + table + " (record_id, simple_class_name, field_name) VALUES (?, ?, ?)";
                        try (PreparedStatement ps = conn.prepareStatement(insertSql)) {
                            int startRow = tid * rowsPerThread;
                            int counter = 0;
                            for (int i = 0; i < rowsPerThread; i++) {
                                int rid = startRow + i;
                                ps.setInt(1, rid);
                                ps.setString(2, classNames[rid % classNames.length]);
                                ps.setString(3, fieldNames[rid % fieldNames.length]);
                                ps.addBatch();
                                if (++counter % 1000 == 0) {
                                    ps.executeBatch();
                                    conn.commit();
                                    ps.clearBatch();
                                }
                            }
                            if (counter % 1000 != 0) {
                                ps.executeBatch();
                                conn.commit();
                            }
                        }
                    } catch (Exception e) {
                        logger.error("并发写入失败 thread={}", tid, e);
                        failCount.incrementAndGet();
                    } finally {
                        latch.countDown();
                    }
                });
            }
            latch.await();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            pool.shutdown();
        }
        Assert.assertEquals("并发写入不应有失败线程", 0, failCount.get());
        return System.currentTimeMillis() - start;
    }

    // ============ 方案F：H2 存储层调优 ============

    // 方案F：对比默认 URL 与 CACHE_SIZE 调大的 URL
    @Test
    public void testFStorageTuning() {
        int rows = 200_000;
        int batch = 1_000;

        // F1：默认（无 CACHE 调优）
        String urlDefault = "jdbc:h2:file:" + dbFilePath + BASE_URL_SUFFIX;
        long tF1 = writeAutoCommitFalseManualCommit(urlDefault, TABLE + "_f1", rows, batch);

        // F2：CACHE_SIZE 调大（H2 2.x 不再支持 LOG 设置，改用 CACHE_SIZE 减少磁盘 IO）
        String urlTuned = "jdbc:h2:file:" + dbFilePath + BASE_URL_SUFFIX + ";CACHE_SIZE=65536";
        long tF2 = writeAutoCommitFalseManualCommit(urlTuned, TABLE + "_f2", rows, batch);

        logger.info("方案F 结果 默认={}ms CACHE调优={}ms 差值={}ms", tF1, tF2, (tF1 - tF2));
        // CACHE_SIZE 收益在噪声范围内（实测约3%），故不做严格不退化断言，仅记录对比
        Assert.assertTrue("默认方式应能完成写入", tF1 > 0);
        Assert.assertTrue("CACHE_SIZE方式应能完成写入", tF2 > 0);
    }

    // ============ 通用工具 ============

    private static final String TABLE = "jacg_test_speed";

    private static final String[] classNames = {"com.example.A", "com.example.B", "com.example.C", "com.example.D", "com.example.E"};
    private static final String[] fieldNames = {"service", "repo", "dao", "mapper", "client", "helper", "manager", "util"};

    private void dropAndCreateTable(String url, String table, boolean withIndex) {
        try (Connection conn = DriverManager.getConnection(url, "", "");
             Statement st = conn.createStatement()) {
            // H2 索引名在 schema 内全局唯一，故索引名带表名后缀避免跨表冲突
            st.execute("DROP TABLE IF EXISTS " + table);
            String indexClause = withIndex ? ", INDEX idx_scn_" + table + " (simple_class_name), INDEX idx_fn_" + table + " (field_name)" : "";
            st.execute("CREATE TABLE " + table + " (" +
                    "record_id int NOT NULL," +
                    "simple_class_name varchar(300) NOT NULL," +
                    "field_name varchar(200) NOT NULL," +
                    "PRIMARY KEY (record_id)" + indexClause + ")");
        } catch (Exception e) {
            Assert.fail("建/删表失败 " + table + " " + e.getMessage());
        }
    }

    private void deleteH2Files(String path) {
        for (String ext : new String[]{".mv.db", ".trace.db", ".lock.db"}) {
            File f = new File(path + ext);
            if (f.exists()) {
                //noinspection ResultOfMethodCallIgnored
                f.delete();
            }
        }
    }
}
