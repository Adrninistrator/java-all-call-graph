package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.IndexTypeEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.common.enums.SqlTypeEnum;
import com.adrninistrator.jacg.comparator.Comparator4FullMethodWithReturnType;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.callgraph.CallGraphNode4Caller;
import com.adrninistrator.jacg.dto.db.QueryDataResult;
import com.adrninistrator.jacg.dto.db.SqlInfo;
import com.adrninistrator.jacg.dto.db.TableColumnInfo;
import com.adrninistrator.jacg.dto.db.TableIndexInfo;
import com.adrninistrator.jacg.dto.db.TableStructureInfo;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.alibaba.druid.DbType;
import com.alibaba.druid.sql.SQLUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2022/8/23
 * @description: 数据库公共操作封装对象
 */
public class DbOperWrapper {
    private static final Logger logger = LoggerFactory.getLogger(DbOperWrapper.class);

    protected static final AtomicInteger ATOMIC_INTEGER = new AtomicInteger(0);

    // 预编译SQL语句缓存，不能使用静态字段，否则多个任务之间会相互影响
    private final Map<String, String> sqlCacheMap = new ConcurrentHashMap<>();

    /*
        类名相同但包名不同的类名Map
        key     表名后缀
        value   类名相同但包名不同的类名Set
     */
    protected final Map<String, Set<String>> duplicateSimpleClassNameMap = new HashMap<>();

    protected ConfigureWrapper configureWrapper;

    protected DbOperator dbOperator;

    protected String appName;

    protected String tableSuffix;

    protected String objSeq;

    protected int dbInsertBatchSize;

    // 类名相同但包名不同的类名Set
    protected Set<String> duplicateSimpleClassNameSet = null;

    /*
        key: 任务中指定的类名，可能是简单类名或完整类名
        value: 对应的唯一类名
     */
    protected Map<String, String> simpleClassNameInTaskMap = new HashMap<>();

    protected DbOperWrapper() {
    }

    DbOperWrapper(ConfigureWrapper configureWrapper, DbOperator dbOperator) {
        this.configureWrapper = configureWrapper;
        this.dbOperator = dbOperator;
        appName = dbOperator.getAppName();
        tableSuffix = dbOperator.getTableSuffix();

        objSeq = "dbwo@" + ATOMIC_INTEGER.incrementAndGet();
        dbInsertBatchSize = dbOperator.getDbInsertBatchSize();
        logger.info("objSeq [{}]", objSeq);
    }

    private String genSqlKey(String sqlKey, int num) {
        if (num == 0) {
            return sqlKey;
        }
        return sqlKey + JACGConstants.FLAG_AT + num;
    }

    private String getCachedSql(String sqlKey, int num) {
        return sqlCacheMap.get(genSqlKey(sqlKey, num));
    }

    /**
     * 获取缓存的sql语句，key使用枚举，参数数量可变
     *
     * @param sqlKeyEnum
     * @param num        参数数量
     * @return
     */
    public String getCachedSql(SqlKeyEnum sqlKeyEnum, int num) {
        return getCachedSql(String.valueOf(sqlKeyEnum.ordinal()), num);
    }

    /**
     * 获取缓存的sql语句，key使用字符串，参数数量固定为0
     *
     * @param sqlKey
     * @return
     */
    public String getCachedSql(String sqlKey) {
        return getCachedSql(sqlKey, 0);
    }

    /**
     * 获取缓存的sql语句，key使用枚举，参数数量固定为0
     *
     * @param sqlKeyEnum
     * @return
     */
    public String getCachedSql(SqlKeyEnum sqlKeyEnum) {
        return getCachedSql(String.valueOf(sqlKeyEnum.ordinal()));
    }

    /**
     * 获取缓存的sql语句，key使用枚举，参数数量固定为0，支持表名后缀
     *
     * @param sqlKeyEnum
     * @param tableSuffix
     * @return
     */
    public String getCachedSqlWithSuffix(SqlKeyEnum sqlKeyEnum, String tableSuffix) {
        return getCachedSql(sqlKeyEnum.ordinal() + " " + tableSuffix);
    }

    private String cacheSql(String sqlKey, String sql, String sqlKey4Print, int num) {
        // 根据sql语句的key与参数数量，生成最终的key
        String finalSqlKey = genSqlKey(sqlKey, num);

        // 替换sql语句中的appName
        String finalSql = JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
        if (sqlCacheMap.putIfAbsent(finalSqlKey, finalSql) == null) {
            // 假如有指定用于在日志中打印的key，则在日志中打印出来
            logger.info("[{}] cache sql: [{} {}] [{}]", objSeq, finalSqlKey, sqlKey4Print, finalSql);
        }
        return finalSql;
    }

    /**
     * 缓存并格式化sql，key使用枚举，参数数量可变
     *
     * @param sqlKeyEnum
     * @param sql
     * @param num        参数数量
     */
    public String cacheSql(SqlKeyEnum sqlKeyEnum, String sql, int num) {
        return cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name(), num);
    }

    /**
     * 缓存并格式化sql，key使用枚举，参数数量可变，支持指定用于在日志中打印的sql的key
     *
     * @param sqlKey
     * @param sql
     * @param key4Print
     * @return
     */
    public String cacheSql(String sqlKey, String sql, String key4Print) {
        return cacheSql(sqlKey, sql, key4Print, 0);
    }

    /**
     * 缓存并格式化sql，key使用枚举，参数数量固定为0
     *
     * @param sqlKeyEnum
     * @param sql
     */
    public String cacheSql(SqlKeyEnum sqlKeyEnum, String sql) {
        return cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name());
    }

    /**
     * 缓存并格式化sql，key使用枚举，参数数量固定为0，，支持表名后缀
     *
     * @param sqlKeyEnum
     * @param sql
     * @param tableSuffix
     */
    public String cacheSqlWithSuffix(SqlKeyEnum sqlKeyEnum, String sql, String tableSuffix) {
        return cacheSql(genSqlKeyWithSuffix(sqlKeyEnum, tableSuffix), sql, sqlKeyEnum.name() + tableSuffix);
    }

    private String genSqlKeyWithSuffix(SqlKeyEnum sqlKeyEnum, String tableSuffix) {
        return sqlKeyEnum.ordinal() + " " + tableSuffix;
    }

    /**
     * 格式化sql语句，适用于不需要缓存的sql语句，打印sql语句
     *
     * @param sql 格式化前的sql语句
     * @return 格式化后的sql语句
     */
    public String formatSql(String sql) {
        return formatSql(sql, true);
    }

    /**
     * 格式化sql语句，适用于不需要缓存的sql语句
     *
     * @param sql     格式化前的sql语句
     * @param showSql 是否打印sql语句
     * @return 格式化后的sql语句
     */
    public String formatSql(String sql, boolean showSql) {
        // 替换sql语句中的appName
        String finalSql = JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
        if (showSql) {
            logger.info("[{}] format sql: [{}]", objSeq, finalSql);
        }
        return finalSql;
    }

    /**
     * 生成用于插入数据库的sql语句并缓存
     *
     * @param dbTableInfoEnum
     * @param dbInsertMode
     * @return
     */
    public String genAndCacheInsertSql(DbTableInfoEnum dbTableInfoEnum, DbInsertMode dbInsertMode) {
        String key = dbTableInfoEnum.getInsertSqlKey();
        String sql = getCachedSql(key);
        if (sql == null) {
            String[] columns = dbTableInfoEnum.getColumns();
            sql = dbInsertMode.getMode() + dbTableInfoEnum.getTableName() + JACGSqlUtil.genColumnString(columns) + " values " + JACGSqlUtil.genQuestionString(columns.length);
            sql = cacheSql(key, sql, dbTableInfoEnum.getTableNameKeyword());
        }
        return sql;
    }

    /**
     * 查找类名相同但包名不同的类，使用ThreadLocal中的表名后缀
     *
     * @return
     */
    public boolean findDuplicateClass() {
        return findDuplicateClass(tableSuffix);
    }

    /**
     * 查找类名相同但包名不同的类，使用指定的表名后缀
     *
     * @param tableSuffix
     * @return
     */
    public boolean findDuplicateClass(String tableSuffix) {
        logger.info("查找类名相同但包名不同的类 {}", Objects.toString(tableSuffix, ""));
        Set<String> usedDuplicateSimpleClassNameSet;

        if (StringUtils.isBlank(tableSuffix)) {
            // 表名后缀为null，使用固定的Set对象
            duplicateSimpleClassNameSet = new HashSet<>();
            usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameSet;
        } else {
            // 表名后缀为null，使用Map中的Set对象
            usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameMap.computeIfAbsent(tableSuffix, k -> new HashSet<>());
        }
        // 执行查找类名与唯一类名相同的唯一类名
        List<String> list = doFindDuplicateClass(tableSuffix);
        if (list == null) {
            return false;
        }
        if (list.isEmpty()) {
            return true;
        }

        for (String simpleClassName : list) {
            String duplicateSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(simpleClassName);
            usedDuplicateSimpleClassNameSet.add(duplicateSimpleClassName);
        }
        logger.info("找到类名相同但包名不同的类 {}", StringUtils.join(usedDuplicateSimpleClassNameSet, " "));
        return true;
    }

    // 执行查找类名与唯一类名相同的唯一类名
    protected List<String> doFindDuplicateClass(String tableSuffix) {
        // 以下sql语句不能缓存，因为可能被不同的表名后缀使用
        String sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName(appName, tableSuffix) +
                " where " + DC.CN_DUPLICATE_CLASS + " = ?";
        String finalSql = formatSql(sql);
        return dbOperator.queryListOneColumn(finalSql, String.class, JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 判断指定的简单类名是否属于同名类（类名相同但包名不同的类）
     * <p>
     * 用于查询方法调用关系时，识别简单类名输入是否存在同名类歧义：
     * 同名类在 method_call 的 simple_class_name 列中按完整类名存储，简单类名输入会查不到，需提示使用完整类名。
     * 依赖 findDuplicateClass() 填充的同名类集合（查 class_name.duplicate_class 标志），
     * 不能使用 findDuplicateClassBeforeUpdate()（分析后 simple_class_name 已改写为完整类名，其 GROUP BY HAVING COUNT>1 返回空）。
     *
     * @param simpleClassName 简单类名
     * @return true: 存在同名类；false: 不存在或查询失败
     */
    public boolean isDuplicateSimpleClassName(String simpleClassName) {
        if (StringUtils.isBlank(simpleClassName)) {
            return false;
        }
        Set<String> usedDuplicateSimpleClassNameSet;
        if (StringUtils.isBlank(tableSuffix)) {
            usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameSet;
        } else {
            usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameMap.get(tableSuffix);
        }
        if (usedDuplicateSimpleClassNameSet == null) {
            if (!findDuplicateClass()) {
                logger.error("查找同名类失败");
                return false;
            }
            usedDuplicateSimpleClassNameSet = StringUtils.isBlank(tableSuffix) ? duplicateSimpleClassNameSet : duplicateSimpleClassNameMap.get(tableSuffix);
        }
        return usedDuplicateSimpleClassNameSet != null && usedDuplicateSimpleClassNameSet.contains(simpleClassName);
    }

    /**
     * 将类名表中的同名类更新为使用完整类名之前，查找类名相同但包名不同的类
     *
     * @return
     */
    public Set<String> findDuplicateClassBeforeUpdate() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_DUPLICATE_CLASS_BEFORE_UPDATE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " group by " + DC.CN_SIMPLE_CLASS_NAME +
                    " having count(" + DC.CN_SIMPLE_CLASS_NAME + ") > 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<String> list = dbOperator.queryListOneColumn(sql, String.class);
        if (list == null) {
            return Collections.emptySet();
        }
        return new HashSet<>(list);
    }

    /**
     * 将类名表中的同名类更新为使用完整类名，并记录同名类
     *
     * @return
     */
    public boolean updateSimpleClassName2Full() {
        Set<String> foundDuplicateSimpleClassNameSet = findDuplicateClassBeforeUpdate();
        if (foundDuplicateSimpleClassNameSet.isEmpty()) {
            logger.info("不存在类名相同但包名不同的类");
            return true;
        }

        duplicateSimpleClassNameSet = foundDuplicateSimpleClassNameSet;
        logger.info("找到类名相同但包名不同的类 {}", StringUtils.join(duplicateSimpleClassNameSet, " "));
        // 执行将简单类名更新为完整类名
        return doUpdateSimpleClassName();
    }

    // 执行将简单类名更新为完整类名
    protected boolean doUpdateSimpleClassName() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_UPDATE_SIMPLE_2_FULL;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "update " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " set " + DC.CN_SIMPLE_CLASS_NAME + " = " + DC.CN_CLASS_NAME + "," + DC.CN_DUPLICATE_CLASS + " = ?" +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        for (String duplicateClassName : duplicateSimpleClassNameSet) {
            // 将class_name_表的simple_name更新为full_name
            if (dbOperator.update(sql, JavaCG2YesNoEnum.YES.getIntValue(), duplicateClassName) == null) {
                return false;
            }
        }
        return true;
    }

    /**
     * 根据完整类名获取对应的类名，使用ThreadLocal中的表名后缀
     *
     * @param className
     * @return
     */
    public String querySimpleClassName(String className) {
        return querySimpleClassName(className, tableSuffix);
    }

    /**
     * 根据完整类名获取对应的类名，使用指定的表名后缀
     * 若当前简单类名存在1个以上，则返回完整类名
     * 若当前简单类名只有1个，则返回简单类名
     * 假如当前数据库中不存在对应的类，也会返回非空的类名
     *
     * @param className   完整类名信息
     * @param tableSuffix 表名后缀
     * @return 完整类名或简单类名
     */
    public String querySimpleClassName(String className, String tableSuffix) {
        Set<String> usedDuplicateSimpleClassNameSet = null;
        // 根据表名后缀选择当前使用的类名相同但包名不同的类名Set
        if (StringUtils.isBlank(tableSuffix)) {
            if (duplicateSimpleClassNameSet != null) {
                usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameSet;
            }
        } else {
            if (duplicateSimpleClassNameMap.containsKey(tableSuffix)) {
                usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameMap.get(tableSuffix);
            }
        }
        if (usedDuplicateSimpleClassNameSet == null) {
            // 查找类名相同但包名不同的类
            if (!findDuplicateClass(tableSuffix)) {
                throw new JavaCG2RuntimeException("查询同名类失败");
            }
            if (StringUtils.isBlank(tableSuffix)) {
                usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameSet;
            } else {
                usedDuplicateSimpleClassNameSet = duplicateSimpleClassNameMap.get(tableSuffix);
            }
        }

        String simpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
        if (usedDuplicateSimpleClassNameSet.contains(simpleClassName)) {
            return className;
        }
        return simpleClassName;
    }

    /**
     * 根据任务中的简单类名或完整类名获取唯一类名
     *
     * @param className
     * @return null: 未获取到，非null: 若不存在同名类，则返回简单类名；若存在同名类，则返回完整类名
     */
    public String querySimpleClassNameInTask(String className) {
        String simpleClassName = simpleClassNameInTaskMap.get(className);
        if (simpleClassName != null) {
            if (JACGConstants.ILLEGAL_CLASS_FLAG.equals(simpleClassName)) {
                return null;
            }
            return simpleClassName;
        }

        // 执行根据任务中的简单类名或完整类名获取唯一类名
        simpleClassName = doGetSimpleClassNameInTask(className);
        if (simpleClassName == null) {
            simpleClassNameInTaskMap.put(className, JACGConstants.ILLEGAL_CLASS_FLAG);
        } else {
            simpleClassNameInTaskMap.put(className, simpleClassName);
        }
        return simpleClassName;
    }

    // 执行根据任务中的简单类名或完整类名获取唯一类名
    private String doGetSimpleClassNameInTask(String className) {
        if (className.contains(JavaCG2Constants.FLAG_DOT)) {
            // 当前指定的是完整类名，查找对应的简单类名
            String simpleClassName = querySimpleClassNameByFull(className);
            if (simpleClassName == null) {
                logger.warn("指定的完整类名 {} 不存在，请检查，可能因为指定的类所在的jar文件未在配置文件中指定 {}", className, JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR.genConfigUsage());
            }
            return simpleClassName;
        }

        // 当前指定的是简单类名
        String simpleClassName = querySimpleClassNameBySimple(className);
        if (simpleClassName == null) {
            logger.warn("指定的简单类名 {} 不存在，请检查，可能因为以下原因 " +
                            "1. 指定的类所在的jar文件未在配置文件中指定 {} " +
                            "2. 指定的类存在同名类，需要使用完整类名形式",
                    className, JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR.genConfigUsage());
            return null;
        }
        return simpleClassName;
    }

    // 根据完整类名查询对应的唯一类名
    protected String querySimpleClassNameByFull(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_SIMPLE_CLASS_NAME_BY_FULL;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " where " + DC.CN_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, className);
    }

    // 查询唯一类名
    protected String querySimpleClassNameBySimple(String simpleCassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_SIMPLE_CLASS_NAME;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, simpleCassName);
    }

    // 根据唯一类名查询完整类名
    public String queryClassNameBySimple(String simpleCassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_CLASS_NAME_BY_SIMPLE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, simpleCassName);
    }

    // 根据调用方简单类名，查找1个对应的完整方法
    public String queryOneFullMethodByCallerSCN(String callerSimpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_FULL_METHOD;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_SIMPLE_CLASS_NAME + " = ?" +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, String.class, callerSimpleClassName);
    }

    // 通过方法名获取调用方方法
    public List<WriteDbData4MethodCall> queryCallerMethodByName(String callerSimpleClassName, String fullMethodPrefix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_METHOD_BY_NAME;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MC_CALLER_METHOD_HASH, DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_RETURN_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLER_FULL_METHOD + " like concat(?, '%')";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, callerSimpleClassName, fullMethodPrefix);
    }

    // 查询当前节点的一个下层被调用方法
    public WriteDbData4MethodCall queryOneCalleeMethod(CallGraphNode4Caller callGraphNode4Caller, int lineNumStart, int lineNumEnd) {
        // 判断查询时是否使用代码行号
        boolean useLineNum = lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE;
        SqlKeyEnum sqlKeyEnum = useLineNum ? SqlKeyEnum.MC_QUERY_ONE_CALLEE_CHECK_LINE_NUM : SqlKeyEnum.MC_QUERY_ONE_CALLEE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            sql = "select " + JACGSqlUtil.joinColumns(
                    DC.MC_CALL_ID,
                    DC.MC_CALL_TYPE,
                    DC.MC_ENABLED,
                    DC.MC_CALLEE_FULL_METHOD,
                    DC.MC_CALLEE_METHOD_HASH,
                    DC.MC_CALLER_SIMPLE_CLASS_NAME,
                    DC.MC_CALLER_LINE_NUMBER,
                    DC.MC_CALL_FLAGS,
                    DC.MC_CALLER_RETURN_TYPE,
                    DC.MC_RAW_RETURN_TYPE) + " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (useLineNum) {
                sql = sql + " and " + DC.MC_CALLER_LINE_NUMBER + " >= ? and " + DC.MC_CALLER_LINE_NUMBER + " <= ?";
            }
            sql = sql + " order by " + DC.MC_CALL_ID +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>(4);
        argList.add(callGraphNode4Caller.getCallerMethodHash());
        argList.add(callGraphNode4Caller.getMethodCallId());
        if (lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE) {
            argList.add(lineNumStart);
            argList.add(lineNumEnd);
        }

        return dbOperator.queryObject(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    /**
     * 根据方法前缀查询对应的方法HASH+长度
     *
     * @param simpleClassName
     * @param fullMethodPrefix
     * @param returnType
     * @return
     */
    public String queryMethodHashByPrefix(String simpleClassName, String fullMethodPrefix, String returnType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_METHOD_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MI_METHOD_HASH +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_FULL_METHOD + " like concat(?, '%')" +
                    " and " + DC.MI_RETURN_TYPE + " = ?" +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryObjectOneColumn(sql, String.class, simpleClassName, fullMethodPrefix, returnType);
    }

    /**
     * 查询方法行号
     *
     * @param simpleClassName
     * @param methodLineNum
     * @return
     */
    public WriteDbData4MethodLineNumber queryMethodLineNumber(String simpleClassName, int methodLineNum) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MLN_QUERY_METHOD_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MLN_METHOD_HASH, DC.MLN_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_LINE_NUMBER.getTableName() +
                    " where " + DC.MLN_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MLN_MIN_LINE_NUMBER + " <= ?" +
                    " and " + DC.MLN_MAX_LINE_NUMBER + " >= ?" +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4MethodLineNumber.class, simpleClassName, methodLineNum, methodLineNum);
    }

    /**
     * 查询方法调用的额外信息
     *
     * @param isCallee
     * @param methodHash
     * @return
     */
    public WriteDbData4MethodCall queryMethodCallExtraInfo(boolean isCallee, String methodHash) {
        SqlKeyEnum sqlKeyEnum = isCallee ? SqlKeyEnum.MC_QUERY_FLAG_4EE : SqlKeyEnum.MC_QUERY_FLAG_4ER;
        String whereColumnName = isCallee ? DC.MC_CALLEE_METHOD_HASH : DC.MC_CALLER_METHOD_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_RETURN_TYPE, DC.MC_RAW_RETURN_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + whereColumnName + " = ?" +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4MethodCall.class, methodHash);
    }

    /**
     * 根据类名查询相关的方法
     *
     * @param className
     * @return
     */
    public List<FullMethodWithReturnType> queryMethodByClassName(String className) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FULL_METHOD_BY_CLASS;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MI_FULL_METHOD, DC.MI_RETURN_TYPE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        List<WriteDbData4MethodInfo> list = dbOperator.queryList(sql, WriteDbData4MethodInfo.class, querySimpleClassName(className));
        return genFullMethodWithReturnTypeList(list);
    }

    public List<FullMethodWithReturnType> genFullMethodWithReturnTypeList(List<WriteDbData4MethodInfo> list) {
        if (list == null) {
            return null;
        }
        List<FullMethodWithReturnType> fullMethodWithReturnTypeList = new ArrayList<>();
        for (WriteDbData4MethodInfo methodInfo : list) {
            fullMethodWithReturnTypeList.add(new FullMethodWithReturnType(methodInfo.getFullMethod(), methodInfo.getReturnType()));
        }
        fullMethodWithReturnTypeList.sort(Comparator4FullMethodWithReturnType.getInstance());
        return fullMethodWithReturnTypeList;
    }

    /**
     * 根据类名及完整方法前缀查询方法信息
     *
     * @param className
     * @param fullMethodPrefix
     * @return
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoByClassMethodPrefix(String className, String fullMethodPrefix) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_CLASS_METHOD_PREFIX;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_FULL_METHOD + " like concat(?, '%')";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, querySimpleClassName(className), fullMethodPrefix);
    }

    /**
     * 根据类名、完整方法前缀、返回类型查询方法信息
     *
     * @param className
     * @param fullMethodPrefix
     * @return
     */
    public List<WriteDbData4MethodInfo> queryMethodInfoByClassMethodPrefixReturnType(String className, String fullMethodPrefix, String returnType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_ALL_BY_CLASS_METHOD_PREFIX_RETURN_TYPE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_INFO) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_FULL_METHOD + " like concat(?, '%')" +
                    " and " + DC.MI_RETURN_TYPE + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodInfo.class, querySimpleClassName(className), fullMethodPrefix, returnType);
    }

    //
    public ConfigureWrapper getConfigureWrapper() {
        return configureWrapper;
    }

    public DbOperator getDbOperator() {
        return dbOperator;
    }

    public int getDbInsertBatchSize() {
        return dbInsertBatchSize;
    }

    /**
     * 获得当前使用的所有数据库表名及COMMENT
     * 仅返回表名以 DbTableInfoEnum 枚举常量中的 tableNameKeyword_{appName}{tableSuffix} 结尾的表名
     * （属于当前项目的当前app.name对应的数据库表）
     *
     * @return key: 表名, value: 表COMMENT
     */
    public Map<String, String> getTableNameAndCommentMap() {
        // 收集所有DbTableInfoEnum枚举的tableNameKeyword，用于判断表名是否属于当前项目
        Set<String> tableNameKeywordSet = new HashSet<>();
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            tableNameKeywordSet.add(dbTableInfoEnum.getTableNameKeyword());
        }
        // 表名需要匹配的后缀：tableNameKeyword_{appName}{tableSuffix}
        String suffix = appName + tableSuffix;

        Map<String, String> tableNameAndCommentMap = new HashMap<>();
        if (dbOperator.isUseH2Db()) {
            List<Map<String, Object>> list = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT TABLE_NAME, REMARKS FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ?",
                    JACGConstants.H2_SCHEMA);
            for (Map<String, Object> map : list) {
                String tableName = (String) map.get("TABLE_NAME");
                if (tableName != null && isExpectedTableName(tableName, tableNameKeywordSet, suffix)) {
                    String comment = (String) map.get("REMARKS");
                    tableNameAndCommentMap.put(tableName, comment != null ? comment : "");
                }
            }
        } else if (dbOperator.getDbConfInfo().isUsePgDb()) {
            List<Map<String, Object>> list = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT c.relname AS TABLE_NAME, obj_description(c.oid) AS REMARKS " +
                            "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace " +
                            "WHERE n.nspname = current_schema() AND c.relkind = 'r'");
            for (Map<String, Object> map : list) {
                String tableName = (String) map.get("TABLE_NAME");
                if (tableName != null && isExpectedTableName(tableName, tableNameKeywordSet, suffix)) {
                    String comment = (String) map.get("REMARKS");
                    tableNameAndCommentMap.put(tableName, comment != null ? comment : "");
                }
            }
        } else {
            // MySQL
            List<Map<String, Object>> list = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT TABLE_NAME, TABLE_COMMENT FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = DATABASE()");
            for (Map<String, Object> map : list) {
                String tableName = (String) map.get("TABLE_NAME");
                if (tableName != null && isExpectedTableName(tableName, tableNameKeywordSet, suffix)) {
                    String comment = (String) map.get("TABLE_COMMENT");
                    tableNameAndCommentMap.put(tableName, comment != null ? comment : "");
                }
            }
        }
        logger.info("[{}] 获取到匹配 DbTableInfoEnum tableNameKeyword_{} 结尾的表数量: {}", objSeq, suffix, tableNameAndCommentMap.size());
        return tableNameAndCommentMap;
    }

    /**
     * 判断表名是否属于当前项目的当前app.name对应的数据库表
     * 表名需要以 DbTableInfoEnum 枚举常量中的 tableNameKeyword_{appName}{tableSuffix} 结尾才认为属于当前项目
     *
     * @param tableName           数据库表名
     * @param tableNameKeywordSet DbTableInfoEnum枚举中所有tableNameKeyword的集合
     * @param suffix              appName + tableSuffix 组成的后缀
     * @return true: 表名属于当前项目 false: 不属于
     */
    private boolean isExpectedTableName(String tableName, Set<String> tableNameKeywordSet, String suffix) {
        for (String tableNameKeyword : tableNameKeywordSet) {
            // 改为精确等于匹配（复用 DbTableInfoEnum.buildTableName），避免空后缀失配与 dup_* 误配
            if (tableName.equals(DbTableInfoEnum.buildTableName(tableNameKeyword, suffix))) {
                return true;
            }
        }
        return false;
    }

    /**
     * 查询当前配置参数对应指定表的字段信息
     * 仅返回属于当前项目的当前app.name对应的数据库表（表名以 DbTableInfoEnum 枚举常量中的 tableNameKeyword_{appName}{tableSuffix} 结尾）
     *
     * @param tableNameList 表名列表，若为空则查询全部表，若非空则只查询指定表
     * @return 表字段信息列表，包含字段名称、字段类型、字段描述；查询失败返回null
     */
    public List<TableColumnInfo> queryTableColumnInfo(List<String> tableNameList) {
        // 获取当前项目的所有表名及COMMENT，过滤出指定表
        Map<String, String> tableNameAndCommentMap = getTableNameAndCommentMap();
        if (tableNameAndCommentMap.isEmpty()) {
            logger.warn("[{}] 未找到当前项目对应的数据库表", objSeq);
            return null;
        }

        // 筛选需要查询字段信息的表
        List<String> queryTableNameList = new ArrayList<>();
        if (tableNameList != null && !tableNameList.isEmpty()) {
            // 指定了表名，只查询指定表，需过滤掉不属于当前项目的表
            for (String tableName : tableNameList) {
                if (tableNameAndCommentMap.containsKey(tableName)) {
                    queryTableNameList.add(tableName);
                } else {
                    logger.warn("[{}] 表名 {} 不属于当前项目对应的数据库表，忽略", objSeq, tableName);
                }
            }
            if (queryTableNameList.isEmpty()) {
                logger.warn("[{}] 指定的表名均不属于当前项目对应的数据库表", objSeq);
                return null;
            }
        } else {
            // 未指定表名，查询全部表
            queryTableNameList.addAll(tableNameAndCommentMap.keySet());
        }

        List<TableColumnInfo> tableColumnInfoList = new ArrayList<>();
        for (String tableName : queryTableNameList) {
            List<TableColumnInfo> columnInfoList = queryOneTableColumnInfo(tableName);
            if (columnInfoList == null) {
                return null;
            }
            tableColumnInfoList.addAll(columnInfoList);
        }
        logger.info("[{}] 查询到表数量 {} 字段信息数量 {}", objSeq, queryTableNameList.size(), tableColumnInfoList.size());
        return tableColumnInfoList;
    }

    /**
     * 查询指定表的字段信息
     *
     * @param tableName 表名
     * @return 表字段信息列表；查询失败返回null
     */
    private List<TableColumnInfo> queryOneTableColumnInfo(String tableName) {
        List<TableColumnInfo> columnInfoList = new ArrayList<>();
        try {
            if (dbOperator.isUseH2Db()) {
                // H2数据库
                List<Map<String, Object>> columnList = dbOperator.getJdbcTemplate().queryForList(
                        "SELECT COLUMN_NAME, DATA_TYPE, REMARKS " +
                                "FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? ORDER BY ORDINAL_POSITION",
                        JACGConstants.H2_SCHEMA, tableName);
                for (Map<String, Object> col : columnList) {
                    TableColumnInfo columnInfo = new TableColumnInfo();
                    columnInfo.setTableName(tableName);
                    columnInfo.setColumnName((String) col.get("COLUMN_NAME"));
                    columnInfo.setColumnType((String) col.get("DATA_TYPE"));
                    Object remarks = col.get("REMARKS");
                    columnInfo.setColumnComment(remarks != null ? remarks.toString() : "");
                    columnInfoList.add(columnInfo);
                }
            } else if (dbOperator.getDbConfInfo().isUsePgDb()) {
                // PostgreSQL数据库
                List<Map<String, Object>> columnList = dbOperator.getJdbcTemplate().queryForList(
                        "SELECT a.attname AS column_name, pg_catalog.format_type(a.atttypid, a.atttypmod) AS data_type, " +
                                "col_description(a.attrelid, a.attnum) AS column_comment " +
                                "FROM pg_attribute a " +
                                "JOIN pg_class c ON a.attrelid = c.oid " +
                                "JOIN pg_namespace n ON n.oid = c.relnamespace " +
                                "WHERE n.nspname = current_schema() AND c.relname = ? AND a.attnum > 0 AND NOT a.attisdropped " +
                                "ORDER BY a.attnum",
                        tableName);
                for (Map<String, Object> col : columnList) {
                    TableColumnInfo columnInfo = new TableColumnInfo();
                    columnInfo.setTableName(tableName);
                    columnInfo.setColumnName((String) col.get("column_name"));
                    columnInfo.setColumnType((String) col.get("data_type"));
                    Object comment = col.get("column_comment");
                    columnInfo.setColumnComment(comment != null ? comment.toString() : "");
                    columnInfoList.add(columnInfo);
                }
            } else {
                // MySQL数据库
                List<Map<String, Object>> columnList = dbOperator.getJdbcTemplate().queryForList(
                        "SELECT COLUMN_NAME, DATA_TYPE, COLUMN_COMMENT " +
                                "FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = ? ORDER BY ORDINAL_POSITION",
                        tableName);
                for (Map<String, Object> col : columnList) {
                    TableColumnInfo columnInfo = new TableColumnInfo();
                    columnInfo.setTableName(tableName);
                    columnInfo.setColumnName((String) col.get("COLUMN_NAME"));
                    columnInfo.setColumnType((String) col.get("DATA_TYPE"));
                    Object comment = col.get("COLUMN_COMMENT");
                    columnInfo.setColumnComment(comment != null ? comment.toString() : "");
                    columnInfoList.add(columnInfo);
                }
            }
            return columnInfoList;
        } catch (Exception e) {
            logger.error("[{}] 获取表字段信息失败: {}", objSeq, tableName, e);
            return null;
        }
    }

    /**
     * 获得指定数据库表的建表语句
     * 支持MySQL、H2、PostgreSQL
     *
     * @param tableName 表名
     * @return 建表语句，若表不存在返回null
     */
    public String getCreateTableSql(String tableName) {
        if (!dbOperator.checkTableExists(tableName)) {
            logger.error("[{}] 数据库表不存在: {}", objSeq, tableName);
            return null;
        }
        if (dbOperator.isUseH2Db()) {
            // H2数据库，通过INFORMATION_SCHEMA.COLUMNS拼接建表语句
            return genCreateTableSqlFromH2(tableName);
        } else if (dbOperator.getDbConfInfo().isUsePgDb()) {
            // PostgreSQL，使用pg_get_tabledef或拼接
            return genCreateTableSqlFromPg(tableName);
        } else {
            // MySQL，使用SHOW CREATE TABLE
            return genCreateTableSqlFromMySQL(tableName);
        }
    }

    /**
     * 获得所有数据库表的建表语句
     * 分别调用getTableNameAndCommentMap和getCreateTableSql方法
     *
     * @return key: 表名, value: 建表语句
     */
    public Map<String, String> getAllCreateTableSqlMap() {
        Map<String, String> tableNameAndCommentMap = getTableNameAndCommentMap();
        Map<String, String> createTableSqlMap = new HashMap<>();
        for (String tableName : tableNameAndCommentMap.keySet()) {
            String createTableSql = getCreateTableSql(tableName);
            if (createTableSql != null) {
                createTableSqlMap.put(tableName, createTableSql);
            }
        }
        return createTableSqlMap;
    }

    /**
     * 查询指定表的结构化信息（字段信息 + 索引信息 + 建表语句 + 额外描述），全量返回
     * 通过 JDBC DatabaseMetaData 统一获取，适配 H2/MySQL/PostgreSQL
     *
     * @param tableName 表名
     * @return 表结构信息；表不存在或查询失败返回 null
     */
    public TableStructureInfo queryTableStructureInfo(String tableName) {
        return queryTableStructureInfo(tableName, true, true);
    }

    /**
     * 查询指定表的结构化信息，可控制返回内容
     * 通过 JDBC DatabaseMetaData 统一获取，适配 H2/MySQL/PostgreSQL
     *
     * @param tableName            表名
     * @param returnFieldAndIndex  是否返回字段信息与索引信息
     * @param returnCreateTableSql 是否返回建表语句
     * @return 表结构信息（未请求的部分对应字段为null；额外描述始终返回）；表不存在或查询失败返回 null
     */
    public TableStructureInfo queryTableStructureInfo(String tableName, boolean returnFieldAndIndex, boolean returnCreateTableSql) {
        if (!dbOperator.checkTableExists(tableName)) {
            logger.error("[{}] 数据库表不存在: {}", objSeq, tableName);
            return null;
        }

        TableStructureInfo tableStructureInfo = new TableStructureInfo();
        tableStructureInfo.setTableName(tableName);

        if (returnFieldAndIndex) {
            // 按数据库选择 schema/catalog：H2 使用固定 schema，MySQL/PostgreSQL 使用连接默认值
            String schema = dbOperator.isUseH2Db() ? JACGConstants.H2_SCHEMA : null;
            String catalog = null;

            List<TableColumnInfo> fieldList = new ArrayList<>();
            List<TableIndexInfo> indexList = new ArrayList<>();
            // 主键索引名集合，用于在 getIndexInfo 中跳过主键（避免重复）
            Set<String> pkIndexNameSet = new HashSet<>();

            try (Connection conn = dbOperator.getJdbcTemplate().getDataSource().getConnection()) {
                DatabaseMetaData metaData = conn.getMetaData();

                // 1. 字段信息：字段名、数据库字段类型、COMMENT、精度、小数位
                try (ResultSet rs = metaData.getColumns(catalog, schema, tableName, "%")) {
                    while (rs.next()) {
                        TableColumnInfo fieldInfo = new TableColumnInfo();
                        fieldInfo.setTableName(tableName);
                        fieldInfo.setColumnName(rs.getString("COLUMN_NAME"));
                        fieldInfo.setColumnType(rs.getString("TYPE_NAME"));
                        Object remarks = rs.getObject("REMARKS");
                        fieldInfo.setColumnComment(remarks != null ? remarks.toString() : "");
                        fieldInfo.setPrecision(rs.getInt("COLUMN_SIZE"));
                        fieldInfo.setScale(rs.getInt("DECIMAL_DIGITS"));
                        fieldList.add(fieldInfo);
                    }
                }

                // 2. 主键索引：索引名、字段名列表（按 KEY_SEQ 顺序）
                // 使用 LinkedHashMap 保持主键字段顺序
                Map<String, List<String>> pkColumnsMap = new LinkedHashMap<>();
                try (ResultSet rs = metaData.getPrimaryKeys(catalog, schema, tableName)) {
                    while (rs.next()) {
                        String pkName = rs.getString("PK_NAME");
                        String columnName = rs.getString("COLUMN_NAME");
                        if (columnName == null) {
                            continue;
                        }
                        if (pkName != null) {
                            pkIndexNameSet.add(pkName);
                        }
                        // 主键索引名可能为 null（部分数据库不返回），使用 "_PRIMARY_" 作为占位名
                        String indexName = pkName != null ? pkName : "_PRIMARY_";
                        pkColumnsMap.computeIfAbsent(indexName, k -> new ArrayList<>()).add(columnName);
                    }
                }
                for (Map.Entry<String, List<String>> entry : pkColumnsMap.entrySet()) {
                    TableIndexInfo indexInfo = new TableIndexInfo();
                    indexInfo.setIndexType(IndexTypeEnum.PRIMARY);
                    indexInfo.setIndexName(entry.getKey());
                    indexInfo.setColumnNames(entry.getValue());
                    indexList.add(indexInfo);
                }

                // 3. 其他索引：跳过表统计信息、跳过主键索引；按 NON_UNIQUE 判定 唯一/普通
                Map<String, Boolean> indexUniqueMap = new LinkedHashMap<>();
                Map<String, List<String>> indexColumnsMap = new LinkedHashMap<>();
                try (ResultSet rs = metaData.getIndexInfo(catalog, schema, tableName, false, false)) {
                    while (rs.next()) {
                        short type = rs.getShort("TYPE");
                        // 跳过表统计信息
                        if (type == DatabaseMetaData.tableIndexStatistic) {
                            continue;
                        }
                        String indexName = rs.getString("INDEX_NAME");
                        if (indexName == null) {
                            continue;
                        }
                        // 跳过主键索引（已在主键部分处理）
                        if (pkIndexNameSet.contains(indexName)) {
                            continue;
                        }
                        boolean nonUnique = rs.getBoolean("NON_UNIQUE");
                        String columnName = rs.getString("COLUMN_NAME");
                        if (columnName == null) {
                            continue;
                        }
                        indexUniqueMap.put(indexName, !nonUnique);
                        indexColumnsMap.computeIfAbsent(indexName, k -> new ArrayList<>()).add(columnName);
                    }
                }
                for (Map.Entry<String, List<String>> entry : indexColumnsMap.entrySet()) {
                    String indexName = entry.getKey();
                    boolean isUnique = indexUniqueMap.getOrDefault(indexName, false);
                    TableIndexInfo indexInfo = new TableIndexInfo();
                    indexInfo.setIndexType(isUnique ? IndexTypeEnum.UNIQUE : IndexTypeEnum.NORMAL);
                    indexInfo.setIndexName(indexName);
                    indexInfo.setColumnNames(entry.getValue());
                    indexList.add(indexInfo);
                }
            } catch (Exception e) {
                logger.error("[{}] 查询表结构化信息失败: {}", objSeq, tableName, e);
                return null;
            }

            tableStructureInfo.setFieldList(fieldList);
            tableStructureInfo.setIndexList(indexList);
            logger.info("[{}] 查询表结构化信息 表名 {} 字段数量 {} 索引数量 {}", objSeq, tableName, fieldList.size(), indexList.size());
        }

        if (returnCreateTableSql) {
            // 建表语句（内部按数据库查询，开销较大，按需返回）
            tableStructureInfo.setCreateTableSql(getCreateTableSql(tableName));
        }

        // 数据库表额外描述（按表名反查DbTableInfoEnum，内存查找，开销小，始终返回）
        tableStructureInfo.setExtraDesc(getTableExtraDesc(tableName));
        return tableStructureInfo;
    }

    /**
     * 根据数据库表名查找对应的 DbTableInfoEnum
     * 表名需以 jacg_<tableNameKeyword>_<appName><tableSuffix> 形式匹配
     *
     * @param tableName 数据库表名
     * @return 对应的枚举常量；未匹配返回 null
     */
    public DbTableInfoEnum getDbTableInfoEnumByTableName(String tableName) {
        if (tableName == null) {
            return null;
        }
        String suffix = appName + tableSuffix;
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            // 改为精确等于匹配（复用 DbTableInfoEnum.buildTableName），避免空后缀失配与 dup_* 误配
            if (tableName.equals(DbTableInfoEnum.buildTableName(dbTableInfoEnum.getTableNameKeyword(), suffix))) {
                return dbTableInfoEnum;
            }
        }
        return null;
    }

    /**
     * 获取数据库表的额外描述，代表该表存储的数据及作用
     *
     * @param tableName 数据库表名
     * @return 额外描述；未匹配DbTableInfoEnum返回空字符串
     */
    public String getTableExtraDesc(String tableName) {
        DbTableInfoEnum dbTableInfoEnum = getDbTableInfoEnumByTableName(tableName);
        return dbTableInfoEnum != null ? dbTableInfoEnum.getExtraDesc() : "";
    }

    /**
     * 查询任意数据
     *
     * @param sql      需要查询的sql语句（直接使用，不会再进行替换或格式化），参数使用"?"指定
     * @param paramMap 参数Map，key为参数下标（从0开始），value为参数值
     * @return 查询结果
     */
    public QueryDataResult queryData(String sql, Map<Integer, Object> paramMap) {
        if (StringUtils.isBlank(sql)) {
            logger.error("[{}] sql语句为空", objSeq);
            return genFailResult("sql语句为空");
        }

        // 替换sql语句中的 appName 占位符（{appName}），使预置/复用SQL可跨项目执行（query_db_data 路径）
        // 必须在 validateSelectSql 之前替换：含 {appName} 的表名 Druid 无法解析，会误判为"SQL语句解析失败"
        sql = formatSql(sql, false);

        String validateResult = validateSelectSql(sql);
        if (validateResult != null) {
            return genFailResult(validateResult);
        }

        // 将Map参数转为数组
        Object[] args = convertParamMapToArray(paramMap);
        if (args == null && paramMap != null && !paramMap.isEmpty()) {
            return genFailResult("参数下标不连续");
        }

        QueryDataResult queryDataResult = new QueryDataResult();
        long startTime = System.currentTimeMillis();
        try {
            List<Map<String, Object>> resultList;
            if (args == null) {
                resultList = dbOperator.getJdbcTemplate().queryForList(sql);
            } else {
                resultList = dbOperator.getJdbcTemplate().queryForList(sql, args);
            }
            queryDataResult.setSuccess(true);
            queryDataResult.setResultList(resultList);
            queryDataResult.setCostTimeMs(System.currentTimeMillis() - startTime);
            logger.info("[{}] 查询到的数据量 {} 耗时 {} ms {}", objSeq, resultList.size(), queryDataResult.getCostTimeMs(), sql);
            return queryDataResult;
        } catch (Exception e) {
            queryDataResult.setCostTimeMs(System.currentTimeMillis() - startTime);
            queryDataResult.setErrorMsg(e.getMessage());
            logger.error("[{}] 查询数据失败: {} 耗时 {} ms", objSeq, sql, queryDataResult.getCostTimeMs(), e);
            return queryDataResult;
        }
    }

    /**
     * 查询任意数据（带超时时间）
     * 超时时间单位为秒，使用 JDBC 的 PreparedStatement.setQueryTimeout(int seconds) 实现
     * （JDBC 规范仅提供秒级的查询超时设置，不支持毫秒，详见详细方案文档“为什么只能用秒”一节）
     *
     * @param sql            需要查询的sql语句（会先替换 {appName} 占位符为当前项目appName），参数使用"?"指定
     * @param paramMap       参数Map，key为参数下标（从0开始），value为参数值
     * @param timeoutSeconds 超时时间（秒），必须大于0
     * @return 查询结果
     */
    public QueryDataResult queryDataWithTimeout(String sql, Map<Integer, Object> paramMap, int timeoutSeconds) {
        if (timeoutSeconds <= 0) {
            logger.error("[{}] 超时时间必须大于0: {}", objSeq, timeoutSeconds);
            return genFailResult("超时时间必须大于0: " + timeoutSeconds);
        }

        if (StringUtils.isBlank(sql)) {
            logger.error("[{}] sql语句为空", objSeq);
            return genFailResult("sql语句为空");
        }

        // 替换sql语句中的 appName 占位符（{appName}），使预置/复用SQL可跨项目执行（与 queryData 一致）
        // 必须在 validateSelectSql 之前替换：含 {appName} 的表名 Druid 无法解析，会误判为"SQL语句解析失败"
        sql = formatSql(sql, false);

        String validateResult = validateSelectSql(sql);
        if (validateResult != null) {
            return genFailResult(validateResult);
        }

        // 将Map参数转为数组
        Object[] args = convertParamMapToArray(paramMap);
        if (args == null && paramMap != null && !paramMap.isEmpty()) {
            return genFailResult("参数下标不连续");
        }

        QueryDataResult queryDataResult = new QueryDataResult();
        long startTime = System.currentTimeMillis();
        try {
            List<Map<String, Object>> resultList = dbOperator.getJdbcTemplate().queryForListWithTimeout(sql, timeoutSeconds, args);
            queryDataResult.setSuccess(true);
            queryDataResult.setResultList(resultList);
            queryDataResult.setCostTimeMs(System.currentTimeMillis() - startTime);
            logger.info("[{}] 查询到的数据量 {} 耗时 {} ms {}", objSeq, resultList != null ? resultList.size() : 0, queryDataResult.getCostTimeMs(), sql);
            return queryDataResult;
        } catch (Exception e) {
            queryDataResult.setCostTimeMs(System.currentTimeMillis() - startTime);
            // 判断是否为查询超时：执行耗时是否大于等于指定的超时时间，或者接近（相差小于100毫秒）
            boolean isTimeout = checkQueryTimeout(queryDataResult.getCostTimeMs(), timeoutSeconds);
            queryDataResult.setTimeout(isTimeout);
            queryDataResult.setErrorMsg(e.getMessage());
            logger.error("[{}] 查询数据失败: {} 耗时 {} ms 超时: {}", objSeq, sql, queryDataResult.getCostTimeMs(), isTimeout, e);
            return queryDataResult;
        }
    }

    /**
     * 判断是否为查询超时
     * 通过比较执行耗时与指定的超时时间来判断，不依赖异常类型，兼容所有数据库
     *
     * @param costTimeMs     执行耗时（毫秒）
     * @param timeoutSeconds 指定的超时时间（秒）
     * @return true: 查询超时 false: 其他异常
     */
    private boolean checkQueryTimeout(long costTimeMs, int timeoutSeconds) {
        long timeoutMs = timeoutSeconds * 1000L;
        // 执行耗时大于等于超时时间，或者接近超时时间（相差小于100毫秒），视为超时
        return costTimeMs >= timeoutMs - 100;
    }

    /**
     * 生成失败的查询结果
     *
     * @param errorMsg 失败信息
     * @return 失败的查询结果
     */
    private QueryDataResult genFailResult(String errorMsg) {
        QueryDataResult queryDataResult = new QueryDataResult();
        queryDataResult.setErrorMsg(errorMsg);
        return queryDataResult;
    }

    /**
     * 获取当前数据库对应的Druid解析方言类型
     *
     * @return Druid DbType
     */
    private DbType getDbType() {
        if (dbOperator.isUseH2Db()) {
            return DbType.h2;
        }
        if (dbOperator.getDbConfInfo().isUsePgDb()) {
            return DbType.postgresql;
        }
        return DbType.mysql;
    }

    /**
     * 解析SQL语句，获取被操作的表名及SQL语句类型（增删改查）
     * 使用Druid SQL Parser解析，根据当前数据库类型选择对应的方言
     *
     * @param sql 需要解析的SQL语句
     * @return SQL语句解析信息列表，每个元素包含被操作的表名及SQL语句类型；SQL语句不合法时返回null
     */
    public List<SqlInfo> parseSqlInfo(String sql) {
        if (StringUtils.isBlank(sql)) {
            logger.error("[{}] sql语句为空", objSeq);
            return null;
        }
        try {
            // 根据当前数据库类型选择对应的方言进行解析
            DbType dbType = getDbType();
            List<com.alibaba.druid.sql.ast.SQLStatement> statementList = SQLUtils.parseStatements(sql, dbType);
            if (statementList.isEmpty()) {
                logger.error("[{}] SQL语句解析结果为空", objSeq);
                return null;
            }
            List<SqlInfo> sqlInfoList = new ArrayList<>();
            for (com.alibaba.druid.sql.ast.SQLStatement statement : statementList) {
                SqlInfo sqlInfo = parseOneStatement(statement);
                if (sqlInfo == null) {
                    return null;
                }
                sqlInfoList.add(sqlInfo);
            }
            return sqlInfoList;
        } catch (Exception e) {
            logger.error("[{}] SQL语句解析失败，可能是非法SQL", objSeq, e);
            return null;
        }
    }

    /**
     * 解析单条SQL语句，获取被操作的表名及SQL语句类型
     *
     * @param statement Druid SQLStatement对象
     * @return SQL语句解析信息；无法识别的语句类型返回null
     */
    private SqlInfo parseOneStatement(com.alibaba.druid.sql.ast.SQLStatement statement) {
        SqlInfo sqlInfo = new SqlInfo();
        // 根据语句类型设置SQL类型，并获取被操作的表名
        if (statement instanceof com.alibaba.druid.sql.ast.statement.SQLSelectStatement) {
            sqlInfo.setSqlType(SqlTypeEnum.SELECT.getType());
            // SELECT语句通过查询块获取表名
            com.alibaba.druid.sql.ast.statement.SQLSelectStatement selectStatement = (com.alibaba.druid.sql.ast.statement.SQLSelectStatement) statement;
            com.alibaba.druid.sql.ast.statement.SQLSelectQuery query = selectStatement.getSelect().getQuery();
            sqlInfo.setTableName(getTableNameFromSelectQuery(query));
        } else if (statement instanceof com.alibaba.druid.sql.ast.statement.SQLInsertStatement) {
            sqlInfo.setSqlType(SqlTypeEnum.INSERT.getType());
            com.alibaba.druid.sql.ast.statement.SQLInsertStatement insertStatement = (com.alibaba.druid.sql.ast.statement.SQLInsertStatement) statement;
            sqlInfo.setTableName(getTableNameFromTableSource(insertStatement.getTableSource()));
        } else if (statement instanceof com.alibaba.druid.sql.ast.statement.SQLUpdateStatement) {
            sqlInfo.setSqlType(SqlTypeEnum.UPDATE.getType());
            com.alibaba.druid.sql.ast.statement.SQLUpdateStatement updateStatement = (com.alibaba.druid.sql.ast.statement.SQLUpdateStatement) statement;
            sqlInfo.setTableName(getTableNameFromTableSource(updateStatement.getTableSource()));
        } else if (statement instanceof com.alibaba.druid.sql.ast.statement.SQLDeleteStatement) {
            sqlInfo.setSqlType(SqlTypeEnum.DELETE.getType());
            com.alibaba.druid.sql.ast.statement.SQLDeleteStatement deleteStatement = (com.alibaba.druid.sql.ast.statement.SQLDeleteStatement) statement;
            sqlInfo.setTableName(getTableNameFromTableSource(deleteStatement.getTableSource()));
        } else {
            // DDL等其他语句（如DROP/CREATE/ALTER/TRUNCATE），归类为OTHER，由调用方按非SELECT拒绝
            // 不返回null，避免与"SQL语句解析失败"混淆（DDL是合法SQL，应被识别为非SELECT而非解析失败）
            logger.info("[{}] 归类为OTHER的SQL语句类型: {}", objSeq, statement.getClass().getSimpleName());
            sqlInfo.setSqlType(SqlTypeEnum.OTHER.getType());
            sqlInfo.setTableName(null);
        }
        return sqlInfo;
    }

    /**
     * 从SQLTableSource中获取表名
     *
     * @param tableSource SQLTableSource对象
     * @return 表名；无法获取时返回null
     */
    private String getTableNameFromTableSource(com.alibaba.druid.sql.ast.statement.SQLTableSource tableSource) {
        if (tableSource == null) {
            return null;
        }
        // SQLExprTableSource通过getTableName获取表名
        if (tableSource instanceof com.alibaba.druid.sql.ast.statement.SQLExprTableSource) {
            return ((com.alibaba.druid.sql.ast.statement.SQLExprTableSource) tableSource).getTableName();
        }
        // 子查询、JOIN等场景，返回toString
        return tableSource.toString();
    }

    /**
     * 从SELECT查询块中获取表名
     *
     * @param query SQLSelectQuery对象
     * @return 表名；无法获取时返回null
     */
    private String getTableNameFromSelectQuery(com.alibaba.druid.sql.ast.statement.SQLSelectQuery query) {
        if (query == null) {
            return null;
        }
        if (query instanceof com.alibaba.druid.sql.ast.statement.SQLSelectQueryBlock) {
            com.alibaba.druid.sql.ast.statement.SQLSelectQueryBlock queryBlock = (com.alibaba.druid.sql.ast.statement.SQLSelectQueryBlock) query;
            return getTableNameFromTableSource(queryBlock.getFrom());
        }
        // UNION等复合查询，返回toString
        return query.toString();
    }

    /**
     * 验证SQL语句是否为SELECT语句，防止通过查询接口执行非查询操作
     * 复用 parseSqlInfo 方法获取SQL语句信息后判断类型
     *
     * @param sql 需要验证的SQL语句
     * @return null: 合法的SELECT语句；非null: 失败原因（解析异常信息或"SQL语句不是SELECT语句，不允许执行"）
     */
    private String validateSelectSql(String sql) {
        List<SqlInfo> sqlInfoList = parseSqlInfo(sql);
        if (sqlInfoList == null) {
            // 解析失败，parseSqlInfo内部已记录日志，返回解析失败信息
            return "SQL语句解析失败";
        }
        // 检查所有语句是否都是SELECT语句
        for (SqlInfo sqlInfo : sqlInfoList) {
            if (!SqlTypeEnum.SELECT.getType().equals(sqlInfo.getSqlType())) {
                logger.error("[{}] SQL语句不是SELECT语句，不允许执行: {}", objSeq, sqlInfo.getSqlType());
                return "SQL语句不是SELECT语句，不允许执行";
            }
        }
        return null;
    }

    /**
     * 将参数Map转为数组
     *
     * @param paramMap 参数Map，key为参数下标（从0开始），value为参数值
     * @return 参数数组；当paramMap为null或空时返回null；参数下标不连续时返回null
     */
    private Object[] convertParamMapToArray(Map<Integer, Object> paramMap) {
        if (paramMap == null || paramMap.isEmpty()) {
            return null;
        }
        // 检查参数下标是否从0开始连续
        int maxIndex = Collections.max(paramMap.keySet());
        if (maxIndex < 0) {
            logger.error("[{}] 参数下标不能为负数: {}", objSeq, maxIndex);
            return null;
        }
        Object[] args = new Object[maxIndex + 1];
        for (int i = 0; i <= maxIndex; i++) {
            if (!paramMap.containsKey(i)) {
                logger.error("[{}] 参数下标不连续，缺少: {}", objSeq, i);
                return null;
            }
            args[i] = paramMap.get(i);
        }
        return args;
    }

    // MySQL获取建表语句
    private String genCreateTableSqlFromMySQL(String tableName) {
        try {
            List<Map<String, Object>> list = dbOperator.getJdbcTemplate().queryForList("SHOW CREATE TABLE " + tableName);
            if (!list.isEmpty()) {
                Map<String, Object> map = list.get(0);
                // MySQL SHOW CREATE TABLE返回的第二列名称可能是"Create Table"
                for (Object value : map.values()) {
                    if (value instanceof String && ((String) value).toUpperCase().contains("CREATE TABLE")) {
                        return (String) value;
                    }
                }
            }
        } catch (Exception e) {
            logger.error("[{}] MySQL获取建表语句失败: {}", objSeq, tableName, e);
        }
        return null;
    }

    // PostgreSQL获取建表语句
    private String genCreateTableSqlFromPg(String tableName) {
        try {
            StringBuilder sb = new StringBuilder();
            // 记录列注释，用于后续生成COMMENT ON COLUMN语句
            List<String[]> columnCommentList = new ArrayList<>();

            // 1. 获取列定义
            List<Map<String, Object>> columnList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT a.attname AS column_name, pg_catalog.format_type(a.atttypid, a.atttypmod) AS data_type, " +
                            "NOT a.attnotnull AS nullable, pg_catalog.pg_get_expr(d.adbin, d.adrelid) AS column_default, " +
                            "col_description(a.attrelid, a.attnum) AS column_comment " +
                            "FROM pg_attribute a " +
                            "JOIN pg_class c ON a.attrelid = c.oid " +
                            "JOIN pg_namespace n ON n.oid = c.relnamespace " +
                            "LEFT JOIN pg_attrdef d ON a.attrelid = d.adrelid AND a.attnum = d.adnum " +
                            "WHERE n.nspname = current_schema() AND c.relname = ? AND a.attnum > 0 AND NOT a.attisdropped " +
                            "ORDER BY a.attnum",
                    tableName);
            if (columnList == null || columnList.isEmpty()) {
                return null;
            }
            sb.append("CREATE TABLE ").append(tableName).append(" (\n");
            for (int i = 0; i < columnList.size(); i++) {
                Map<String, Object> col = columnList.get(i);
                String columnName = (String) col.get("column_name");
                String dataType = (String) col.get("data_type");
                Boolean nullable = (Boolean) col.get("nullable");
                Object columnDefault = col.get("column_default");
                Object columnComment = col.get("column_comment");

                sb.append("  ").append(columnName).append(" ").append(dataType);
                if (columnDefault != null) {
                    sb.append(" DEFAULT ").append(columnDefault);
                }
                if (nullable != null && !nullable) {
                    sb.append(" NOT NULL");
                }
                sb.append(",\n");

                // 记录列注释
                if (columnComment != null && !columnComment.toString().isEmpty()) {
                    columnCommentList.add(new String[]{columnName, columnComment.toString()});
                }
            }

            // 2. 获取主键信息
            List<Map<String, Object>> pkList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT string_agg(a.attname, ', ' ORDER BY x.n) AS pk_columns " +
                            "FROM pg_constraint c " +
                            "JOIN pg_namespace n ON n.oid = c.connamespace " +
                            "CROSS JOIN LATERAL unnest(c.conkey) WITH ORDINALITY AS x(attnum, n) " +
                            "JOIN pg_attribute a ON a.attrelid = c.conrelid AND a.attnum = x.attnum " +
                            "WHERE n.nspname = current_schema() AND c.conrelid = ?::regclass AND c.contype = 'p' " +
                            "GROUP BY c.conname",
                    tableName);
            if (pkList != null && !pkList.isEmpty()) {
                Object pkColumns = pkList.get(0).get("pk_columns");
                if (pkColumns != null) {
                    sb.append("  PRIMARY KEY (").append(pkColumns).append("),\n");
                }
            }

            // 3. 获取索引信息（排除主键对应的索引）
            List<Map<String, Object>> indexList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT i.relname AS index_name, ix.indisunique AS is_unique, " +
                            "string_agg(a.attname, ', ' ORDER BY x.n) AS index_columns " +
                            "FROM pg_index ix " +
                            "JOIN pg_class t ON t.oid = ix.indrelid " +
                            "JOIN pg_namespace n ON n.oid = t.relnamespace " +
                            "JOIN pg_class i ON i.oid = ix.indexrelid " +
                            "CROSS JOIN LATERAL unnest(ix.indkey) WITH ORDINALITY AS x(attnum, n) " +
                            "JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = x.attnum " +
                            "WHERE n.nspname = current_schema() AND t.relname = ? AND NOT ix.indisprimary " +
                            "GROUP BY i.relname, ix.indisunique",
                    tableName);
            if (indexList != null) {
                for (Map<String, Object> idx : indexList) {
                    String indexName = (String) idx.get("index_name");
                    Boolean isUnique = (Boolean) idx.get("is_unique");
                    Object indexColumns = idx.get("index_columns");
                    sb.append("  ");
                    if (isUnique != null && isUnique) {
                        sb.append("UNIQUE ");
                    }
                    sb.append("INDEX ").append(indexName).append(" (").append(indexColumns).append("),\n");
                }
            }

            // 去除最后的",\n"，替换为"\n);"
            if (sb.length() > 2 && sb.substring(sb.length() - 2).equals(",\n")) {
                sb.setLength(sb.length() - 2);
            }
            sb.append("\n);");

            // 4. 获取表注释
            List<Map<String, Object>> tableCommentList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT obj_description(c.oid) AS table_comment " +
                            "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace " +
                            "WHERE n.nspname = current_schema() AND c.relname = ?",
                    tableName);
            if (tableCommentList != null && !tableCommentList.isEmpty()) {
                Object tableComment = tableCommentList.get(0).get("table_comment");
                if (tableComment != null && !tableComment.toString().isEmpty()) {
                    sb.append("\nCOMMENT ON TABLE ").append(tableName).append(" IS '")
                            .append(tableComment.toString().replace("'", "''")).append("';");
                }
            }

            // 5. 添加列注释
            for (String[] cc : columnCommentList) {
                sb.append("\nCOMMENT ON COLUMN ").append(tableName).append(".").append(cc[0])
                        .append(" IS '").append(cc[1].replace("'", "''")).append("';");
            }

            return sb.toString();
        } catch (Exception e) {
            logger.error("[{}] PostgreSQL获取建表语句失败: {}", objSeq, tableName, e);
        }
        return null;
    }

    // H2数据库获取建表语句
    private String genCreateTableSqlFromH2(String tableName) {
        try {
            // 1. 获取列定义
            List<Map<String, Object>> columnList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT COLUMN_NAME, DATA_TYPE, IS_NULLABLE, COLUMN_DEFAULT, REMARKS " +
                            "FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? ORDER BY ORDINAL_POSITION",
                    JACGConstants.H2_SCHEMA, tableName);
            if (columnList == null || columnList.isEmpty()) {
                return null;
            }
            StringBuilder sb = new StringBuilder();
            sb.append("CREATE TABLE ").append(tableName).append(" (\n");
            for (int i = 0; i < columnList.size(); i++) {
                Map<String, Object> col = columnList.get(i);
                String columnName = (String) col.get("COLUMN_NAME");
                String dataType = (String) col.get("DATA_TYPE");
                String isNullable = (String) col.get("IS_NULLABLE");
                Object columnDefault = col.get("COLUMN_DEFAULT");
                Object remarks = col.get("REMARKS");

                sb.append("  ").append(columnName).append(" ").append(dataType);
                if (columnDefault != null) {
                    String defaultVal = columnDefault.toString();
                    if (!defaultVal.isEmpty()) {
                        sb.append(" DEFAULT ").append(defaultVal);
                    }
                }
                if ("NO".equalsIgnoreCase(isNullable)) {
                    sb.append(" NOT NULL");
                }
                // 添加列注释
                if (remarks != null && !remarks.toString().isEmpty()) {
                    sb.append(" COMMENT '").append(remarks.toString().replace("'", "''")).append("'");
                }
                sb.append(",\n");
            }

            // 2. 获取主键信息
            List<Map<String, Object>> pkList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT string_agg(kcu.COLUMN_NAME, ', ' ORDER BY kcu.ORDINAL_POSITION) AS pk_columns " +
                            "FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc " +
                            "JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu " +
                            "ON tc.CONSTRAINT_SCHEMA = kcu.CONSTRAINT_SCHEMA " +
                            "AND tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME " +
                            "WHERE tc.CONSTRAINT_SCHEMA = ? AND tc.TABLE_NAME = ? AND tc.CONSTRAINT_TYPE = 'PRIMARY KEY' " +
                            "GROUP BY tc.CONSTRAINT_NAME",
                    JACGConstants.H2_SCHEMA, tableName);
            if (pkList != null && !pkList.isEmpty()) {
                Object pkColumns = pkList.get(0).get("pk_columns");
                if (pkColumns != null) {
                    sb.append("  PRIMARY KEY (").append(pkColumns).append("),\n");
                }
            }

            // 3. 获取索引信息（使用JDBC DatabaseMetaData，因为H2的INFORMATION_SCHEMA.INDEXES列名不确定）
            Map<String, Boolean> indexUniqueMap = new LinkedHashMap<>();
            Map<String, List<String>> indexColumnsMap = new LinkedHashMap<>();
            try (Connection conn = dbOperator.getJdbcTemplate().getDataSource().getConnection()) {
                DatabaseMetaData metaData = conn.getMetaData();
                try (ResultSet rs = metaData.getIndexInfo(null, JACGConstants.H2_SCHEMA, tableName, false, false)) {
                    while (rs.next()) {
                        short type = rs.getShort("TYPE");
                        // 跳过表统计信息
                        if (type == DatabaseMetaData.tableIndexStatistic) {
                            continue;
                        }
                        String indexName = rs.getString("INDEX_NAME");
                        if (indexName == null) {
                            continue;
                        }
                        // 跳过主键索引
                        if (isH2PrimaryKeyIndex(indexName, tableName)) {
                            continue;
                        }
                        boolean nonUnique = rs.getBoolean("NON_UNIQUE");
                        String columnName = rs.getString("COLUMN_NAME");
                        if (columnName == null) {
                            continue;
                        }
                        indexUniqueMap.put(indexName, !nonUnique);
                        indexColumnsMap.computeIfAbsent(indexName, k -> new ArrayList<>()).add(columnName);
                    }
                }
            }
            for (Map.Entry<String, List<String>> entry : indexColumnsMap.entrySet()) {
                String indexName = entry.getKey();
                boolean isUnique = indexUniqueMap.getOrDefault(indexName, false);
                List<String> columns = entry.getValue();
                sb.append("  ");
                if (isUnique) {
                    sb.append("UNIQUE ");
                }
                sb.append("INDEX ").append(indexName).append(" (").append(String.join(", ", columns)).append("),\n");
            }

            // 去除最后的",\n"，替换为"\n);"
            if (sb.length() > 2 && sb.substring(sb.length() - 2).equals(",\n")) {
                sb.setLength(sb.length() - 2);
            }
            sb.append("\n);");

            // 4. 获取表注释
            List<Map<String, Object>> tableInfoList = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT REMARKS FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ?",
                    JACGConstants.H2_SCHEMA, tableName);
            if (tableInfoList != null && !tableInfoList.isEmpty()) {
                Object tableRemarks = tableInfoList.get(0).get("REMARKS");
                if (tableRemarks != null && !tableRemarks.toString().isEmpty()) {
                    sb.append("\nCOMMENT ON TABLE ").append(tableName).append(" IS '")
                            .append(tableRemarks.toString().replace("'", "''")).append("';");
                }
            }

            return sb.toString();
        } catch (Exception e) {
            logger.error("[{}] H2获取建表语句失败: {}", objSeq, tableName, e);
        }
        return null;
    }

    // 判断H2索引是否为主键索引
    private boolean isH2PrimaryKeyIndex(String indexName, String tableName) {
        try {
            List<Map<String, Object>> list = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT COUNT(*) AS cnt FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS " +
                            "WHERE CONSTRAINT_SCHEMA = ? AND TABLE_NAME = ? AND CONSTRAINT_NAME = ? AND CONSTRAINT_TYPE = 'PRIMARY KEY'",
                    JACGConstants.H2_SCHEMA, tableName, indexName);
            if (list != null && !list.isEmpty()) {
                Number cnt = (Number) list.get(0).get("cnt");
                return cnt != null && cnt.intValue() > 0;
            }
        } catch (Exception e) {
            logger.error("[{}] H2判断是否主键索引失败: {}", objSeq, indexName, e);
        }
        return false;
    }

    // 判断H2索引是否为唯一索引
    private boolean checkH2IndexUnique(String indexName, String tableName) {
        try {
            List<Map<String, Object>> list = dbOperator.getJdbcTemplate().queryForList(
                    "SELECT COUNT(*) AS cnt FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS " +
                            "WHERE CONSTRAINT_SCHEMA = ? AND TABLE_NAME = ? AND CONSTRAINT_NAME = ? AND CONSTRAINT_TYPE = 'UNIQUE'",
                    JACGConstants.H2_SCHEMA, tableName, indexName);
            if (list != null && !list.isEmpty()) {
                Number cnt = (Number) list.get(0).get("cnt");
                return cnt != null && cnt.intValue() > 0;
            }
        } catch (Exception e) {
            logger.error("[{}] H2判断索引唯一性失败: {}", objSeq, indexName, e);
        }
        return false;
    }
}
