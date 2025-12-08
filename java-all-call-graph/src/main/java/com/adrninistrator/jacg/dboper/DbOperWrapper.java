package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.comparator.Comparator4FullMethodWithReturnType;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.callgraph.CallGraphNode4Caller;
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
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
            return simpleClassName;
        }

        // 执行根据任务中的简单类名或完整类名获取唯一类名
        simpleClassName = doGetSimpleClassNameInTask(className);
        simpleClassNameInTaskMap.put(className, simpleClassName);
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
            return className;
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
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_TOP_METHOD;
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
}
