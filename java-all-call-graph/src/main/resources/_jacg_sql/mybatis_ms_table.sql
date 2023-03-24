CREATE TABLE if not exists jacg_mybatis_ms_table_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  mapper_simple_class_name varchar(500) NOT NULL COMMENT 'MyBatis Mapper唯一类名',
  mapper_method_name varchar(300) NOT NULL COMMENT 'MyBatis Mapper方法名',
  sql_statement varchar(5) NOT NULL COMMENT 'sql语句类型，s:select，su:select_for_update，i:insert_into，ii:insert_ignore_into，iu:insert_into_on_duplicate_key_update，r:replace_into，u:update，del:delete，a:alter_table，t:truncate_table，c:create_table，drop:drop_table',
  table_seq tinyint NOT NULL COMMENT '数据库表序号',
  table_name varchar(100) NOT NULL COMMENT '数据库表名',
  mapper_class_name varchar(500) NOT NULL COMMENT 'MyBatis Mapper完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_mmt_scm_{appName}(mapper_simple_class_name(255), mapper_method_name(255), sql_statement, table_seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='MyBatis数据库表信息（使用MySQL）';