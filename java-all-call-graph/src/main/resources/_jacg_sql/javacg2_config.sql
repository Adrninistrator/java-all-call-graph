CREATE TABLE if not exists jacg_javacg2_config_{appName} (
  config_file_name varchar(100) NOT NULL COMMENT '配置文件名',
  config_key varchar(100) NOT NULL COMMENT '配置参数名，List/Set类型的参数代表序号',
  config_value varchar(255) NOT NULL COMMENT '配置参数值',
  config_type varchar(10) NOT NULL COMMENT '配置参数类型',
  PRIMARY KEY (config_file_name, config_key, config_value)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='java-callgraph2组件使用的配置参数信息表';