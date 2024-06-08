CREATE TABLE if not exists jacg_method_argument_{appName} (
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  arg_seq int NOT NULL COMMENT '参数序号，从0开始',
  simple_arg_type varchar(255) NOT NULL COMMENT '参数类型唯一类名',
  arg_name varchar(255) NOT NULL COMMENT '参数名称',
  arg_type varchar(255) NOT NULL COMMENT '参数类型类名',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  PRIMARY KEY (method_hash, arg_seq),
  INDEX idx_marg_sat_{appName}(simple_arg_type),
  INDEX idx_marg_scn_{appName}(simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法参数类型';