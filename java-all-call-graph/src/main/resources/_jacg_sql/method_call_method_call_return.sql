CREATE TABLE IF NOT EXISTS jacg_method_call_method_call_return_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  call_id int NOT NULL COMMENT '方法调用序号，从1开始',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  array_flag int NOT NULL COMMENT '是否为数组格式，1:是，0:否',
  use_return_call_id int NOT NULL COMMENT '返回值被使用的方法调用序号，从1开始',
  callee_method_hash varchar(32) COLLATE utf8_bin NOT NULL COMMENT '被调用方，方法hash+字节数',
  callee_simple_class_name varchar(300) COLLATE utf8_bin NOT NULL COMMENT '被调用方，唯一类名（完整类名或简单类名），需要有单列索引',
  callee_method_name varchar(300) COLLATE utf8_bin NOT NULL COMMENT '被调用方，方法名',
  callee_full_method text COLLATE utf8_bin NOT NULL COMMENT '被调用方，完整方法（类名+方法名+参数）',
  callee_return_type varchar(255) NOT NULL COMMENT '被调用方，方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_mcmcr_eemh_{appName}(callee_method_hash),
  INDEX idx_mcmcr_eecm_{appName}(callee_simple_class_name(255), callee_method_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用使用方法调用返回值信息表';