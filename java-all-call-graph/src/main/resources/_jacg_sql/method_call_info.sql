CREATE TABLE if not exists jacg_method_call_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  call_id int NOT NULL COMMENT '方法调用序号，从1开始',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  caller_method_hash varchar(30) NOT NULL COMMENT '调用方，方法hash+字节数',
  type varchar(10) NOT NULL COMMENT '类型，含义参考 JavaCGMethodCallInfoTypeEnum 类',
  array_flag int NOT NULL COMMENT '是否为数组格式，1:是，0:否',
  value_type varchar(30) NULL COMMENT '值的类型，含义参考 JavaCGConstantTypeEnum 类',
  the_value text COLLATE utf8mb4_bin NOT NULL COMMENT '对应的值',
  PRIMARY KEY (record_id),
  INDEX idx_mci_cos_{appName}(call_id, obj_args_seq, seq),
  INDEX idx_mci_ctv_{appName}(caller_method_hash, type, the_value(191))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用信息表';