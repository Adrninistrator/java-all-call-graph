CREATE TABLE IF NOT EXISTS jacg_method_call_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  call_id int NOT NULL COMMENT '方法调用序号，从1开始',
  obj_args_seq int NOT NULL COMMENT '被调用对象或参数序号，0代表被调用对象，1开始为参数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  type varchar(10) NOT NULL COMMENT '类型，含义参考 JavaCG2MethodCallInfoTypeEnum 类',
  array_flag int NOT NULL COMMENT '是否为数组格式，1:是，0:否',
  array_collection_seq int NOT NULL COMMENT '数组值组合序号，从0开始，非数组时为-1',
  array_dimensions int NOT NULL COMMENT '数组维度，从1开始，非数组时为0',
  array_index varchar(100) DEFAULT NULL COMMENT '数组下标，逗号分隔，如"0"、"0,1"、"0,1,2"',
  value_type varchar(30) DEFAULT NULL COMMENT '值的类型，含义参考 JavaCG2ConstantTypeEnum 类',
  the_value text COLLATE utf8mb4_bin NOT NULL COMMENT '对应的值',
  caller_method_hash varchar(32) NOT NULL COMMENT '调用方，方法hash+字节数',
  PRIMARY KEY (record_id),
  INDEX idx_mci_cios_{appName}(call_id, obj_args_seq, seq),
  INDEX idx_mci_value_{appName}(the_value(191)),
  INDEX idx_mci_ermhos_{appName}(caller_method_hash, obj_args_seq, seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用信息表';
