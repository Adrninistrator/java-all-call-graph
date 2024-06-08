CREATE TABLE if not exists jacg_method_throw_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  throw_offset int NOT NULL COMMENT 'throw指令的偏移量',
  line_number int NOT NULL COMMENT 'throw的代码行号',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  throw_exception_type varchar(255) NOT NULL COMMENT 'throw的异常类型',
  throw_flag varchar(5) NOT NULL COMMENT 'throw的标志，ce:catch的异常对象，mcr:方法调用返回值，unk:未知情况',
  catch_start_offset int NULL COMMENT '抛出异常属于catch的异常对象时，对应的catch代码块开始指令偏移量',
  catch_exception_variable_name varchar(255) NULL COMMENT '抛出异常对应的catch的异常对象变量名称',
  call_id int NULL COMMENT '抛出异常属于方法调用返回值时，对应的方法调用ID',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  PRIMARY KEY (record_id),
  INDEX idx_mt_mhcso_{appName}(method_hash, catch_start_offset),
  INDEX idx_mt_mhci_{appName}(method_hash, call_id),
  INDEX idx_mt_scn_{appName}(simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法中throw的异常信息';