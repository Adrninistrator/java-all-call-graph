CREATE TABLE IF NOT EXISTS jacg_method_call_raw_callee_{appName} (
  call_id int NOT NULL COMMENT '方法调用序号，从1开始',
  raw_callee_class_name varchar(300) NOT NULL COMMENT '原始的被调用完整类名',
  PRIMARY KEY (call_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用被调用对象的原始类型表';