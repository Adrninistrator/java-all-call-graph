CREATE TABLE IF NOT EXISTS jacg_method_call_field_actual_type{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  caller_simple_class_name varchar(300) NOT NULL COMMENT '调用方唯一类名（完整类名或简单类名）',
  caller_class_name varchar(300) NOT NULL COMMENT '调用方完整类名',
  caller_method_name varchar(300) NOT NULL COMMENT '调用方方法名',
  caller_full_method varchar(500) NOT NULL COMMENT '调用方，完整方法（类名+方法名+参数）',
  caller_line_number int NOT NULL COMMENT '方法调用指令对应的代码行号',
  field_type varchar(255) NOT NULL COMMENT '被调用方字段声明类型',
  field_name varchar(200) NOT NULL COMMENT '被调用方字段名称',
  field_actual_type varchar(255) NOT NULL COMMENT '被调用方字段实际类型（运行时多态，与声明类型不同才记录）',
  PRIMARY KEY (record_id),
  INDEX idx_mcfat_cscncmncln{appName}(caller_simple_class_name(255), caller_method_name, caller_line_number),
  INDEX idx_mcfat_cscnfn{appName}(caller_simple_class_name(255), field_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用被调用对象为非静态字段时的实际类型（运行时多态）表';
