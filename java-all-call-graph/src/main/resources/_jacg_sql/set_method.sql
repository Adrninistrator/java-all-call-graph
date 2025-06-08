CREATE TABLE if not exists jacg_set_method_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  method_name varchar(200) NOT NULL COMMENT '方法名',
  field_name varchar(200) NOT NULL COMMENT '字段名',
  field_category varchar(5) NOT NULL COMMENT '字段分类，J:JDK中的类型，C:自定义类型，GJ:泛型类型，只涉及JDK中的类型，GC:泛型类型，涉及自定义类型',
  simple_field_type_nad varchar(255) DEFAULT NULL COMMENT '字段类型唯一类名（不包含数组标志）',
  field_type_nad varchar(255) NOT NULL COMMENT '字段类型（不包含数组标志）',
  array_dimensions tinyint NOT NULL COMMENT '字段数组类型的维度，为0代表不是数组类型',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_sm_cm_{appName}(simple_class_name, method_name),
  INDEX idx_sm_cf_{appName}(simple_class_name, field_name),
  INDEX idx_sm_sft_{appName}(simple_field_type_nad),
  INDEX idx_sm_mh_{appName}(method_hash)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='dto的set方法及字段';