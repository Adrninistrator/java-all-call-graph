CREATE TABLE IF NOT EXISTS jacg_class_signature_generics_type_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  seq int NOT NULL COMMENT '类的签名中泛型的序号，从0开始',
  type_variables_name varchar(255) NOT NULL COMMENT '类的签名中的泛型类型变量名称',
  generics_extends_class_name varchar(300) NOT NULL COMMENT '类的签名中的泛型的父类类名',
  class_name varchar(300) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_csgt_scn_{appName}(simple_class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的签名中的泛型信息';