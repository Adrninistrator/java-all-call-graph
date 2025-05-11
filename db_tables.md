# 1. jacg_business_data 方法调用业务功能数据表

- 表名前缀

jacg_business_data

- 注释

方法调用业务功能数据表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|call_id|INT|10|方法调用序号，从1开始|
|data_type|VARCHAR|30|数据类型，默认类型参考 BusinessDataTypeEnum 枚举类，也支持自定义类型|
|data_value|TEXT|65535|数据内容，JSON字符串格式|

# 2. jacg_class_annotation 类上的注解信息表

- 表名前缀

jacg_class_annotation

- 注释

类上的注解信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|annotation_name|VARCHAR|255|注解类名|
|attribute_name|VARCHAR|200|注解属性名称，空字符串代表无注解属性|
|attribute_type|VARCHAR|5|注解属性类型，参考AnnotationAttributesTypeEnum类|
|attribute_value|TEXT|65535|注解属性值|
|class_name|VARCHAR|255|完整类名|

# 3. jacg_class_ext_impl_generics_type 类的继承或实现的泛型信息

- 表名前缀

jacg_class_ext_impl_generics_type

- 注释

类的继承或实现的泛型信息

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|ext_type|CHAR|1|继承或实现类型，e:继承，i:实现|
|seq|INT|10|继承或实现的序号，从0开始|
|super_itf_simple_class_name|VARCHAR|255|父类或接口的唯一类名|
|generics_seq|TINYINT|3|类的继承或实现中的泛型类型序号，从0开始|
|simple_generics_type_nad|VARCHAR|255|类的继承或实现中的泛型类型唯一类名（不包含数组标志）|
|generics_array_dimensions|TINYINT|3|类的继承或实现中的泛型数组类型的维度，为0代表不是数组类型|
|type_variables_name|VARCHAR|255|类的继承或实现中的泛型类型变量名称|
|generics_category|VARCHAR|5|类的继承或实现中的泛型类型分类，J:JDK中的类型，C:自定义类型|
|generics_type_nad|VARCHAR|255|类的继承或实现中的泛型类型类名（不包含数组标志）|
|class_name|VARCHAR|255|完整类名|
|super_itf_class_name|VARCHAR|255|父类或接口的类名|

# 4. jacg_class_info 类的信息表

- 表名前缀

jacg_class_info

- 注释

类的信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|access_flags|INT|10|类的access_flags|
|class_name|VARCHAR|255|完整类名|
|package_name|VARCHAR|255|包名|
|package_level|INT|10|包名层级，等于包名中的.数量+1|
|class_file_hash|VARCHAR|32|类文件的HASH值（MD5）|
|jar_num|INT|10|类所在的Jar包序号|
|class_path_in_jar|VARCHAR|300|类在jar包中的路径|

# 5. jacg_class_name 类名信息表

- 表名前缀

jacg_class_name

- 注释

类名信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|class_name|VARCHAR|255|完整类名|
|simple_class_name|VARCHAR|255|唯一类名|
|duplicate_class|TINYINT|3|是否存在同名类，1:是，0:否|

# 6. jacg_class_reference 类的引用关系表

- 表名前缀

jacg_class_reference

- 注释

类的引用关系表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|class_name|VARCHAR|255|引用的完整类名|
|simple_class_name|VARCHAR|255|引用的唯一类名|
|referenced_class_name|VARCHAR|255|被引用的完整类名|
|referenced_simple_class_name|VARCHAR|255|被引用的唯一类名|

# 7. jacg_class_signature_generics_type 类的签名中的泛型信息

- 表名前缀

jacg_class_signature_generics_type

- 注释

类的签名中的泛型信息

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|seq|INT|10|类的签名中泛型的序号，从0开始|
|type_variables_name|VARCHAR|255|类的签名中的泛型类型变量名称|
|generics_extends_class_name|VARCHAR|255|类的签名中的泛型的父类类名|
|class_name|VARCHAR|255|完整类名|

# 8. jacg_dup_class_info 重复同名类的信息表

- 表名前缀

jacg_dup_class_info

- 注释

重复同名类的信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|access_flags|INT|10|类的access_flags|
|class_name|VARCHAR|255|完整类名|
|package_name|VARCHAR|255|包名|
|package_level|INT|10|包名层级，等于包名中的.数量+1|
|class_file_hash|VARCHAR|32|类文件的HASH值（MD5）|
|jar_num|INT|10|类所在的Jar包序号|
|class_path_in_jar|VARCHAR|300|类在jar包中的路径|

# 9. jacg_dup_method_info 重复同名类的方法的信息表

- 表名前缀

jacg_dup_method_info

- 注释

重复同名类的方法的信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|access_flags|INT|10|方法的access_flags|
|method_name|VARCHAR|200|方法名|
|simple_return_type_nad|VARCHAR|255|返回类型唯一类名（不包含数组标志）|
|return_type_nad|VARCHAR|255|返回类型类名（不包含数组标志）|
|return_array_dimensions|TINYINT|3|返回类型数组的维度，为0代表不是数组类型|
|return_type|VARCHAR|255|返回类型类名（包含数组标志）|
|return_category|VARCHAR|5|返回类型分类，J:JDK中的类型，C:自定义类型|
|return_exists_generics_type|TINYINT|3|返回类型是否存在泛型类型，1:是，0:否|
|class_name|VARCHAR|255|完整类名|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|method_instructions_hash|VARCHAR|32|方法指令的HASH值（MD5），可能为空字符串|
|jar_num|INT|10|方法所在的Jar包序号|

# 10. jacg_enum_init_arg_field 枚举类构造函数参数与字段赋值关系表

- 表名前缀

jacg_enum_init_arg_field

- 注释

枚举类构造函数参数与字段赋值关系表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|枚举唯一类名|
|arg_seq|INT|10|枚举类构造函数用于赋值的参数序号（从1开始）|
|field_type|VARCHAR|255|枚举类构造函数被赋值的字段类型|
|field_name|VARCHAR|255|枚举类构造函数被赋值的字段名|
|class_name|VARCHAR|255|枚举类完整类名|
|full_method|TEXT|65535|枚举类构造函数完整方法（类名+方法名+参数）|

# 11. jacg_enum_init_assign_info 枚举类初始化赋值信息表

- 表名前缀

jacg_enum_init_assign_info

- 注释

枚举类初始化赋值信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|枚举唯一类名|
|const_name|VARCHAR|255|枚举常量名称|
|ordinal|INT|10|枚举字段序号|
|arg_seq|INT|10|通过枚举类构造函数被赋值的参数序号（从1开始，最小为3）|
|field_type|VARCHAR|255|通过枚举类构造函数被赋值的字段类型|
|field_value|VARCHAR|255|通过枚举类构造函数被赋值的字段值|
|class_name|VARCHAR|255|枚举完整类名|
|full_method|TEXT|65535|枚举类构造函数完整方法（类名+方法名+参数）|

# 12. jacg_extends_impl 继承与实现相关信息表

- 表名前缀

jacg_extends_impl

- 注释

继承与实现相关信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|class_name|VARCHAR|255|完整类名|
|access_flags|INT|10|类的access_flags|
|type|CHAR|1|类型，e:继承，i:实现|
|seq|INT|10|序号，从0开始，支持实现多个接口|
|exists_downward_classes|TINYINT|3|是否存在子类或子接口，0:不存在；1:存在|
|upward_simple_class_name|VARCHAR|255|父类或接口的唯一类名|
|upward_class_name|VARCHAR|255|父类或接口的完整类名|

# 13. jacg_field_annotation 字段上的注解信息表

- 表名前缀

jacg_field_annotation

- 注释

字段上的注解信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|field_name|VARCHAR|200|字段名称|
|annotation_name|VARCHAR|255|注解类名|
|attribute_name|VARCHAR|200|注解属性名称，空字符串代表无注解属性|
|attribute_type|VARCHAR|5|注解属性类型，参考AnnotationAttributesTypeEnum类|
|attribute_value|TEXT|65535|注解属性值|
|class_name|VARCHAR|255|完整类名|

# 14. jacg_field_generics_type 非静态字段中涉及的泛型类型

- 表名前缀

jacg_field_generics_type

- 注释

非静态字段中涉及的泛型类型

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|field_name|VARCHAR|200|字段名|
|type|VARCHAR|5|类型，t:字段类型，gt:字段中的泛型类型|
|type_seq|TINYINT|3|类型序号，字段类型固定为0，字段的泛型类型从0开始|
|simple_generics_type_nad|VARCHAR|255|非静态字段类型或其中的泛型类型唯一类名（不包含数组标志）|
|generics_array_dimensions|TINYINT|3|非静态字段中的泛型数组类型的维度，为0代表不是数组类型|
|type_variables_name|VARCHAR|255|非静态字段中的泛型类型变量名称|
|wildcard|VARCHAR|8|非静态字段中的泛型通配符|
|reference_type|VARCHAR|255|非静态字段中的泛型通配符引用的类型|
|generics_category|VARCHAR|5|非静态字段中的泛型类型分类，J:JDK中的类型，C:自定义类型|
|generics_type_nad|VARCHAR|255|非静态字段类型或其中的泛型类型类名（不包含数组标志）|
|class_name|VARCHAR|255|完整类名|

# 15. jacg_field_info 字段信息表

- 表名前缀

jacg_field_info

- 注释

字段信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|field_name|VARCHAR|350|字段名称|
|field_type_nad|VARCHAR|255|字段类型（不包含数组标志）|
|array_dimensions|TINYINT|3|字段数组类型的维度，为0代表不是数组类型|
|field_category|VARCHAR|5|字段类型分类，J:JDK中的类型，C:自定义类型|
|modifiers|VARCHAR|10|字段修饰符|
|primitive_type|TINYINT|3|基本类型，1:是，0:否|
|static_flag|TINYINT|3|static标志，1:是，0:否|
|final_flag|TINYINT|3|final标志，1:是，0:否|
|exists_get_method|TINYINT|3|是否存在对应的get方法，1:是，0:否|
|exists_set_method|TINYINT|3|是否存在对应的set方法，1:是，0:否|
|exists_generics_type|TINYINT|3|是否存在泛型类型，1:是，0:否|
|class_name|VARCHAR|255|完整类名|

# 16. jacg_field_relationship 通过get/set方法关联的字段关系

- 表名前缀

jacg_field_relationship

- 注释

通过get/set方法关联的字段关系

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|fld_relationship_id|INT|10|字段关联关系id，从1开始|
|get_method_call_id|INT|10|get方法调用序号，从1开始|
|set_method_call_id|INT|10|set方法调用序号，从1开始|
|caller_full_method|TEXT|65535|调用方，完整方法（类名+方法名+参数）|
|caller_line_number|INT|10|调用方，源代码行号|
|get_simple_class_name|VARCHAR|255|get方法唯一类名|
|get_method_name|VARCHAR|200|get方法方法名|
|get_class_name|VARCHAR|255|get方法完整类名|
|set_simple_class_name|VARCHAR|255|set方法唯一类名|
|set_method_name|VARCHAR|200|set方法方法名|
|set_class_name|VARCHAR|255|set方法完整类名|
|valid|TINYINT|3|关联关系是否有效，1:是，0:否|
|type|VARCHAR|10|关联关系类型，参考 java-callgraph2 项目 JavaCG2FieldRelationshipTypeEnum 类|
|relationship_flags|INT|10|字段关联关系标志|
|bean_util_call_id|INT|10|BeanUtil方法调用序号，从1开始|
|bean_util_method|TEXT|65535|BeanUtil属性拷贝方法|

# 17. jacg_get_method dto的get方法及字段

- 表名前缀

jacg_get_method

- 注释

dto的get方法及字段

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|method_name|VARCHAR|200|方法名|
|field_name|VARCHAR|200|字段名|
|field_category|VARCHAR|5|字段分类，J:JDK中的类型，C:自定义类型，GJ:泛型类型，只涉及JDK中的类型，GC:泛型类型，涉及自定义类型|
|simple_field_type_nad|VARCHAR|255|字段类型唯一类名（不包含数组标志）|
|field_type_nad|VARCHAR|255|字段类型（不包含数组标志）|
|array_dimensions|TINYINT|3|字段数组类型的维度，为0代表不是数组类型|
|class_name|VARCHAR|255|完整类名|
|method_hash|VARCHAR|30|方法hash+字节数|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 18. jacg_inner_class 内部类的信息表

- 表名前缀

jacg_inner_class

- 注释

内部类的信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|inner_simple_class_name|VARCHAR|255|内部类唯一类名|
|inner_class_name|VARCHAR|255|内部类完整类名|
|outer_simple_class_name|VARCHAR|255|外部类唯一类名|
|outer_class_name|VARCHAR|255|外部类完整类名|
|anonymous_class|TINYINT|3|是否为匿名内部类，1:是，0:否|

# 19. jacg_jar_info jar文件信息表

- 表名前缀

jacg_jar_info

- 注释

jar文件信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|jar_num|INT|10|jar文件序号|
|jar_type|VARCHAR|5|jar文件类型，J: jar/war文件，D: 目录，JIJ: jar/war文件中的jar，R: 解析结果文件保存目录|
|jar_path_hash|VARCHAR|30|外层jar文件路径HASH+字节数|
|jar_full_path|TEXT|65535|外层jar文件完整路径|
|jar_file_name|VARCHAR|255|外层jar文件名|
|jar_file_name_head|VARCHAR|255|外层jar文件名，不包含版本号及文件后缀名|
|jar_file_name_ext|VARCHAR|255|外层jar文件名的后缀名，以.开头|
|last_modified_time|VARCHAR|20|外层jar文件上次修改时间（精度到秒）|
|jar_file_hash|VARCHAR|32|外层jar文件HASH|
|inner_jar_path|TEXT|65535|jar/war文件中的jar文件路径|
|inner_jar_file_name|VARCHAR|255|jar/war文件中的jar文件名|
|import_time|DATETIME|23|导入时间|

# 20. jacg_javacg2_config java-callgraph2组件使用的配置参数信息表

- 表名前缀

jacg_javacg2_config

- 注释

java-callgraph2组件使用的配置参数信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|config_file_name|VARCHAR|100|配置文件名|
|config_key|VARCHAR|100|配置参数名，List/Set类型的参数代表序号|
|config_value|VARCHAR|255|配置参数值|
|config_type|VARCHAR|10|配置参数类型|

# 21. jacg_lambda_method_info Lambda表达式方法信息表

- 表名前缀

jacg_lambda_method_info

- 注释

Lambda表达式方法信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|call_id|INT|10|方法调用序号，从1开始|
|lambda_callee_class_name|VARCHAR|255|Lambda表达式被调用方类名|
|lambda_callee_method_name|VARCHAR|200|Lambda表达式被调用方方法名|
|lambda_callee_full_method|TEXT|65535|Lambda表达式被调用方完整方法（类名+方法名+参数）|
|lambda_next_class_name|VARCHAR|255|Lambda表达式下一个被调用类名|
|lambda_next_method_name|VARCHAR|200|Lambda表达式下一个被调用方法名|
|lambda_next_full_method|TEXT|65535|Lambda表达式下一个被调用完整方法（类名+方法名+参数）|
|lambda_next_is_stream|TINYINT|3|下一个被调用方法是否为Stream，1:是，0:否|
|lambda_next_is_intermediate|TINYINT|3|下一个被调用方法是否为Stream的intermediate（中间）操作，1:是，0:否|
|lambda_next_is_terminal|TINYINT|3|下一个被调用方法是否为Stream的terminal（终端）操作，1:是，0:否|

# 22. jacg_method_annotation 方法上的注解信息表

- 表名前缀

jacg_method_annotation

- 注释

方法上的注解信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|annotation_name|VARCHAR|255|注解类名|
|attribute_name|VARCHAR|200|注解属性名称，空字符串代表无注解属性|
|attribute_type|VARCHAR|5|注解属性类型，参考AnnotationAttributesTypeEnum类|
|attribute_value|TEXT|65535|注解属性值|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|
|simple_class_name|VARCHAR|255|唯一类名|

# 23. jacg_method_arg_annotation 方法参数上的注解信息表

- 表名前缀

jacg_method_arg_annotation

- 注释

方法参数上的注解信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|arg_seq|INT|10|参数序号，从0开始|
|annotation_name|VARCHAR|255|注解类名|
|attribute_name|VARCHAR|200|注解属性名称，空字符串代表无注解属性|
|attribute_type|VARCHAR|5|注解属性类型，参考AnnotationAttributesTypeEnum类|
|attribute_value|TEXT|65535|注解属性值|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|
|simple_class_name|VARCHAR|255|唯一类名|

# 24. jacg_method_arg_generics_type 方法参数泛型类型

- 表名前缀

jacg_method_arg_generics_type

- 注释

方法参数泛型类型

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|seq|INT|10|参数序号，从0开始|
|type|VARCHAR|5|类型，t:参数类型，gt:参数中的泛型类型|
|type_seq|TINYINT|3|类型序号，参数类型固定为0，参数中的泛型类型从0开始|
|simple_generics_type_nad|VARCHAR|255|方法参数类型或其中的泛型类型唯一类名（不包含数组标志）|
|generics_array_dimensions|TINYINT|3|方法参数中的泛型数组类型的维度，为0代表不是数组类型|
|type_variables_name|VARCHAR|255|方法参数中的泛型类型变量名称|
|wildcard|VARCHAR|8|方法参数中的泛型通配符|
|reference_type|VARCHAR|255|方法参数中的泛型通配符引用的类型|
|generics_category|VARCHAR|5|方法参数中的泛型类型分类，J:JDK中的类型，C:自定义类型|
|generics_type_nad|VARCHAR|255|方法参数类型或其中的泛型类型类名（不包含数组标志）|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 25. jacg_method_argument 方法参数类型

- 表名前缀

jacg_method_argument

- 注释

方法参数类型

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|arg_seq|INT|10|参数序号，从0开始|
|simple_class_name|VARCHAR|255|唯一类名|
|simple_arg_type_nad|VARCHAR|255|参数类型唯一类名（不包含数组标志）|
|arg_name|VARCHAR|255|参数名称|
|array_dimensions|TINYINT|3|参数数组类型的维度，为0代表不是数组类型|
|arg_category|VARCHAR|5|参数类型分类，J:JDK中的类型，C:自定义类型|
|exists_generics_type|TINYINT|3|是否存在泛型类型，1:是，0:否|
|arg_type_nad|VARCHAR|255|参数类型类名（不包含数组标志）|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 26. jacg_method_call_info 方法调用信息表

- 表名前缀

jacg_method_call_info

- 注释

方法调用信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|call_id|INT|10|方法调用序号，从1开始|
|obj_args_seq|INT|10|被调用对象或参数序号，0代表被调用对象，1开始为参数|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|caller_method_hash|VARCHAR|30|调用方，方法hash+字节数|
|type|VARCHAR|10|类型，含义参考 JavaCG2MethodCallInfoTypeEnum 类|
|array_flag|INT|10|是否为数组格式，1:是，0:否|
|value_type|VARCHAR|30|值的类型，含义参考 JavaCG2ConstantTypeEnum 类|
|the_value|TEXT|65535|对应的值|

# 27. jacg_method_call_method_call_return 方法调用使用方法调用返回值信息表

- 表名前缀

jacg_method_call_method_call_return

- 注释

方法调用使用方法调用返回值信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|call_id|INT|10|方法调用序号，从1开始|
|obj_args_seq|INT|10|被调用对象或参数序号，0代表被调用对象，1开始为参数|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|array_flag|INT|10|是否为数组格式，1:是，0:否|
|use_return_call_id|INT|10|返回值被使用的方法调用序号，从1开始|
|callee_method_hash|VARCHAR|30|被调用方，方法hash+字节数|
|callee_simple_class_name|VARCHAR|255|被调用方，唯一类名（完整类名或简单类名），需要有单列索引|
|callee_method_name|VARCHAR|300|被调用方，方法名|
|callee_full_method|TEXT|65535|被调用方，完整方法（类名+方法名+参数）|
|callee_return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 28. jacg_method_call_static_field 方法调用使用静态字段信息表

- 表名前缀

jacg_method_call_static_field

- 注释

方法调用使用静态字段信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|call_id|INT|10|方法调用序号，从1开始|
|obj_args_seq|INT|10|被调用对象或参数序号，0代表被调用对象，1开始为参数|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|caller_method_hash|VARCHAR|30|调用方，方法hash+字节数|
|simple_class_name|VARCHAR|255|静态字段所在类唯一类名|
|field_name|VARCHAR|200|静态字段名称|
|simple_field_type|VARCHAR|255|静态字段类型唯一类名|
|class_name|VARCHAR|255|静态字段所在类完整类名|
|field_type|VARCHAR|255|静态字段类型|

# 29. jacg_method_call 方法调用关系表

- 表名前缀

jacg_method_call

- 注释

方法调用关系表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|call_id|INT|10|方法调用序号，从1开始|
|enabled|TINYINT|3|是否启用，1:启用，0:未启用|
|call_type|VARCHAR|10|调用类型，参考 JavaCG2CallTypeEnum 枚举类|
|caller_method_hash|VARCHAR|30|调用方，方法hash+字节数|
|caller_simple_class_name|VARCHAR|255|调用方，唯一类名（完整类名或简单类名）|
|caller_method_name|VARCHAR|300|调用方，方法名|
|caller_full_method|TEXT|65535|调用方，完整方法（类名+方法名+参数）|
|caller_line_number|INT|10|调用方法源代码行号|
|caller_return_type|VARCHAR|255|调用方法的返回类型|
|callee_method_hash|VARCHAR|30|被调用方，方法hash+字节数|
|callee_simple_class_name|VARCHAR|255|被调用方，唯一类名（完整类名或简单类名），需要有单列索引|
|callee_method_name|VARCHAR|300|被调用方，方法名|
|callee_full_method|TEXT|65535|被调用方，完整方法（类名+方法名+参数）|
|callee_array_dimensions|TINYINT|3|被调用方，对象数组的维度，为0代表不是数组类型|
|callee_obj_type|VARCHAR|10|被调用对象类型，t:调用当前实例的方法，sf:调用静态字段的方法，f:调用字段的方法，v:调用其他变量的方法|
|raw_return_type|VARCHAR|255|被调用方法原始的返回类型|
|actual_return_type|VARCHAR|255|被调用方法实际的返回类型|
|call_flags|INT|10|方法调用标志|
|caller_jar_num|INT|10|调用方法Jar包序号|
|callee_jar_num|INT|10|被调用方法Jar包序号|
|description|VARCHAR|255|描述信息，默认为空|

# 30. jacg_method_catch 方法的catch信息

- 表名前缀

jacg_method_catch

- 注释

方法的catch信息

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|method_name|VARCHAR|300|方法名称|
|simple_catch_exception_type|VARCHAR|255|catch捕获的异常类型唯一类名|
|catch_exception_type|VARCHAR|255|catch捕获的异常类型|
|catch_flag|VARCHAR|20|catch标志，switch: 编译器为switch生成的catch代码块，try-with-resource: 编译器为try-with-resource生成的catch代码块|
|try_start_line_number|INT|10|try代码块开始代码行号|
|try_end_line_number|INT|10|try代码块结束代码行号|
|try_min_call_id|INT|10|try代码块最小方法调用ID|
|try_max_call_id|INT|10|try代码块最大方法调用ID|
|catch_start_offset|INT|10|catch代码块开始指令偏移量|
|catch_end_offset|INT|10|catch代码块结束指令偏移量|
|catch_start_line_number|INT|10|catch代码块开始代码行号|
|catch_end_line_number|INT|10|catch代码块结束代码行号|
|catch_min_call_id|INT|10|catch代码块最小方法调用ID|
|catch_max_call_id|INT|10|catch代码块最大方法调用ID|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 31. jacg_method_finally 方法的finally信息

- 表名前缀

jacg_method_finally

- 注释

方法的finally信息

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|try_catch|VARCHAR|7|当前的finally对应try或catch|
|try_catch_start_line_number|INT|10|try或catch代码块开始代码行号|
|try_catch_end_line_number|INT|10|try或catch代码块结束代码行号|
|try_catch_min_call_id|INT|10|try或catch代码块最小方法调用ID|
|try_catch_max_call_id|INT|10|try或catch代码块最大方法调用ID|
|finally_start_line_number|INT|10|finally代码块开始代码行号|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 32. jacg_method_info 方法的信息表

- 表名前缀

jacg_method_info

- 注释

方法的信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|access_flags|INT|10|方法的access_flags|
|method_name|VARCHAR|300|方法名|
|simple_return_type_nad|VARCHAR|255|返回类型唯一类名（不包含数组标志）|
|return_type_nad|VARCHAR|255|返回类型类名（不包含数组标志）|
|return_array_dimensions|TINYINT|3|返回类型数组的维度，为0代表不是数组类型|
|return_type|VARCHAR|255|返回类型类名（包含数组标志）|
|return_category|VARCHAR|5|返回类型分类，J:JDK中的类型，C:自定义类型|
|return_exists_generics_type|TINYINT|3|返回类型是否存在泛型类型，1:是，0:否|
|class_name|VARCHAR|255|完整类名|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|method_instructions_hash|VARCHAR|32|方法指令的HASH值（MD5），可能为空字符串|
|jar_num|INT|10|方法所在的Jar包序号|

# 33. jacg_method_line_number 方法代码行号信息表

- 表名前缀

jacg_method_line_number

- 注释

方法代码行号信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|method_name|VARCHAR|300|方法名|
|min_line_number|INT|10|起始代码行号|
|max_line_number|INT|10|结束代码行号|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 34. jacg_method_return_arg_seq 方法返回值对应的方法参数序号信息表

- 表名前缀

jacg_method_return_arg_seq

- 注释

方法返回值对应的方法参数序号信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|return_arg_seq|INT|10|方法返回值对应的方法参数序号，从0开始|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|
|equivalent_conversion|TINYINT|3|是否返回等值转换前的方法参数，1:是，0:否|

# 35. jacg_method_return_call_id 方法返回值对应的方法调用序号信息表

- 表名前缀

jacg_method_return_call_id

- 注释

方法返回值对应的方法调用序号信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|return_call_id|INT|10|方法返回值对应的方法调用序号，从1开始|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|
|equivalent_conversion|TINYINT|3|是否返回等值转换前的方法调用，1:是，0:否|

# 36. jacg_method_return_const_value 方法返回的常量值（含null）

- 表名前缀

jacg_method_return_const_value

- 注释

方法返回的常量值（含null）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|seq|INT|10|某个方法返回的常量值序号，从0开始|
|const_type|VARCHAR|30|常量类型，含义参考 JavaCG2ConstantTypeEnum 类|
|const_value|TEXT|65535|常量的值|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 37. jacg_method_return_field_info 方法返回的字段（含枚举）

- 表名前缀

jacg_method_return_field_info

- 注释

方法返回的字段（含枚举）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|seq|INT|10|某个方法返回的字段信息序号，从0开始|
|static_field|TINYINT|3|方法返回的字段是否为静态，1:是，0:否|
|field_of_this|TINYINT|3|方法返回的字段是否属于this对象，1:是，0:否|
|field_in_simple_class_name|VARCHAR|255|方法返回的字段所在的类唯一类名|
|simple_field_type_nad|VARCHAR|255|方法返回的字段类型唯一类名（不包含数组标志）|
|field_array_dimensions|TINYINT|3|方法返回的字段数组类型的维度，为0代表不是数组类型|
|field_name|VARCHAR|255|方法返回的字段名称|
|field_in_class_name|VARCHAR|255|方法返回的字段所在的类完整类名|
|field_type_nad|VARCHAR|255|方法返回的字段类型完整类名（不包含数组标志）|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 38. jacg_method_return_generics_type 方法返回泛型类型

- 表名前缀

jacg_method_return_generics_type

- 注释

方法返回泛型类型

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|type|VARCHAR|5|类型，t:方法返回类型，gt:方法返回类型中的泛型类型|
|type_seq|TINYINT|3|类型序号，方法返回类型固定为0，方法返回类型中的泛型类型从0开始|
|simple_generics_type_nad|VARCHAR|255|方法返回类型或其中的泛型类型唯一类名（不包含数组标志）|
|generics_array_dimensions|TINYINT|3|方法返回类型中的泛型数组类型的维度，为0代表不是数组类型|
|type_variables_name|VARCHAR|255|方法返回类型中的泛型类型变量名称|
|wildcard|VARCHAR|8|方法返回类型中的泛型通配符|
|reference_type|VARCHAR|255|方法返回类型中的泛型通配符引用的类型|
|generics_category|VARCHAR|5|方法返回类型中的泛型类型分类，J:JDK中的类型，C:自定义类型|
|generics_type_nad|VARCHAR|255|方法返回类型或其中的泛型类型类名（不包含数组标志）|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 39. jacg_method_throw 方法中throw的异常信息

- 表名前缀

jacg_method_throw

- 注释

方法中throw的异常信息

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|simple_class_name|VARCHAR|255|唯一类名|
|throw_offset|INT|10|throw指令的偏移量|
|line_number|INT|10|throw的代码行号|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|throw_exception_type|VARCHAR|255|throw的异常类型|
|throw_flag|VARCHAR|5|throw的标志，ce:catch的异常对象，mcr:方法调用返回值，unk:未知情况|
|catch_start_offset|INT|10|抛出异常属于catch的异常对象时，对应的catch代码块开始指令偏移量|
|catch_exception_variable_name|VARCHAR|255|抛出异常对应的catch的异常对象变量名称|
|call_id|INT|10|抛出异常属于方法调用返回值时，对应的方法调用ID|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 40. jacg_mybatis_ms_column MyBatis的Entity与数据库字段名信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_column

- 注释

MyBatis的Entity与数据库字段名信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|entity_simple_class_name|VARCHAR|255|MyBatis Entity类唯一类名|
|entity_field_name|VARCHAR|200|Entity类字段名|
|column_name|VARCHAR|200|数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）|
|entity_class_name|VARCHAR|255|MyBatis Entity类完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 41. jacg_mybatis_ms_entity MyBatis的Entity与Mapper、表名（使用MySQL）

- 表名前缀

jacg_mybatis_ms_entity

- 注释

MyBatis的Entity与Mapper、表名（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|entity_simple_class_name|VARCHAR|255|MyBatis Entity类唯一类名|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|entity_class_name|VARCHAR|255|MyBatis Entity类完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 42. jacg_mybatis_ms_formated_sql MyBatis XML中格式化后的sql文本（使用MySQL）

- 表名前缀

jacg_mybatis_ms_formated_sql

- 注释

MyBatis XML中格式化后的sql文本（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|sql_id|VARCHAR|200|MyBatis XML中的sql id|
|sql_seq|TINYINT|3|sql文本序号，从0开始|
|xml_element_name|VARCHAR|15|XML元素名称，如select、insert、update等|
|formated_sql|TEXT|65535|格式化后的sql文本|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 43. jacg_mybatis_ms_get_set_db 使用MyBatis时get/set方法所关联的数据库信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_get_set_db

- 注释

使用MyBatis时get/set方法所关联的数据库信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|fld_relationship_id|INT|10|通过get/set方法关联的字段关系id，从1开始|
|get_or_set|VARCHAR|3|对应get方法还是set方法|
|get_method_call_id|INT|10|get方法对应的方法调用ID，从1开始|
|set_method_call_id|INT|10|set方法对应的方法调用ID，从1开始|
|db_operate|VARCHAR|20|数据库操作，包含sql语句，除select、insert、update、delete外，后面可能加上@set、@where|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|column_name|VARCHAR|200|数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）|
|column_relate_desc|VARCHAR|10|MyBatis字段与Java代码字段关联方式描述，参考 MyBatisColumnRelateDescEnum 枚举类|

# 44. jacg_mybatis_ms_select_column MyBatis的XML中select的字段信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_select_column

- 注释

MyBatis的XML中select的字段信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|mapper_method_name|VARCHAR|200|MyBatis Mapper方法名|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|column_name|VARCHAR|200|数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）|
|column_alias|VARCHAR|64|数据库字段别名|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 45. jacg_mybatis_ms_set_column MyBatis的XML中update set子句的字段信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_set_column

- 注释

MyBatis的XML中update set子句的字段信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|mapper_method_name|VARCHAR|200|MyBatis Mapper方法名|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|column_name|VARCHAR|200|数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）|
|param_obj_name|VARCHAR|200|数据库字段赋值的参数对象名称|
|param_name|VARCHAR|200|数据库字段赋值的参数名称，不包含参数对象名称|
|param_raw_name|VARCHAR|200|数据库字段赋值的参数原始名称，包含参数对象名称|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 46. jacg_mybatis_ms_table MyBatis Mapper方法操作的数据库表信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_table

- 注释

MyBatis Mapper方法操作的数据库表信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|mapper_method_name|VARCHAR|200|MyBatis Mapper方法名|
|sql_statement|VARCHAR|15|sql语句类型|
|table_seq|TINYINT|3|数据库表序号|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 47. jacg_mybatis_ms_where_column MyBatis的XML中where子句的字段信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_where_column

- 注释

MyBatis的XML中where子句的字段信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|mapper_method_name|VARCHAR|200|MyBatis Mapper方法名|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|column_name|VARCHAR|200|数据库字段名（MyBatis XML中可能使用函数，长度需要长一些）|
|operation|VARCHAR|20|数据库字段进行比较的方式|
|param_obj_name|VARCHAR|200|数据库字段赋值的参数对象名称|
|param_name|VARCHAR|200|数据库字段赋值的参数名称，不包含参数对象名称|
|param_raw_name|VARCHAR|200|数据库字段赋值的参数原始名称，包含参数对象名称|
|param_type|CHAR|1|数据库字段用于比较的参数的使用方式，#/$|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 48. jacg_mybatis_ms_write_table MyBatis Mapper方法写的数据库表信息（使用MySQL）

- 表名前缀

jacg_mybatis_ms_write_table

- 注释

MyBatis Mapper方法写的数据库表信息（使用MySQL）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|mapper_simple_class_name|VARCHAR|255|MyBatis Mapper唯一类名|
|mapper_method_name|VARCHAR|200|MyBatis Mapper方法名|
|sql_statement|VARCHAR|15|写操作sql语句类型|
|table_name|VARCHAR|200|数据库表名（MyBatis XML中可能使用函数，长度需要长一些）|
|mapper_class_name|VARCHAR|255|MyBatis Mapper完整类名|
|xml_file_name|VARCHAR|255|MyBatis XML文件名|
|xml_file_path|VARCHAR|500|MyBatis XML文件路径|

# 49. jacg_properties_conf properties文件配置信息表

- 表名前缀

jacg_properties_conf

- 注释

properties文件配置信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|properties_key|VARCHAR|255|properties配置名称|
|properties_file_path|VARCHAR|500|properties配置文件路径|
|properties_file_name|VARCHAR|255|properties配置文件名|
|properties_value|TEXT|65535|properties配置内容|

# 50. jacg_set_method_assign_info dto的set方法被调用时的赋值信息

- 表名前缀

jacg_set_method_assign_info

- 注释

dto的set方法被调用时的赋值信息

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|set_record_id|INT|10|set方法记录id，从1开始|
|set_method_call_id|INT|10|set方法被调用时的方法调用序号，从1开始|
|seq|INT|10|set方法当前被调用时被赋值情况的序号，从0开始|
|step|INT|10|set方法当前被调用时被赋值时通过方法调用传递的步骤，从0开始|
|fld_relationship_id|INT|10|字段关联关系id，从1开始|
|curr_call_id|INT|10|当前的方法调用序号，从1开始|
|caller_method_hash|VARCHAR|30|调用方，方法hash+字节数|
|caller_full_method|TEXT|65535|调用方，完整方法（类名+方法名+参数）|
|caller_line_number|INT|10|调用方法源代码行号|
|callee_full_method|TEXT|65535|被调用方，完整方法（类名+方法名+参数）|
|set_method_hash|VARCHAR|30|set方法hash+字节数|
|set_full_method|TEXT|65535|set方法完整方法（类名+方法名+参数）|
|set_method_in_super|TINYINT|3|set方法是否在超类中，1:是，0:否|
|flag|VARCHAR|20|set方法被调用时的赋值情况标志，见 SetMethodAssignFlagEnum 类|
|flag_desc|VARCHAR|50|set方法被调用时的赋值情况标志描述|
|assign_info|TEXT|65535|set方法被调用时的赋值信息|
|equivalent_conversion|TINYINT|3|是否属于等值转换前的数据，1:是，0:否|

# 51. jacg_set_method dto的set方法及字段

- 表名前缀

jacg_set_method

- 注释

dto的set方法及字段

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|method_name|VARCHAR|200|方法名|
|field_name|VARCHAR|200|字段名|
|field_category|VARCHAR|5|字段分类，J:JDK中的类型，C:自定义类型，GJ:泛型类型，只涉及JDK中的类型，GC:泛型类型，涉及自定义类型|
|simple_field_type_nad|VARCHAR|255|字段类型唯一类名（不包含数组标志）|
|field_type_nad|VARCHAR|255|字段类型（不包含数组标志）|
|array_dimensions|TINYINT|3|字段数组类型的维度，为0代表不是数组类型|
|class_name|VARCHAR|255|完整类名|
|method_hash|VARCHAR|30|方法hash+字节数|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 52. jacg_sf_field_method_call static、final字段初始化方法信息表（含枚举）

- 表名前缀

jacg_sf_field_method_call

- 注释

static、final字段初始化方法信息表（含枚举）

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|simple_class_name|VARCHAR|255|唯一类名|
|field_name|VARCHAR|200|字段名|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|call_id|INT|10|字段初始化对应的方法调用序号，从1开始|
|field_type_nad|VARCHAR|255|字段类型（不包含数组标志）|
|array_dimensions|TINYINT|3|字段数组类型的维度，为0代表不是数组类型|
|class_name|VARCHAR|255|完整类名|
|callee_class_name|VARCHAR|255|初始化方法被调类名|
|callee_method_name|VARCHAR|200|初始化方法被调用方法名|

# 53. jacg_spring_bean Spring Bean信息表

- 表名前缀

jacg_spring_bean

- 注释

Spring Bean信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|spring_bean_name|VARCHAR|255|Spring Bean的名称|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|class_name|VARCHAR|255|完整类名|
|bean_type|VARCHAR|2|Spring Bean的定义方式，j: 在Java代码中定义，x: 在XML文件中定义|

# 54. jacg_spring_controller Spring Controller信息表

- 表名前缀

jacg_spring_controller

- 注释

Spring Controller信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|seq|INT|10|序号，从0开始，大于0代表有多种可能|
|show_uri|VARCHAR|250|用于显示的URI|
|class_path|VARCHAR|250|类上的注解path属性原始值|
|method_path|VARCHAR|250|方法上的注解path属性原始值|
|annotation_name|VARCHAR|255|注解类名|
|simple_class_name|VARCHAR|255|唯一类名|
|maybe_file_upload|TINYINT|3|方法可能用于文件上传，1:是，0:否|
|maybe_file_download|TINYINT|3|方法可能用于文件下载，1:是，0:否|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

# 55. jacg_spring_task Spring定时任务信息表

- 表名前缀

jacg_spring_task

- 注释

Spring定时任务信息表

|字段名|字段类型|字段大小|字段注释|
|---|---|---|---|
|record_id|INT|10|记录id，从1开始|
|method_hash|VARCHAR|30|方法hash+字节数|
|spring_bean_name|VARCHAR|255|Spring Bean的名称|
|class_name|VARCHAR|255|完整类名|
|method_name|VARCHAR|200|方法名|
|type|VARCHAR|10|类型，XML: 在XML文件中定义，annotation: 通过注解定义|
|full_method|TEXT|65535|完整方法（类名+方法名+参数）|
|return_type|VARCHAR|255|方法返回类型，包含数组标志|

