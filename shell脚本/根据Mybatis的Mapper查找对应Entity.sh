# 执行当前脚本前，需要将对应应用的jar/war包解压，以下find_dir参数为保存Mybatis的XML文件的目录
array_string="mapper1 mapper2"

find_dir=/test/dir1/

array_mapper=($array_string)
for i in ${!array_mapper[@]}
do 
mapper=${array_mapper[i]}
xml_file=`find $find_dir -name \*.xml | xargs grep 'mapper [ ]*namespace' | grep "\.$mapper\"" | head -1 | awk -F ':' '{print $1}'`
entity=`grep 'resultMap' $xml_file | grep ' type=' | head -1 | awk -F '"' '{print $4}' | awk -F '.' '{print $NF}'`

echo $mapper $entity
done
