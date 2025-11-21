# CSV to JSON 工具类 - 重新设计版

这是一个完全重新设计的CSV转JSON工具类，解决了之前版本的所有问题，提供了更健壮、更灵活的API。

## 主要改进

- ✅ **正确的JSON转义**：使用标准JSON转义规则
- ✅ **完整的CSV解析**：支持RFC 4180标准，包括引号转义
- ✅ **类型转换可选**：默认不转换类型，通过配置启用
- ✅ **保持列顺序**：使用LinkedHashMap确保列顺序
- ✅ **无trim()破坏数据**：保留原始数据格式
- ✅ **统一的API接口**：提供清晰的Builder模式和静态工厂方法
- ✅ **完整测试覆盖**：所有功能都有对应的测试

## 快速开始

### 基本用法

```java
import org.apache.commons.lang3.csv.CsvToJson;

// 从CSV字符串
String csv = "name,age,city\nJohn,25,New York\nJane,30,Los Angeles";
String json = CsvToJson.fromString(csv).toJson();

// 从CSV文件
String json = CsvToJson.fromFile(new File("data.csv")).toJson();

// 从字节数组
String json = CsvToJson.fromBytes(csvBytes).toJson();
```

### 使用Builder模式进行配置

```java
String csv = "name;age;city\nJohn;25;New York";
String json = CsvToJson.builder()
        .withDelimiter(';')
        .withQuoteChar('"')
        .withHeaders(true)
        .fromString(csv)
        .toJson();
```

## 完整API参考

### 输入源

- `CsvToJson.fromString(String csv)` - 从字符串
- `CsvToJson.fromFile(File file)` - 从文件（UTF-8）
- `CsvToJson.fromFile(File file, Charset charset)` - 从文件（指定编码）
- `CsvToJson.fromBytes(byte[] bytes)` - 从字节数组（UTF-8）
- `CsvToJson.fromBytes(byte[] bytes, Charset charset)` - 从字节数组（指定编码）
- `CsvToJson.fromReader(Reader reader)` - 从Reader

### Builder配置选项

```java
CsvToJson.builder()
    .withDelimiter(',')          // 设置分隔符，默认逗号
    .withQuoteChar('"')          // 设置引号字符，默认双引号
    .withHeaders(true)           // 设置是否有标题行，默认true
    .withCustomHeaders("col1", "col2") // 设置自定义标题
    .skipEmptyLines(true)        // 是否跳过空行，默认true
```

### JSON配置选项

```java
CsvToJson.JsonConfig config = new CsvToJson.JsonConfig()
    .withTypeConversion(true);   // 是否进行类型转换，默认false

String json = csvToJson.toJson(config);
```

## 功能特性

### 1. 标准CSV解析
- 支持RFC 4180标准
- 正确处理引号转义（""）
- 支持包含分隔符的字段
- 支持多行字段

### 2. 数据完整性
- 保留原始数据格式（不自动trim）
- 正确处理空值和空字符串
- 保持列顺序

### 3. 灵活配置
- 自定义分隔符
- 自定义引号字符
- 可选标题行处理
- 自定义列名

### 4. 类型转换（可选）
- 字符串 → 数字
- 字符串 → 布尔值
- 字符串 → null
- 默认禁用，通过配置启用

### 5. 错误处理
- 清晰的异常信息
- 输入验证
- 文件存在性检查

## 示例

### 示例1：基本转换

**输入CSV：**
```csv
name,age,city
John Doe,25,New York
Jane Smith,30,Los Angeles
```

**代码：**
```java
String csv = "name,age,city\nJohn Doe,25,New York\nJane Smith,30,Los Angeles";
String json = CsvToJson.fromString(csv).toJson();
```

**输出JSON：**
```json
[{"name":"John Doe","age":"25","city":"New York"},{"name":"Jane Smith","age":"30","city":"Los Angeles"}]
```

### 示例2：带引号的CSV

**输入CSV：**
```csv
name,description,skills
"John Doe","Software Engineer","Java,Python,JavaScript"
"Jane ""The Great"" Smith","Data Scientist","Python,R,SQL"
```

**代码：**
```java
String csv = "name,description,skills\n\"John Doe\",\"Software Engineer\",\"Java,Python,JavaScript\"\n\"Jane \"\"The Great\"\" Smith\",\"Data Scientist\",\"Python,R,SQL\"";
String json = CsvToJson.fromString(csv).toJson();
```

### 示例3：自定义分隔符

**输入CSV：**
```csv
name;age;city
John;25;New York
Jane;30;Los Angeles
```

**代码：**
```java
String csv = "name;age;city\nJohn;25;New York\nJane;30;Los Angeles";
String json = CsvToJson.builder()
        .withDelimiter(';')
        .fromString(csv)
        .toJson();
```

### 示例4：无标题行（自定义列名）

**输入CSV：**
```csv
John,25,true
Jane,30,false
Bob,35,true
```

**代码：**
```java
String csv = "John,25,true\nJane,30,false\nBob,35,true";
String json = CsvToJson.builder()
        .withHeaders(false)
        .withCustomHeaders("name", "age", "active")
        .fromString(csv)
        .toJson();
```

**输出JSON：**
```json
[{"name":"John","age":"25","active":"true"},{"name":"Jane","age":"30","active":"false"},{"name":"Bob","age":"35","active":"true"}]
```

### 示例5：启用类型转换

**输入CSV：**
```csv
name,age,salary,active
John,25,75000.50,true
Jane,30,85000,false
```

**代码：**
```java
String csv = "name,age,salary,active\nJohn,25,75000.50,true\nJane,30,85000,false";
String json = CsvToJson.fromString(csv)
        .toJson(new CsvToJson.JsonConfig().withTypeConversion(true));
```

**输出JSON：**
```json
[{"name":"John","age":25,"salary":75000.5,"active":true},{"name":"Jane","age":30,"salary":85000,"active":false}]
```

## 错误处理

所有方法都提供了清晰的错误处理：

- `NullPointerException` - 当输入为null时
- `IllegalArgumentException` - 当文件不存在或配置无效时
- `IOException` - 当发生I/O错误时

## 性能考虑

- 使用StringBuilder进行高效的JSON构建
- 流式读取大文件
- 最小化内存分配

## 兼容性

- Java 8及以上版本
- 支持所有标准字符编码
- 支持Windows、Linux、macOS等所有主流操作系统

## 迁移指南

从旧版本迁移：

```java
// 旧版本（已删除）
String json = CsvToJsonUtils.toJsonString(csv);

// 新版本
String json = CsvToJson.fromString(csv).toJson();
```

## 许可证

Apache License 2.0 - 与Apache Commons Lang项目相同