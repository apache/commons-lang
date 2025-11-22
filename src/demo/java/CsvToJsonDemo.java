import org.apache.commons.lang3.csv.CsvToJson;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Demo class to showcase the CsvToJson functionality.
 */
public class CsvToJsonDemo {
    
    public static void main(String[] args) {
        try {
            System.out.println("=== CSV to JSON Demo ===\n");
            
            // Demo 1: Basic CSV string to JSON
            System.out.println("1. Basic CSV string to JSON:");
            String csv1 = "name,age,city\nJohn Doe,25,New York\nJane Smith,30,Los Angeles";
            String json1 = CsvToJson.fromString(csv1).toJson();
            System.out.println("CSV Input:");
            System.out.println(csv1);
            System.out.println("\nJSON Output:");
            System.out.println(json1);
            
            // Demo 2: CSV with quoted fields
            System.out.println("\n2. CSV with quoted fields:");
            String csv2 = "name,description,skills\n\"John Doe\",\"Software Engineer\",\"Java,Python,JavaScript\"\n\"Jane Smith\",\"Data Scientist\",\"Python,R,SQL\"";
            String json2 = CsvToJson.fromString(csv2).toJson();
            System.out.println("CSV Input:");
            System.out.println(csv2);
            System.out.println("\nJSON Output:");
            System.out.println(json2);
            
            // Demo 3: CSV with custom delimiter
            System.out.println("\n3. CSV with custom delimiter (semicolon):");
            String csv3 = "name;age;city\nAlice;28;New York\nBob;32;Los Angeles\nCharlie;25;Chicago";
            String json3 = CsvToJson.builder()
                    .withDelimiter(';')
                    .fromString(csv3)
                    .toJson();
            System.out.println("CSV Input:");
            System.out.println(csv3);
            System.out.println("\nJSON Output:");
            System.out.println(json3);
            
            // Demo 4: CSV without headers
            System.out.println("\n4. CSV without headers (using custom headers):");
            String csv4 = "John,25,true\nJane,30,false\nBob,35,true";
            String json4 = CsvToJson.builder()
                    .withHeaders(false)
                    .withCustomHeaders("employee_name", "years_experience", "is_active")
                    .fromString(csv4)
                    .toJson();
            System.out.println("CSV Input:");
            System.out.println(csv4);
            System.out.println("\nJSON Output with custom headers:");
            System.out.println(json4);
            
            // Demo 5: CSV with type conversion
            System.out.println("\n5. CSV with automatic type conversion:");
            String csv5 = "name,age,salary,active\nJohn,25,75000.50,true\nJane,30,85000,false";
            String json5 = CsvToJson.fromString(csv5)
                    .toJson(new CsvToJson.JsonConfig().withTypeConversion(true));
            System.out.println("CSV Input:");
            System.out.println(csv5);
            System.out.println("\nJSON Output with type conversion:");
            System.out.println(json5);
            
            // Demo 6: CSV file to JSON
            System.out.println("\n6. CSV file to JSON:");
            File csvFile = new File("demo-data.csv");
            String csvFileData = "product,price,stock\nLaptop,999.99,25\nMouse,19.99,100\nKeyboard,49.99,75";
            Files.write(Paths.get(csvFile.getAbsolutePath()), csvFileData.getBytes(StandardCharsets.UTF_8));
            
            String json6 = CsvToJson.fromFile(csvFile).toJson();
            System.out.println("CSV File Content:");
            System.out.println(csvFileData);
            System.out.println("\nJSON Output:");
            System.out.println(json6);
            
            // Demo 7: Handling empty lines and null values
            System.out.println("\n7. Handling empty lines and null values:");
            String csv7 = "name,age,city\n\nJohn,25,\n\nJane,,Los Angeles\n";
            String json7 = CsvToJson.fromString(csv7).toJson();
            System.out.println("CSV Input:");
            System.out.println(csv7);
            System.out.println("\nJSON Output:");
            System.out.println(json7);
            
            // Demo 8: Complex CSV with special characters
            System.out.println("\n8. Complex CSV with special characters:");
            String csv8 = "name,description,notes\n\"John O'Brien\",\"Line 1\nLine 2\",\"Special chars: \t\n\"\"\n\"Jane \"\"The Great\"\" Smith\",\"Multi-line\nfield\",\"Unicode: 你好\n世界\"";
            String json8 = CsvToJson.fromString(csv8).toJson();
            System.out.println("CSV Input:");
            System.out.println(csv8);
            System.out.println("\nJSON Output:");
            System.out.println(json8);
            
            // Demo 9: Builder pattern with full configuration
            System.out.println("\n9. Builder pattern with full configuration:");
            String csv9 = "name|age|salary|active\n\"John Doe\"|25|75000.50|true\n\"Jane Smith\"|30|85000.00|false";
            String json9 = CsvToJson.builder()
                    .withDelimiter('|')
                    .withQuoteChar('"')
                    .withHeaders(true)
                    .skipEmptyLines(true)
                    .fromString(csv9)
                    .toJson(new CsvToJson.JsonConfig().withTypeConversion(true));
            System.out.println("CSV Input:");
            System.out.println(csv9);
            System.out.println("\nJSON Output:");
            System.out.println(json9);
            
            // Demo 10: Error handling
            System.out.println("\n10. Error handling examples:");
            try {
                CsvToJson.fromFile(new File("nonexistent.csv")).toJson();
            } catch (IllegalArgumentException e) {
                System.out.println("Expected error for nonexistent file: " + e.getMessage());
            }
            
            try {
                CsvToJson.builder()
                        .withHeaders(false)
                        .fromString("John,25")
                        .toJson();
            } catch (IllegalArgumentException e) {
                System.out.println("Expected error for missing headers: " + e.getMessage());
            }
            
            System.out.println("\n=== All demos completed successfully! ===");
            
        } catch (IOException e) {
            System.err.println("Error during demo: " + e.getMessage());
            e.printStackTrace();
        }
    }
}