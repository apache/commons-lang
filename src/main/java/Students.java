public class Students {
    private String name;
    private int age;
    private int studentId;

    public Students(String name, int age, int studentId) {
        this.name = name;
        this.age = age;
        this.studentId = studentId;
    }

    public String getName(){
        return name;
    }
    public int getAge(){
        return age;
    }
    public int getStudentId(){
        return studentId;
    }

    public void setName(String name){
        this.name = name;
    }
    public void setAge(int age){
        this.age = age;
    }
    public void setStudentId(int studentId){
        this.studentId = studentId;
    }
    @Override
    public String toString() {
        return "Students{" +
                "name='" + name + '\'' +
                ", age=" + age +
                ", studentId=" + studentId +
                '}';
    }
    
}
