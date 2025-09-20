public class Grade {

    public static String gradeFromScore(int score) {
        if (score >= 90) return "A";
        if (score >= 80) return "B";
        if (score >= 70) return "C";
        if (score >= 60) return "D";
        return "F";
    }

    public static void main(String[] args) {
        System.out.println("Score 85 => " + gradeFromScore(85));
    }
}