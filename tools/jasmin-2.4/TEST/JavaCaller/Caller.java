
/*
class AddInstructions_java {
  public static int add(int a , int b) {
      return a + b;
  }
}
*/

public class Caller {
  public static void main(String args[]) {
//     int res = AddInstructions_java.add(10,20);
     int res = AddInstructions.add(10,20);
     System.out.println("Res = " + res);
  }
}
