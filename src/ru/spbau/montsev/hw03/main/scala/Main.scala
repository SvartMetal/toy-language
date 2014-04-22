package ru.spbau.montsev.hw03.main.scala


object Main extends App {
  val evaluator = Evaluator()

  val s0 =
    """
       x = 1.0;
       y = 1;
       println(y);
       print(x);
       z = 3;
       println(z);
       print(x);
       print(y);
       println(true);
       println(false);
       println("hello");
       println("xxx");
       println(z);
       z = 3.0;
       print(z);
       {
       }
    """

  val s1 =
    """
       x = 1.0;
       y = 1;
       println(y);
       print(x);
       z = 3;
       println(z);
       print(x);
       print(y);
       println(true);
       println(false);
       println("hello");
       println("xxx");
       println(z);
       z = 3.0;
       print(z);
       t = [ ];
       t = [ x = "321"; y = [x = 2.0; y = 3.0;]; ];
       println(t);
       println(t.y.y);
       {
         println(t.x);
       }
       t.x = 3.0;
       println(t.x);
    """

  val s2 =
    """
       for (i <- 1 to 10) {
         println(i);
       }

       f = (x = 1) => {
         println(x);
         x + 1;
       };

       y = 10;
       while (y > 0) {
         println(y);
         {
           y = y - 2;
         }
       }

       println(f());

       s = [
         x = 1;
         y = (x) => {
           x + 3;
           [
             v = 15;
             t = (x) => {
               println(x);
               (x) => {
                 x * 10;
                 [
                   z = v * 30;
                 ];
               };
             };
           ];
         };
       ];

       println(s.y(3).t(10)(10).z);

    """.stripMargin

  val s3 =
    """
       fib = (n = 0) => {
         if (n == 1 || n == 2) {
           1;
         } else {
           fib(n - 1) + fib(n - 2);
         }
       };

       println(fib(15));
    """.stripMargin

  val s4 =
    """
       i = 2;
       n = 15;
       fibSum = 1;
       fib1 = 1;
       fib2 = 1;
       while (i < n) {
         fibSum = fib1 + fib2;
         fib2 = fib1;
         fib1 = fibSum;
         i = i + 1;
       }
       println(fibSum);
    """.stripMargin

  println("\n\n----------------------------------- Program s0 -----------------------------------\n")
  evaluator.eval(Parser.parse(s0))

  println("\n\n----------------------------------- Program s1 -----------------------------------\n")
  evaluator.eval(Parser.parse(s1))

  println("\n\n----------------------------------- Program s2 -----------------------------------\n")
  evaluator.eval(Parser.parse(s2))

  println("\n\n----------------------------------- Program s3 -----------------------------------\n")
  evaluator.eval(Parser.parse(s3))

  println("\n\n----------------------------------- Program s4 -----------------------------------\n")
  evaluator.eval(Parser.parse(s4))

}
