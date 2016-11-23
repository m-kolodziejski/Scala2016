package quickcheck;


import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.function.BinaryOperator;
import java.util.stream.IntStream;
import java.util.stream.Stream;




public class CoffeeMaker {

    public static void main(String[] arg) {
        LocalDate date = LocalDate.of(2016, Month.DECEMBER, 12);
        System.out.println(date.plus(3, ChronoUnit.MONTHS));
        System.out.print(date);


    }
}