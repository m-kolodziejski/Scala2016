package example;

/**
 * Created by mzki on 1/4/2017.
 */
public class Outside {

    private int a;

    public static void main(String str[]) {
        new Outside().foo(1);
    }

    public void foo(int b) {
        int c = a;
        int d = b;
        class Inside {
            public Inside() {

            }
        }
        Inside e = new Inside();
    }

}
