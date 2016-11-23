package recfun;

/**
 * Created by vivekanandganapathynagarajan on 10/12/16.
 */
public class CoinChange {
    int[][] A;
    int[] W;

    CoinChange(int[] W, int value){
        this.W = W;
        A = new int[value+1][W.length+1];
    }

    int run(){
        //init step
        for (int i=1; i < A[0].length; i++){
            A[0][i] = 1;
        }
        for (int val=1; val < A.length; val++ ){
            for (int w =1; w < A[0].length; w++){
                int prev =  A[val][w-1];
                int current = 0;
                if (val >= W[w-1]){
                   current =  A[val- W[w-1]][w];
                }

                A[val][w] = prev + current;
            }
        }
        return A[A.length-1][W.length];
    }

    public static void main(String[] args){
        CoinChange c = new CoinChange(new int[]{5,10,20,50,100,200,500}, 300);
        System.out.println(c.run());
    }
}
