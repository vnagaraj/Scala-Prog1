package recfun;

/**
 * Created by vivekanandganapathynagarajan on 10/13/16.
 */
public class Solution {



        public int coinChange(int[] coins, int amount) {
            Integer[][] A = new Integer[amount+1][coins.length+1];
            //init  step
            for (int i=0; i < A[0].length; i++){
                A[0][i] = 0;
            }
            //A[amount][coin(j)] = min(A[amount][coin(j-1)], A[amount-coin(j)][coin(j)]
            for (int amt=1; amt < A.length; amt++){
                for (int coin=1; coin < A[0].length; coin++){
                    Integer prev = A[amt][coin-1];
                    Integer val = amt - coins[coin-1];
                    if (prev != null && val >= 0 && A[val][coin] != null)
                        A[amt][coin] = Math.min(prev, A[val][coin]+1);
                    else if (prev == null && val >= 0 && A[val][coin] != null)
                        A[amt][coin] = A[val][coin]+1;
                    else if (prev != null)
                        A[amt][coin] = prev;
                }
            }
            if (A[amount][coins.length] == null){
                return -1;
            }
            return A[amount][coins.length];
        }

        public static void main(String[] args){
           int amount = 27;
            int[] coins = new int[] {2,5,10,1};
            System.out.println(new Solution().coinChange(coins, amount));
        }
}
