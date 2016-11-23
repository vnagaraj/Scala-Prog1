package recfun;

/**
 * Created by vivekanandganapathynagarajan on 10/13/16.
 */
public class MaxSubArraySum {
    public int maxSubArray(int[] nums) {
        int[] S = new int[nums.length+1];
        boolean[] b = new boolean[nums.length +1];//to indicate last elem in seq belongs to opt solution
        int[] C = new int[nums.length+1];
        S[0] = Integer.MIN_VALUE; //no elements so infinity
        S[1] = nums[0];
        b[1] = true; //first element of nums always belongs to sequence
        C[1] = Integer.MIN_VALUE;
        if (nums[0] >= 0)
            C[1] = nums[0];
        for (int i=2; i < S.length; i++){
            int val1 = Integer.MIN_VALUE;
            if (b[i-1]){
                val1 = nums[i-1] + S[i-1];
            }
            C[i] = Integer.MIN_VALUE;
            if (C[i-1] != Integer.MIN_VALUE) {
                if (C[i - 1] + nums[i - 1] >= 0) {
                    C[i] = C[i - 1] + nums[i - 1];
                }
            }
            if (nums[i - 1] >= 0 && nums[i-1] > C[i]){
                C[i] = nums[i-1];
            }
            int val = Math.max(Math.max(nums[i-1], S[i-1]), val1);
            val = Math.max(val, C[i]);
            if (val == nums[i-1] || val == val1 || val == C[i]){ //inicates includes last element
               b[i] = true;
            }
            S[i] = val;
        }
        return S[nums.length];
    }

    public static void main(String[] args){
        int[] nums = new int[]{-2,1,-3,4,-1,2,1,-5,4};
        System.out.println(new MaxSubArraySum().maxSubArray(nums));
    }
}
