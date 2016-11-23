package recfun;

/**
 * Created by vivekanandganapathynagarajan on 10/16/16.
 */
public class PartitionEqualSubSet {

    public boolean canPartition(int[] nums) {
        int sum = computeSum(nums);
        if (sum % 2 == 1){ //sum is odd cannot be partitioned
            return false;
        }
        sum = sum/2;
        return dynaSol(nums, sum);
    }

    private boolean dynaSol(int[] nums, int sum){
        boolean[][] opt = new boolean[sum+1][nums.length+1];
        //init step sum with 0 elements
        for (int i=1; i < sum; i++){
            opt[i][0] = false;
        }
        for (int i =0; i < nums.length; i++){
            opt[0][i] = true;
        }
        for (int s=1; s < sum+1; s++){
            for (int i=1; i < nums.length+1; i++){
                boolean val1 = false;
                if (s - nums[i-1] >= 0){
                    val1 = opt[s- nums[i-1]][i-1];
                }
                boolean val2 = opt[s][i-1];
                if (val1 || val2){
                    opt[s][i] = true;
                }
            }
        }
        return opt[sum][nums.length];
    }

    int computeSum(int[] nums){
        int sum = 0;
        for (int i =0;i < nums.length; i++){
            sum+= nums[i];
        }
        return sum;
    }

    public static void main(String[] args){
        int[] nums = new int[]{1, 5, 11, 5};
        System.out.println(new PartitionEqualSubSet().canPartition(nums));
    }
}
