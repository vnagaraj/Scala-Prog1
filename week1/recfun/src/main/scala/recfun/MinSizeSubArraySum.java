package recfun;

/**
 * Created by vivekanandganapathynagarajan on 10/13/16.
 */
public class MinSizeSubArraySum {

    Integer[][] mem = null;
    int[] nums;
    public int minSubArrayLen(int s, int[] nums) {
        mem = new Integer[s+1][nums.length+1];
        this.nums = nums;
        int val =  recursive_min(s, nums.length);
        if (val == -1){
            return 0;
        }
        return val;
    }

    public int  recursive_min(int s, int i){
        if (s <= 0){
            return 0;
        }
        if (i <=0){
            return -1;
        }
        if (mem[s][i] == null){
            int val1= recursive_min(s-nums[i-1],i-1);
            int val2 = recursive_min(s, i-1);
            if (val1 == -1 && val2 == -1){
                mem[s][i] = -1;
            } else if (val1 == -1){
                mem[s][i] = val2;
            }else if (val2 == -1){
                mem[s][i] = val1 + 1;
            } else
                mem[s][i]= Math.min(val1+1, val2);
        }
        return mem[s][i];

    }

    public static void main(String[] args){
        int s = 20;
        int[] nums = new int[]{2,16,14,15};
        System.out.println(new MinSizeSubArraySum().minSubArrayLen(s, nums));
    }
}
