package funsets;
import apple.laf.JRSUIUtils;

import java.util.HashMap;
import java.util.ArrayList;

/**
 * Created by vivekanandganapathynagarajan on 10/21/16.
 */

class TreeNode {
    int val;
    TreeNode left;
    TreeNode right;
    TreeNode(int x) { val = x; }
}

public class LCABinaryTree {


        public TreeNode lowestCommonAncestor(TreeNode root, TreeNode p, TreeNode q) {
            HashMap<TreeNode, TreeNode> nodeParent = new HashMap<TreeNode, TreeNode>();
            inOrderTraversal(root, nodeParent);
            ArrayList<TreeNode> pathToP = new ArrayList<TreeNode>();
            ArrayList<TreeNode> pathToQ = new ArrayList<TreeNode>();
            updatePath(nodeParent, root, p, pathToP);
            updatePath(nodeParent, root, q, pathToQ);
            return lowestCommonAncestor(pathToP, pathToQ);
        }

        void updatePath(HashMap<TreeNode, TreeNode> nodeParent, TreeNode root, TreeNode node, ArrayList<TreeNode> path){
            path.add(node);
            while (node != root){
                node = nodeParent.get(node);
                path.add(node);
            }
        }

        TreeNode lowestCommonAncestor(ArrayList<TreeNode> pathToP, ArrayList<TreeNode> pathToQ){
            int i = pathToP.size();
            int j = pathToQ.size();
            TreeNode lowest = null;
            while (pathToP.get(i-1) == pathToQ.get(j-1)){
                lowest = pathToP.get(i-1);
                i--;
                j --;
                if (i <= 0 || j <= 0){
                    break;
                }
            }
            return lowest;
        }

        void inOrderTraversal(TreeNode node, HashMap<TreeNode, TreeNode> nodeParent ){
            if (node == null){
                return;
            }
            if (node.left != null){
                nodeParent.put(node.left, node);
            }
            if (node.right != null){
                nodeParent.put(node.right, node);
            }
            inOrderTraversal(node.left, nodeParent);
            inOrderTraversal(node.right, nodeParent);
        }



    public static void main(String[] args){
        TreeNode node = new TreeNode(1);
        TreeNode left = new TreeNode(2);
        node.left = left;
        TreeNode result = new LCABinaryTree().lowestCommonAncestor(node, node, left);

    }
}
