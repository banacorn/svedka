#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define True 1
#define False 0
#define Balanced 10
#define LL 11
#define LR 12
#define RL 13
#define RR 14

int ll = 0, lr = 0, rl = 0, rr = 0;


typedef int Bool;
typedef int Balance;

typedef struct Tree {
    Bool nil;
    int node;
    struct Tree * left;
    struct Tree * right;
} Tree;

void showPreorder (Tree * tree) {
    if (! tree -> nil) {
        printf("%d,", tree -> node);
        showPreorder(tree -> left);
        showPreorder(tree -> right);
    }
}
void showInorder (Tree * tree) {
    if (! tree -> nil) {
        showInorder(tree -> left);
        printf("%d\n", tree -> node);
        showInorder(tree -> right);
    }
}

Tree * newTree () {
    Tree * head = malloc(sizeof(Tree));
    head -> nil = True;
    return head;
}

void killTree (Tree * tree) {
    if (! tree -> nil) {
        killTree(tree -> left);
        killTree(tree -> right);
    }
    free(tree);
}

int max (int a, int b) {
    if (a > b)
        return a;
    else
        return b;
}

int height (Tree * tree) {

    if (tree -> nil)
        return 0;

    return 1 + max(height(tree -> left), height(tree -> right));
}

Balance balanceCase (Tree * tree) {

    // nil
    if (tree -> nil) {
        return Balanced;
    }

    // left is nil
    if (tree -> left -> nil) {
        if (height(tree -> right) < 2)
            return Balanced;
        if (height(tree -> right) >= 2 && height(tree -> right -> left) > height(tree -> right ->right))
            return RL;
        if (height(tree -> right) >= 2 && height(tree -> right ->left) < height(tree -> right ->right))
            return RR;
    }

    // right is nil
    if (tree -> right -> nil) {
        if (height(tree -> left) < 2)
            return Balanced;
        if (height(tree -> left) >= 2 && height(tree -> left -> left) > height(tree -> left -> right))
            return LL;
        if (height(tree -> left) >= 2 && height(tree -> left -> left) < height(tree -> left -> right))
            return LR;
    }
    
    // both left and right are not nil

    int heightL = height(tree -> left);
    int heightR = height(tree -> right);
    int heightLR = height(tree -> left -> right);
    int heightLL = height(tree -> left -> left);
    int heightRR = height(tree -> right -> right);
    int heightRL = height(tree -> right -> left);

    if (heightL - heightR < 2 && heightL - heightR > -2)
        return Balanced;

    if (heightL > heightR) {
        if (heightLL > heightLR)
            return LL;
        if (heightLR > heightLL)
            return LR;
    }

    if (heightL < heightR) {
        if (heightRL > heightRR)
            return RL;
        if (heightRR > heightRL)
            return RR;
    }
}

Tree * rotateLL (Tree * tree) {

    Tree * this = tree;
    Tree * l = tree -> left;
    Tree * lr = tree -> left -> right;


    tree = l;
    tree -> right = this;
    tree -> right -> left = lr;

    return tree;
} 


Tree * rotateRR (Tree * tree) {

    Tree * this = tree;
    Tree * r = tree -> right;
    Tree * rl = tree -> right -> left;


    tree = r;
    tree -> left = this;
    tree -> left -> right = rl;

    return tree;
}


Tree * rotateLR (Tree * tree) {

    Tree * l = tree -> left;
    Tree * lr = tree -> left -> right;
    Tree * lrl = tree -> left -> right -> left;

    tree -> left = lr;
    tree -> left -> left = l;
    tree -> left -> left -> right = lrl;

    tree = rotateLL(tree);

    return tree;
} 


Tree * rotateRL (Tree * tree) {

    Tree * r = tree -> right;
    Tree * rl = tree -> right -> left;
    Tree * rlr = tree -> right -> left -> right;

    tree -> right = rl;
    tree -> right -> right = r;
    tree -> right -> right -> left = rlr;

    tree = rotateRR(tree);

    return tree;
} 


Tree * balance (Tree * tree) {
    if (tree -> nil)
        return tree;

    Balance balanced = balanceCase(tree);

    switch (balanced) {
        case Balanced:
            tree -> left = balance(tree -> left);
            tree -> right = balance(tree -> right);
            return tree;
        case LL:
            ll++;
            tree -> left = balance(tree -> left);
            tree -> right = balance(tree -> right);
            return rotateLL(tree);
        case LR:
            lr++;
            tree -> left = balance(tree -> left);
            tree -> right = balance(tree -> right);
            return rotateLR(tree);
        case RL:
            rl++;
            tree -> left = balance(tree -> left);
            tree -> right = balance(tree -> right);
            return rotateRL(tree);
        case RR:
            rr++;
            tree -> left = balance(tree -> left);
            tree -> right = balance(tree -> right);
            return rotateRR(tree);
    }
}


Tree * insert (Tree * tree, int node) {

    if (tree -> nil) {
        tree -> nil = False;
        tree -> node = node;
        tree -> left = newTree();
        tree -> right = newTree();
        return tree;
    } else {

        if (node == tree -> node) {
            return tree;
        }

        if (node < tree -> node) {
            tree -> left = insert(tree -> left, node);
        } else {
            tree -> right = insert(tree -> right, node);
        }

        // showPreorder(tree);
        // tree = balance(tree);
        // printf("- -\n");
        // showPreorder(tree);
        // printf("---\n");
        return balance(tree);
        // return tree;
    }
}


int main (int argc, char ** argv) {

    Tree * tree = newTree();
    char file[100];
    char input[100];
    int number;

    strcpy(file, argv[1]);

    FILE * fp = fopen(file, "r");

    if (fp == NULL) perror("error opening FILE");

    while (fgets(input, 100, fp) != NULL) {
        number = atoi(input);

        tree = insert(tree, number);

    }

    printf("Pre-order:");
    showPreorder(tree);    
    printf("\n");

    printf("LL:%d\n", ll);
    printf("LR:%d\n", lr);
    printf("RL:%d\n", rl);
    printf("RR:%d\n", rr);

    
    fclose(fp);



    killTree(tree);
    return 0;
}