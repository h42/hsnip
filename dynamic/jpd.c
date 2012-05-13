#include <stdio.h>

double dcadd(double x, double y);
int dcmul(int x, double y);

int main() {
    printf("dcadd = %.2f\ndcmul=%d\n", dcadd(3,4), dcmul(3,4));
    return 0;
}
