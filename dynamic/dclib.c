double dcadd(double x, double y) {
    return  x+y;
}

int dcmul(int x, double y) {
    return x * y;
}

int intptr(int * ip) {
    ip[0] = 6;
    ip[1] = 28;
    return 1;
}
