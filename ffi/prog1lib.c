#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int getfilemode(char *fn) {
    struct stat st;
    int rc = stat(fn, &st);
    int mode = (rc==0) ? st.st_mode & 07777 : 0;
    //printf("%d %o\n",mode,mode);
    return mode;
}

int setfilemode(char *fn, int mode) {
    return chmod(fn, mode);
}

// ARRAY TEST
void print_array(int n, int *a) {
    int i;
    for (i=0;i<n;i++) printf("%d\n",a[i]);
}

// STRING OUTPUT
char *get_string() {
    return "Hello World!";
}
    
// Malloc
char *get_bytes(char *buf) {
    strcat(buf,"Hey - get_bytes");
    return buf;
}
