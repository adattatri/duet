#include <pthread.h>

void* thread(void *arg) {
    int x;
    int i;
    x = __VERIFIER_nondet_int();
    __VERIFIER_assume(x > 0);
    i = 0;
    while (i < x) {
	i++;
    }
    assert(i == x);
    return NULL;
}

void main() {
    pthread_t t;
    //    while(rand()){
    
    pthread_create(&t, NULL, thread, NULL);
    pthread_create(&t, NULL, thread, NULL);
    //    pthread_create(&t, NULL, thread, NULL);
    //    pthread_create(&t, NULL, thread, NULL);
	//}
}
