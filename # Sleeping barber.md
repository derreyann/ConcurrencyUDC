# Sleeping barber

## Init

```c
#include <pthread.h>
#define sleeping 0
#define working 1
#define max_waiting


int barber_state;
int customers=0;

pthread_mutex_t mutex;

cond no_customers, waiting_room;
```

## Barber
```c
lock(shop);
if(customers==0){
    barber_state=sleeping;
    wait(no_customers, shop);
    barber_state=working;
}else{
    pthread_cond signal(waiting_room);
}
unlock(shop);
cut_hair();
```
## Customer

```c
lock(shop);
if(customers==max_waiting){
    unlock(shop);
}else{
    if(barber_state==working){
        customers++;
        wait(waiting_room);
    }else{ //baber sleeps
        signal(no_customers)
        barber_state(working);
    }
    unlock(shop);   
    cut_hair();
}
```
