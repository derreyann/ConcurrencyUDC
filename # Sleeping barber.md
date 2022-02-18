- [Sleeping barber](#sleeping-barber)
  - [Init](#init)
  - [Barber](#barber)
  - [Customer](#customer)
- [Semaphores version](#semaphores-version)
  - [Barbers](#barbers)
  - [Customers](#customers)

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
    //barber_state=sleeping;
    barbers++
    wait(no_customers, shop);
    barber_state=working;
}else{
    pthread_cond signal(waiting_room);
    customers--;
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
        barbers--;
        //barber_state(working);
    }
    unlock(shop);   
    cut_hair();
}
```

# Semaphores version
```c
sem barbers=0, customers=0, waiting_room=, max_waiting;
```
## Barbers
```c
V(barbers);
P(customers);
```
## Customers
```c
if(try(p(waiting_room))==-1){
} //trywait() in the sem library
else{
V(customers);
P(Barbers);
V(waiting_room);
cut_hair();
}
```