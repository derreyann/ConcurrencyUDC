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


//adding queuing to barber problem
struct sleep_info{
    int bnum, cnum;
    cond sleep;
}
queue customerq;
queue barberq;
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
    /** alt version queuing
     * 
     * customers--;
     * struct sleep_info *cust;
     * cust=removequeue(customerq);
     * signal(cust->sleep);
     * cust->bnum=bnm;
     * 
     **/
    pthread_cond signal(waiting_room);
    customers--;
}
unlock(shop);
cut_hair();
```
## Customer

```c
//init variables
int cnum;
struct sleep_info inf;

lock(shop);
if(customers==max_waiting){
    unlock(shop);
}else{
    if(barber_state==working){
        customers++;
        /** alt version queuing
         * 
         * inf.cnum=cnum;
         * insert_queue(customerq, inf);
         * wait(inf.sleep, shop); 
         * 
         **/
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