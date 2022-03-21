# Writers/Readers

- [Writers/Readers](#writersreaders)
  - [Code Mutex](#code-mutex)
  - [Readers](#readers)
  - [Writers](#writers)
    - [Conditional writers init](#conditional-writers-init)

## Code Mutex
```c
#include <pthread.h>

pthread_mutex_t lock; //protecting data
int Readers0;
pthread_mutex_t readers_m, wp;
```

## Readers
```c
//r2, r3 stop here
lock(wp);
Lock(readers_m);
readers++;
if(readers==1){ //meaning we 1st
    //r1 goes here
    lock(lock); //lock shared area
}
unlock(readers_m);
unlock(wp);
read();
lock(readers_m);
readers--;
if(readers==0){ //last one 
    unlock(lock);
}
unlock(readers_m);
```

## Writers
```c
//w1 decalred
lock(writers_m);
writers++;
if(writers1){
    lock(wp);
}
unlock(writers_m);
lock(lock);
write();
unlock(lock);
lock(writers_m);
writers--;
if(writers==0){
    unlock(wp);
}
unlock(writers_m);
```

### Conditional writers init
```c
Cond stop;
readers(with conds){
    while (writers>0){
        wait(stop, lock);
    }
    readers++;
    unlock(lock);
    read();
    lock(lock);
    readers--;
    if(readers==0);
    signal(stop); //wait for only one writer to stop (only one will be able to access)
}
```
```c
writer{
    lock(lock);
    while(readers>0||writer>0){
        wait(stop, lock);
    }
    writer++;
    unlock(lock);
    write();
    lock(lock);
    writers--;
    broadcast(stop);
    unlock(lock);
}
```
